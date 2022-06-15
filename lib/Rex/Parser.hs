{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

{- |

  Produces a single Rex value by reducing a list of lexer `Frag`s.

  TODO Produce better errors on merge failure.

      We will need to associate a SrcSpan with every Rex in order to
      do this.

-}

module Rex.Parser
    -- (parseBlock, formToRex)
where

import PlunderPrelude     hiding (bracket, cons, many, some, try)
import Rex.Print
import Rex.Lexer as L

import Prelude (foldl1)

import qualified Rex.Types as R

-- Basic Concepts --------------------------------------------------------------

type Parser = Either Text

data TFrag
  = TFORM Form
  | TRUNE Text
  | TLINE Bool Text
 deriving (Show)

data Eye = I Text [R.Rex] (Maybe R.Rex)
         | T Bool Text    (Maybe R.Rex)
  deriving (Show)

class Rexy a where
  toRex :: a -> R.Rex

instance Rexy Eye where
  toRex (I t x k) = R.N R.OPEN t (reverse x) k
  toRex (T f t k) = R.T (if f then R.THIC_LINE else R.THIN_LINE) t k

instance Rexy TFrag where
  toRex (TRUNE rune)    = R.N R.OPEN rune [] Nothing
  toRex (TFORM wide)    = formToRex wide
  toRex (TLINE True t)  = R.T R.THIC_LINE t Nothing
  toRex (TLINE False t) = R.T R.THIN_LINE t Nothing


-- Converting Forms into Runic Trees -------------------------------------------

leaf :: Leaf -> R.Leaf
leaf = \case
    N n       -> (R.BARE_WORD, n)
    C True c  -> (R.THIC_CORD, c)
    C False c -> (R.THIN_CORD, c)

formToRex :: Form -> R.Rex
formToRex = form
 where
  form :: Form -> R.Rex
  form (BEFO ru bod) = rn R.SHUT_PREFIX ru [form bod]
  form (SHIN i [])   = itmz i
  form (SHIN i is)   = R.C (R.AS (itmz i) (over _2 itmz <$> is)) Nothing

  nest :: Nest -> R.Rex
  nest (WRAPD f)    = form f
  nest (PREFX r fs) = rn R.NEST_PREFIX r (form <$> fs)
  nest (INFIX w []) = goApp w
  nest (INFIX w ws) = R.C (R.AN (goApp w) (over _2 goApp <$> ws)) Nothing

  goApp :: [Form] -> R.Rex
  goApp = form . \case [x] -> x
                       xs  -> SHIN (NEST (PREFX "|" xs) :| []) []

  itmz :: Itmz -> R.Rex
  itmz (i :| [])   = item i
  itmz (i :| k:ks) = rexAddCont (item i) (itmz (k:|ks))

  item :: Item -> R.Rex
  item (LEAF (N t))       = R.T R.BARE_WORD t Nothing
  item (LEAF (C True t))  = R.T R.THIC_CORD t Nothing
  item (LEAF (C False t)) = R.T R.THIN_CORD t Nothing
  item (NEST n)           = nest n

  rn m r cs = R.N m r cs Nothing

  rexAddCont :: R.GRex v -> R.GRex v -> R.GRex v
  rexAddCont (R.T s t Nothing) c    = R.T s t (Just c)
  rexAddCont (R.T s t (Just k)) c   = R.T s t (Just $ rexAddCont k c)
  rexAddCont (R.N m r x Nothing) c  = R.N m r x (Just c)
  rexAddCont (R.N m r x (Just k)) c = R.N m r x (Just $ rexAddCont k c)
  rexAddCont (R.C x Nothing) c      = R.C x (Just c)
  rexAddCont (R.C x (Just k)) c     = R.C x (Just $ rexAddCont k c)


-- Reducing the Eye Stack -----------------------------------------------------

-- | Marks unreachable code corresponding to violation of item stack invariants:
--     - stack must be nonempty
--     - if i is tipwards of j, pos i >= pos j
impossible :: String -> a
impossible str = error ("Impossible: " <> str)

type Tok = (Int, TFrag)

toTFrag :: Frag -> TFrag
toTFrag (RUNE r) = TRUNE r
toTFrag (FORM f) = TFORM f
toTFrag (PAGE th p) = TLINE th p

tokenize :: [(Int, Frag)] -> [Tok]
tokenize = fmap (over _2 toTFrag)

-- | Add a rex value into an Eye, either as a parameter or as a
-- continuation, depending on depth.
--
-- This cannot merge a less-deeply nested item into a more-deeply nested
-- one, the caller must maintain that invariant.
--
-- It is invalid for line-strings to have child-nodes, that's the only
-- input that we handle but reject.
--
--     """ Hello, hello
--         """ Yeah, that's right?
--         """ What're you going to do about it?
--
-- There is a situation where we are adding a continuation to a form
-- that already has a continuation, for example:
--
--     | x
--     y
--     z
--
-- This can only happen with closed forms, so we just choose the last
-- same-depth form as the continuation and interpret all of the
-- same-depth forms before that as arguments:
--
--     | x y
--     z
--
merge :: (Show a, Rexy a) => (Int, a) -> (Int, Eye) -> Parser (Int, Eye)
merge (rp,r) (ip,i) =
  case (compare rp ip, i) of
    (LT , _              ) -> impossible "invalid merge"
    (_  , T _  _ Just{}  ) -> Left "Line strings cannot have child nodes."
    (GT , T _  _ Nothing ) -> Left "Line strings cannot have child nodes."
    (EQ , T th t Nothing ) -> pure (ip, T th t (Just $ toRex r))
    (_  , I t cs (Just k)) -> pure (ip, I t (k:cs) (Just $ toRex r))
    (EQ , I t cs Nothing ) -> pure (ip, I t cs (Just $ toRex r))
    (GT , I t cs Nothing ) -> pure (ip, I t (toRex r:cs) Nothing)

dent :: Text -> Text
dent = unlines . fmap dentLine . lines
 where dentLine "" = ""
       dentLine ln = "    " <> ln

-- | Reduce the stack until singleton or top item has position <= p.
--
-- If the first rune in the file is indented less deeply than subsequent
-- runes, that cannot be closed out, so we reject the input.
closeOut :: Int -> TFrag -> [(Int, Eye)] -> Parser [(Int, Eye)]
closeOut _ _ []                   = pure []
closeOut p _ (i:is)  | p >= fst i = pure (i:is)
closeOut p f (i:j:k)              = do ij <- merge i j
                                       closeOut p f (ij:k)
closeOut p f [i]                  = Left $ unlines
    [ "First item in file:\n"
    , dent $ rexFile $ toRex (snd i)
    , "Indented more than fragment:\n"
    , dent $ rexFile $ toRex f
    , dent ("(which is at position " <> tshow p <> ")")
    ]

{-
  Pushes a fragment onto the item stack.
  - Runes are simply pushed onto the stack as empty items.
  - Forms are merged into the head of the stack.
-}
pushOnto :: [(Int, Eye)] -> (Int, TFrag) -> Parser [(Int, Eye)]
pushOnto stack (fp,f) = do
  stc <- closeOut fp f stack
  case (f, stc) of
    (TFORM _, [])   -> impossible "Just-Form case already handled in `rush`"
    (TLINE{}, [])   -> impossible "Just-Page case already handled in `rush`"
    (TRUNE r, is)   -> pure ((fp, I r [] Nothing) : is)
    (TLINE s l, is) -> pure ((fp, T s l Nothing) : is)
    (TFORM _, i:is) -> (:is) <$> merge (fp,f) i

-- | Given a list of fragments in parse order, produce a Rex.
rush :: [(Int, TFrag)] -> Parser (Maybe R.Rex)
rush = \case
  []                  -> Right Nothing --- TODO This one is an internal
                                       --- error, basically.  The block
                                       --- parser should just not give
                                       --- it to us.
  (p, TRUNE r)   : fs -> fmap (Just . toRex . snd . foldl1 forceMerge)
                              (pushAll (p, I r [] Nothing) fs)
  (p, TLINE th l) : fs -> fmap (Just . toRex . snd . foldl1 forceMerge)
                              (pushAll (p, T th l Nothing) fs)
  (_, w@TFORM{}) : [] -> pure $ Just $ toRex w
  (_, TFORM{})   : _  -> Left oneForm

 where
  oneForm = "Blocks starting with closed forms may only contain one form."

  pushAll :: (Int, Eye) -> [(Int, TFrag)] -> Parser [(Int, Eye)]
  pushAll pf fs = foldlM pushOnto [pf] fs

  forceMerge :: (Int, Eye) -> (Int, Eye) -> (Int, Eye)
  forceMerge a b = either (impossible . unpack) id (merge a b)


-- Entry Point -----------------------------------------------------------------

parseBlock :: [[(Int, Frag)]] -> Either Text (Maybe R.Rex)
parseBlock =
    \top -> do
        -- for_ top (traceM . show)
        (go . fmap tokenize) top
  where
    go = \case
        [] : fs -> go fs
--      [ln]    -> flatten ln >>= lineHack
--      fs      -> flatten (concat fs) >>= rush
        [ln]    -> lineHack ln
        fs      -> rush (concat fs)

    lineHack (a:b:cs) | isForm a = rush ((-1, TRUNE "|") : a : b : cs)
    lineHack cs       = rush cs

    isForm (_, TFORM _) = True
    isForm (_, _      ) = False
