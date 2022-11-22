{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

{- | This module handles wide-mode rex printing. -}

{-
    TODO: Move these all back into Rex.Print and don't export them.

    TODO: Simply assume that all leaves are valid and print them without
    handling edge-cases.  The code avoid this should enforce.

    Or make this explicit using a ValidRex smart constructor, and move
    the transformations into this module.
-}

module Rex.Print.Prim
    ( RexBuilder(..), rbRun, rbText, rbChar, rbIntercalate
    , RexColorScheme(..)
    , RexColor
    , rexLine
    , rexLineBuilder
    , cRune
    , cOpen
    , cText
    , cNest
    , welcomeComment
    , errorComment
    )
where

import PlunderPrelude
import Rex.Types

import Rex.Lexer (isName)

import qualified Data.Text    as T
import qualified Text.Builder as TB


--------------------------------------------------------------------------------

data RexBuilder = RB
    { width :: !Int
    , chars :: !TB.Builder
    }

instance Semigroup RexBuilder where
    (<>) a b = RB (a.width + b.width) (a.chars <> b.chars)

instance Monoid RexBuilder where
    mempty = RB 0 mempty

instance IsString RexBuilder where
    fromString str = rbText (pack str)

instance Show RexBuilder where show = show . rbRun

rbText :: Text -> RexBuilder
rbText txt = RB (length txt) (TB.text txt)

rbChar :: Char -> RexBuilder
rbChar chr = RB 1 (TB.char chr)

rbRun :: RexBuilder -> Text
rbRun rb = TB.run rb.chars

rbIntercalate :: Text -> [RexBuilder] -> RexBuilder
rbIntercalate _ []  = mempty
rbIntercalate _ [x] = x
rbIntercalate t (x:y:z) = x <> rbText t <> rbIntercalate t (y:z)

--------------------------------------------------------------------------------

data RexColorScheme
    = NoColors
    | BoldColors
  deriving (Eq, Ord, Show)

-- TODO What's the difference between TB.Builder and TextBuilder?
data RexColoring = RC
    { rcOpen :: Text -> RexBuilder
    , rcRune :: Text -> RexBuilder
    , rcText :: RexBuilder -> RexBuilder
    , rcBare :: RexBuilder -> RexBuilder
    , rcNest :: RexBuilder -> RexBuilder
    , rcErro :: RexBuilder -> RexBuilder
    }

noColoring :: RexColoring
noColoring = RC
    { rcOpen = rbText
    , rcRune = rbText
    , rcText = id
    , rcBare = id
    , rcNest = id
    , rcErro = id
    }

{-
   boldColoring takes care not to increase the (.width) of text by
   coloring it.
-}
boldColoring :: RexColoring
boldColoring = RC
    { rcOpen = boldYellow . rbText
    , rcRune = \case
        rt | lightRune rt -> boldYellow (rbText rt)
        rt                -> yellow (rbText rt)
    , rcText = green
    , rcBare = id
    , rcNest = boldMagenta
    , rcErro = boldMagenta -- TODO Red
    }
  where
    esc code = "\x001b[" <> code <> "m"

    green t       = RB t.width (esc "32"   <> t.chars <> esc "0")
    yellow t      = RB t.width (esc "33"   <> t.chars <> esc "0")
    boldYellow  t = RB t.width (esc "33;1" <> t.chars <> esc "0")
    boldMagenta t = RB t.width (esc "35;1" <> t.chars <> esc "0")

    lightRune "-" = True
    lightRune "`" = True
    lightRune "." = True
    lightRune _   = False

rc :: RexColorScheme -> RexColoring
rc NoColors   = noColoring
rc BoldColors = boldColoring

type RexColor = (?rexColors :: RexColorScheme)

cNest :: RexColor => RexBuilder -> RexBuilder
cNest = rcNest where RC{..} = rc ?rexColors

cRune :: RexColor => Text -> RexBuilder
cRune rune = rcRune rune where RC{..} = rc ?rexColors

cOpen :: RexColor => Text -> RexBuilder
cOpen rune = rcOpen rune where RC{..} = rc ?rexColors

cText :: RexColor => RexBuilder -> RexBuilder
cText = rcText where RC{..} = rc ?rexColors

cErr :: RexColor => RexBuilder -> RexBuilder
cErr = rcErro where RC{..} = rc ?rexColors

cBare :: RexColor => RexBuilder -> RexBuilder
cBare = rcBare where RC{..} = rc ?rexColors

welcomeComment :: RexColor => Text -> Text
welcomeComment = rbRun . cText . rbText

errorComment :: RexColor => Text -> Text
errorComment = rbRun . cErr . rbText

-- Expression ------------------------------------------------------------------

{-
    This presumes that the input is not a LINE and that it is quote-safe.
-}
wideLeaf :: RexColor => TextShape -> Text -> RexBuilder
wideLeaf = curry \case
    (BARE_WORD, t) -> cBare (rbText t)
    (THIN_CORD, t) -> cText (cord '\'' '\'' t)
    (THIC_CORD, t) -> cText (cord '"'  '"'  t)
    (CURL_CORD, t) -> cText (cord '{'  '}'  t)
    (THIN_LINE, _) -> error "Impossible"
    (THIC_LINE, _) -> error "Impossible"
  where
    cord top end txt = rbChar top <> rbText txt <> rbChar end

{-
   TOOD Test this.
-}
fixWide :: TextShape -> Text -> Maybe Rex
fixWide THIN_LINE t = Just (T 0 THIN_CORD t Nothing)
fixWide THIC_LINE t = Just (T 0 THIC_CORD t Nothing)
fixWide CURL_CORD _ = Nothing
fixWide BARE_WORD t =
    if isName t
    then Nothing
    else Just (T 0 THIN_CORD t Nothing)


fixWide THIC_CORD t =
    case (elem '"' t, elem '\'' t) of
        (False, _) -> Nothing
        (_, False) -> Just (T 0 THIN_CORD t Nothing)
        (_, _)     -> let (x,qy) = T.breakOn "\"" t
                          y = drop 1 qy
                      in Just ( T 0 THIC_CORD x
                              $ Just
                              $ T 0 THIC_CORD y
                              $ Nothing
                              )

fixWide THIN_CORD t =
    case (elem '"' t, elem '\'' t) of
        (_, False) -> Nothing
        (False, _) -> Just (T 0 THIC_CORD t Nothing)
        (_, _)     -> let (x,qy) = T.breakOn "'" t
                          y = drop 1 qy
                      in Just (T 0 THIN_CORD x $ Just $ T 0 THIN_CORD y $ Nothing)

isShut :: Rex -> Bool
isShut (N _ SHUT_PREFIX  _ _ _) = True
isShut (N _ SHUT_INFIX   _ _ _) = True
isShut (C _ v _)                = absurd v
isShut _                        = False

rexLine :: RexColor => Rex -> Text
rexLine = rbRun . rexLineBuilder

{-
  TODO Some combinations of shut forms do not need to be wrapped.
-}
wrapRex :: RexColor => Rex -> RexBuilder
wrapRex x | isShut x = cNest "(" <> rexLineBuilder x <> cNest ")"
wrapRex x            = rexLineBuilder x

wrapHeir :: RexColor => Rex -> RexBuilder
wrapHeir x@(N _ SHUT_PREFIX _ _ _) = wrapRex x
wrapHeir x                         = rexLineBuilder x

barNest :: RexColor => [RexBuilder] -> RexBuilder
barNest [x] = parens [rbText "|", x]
barNest xs  = parens xs

parens :: RexColor => [RexBuilder] -> RexBuilder
parens xs = cNest "(" <> intercalate " " xs <> cNest ")"

rexLineBuilder :: RexColor => Rex -> RexBuilder
rexLineBuilder = go
 where

  go :: Rex -> RexBuilder
  go = \case
    T _ s t Nothing         -> case fixWide s t of
                                 Nothing -> wideLeaf s t
                                 Just rx -> go rx
    T _ s t (Just k)        -> go (T 0 s t Nothing) <> wrapHeir k
    N _ OPEN  r ps k        -> go (N 0 NEST_PREFIX r ps k)
    N _ s     r ps (Just k) -> wrapRex (N 0 s r ps Nothing) <> wrapHeir k
    C _ c _                 -> absurd c
    N _ s     r ps Nothing  ->
      case s of
        SHUT_PREFIX -> cRune r <> wrapRex (unsafeHead ps)
        SHUT_INFIX  -> intercalate (cRune r) (wrapRex <$> ps)
        NEST_INFIX  -> parens $ intersperse (cRune r) (infixApp <$> ps)
        NEST_PREFIX -> case r of
          "|" -> barNest (go <$> ps)
          "," -> brackets (go <$> ps)
          _   -> parens (cRune r : fmap go ps)

  brackets :: [RexBuilder] -> RexBuilder
  brackets xs = cNest "[" <> intercalate " " xs <> cNest "]"

  infixApp :: Rex -> RexBuilder
  infixApp x@T{}            = go x
  infixApp x@(C{})          = go x
  infixApp x@(N _ t r ps k) =
    if isApp
    then rbIntercalate " " (go <$> params)
    else go x
   where
    params = (ps <> toList k)
    isApp = case (params, r, t) of
              ([],     _,   _          ) -> False
              (_:_:_,  "|", NEST_PREFIX) -> True
              _                          -> False
