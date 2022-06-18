{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module SireExe
    ( main
    , showPlun
    )
where

import Plun.Print
import PlunderPrelude
import Rainbow
import Rex
import Sire
import Sire.Backend
import System.Directory

import Control.Monad.Except    (ExceptT, runExceptT)
import Control.Monad.State     (get, put)
import Control.Monad.State     (State, evalState, evalStateT)
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import Plun                    (pattern AT, (%%))
import Sire.Sugar              (desugarCmd, resugarRul, resugarVal)

import qualified Data.Map  as M
import qualified Data.Text as T
import qualified Plun      as P
import qualified Runner    as Run

--------------------------------------------------------------------------------

replActor :: MVar P.Val -> MVar (P.Val, P.Val) -> Run.Ship -> IO a
replActor mRequest mReply initShip =
    evalStateT loop initShip
  where
    loop = do
        -- putStrLn "replActor: READMVAR"
        reqVal <- takeMVar mRequest
        assign #noun (AT 0 %% reqVal)
        Run.execFx
        Run.getResponse' >>= putMVar mReply
        loop

main :: IO ()
main = do
    filz <- fmap unpack <$> getArgs

    plunActor <- do
        mReq <- newEmptyMVar
        mRsp <- newEmptyMVar

        (inq, ship) <- Run.newTopShip "sire" Nothing (AT 0)

        tid <- async (replActor mReq mRsp ship)

        atomically $ writeTVar Run.vShips
                   $ M.singleton "sire" (inq, tid)

        pure \fx -> do
            -- shown <- showPlun fx
            -- putStrLn ("FX: " <> shown)
            putMVar mReq fx
            -- putStrLn "put fx, waiting for response"
            takeMVar mRsp

    vEnv <- newIORef mempty
    vMac <- newIORef mempty
    writeIORef P.vShowPlun (pure . showPlun)
    modifyIORef' P.state \st -> st { P.stFast = P.jetMatch }

    for_ filz $ \p -> do
        replFile p (runBlockPlun False plunActor vEnv vMac)
    replStdin (runBlockPlun True plunActor vEnv vMac)

runBlockPlun
    :: Bool
     -> (P.Val -> IO (P.Val, P.Val))
    -> IORef (Map Text P.Val)
    -> IORef (Map Text P.Val)
    -> Block
    -> IO ()
runBlockPlun okErr actor vEnv vMac block =
  runExceptT (runBlock actor vEnv vMac block) >>= \case
        Right () -> pure ()
        Left err -> do
            liftIO $ putChunk
                   $ bold
                   $ fore red
                   $ chunk
                   $ dent ";;;" err
            unless okErr $ do
                error "EXITING"

-- TODO Implement shallow loads.

runBlock
    :: (Pln -> IO (Pln, Pln))
    -> IORef (Map Text Pln)
    -> IORef (Map Text Pln)
    -> Block
    -> ExceptT Text IO ()
runBlock actor vEnv vMacros (BLK _ _ eRes) = do
    rexed  <- liftEither eRes
    vgs    <- newIORef (0::Nat)
    env    <- readIORef vEnv
    eVl    <- pure (valPlun $ envVal env)
    mac    <- readIORef vMacros
    parsed <- liftEither (rexCmd (MacroEnv vgs eVl mac) rexed)
    let sugarFree = desugarCmd parsed
    env' <- runCmd actor env vMacros sugarFree
    writeIORef vEnv env'

showPin :: Text -> Val Text -> Text
showPin self =
    rexFileColor boldColoring . joinRex . \case
        LAW ln lt lb ->
            let XLAW t as b = resugarRul self (RUL ln lt lb)
                vl = hackup (bodRex b)
            in chooseMode vl
                 (N SHUT_INFIX ":=" [xtagApp t as, vl] Nothing)
                 (N OPEN       ":=" [xtagApp t as] (Just vl))
        v ->
            let vl = hackup (valRex (resugarVal v))
            in chooseMode vl
                 (N SHUT_INFIX ":=" [parens [nameRex self], vl] Nothing)
                 (N OPEN       ":=" [parens [nameRex self]] (Just vl))
  where
    hackup (N SHUT_INFIX "-" cs Nothing) = N NEST_PREFIX "|" cs Nothing
    hackup x                             = x

showAlias :: TextShape -> Text -> Val Text -> Text
showAlias shape bind vl =
    rexFileColor boldColoring (joinRex rx)
  where
    vr = valRex (resugarVal vl)
    rx = chooseMode vr
             (N SHUT_INFIX "/" [textRex shape bind, vr] Nothing)
             (N OPEN "/" [textRex shape bind] (Just vr))

chooseMode :: GRex a -> GRex a -> GRex a -> GRex a
chooseMode (N OPEN _ _ _) _    open = open
chooseMode _              wide _    = wide

printValue :: Bool -> Maybe (TextShape, Text) -> Pln -> ExceptT Text IO ()
printValue shallow mBinder vl = do
    let clz = (if shallow then plunShallow else plunClosure) vl
    putStrLn $ showClz mBinder clz

unsafeValueRex :: Pln -> GRex Rex
unsafeValueRex vl = unsafePerformIO $ do
    let NAMED_CLOSURE _nam _env val = nameClosure (plunShallow vl)
    let res = valRex (resugarVal val)
    pure (N OPEN "Δ" [res] Nothing)

showClz :: Maybe (TextShape, Text) -> Closure v -> Text
showClz mBinder clz =
    niceLns True $ fmap T.stripEnd (pins <> tops)
  where
    NAMED_CLOSURE nam env val = nameClosure clz

    pins = (flip mapMaybe $ toList nam) \n -> do
             lookup n env & \case
                 Nothing -> Nothing
                 Just vl -> Just (showPin n vl)

    tops = case (mBinder, val) of
             (Just (_,n), REF m) | m==n -> []
             (Just (s,n), _)            -> [showAlias s n val]
             (Nothing, REF _)           -> []
             (Nothing, _)               -> [showAlias BARE_WORD "_" val]

runCmd :: (Pln -> IO (Pln, Pln))
       -> Map Text Pln
       -> IORef (Map Text Pln)
       -> Cmd Pln Text Text
       -> ExceptT Text IO (Map Text Pln)
runCmd runFx scope vMacros = \case
    ANOTE _ _ -> do
        pure scope
    MKRUL n r -> do
        rul <- traverse (getRef scope) r
        let pln = P.mkPin (rulePlun rul)
        printValue True (Just (BARE_WORD, n)) pln
        pure $ insertMap n pln scope
    PRINT v -> do
        val <- injectExp scope v
        printValue True Nothing val
        pure $ insertMap ("_"::Text) val scope
    VOPEN ns x -> do
        -- TODO Use a less hacky approach to avoid this limiatation.
        for_ ns $ \n ->
            when (n == "_") do
                error "TODO Currently don't support `_` in top-level * binds"
        v <- injectExp scope x
        let s = insertMap "_" v scope
        let a = ALIAS $ zip [0..] ns <&> \(i,n) ->
                  (n, REF "idx" `APP` NAT i `APP` REF "_")
        runCmd runFx s vMacros a
    DUMPY v -> do
        pln <- injectExp scope v
        printValue False Nothing pln
        pure scope
    CHECK checks -> do
        for checks $ \(raw, v) -> do
            val <- injectExp scope v
            let NAMED_CLOSURE _ _ top = nameClosure (plunShallow val)
            unless (top == 1)
                $ throwError . rexFile
                $ N OPEN "!=" [ T BARE_WORD "1" Nothing
                              , (joinRex . valRex . resugarVal) top
                              ]
                $ Just . joinRex . expRex
                $ fmap rawRex raw
                    -- TODO Nope, definitly not impossible
        pure scope
    ALIAS [] -> pure scope
    ALIAS ((n,v):m) -> do
        val <- traverse (getRef scope) v
        pln <- pure (valPlun val)
        printValue True (Just (BARE_WORD,n)) pln
        let scope' = insertMap n pln scope
        runCmd runFx scope' vMacros (ALIAS m)
    DEFUN [] ->
        pure scope
    DEFUN ((nam, FUN _ _ [] e) : more) -> do
        pln <- injectExp scope e
        printValue True (Just (BARE_WORD,nam)) pln
        runCmd runFx (insertMap nam pln scope) vMacros (DEFUN more)
    DEFUN ((nam, f) : more) -> do
        fun <- traverse (getRef scope) (resolveTopFun f)
        pln <- P.mkPin <$> injectFun fun
        printValue True (Just (BARE_WORD,nam)) pln
        runCmd runFx (insertMap nam pln scope) vMacros (DEFUN more)
    SAVEV v   -> do
        pln <- injectExp scope v
        hom <- liftIO getHomeDirectory
        has <- plunSave (hom <> "/.sire") pln
        outBtc has >> outHex has
        pure scope
    IOEFF i r fx -> do
        pln <- injectExp scope fx
        printValue True (Just (BARE_WORD, "__FX__")) pln
        (rid, res) <- liftIO (runFx pln)
        printValue True (Just (BARE_WORD, i)) rid
        printValue True (Just (BARE_WORD, r)) res
        pure $ insertMap i rid $ insertMap r res $ scope
    MACRO runeTxt e -> do
        pln <- injectExp scope e
        printValue True (Just (THIC_CORD, runeTxt)) pln
        modifyIORef' vMacros (insertMap runeTxt pln)
        pure (insertMap runeTxt pln scope)
    EPLOD e -> do
        putStrLn "██████████"
        liftIO $ putChunk
               $ (bold . fore yellow . chunk)
               $ rexFile
               $ joinRex
               $ expRex
               $ fmap (fmap joinRex unsafeValueRex)
               $ e
        putStrLn "██████████"
        pure scope

rawRex :: Pln -> Rex
rawRex x =
    joinRex
        $ valRex
        $ resugarVal
        $ fmap P.valName
        $ plunVal x

envVal :: ∀a. Map Text a -> Val a
envVal =
    TAB . mapFromList . fmap f . mapToList
  where
    f :: (Text, a) -> (Nat, Val a)
    f (nm, v) = (utf8Nat nm, REF v)

getRef :: Map Text Pln -> Text -> ExceptT Text IO Pln
getRef env nam =
    maybe unresolved pure (lookup nam env)
  where
    unresolved = throwError ("Unresolved Reference: " <> nam)

injectExp :: Map Text Pln -> Exp Pln Text Text -> ExceptT Text IO Pln
injectExp scope ast = do
    fun <-
        traverse (getRef scope) $ flip evalState 0 $ do
            sel <- gensym "sel"
            arg <- gensym "arg"
            res <- resolveExp mempty ast
            pure (FUN sel (LN 0) [arg] res)

    v <- injectFun fun
    pure $ valPlun (APP (REF v) (NAT 0))

injectFun :: Fun Pln Refr Pln -> ExceptT Text IO Pln
injectFun (FUN self nam args exr) = do
    (_, b) <- expBod (nex, tab) exr
    let rul = RUL nam (fromIntegral ari) b
    pure (rulePlun rul)
  where
    ari = length args
    nex = succ ari
    tab = mapFromList $ zip (refrKey <$> (self:args))
                            ((0,) . BVAR <$> [0..])

numRefs :: Int -> Exp a Refr a -> Int
numRefs k = \case
    EVAR r                 -> if refrKey r == k then 1 else 0
    EBED{}                 -> 0
    EREF{}                 -> 0
    EHAZ{}                 -> 0
    ENAT{}                 -> 0
    EBAR{}                 -> 0
    ECOW{}                 -> 0
    ECAB{}                 -> 0
    ETAB ps                -> sum (go <$> ps)
    EVEC vs                -> sum (go <$> vs)
    EAPP f x               -> go f + go x
    EREC _ v b             -> go v + go b
    ELET _ v b             -> go v + go b
    ELAM (FUN _ _ _ b)     -> min 1 (go b)
    ELIN (FUN _ _ _ b)     -> min 1 (go b)
    --- Multiple references from a sub-functions only counts as one because
    --- it will be lambda-lifted (hence only used once).
    ECOR{}                 -> error "TODO: Macro expand [=*] first"
    EOPN{}                 -> error "TODO: Macro expand [*] first"
    EBAT{}                 -> error "TODO: Macro expand [*] first"
    EPAT{}                 -> error "TODO: Macro expand [?+] first"
  where
    go = numRefs k

optimizeLet
    :: (Int, IntMap (Int, Bod Pln))
    -> Refr
    -> Exp Pln Refr Pln
    -> Exp Pln Refr Pln
    -> ExceptT Text IO (Int, Bod Pln)
optimizeLet s@(nex, tab) refNam expr body = do
  let recurRef = numRefs k expr > 0
      multiRef = numRefs k body >= 2
  if
    trivialExp expr || (not recurRef && not multiRef)
  then do

    (varg, vv) <- expBod s expr
    let s' = (nex, insertMap k (varg, vv) tab)
    (barg, bb) <- expBod s' body
    let rarg = if (varg == 0) then 0 else barg
    pure (rarg, bb)
  else do
    let s' = (nex+1, insertMap k (0, var nex) tab)
    (varg, vv) <- expBod s' expr
    (barg, bb) <- expBod s' body
    let rarg = if (varg == 0 || barg == 0) then 0 else barg-1
    pure (rarg, BLET vv bb)
  where
    k = refrKey refNam
    var = BVAR . fromIntegral

-- Trivial if duplicating is cheaper than a let-reference.
trivialExp :: Exp v a b -> Bool
trivialExp = \case
    ENAT n -> n < 4294967296
    EBAR _ -> False
    EREF _ -> True
    EVAR _ -> True
    _      -> False

atomArity :: Nat -> Int
atomArity = fromIntegral . P.natArity

freeVars :: Fun a Refr a -> Set Refr
freeVars = goFun mempty
 where
    goFun :: Set Refr -> Fun a Refr a -> Set Refr
    goFun ours (FUN self _ args body) =
      let keyz = setFromList (self:args)
      in go (ours <> keyz) body

    go :: Set Refr -> Exp a Refr a -> Set Refr
    go ours = \case
        EBED{}     -> mempty
        ENAT{}     -> mempty
        EBAR{}     -> mempty
        EREF{}     -> mempty
        EHAZ{}     -> mempty
        ECOW{}     -> mempty
        ECAB{}     -> mempty
        EVAR r     -> if (r `elem` ours)
                      then mempty
                      else singleton r
        ELAM f     -> goFun ours f
        ELIN f     -> goFun ours f
        EAPP f x   -> go ours f <> go ours x
        EREC n v b -> let ours' = insertSet n ours
                      in go ours' v <> go ours' b
        ELET n v b -> let ours' = insertSet n ours
                      in go ours' v <> go ours' b
        ETAB ds    -> concat (go ours <$> toList ds)
        EVEC vs    -> concat (go ours <$> vs)
        ECOR{}     -> error "Macro Expand [=*] first."
        EOPN{}     -> error "Macro Expand [*] first."
        EBAT{}     -> error "Macro Expand [*] first."
        EPAT{}     -> error "Macro Expand [?+] first."

--  TODO Don't lift trivial aliases, just inline them (small atom, law)
--  TODO If we lifted anything, need to replace self-reference with a
--       new binding.
lambdaLift
    :: (Int, IntMap (Int, Bod Pln))
    -> Fun Pln Refr Pln
    -> ExceptT Text IO (Exp Pln Refr Pln)
lambdaLift _s f@(FUN self tag args body) = do
    let lifts = toList (freeVars f)
    let liftV = EVAR <$> lifts
    let self' = self { refrKey = 2348734 }
    let body' = EREC self (app (EVAR self') liftV)  body
    let funct = FUN self' tag (lifts <> args) body'
    pln <- injectFun funct
    pure $ app (EREF pln) liftV
  where
    app fn []     = fn
    app fn (x:xs) = app (EAPP fn x) xs

-- TODO This only looks at `EVAR`, would be much shorter to write using
-- `uniplate` or whatever.
inlineTrivial
    :: (b, IntMap (Int, Bod a))
    -> Fun a Refr a
    -> Fun a Refr a
inlineTrivial s@(_, tab) (FUN self tag args body) =
    FUN self tag args (go body)
  where
    goFun = inlineTrivial s
    go = \case
        EBED b     -> EBED b
        ENAT n     -> ENAT n
        EBAR b     -> EBAR b
        EREF r     -> EREF r
        EHAZ h     -> EHAZ h
        ECOW n     -> ECOW n
        ECAB k     -> ECAB k
        EVAR v     -> case lookup (refrKey v) tab of
                        Nothing                               -> EVAR v
                        Just (0, _)                           -> EVAR v
                        Just (_, BVAR{})                      -> EVAR v
                        Just (_, BCNS (REF x))                -> EREF x
                        Just (_, BCNS (NAT n)) | n<4294967296 -> ENAT n
                        _                                     -> EVAR v
        ELAM f     -> ELAM (goFun f)
        ELIN f     -> ELIN (goFun f)
        EAPP f x   -> EAPP (go f) (go x)
        EREC n v b -> EREC n (go v) (go b)
        ELET n v b -> ELET n (go v) (go b)
        ETAB ds    -> ETAB (go <$> ds)
        EVEC vs    -> EVEC (go <$> vs)
        EOPN v x b -> EOPN v (go x) (go b)
        EBAT t x b -> EBAT t (go x) (go b)
        ECOR n b f -> ECOR n (go b) (over _2 goFun <$> f)
        EPAT x f p -> EPAT x (go f) (over _2 go <$> p)

expBod
    :: (Int, IntMap (Int, Bod Pln))
    -> Exp Pln Refr Pln
    -> ExceptT Text IO (Int, Bod Pln)
expBod s@(_, tab) = \case
    EBED b         -> do let ari = P.trueArity b
                         pure (fromIntegral ari, BCNS (REF b))
    ENAT n         -> pure (atomArity n, BCNS (NAT n))
    EBAR n         -> pure (1, BCNS (BAR n))
    ELAM f         -> expBod s =<< lambdaLift s (inlineTrivial s f)
    ELIN f         -> expBod s =<< lambdaLift s (inlineTrivial s f)
                        -- TODO Inlining
    EVAR REFR{..}  -> pure $ fromMaybe (error "Internal Error")
                           $ lookup refrKey tab
    EREF t         -> pure ( fromIntegral (P.trueArity t)
                           , BCNS (REF t)
                           )
    EAPP f x       -> do fR <- expBod s f
                         xR <- expBod s x
                         pure $ case (fR, xR) of
                                 ((_, fv),     (0,xv)   )   -> (0,   BAPP fv xv)
                                 ((0, fv),     (_,xv)   )   -> (0,   BAPP fv xv)
                                 ((1, fv),     (_,xv)   )   -> (0,   BAPP fv xv)
                                 ((a, BCNS fk),(_,BCNS xk)) -> (a-1, BCNS (APP fk xk))
                                 ((a, fv),     (_,xv)   )   -> (a-1, BAPP fv xv)
    EREC n v b     -> optimizeLet s n v b
    ELET n v b     -> optimizeLet s n v b
    ETAB ds        -> doTab ds
    EVEC vs        -> doVec vs
    ECOW n         -> pure (fromIntegral n, BCNS (COW n)) -- TODO What arity?
    ECAB n         -> pure (length n, BCNS (CAB n))       -- TODO What arity?
    EHAZ haz       -> do
        hom <- liftIO getHomeDirectory
        pln <- plunLoad (hom <> "/.sire") haz
        arg <- pure (P.trueArity pln)
        pure (fromIntegral arg, BCNS (REF pln))
    EOPN{} -> error "Macro-expand [*] before this step"
    EBAT{} -> error "Macro-expand [*] before this step"
    ECOR{} -> error "Macro-expand [=*] before this step"
    EPAT{} -> error "Macro-expand [?+] before this step"

  where
    doVec vs = do
        es <- traverse (expBod s) vs

        let ex = vecApp (vecLaw $ fromIntegral $ succ $ length vs) (snd <$> es)

        pure $ if all (> 0) (fst <$> es)
               then (1, ex)
               else (0, ex)

    doTab :: Map Nat (Exp Pln Refr Pln) -> ExceptT Text IO (Int, Bod Pln)
    doTab ds = do
        let tups = M.toAscList ds
            keyz = fst <$> tups
            exps = snd <$> tups

        rs <- traverse (expBod s) exps

        let aris = fst <$> rs
            vals = snd <$> rs
            ex = vecApp (tabLaw keyz) vals
            ar = if all (> 0) aris then 1 else 0

        pure (ar, ex)

    tabLaw :: [Nat] -> Val a
    tabLaw ks = vecTmpl (LN 2) ar (valApp (vecLaw ar) (NAT <$> ks))
     where ar = fromIntegral (length ks + 1)

    vecLaw :: Nat -> Val a
    vecLaw ar = vecTmpl (LN 1) ar 0

    vecTmpl :: LawName -> Nat -> Val a -> Val a
    vecTmpl nm ar bd = NAT 0 `APP` NAT (lawNameNat nm)
                             `APP` NAT ar
                             `APP` bd

    vecApp :: Val a -> [Bod a] -> Bod a
    vecApp cnstr params = cnsApp cnstr params

    valApp :: Val a -> [Val a] -> Val a
    valApp f = \case []   -> f
                     x:xs -> valApp (APP f x) xs

    bodApp f = \case []   -> f
                     x:xs -> bodApp (BAPP f x) xs

    cnsApp :: Val a -> [Bod a] -> Bod a
    cnsApp f = \case []          -> BCNS f
                     BCNS x : xs -> cnsApp (APP f x) xs
                     x      : xs -> bodApp (BCNS f)   (x:xs)

-- Name Resolution -------------------------------------------------------------

data Refr = REFR
    { _refrName :: !Text
    , refrKey   :: !Int
    }
  deriving Show

instance Eq  Refr where (==)    x y = (==)    (refrKey x) (refrKey y)
instance Ord Refr where compare x y = compare (refrKey x) (refrKey y)

gensym :: Text -> State Int Refr
gensym nam = do
    key <- get
    put (key+1)
    pure (REFR nam key)

resolveFun :: Map Text Refr -> Fun v Text Text -> State Int (Fun v Refr Text)
resolveFun env (FUN self tag args body) = do
    selfR <- gensym self
    argsR <- traverse gensym args
    envir <- pure $ M.union (mapFromList $ zip (self:args) (selfR:argsR)) env
    bodyR <- resolveExp envir body
    pure (FUN selfR tag argsR bodyR)

resolveTopFun :: Fun v Text Text -> Fun v Refr Text
resolveTopFun f = evalState (resolveFun mempty f) 0

resolveExp :: Map Text Refr -> Exp a Text Text -> State Int (Exp a Refr Text)
resolveExp e = \case
    EBED b     -> pure (EBED b)
    ENAT n     -> pure (ENAT n)
    EBAR n     -> pure (EBAR n)
    EHAZ h     -> pure (EHAZ h)
    ECOW n     -> pure (ECOW n)
    ECAB n     -> pure (ECAB n)
    EREF r     -> case lookup r e of
                    Nothing -> pure (EREF r)
                    Just rf -> pure (EVAR rf)

    -- TODO Should this be `(absurd v)`?
    EVAR v     -> case lookup v e of
                    Nothing -> pure (EREF v)
                    Just rf -> pure (EVAR rf)

    EAPP f x   -> EAPP <$> go e f <*> go e x
    EVEC vs    -> EVEC <$> traverse (go e) vs
    ETAB ps    -> ETAB <$> traverse (go e) ps
    EREC n v b -> do r <- gensym n
                     let e2 = insertMap n r e
                     EREC r <$> go e2 v <*> go e2 b
    ELET n v b -> do r <- gensym n
                     let e2 = insertMap n r e
                     ELET r <$> go e v <*> go e2 b

    ELAM f     -> ELAM <$> resolveFun e f
    ELIN f     -> ELIN <$> resolveFun e f

    -- TODO Before we can delete this, we need to turn EPAT and ECOR
    -- into a proper macro too.
    EOPN v x b -> do
        vec <- gensym "vec"
        var <- traverse gensym v
        env <- pure $ mapFromList (zip v var) <> e
        exr <- go e x
        bod <- go env b

        let idxE i = foldl' EAPP (EHAZ P.idxHash) [ENAT i, EVAR vec]

        let bindSlots []         = bod
            bindSlots ((i,w):ws) = ELET w (idxE i) (bindSlots ws)

        pure $ ELET vec exr (bindSlots $ zip [0..] var)

    EBAT t x b -> do
        tab <- gensym "tab"
        var <- traverse gensym t
        env <- pure $ mapFromList (zip (toList t) (toList var))  <> e
        exr <- go e x
        bod <- go env b

        let tabIdxE k = foldl' EAPP (EHAZ P.tabIdxHash) [ENAT k, EVAR tab]

        let bindSlots []         = bod
            bindSlots ((i,w):ws) = ELET w (tabIdxE i) (bindSlots ws)

        pure $ ELET tab exr (bindSlots (mapToList var))

    EPAT x fb ps -> do
        let luslus :: [Text] -> Exp a Text Text -> Exp a Text Text
            luslus namz expr = (EOPN ("_" : namz) (EVAR "scrut") expr)
        let kz = keys ps
        case and (zipWith (==) [0..] kz) of
            True -> go e (ELET "scrut" x
                          (matchE (EREF "scrut") fb
                           (uncurry luslus <$> toList ps)))
            False -> go e (ELET "scrut" x
                           (tabMatchE (EREF "scrut") fb
                            (uncurry luslus <$> ps)))

    ECOR coreIdnt b fs -> do

      let coreTag = LN (utf8Nat coreIdnt)
          arms    = zip [0..] fs

      let bindArms :: Exp v Text Text
                   -> [(Nat, (Text, Fun v Text Text))]
                   -> Exp v Text Text
          bindArms bod []             = bod
          bindArms bod ((i,(n,f)):as) =
              let FUN idn _ arg _ = f
              in ELET n
                      (ELIN
                        (FUN idn (LN 0) arg
                          (EAPP (EVAR coreIdnt)
                                (EVEC (ENAT i : fmap EVAR arg)))))
                      (bindArms bod as)

      let branches  = arms <&> \(_, (_, FUN _ _ arg bod)) ->
                        EOPN ("_" : arg) (EVAR "xxx") bod

      let coreBody  = matchE (EVAR "xxx") (ENAT 0) branches

          {-
              | VCASE x
              , * {_ n} x
                | IF [ISZERO n] 1 [odd | DEC n]
              , * {_ n} x
                | IF [ISZERO n] 0 [even | DEC n]
          -}

      let coreFun = FUN coreIdnt coreTag ["xxx"]
                  $ bindArms coreBody arms
      go e $ ELET coreIdnt (ELAM coreFun) (bindArms b arms)
  where
    go = resolveExp

    matchE :: Exp v a b -> Exp v a b -> [Exp v a b] -> Exp v a b
    matchE x f ps = foldl' EAPP (EHAZ P.matchHash) [x, f, EVEC ps]

    tabMatchE :: Exp v a b -> Exp v a b -> Map Nat (Exp v a b) -> Exp v a b
    tabMatchE x f ps = foldl' EAPP (EHAZ P.tabMatchHash) [x, f, ETAB ps]


-- Plun Printer ----------------------------------------------------------------

showPlun :: P.Val -> Text
showPlun = showClz Nothing . plunShallow


-- Hacky Snapshot System -------------------------------------------------------

bsHex :: ByteString -> Text
bsHex = decodeUtf8 . ("%0x" <>) . toStrict . toLazyByteString . byteStringHex

outHex :: MonadIO m => ByteString -> m ()
outHex = liftIO . putChunkLn . bold . fore grey . chunk . bsHex

outBtc :: MonadIO m => ByteString -> m ()
outBtc = liftIO . putChunkLn . bold . fore yellow . chunk . encodeBtc

