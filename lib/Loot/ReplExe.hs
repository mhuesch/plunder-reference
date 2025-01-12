{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Loot.ReplExe
    ( main
    , replMain
    , showPlun
    , plunRex
    , runBlock
    , printValue
    , renderValue
    , replFile
    , showClosure
    )
where

import Loot
import Loot.Backend
import Plun.Print
import PlunderPrelude
import Rex

import qualified Rex.Print.Prim

import Control.Exception (throw)
import Data.Text.IO      (hPutStr, hPutStrLn)
import Loot.Sugar        (desugarCmd, resugarRul, resugarVal)

import qualified Data.Text as T
import qualified Plun      as P

--------------------------------------------------------------------------------

main :: IO ()
main = do
    filz <- fmap unpack <$> getArgs
    colorsOnlyInTerminal (replMain filz)

replMain :: RexColor => [String] -> IO ()
replMain filz = do

    -- TODO Can probably hard-code these?
    writeIORef P.vShowPlun (pure . showPlun)
    modifyIORef' P.state \st -> st { P.stFast = P.jetMatch }

    vEnv <- newIORef (mempty :: Map Symb Fan)
    for_ filz (\p -> replFile p (runBlock stdout False vEnv))
    welcome stdout
    replStdin (runBlock stdout True vEnv)

welcome :: RexColor => Handle -> IO ()
welcome h = out txt
  where
    out = hPutStr h . Rex.Print.Prim.welcomeComment

    txt = unlines
        [ ";"
        , "; ==== Loot REPL ===="
        , ";"
        , "; Since input is multi-line, there is currently no input-prompt."
        , "; Just type away!"
        , ";"
        , ""
        ]

showError :: RexColor => Handle -> Bool -> Text -> IO ()
showError h tolerateErrors err = do
    liftIO $ hPutStrLn h $ Rex.Print.Prim.errorComment $ dent ";;;" err
    unless tolerateErrors (error "EXITING")

newtype ReplErr = REPL_ERR Text
  deriving newtype (Show)
  deriving anyclass (Exception)

replErr :: Text -> IO a
replErr = throw . REPL_ERR

runBlock
    :: RexColor
    => Handle -> Bool -> IORef (Map Symb Fan) -> Block -> IO ()
runBlock h okErr vEnv (BLK _ _ eRes) = do
    let onErr (REPL_ERR txt) = showError stderr okErr txt
    handle onErr do
        rexed  <- either replErr pure eRes
        env    <- readIORef vEnv
        parsed <- either replErr pure (rexCmd rexed)
        bitter <- pure (let ?env=env in desugarCmd parsed)
        runCmd h vEnv bitter

showPin :: RexColor => Symb -> ByteString -> Val Symb -> Text
showPin self _pinKey =
    rexFile . joinRex . showIt
  where
    hackup (N _ SHUT_INFIX "-" cs Nothing) = N 0 NEST_PREFIX "|" cs Nothing
    hackup x                               = x

    showIt (LAW ln lt lb) =
        let (t,as,b) = case resugarRul mempty self (RUL ln lt lb) of
                         XLAW t_ as_ b_ -> (t_, as_, b_)
                         _              -> error "impossible"
            vl = hackup (bodRex b)
        in chooseMode vl
             (\vl2 -> absurd<$>(N 0 SHUT_INFIX "=" [xtagApp t as, joinRex vl2] Nothing))
             (\vl2 -> absurd<$>(N 0 OPEN       "=" [xtagApp t as] (Just $ joinRex vl2)))

    showIt v =
        let vl = hackup (valRex (resugarVal mempty v))
        in chooseMode vl
             (\vl2 -> absurd<$>(N 0 SHUT_INFIX "=" [parens [keyRex self], joinRex vl2] Nothing))
             (\vl2 -> absurd<$>(N 0 OPEN       "=" [parens [keyRex self]] (Just $ joinRex vl2)))

plunRex :: Fan -> Rex
plunRex pln = joinRex $ valRex (resugarVal mempty val)
  where
    clz = loadShallow pln
    NAMED_CLOSURE _ _ val = nameClosure clz

-- TODO Can I just rexFile a Loot command?  I remember I was doing this
-- before, but I don't remember why I stopped.
showAlias :: RexColor => Maybe Symb -> Val Symb -> Text
showAlias mSymb vl =
    rexFile (joinRex rx)
  where
    vr = valRex (resugarVal mempty vl)
    rx = case mSymb of
             Nothing   -> chooseMode vr id id
             Just symb -> chooseMode vr
                 (\vr2 -> absurd<$>(N 0 SHUT_INFIX "=" [keyRex symb, joinRex vr2] Nothing))
                 (\vr2 -> absurd<$>(N 0 OPEN "=" [keyRex symb] (Just $ joinRex vr2)))

-- TODO Jank AF.  Much hack.
chooseMode :: GRex a -> (GRex a -> GRex a) -> (GRex a -> GRex a) -> GRex a
chooseMode vr@(N 0 OPEN _ _ _)          _    open = open vr
chooseMode    (N 0 SHUT_INFIX "-" k h)  wide _    = wide (N 0 NEST_PREFIX "|" k h)
chooseMode vr@_                         wide _    = wide vr

printValue
    :: RexColor
    => Handle -> Bool -> Maybe Symb -> Fan -> IO ()
printValue h shallow mBinder vl = do
    let clz = (if shallow then loadShallow else loadClosure) vl
    hPutStrLn h $ showClosure mBinder clz

renderValue :: RexColor => Bool -> Maybe Symb -> Fan -> Text
renderValue shallow mBinder vl =
    let clz = (if shallow then loadShallow else loadClosure) vl
    in showClosure mBinder clz

showClosure :: RexColor => Maybe Symb -> Closure -> Text
showClosure mBinder clz =
    niceLns True $ fmap T.stripEnd (pins <> tops)
  where
    NAMED_CLOSURE nam env val = nameClosure clz

    pins = (flip mapMaybe $ toList nam) \n -> do
             lookup n env & \case
                 Nothing      -> Nothing
                 Just (vl, h) -> Just (showPin n h vl)

    tops = case (pins, mBinder, val) of
             (_, Just n, REF m) | m==n -> []
             (_, Just n, _)            -> [showAlias (Just n) val]
             (_, Nothing, REF _)       -> []
             ([],Nothing, _)           -> [showAlias Nothing val]
             (_, Nothing, _)           -> [showAlias (Just cab) val]


cab :: Symb
cab = utf8Nat "_"

resolveWith
    :: (Show k, Ord k, Traversable f)
    => IORef (Map k Fan) -> f k -> IO (f Fan)
resolveWith vEnv obj = do
    env <- readIORef vEnv
    for obj (\k -> maybe (onErr k) pure (lookup k env))
  where
    onErr = throw . REPL_ERR . ("Unresolved Reference: " <>) . tshow

runCmd
    :: RexColor
    => Handle -> IORef (Map Symb Fan) -> Cmd Fan Symb Symb -> IO ()
runCmd h vEnv =
    go
  where
    resolve :: Traversable f => f Symb -> IO (f Fan)
    resolve = resolveWith vEnv

    go :: Cmd Fan Symb Symb -> IO ()

    go (DEFINE ds) = do
        for_ ds \case
            BIND_PL n r -> do
                rul <- resolve r
                let pln = P.mkPin (rulePlunRaw rul)
                printValue h True (Just n) pln
                modifyIORef' vEnv (insertMap n pln)
            BIND_VL n v -> do
                val <- resolve v
                pln <- pure (valPlun val)
                printValue h True (Just n) pln
                modifyIORef' vEnv (insertMap n pln)
            BIND_PN n v -> do
                val <- resolve v
                pln <- pure (P.mkPin $ valPlun val)
                printValue h True (Just n) pln
                modifyIORef' vEnv (insertMap n pln)

    go (OUTPUT v) = do
        val <- resolve v
        let pln = valPlun val
        printValue h True Nothing pln
        modifyIORef' vEnv (insertMap cab pln)

    go (DUMPIT v) = do
        val <- resolve v
        let pln = valPlun val
        printValue h False Nothing pln

    go (ASSERT checks) = do
        for_ checks $ \(raw, v) -> do
            val <- resolve v
            let pln = valPlun val
            let NAMED_CLOSURE _ _ top = nameClosure (loadShallow pln)
            unless (top == 1) do
                let heir = joinRex $ valRex raw
                let oneV = T 0 BARE_WORD "1" Nothing
                let expr = joinRex $ valRex $ resugarVal mempty top
                let errE = N 0 OPEN "!=" [oneV, expr] (Just heir)
                throw $ REPL_ERR $ rexFile errE


-- Plun Printer ----------------------------------------------------------------

showPlun :: Fan -> Text
showPlun =
    let ?rexColors = NoColors
    in showClosure Nothing . loadShallow
