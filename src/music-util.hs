
{-# LANGUAGE OverloadedStrings, ViewPatterns, CPP #-}

import           Control.Applicative
import           Control.Exception                     (SomeException, try)
-- import           Data.Graph
-- import qualified Data.Graph                            as OG

import Data.Graph.Inductive hiding (run, run_)

#ifdef HAS_GRAPHVIZ
import Data.GraphViz
import Data.GraphViz.Attributes.Complete
#endif

import System.Process (system, runInteractiveCommand)
import Control.Monad
import qualified Data.List                             as List
import           Data.List.Split
import           Data.Map                              (Map)
import qualified Data.Map                              as Map
import           Data.Maybe
import           Data.Monoid
import           Data.String                           (IsString, fromString)
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import qualified Data.Text.Lazy                        as TL
import qualified Distribution.ModuleName               as ModuleName
import qualified Distribution.PackageDescription       as PackageDescription
import           Distribution.PackageDescription.Parse (readPackageDescription)
import           Distribution.Verbosity                (silent)
import           Shelly
import qualified System.Environment                    as E
import Data.Version (showVersion)
import qualified Paths_music_util                      as Paths
import System.IO (hGetContents)
default (T.Text)

main = shelly $ verbosely $ main2

show' :: (Show a, IsString b) => a -> b
show' = fromString . show

getEnvOr :: String -> String -> IO String
getEnvOr n def = fmap (either (const def) id) $ (try (E.getEnv n) :: IO (Either SomeException String))

replPackage :: String
replPackage = "music-preludes"

packages :: [String]
packages = labels dependencies

realPackages :: [String]
realPackages = packages `sans` "music-util" `sans` "music-docs"

dependencies :: Gr String String
dependencies = mkGraph
    [
        (-3,  "music-pitch-literal")       ,
        (-4,  "music-dynamics-literal")    ,
        (0,  "abcnotation")               ,
        (1,  "musicxml2")                 ,
        (2,  "lilypond")                  ,
        (5,  "music-score")               ,
        (6,  "music-pitch")               ,
        (7,  "music-dynamics")            ,
        (8,  "music-articulation")        ,
        (9,  "music-parts")               ,
        (10, "music-preludes")            ,
        (11, "music-graphics")            ,
        (12, "music-sibelius")            ,
        (90, "music-suite")               ,
        (99, "music-docs")
    ]
    [
        -- First value depends on second value
        (0, -3, ""),
        (0, -4, ""),
        (1, -3, ""),
        (1, -4, ""),
        (2, -3, ""),
        (2, -4, ""),
        (6, -3, ""),
        (7, -4, ""),

        (5, 0, ""),
        (5, 1, ""),
        (5, 2, ""),
        (5, -3, ""),
        (5, -4, ""),

        (10, 5, ""),
        (10, 6, ""),
        (10, 7, ""),
        (10, 8, ""),
        (10, 9, ""),

        (11, 10, ""),
        (12, 10, ""),

        (90, -3, ""),
        (90, -4, ""),
        (90, 0, ""),
        (90, 1, ""),
        (90, 2, ""),
        (90, 5, ""),
        (90, 6, ""),
        (90, 7, ""),
        (90, 8, ""),
        (90, 9, ""),
        (90, 10, "")
    ]


#ifdef HAS_GRAPHVIZ

dependencyParams :: GraphvizParams Int String String () String
dependencyParams = nonClusteredParams {
     globalAttributes = ga,
     fmtNode = fn,
     fmtEdge = fe
 }
 where
     ga = [
         GraphAttrs [],
         NodeAttrs []
         ]

     fn (n,l) = [(Label . StrLabel . TL.pack) l]
     fe (f,t,l) = [(Label . StrLabel . TL.pack) l]        

showDependencyGraph :: Sh ()
showDependencyGraph = do
    writefile "/tmp/deps.dot" (TL.toStrict $ printDotGraph dg)
    liftIO $ system "dot -Tpdf /tmp/deps.dot > music-suite-deps.pdf"
    liftIO $ system "open music-suite-deps.pdf"
    return ()
    where
        dg = graphToDot dependencyParams dependencies

#endif

getPackageDeps :: String -> [String]
getPackageDeps l = l : concatMap getPackageDeps children
    where
        children = getPackageDeps1 l

getPackageDeps1 :: String -> [String]
getPackageDeps1 label = depNames -- TODO recur
    where
        -- node id of given package
        node :: Node
        node = fromMaybe (error "Unknown package") $ fromLab label dependencies
        
        -- node ids of direct deps
        deps :: [Node]
        deps = suc dependencies node

        depNames :: [String]
        depNames = catMaybes $ fmap (lab dependencies) deps 




main2 :: Sh ()
main2 = do
    args <- liftIO $ E.getArgs
    path <- liftIO $ getEnvOr "MUSIC_SUITE_DIR" ""
    if path == "" then error "Needs $MUSIC_SUITE_DIR to be set" else return ()

    -- TODO check path

    -- echo $ fromString path
    chdir (fromString path) (main3 args)

main3 []              = usage
main3 (subCmd : args) =
    if length (subCmd : args) <= 0 
      then usage 
      else case subCmd of        
        "--version" -> printVersion args
        "repl"      -> repl args
        "document"  -> document args
        "install"   -> install args
        "list"      -> list args
        "graph"     -> graph args
        "foreach"   -> forEach args
        "setup"     -> setup args
        "package-path" -> packagePath args
        "help"      -> usage
        _           -> echo "Unknown command, try music-util help"
                                                                 
usage :: Sh ()
usage = do
    echo $ "usage: music-util <command> [<args>]"
    echo $ ""
    echo $ "Commands:"
    echo $ "   repl               Start a GHCI session with the development version of the suite"
    echo $ "   list               Show a list all packages in the Music Suite"
    echo $ "   graph              Show a graph all packages in the Music Suite (requires Graphviz)"
    echo $ "   setup              Download all packages and setup sandbox"
    echo $ "   setup clone          Download all packages"
    echo $ "   setup sandbox        Setup the sandbox"
    echo $ "   foreach <command>  Run a command in each source directory"
    echo $ "   package-path       Print a suitable GHC_PACKAGE_PATH value for use with runhaskell etc"
    echo $ "   document           Generate and upload documentation"
    echo $ "                        --reinstall-transf  Reinstall the transf package"
    echo $ "                        --no-api            Skip creating the API documentation"
    echo $ "                        --no-reference      Skip creating the reference documentation"
    echo $ "                        --local             Skip uploading"
    echo $ ""
    echo $ "Deprecated commands:"
    echo $ "   install <package>  Reinstall the given package and its dependencies"

printVersion :: [String] -> Sh ()
printVersion _ = do
    prg <- liftIO $ E.getProgName
    echo $ fromString prg <> ", version " <> fromString (showVersion Paths.version)

repl :: [String] -> Sh ()
repl _ = do
  path <- pwd
  liftIO (system $ "pushd " ++ unFilePath path ++ "/" ++ replPackage ++ " && cabal repl && popd")
  return ()

setup :: [String] -> Sh ()
setup ("clone":_)   = setupClone (return ())
setup ("sandbox":_) = setupSandbox
setup _             = setupClone setupSandbox

setupClone cont = do
    path <- pwd
    echo $ "Ready to setup music-suite sources in path\n    " <> unFilePath path
    echo $ ""
    echo $ "Please enter 'ok' to confirm..."
    conf <- liftIO $ getLine
    if conf /= "ok" then 
        echo "Aborted" 
    else do
        forM_ packages clonePackage
        cont
        return ()
    return ()

setupSandbox :: Sh ()
setupSandbox = hasCabalSandboxes >>= (`when` setupSandbox')

setupSandbox' = do
    rm_rf "music-sandbox"
    mkdir "music-sandbox"
    chdir "music-sandbox" $ do
        -- Create the sandbox
        run "cabal" ["sandbox", "init", "--sandbox", "."]
        -- Add all music-suite repos as sources to the sandbox 
        forM_ packages $ \p -> do
            run "cabal" ["sandbox", "add-source", "../" <> T.pack p]

    -- Tell cabal to use the sandbox in all music-suite packages.
    -- This is typically only needed for top-level packages such as music-preludes, but no
    -- harm in doing it in all packages.
    forM_ realPackages $ \p -> chdir (fromString p) $ do
        run "cabal" ["sandbox", "init", "--sandbox", "../music-sandbox"]
        run "cabal" ["install", "--only-dependencies"]
        run "cabal" ["configure"]
        -- run "cabal" ["install"]

    return ()

hasCabalSandboxes :: Sh Bool
hasCabalSandboxes = do
    cb <- run (fromString "cabal") [fromString "--version"]
    return $ List.isInfixOf "1.18" . head . lines. T.unpack $ cb


clonePackage :: String -> Sh ()
clonePackage name = do
    echo $ yellow $ "======================================================================"
    echo $ yellow $ fromString name
    liftIO $ system $ "git clone git@github.com:music-suite/" <> name <> ".git"
    return ()

packagePath :: [String] -> Sh ()
packagePath _ = liftIO $ getPackagePath >>= putStr

-- | TODO Hack , see <https://mappend.net/posts/ghc-and-cabal-sandbox-playing-ni>
getPackagePath :: IO String
getPackagePath = do
    (_, out, _, _) <- runInteractiveCommand $ "cd $MUSIC_SUITE_DIR/music-preludes; cabal sandbox hc-pkg list | grep \\: |  awk '{print NR,$0}' | sort -nr | sed 's/^[0-9]* //' | sed 's/://' | paste -d: - -"
    fmap (replace "\n" "") $ hGetContents out

getPrimaryPackagePath :: IO String
getPrimaryPackagePath = fmap (takeWhile (/= ':')) getPackagePath

list :: [String] -> Sh ()
list _ = mapM_ (echo . fromString) packages

graph :: [String] -> Sh ()
graph _ = do
#ifdef HAS_GRAPHVIZ
    showDependencyGraph
#else
    fail "music-util compiled without Graphviz support"
#endif

forEach :: [String] -> Sh ()
forEach cmdArgs = do
    mapM_ (forEach' cmdArgs) packages
    return ()

forEach' :: [String] -> String -> Sh ()
forEach' []         _    = error "foreach: empty command list"
forEach' (cmd:args) name = do
    -- TODO check dir exists, otherwise return and warn
    echo $ yellow $ "======================================================================"
    echo $ yellow $ fromString name
    chdir (fromString name) $ do
        run_ (fromString $ substName $ cmd) (fmap (fromString . substName) args)
    where
        substName = rep "MUSIC_PACKAGE" name

install :: [String] -> Sh ()
install (name:_) = do
    let all = List.nub $ reverse $ getPackageDeps name
    -- echo $ show' $ all

    echo $ yellow $ "======================================================================"
    echo $ yellow $ "Reinstalling the following packages:"
    mapM (\x -> echo $ "        " <> fromString x) all

    mapM reinstall all
    return ()


document :: [String] -> Sh ()
document args = do

    let flagReinstall   = "--reinstall-transf" `elem` args
    let flagNoApi       = "--no-api" `elem` args
    let flagNoRef       = "--no-reference" `elem` args
    let flagLocal       = "--local" `elem` args

    if (flagReinstall)
        then do
            echo $ yellow $ "======================================================================"
            echo $ yellow $ "Reinstalling transf"
            reinstallTransf
        else return ()
    if (not flagNoApi)
        then do
            echo $ yellow $ "======================================================================"
            echo $ yellow $ "Making API documentation"
            makeApiDocs
        else return ()
    if (not flagNoRef)
        then do
            echo $ yellow $ "======================================================================"
            echo $ yellow $ "Making reference documentation"
            makeRef
        else return ()
    if (not flagLocal)
        then do
            echo $ yellow $ "======================================================================"
            echo $ yellow $ "Uploading documentation"
            upload
        else return ()
    return ()


reinstallTransf :: Sh ()
reinstallTransf = do
    -- TODO check dir exists, otherwise return and warn
    chdir "transf" $ do
        run_ "cabal" ["clean"]
        run_ "cabal" ["configure"]
        run_ "cabal" ["install"]

reinstall :: String -> Sh ()
reinstall name = do
    -- TODO check dir exists, otherwise return and warn
    chdir (fromString name) $ do
        run_ "cabal" ["install",
          "--force-reinstalls",
          "--disable-documentation" -- speeds things up considerably
          ]


suffM :: Monad m => [Char] -> Shelly.FilePath -> m Bool
suffM s = (\p -> return $ List.isSuffixOf s (unFilePath p::String))

-- Real hack...
unFilePath :: IsString a => Shelly.FilePath -> a
unFilePath = fromString . read . drop (length ("FilePath "::String)) . show

getHsFiles :: Sh [Shelly.FilePath]
getHsFiles = do
    let srcPaths = fmap (<> "src") (fmap fromString packages)
    fmap concat $ mapM (findWhen (suffM ".hs")) srcPaths

{-
getHsFiles2 :: Sh [Shelly.FilePath]
getHsFiles2 = do
    let cabalFilePaths = fmap (\x -> x <> "/" <> x <> ".cabal") $ packages
    cabalDefs <- liftIO $ mapM (readPackageDescription silent) cabalFilePaths
    let cabalLibs = fmap (PackageDescription.condLibrary) $ cabalDefs
    let cabalPublicMods = map (condLibToModList . PackageDescription.condLibrary) $ cabalDefs
    let cabalPublicMods2 = zipWith (\package (fmap ModuleName.components -> mods) ->
            (\m -> (List.intercalate "/" $ [package, "src"] ++ m) ++ ".hs") `fmap` mods
            ) packages cabalPublicMods

    echo $ show' cabalPublicMods2
    return $ fmap fromString $ concat cabalPublicMods2
    where
        condLibToModList Nothing = []
        condLibToModList (Just (PackageDescription.CondNode (PackageDescription.Library ms _ _) _ _)) = ms
-}

{-
TODO works but wrong w.r.t. hslinks

makeApiDocs :: Sh ()
makeApiDocs = do
    home <- liftIO $ getEnvOr "HOME" ""
    let pdb = home ++ "/.ghc/x86_64-darwin-7.6.3/package.conf.d"
    run "standalone-haddock" $ ["-o", "musicsuite.github.io/docs/api", "--package-db", fromString pdb] ++ fmap fromString packages
    return ()
-}

makeApiDocs :: Sh ()
makeApiDocs = do
    mkdir_p "musicsuite.github.io/docs/api/src"

    path <- liftIO $ getEnvOr "MUSIC_SUITE_DIR" ""
    -- let packDbPath = fromString path <> "/music-sandbox/x86_64-osx-ghc-7.6.3-packages.conf.d"
    packDbPath <- liftIO (fmap fromString getPrimaryPackagePath)
    
    -- TODO generate this
    let hyperl = ["--hyperlink-source"]
    let packDb = ["--package-db"::Text, packDbPath]
    let out    = ["-o", fromString path <> "/musicsuite.github.io/docs/api"]
    run "standalone-haddock" $ concat [hyperl, packDb, out, fmap fromString realPackages]
    return ()

    -- hsFiles <- getHsFiles
    -- liftIO $ mapM_ print $ hsFiles
    -- 
    -- mkdir_p "musicsuite.github.io/docs/api/src"
    -- 
    -- let opts  = ["-h"::Text, "--odir=musicsuite.github.io/docs/api"]
    -- -- let opts1 = "--source-module=src/%{MODULE/./-}.html --source-entity=src/%{MODULE/./-}.html#%{NAME}"
    -- let opts1 = []
    -- let opts2 = ["--title=The\xA0Music\xA0Suite"] -- Must use nbsp
    -- 
    -- run "haddock" (concat [opts, opts1, opts2, fmap unFilePath hsFiles])
    -- return ()

makeRef :: Sh ()
makeRef = do
    -- TODO check dir exists, otherwise return and warn
    chdir "music-docs" $ do
        run_ "make" ["html"]

    -- cp_r "music-docs/build/" "musicsuite.github.io/docs/ref"
    run_ "cp" ["-r", "-f", "music-docs/build/", "musicsuite.github.io/docs/ref"]

appendNL path = do
    c <- readfile path
    writefile path (c <> "\n")

upload :: Sh ()
upload = do
    chdir "musicsuite.github.io" $ do

        -- Hack: Append a newline to a file so that git commands never fail
        appendNL "docs/ref/index.html"

        run "git" ["add", "docs/api"]
        run "git" ["add", "docs/ref"]
        run "git" ["commit", "-m", "Documentation"]
        run "git" ["push"]
    return ()


-- TODO move to fgl
-- | Get all labels
labels :: Graph gr => gr a b -> [a]
labels gr = catMaybes . fmap (lab gr) . nodes $ gr

-- | Get node from label
fromLab :: Eq a => Graph gr => a -> gr a b -> Maybe Node
fromLab l' = getFirst . ufold (\(_,n,l,_) -> if l == l' then (<> First (Just n)) else id) mempty


rep :: Eq a => [a] -> [a] -> [a] -> [a]
rep a b s@(x:xs) = if List.isPrefixOf a s
                     then b++rep a b (drop (length a) s)
                     else x:rep a b xs
rep _ _ [] = []

xs `sans` x = filter (/= x) xs


red    s = "\x1b[31m" <> s <> "\x1b[0m"
green  s = "\x1b[32m" <> s <> "\x1b[0m"
yellow s = "\x1b[33m" <> s <> "\x1b[0m"

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new = List.intercalate new . splitOn old

