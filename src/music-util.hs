#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Shelly   
import Data.Monoid
import Data.Graph
import Control.Applicative
import Control.Exception(SomeException, try)
import Data.Text(Text)   
import Data.String(IsString, fromString)
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Graph as Graph
import qualified System.Environment as E
import qualified Distribution.PackageDescription as PackageDescription
import Distribution.Verbosity (silent)
import Distribution.PackageDescription.Parse (readPackageDescription)
import qualified Distribution.ModuleName as ModuleName
default (T.Text)
main = shelly $ verbosely $ main2

show' :: (Show a, IsString b) => a -> b
show' = fromString . show

getEnvOr :: String -> String -> IO String
getEnvOr n def = fmap (either (const def) id) $ (try (E.getEnv n) :: IO (Either SomeException String))

packages :: [String]
packages = [
        "abcnotation"               ,
        "musicxml2"                 ,
        "lilypond"                  ,
        "music-pitch-literal"       ,
        "music-dynamics-literal"    ,
        "music-score"               ,
        "music-pitch"               ,
        "music-dynamics"            ,
        "music-articulation"        ,
        "music-parts"               ,
        "music-preludes"            ,
        "music-graphics"            ,
        "music-sibelius"
        ]

dependencies :: (Graph, Vertex -> (String, String, [String]), String -> Maybe Vertex)
dependencies = Graph.graphFromEdges [
        ("", "reenact", []),

        ("", "music-dynamics-literal", []),
        ("", "music-pitch-literal", []),

        ("", "abcnotation", ["music-pitch-literal", "music-dynamics-literal"]),
        ("", "musicxml2", ["music-pitch-literal", "music-dynamics-literal"]),
        ("", "lilypond", ["music-pitch-literal", "music-dynamics-literal"]),

        ("", "music-pitch", ["music-pitch-literal"]),
        ("", "music-dynamics", ["music-dynamics-literal"]),
        ("", "music-articulation", []),
        ("", "music-parts", []),

        ("", "music-score", ["music-pitch-literal", "music-dynamics-literal",
            "abcnotation","lilypond","musicxml2"]),
            
        ("", "music-preludes", [
            "music-pitch", "music-dynamics", "music-articulation", "music-parts",
            "music-score"]),

        ("", "music-graphics", ["music-preludes"]),
        ("", "music-sibelius", ["music-preludes"])

        ]
(depGraph, getDepNode, getDepVertex) = dependencies

getPackageDeps :: String -> [String]
getPackageDeps name = let
    vertex = fromMaybe (error $ "Unknown package: " ++ name) $ getDepVertex name
    (_,_,children) = getDepNode vertex
    in [name] ++ concatMap getPackageDeps children


main2 :: Sh ()
main2 = do
    args <- liftIO $ E.getArgs
    path <- liftIO $ getEnvOr "MUSIC_SUITE_DIR" ""
    if path == "" then error "Needs $MUSIC_SUITE_DIR to be set" else return ()

    -- TODO check path
    
    -- echo $ fromString path
    chdir (fromString path) (main3 args)

main3 args = do    
    if length args <= 0 then usage else do
        if (args !! 0 == "document") then document args else return ()
        if (args !! 0 == "install") then install args else return ()
        if (args !! 0 == "list") then list args else return ()
        if (args !! 0 == "foreach") then forEach (tail args) else return ()

usage :: Sh ()
usage = do
    echo $ "usage: music <command> [<args>]"
    echo $ ""
    echo $ "When commands is one of:"
    echo $ "   install name       Reinstall the given package and its dependencies"
    echo $ "   foreach <command>  Run a command in each source directory"
    echo $ "   document           Generate and upload documentation"
    echo $ "                        --reinstall-transf  Reinstall the transf package"
    echo $ "                        --no-api            Skip creating the API documentation"
    echo $ "                        --no-reference      Skip creating the reference documentation"
    echo $ "                        --local             Skip uploading"
    echo $ "   list               List all packages in the Music Suite"
    echo ""
    

list :: [String] -> Sh ()
list _ = mapM_ (echo . fromString) packages

forEach :: [String] -> Sh ()
forEach cmdArgs = do
    mapM_ (forEach' cmdArgs) packages
    return ()
                          
forEach' :: [String] -> String -> Sh ()
forEach' []         _    = error "foreach: empty command list"
forEach' (cmd:args) name = do
    -- TODO check dir exists, otherwise return and warn
    echo "======================================================================"
    echo (fromString name)
    echo "======================================================================"
    chdir (fromString name) $ do
        run_ (fromString cmd) (fmap fromString args)

install :: [String] -> Sh ()
install (_:name:_) = do
    let all = List.nub $ reverse $ getPackageDeps name
    -- echo $ show' $ all

    echo "======================================================================"
    echo "Reinstalling the following packages:"
    mapM (\x -> echo $ "        " <> fromString x) all
    echo "======================================================================"

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
            echo ""
            echo "======================================================================"
            echo "Reinstalling transf"
            echo "======================================================================"
            reinstallTransf
        else return ()
    if (not flagNoApi)
        then do
            echo ""
            echo "======================================================================"
            echo "Making API documentation"
            echo "======================================================================"
            makeApiDocs
        else return ()
    if (not flagNoRef)
        then do
            echo ""
            echo "======================================================================"
            echo "Making reference documentation"
            echo "======================================================================"
            makeRef
        else return ()
    if (not flagLocal)
        then do 
            echo ""
            echo "======================================================================"
            echo "Uploading documentation"
            echo "======================================================================"
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
    
    hsFiles <- getHsFiles
    liftIO $ mapM_ print $ hsFiles
    
    mkdir_p "musicsuite.github.io/docs/api/src"
    
    let opts  = ["-h"::Text, "--odir=musicsuite.github.io/docs/api"]
    -- let opts1 = "--source-module=src/%{MODULE/./-}.html --source-entity=src/%{MODULE/./-}.html#%{NAME}"
    let opts1 = []
    let opts2 = ["--title=The\xA0Music\xA0Suite"] -- Must use nbsp

    run "haddock" (concat [opts, opts1, opts2, fmap unFilePath hsFiles])
    return ()    

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
