#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}
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
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Graph as Graph
import qualified System.Environment as E
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
        "music-sibelius"
        ]

dependencies :: (Graph, Vertex -> (String, String, [String]), String -> Maybe Vertex)
dependencies = Graph.graphFromEdges [
        ("", "abcnotation", []),
        ("", "music-pitch-literal", []),
        ("", "music-pitch", ["music-pitch-literal"]),
        ("", "music-score", ["music-pitch-literal"]),
        ("", "music-preludes", ["music-pitch", "music-score"])
        ]
(depGraph, getDepNode, getDepVertex) = dependencies
fromJust (Just x) = x

getPackageDeps :: String -> [String]
getPackageDeps name = let
    vertex = fromJust $ getDepVertex name
    (_,_,children) = getDepNode vertex
    in [name] ++ concatMap getPackageDeps children

-- TODO Move down
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
    





main2 :: Sh ()
main2 = do
    args <- liftIO $ E.getArgs
    path <- liftIO $ getEnvOr "MUSIC_SUITE_DIR" "/usr/local/music"

    -- TODO check path
    
    -- echo $ fromString path
    chdir (fromString path) (main3 args)

main3 args = do    
    if length args <= 0 then usage else do
        if (args !! 0 == "doc") then doc args else return ()
        if (args !! 0 == "install") then install args else return ()

usage :: Sh ()
usage = do
    echo $ "usage: music-util <command> [<args>]"
    echo $ ""
    echo $ "The most commonly used git commands are:"
    echo $ "   install name     Reinstall the given package and its dependencies"
    echo $ "   doc              Generate and upload documentation"
    echo $ "                        --reinstall-transf"
    echo $ "                        --no-reinstall"
    echo $ "                        --no-api"
    echo $ "                        --no-ref"
    echo $ "                        --local"
    echo ""
    

doc :: [String] -> Sh ()
doc args = do
    
    let flagReinstall   = "--reinstall-transf" `elem` args
    let flagNoReinstall = "--no-reinstall" `elem` args
    let flagNoApi       = "--no-api" `elem` args
    let flagNoRef       = "--no-ref" `elem` args
    let flagLocal       = "--local" `elem` args
    
    if (flagReinstall && not flagNoReinstall)
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
        run_ "cabal" ["install"]

reinstall :: String -> Sh ()
reinstall name = do
    -- TODO check dir exists, otherwise return and warn
    chdir (fromString name) $ do
        run_ "cabal" ["install", "--force-reinstalls"]


suffM :: Monad m => [Char] -> Shelly.FilePath -> m Bool
suffM s = (\p -> return $ List.isSuffixOf s (unFilePath p::String))

unFilePath :: IsString a => Shelly.FilePath -> a
unFilePath = fromString . read . drop (length ("FilePath "::String)) . show

-- TODO bit of hack, we should really parse the cabal files
makeApiDocs :: Sh ()
makeApiDocs = do        
    
    let srcPaths = fmap (<> "src") (fmap fromString packages)
    
    allHsFiles <- fmap concat $ mapM (findWhen (suffM ".hs")) srcPaths
    -- echo $ show' $ allHsFiles

    mkdir_p "musicsuite.github.io/docs/api/src"
    
    let opts  = ["-h"::Text, "--odir=musicsuite.github.io/docs/api"]
    -- let opts1 = "--source-module=src/%{MODULE/./-}.html --source-entity=src/%{MODULE/./-}.html#%{NAME}"
    let opts1 = []
    let opts2 = ["--title=The\xA0Music\xA0Suite"] -- Must use nbsp

    run "haddock" (concat [opts, opts1, opts2, fmap unFilePath allHsFiles])
    return ()    

makeRef :: Sh ()
makeRef = do
    -- TODO check dir exists, otherwise return and warn
    chdir "music-docs" $ do
        run_ "make" ["html"]
    -- rm_rf "musicsuite.github.io/docs/ref" -- cp_r does not overwrite
    cp_r "music-docs/build/" "musicsuite.github.io/docs/ref"

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
