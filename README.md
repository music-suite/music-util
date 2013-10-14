
# music-util

A simple utility program `music` for working with the Music Suite <http://musicsuite.github.io>.

Here are some of the things it can do:

## Rebuilding parts of the suite 

This command reinstalls a specific library *and its dependencies* from the local sources.
 
    music install music-pitch

Withoug this utility, the developer would need to manually traverse and reinstall all dependencies whenever a low-level package is rebuilt. The dependency tree is hardcoded into the `music` program for now.

> Note: This is *not* a replacement for `cabal install`, it works on the source level and
needs cabal to operate properly.

## Generate documentation

This command generates and uploads API and reference documentation to the Music Suite web site:

    music document

## Run a command in each source directory

    music foreach git status

## Requirements

* [Haskell Platform](http://www.haskell.org/platform)

## Installation

    cabal configure
    cabal install

`music-util` assumes that all your local copies of the Music Suite repositories are in the same directory, and that this directory is the value of the environment variable `$MUSIC_SUITE_DIR`. You can se this in your `.profile` as follows:

    export MUSIC_SUITE_DIR = /example/directory