
# music-util

A simple utility program `music` for working with the Music Suite <http://musicsuite.github.com>.

Here are some of the things it can do:

## Rebuilding parts of the suite 

This command reinstalls a specific library *and its dependencies* from the local sources.
 
    music install music-pitch

Withoug this utility, the developer would need to manually traverse and reinstall all dependencies whenever a low-level package is rebuilt. The dependency tree is hardcoded into the `music` program for now.

## Generate documentation

This command generates and uploads API and reference documentation to the Music Suite web site:

    music document



## Requirements

* [Haskell Platform](http://www.haskell.org/platform)

## Installation

    cabal configure
    cabal install
