# league-champion-top10 (lctop)

lctop is a command line utility for retrieving the top 10 highest ranked players from op.gg champion enthusiasts pages. Players are sorted in descending order by rank, games played on champion and winrate, in that order.

## Installation

### Windows

Windows users can download the [latest precompiled binary](https://github.com/VeeEM/league-champion-top10/releases) and place it in some directory in %PATH%. The easiest way to build from source on Windows is by using the [Haskell Platform](https://www.haskell.org/platform). The Haskell Platform includes GHC and Cabal which are required to build this project.
 
### Building from source

To build and install lctop from source one needs to have GHC and Cabal installed. 
Installation can be as simple as running: 

```
cabal new-install
```

Windows users probably have to run something like the following, depending on how Cabal is configured.

```
cabal new-install --install-method=copy --installdir=C:\desired\install\dir
```

## Using lctop

lctop uses champion.json to know the names and id numbers of League of Legends champions. When using lctop for the first time one must start by running the update command.

```
lctop update
```

This downloads champion.json and saves it in the users home directory on Unix-like systems and in %APPDATA% on Windows. When League of Legends is updated with new champions, run the update command to be able to query top players for those new champions.

To view top10:
```
lctop REGION CHAMPION
```

Available regions:

- euw
- lan
- las
- na
- ru
- oce
- www
- tr
- eune
- br
- jp
