#!/bin/sh 

set -e

# You can execute this script with different parameters:
# components - compiling components needed for doublecmd
# doublecmd - compiling doublecmd only (release mode)
# plugins - compiling all doublecmd plugins
# debug - compiling components, plugins and doublecmd (debug mode)
# release - compile in release mode (using by default)

# path to lazbuild
export lazbuild=$(which lazbuild)

# Set up widgetset: gtk2 or qt or qt5 or cocoa
# Set up processor architecture: i386 or x86_64
if [ $2 ]
  then export lcl=$2
fi
if [ $lcl ] && [ $CPU_TARGET ]
  then export DC_ARCH=$(echo "--widgetset=$lcl")" "$(echo "--cpu=$CPU_TARGET")
elif [ $lcl ]
  then export DC_ARCH=$(echo "--widgetset=$lcl")
elif [ $CPU_TARGET ]
  then export DC_ARCH=$(echo "--cpu=$CPU_TARGET")
fi

build_doublecmd()
{
  # Build Double Commander
  $lazbuild src/doublecmd.lpi --bm=release $DC_ARCH

  # Build Dwarf LineInfo Extractor
  $lazbuild tools/extractdwrflnfo.lpi

  # Extract debug line info
  chmod a+x tools/extractdwrflnfo
  if [ -f doublecmd.dSYM/Contents/Resources/DWARF/doublecmd ]; then
    mv -f doublecmd.dSYM/Contents/Resources/DWARF/doublecmd $(pwd)/doublecmd.dbg
  fi
  tools/extractdwrflnfo doublecmd.dbg

  # Strip debug info
  strip doublecmd
}

build_release()
{
  components/build.sh
  plugins/build.sh
  build_doublecmd
}

build_debug()
{
  components/build.sh
  plugins/build.sh

  # Build Double Commander
  $lazbuild src/doublecmd.lpi --bm=debug $DC_ARCH
}


case $1 in
  components)  components/build.sh;;
   doublecmd)  build_doublecmd;;
     plugins)  plugins/build.sh;;
       debug)  build_debug;;
           *)  build_release;;
esac
