#!/bin/bash 

# if you compile first time you must change variable "lazpath" and "lcl"
# after it execute this script with parameter "all" at doublecmd dir 
# "./build.sh all" it build doublecmd
#                                                 by Attid


# You can execute this script with different parameters:
# components - compiling components needed for DC
# plugins - compiling all DC plugins
# all - compiling components, plugins and DC
# default - compiling DC only (using by default)

# path to lazbuild
export lazbuild=$(which lazbuild)

# Set up widgetset: gtk or gtk2 or qt
if [ $2 ]
  then export lcl=$2
fi
if [ $lcl ]
  then export DC_ARCH=$(echo "$DC_ARCH --widgetset=$lcl")
fi

# Set up processor architecture
if [ $CPU_TARGET ] 
  then export DC_ARCH=$(echo "$DC_ARCH --cpu=$CPU_TARGET")
fi

build_default()
{
  $lazbuild src/doublecmd.lpi $DC_ARCH
  
  strip --strip-all doublecmd
}

build_all()
{
  components/build.sh
  plugins/build.sh
  build_default
}


case $1 in
  components)  components/build.sh;;
     plugins)  plugins/build.sh;;
         all)  build_all;;
           *)  build_default;;
esac
