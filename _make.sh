#!/bin/bash 

# if you compile first time you must have "lazres" tools
# to create it execute "$lazpath/lazbuild $lazpath/tools/lazres.lpi"
# and change variable "lazpath" and "lcl"
# after it execute this script with parameter "all" at doublecmd dir 
# "./_make.sh all" it create lrs file and build doublecmd  
# and you can comment last line and uncomment next to last to use lazbuild
#                                                 by Attid


# You can execute this script with different parameters:
# components - compiling components needed for DC
# plugins - compiling all DC plugins
# doublecmd - compiling only DC without *.lrs files generation
# all - compiling components, plugins and DC with *.lrs files generation
# default - compiling DC with *.lrs files generation (using by default)

#path to lazarus
export lazpath=/home/alexx/bin/lazarus
#widgetset gtk or gtk2
if [ -z $lcl ]
  then export lcl=gtk2
fi

build_doublecmd()
{
  $lazpath/lazbuild --widgetset=$lcl src/doublecmd.lpi
  
  strip --strip-all doublecmd
}

build_default()
{
  for LFM in `ls ./src/*.lfm`
  do
    LRS=`echo $LFM | sed -e 's/\.lfm$/\.lrs/'`
    $lazpath/tools/lazres $LRS $LFM
  done
  build_doublecmd
}

build_all()
{
  components/build.sh
  plugins/build.sh
  build_default
  build_doublecmd
}


case $1 in
  components)  components/build.sh;;
     plugins)  plugins/build.sh;;
   doublecmd)  build_doublecmd;;
         all)  build_all;;
           *)  build_default;;
esac
