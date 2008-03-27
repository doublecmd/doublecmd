#!/bin/bash 

# if you compile first time you must have "lazres" tools
# to create it execute "$lazpath/lazbuild $lazpath/tools/lazres.lpi"
# and change variable "lazpath" and "lcl"
# after it execute this script with parameter "all" at doublecmd dir 
# "./_make.sh all" it create lrs file and build doublecmd  
# and you can comment last line and uncomment next to last to use lazbuild
#                                                 by Attid
# ToDo recompile component when use parameter "all"

#path to lazarus
lazpath=/root/Prog/lazarus
#widgetset gtk or gtk2
lcl=gtk2

rm interface.inc
echo "{\$DEFINE $lcl}">interface.inc

if [ "$1" = "all" ]
then
  for LFM in `ls /at/project/doublecmd/*.lfm`
  do
    LRS=`echo $LFM | sed -e 's/\.lfm$/\.lrs/'`
    /home/at/lazarus/svn/tools/lazres $LRS $LFM
  done
fi

#$lazpath/lazbuild --widgetset=$lcl doublecmd.lpi
fpc doublecmd.lpr -S2cdgi -OG3 -g -gl -vewnhi -l -Ficomponents/KASToolBar/ -Ficomponents/KASToolBar/lib/i386-linux/ -Ficomponents/viewer/ -Fu$lazpath/components/jpeg/lib/i386-linux/ -Fucomponents/KASToolBar/lib/i386-linux/ -Fu$lazpath/components/synedit/units/i386-linux/ -Fu$lazpath/lcl/units/i386-linux/ -Fu$lazpath/lcl/units/i386-linux/$lcl/ -Fucomponents/viewer/lib/i386-linux/ -Fu$lazpath/packager/units/i386-linux/ -Fu. -odoublecmd -dLCL -dLCL$lcl
