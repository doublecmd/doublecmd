#!/bin/bash 

lazpath=/at/lazarus

fpc doublecmd.lpr -S2cdgi -OG3 -g -gl -vewnhi -l -Ficomponents/KASToolBar/ -Ficomponents/KASToolBar/lib/i386-linux/ -Ficomponents/viewer/ -Fu$lazpath/components/jpeg/lib/i386-linux/ -Fucomponents/KASToolBar/lib/i386-linux/ -Fu$lazpath/components/synedit/units/i386-linux/ -Fu$lazpath/lcl/units/i386-linux/ -Fu$lazpath/lcl/units/i386-linux/gtk/ -Fucomponents/viewer/lib/i386-linux/ -Fu$lazpath/packager/units/i386-linux/ -Fu. -odoublecmd -dLCL -dLCLgtk
