rem the next line must be changed before run on your computer 
set lazpath=D:\Alexx\Prog\FreePascal\Lazarus
 
for %%f in (*.lfm) do %lazpath%\Tools\lazres %%~nf.lrs %%f
 
%lazpath%\lazbuild.exe components\KASToolBar\kascomp.lpk
%lazpath%\lazbuild.exe components\viewer\viewerpackage.lpk
 
fpc doublecmd.lpr -S2cdgi -OG3 -g -gl -vewnhi -l -Ficomponents\KASToolBar\ -Ficomponents\KASToolBar\lib\i386-win32\ -Ficomponents\viewer\ -Fu%lazpath%\components\images\lib\i386-win32\ -Fucomponents\KASToolBar\lib\i386-win32\ -Fu%lazpath%\components\synedit\units\i386-win32\ -Fu%lazpath%\lcl\units\i386-win32\ -Fu%lazpath%\lcl\units\i386-win32\win32\ -Fucomponents\viewer\lib\i386-win32\ -Fu%lazpath%\packager\units\i386-win32\ -Fu. -odoublecmd.exe -Fi%lazpath%\ide\