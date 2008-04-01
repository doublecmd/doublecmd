rem the next line must be changed before run on your computer 
set lazpath=D:\Alexx\Prog\FreePascal\Lazarus

rem You can execute this script with different parameters:
rem components - compiling components needed for DC
rem plugins - compiling all DC plugins
rem doublecmd - compiling only DC without *.lrs files generation
rem all - compiling components, plugins and DC with *.lrs files generation
rem default - compiling DC with *.lrs files generation (using by default)

if "%1"=="components" components\build.bat
if "%1"=="plugins" plugins\build.bat
if "%1"=="doublecmd" goto doublecmd
if "%1"=="all" goto all
goto default

:all
call components\build.bat
call plugins\build.bat

:default
for %%f in (*.lfm) do %lazpath%\Tools\lazres %%~nf.lrs %%f

:doublecmd
fpc doublecmd.lpr -S2cdgi -OG3 -g -gl -vewnhi -l -Ficomponents\KASToolBar\ -Ficomponents\KASToolBar\lib\i386-win32\ -Ficomponents\viewer\ -Fu%lazpath%\components\images\lib\i386-win32\ -Fucomponents\KASToolBar\lib\i386-win32\ -Fu%lazpath%\components\synedit\units\i386-win32\ -Fu%lazpath%\lcl\units\i386-win32\ -Fu%lazpath%\lcl\units\i386-win32\win32\ -Fucomponents\viewer\lib\i386-win32\ -Fu%lazpath%\packager\units\i386-win32\ -Fu. -odoublecmd.exe -Fi%lazpath%\ide\