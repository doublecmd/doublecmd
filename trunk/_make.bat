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
for %%f in (src\*.lfm) do %lazpath%\Tools\lazres %%~nf.lrs %%f

:doublecmd
%lazpath%\lazbuild src\doublecmd.lpi

strip --strip-all doublecmd.exe