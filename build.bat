rem the next line must be changed before run on your computer 
set lazpath=D:\Alexx\Prog\FreePascal\Lazarus

rem You can execute this script with different parameters:
rem components - compiling components needed for DC
rem plugins - compiling all DC plugins
rem all - compiling components, plugins and DC
rem default - compiling DC only (using by default)

if not "%OS_TARGET%" == "" (
  set DC_ARCH=%DC_ARCH% --os=%OS_TARGET%
)
if not "%CPU_TARGET%" == "" (
  set DC_ARCH=%DC_ARCH% --cpu=%CPU_TARGET%
)

if "%1"=="components" components\build.bat
if "%1"=="plugins" plugins\build.bat
if "%1"=="all" goto all
goto default

:all
call components\build.bat
call plugins\build.bat

:default
%lazpath%\lazbuild src\doublecmd.lpi %DC_ARCH%

strip --strip-all doublecmd.exe
