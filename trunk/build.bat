@echo off

rem the next two line must be changed before run on your computer
set lazpath=D:\Alexx\Prog\FreePascal\Lazarus

set PATH=%lazpath%;%PATH%

rem You can execute this script with different parameters:
rem components - compiling components needed for DC
rem plugins - compiling all DC plugins
rem all - compiling components, plugins and DC
rem default - compiling DC only 
rem beta - compile in beta mode (using by default)
if not "%OS_TARGET%" == "" (
  set DC_ARCH=%DC_ARCH% --os=%OS_TARGET%
)
if not "%CPU_TARGET%" == "" (
  set DC_ARCH=%DC_ARCH% --cpu=%CPU_TARGET%
)

if "%1"=="components" ( call :components
) else (
if "%1"=="plugins" ( call :plugins
) else (
if "%1"=="beta" ( call :beta
) else (
if "%1"=="default" ( call :default
) else (
if "%1"=="nightly" ( call :nightly
) else (
if "%1"=="all" ( call :all
) else (
if "%1"=="" ( call :beta
) else (
  echo ERROR: Mode not defined: %1
  echo Available modes: components, plugins, default, nightly, all, beta
)))))))

pause
GOTO:EOF

:components
  call components\build.bat
GOTO:EOF

:plugins
  call plugins\build.bat
GOTO:EOF

:beta
  call :components
  call :plugins

  rem Build Double Commander
  call :replace_old
  lazbuild src\doublecmd.lpi --bm=beta %DC_ARCH%

  rem Build Dwarf LineInfo Extractor
  lazbuild tools\extractdwrflnfo.lpi

  rem Extract debug line info
  tools\extractdwrflnfo doublecmd.dbg
GOTO:EOF

:all
  call :components
  call :plugins
  call :default
GOTO:EOF

:default
  call :replace_old
  lazbuild src\doublecmd.lpi %DC_ARCH%
GOTO:EOF

:nightly
  call :replace_old
  lazbuild src\doublecmd.lpi --bm=nightly %DC_ARCH%
GOTO:EOF

:replace_old
  del /Q doublecmd.exe.old
  ren doublecmd.exe doublecmd.exe.old
GOTO:EOF
