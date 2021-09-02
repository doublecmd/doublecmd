@echo off

rem Add Lazarus installation to path
if [%LAZARUS_HOME%] == [] set LAZARUS_HOME=D:\Alexx\Prog\FreePascal\Lazarus
set PATH=%LAZARUS_HOME%;%PATH%

rem You can execute this script with different parameters:
rem components - compiling components needed for doublecmd
rem doublecmd - compiling doublecmd only (release mode)
rem plugins - compiling all doublecmd plugins
rem debug - compiling components, plugins and doublecmd (debug mode)
rem release - compile in release mode (using by default)
if not "%OS_TARGET%" == "" (
  set DC_ARCH=%DC_ARCH% --os=%OS_TARGET%
)
if not "%CPU_TARGET%" == "" (
  set DC_ARCH=%DC_ARCH% --cpu=%CPU_TARGET%
)
if not "%LCL_PLATFORM%" == "" (
  set DC_ARCH=%DC_ARCH% --ws=%LCL_PLATFORM%
)

if "%1"=="components" ( call :components
) else (
if "%1"=="plugins" ( call :plugins
) else (
if "%1"=="beta" ( call :release
) else (
if "%1"=="doublecmd" ( call :doublecmd
) else (
if "%1"=="release" ( call :release
) else (
if "%1"=="debug" ( call :debug
) else (
if "%1"=="" ( call :release
) else (
  echo ERROR: Mode not defined: %1
  echo Available modes: components, plugins, doublecmd, release, debug
)))))))

pause
GOTO:EOF

:components
  call components\build.bat
GOTO:EOF

:plugins
  call plugins\build.bat
GOTO:EOF

:release
  call :components
  call :plugins
  call :doublecmd
GOTO:EOF

:debug
  call :components
  call :plugins

  rem Build Double Commander
  call :replace_old
  lazbuild src\doublecmd.lpi --bm=debug %DC_ARCH%
GOTO:EOF

:doublecmd
  rem Build Double Commander
  call :replace_old
  lazbuild src\doublecmd.lpi --bm=release %DC_ARCH%

  rem Build Dwarf LineInfo Extractor
  lazbuild tools\extractdwrflnfo.lpi

  rem Extract debug line info
  tools\extractdwrflnfo doublecmd.dbg
GOTO:EOF

:replace_old
  del /Q doublecmd.exe.old
  ren doublecmd.exe doublecmd.exe.old
GOTO:EOF
