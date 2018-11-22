@echo off

rem This script converts portable *.zip package to *.msi package

rem Check command arguments
if "%1" == "" (
  echo.
  echo Syntax:
  echo.
  echo   %~nx0 ^<Full path to portable .zip package^>
  goto :eof
)

rem Path to Windows Installer XML (WiX) toolset
set PATH=%PATH%;"C:\Program Files (x86)\WiX Toolset v3.11\bin"

rem The new package will be created from here
set BUILD_PACK_DIR=%TEMP%\doublecmd-%DATE: =%

rem The new package will be saved here
set PACK_DIR=%~dp0/release

rem Determine package file name
for /f %%i in ("%1") do set PACKAGE=%%~ni

rem Get package version and architecture
for /f "tokens=1,2,3,4,5 delims=-." %%a in ("%PACKAGE%") do (
  set DC_VER=%%b.%%c.%%d
  set CPU_TARGET=%%e
)

rem Prepare needed variables
if "%CPU_TARGET%" == "i386" (
  set CPU_TARGET=x86
  set PF=ProgramFilesFolder
) else if "%CPU_TARGET%" == "x86_64" (
  set CPU_TARGET=x64
  set PF=ProgramFiles64Folder
)

rem Prepare package build dir
mkdir %BUILD_PACK_DIR%

rem Extract archive
unzip %1 -d %BUILD_PACK_DIR%

rem Copy needed files
copy license.rtf             %BUILD_PACK_DIR%\
copy doublecmd.wxs           %BUILD_PACK_DIR%\
copy ..\..\src\doublecmd.ico %BUILD_PACK_DIR%\

pushd %BUILD_PACK_DIR%

del /Q doublecmd\doublecmd.inf
move doublecmd "Double Commander"
heat dir "Double Commander" -ag -cg HeatGroup -dr %PF% -var var.SourcePath -o include.wxs
candle -arch %CPU_TARGET% -dProductVersion=%DC_VER% -dSourcePath="Double Commander" -dProgramFiles=%PF% doublecmd.wxs include.wxs
light -ext WixUIExtension -cultures:en-us include.wixobj doublecmd.wixobj -o %PACKAGE%.msi

rem Move created package
move %PACKAGE%.msi %PACK_DIR%/

rem Clean temp directories
popd
rmdir /S /Q %BUILD_PACK_DIR%
