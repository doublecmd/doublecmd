
rem Path to Git
set GIT_EXE="%ProgramFiles%\Git\bin\git.exe"

rem Path to Inno Setup compiler
set ISCC_EXE="%ProgramFiles(x86)%\Inno Setup 5\ISCC.exe"

rem Path to Windows Installer XML (WiX) toolset
set PATH=%PATH%;"%ProgramFiles(x86)%\WiX Toolset v3.11\bin"

rem The new package will be created from here
set BUILD_PACK_DIR=%TEMP%\doublecmd-%DATE: =%

rem The new package will be saved here
set PACK_DIR=%CD%\windows\release

rem Read version number
for /f tokens^=2delims^=^" %%a in ('findstr "MajorVersionNr" ..\src\doublecmd.lpi') do (set DC_MAJOR=%%a)
for /f tokens^=2delims^=^" %%a in ('findstr "MinorVersionNr" ..\src\doublecmd.lpi') do (set DC_MINOR=%%a)
for /f tokens^=2delims^=^" %%a in ('findstr "RevisionNr" ..\src\doublecmd.lpi') do (set DC_MICRO=%%a)
if [%DC_MINOR%] == [] set DC_MINOR=0
if [%DC_MICRO%] == [] set DC_MICRO=0
set DC_VER=%DC_MAJOR%.%DC_MINOR%.%DC_MICRO%

rem Create temp dir for building
set BUILD_DC_TMP_DIR=%TEMP%\doublecmd-%DC_VER%
rm -rf %BUILD_DC_TMP_DIR%
mkdir %BUILD_DC_TMP_DIR%
%GIT_EXE% -C ..\ checkout-index -a -f --prefix=%BUILD_DC_TMP_DIR%\

rem Get processor architecture
if "%CPU_TARGET%" == "" (
  if "%PROCESSOR_ARCHITECTURE%" == "x86" (
    set CPU_TARGET=i386
    set OS_TARGET=win32
  ) else if "%PROCESSOR_ARCHITECTURE%" == "AMD64" (
    set CPU_TARGET=x86_64
    set OS_TARGET=win64
  )
)

rem Prepare needed variables
if "%CPU_TARGET%" == "i386" (
  set CPU_ARCH=x86
  set PF=ProgramFilesFolder
) else if "%CPU_TARGET%" == "x86_64" (
  set CPU_ARCH=x64
  set PF=ProgramFiles64Folder
)

rem Save revision number
set OUT=..\units\%CPU_TARGET%-%OS_TARGET%-win32
call ..\src\platform\git2revisioninc.exe.cmd %OUT%
copy %OUT%\dcrevision.inc %BUILD_DC_TMP_DIR%\units\

rem Prepare package build dir
rm -rf %BUILD_PACK_DIR%
mkdir %BUILD_PACK_DIR%
mkdir %BUILD_PACK_DIR%\release

rem Copy needed files
copy windows\doublecmd.iss %BUILD_PACK_DIR%\
copy windows\doublecmd.wxs %BUILD_PACK_DIR%\
copy windows\license.rtf   %BUILD_PACK_DIR%\
copy ..\src\doublecmd.ico  %BUILD_PACK_DIR%\

rem Copy libraries
copy windows\lib\%CPU_TARGET%\*.dll             %BUILD_DC_TMP_DIR%\
copy windows\lib\%CPU_TARGET%\winpty-agent.exe  %BUILD_DC_TMP_DIR%\

cd /D %BUILD_DC_TMP_DIR%

rem Build all components of Double Commander
call build.bat darkwin

rem Prepare install files
call %BUILD_DC_TMP_DIR%\install\windows\install.bat

cd /D %BUILD_PACK_DIR%
rem Create *.exe package
%ISCC_EXE% /F"doublecmd-%DC_VER%.%CPU_TARGET%-%OS_TARGET%" /DDisplayVersion=%DC_VER% doublecmd.iss

rem Move created package
move release\*.exe %PACK_DIR%

rem Create *.msi package
heat dir doublecmd -ag -cg HeatGroup -srd -dr APPLICATIONFOLDER -var var.SourcePath -o include.wxs
candle -arch %CPU_ARCH% -dProductVersion=%DC_VER% -dSourcePath=doublecmd -dProgramFiles=%PF% doublecmd.wxs include.wxs
light -ext WixUIExtension -cultures:en-us include.wixobj doublecmd.wixobj -o %PACK_DIR%\doublecmd-%DC_VER%.%CPU_TARGET%-%OS_TARGET%.msi

rem Create *.zip package
mkdir doublecmd\settings
copy NUL doublecmd\settings\doublecmd.inf
zip -9 -Dr %PACK_DIR%\doublecmd-%DC_VER%.%CPU_TARGET%-%OS_TARGET%.zip doublecmd

rem Clean temp directories
cd \
rm -rf %BUILD_DC_TMP_DIR%
rm -rf %BUILD_PACK_DIR%
