
rem Set Double Commander version
set DC_VER=1.1.0

rem Path to Git
set GIT_EXE="%ProgramFiles%\Git\bin\git.exe"

rem Path to Inno Setup compiler
set ISCC_EXE="%ProgramFiles(x86)%\Inno Setup 5\ISCC.exe"

rem The new package will be created from here
set BUILD_PACK_DIR=%TEMP%\doublecmd-%DATE: =%

rem The new package will be saved here
set PACK_DIR=%CD%\windows\release

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

rem Copy libraries
copy windows\lib\%CPU_TARGET%\*.dll    %BUILD_DC_TMP_DIR%\

cd /D %BUILD_DC_TMP_DIR%

rem Build all components of Double Commander
call build.bat release

rem Prepare install files
call %BUILD_DC_TMP_DIR%\install\windows\install.bat

cd /D %BUILD_PACK_DIR%
rem Create *.exe package
%ISCC_EXE% /F"doublecmd-%DC_VER%.%CPU_TARGET%-%OS_TARGET%" /DDisplayVersion=%DC_VER% doublecmd.iss

rem Move created package
move release\*.exe %PACK_DIR%

rem Create *.zip package
copy NUL doublecmd\doublecmd.inf
zip -9 -Dr %PACK_DIR%\doublecmd-%DC_VER%.%CPU_TARGET%-%OS_TARGET%.zip doublecmd

rem Clean temp directories
cd \
rm -rf %BUILD_DC_TMP_DIR%
rm -rf %BUILD_PACK_DIR%
