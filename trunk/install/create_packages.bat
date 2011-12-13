
rem Set Double Commander version
set DC_VER=0.5.5

rem Path to subversion
set SVN_EXE="c:\Program Files\SlikSvn\bin\svn.exe"

rem Path to Inno Setup compiler
set ISCC_EXE="c:\Program Files\Inno Setup 5\ISCC.exe"

rem The new package will be created from here
set BUILD_PACK_DIR=%TEMP%\doublecmd-%DATE: =%

rem The new package will be saved here
set PACK_DIR=%CD%\windows\release

rem Create temp dir for building
set BUILD_DC_TMP_DIR=%TEMP%\doublecmd-%DC_VER%
rm -rf %BUILD_DC_TMP_DIR%
%SVN_EXE% export ..\ %BUILD_DC_TMP_DIR%

rem Save revision number
mkdir %BUILD_DC_TMP_DIR%\.svn
copy ..\.svn\entries %BUILD_DC_TMP_DIR%\.svn\

rem Prepare package build dir
rm -rf %BUILD_PACK_DIR%
mkdir %BUILD_PACK_DIR%
mkdir %BUILD_PACK_DIR%\release

rem Copy needed files
copy windows\doublecmd.iss %BUILD_PACK_DIR%\
copy windows\portable.diff %BUILD_PACK_DIR%\

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

rem Copy libraries
copy windows\lib\%CPU_TARGET%\*.dll    %BUILD_DC_TMP_DIR%\

cd /D %BUILD_DC_TMP_DIR%

rem Build all components of Double Commander
call build.bat all

rem Prepare install files
call %BUILD_DC_TMP_DIR%\install\windows\install.bat

cd /D %BUILD_PACK_DIR%
rem Create *.exe package
%ISCC_EXE% /F"doublecmd-%DC_VER%.%CPU_TARGET%-%OS_TARGET%" doublecmd.iss

rem Move created package
move release\*.exe %PACK_DIR%

rem Create *.zip package
patch doublecmd/doublecmd.xml portable.diff
zip -9 -Dr %PACK_DIR%\doublecmd-%DC_VER%.%CPU_TARGET%-%OS_TARGET%.zip doublecmd 

rem Create help packages
cd /D %BUILD_DC_TMP_DIR%
rem Copy help files
call %BUILD_DC_TMP_DIR%\install\windows\install-help.bat
rem Create help package for each language
cd %BUILD_PACK_DIR%\doublecmd
for /D %%f in (doc\*) do zip -9 -Dr %PACK_DIR%\doublecmd-help-%%~nf-%DC_VER%.noarch.zip %%f

rem Clean temp directories
cd \
rm -rf %BUILD_DC_TMP_DIR%
rm -rf %BUILD_PACK_DIR%
