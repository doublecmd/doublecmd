
rem Set Double Commander version
set DC_VER=0.3.5

rem Path to subversion
set SVN_EXE="c:\Program Files\svn-win32-1.4.3\bin\svn.exe"

rem Path to Inno Setup compiler
set ISCC_EXE="c:\Program Files\Inno Setup 5\ISCC.exe"

rem The new package will be created from here
set BUILD_PACK_DIR=%TEMP%\doublecmd-%DATE%

rem The new package will be saved here
set PACK_DIR=%CD%\windows\release

rem Create temp dir for building
set BUILD_DC_TMP_DIR=%TEMP%\doublecmd-%DC_VER%
rm -rf %BUILD_DC_TMP_DIR%
%SVN_EXE% export ..\ %BUILD_DC_TMP_DIR%

rem Prepare package build dir
rm -rf %BUILD_PACK_DIR%
mkdir %BUILD_PACK_DIR%
mkdir %BUILD_PACK_DIR%\release
rem Copy package description file
copy windows\doublecmd.iss %BUILD_PACK_DIR%\

rem Copy libraries
copy windows\lib\*.dll %BUILD_DC_TMP_DIR%\

cd /D %BUILD_DC_TMP_DIR%
rem Build all components of Double Commander
call _make.bat all

rem Prepare install files
call %BUILD_DC_TMP_DIR%\install\windows\install.bat

cd /D %BUILD_PACK_DIR%
rem Create *.exe package
%ISCC_EXE% doublecmd.iss

rem Move created package
move release\*.exe %PACK_DIR%

rem Create *.zip package
zip -9 -Dr %PACK_DIR%\doublecmd-%DC_VER%.i386-win32.zip doublecmd 

rem Clean temp directories
cd \
rm -rf %BUILD_DC_TMP_DIR%
rm -rf %BUILD_PACK_DIR%