rem This script run from create_packages.bat
rem If you run it direct, set up %BUILD_PACK_DIR% first

set DC_HELP_INSTALL_DIR=%BUILD_PACK_DIR%\doublecmd\doc

rem Clean help directory
rm -rf %DC_HELP_INSTALL_DIR%\

rem Copy Russian help files
xcopy /E doc\ru %DC_HELP_INSTALL_DIR%\ru\