rem This script run from create_packages.bat
rem If you run it direct, set up %BUILD_PACK_DIR% first

rem Prepare all installation files

set DC_INSTALL_DIR=%BUILD_PACK_DIR%\doublecmd
mkdir  %DC_INSTALL_DIR%

mkdir  %DC_INSTALL_DIR%\plugins
rem WCX plugins directories
mkdir  %DC_INSTALL_DIR%\plugins\wcx
mkdir  %DC_INSTALL_DIR%\plugins\wcx\cpio
mkdir  %DC_INSTALL_DIR%\plugins\wcx\deb
mkdir  %DC_INSTALL_DIR%\plugins\wcx\lzma
mkdir  %DC_INSTALL_DIR%\plugins\wcx\rpm
mkdir  %DC_INSTALL_DIR%\plugins\wcx\unrar
mkdir  %DC_INSTALL_DIR%\plugins\wcx\unbz2
mkdir  %DC_INSTALL_DIR%\plugins\wcx\zip
rem WDX plugins directories
mkdir  %DC_INSTALL_DIR%\plugins\wdx
mkdir  %DC_INSTALL_DIR%\plugins\wdx\scripts
mkdir  %DC_INSTALL_DIR%\plugins\wdx\rpm_wdx
mkdir  %DC_INSTALL_DIR%\plugins\wdx\deb_wdx

rem Copy directories
xcopy /E language %DC_INSTALL_DIR%\language\
xcopy /E doc\en   %DC_INSTALL_DIR%\doc\en\
xcopy /E pixmaps  %DC_INSTALL_DIR%\pixmaps\
rem Copy files
copy doc\*.txt              %DC_INSTALL_DIR%\doc\
copy doublecmd.exe          %DC_INSTALL_DIR%\
copy doublecmd.xml          %DC_INSTALL_DIR%\
copy doublecmd.ext.example  %DC_INSTALL_DIR%\
copy editor.col             %DC_INSTALL_DIR%\
copy twilight.col           %DC_INSTALL_DIR%\
copy pixmaps.txt            %DC_INSTALL_DIR%\
copy default.bar            %DC_INSTALL_DIR%\
rem Copy libraries
copy *.dll                  %DC_INSTALL_DIR%\

rem copy plugins
rem WCX
copy  plugins\wcx\cpio\lib\cpio.wcx        %DC_INSTALL_DIR%\plugins\wcx\cpio\
copy  plugins\wcx\deb\lib\deb.wcx          %DC_INSTALL_DIR%\plugins\wcx\deb\
copy  plugins\wcx\lzma\lib\lzma.wcx        %DC_INSTALL_DIR%\plugins\wcx\lzma\
copy  plugins\wcx\rpm\lib\rpm.wcx          %DC_INSTALL_DIR%\plugins\wcx\rpm\
copy  plugins\wcx\unrar\lib\unrar.wcx      %DC_INSTALL_DIR%\plugins\wcx\unrar\
copy  plugins\wcx\unbz2\lib\unbz2.wcx      %DC_INSTALL_DIR%\plugins\wcx\unbz2\
copy  plugins\wcx\zip\lib\zip.wcx          %DC_INSTALL_DIR%\plugins\wcx\zip\
rem WDX
copy  plugins\wdx\rpm_wdx\lib\rpm_wdx.wdx  %DC_INSTALL_DIR%\plugins\wdx\rpm_wdx\
copy  plugins\wdx\deb_wdx\lib\deb_wdx.wdx  %DC_INSTALL_DIR%\plugins\wdx\deb_wdx\
copy  plugins\wdx\scripts\*                %DC_INSTALL_DIR%\plugins\wdx\scripts\
