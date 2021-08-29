rem This script run from create_packages.bat
rem If you run it direct, set up %BUILD_PACK_DIR% first

rem Prepare all installation files

set DC_INSTALL_DIR=%BUILD_PACK_DIR%\doublecmd
mkdir  %DC_INSTALL_DIR%

mkdir  %DC_INSTALL_DIR%\plugins
rem WCX plugins directories
mkdir  %DC_INSTALL_DIR%\plugins\wcx
mkdir  %DC_INSTALL_DIR%\plugins\wcx\rpm
mkdir  %DC_INSTALL_DIR%\plugins\wcx\sevenzip
mkdir  %DC_INSTALL_DIR%\plugins\wcx\unrar
mkdir  %DC_INSTALL_DIR%\plugins\wcx\zip
rem WDX plugins directories
mkdir  %DC_INSTALL_DIR%\plugins\wdx
mkdir  %DC_INSTALL_DIR%\plugins\wdx\scripts
mkdir  %DC_INSTALL_DIR%\plugins\wdx\rpm_wdx
mkdir  %DC_INSTALL_DIR%\plugins\wdx\deb_wdx
mkdir  %DC_INSTALL_DIR%\plugins\wdx\audioinfo
rem WFX plugins directories
mkdir  %DC_INSTALL_DIR%\plugins\wfx
mkdir  %DC_INSTALL_DIR%\plugins\wfx\ftp
rem WLX plugins directories
mkdir  %DC_INSTALL_DIR%\plugins\wlx
mkdir  %DC_INSTALL_DIR%\plugins\wlx\preview

mkdir  %DC_INSTALL_DIR%\doc
rem Copy directories
xcopy /E language     %DC_INSTALL_DIR%\language\
xcopy /E pixmaps      %DC_INSTALL_DIR%\pixmaps\
xcopy /E highlighters %DC_INSTALL_DIR%\highlighters\
rem Copy files
copy doc\*.txt                      %DC_INSTALL_DIR%\doc\
copy doublecmd.exe                  %DC_INSTALL_DIR%\
copy doublecmd.help                 %DC_INSTALL_DIR%\
copy doublecmd.zdli                 %DC_INSTALL_DIR%\
copy doublecmd.ext.example          %DC_INSTALL_DIR%\
copy pixmaps.txt                    %DC_INSTALL_DIR%\
copy multiarc.ini                   %DC_INSTALL_DIR%\
copy pinyin.tbl                     %DC_INSTALL_DIR%\
rem Copy libraries
copy *.dll                          %DC_INSTALL_DIR%\

rem copy plugins
rem WCX
copy  plugins\wcx\rpm\rpm.wcx             %DC_INSTALL_DIR%\plugins\wcx\rpm\
copy  plugins\wcx\sevenzip\sevenzip.wcx   %DC_INSTALL_DIR%\plugins\wcx\sevenzip\
copy  plugins\wcx\unrar\unrar.wcx         %DC_INSTALL_DIR%\plugins\wcx\unrar\
xcopy /E plugins\wcx\unrar\language       %DC_INSTALL_DIR%\plugins\wcx\unrar\language\
copy  plugins\wcx\zip\zip.wcx             %DC_INSTALL_DIR%\plugins\wcx\zip\
xcopy /E plugins\wcx\zip\language         %DC_INSTALL_DIR%\plugins\wcx\zip\language\
rem WDX
copy  plugins\wdx\rpm_wdx\rpm_wdx.wdx     %DC_INSTALL_DIR%\plugins\wdx\rpm_wdx\
copy  plugins\wdx\deb_wdx\deb_wdx.wdx     %DC_INSTALL_DIR%\plugins\wdx\deb_wdx\
copy  plugins\wdx\scripts\*               %DC_INSTALL_DIR%\plugins\wdx\scripts\
copy  plugins\wdx\audioinfo\audioinfo.wdx %DC_INSTALL_DIR%\plugins\wdx\audioinfo\
copy  plugins\wdx\audioinfo\audioinfo.lng %DC_INSTALL_DIR%\plugins\wdx\audioinfo\
rem WFX
copy  plugins\wfx\ftp\ftp.wfx             %DC_INSTALL_DIR%\plugins\wfx\ftp\
xcopy /E plugins\wfx\ftp\language         %DC_INSTALL_DIR%\plugins\wfx\ftp\language\
rem WLX
copy  plugins\wlx\preview\preview.wlx     %DC_INSTALL_DIR%\plugins\wlx\preview\
