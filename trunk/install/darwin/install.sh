#!/bin/bash

# Set processor architecture
if [ -z $CPU_TARGET ]; then
   export CPU_TARGET=$(fpc -iTP)
fi

export DC_APP_DIR=$1/doublecmd.app
export DC_INSTALL_DIR=$DC_APP_DIR/Contents/MacOS

mkdir -p $DC_INSTALL_DIR

mkdir -p $DC_INSTALL_DIR/plugins

# WCX plugins directories
mkdir -p $DC_INSTALL_DIR/plugins/wcx
mkdir -p $DC_INSTALL_DIR/plugins/wcx/cpio
mkdir -p $DC_INSTALL_DIR/plugins/wcx/deb
mkdir -p $DC_INSTALL_DIR/plugins/wcx/lzma
mkdir -p $DC_INSTALL_DIR/plugins/wcx/rpm
mkdir -p $DC_INSTALL_DIR/plugins/wcx/unrar
# WDX plugins directories
mkdir -p $DC_INSTALL_DIR/plugins/wdx
mkdir -p $DC_INSTALL_DIR/plugins/wdx/scripts
mkdir -p $DC_INSTALL_DIR/plugins/wdx/rpm_wdx
mkdir -p $DC_INSTALL_DIR/plugins/wdx/deb_wdx
# WFX plugins directories
mkdir -p $DC_INSTALL_DIR/plugins/wfx
mkdir -p $DC_INSTALL_DIR/plugins/wfx/ftp
mkdir -p $DC_INSTALL_DIR/plugins/wfx/samba

# Copy files
cp -r doublecmd.app/*              $DC_APP_DIR/
cp -a doublecmd                    $DC_INSTALL_DIR/
cp -a install/darwin/doublecmd.xml $DC_INSTALL_DIR/
cp -a doublecmd.ext.example        $DC_INSTALL_DIR/
cp -a pixmaps.txt                  $DC_INSTALL_DIR/
cp -a default.bar                  $DC_INSTALL_DIR/
cp -a multiarc.ini                 $DC_INSTALL_DIR/
cp -a 7za                          $DC_INSTALL_DIR/

# Copy plugins

# WCX
install -m 644 plugins/wcx/cpio/lib/cpio.wcx        $DC_INSTALL_DIR/plugins/wcx/cpio/
install -m 644 plugins/wcx/deb/lib/deb.wcx          $DC_INSTALL_DIR/plugins/wcx/deb/
install -m 644 plugins/wcx/lzma/lib/lzma.wcx        $DC_INSTALL_DIR/plugins/wcx/lzma/
install -m 644 plugins/wcx/rpm/lib/rpm.wcx          $DC_INSTALL_DIR/plugins/wcx/rpm/
install -m 644 plugins/wcx/unrar/lib/unrar.wcx      $DC_INSTALL_DIR/plugins/wcx/unrar/
# WDX
install -m 644 plugins/wdx/rpm_wdx/lib/rpm_wdx.wdx  $DC_INSTALL_DIR/plugins/wdx/rpm_wdx/
install -m 644 plugins/wdx/deb_wdx/lib/deb_wdx.wdx  $DC_INSTALL_DIR/plugins/wdx/deb_wdx/
install -m 644 plugins/wdx/scripts/*                $DC_INSTALL_DIR/plugins/wdx/scripts/
# WFX
install -m 644 plugins/wfx/ftp/lib/ftp.wfx          $DC_INSTALL_DIR/plugins/wfx/ftp/
install -m 644 plugins/wfx/samba/lib/samba.wfx      $DC_INSTALL_DIR/plugins/wfx/samba/

# Copy documentation
mkdir -p $DC_INSTALL_DIR/doc
cp -r doc/en    $DC_INSTALL_DIR/doc/
cp -a doc/*.txt $DC_INSTALL_DIR/doc/

# Copy directories
cp -r language $DC_INSTALL_DIR/
cp -r pixmaps  $DC_INSTALL_DIR/

# Copy libraries
cp -a *.dylib             $DC_INSTALL_DIR/

