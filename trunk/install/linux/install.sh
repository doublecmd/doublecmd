#!/bin/sh
DC_INSTALL_DIR=/opt/doublecmd
mkdir -p $DC_INSTALL_DIR

mkdir -p $DC_INSTALL_DIR/plugins
# WCX plugins directories
mkdir -p $DC_INSTALL_DIR/plugins/wcx
mkdir -p $DC_INSTALL_DIR/plugins/wcx/cpio
mkdir -p $DC_INSTALL_DIR/plugins/wcx/deb
mkdir -p $DC_INSTALL_DIR/plugins/wcx/rpm
mkdir -p $DC_INSTALL_DIR/plugins/wcx/unrar
mkdir -p $DC_INSTALL_DIR/plugins/wcx/unbz2
mkdir -p $DC_INSTALL_DIR/plugins/wcx/zip
# WDX plugins directories
mkdir -p $DC_INSTALL_DIR/plugins/wdx
mkdir -p $DC_INSTALL_DIR/plugins/wdx/rpm_wdx

# Copy directories
cp -a language $DC_INSTALL_DIR/
cp -a doc $DC_INSTALL_DIR/
cp -a pixmaps $DC_INSTALL_DIR/
# Copy files
cp -a doublecmd      $DC_INSTALL_DIR/
cp -a doublecmd.ini  $DC_INSTALL_DIR/
cp -a doublecmd.ext  $DC_INSTALL_DIR/
cp -a editor.col     $DC_INSTALL_DIR/
cp -a twilight.col   $DC_INSTALL_DIR/
cp -a pixmaps.txt    $DC_INSTALL_DIR/
cp -a default.bar    $DC_INSTALL_DIR/
# Copy libraries
cp -a *.so           /usr/lib/

# copy plugins
# WCX
cp -a plugins/wcx/cpio/lib/cpio.wcx        $DC_INSTALL_DIR/plugins/wcx/cpio/
cp -a plugins/wcx/deb/lib/deb.wcx          $DC_INSTALL_DIR/plugins/wcx/deb/
cp -a plugins/wcx/rpm/lib/rpm.wcx          $DC_INSTALL_DIR/plugins/wcx/rpm/
cp -a plugins/wcx/unrar/lib/unrar.wcx      $DC_INSTALL_DIR/plugins/wcx/unrar/
cp -a plugins/wcx/unbz2/lib/unbz2.wcx      $DC_INSTALL_DIR/plugins/wcx/unbz2/
cp -a plugins/wcx/zip/lib/zip.wcx          $DC_INSTALL_DIR/plugins/wcx/zip/
# WDX
cp -a plugins/wdx/rpm_wdx/lib/rpm_wdx.wcx  $DC_INSTALL_DIR/plugins/wdx/rpm_wdx/

# Create symlink and desktop files
ln -sf $DC_INSTALL_DIR/doublecmd /usr/bin/doublecmd
install -m 644 icon.png /usr/share/pixmaps/doublecmd.png
install -m 644 install/linux/doublecmd.desktop /usr/share/applications/doublecmd.desktop

