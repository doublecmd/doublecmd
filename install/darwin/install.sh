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
mkdir -p $DC_INSTALL_DIR/plugins/wcx/base64
mkdir -p $DC_INSTALL_DIR/plugins/wcx/cpio
mkdir -p $DC_INSTALL_DIR/plugins/wcx/deb
mkdir -p $DC_INSTALL_DIR/plugins/wcx/rpm
mkdir -p $DC_INSTALL_DIR/plugins/wcx/unrar
mkdir -p $DC_INSTALL_DIR/plugins/wcx/zip
# WDX plugins directories
mkdir -p $DC_INSTALL_DIR/plugins/wdx
mkdir -p $DC_INSTALL_DIR/plugins/wdx/scripts
mkdir -p $DC_INSTALL_DIR/plugins/wdx/rpm_wdx
mkdir -p $DC_INSTALL_DIR/plugins/wdx/deb_wdx
mkdir -p $DC_INSTALL_DIR/plugins/wdx/audioinfo
# WFX plugins directories
mkdir -p $DC_INSTALL_DIR/plugins/wfx
mkdir -p $DC_INSTALL_DIR/plugins/wfx/ftp
mkdir -p $DC_INSTALL_DIR/plugins/wfx/MacCloud
# WLX plugins directories
mkdir -p $DC_INSTALL_DIR/plugins/wlx
mkdir -p $DC_INSTALL_DIR/plugins/wlx/MacPreview

# Copy files
cp -r doublecmd.app/*              $DC_APP_DIR/
cp -a doublecmd                    $DC_INSTALL_DIR/
cp -a doublecmd.help               $DC_INSTALL_DIR/
cp -a doublecmd.zdli               $DC_INSTALL_DIR/
cp -a pinyin.tbl                   $DC_INSTALL_DIR/

# Copy plugins

# WCX
install -m 644 plugins/wcx/base64/base64.wcx        $DC_INSTALL_DIR/plugins/wcx/base64/
install -m 644 plugins/wcx/cpio/cpio.wcx            $DC_INSTALL_DIR/plugins/wcx/cpio/
install -m 644 plugins/wcx/deb/deb.wcx              $DC_INSTALL_DIR/plugins/wcx/deb/
install -m 644 plugins/wcx/rpm/rpm.wcx              $DC_INSTALL_DIR/plugins/wcx/rpm/
cp -r plugins/wcx/unrar/language                    $DC_INSTALL_DIR/plugins/wcx/unrar
install -m 644 plugins/wcx/unrar/unrar.wcx          $DC_INSTALL_DIR/plugins/wcx/unrar/
cp -r plugins/wcx/zip/language                      $DC_INSTALL_DIR/plugins/wcx/zip
install -m 644 plugins/wcx/zip/zip.wcx              $DC_INSTALL_DIR/plugins/wcx/zip/
# WDX
install -m 644 plugins/wdx/rpm_wdx/rpm_wdx.wdx      $DC_INSTALL_DIR/plugins/wdx/rpm_wdx/
install -m 644 plugins/wdx/deb_wdx/deb_wdx.wdx      $DC_INSTALL_DIR/plugins/wdx/deb_wdx/
install -m 644 plugins/wdx/scripts/*                $DC_INSTALL_DIR/plugins/wdx/scripts/
install -m 644 plugins/wdx/audioinfo/audioinfo.wdx  $DC_INSTALL_DIR/plugins/wdx/audioinfo/
install -m 644 plugins/wdx/audioinfo/audioinfo.lng  $DC_INSTALL_DIR/plugins/wdx/audioinfo/
# WFX
cp -r plugins/wfx/ftp/language                      $DC_INSTALL_DIR/plugins/wfx/ftp
install -m 644 plugins/wfx/ftp/ftp.wfx              $DC_INSTALL_DIR/plugins/wfx/ftp/
install -m 644 plugins/wfx/ftp/src/ftp.ico          $DC_INSTALL_DIR/plugins/wfx/ftp/
cp -r plugins/wfx/MacCloud/language                 $DC_INSTALL_DIR/plugins/wfx/MacCloud
cp -r plugins/wfx/MacCloud/drivers                  $DC_INSTALL_DIR/plugins/wfx/MacCloud
install -m 644 plugins/wfx/MacCloud/MacCloud.wfx    $DC_INSTALL_DIR/plugins/wfx/MacCloud/
install -m 644 plugins/wfx/MacCloud/MacCloud.ico    $DC_INSTALL_DIR/plugins/wfx/MacCloud/
# WLX
install -m 644 plugins/wlx/MacPreview/MacPreview.wlx $DC_INSTALL_DIR/plugins/wlx/MacPreview/

# Copy documentation
mkdir -p $DC_INSTALL_DIR/doc
cp -a doc/*.txt $DC_INSTALL_DIR/doc/

# Copy scripts
mkdir -p $DC_INSTALL_DIR/scripts
cp -a scripts/terminal.sh $DC_INSTALL_DIR/scripts/

# Copy directories
cp -r default      $DC_INSTALL_DIR/
cp -r language     $DC_INSTALL_DIR/
cp -r pixmaps      $DC_INSTALL_DIR/
cp -r highlighters $DC_INSTALL_DIR/

# Copy libraries
cp -a *.dylib             $DC_INSTALL_DIR/

# Install instruction
cp -r install/darwin/dmg/. $1

# Update icon-theme.cache time
touch -r $DC_INSTALL_DIR/pixmaps/dctheme $DC_INSTALL_DIR/pixmaps/dctheme/icon-theme.cache
