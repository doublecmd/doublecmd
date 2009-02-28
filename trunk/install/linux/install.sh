#!/bin/sh

if [ -z $1 ]
  then DC_INSTALL_DIR=/opt/doublecmd
  else DC_INSTALL_DIR=$1/doublecmd
fi

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
mkdir -p $DC_INSTALL_DIR/plugins/wdx/scripts
mkdir -p $DC_INSTALL_DIR/plugins/wdx/rpm_wdx
mkdir -p $DC_INSTALL_DIR/plugins/wdx/deb_wdx
# WLX plugins directories
mkdir -p $DC_INSTALL_DIR/plugins/wlx
mkdir -p $DC_INSTALL_DIR/plugins/wlx/WlxMplayer
# DSX plugins directories
mkdir -p $DC_INSTALL_DIR/plugins/dsx
mkdir -p $DC_INSTALL_DIR/plugins/dsx/DSXLocate

# Copy directories
cp -a language $DC_INSTALL_DIR/
cp -a doc $DC_INSTALL_DIR/
cp -a pixmaps $DC_INSTALL_DIR/
# Copy files
cp -a doublecmd                    $DC_INSTALL_DIR/
cp -a install/linux/doublecmd.ini  $DC_INSTALL_DIR/
cp -a doublecmd.ext.example        $DC_INSTALL_DIR/
cp -a editor.col                   $DC_INSTALL_DIR/
cp -a twilight.col                 $DC_INSTALL_DIR/
cp -a pixmaps.txt                  $DC_INSTALL_DIR/
cp -a default.bar                  $DC_INSTALL_DIR/

# copy plugins
# WCX
cp -a plugins/wcx/cpio/lib/cpio.wcx        $DC_INSTALL_DIR/plugins/wcx/cpio/
cp -a plugins/wcx/deb/lib/deb.wcx          $DC_INSTALL_DIR/plugins/wcx/deb/
cp -a plugins/wcx/rpm/lib/rpm.wcx          $DC_INSTALL_DIR/plugins/wcx/rpm/
cp -a plugins/wcx/unrar/lib/unrar.wcx      $DC_INSTALL_DIR/plugins/wcx/unrar/
cp -a plugins/wcx/unbz2/lib/unbz2.wcx      $DC_INSTALL_DIR/plugins/wcx/unbz2/
cp -a plugins/wcx/zip/lib/zip.wcx          $DC_INSTALL_DIR/plugins/wcx/zip/
cp -a plugins/wcx/zip/ZipConfDlg.lfm       $DC_INSTALL_DIR/plugins/wcx/zip/
# WDX
cp -a plugins/wdx/rpm_wdx/lib/rpm_wdx.wdx  $DC_INSTALL_DIR/plugins/wdx/rpm_wdx/
cp -a plugins/wdx/deb_wdx/lib/deb_wdx.wdx  $DC_INSTALL_DIR/plugins/wdx/deb_wdx/
install -m 644 plugins/wdx/scripts/*       $DC_INSTALL_DIR/plugins/wdx/scripts/
# WLX
cp -a plugins/wlx/WlxMplayer/lib/wlxMplayer.wlx  $DC_INSTALL_DIR/plugins/wlx/WlxMplayer/
# DSX
cp -a plugins/dsx/DSXLocate/lib/DSXLocate.dsx  $DC_INSTALL_DIR/plugins/dsx/DSXLocate/

if [ -z $1 ]
  then
    # Copy libraries
    if [ "$(fpc -iTP)" = "x86_64" ]
      then
        install -m 644 *.so           /usr/lib64/
      else
        install -m 644 *.so           /usr/lib/
    fi
    # Create symlink and desktop files
    ln -sf $DC_INSTALL_DIR/doublecmd /usr/bin/doublecmd
    install -m 644 doublecmd.png /usr/share/pixmaps/doublecmd.png
    install -m 644 install/linux/doublecmd.desktop /usr/share/applications/doublecmd.desktop
  else
    cp -a doublecmd.sh $DC_INSTALL_DIR/
    # Copy libraries
    cp -a *.so     $DC_INSTALL_DIR/
    # Copy DC icon
    cp -a doublecmd.png $DC_INSTALL_DIR/doublecmd.png
fi
