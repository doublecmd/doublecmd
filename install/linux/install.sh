#!/bin/bash

# Parse input parameters
CKNAME=$(basename "$0")
args=$(getopt -n $CKNAME -o P:,I: -l portable-prefix:,install-prefix:,default -- "$@")
eval set -- $args
for A
do
  case "$A" in
       --)
            DC_INSTALL_DIR=/opt/doublecmd
            ;;
        -P|--portable-prefix)
            shift
            CK_PORTABLE=1
            DC_INSTALL_DIR=$(eval echo $1/doublecmd)
            break
            ;;
        -I|--install-prefix)
            shift
            DC_INSTALL_PREFIX=$(eval echo $1)
            DC_INSTALL_DIR=$DC_INSTALL_PREFIX/opt/doublecmd
            break
            ;;
  esac
  shift
done

mkdir -p $DC_INSTALL_DIR

mkdir -p $DC_INSTALL_DIR/doc

mkdir -p $DC_INSTALL_DIR/plugins
# WCX plugins directories
mkdir -p $DC_INSTALL_DIR/plugins/wcx
mkdir -p $DC_INSTALL_DIR/plugins/wcx/cpio
mkdir -p $DC_INSTALL_DIR/plugins/wcx/deb
mkdir -p $DC_INSTALL_DIR/plugins/wcx/lzma
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
cp -r language $DC_INSTALL_DIR/
cp -r doc/en   $DC_INSTALL_DIR/doc/
cp -r pixmaps  $DC_INSTALL_DIR/
# Copy files
cp -a doc/*.txt                    $DC_INSTALL_DIR/doc/   
cp -a doublecmd                    $DC_INSTALL_DIR/
cp -a install/linux/doublecmd.ini  $DC_INSTALL_DIR/
cp -a doublecmd.ext.example        $DC_INSTALL_DIR/
cp -a editor.col                   $DC_INSTALL_DIR/
cp -a twilight.col                 $DC_INSTALL_DIR/
cp -a pixmaps.txt                  $DC_INSTALL_DIR/
cp -a default.bar                  $DC_INSTALL_DIR/

# copy plugins
# WCX
install -m 644 plugins/wcx/cpio/lib/cpio.wcx        $DC_INSTALL_DIR/plugins/wcx/cpio/
install -m 644 plugins/wcx/deb/lib/deb.wcx          $DC_INSTALL_DIR/plugins/wcx/deb/
install -m 644 plugins/wcx/lzma/lib/lzma.wcx        $DC_INSTALL_DIR/plugins/wcx/lzma/
install -m 644 plugins/wcx/rpm/lib/rpm.wcx          $DC_INSTALL_DIR/plugins/wcx/rpm/
install -m 644 plugins/wcx/unrar/lib/unrar.wcx      $DC_INSTALL_DIR/plugins/wcx/unrar/
install -m 644 plugins/wcx/unbz2/lib/unbz2.wcx      $DC_INSTALL_DIR/plugins/wcx/unbz2/
install -m 644 plugins/wcx/zip/lib/zip.wcx          $DC_INSTALL_DIR/plugins/wcx/zip/
install -m 644 plugins/wcx/zip/src/ZipConfDlg.lfm   $DC_INSTALL_DIR/plugins/wcx/zip/
# WDX
install -m 644 plugins/wdx/rpm_wdx/lib/rpm_wdx.wdx  $DC_INSTALL_DIR/plugins/wdx/rpm_wdx/
install -m 644 plugins/wdx/deb_wdx/lib/deb_wdx.wdx  $DC_INSTALL_DIR/plugins/wdx/deb_wdx/
install -m 755 plugins/wdx/scripts/*                $DC_INSTALL_DIR/plugins/wdx/scripts/
# WLX
install -m 644 plugins/wlx/WlxMplayer/lib/wlxMplayer.wlx  $DC_INSTALL_DIR/plugins/wlx/WlxMplayer/
# DSX
install -m 644 plugins/dsx/DSXLocate/lib/DSXLocate.dsx  $DC_INSTALL_DIR/plugins/dsx/DSXLocate/

if [ -z $CK_PORTABLE ]
  then
    # Copy libraries
    if [ "$CPU_TARGET" = "x86_64" ]
      then
        # for cross compiling try to create library directory
        install -d $DC_INSTALL_PREFIX/usr/lib64
        install -m 644 *.so           $DC_INSTALL_PREFIX/usr/lib64/
      else
        install -d $DC_INSTALL_PREFIX/usr/lib
        install -m 644 *.so           $DC_INSTALL_PREFIX/usr/lib/
    fi
    # Create symlink and desktop files
    install -d $DC_INSTALL_PREFIX/usr/bin
    install -d $DC_INSTALL_PREFIX/usr/share/pixmaps
    install -d $DC_INSTALL_PREFIX/usr/share/applications
    ln -sf $DC_INSTALL_DIR/doublecmd $DC_INSTALL_PREFIX/usr/bin/doublecmd
    install -m 644 doublecmd.png $DC_INSTALL_PREFIX/usr/share/pixmaps/doublecmd.png
    install -m 644 install/linux/doublecmd.desktop $DC_INSTALL_PREFIX/usr/share/applications/doublecmd.desktop
  else
    cp -a doublecmd.sh $DC_INSTALL_DIR/
    # Copy libraries
    cp -a *.so     $DC_INSTALL_DIR/
    # Copy DC icon
    cp -a doublecmd.png $DC_INSTALL_DIR/doublecmd.png
fi
