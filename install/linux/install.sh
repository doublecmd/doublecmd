#!/bin/bash

# Set processor architecture
if [ -z $CPU_TARGET ]; then
   export CPU_TARGET=$(fpc -iTP)
fi

# Determine library directory
if [ "$CPU_TARGET" = "x86_64" ] && [ ! -f "/etc/debian_version" ]
   then
       OS_LIB_DIR=usr/lib64
   else
       OS_LIB_DIR=usr/lib
fi

# Parse input parameters
CKNAME=$(basename "$0")
args=$(getopt -n $CKNAME -o P:,I: -l portable-prefix:,install-prefix:,default -- "$@")
eval set -- $args
for A
do
  case "$A" in
       --)
            DC_INSTALL_DIR=/$OS_LIB_DIR/doublecmd
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
            DC_INSTALL_DIR=$DC_INSTALL_PREFIX/$OS_LIB_DIR/doublecmd
            break
            ;;
  esac
  shift
done

mkdir -p $DC_INSTALL_DIR

mkdir -p $DC_INSTALL_DIR/plugins
# WCX plugins directories
mkdir -p $DC_INSTALL_DIR/plugins/wcx
mkdir -p $DC_INSTALL_DIR/plugins/wcx/cpio
mkdir -p $DC_INSTALL_DIR/plugins/wcx/deb
mkdir -p $DC_INSTALL_DIR/plugins/wcx/lzma
mkdir -p $DC_INSTALL_DIR/plugins/wcx/rpm
mkdir -p $DC_INSTALL_DIR/plugins/wcx/unrar
mkdir -p $DC_INSTALL_DIR/plugins/wcx/zip
# WDX plugins directories
mkdir -p $DC_INSTALL_DIR/plugins/wdx
mkdir -p $DC_INSTALL_DIR/plugins/wdx/scripts
mkdir -p $DC_INSTALL_DIR/plugins/wdx/rpm_wdx
mkdir -p $DC_INSTALL_DIR/plugins/wdx/deb_wdx
# WFX plugins directories
mkdir -p $DC_INSTALL_DIR/plugins/wfx
mkdir -p $DC_INSTALL_DIR/plugins/wfx/ftp
# WLX plugins directories
mkdir -p $DC_INSTALL_DIR/plugins/wlx
mkdir -p $DC_INSTALL_DIR/plugins/wlx/wlxmplayer
# DSX plugins directories
mkdir -p $DC_INSTALL_DIR/plugins/dsx
mkdir -p $DC_INSTALL_DIR/plugins/dsx/dsxlocate

# Copy files
cp -a doublecmd                    $DC_INSTALL_DIR/
cp -a install/linux/doublecmd.xml  $DC_INSTALL_DIR/
cp -a doublecmd.ext.example        $DC_INSTALL_DIR/
cp -a editor.col                   $DC_INSTALL_DIR/
cp -a twilight.col                 $DC_INSTALL_DIR/
cp -a pixmaps.txt                  $DC_INSTALL_DIR/
cp -a default.bar                  $DC_INSTALL_DIR/
cp -a multiarc.ini                 $DC_INSTALL_DIR/

# copy plugins
# WCX
install -m 644 plugins/wcx/cpio/lib/cpio.wcx        $DC_INSTALL_DIR/plugins/wcx/cpio/
install -m 644 plugins/wcx/deb/lib/deb.wcx          $DC_INSTALL_DIR/plugins/wcx/deb/
install -m 644 plugins/wcx/lzma/lib/lzma.wcx        $DC_INSTALL_DIR/plugins/wcx/lzma/
install -m 644 plugins/wcx/rpm/lib/rpm.wcx          $DC_INSTALL_DIR/plugins/wcx/rpm/
install -m 644 plugins/wcx/unrar/lib/unrar.wcx      $DC_INSTALL_DIR/plugins/wcx/unrar/
install -m 644 plugins/wcx/zip/lib/zip.wcx          $DC_INSTALL_DIR/plugins/wcx/zip/
# WDX
install -m 644 plugins/wdx/rpm_wdx/lib/rpm_wdx.wdx  $DC_INSTALL_DIR/plugins/wdx/rpm_wdx/
install -m 644 plugins/wdx/deb_wdx/lib/deb_wdx.wdx  $DC_INSTALL_DIR/plugins/wdx/deb_wdx/
install -m 644 plugins/wdx/scripts/*                $DC_INSTALL_DIR/plugins/wdx/scripts/
# WFX
install -m 644 plugins/wfx/ftp/lib/ftp.wfx          $DC_INSTALL_DIR/plugins/wfx/ftp/
# WLX
install -m 644 plugins/wlx/WlxMplayer/lib/wlxmplayer.wlx  $DC_INSTALL_DIR/plugins/wlx/wlxmplayer/
# DSX
install -m 644 plugins/dsx/DSXLocate/lib/dsxlocate.dsx  $DC_INSTALL_DIR/plugins/dsx/dsxlocate/

if [ -z $CK_PORTABLE ]
  then
    # Copy libraries
    install -d                $DC_INSTALL_PREFIX/$OS_LIB_DIR
    install -m 644 *.so       $DC_INSTALL_PREFIX/$OS_LIB_DIR
    # Create directory for platform independed files
    install -d                $DC_INSTALL_PREFIX/usr/share/doublecmd
    # Copy documentation
    install -d                $DC_INSTALL_PREFIX/usr/share/doublecmd/doc
    install -m 644 doc/*.txt  $DC_INSTALL_PREFIX/usr/share/doublecmd/doc
    ln -sf /usr/share/doublecmd/doc $DC_INSTALL_DIR/doc
    # Copy languages
    cp -r language $DC_INSTALL_PREFIX/usr/share/doublecmd
    ln -sf /usr/share/doublecmd/language $DC_INSTALL_DIR/language
    # Copy pixmaps
    cp -r pixmaps $DC_INSTALL_PREFIX/usr/share/doublecmd
    ln -sf /usr/share/doublecmd/pixmaps $DC_INSTALL_DIR/pixmaps
    # Create symlink and desktop files
    install -d $DC_INSTALL_PREFIX/usr/bin
    install -d $DC_INSTALL_PREFIX/usr/share/pixmaps
    install -d $DC_INSTALL_PREFIX/usr/share/applications
    ln -sf  /$OS_LIB_DIR/doublecmd/doublecmd $DC_INSTALL_PREFIX/usr/bin/doublecmd
    install -m 644 doublecmd.png $DC_INSTALL_PREFIX/usr/share/pixmaps/doublecmd.png
    install -m 644 install/linux/doublecmd.desktop $DC_INSTALL_PREFIX/usr/share/applications/doublecmd.desktop
  else
    # Copy documentation
    mkdir -p $DC_INSTALL_DIR/doc
    cp -a doc/*.txt $DC_INSTALL_DIR/doc/
    # Copy script for execute portable version
    cp -a doublecmd.sh $DC_INSTALL_DIR/
    # Copy directories
    cp -r language $DC_INSTALL_DIR/
    cp -r pixmaps  $DC_INSTALL_DIR/
    # Copy libraries
    cp -a *.so     $DC_INSTALL_DIR/
    # Copy DC icon
    cp -a doublecmd.png $DC_INSTALL_DIR/doublecmd.png
fi
