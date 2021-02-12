#!/bin/bash

set -e

# Set processor architecture
if [ -z $CPU_TARGET ]; then
   export CPU_TARGET=$(fpc -iTP)
fi

# Determine library directory
if [ "$CPU_TARGET" = "x86_64" ] && [ ! -f "/etc/debian_version" ]
   then
       LIB_SUFFIX=64
   else
       LIB_SUFFIX=
fi

# Parse input parameters
CKNAME=$(basename "$0")
args=$(getopt -n $CKNAME -o P:,I: -l portable-prefix:,install-prefix:,default -- "$@")
eval set -- $args
for A
do
  case "$A" in
       --)
            DC_INSTALL_DIR=/usr/lib$LIB_SUFFIX/doublecmd
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
            DC_INSTALL_DIR=$DC_INSTALL_PREFIX/usr/lib$LIB_SUFFIX/doublecmd
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
mkdir -p $DC_INSTALL_DIR/plugins/wfx/samba
# WLX plugins directories
mkdir -p $DC_INSTALL_DIR/plugins/wlx
mkdir -p $DC_INSTALL_DIR/plugins/wlx/wlxmplayer
# DSX plugins directories
mkdir -p $DC_INSTALL_DIR/plugins/dsx
mkdir -p $DC_INSTALL_DIR/plugins/dsx/dsxlocate

# Copy files
cp -a doublecmd                    $DC_INSTALL_DIR/
cp -a doublecmd.help               $DC_INSTALL_DIR/
cp -a doublecmd.zdli               $DC_INSTALL_DIR/
cp -a doublecmd.ext.example        $DC_INSTALL_DIR/
cp -a pixmaps.txt                  $DC_INSTALL_DIR/
cp -a multiarc.ini                 $DC_INSTALL_DIR/

# copy plugins
# WCX
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
install -m 644 plugins/wfx/samba/samba.wfx          $DC_INSTALL_DIR/plugins/wfx/samba/
# WLX
install -m 644 plugins/wlx/WlxMplayer/wlxmplayer.wlx  $DC_INSTALL_DIR/plugins/wlx/wlxmplayer/
# DSX
install -m 644 plugins/dsx/DSXLocate/dsxlocate.dsx    $DC_INSTALL_DIR/plugins/dsx/dsxlocate/

if [ -z $CK_PORTABLE ]
  then
    # Copy libraries
    install -d                $DC_INSTALL_PREFIX/usr/lib$LIB_SUFFIX
    if [ "$(echo *.so*)" != "*.so*" ]; then
      install -m 644 *.so*    $DC_INSTALL_PREFIX/usr/lib$LIB_SUFFIX
    fi
    # Create directory for platform independed files
    install -d                $DC_INSTALL_PREFIX/usr/share/doublecmd
    # Copy man files
    install -d -m 755                      $DC_INSTALL_PREFIX/usr/share/man/man1
    install -c -m 644 install/linux/*.1    $DC_INSTALL_PREFIX/usr/share/man/man1
    # Copy documentation
    install -d                $DC_INSTALL_PREFIX/usr/share/doublecmd/doc
    install -m 644 doc/*.txt  $DC_INSTALL_PREFIX/usr/share/doublecmd/doc
    ln -sf ../../share/doublecmd/doc $DC_INSTALL_DIR/doc
    # Copy scripts
    install -d         $DC_INSTALL_DIR/scripts
    cp -a scripts/*.py $DC_INSTALL_DIR/scripts/
    # Copy languages
    cp -r language $DC_INSTALL_PREFIX/usr/share/doublecmd
    ln -sf ../../share/doublecmd/language $DC_INSTALL_DIR/language
    # Copy pixmaps
    cp -r pixmaps $DC_INSTALL_PREFIX/usr/share/doublecmd
    ln -sf ../../share/doublecmd/pixmaps $DC_INSTALL_DIR/pixmaps
    # Copy highlighters
    cp -r highlighters $DC_INSTALL_PREFIX/usr/share/doublecmd
    ln -sf ../../share/doublecmd/highlighters $DC_INSTALL_DIR/highlighters
    # Create symlink and desktop files
    install -d $DC_INSTALL_PREFIX/usr/bin
    install -d $DC_INSTALL_PREFIX/usr/share/pixmaps
    install -d $DC_INSTALL_PREFIX/usr/share/applications
    install -d $DC_INSTALL_PREFIX/usr/share/icons/hicolor/scalable/apps
    ln -sf  ../lib$LIB_SUFFIX/doublecmd/doublecmd $DC_INSTALL_PREFIX/usr/bin/doublecmd
    install -m 644 doublecmd.png $DC_INSTALL_PREFIX/usr/share/pixmaps/doublecmd.png
    install -m 644 install/linux/doublecmd.desktop $DC_INSTALL_PREFIX/usr/share/applications/doublecmd.desktop
    ln -sf ../../../../doublecmd/pixmaps/mainicon/alt/dcfinal.svg \
           $DC_INSTALL_PREFIX/usr/share/icons/hicolor/scalable/apps/doublecmd.svg
    install -d $DC_INSTALL_PREFIX/usr/share/polkit-1/actions
    install -m 644 install/linux/org.doublecmd.root.policy $DC_INSTALL_PREFIX/usr/share/polkit-1/actions/
  else
    # Make portable version
    touch $DC_INSTALL_DIR/doublecmd.inf
    # Copy documentation
    mkdir -p $DC_INSTALL_DIR/doc
    cp -a doc/*.txt $DC_INSTALL_DIR/doc/
    # Copy script for execute portable version
    cp -a doublecmd.sh $DC_INSTALL_DIR/
    # Copy directories
    cp -r language     $DC_INSTALL_DIR/
    cp -r pixmaps      $DC_INSTALL_DIR/
    cp -r highlighters $DC_INSTALL_DIR/
    # Copy scripts
    install -d         $DC_INSTALL_DIR/scripts
    cp -a scripts/*.py $DC_INSTALL_DIR/scripts/
    # Copy libraries
    install -m 644 *.so*    $DC_INSTALL_DIR/
    # Copy DC icon
    cp -a doublecmd.png     $DC_INSTALL_DIR/doublecmd.png
fi
