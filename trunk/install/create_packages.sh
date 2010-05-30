#!/bin/sh

# Set Double Commander version
DC_VER=0.4.6

# The new package will be saved here
PACK_DIR=$(pwd)/linux/release

# Temp dir for creating *.tar.bz2 package
BUILD_PACK_DIR=/var/tmp/doublecmd-$(date +%y.%m.%d)

# Create temp dir for building
BUILD_DC_TMP_DIR=/var/tmp/doublecmd-$DC_VER

help()
{
         echo 'Usage: create_packages.sh [options]'
         echo
         echo "Options:"
         echo '-A:               All packages (by default)'
         echo '-D:               Debian package'
         echo '-R:               RPM package'
         echo '-S:               Slackware package'
         echo '-P:               Portable package'
         echo '-H:               Help package'
         echo '--cpu=<cpu>:      Target CPU'
         echo '--ws=<widgetset>: Target widgetset'
         echo
         exit 1
}

# Parse input parameters
CKNAME=$(basename "$0")
args=$(getopt -n $CKNAME -o ADRSPHh -l cpu:,ws:,help,default -- "$@")
eval set -- $args
while [ "$1" != "--" ]; do
  case "$1" in
        -h|--help) help;;
        -A) shift;CK_DEBIAN=1;CK_REDHAT=1;CK_SLACKWARE=1;CK_PORTABLE=1;CK_HELP=1;;
        -D) shift;CK_DEBIAN=1;;
        -R) shift;CK_REDHAT=1;;
        -S) shift;CK_SLACKWARE=1;;
        -P) shift;CK_PORTABLE=1;;
        -H) shift;CK_HELP=1;;
        --cpu) shift;CPU_TARGET=$(eval echo $1);shift;;
        --ws) shift;lcl=$(eval echo $1);shift;;
  esac
done

if [ -z "$CK_DEBIAN" ] && [ -z "$CK_REDHAT" ] && [ -z "$CK_SLACKWARE" ] && [ -z "$CK_PORTABLE" ] && [ -z "$CK_HELP" ]; then
   CK_DEBIAN=1
   CK_REDHAT=1
   CK_SLACKWARE=1
   CK_PORTABLE=1
   CK_HELP=1
fi

# Export from SVN
rm -rf $BUILD_DC_TMP_DIR
svn export ../ $BUILD_DC_TMP_DIR

# Save revision number
mkdir $BUILD_DC_TMP_DIR/.svn
cp -a ../.svn/entries $BUILD_DC_TMP_DIR/.svn/

# Copy package description file
cp linux/description-pak $BUILD_DC_TMP_DIR/

# Set widgetset
if [ -z $lcl ]; then
   export lcl=gtk2
fi

# Set processor architecture
if [ -z $CPU_TARGET ]; then
   export CPU_TARGET=$(fpc -iTP)
fi

# Debian package architecture
if [ "$CPU_TARGET" = "x86_64" ]
  then
    export DEB_ARCH="amd64"
  else
    export DEB_ARCH=$CPU_TARGET
fi

# Copy libraries
cp -a linux/lib/$CPU_TARGET/*.so         $BUILD_DC_TMP_DIR/
cp -a linux/lib/$CPU_TARGET/$lcl/*.so    $BUILD_DC_TMP_DIR/

cd $BUILD_DC_TMP_DIR

# Build all components of Double Commander
./_make.sh all

# Export variables for checkinstall
export MAINTAINER="Alexander Koblov <Alexx2000@mail.ru>"

if [ "$CK_REDHAT" ]; then
  # Create *.rpm package
  checkinstall -R --default --pkgname=doublecmd --pkgversion=$DC_VER --pkgarch=$CPU_TARGET --pkgrelease=1.$lcl --pkglicense=GPL --pkggroup=Applications/File --nodoc --pakdir=$PACK_DIR $BUILD_DC_TMP_DIR/install/linux/install.sh
fi

if [ "$CK_DEBIAN" ]; then
  # Create *.deb package
  checkinstall -D --default --pkgname=doublecmd --pkgversion=$DC_VER --pkgarch=$DEB_ARCH --pkgrelease=1.$lcl --pkglicense=GPL --pkggroup=contrib/misc --requires=libx11-6 --nodoc --pakdir=$PACK_DIR $BUILD_DC_TMP_DIR/install/linux/install.sh
fi

if [ "$CK_SLACKWARE" ]; then
  # Create *.tgz package
  checkinstall -S --default --pkgname=doublecmd --pkgversion=$DC_VER --pkgarch=$CPU_TARGET --pkgrelease=1.$lcl --pkglicense=GPL --pkggroup=Applications/File --nodoc --pakdir=$PACK_DIR $BUILD_DC_TMP_DIR/install/linux/install.sh
fi

if [ "$CK_PORTABLE" ]; then
  # Create *.tar.bz2 package
  mkdir -p $BUILD_PACK_DIR
  install/linux/install.sh --portable-prefix=$BUILD_PACK_DIR
  cd $BUILD_PACK_DIR
  sed -i -e 's/UseIniInProgramDir=0/UseIniInProgramDir=1/' doublecmd/doublecmd.ini
  tar -cvjf $PACK_DIR/doublecmd-$DC_VER-1.$lcl.$CPU_TARGET.tar.bz2 doublecmd
fi

if [ "$CK_HELP" ]; then
  # Create help packages
  cd $BUILD_DC_TMP_DIR
  # Copy help files
  install/linux/install-help.sh --portable-prefix=$BUILD_PACK_DIR
  # Create help package for each language
  cd $BUILD_PACK_DIR/doublecmd/doc
  for HELP_LANG in `ls`
    do
      cd $BUILD_PACK_DIR/doublecmd
      tar -cvjf $PACK_DIR/doublecmd-help.$HELP_LANG-$DC_VER.noarch.tar.bz2 doc/$HELP_LANG
    done
fi

# Clean DC build dir
rm -rf $BUILD_DC_TMP_DIR
rm -rf $BUILD_PACK_DIR
