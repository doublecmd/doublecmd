#!/bin/sh

# Determine library directory
if [ "$CPU_TARGET" = "x86_64" ]
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
            DC_HELP_INSTALL_DIR=/$OS_LIB_DIR/doublecmd/doc
            ;;
        -P|--portable-prefix)
            shift
            DC_HELP_INSTALL_DIR=$(eval echo $1/doublecmd/doc)
            break
            ;;
        -I|--install-prefix)
            shift
            DC_INSTALL_PREFIX=$(eval echo $1)
            DC_HELP_INSTALL_DIR=$DC_INSTALL_PREFIX/$OS_LIB_DIR/doublecmd/doc
            break
            ;;
  esac
  shift
done

# Clean help directory
rm -rf $DC_HELP_INSTALL_DIR/*

# Copy English help files
cp -r doc/en   $DC_HELP_INSTALL_DIR/

# Copy Russian help files
cp -r doc/ru   $DC_HELP_INSTALL_DIR/
