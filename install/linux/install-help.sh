#!/bin/sh

if [ -z $1 ]
  then DC_HELP_INSTALL_DIR=$DC_INSTALL_PREFIX/opt/doublecmd/doc
  else DC_HELP_INSTALL_DIR=$1/doublecmd/doc
fi

# Clean help directory
rm -rf $DC_HELP_INSTALL_DIR/*

# Copy Russian help files
cp -r doc/ru $DC_HELP_INSTALL_DIR/
