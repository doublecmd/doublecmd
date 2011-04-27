#!/bin/sh -e

VERSION=$1
SEDSCRIPT="s/Last Changed Rev: \([0-9]\+\)/const RevisionStr = '\1';/p"

SVNVER=$(echo $VERSION | tr . _)
SVNURL=http://svn.freepascal.org/svn/lazarus/tags/lazarus_$SVNVER

DIR=lazarus-$VERSION
TAR=../lazarus_$VERSION.orig.tar.gz

svn export $SVNURL $DIR
# Add revision.inc
svn info $SVNURL | sed -ne "$SEDSCRIPT" > $DIR/ide/revision.inc
tar czf $TAR $DIR
rm -rf $DIR

# move to directory 'tarballs'
if [ -r .svn/deb-layout ]; then
    . .svn/deb-layout
    mv $TAR $origDir
    echo "moved $TAR to $origDir"
fi

