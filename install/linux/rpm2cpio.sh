#!/bin/sh

# Prevent gawk >= 4.0.x from getting funny ideas wrt UTF in printf()
LANG=C

pkg=$1
if [ "$pkg" = "" -o ! -e "$pkg" ]; then
    echo "no package supplied" 1>&2
   exit 1
fi

leadsize=96
o=`expr $leadsize + 8`
set `od -j $o -N 8 -t u1 $pkg`
il=`expr 256 \* \( 256 \* \( 256 \* $2 + $3 \) + $4 \) + $5`
dl=`expr 256 \* \( 256 \* \( 256 \* $6 + $7 \) + $8 \) + $9`
# echo "sig il: $il dl: $dl"

sigsize=`expr 8 + 16 \* $il + $dl`
o=`expr $o + $sigsize + \( 8 - \( $sigsize \% 8 \) \) \% 8 + 8`
set `od -j $o -N 8 -t u1 $pkg`
il=`expr 256 \* \( 256 \* \( 256 \* $2 + $3 \) + $4 \) + $5`
dl=`expr 256 \* \( 256 \* \( 256 \* $6 + $7 \) + $8 \) + $9`
# echo "hdr il: $il dl: $dl"

hdrsize=`expr 8 + 16 \* $il + $dl`
o=`expr $o + $hdrsize`

comp=`dd if="$pkg" ibs=$o skip=1 count=1 2>/dev/null \
      | dd bs=3 count=1 2>/dev/null`

gz="`echo . | awk '{ printf("%c%c", 0x1f, 0x8b); }'`"
lzma="`echo . | awk '{ printf("%cLZ", 0xff); }'`"
xz="`echo . | awk '{ printf("%c7z", 0xfd); }'`"
case "$comp" in
    BZh)      dd if="$pkg" ibs=$o skip=1 2>/dev/null | bunzip2 ;;
    "$gz"*)   dd if="$pkg" ibs=$o skip=1 2>/dev/null | gunzip ;;
    "$xz"*)   dd if="$pkg" ibs=$o skip=1 2>/dev/null | xzcat ;;
    "$lzma"*) dd if="$pkg" ibs=$o skip=1 2>/dev/null | unlzma ;;
    *)        echo "Unrecognized rpm file: $pkg"; exit 1 ;;
esac
