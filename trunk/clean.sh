#!/bin/sh

rm -f units/i386-linux-gtk2/*
rm -f units/i386-linux-qt/*
rm -f units/x86_64-linux-gtk2/*
rm -f units/x86_64-linux-qt/*

# Clean up components output directories
rm -rf components/chsdet/lib/*
rm -rf components/CmdLine/lib/*
rm -rf components/dcpcrypt/lib/*
rm -rf components/doublecmd/lib/*
rm -rf components/gifanim/lib/*
rm -rf components/KASToolBar/lib/*
rm -rf components/viewer/lib/*
rm -rf components/ZVDateTimeCtrls/lib/*

# Clean up all temporary files
find . -iname '*.compiled' -delete
find . -iname '*.ppu' -delete
find . -iname '*.o' -delete
find plugins -iname '*.w?x' -delete
find plugins -iname '*.dsx' -delete
find plugins -iname '*.or'  -delete
rm -f src/doublecmd.res doublecmd
rm -f tools/extractdwrflnfo
rm -f plugins/wcx/zip/lib/ZipConfDlg.lfm
rm -f plugins/wcx/zip/lib/abresstring.rst
rm -f plugins/wfx/ftp/lib/FtpConfDlg.lfm
rm -f plugins/wfx/samba/lib/smbauthdlg.lfm

# Remove debug files
rm -f  doublecmd.zdli doublecmd.dbg
rm -rf doublecmd.dSYM
