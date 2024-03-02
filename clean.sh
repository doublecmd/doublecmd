#!/bin/sh

# Clean up output directories
rm -f units/*/*

# Clean up components output directories
rm -rf components/chsdet/lib/*
rm -rf components/kascrypt/lib/*
rm -rf components/doublecmd/lib/*
rm -rf components/gifanim/lib/*
rm -rf components/Image32/lib/*
rm -rf components/KASToolBar/lib/*
rm -rf components/multithreadprocs/lib/*
rm -rf components/viewer/lib/*
rm -rf components/synunihighlighter/lib/*
rm -rf components/virtualterminal/lib/*

# Clean up all temporary files
find . -iname '*.compiled' -delete
find . -iname '*.ppu' -delete
find . -iname '*.o' -delete
find plugins -iname '*.w?x' -delete
find plugins -iname '*.dsx' -delete
find plugins -iname '*.or'  -delete
find plugins -iname '*.res' -not -path "*/sevenzip/src/*" -delete
find plugins -iname '*.a'  -delete
rm -f src/doublecmd.res doublecmd
rm -f tools/extractdwrflnfo
rm -f plugins/wcx/unrar/lib/rarconfdlg.lfm
rm -f plugins/wcx/unrar/lib/rarlng.rsj
rm -f plugins/wcx/zip/lib/ZipConfDlg.lfm
rm -f plugins/wcx/zip/lib/ZipLng.rsj
rm -f plugins/wcx/zip/lib/abresstring.rs?
rm -f plugins/wfx/ftp/lib/FtpConfDlg.lfm
rm -f plugins/wfx/ftp/lib/ftppropdlg.lfm
rm -f plugins/wfx/samba/lib/smbauthdlg.lfm

# Remove debug files
rm -f  doublecmd.zdli doublecmd.dbg
rm -rf doublecmd.dSYM
