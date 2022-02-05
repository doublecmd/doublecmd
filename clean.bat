@echo Clean up output directory
@del /Q /S units\i386-win32-win32\*.*
@del /Q /S units\x86_64-win64-win32\*.*
@del /Q src\*.*~
@del /Q src\*.~*
@del /Q doublecmd.dbg
@del /Q doublecmd.zdli
@del /Q doublecmd*.exe
@del /Q doublecmd*.old

@echo Remove generated help files
@del /Q doc\en\dev-help\*.*

@echo Clean up tools output directories
@del /Q /S tools\lib\*.*
@del /Q tools\extractdwrflnfo.exe

@echo Clean up plugins output directories
@del /Q /S plugins\*.dsx
@del /Q /S plugins\*.w?x

@del /Q /S plugins\dsx\DSXLocate\lib\*.*

@del /Q /S plugins\wcx\base64\lib\*.*
@del /Q /S plugins\wcx\cpio\lib\*.*
@del /Q /S plugins\wcx\deb\lib\*.*
@del /Q /S plugins\wcx\rpm\lib\*.*
@del /Q /S plugins\wcx\sevenzip\lib\*.*
@del /Q /S plugins\wcx\torrent\lib\*.*
@del /Q /S plugins\wcx\unbz2\lib\*.*
@del /Q /S plugins\wcx\unrar\lib\*.*
@del /Q /S plugins\wcx\zip\lib\*.*

@del /Q /S plugins\wdx\deb_wdx\lib\*.*
@del /Q /S plugins\wdx\rpm_wdx\lib\*.*
@del /Q /S plugins\wdx\audioinfo\lib\*.*

@del /Q /S plugins\wfx\ftp\lib\*.*
@del /Q /S plugins\wfx\gvfs\lib\*.*
@del /Q /S plugins\wfx\samba\lib\*.*
@del /Q /S plugins\wfx\sample\lib\*.*

@del /Q /S plugins\wlx\preview\lib\*.*
@del /Q /S plugins\wlx\simplewlx\lib\*.*
@del /Q /S plugins\wlx\WlxMplayer\lib\*.*

@echo Remove backup files
@del /Q /S plugins\*.*~
@del /Q /S plugins\*.bak

@echo Clean up components output directories

@del /Q /S components\chsdet\lib\*.*
@del /Q /S components\dcpcrypt\lib\*.*
@del /Q /S components\doublecmd\lib\*.*
@del /Q /S components\gifanim\lib\*.*
@del /Q /S components\KASToolBar\lib\*.*
@del /Q /S components\multithreadprocs\lib\*.*
@del /Q /S components\viewer\lib\*.*
@del /Q /S components\synunihighlighter\lib\*.*
@del /Q /S components\virtualterminal\lib\*.*

@echo Done.