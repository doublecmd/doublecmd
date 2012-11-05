@echo Clean up output directory
@del /Q /S units\*.*
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
@del /Q /S plugins\dsx\DSXLocate\lib\*.*

@del /Q /S plugins\wcx\cpio\lib\*.*
@del /Q /S plugins\wcx\deb\lib\*.*
@del /Q /S plugins\wcx\lzma\lib\*.*
@del /Q /S plugins\wcx\rpm\lib\*.*
@del /Q /S plugins\wcx\unbz2\lib\*.*
@del /Q /S plugins\wcx\unrar\lib\*.*
@del /Q /S plugins\wcx\zip\lib\*.*

@del /Q /S plugins\wdx\deb_wdx\lib\*.*
@del /Q /S plugins\wdx\rpm_wdx\lib\*.*
@del /Q /S plugins\wdx\svn_wdx\lib\*.*
@del /Q /S plugins\wdx\xpi_wdx\lib\*.*

@del /Q /S plugins\wfx\ftp\lib\*.*
@del /Q /S plugins\wfx\gvfs\lib\*.*
@del /Q /S plugins\wfx\Network\lib\*.*
@del /Q /S plugins\wfx\samba\lib\*.*
@del /Q /S plugins\wfx\sample\lib\*.*

@del /Q /S plugins\wlx\simplewlx\lib\*.*
@del /Q /S plugins\wlx\WlxMplayer\lib\*.*

@echo Remove backup files
@del /Q plugins\wcx\cpio\src\*.bak
@del /Q plugins\wcx\rpm\src\*.bak
@del /Q plugins\wcx\deb\src\*.bak
@del /Q plugins\wcx\lzma\src\*.bak
@del /Q plugins\wcx\zip\src\*.bak
@del /Q plugins\wcx\unbz2\src\*.bak
@del /Q plugins\wcx\unrar\src\*.bak

@del /Q plugins\wcx\cpio\src\*.*~
@del /Q plugins\wcx\rpm\src\*.*~
@del /Q plugins\wcx\deb\src\*.*~
@del /Q plugins\wcx\lzma\src\*.*~
@del /Q plugins\wcx\zip\src\*.*~
@del /Q plugins\wcx\unbz2\src\*.*~
@del /Q plugins\wcx\unrar\src\*.*~

@echo Clean up components output directories

@del /Q /S components\chsdet\lib\*.*
@del /Q /S components\CmdLine\lib\*.*
@del /Q /S components\dcpcrypt\lib\*.*
@del /Q /S components\doublecmd\lib\*.*
@del /Q /S components\gifanim\lib\*.*
@del /Q /S components\KASToolBar\lib\*.*
@del /Q /S components\viewer\lib\*.*
@del /Q /S components\ZVDateTimeCtrls\lib\*.*

@echo Done.