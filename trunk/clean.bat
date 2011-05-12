rem Clean up output directory
del /Q units\i386-win32-win32\*.*
del /Q units\x86_64-win64-win32\*.*
del /Q src\*.*~
del /Q src\*.~*
del /Q doublecmd*.exe

rem Remove generated help files
del /Q doc\en\dev-help\*.*

rem Clean up plugins output directories
del /Q plugins\wcx\cpio\lib\*.*
del /Q plugins\wcx\rpm\lib\*.*
del /Q plugins\wcx\deb\lib\*.*
del /Q plugins\wcx\lzma\lib\*.*
del /Q plugins\wcx\zip\lib\*.*
del /Q plugins\wcx\unbz2\lib\*.*
del /Q plugins\wcx\unrar\lib\*.*

rem Remove backup files
del /Q plugins\wcx\cpio\src\*.bak
del /Q plugins\wcx\rpm\src\*.bak
del /Q plugins\wcx\deb\src\*.bak
del /Q plugins\wcx\lzma\src\*.bak
del /Q plugins\wcx\zip\src\*.bak
del /Q plugins\wcx\unbz2\src\*.bak
del /Q plugins\wcx\unrar\src\*.bak

del /Q plugins\wcx\cpio\src\*.*~
del /Q plugins\wcx\rpm\src\*.*~
del /Q plugins\wcx\deb\src\*.*~
del /Q plugins\wcx\lzma\src\*.*~
del /Q plugins\wcx\zip\src\*.*~
del /Q plugins\wcx\unbz2\src\*.*~
del /Q plugins\wcx\unrar\src\*.*~

rem Clean up components output directories
del /Q components\CmdLine\lib\i386-win32\*.*
del /Q components\CmdLine\lib\x86_64-win64\*.*
del /Q components\KASToolBar\lib\i386-win32\*.*
del /Q components\KASToolBar\lib\x86_64-win64\*.*
del /Q components\viewer\lib\i386-win32\*.*
del /Q components\viewer\lib\x86_64-win64\*.*
