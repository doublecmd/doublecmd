For using this plugin you need unrar library. You can download it from
http://www.rarlab.com/rar_add.htm

Windows:
  Download "UnRAR.dll" - Self-extracting archive UnRARDLL.exe, unpack it
  and copy unrar.dll in Double Commander (or %windir%\system32) directory  
Linux:
   Download "UnRAR source" unrarsrc-x.x.x.tar.gz, unpack it:
   $ tar -xf unrarsrc-x.x.x.tar.gz
   go to "unrar" directory:
   $ cd unrar
   make symlink makefile.unix -> makefile:
   $ ln -s makefile.unix makefile
   set CXX environment variable to "g++ -DSILENT"
   $export CXX="g++ -DSILENT"
   and build library: 
   $ make lib
   After compiling, copy "libunrar.so" in "/usr/lib" directory:
   $ cp libunrar.so /usr/lib/libunrar.so