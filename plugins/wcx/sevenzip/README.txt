SevenZip plugin can store configuration in two places:

1. If sevenzip.ini exists in plugin directory then plugin will use it

2. Otherwise it will store sevenzip.ini in commander configuration directory


SevenZip plugin search 7z.dll in next places:

1. Path from sevenzip.ini

  [Library]
  i386=<full path to 7z.dll 32 bit>
  x86_64=<full path to 7z.dll 64 bit>

2. Plugin directory

                   \i386\7z.dll
                   \x86_64\7z.dll
                   \7z.dll

3. Commander directory

4. Windows system directory

SevenZip plugin can load external codecs. Plugin searches codecs in
subdirectory "Codecs" near 7z.dll.
