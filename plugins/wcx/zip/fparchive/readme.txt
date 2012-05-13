Abbrevia

Version: 5.0  
Revision: 512  
Home Page: http://tpabbrevia.sourceforge.net

Abbrevia is a compression toolkit for Embarcadero Delphi, C++ Builder, and
Kylix and FreePascal. It supports PKZip, Microsoft CAB, tar, gzip, bzip2 and
zlib compression formats, and the creation of self-extracting executables.
It includes several visual components that simplify displaying zip files.

Some modifications done for Double Commander (see doublecmd.diff).

NOTES:

 Functions AbDetectCharSet and IsOEM from AbCharset unit fails with some code
 pages and characters (eg. 936 and 图片) ! Don't use it when merging with Abbrevia.
 Better to try to convert with MultiByteToWideChar (see DCConvertEncoding CeTryEncode
 and CeTryDecode).

 Abbrevia sets current directory before reading files from disk in case paths are relative
 and uses ExpandFileName (which relies on current directory) to change relative paths
 to absolute. Since Double Commander uses the toolkit from a non-main thread it cannot
 rely on current directory not changing while working. Instead, always full paths
 in archive items are used, both archive file name and disk file name, paths are rebased
 against TAbArchive.BaseDirectory (which doesn't change during working) and all calls
 to functions changing current directory have been removed.
