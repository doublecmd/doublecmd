unit uNTFSLinks;
{
Create and read link(s) on NTFS.

*** Based on: ***
}
{ **** UBPFD *********** by kladovka.net.ru ****
>> —оздание hardlink и symbolic link.

»сходный код утилиты, котора€ создает hard и symbolic links почти как в unix.
Hardlink можно создать только дл€ файлов и только на NTFS.
Symbolic link можно создать только дл€ директориев и только на NTFS5 (Win2K/XP) и он не может указывать на сетевой ресурс.

«ависимости: Windows, SysUtils
јвтор:       Alex Konshin, akonshin@earthlink.net, Boston, USA
Copyright:   http://home.earthlink.net/~akonshin/files/xlink.zip
ƒата:        30 декабр€ 2002 г.
********************************************** }
{
*** and ***
}
//====================================================================
// Junction creation and listing utility, based on Junction.c source
// by Mark Russinovich, http://www.sysinternals.com. Thanks Mark!
//
// Note: targets of some rare reparse point types are not recognized,
// as in Mark's source.
//
// (C) Alexey Torgashin, http://alextpp.narod.ru, atorg@yandex.ru
// 18.02.06 - initial version
//====================================================================


interface

uses
  Windows, SysUtils;

type

  TOptions = set of (optSymbolicLink,optOverwrite,optRecursive,optDirectory);

  int64rec = packed record
    lo: LongWord;
    hi: LongInt;
  end;
  
   TReparsePointType = (
    slUnknown,
    slJunction,
    slMountPoint,
    slSymLink,
    slHSM,
    slSIS,
    slDFS
    );

const
  FILE_DOES_NOT_EXIST = DWORD(-1);
  FILE_ATTRIBUTE_DEVICE               = $00000040;
  FILE_ATTRIBUTE_SPARSE_FILE          = $00000200;
  FILE_ATTRIBUTE_REPARSE_POINT        = $00000400;
  FILE_ATTRIBUTE_OFFLINE              = $00001000;
  FILE_ATTRIBUTE_NOT_CONTENT_INDEXED  = $00002000;
  FILE_ATTRIBUTE_ENCRYPTED            = $00004000;
  SReparsePointType: array[TReparsePointType] of PChar = (
    'Unknown point type',
    'Junction',
    'Mount Point',
    'Symbolic Link',
    'Hierarchical Storage Management point',
    'Single Instance Store point',
    'Distributed File System point'
    );

(* To create symbolic link (works on Windows 2k/XP for directories only) *)
function CreateSymlink( ATargetName, ALinkName: WideString; const options: TOptions = []): Boolean;
(* To create hardlink(s) (works only for files) *)
procedure CreateHardlink( AFileName, ALinkName: WideString; options: TOptions = []);

function FGetSymlinkInfo(const fn: WideString; var Target: WideString; var LinkType: TReparsePointType): boolean;
function FDriveSupportsSymlinks(const fn: WideString): boolean;

implementation
uses
  LCLProc;

//-------------------------------------------------------------
// warning: function assumes that it is correct directory name
function isDirectoryEmpty( const ADirectoryName: WideString ): Boolean;
var
  h: THandle;
  len : Integer;
  wFindData: TWin32FindDataW;
  wSeachMask : WideString;
begin
  len := Length(ADirectoryName);
  if (PWideChar(ADirectoryName)+len-1)^='\' then
    wSeachMask:= ADirectoryName+'*'
  else
    wSeachMask:= ADirectoryName+'\*';
  h:= Windows.FindFirstFileW( PWideChar(wSeachMask), wFindData );
  Result := (h=INVALID_HANDLE_VALUE);
  Windows.FindClose(h);
end;

//-------------------------------------------------------------
function SysErrorMessage( ErrorCode: Integer ): string;
var
  Len: Integer;
  Buffer: Array[0..255] of Char;
begin
  Len := FormatMessage(
    FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_ARGUMENT_ARRAY,
    nil, ErrorCode, 0, Buffer, SizeOf(Buffer), nil );
  while (Len>0) and (Buffer[Len-1] in [#0..#32, '.']) do Dec(Len);
  SetString( Result, Buffer, Len );
end;

//-------------------------------------------------------------
procedure _CreateHardlink( AFileName : WideString; ALinkName: WideString; overwrite: Boolean );
var
  aLinkFullName: Array[0..MAX_PATH] of WChar;
  pwFilePart: LPWSTR;
  hFileSource: THandle;
  rStreamId: WIN32_STREAM_ID;
  cbPathLen, dwStreamHeaderSize, dwBytesWritten: DWORD;
  lpContext: Pointer;
begin

  hFileSource :=
    Windows.CreateFileW(
      PWChar(AFileName),
      GENERIC_READ or GENERIC_WRITE,
      FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
      nil,
      OPEN_EXISTING,
      0,
      0
    );

  if hFileSource=INVALID_HANDLE_VALUE then 
    raise Exception.Create('Can''t open file "'+AFileName+'"');

  try
    cbPathLen := Windows.GetFullPathNameW( PWChar(ALinkName), MAX_PATH,
      aLinkFullName, pwFilePart );
    if cbPathLen<=0 then 
      raise Exception.Create('Invalid link name "'+ALinkName+'"');

    cbPathLen := (cbPathLen+1)*SizeOf(WChar);

    lpContext := nil;

    rStreamId.dwStreamId := BACKUP_LINK;
    rStreamId.dwStreamAttributes := 0;
    rStreamId.dwStreamNameSize := 0;
    int64rec(rStreamId.Size).hi := 0;
    int64rec(rStreamId.Size).lo := cbPathLen;
    dwStreamHeaderSize := PChar(@rStreamId.cStreamName)-PChar(@rStreamId)
      +LongInt(rStreamId.dwStreamNameSize);

    if not BackupWrite(
        hFileSource,
        Pointer(@rStreamId), // buffer to write
        dwStreamHeaderSize, // number of bytes to write
        dwBytesWritten,
        False, // don't abort yet
        False, // don't process security
        lpContext
      ) then RaiseLastOSError;

    if not BackupWrite(
        hFileSource,
        Pointer(@aLinkFullName), // buffer to write
        cbPathLen, // number of bytes to write
        dwBytesWritten,
        False, // don't abort yet
        False, // don't process security
        lpContext
      ) then RaiseLastOSError;

    // free context
    if not BackupWrite(
        hFileSource,
        nil, // buffer to write
        0, // number of bytes to write
        dwBytesWritten,
        True, // abort
        False, // don't process security
        lpContext
      ) then RaiseLastOSError;

  finally
    CloseHandle(hFileSource);
  end;
end;

//-------------------------------------------------------------
// ADirName and ADirForLinks must not end with backslach
procedure _CreateHardlinksForSubDirectory( const ADirName, ADirForLinks: WideString; options: TOptions );
var
  h: THandle;
  wExistedFile, wLinkName : WideString;
  dwAttributes : DWORD;
  rFindData: TWin32FindDataW;
begin
  dwAttributes := GetFileAttributesW( PWideChar(ADirForLinks) );
  if dwAttributes=FILE_DOES_NOT_EXIST then
    begin
// WriteLN(output, 'Create Directory ',ADirForLinks);
      if not CreateDirectoryW(PWideChar(ADirForLinks), nil) then
        raise Exception.Create('Can''t create directory "'+ADirForLinks+'".');
    end
  else if (dwAttributes and FILE_ATTRIBUTE_DIRECTORY)=0 then 
    raise Exception.Create('File "'+ADirName
      +'" already exists and it is not a directory.');
  h := Windows.FindFirstFileW( PWideChar(ADirName+'\*'), rFindData );
  if h=INVALID_HANDLE_VALUE then Exit;
  try
    repeat
      if (rFindData.cFileName[0]='.') and 
         ( (rFindData.cFileName[1]=#0) or ((rFindData.cFileName[1]='.') and
           (rFindData.cFileName[2]=#0))) then Continue;
      wExistedFile := ADirName+'\'+rFindData.cFileName;
      wLinkName := ADirForLinks+'\'+rFindData.cFileName;
      if (rFindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY)=0 then
        begin
          _CreateHardlink( wExistedFile, wLinkName, optOverwrite in options );
        end
      else if optRecursive in options then
        begin
          _CreateHardlinksForSubDirectory(wExistedFile,wLinkName,options);
        end;

    until not Windows.FindNextFileW(h,rFindData);
  finally
    Windows.FindClose(h);
  end;
end;

//-------------------------------------------------------------
procedure CreateHardlink( AFileName, ALinkName: WideString; options: TOptions );
var
  dwAttributes: DWORD;
begin
  dwAttributes := Windows.GetFileAttributesW(PWChar(AFileName));
  if dwAttributes=FILE_DOES_NOT_EXIST then 
    raise Exception.Create('File "'+AFileName+'" does not exist.');
  if (dwAttributes and FILE_ATTRIBUTE_DIRECTORY)<>0 then 
    raise Exception.Create('Can''t create hardlink for directory (file "'
      +AFileName+'").');

  dwAttributes := Windows.GetFileAttributesW(PWChar(ALinkName));
  if dwAttributes<>FILE_DOES_NOT_EXIST then
  begin
    if not(optOverwrite in options) then 
      raise Exception.Create('File "'+ALinkName+'" already exists.');
    if (dwAttributes and FILE_ATTRIBUTE_DIRECTORY)<>0 then 
      raise Exception.Create('Can''t overwrite directory "'+AFileName+'".');
  end;

  _CreateHardlink( AFileName, ALinkName, optOverwrite in options );

end;

//-------------------------------------------------------------
procedure CreateHardlinksForDirectory( const ADirName, ADirForLinks: WideString; options: TOptions );
var
  dwAttributes: DWORD;
  len : Integer;
  wDirName, wDirForLinks : WideString;
begin
  dwAttributes := Windows.GetFileAttributesW(PWChar(ADirName));
  if dwAttributes=FILE_DOES_NOT_EXIST then 
    raise Exception.Create('Directory "'+ADirName+'" does not exist.');
  if (dwAttributes and FILE_ATTRIBUTE_DIRECTORY)=0 then 
    raise Exception.Create('File "'+ADirName+'" is not a directory.');
  len := Length(ADirName);
  if (PWChar(ADirName)+len-1)^='\' then
    wDirName := Copy(ADirName,1,len-1)
  else 
    wDirName := ADirName;
  if (PWChar(ADirForLinks)+Length(ADirForLinks)-1)^<>'\' then
    wDirForLinks := ADirForLinks
  else 
    wDirForLinks := Copy(ADirForLinks,1,Length(ADirForLinks)-1);
  _CreateHardlinksForSubDirectory(wDirName,wDirForLinks,options);
end;

//-------------------------------------------------------------
procedure CreateHardlinksInDirectory( const AFileName, ADirForLinks: WideString; options: TOptions );
var
  dwAttributes: DWORD;
  len : Integer;
  wFileName, wDirForLinks, wLinkName : WideString;
begin
  dwAttributes := Windows.GetFileAttributesW(PWChar(AFileName));
  if dwAttributes=FILE_DOES_NOT_EXIST then 
    raise Exception.Create('File or directory "'+AFileName+'" does not exist.');
  if (dwAttributes and FILE_ATTRIBUTE_DIRECTORY)=0 then
    begin

      wLinkName := ADirForLinks+'\'+SysUtils.ExpandFileName(AFileName);
      dwAttributes := Windows.GetFileAttributesW(PWChar(wLinkName));
      if dwAttributes<>FILE_DOES_NOT_EXIST then
      begin
        if not(optOverwrite in options) then 
          raise Exception.Create('File "'+wLinkName+'" already exists.');
        if (dwAttributes and FILE_ATTRIBUTE_DIRECTORY)<>0 then 
          raise Exception.Create('Can''t overwrite directory "'+AFileName+'".');
      end;
      _CreateHardlink( AFileName, wLinkName, optOverwrite in options );
    end
  else
    begin
      len := Length(AFileName);
      if (PWChar(AFileName)+len-1)^='\' then
        wFileName := Copy(AFileName,1,len-1)
      else 
        wFileName := AFileName;
      if (PWChar(ADirForLinks)+Length(ADirForLinks)-1)^<>'\' then
        wDirForLinks := ADirForLinks
      else 
        wDirForLinks := Copy(ADirForLinks,1,Length(ADirForLinks)-1);
      _CreateHardlinksForSubDirectory(wFileName,wDirForLinks,options);
    end;
end;

//-------------------------------------------------------------
procedure DeleteDirectoryContent( const ADirName: WideString );
type
  PDirRef = ^TDirRef;
  PPDirRef = ^PDirRef;
  TDirRef = record
    Next : PDirRef;
    DirName : WideString;
  end;
var
  h: THandle;
  sFileName : WideString;
  pSubDirs : PDirRef;
  ppLast : PPDirRef;
  pDir : PDirRef;
  rFindData: TWin32FindDataW;
begin
  pSubDirs := nil;
  ppLast := @pSubDirs;
  h := Windows.FindFirstFileW( PWChar(ADirName+'\*'), rFindData );
  if h=INVALID_HANDLE_VALUE then Exit;
  try
    try
      repeat
        if (rFindData.cFileName[0]='.') and 
          ( (rFindData.cFileName[1]=#0) or ((rFindData.cFileName[1]='.') and
          (rFindData.cFileName[2]=#0))) then Continue;
        sFileName := ADirName+'\'+rFindData.cFileName;
        if (rFindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY)<>0 then
          begin
            New(pDir);
            with pDir^ do
            begin
              Next := nil;
              DirName := sFileName;
            end;
            ppLast^ := pDir;
            ppLast := @pDir^.Next;
          end
        else if not DeleteFileW(PWChar(sFileName)) then 
          raise Exception.Create('Can''t delete file "'+sFileName+'".');

      until not Windows.FindNextFileW(h,rFindData);
    finally
      Windows.FindClose(h);
    end;
    if pSubDirs<>nil then
    begin
      repeat
        pDir := pSubDirs;
        pSubDirs := pDir^.Next;
        sFileName := pDir^.DirName;
        Dispose(pDir);
        DeleteDirectoryContent(sFileName);
        if not RemoveDirectoryW(PWChar(sFileName)) then 
          raise Exception.Create('Can''t delete directory "'+sFileName+'".');
      until pSubDirs=nil;
    end;
  except
    while pSubDirs<>nil do
    begin
      pDir := pSubDirs;
      pSubDirs := pDir^.Next;
      Dispose(pDir);
    end;
    raise;
  end;
end;

//-------------------------------------------------------------
const
  FILE_DEVICE_FILE_SYSTEM = $0009;
  // Define the method codes for how buffers are passed for I/O and FS controls
  METHOD_BUFFERED = 0;
  METHOD_IN_DIRECT = 1;
  METHOD_OUT_DIRECT = 2;
  METHOD_NEITHER = 3;

  // Define the access check value for any access
  FILE_ANY_ACCESS = 0;
  FILE_READ_DATA = 1;
  FILE_WRITE_DATA = 2;

  FSCTL_SET_REPARSE_POINT = (FILE_DEVICE_FILE_SYSTEM shl 16) or
    (FILE_ANY_ACCESS shl 14) or (41 shl 2) or (METHOD_BUFFERED);
  FSCTL_GET_REPARSE_POINT = (FILE_DEVICE_FILE_SYSTEM shl 16) or
    (FILE_ANY_ACCESS shl 14) or (42 shl 2) or (METHOD_BUFFERED);
  FSCTL_DELETE_REPARSE_POINT = (FILE_DEVICE_FILE_SYSTEM shl 16) or
    (FILE_ANY_ACCESS shl 14) or (43 shl 2) or (METHOD_BUFFERED);

  FILE_FLAG_OPEN_REPARSE_POINT = $00200000;


  REPARSE_MOUNTPOINT_HEADER_SIZE = 8;

  MAX_REPARSE_SIZE = 17000;
  MAX_NAME_LENGTH = 1024;


  IO_REPARSE_TAG_RESERVED_ZERO  = $000000000;
  IO_REPARSE_TAG_SYMBOLIC_LINK  = IO_REPARSE_TAG_RESERVED_ZERO;
  IO_REPARSE_TAG_RESERVED_ONE   = $000000001;
  IO_REPARSE_TAG_RESERVED_RANGE = $000000001;
  IO_REPARSE_TAG_VALID_VALUES   = $0E000FFFF;
  IO_REPARSE_TAG_HSM            = $0C0000004;
  IO_REPARSE_TAG_NSS            = $080000005;
  IO_REPARSE_TAG_NSSRECOVER     = $080000006;
  IO_REPARSE_TAG_SIS            = $080000007;
  IO_REPARSE_TAG_DFS            = $080000008;
  IO_REPARSE_TAG_MOUNT_POINT    = $0A0000003;


  FILE_SUPPORTS_REPARSE_POINTS = $00000080;

type
  REPARSE_MOUNTPOINT_DATA_BUFFER = packed record
    ReparseTag : DWORD;
    ReparseDataLength : DWORD;
    Reserved : Word;
    ReparseTargetLength : Word;
    ReparseTargetMaximumLength : Word;
    Reserved1 : Word;
    ReparseTarget : Array [0..0] of WChar;
  end;
  TReparseMountpointDataBuffer = REPARSE_MOUNTPOINT_DATA_BUFFER;
  PReparseMountpointDataBuffer = ^TReparseMountpointDataBuffer;

  REPARSE_DATA_BUFFER = packed record
    ReparseTag: DWORD;
    ReparseDataLength: Word;
    Reserved: Word;
    SubstituteNameOffset: Word;
    SubstituteNameLength: Word;
    PrintNameOffset: Word;
    PrintNameLength: Word;
    PathBuffer: array[0..0] of WideChar;
  end;
  TReparseDataBuffer = REPARSE_DATA_BUFFER;
  PReparseDataBuffer = ^TReparseDataBuffer;

//-------------------------------------------------------------
function CreateSymlink( ATargetName, ALinkName: WideString; const options: TOptions ): Boolean;
const
  wNativeFileNamePrefix : WideString = '\??\';
var
  hLink : THandle;
  pReparseInfo : PReparseMountPointDataBuffer;
  BufSize : Integer;
  pwcLinkFileName : PWideChar;
  wTargetNativeFileName : WideString;
  dwBytesReturned : DWORD;
  dwAttributes : DWORD;
  bDirectoryCreated : Boolean;
begin
  Result := False;
  pReparseInfo := nil;
  hLink := INVALID_HANDLE_VALUE;
  bDirectoryCreated := False;

  try
    pwcLinkFileName:= PWideChar(ALinkName);
    dwAttributes := Windows.getFileAttributesW( pwcLinkFileName );
    if dwAttributes<>FILE_DOES_NOT_EXIST then
    begin
      if not(optOverwrite in options) then
        begin
          if (dwAttributes and FILE_ATTRIBUTE_DIRECTORY)=0 then 
            raise Exception.Create('The file "'+ALinkName+'" already exists');
          if not isDirectoryEmpty(ALinkName) then 
            raise Exception.Create(
              'The directory "'+ALinkName+'" already exists and is not empty');
          dwAttributes := FILE_DOES_NOT_EXIST;
        end
      else if ((dwAttributes and FILE_ATTRIBUTE_DIRECTORY)=0) then
        begin
          if not DeleteFileW(pwcLinkFileName) then
            raise Exception.Create('Can''t overwrite file "'+ALinkName+'"');
          dwAttributes := FILE_DOES_NOT_EXIST;
        end
      else if (dwAttributes and FILE_ATTRIBUTE_REPARSE_POINT)
               <>FILE_ATTRIBUTE_REPARSE_POINT then
        if not isDirectoryEmpty(ALinkName) then
          begin
            if not(optDirectory in options) then 
              raise Exception.Create('Can''t overwrite non-empty directory "'
                +ALinkName+'"');
            DeleteDirectoryContent(ALinkName);
          end;
    end;
    if dwAttributes=FILE_DOES_NOT_EXIST then
    begin
      Windows.CreateDirectoryW( pwcLinkFileName, nil );
      bDirectoryCreated := True;
    end;

    try
      hLink := Windows.CreateFileW( pwcLinkFileName, GENERIC_WRITE, 0, nil,
          OPEN_EXISTING,
          FILE_FLAG_OPEN_REPARSE_POINT or FILE_FLAG_BACKUP_SEMANTICS, 0 );

      if hLink=INVALID_HANDLE_VALUE then RaiseLastOSError;

      //--------------------------------------------------------
      wTargetNativeFileName:= wNativeFileNamePrefix+ATargetName;
      BufSize:= (Length(wNativeFileNamePrefix)+Length(ATargetName)+1)*2+REPARSE_MOUNTPOINT_HEADER_SIZE+12;
      GetMem(pReparseInfo, BufSize);
      FillChar(pReparseInfo^, BufSize, 0);

      with pReparseInfo^ do
      begin
        Move(wTargetNativeFileName[1], ReparseTarget, (Length(wTargetNativeFileName)+1)*2);
        ReparseTag:= IO_REPARSE_TAG_MOUNT_POINT;
        ReparseTargetLength:= Length(wTargetNativeFileName)*2;
        ReparseTargetMaximumLength:= ReparseTargetLength+2;
        ReparseDataLength:= ReparseTargetLength+12;
      end;
      //--------------------------------------------------------

      dwBytesReturned := 0;
      if not DeviceIoControl( hLink, FSCTL_SET_REPARSE_POINT, pReparseInfo,
              pReparseInfo^.ReparseDataLength + REPARSE_MOUNTPOINT_HEADER_SIZE,
              nil, 0, dwBytesReturned, nil ) then RaiseLastOSError;

    except
      if bDirectoryCreated then RemoveDirectoryW( pwcLinkFileName );
      raise;
    end;

    Result := True;

  finally
    if hLink<>INVALID_HANDLE_VALUE then Windows.CloseHandle(hLink);
    if pReparseInfo<>nil then System.FreeMem(pReparseInfo);
  end;

end;

//-------------------------------------------------------------
{
procedure Execute;
var
  iArg : Integer;
  sArg : String;
  ptr : PChar;
  options : TOptions;
  sExistedFileName : String;
  sLink : String;
  dwAttrs : DWORD;
begin
  iArg := 1;
  repeat
    sArg := ParamStr(iArg);
    if sArg='' then Help;
    if PChar(sArg)^<>'-' then Break;
    ptr := PChar(sArg)+1;
    while ptr^<>#0 do
    begin
      case ptr^ of
      's','S': Include( options, optSymbolicLink );
      'h','H': Help;
      'F': options := options + [optOverwrite,optDirectory];
      'f': Include( options, optOverwrite );
      'r','R': Include( options, optRecursive );
      'd','D': Include( options, optDirectory );
      else
        WriteLN(output, 'Error: Invalid option ''-',ptr^,'''');
        Exit;
      end;
      Inc(ptr);
    end;
    Inc(iArg);
  until iArg<=ParamCount;

  if ParamCount<=iArg then Help;
  if ParamCount-iArg>1 then Include( options, optDirectory );

  if optSymbolicLink in options then
    begin
      sLink := ParamStr(ParamCount);
      repeat
        sExistedFileName := ParamStr(iArg);
        if not CreateSymlink( sExistedFileName, sLink, options ) then 
          WriteLN(output,  'The symbolic link creation failed.' );
        Inc(iArg);
      until iArg>=ParamCount;
    end
  else if (options*[optRecursive,optDirectory])<>[] then
    begin

      sLink := ParamStr(ParamCount);
      repeat
        sExistedFileName := ParamStr(iArg);
        CreateHardlinksInDirectory( sExistedFileName, sLink, options );

        Inc(iArg);
      until iArg>=ParamCount;

    end
  else
    begin

      sExistedFileName := ParamStr(iArg);
      sLink := ParamStr(ParamCount);
      dwAttrs := GetFileAttributes( PChar(sExistedFileName) );

      if dwAttrs=FILE_DOES_NOT_EXIST then
      begin
        WriteLN(output, 'Error: The source file does not exist');
        Exit;
      end;
      if (dwAttrs and FILE_ATTRIBUTE_DIRECTORY)<>0 then
      begin
        WriteLN(output, 'Error: Can''t create hardlink for directory');
        Exit;
      end;
      CreateHardlink( sExistedFileName, sLink, options );
    end;



end;
}


//-------------------------------------------------------------
const
  Prefix: WideString = '\??\';

//-------------------------------------------------------------
function FGetSymlinkInfo(const fn: WideString; var Target: WideString; var LinkType: TReparsePointType): boolean;
var
  attr: DWORD;
  h: THandle;
  reparseBuffer: array[0..MAX_REPARSE_SIZE-1] of char;
  reparseInfo: PReparseDataBuffer;
  reparseData: pointer;
  //reparseData1,
  reparseData2: PWChar;
  //name1,
  name2: array[0..MAX_NAME_LENGTH-1] of WideChar;
  returnedLength: DWORD;
  control: boolean;
begin
  Result:= false;
  Target:= '';
  LinkType:= slUnknown;

  attr:= GetFileAttributesW(PWChar(fn));
  if (attr and FILE_ATTRIBUTE_REPARSE_POINT)=0 then Exit;

  if (attr and FILE_ATTRIBUTE_DIRECTORY)<>0 then
    h:= CreateFileW(PWChar(fn), 0,
      FILE_SHARE_READ or FILE_SHARE_WRITE, nil,
      OPEN_EXISTING,
      FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OPEN_REPARSE_POINT, 0)
  else
    h:= CreateFileW(PWChar(fn), 0,
      FILE_SHARE_READ or FILE_SHARE_WRITE, nil,
      OPEN_EXISTING,
      FILE_FLAG_OPEN_REPARSE_POINT, 0);

  if h=INVALID_HANDLE_VALUE then
    begin
    DebugLn('CreateFileW failed');
    Exit
    end;

  reparseInfo:= @reparseBuffer;
  control:= DeviceIoControl(h, FSCTL_GET_REPARSE_POINT,
    nil, 0, reparseInfo, SizeOf(reparseBuffer),
    returnedLength, nil);
  CloseHandle(h);
  if not control then
    begin
    DebugLn('DeviceIoControl failed');
    Exit
    end;

  case reparseInfo^.ReparseTag of
    IO_REPARSE_TAG_MOUNT_POINT:
      begin
      reparseData:= @reparseInfo.PathBuffer;

      {
      FillChar(name1, SizeOf(name1), 0);
      reparseData1:= pointer(integer(reparseData)+reparseInfo.PrintNameOffset);
      lstrcpynW(name1, reparseData1, reparseInfo.PrintNameLength);
      }

      FillChar(name2, SizeOf(name2), 0);
      reparseData2:= pointer(integer(reparseData)+reparseInfo.SubstituteNameOffset);
      lstrcpynW(name2, reparseData2, reparseInfo.SubstituteNameLength);

      Target:= name2;
      if Pos(Prefix, Target)=1 then
        Delete(Target, 1, Length(Prefix));

      if Pos(':', Target)>0
        then LinkType:= slJunction
        else LinkType:= slMountPoint;

      Result:= true;
      end;

    IO_REPARSE_TAG_SYMBOLIC_LINK or $80000000:
      begin
      reparseData:= @reparseInfo.PathBuffer;

      {
      FillChar(name1, SizeOf(name1), 0);
      reparseData1:= pointer(integer(reparseData)+reparseInfo.PrintNameOffset);
      lstrcpynW(name1, reparseData1, reparseInfo.PrintNameLength);
      }

      FillChar(name2, SizeOf(name2), 0);
      reparseData2:= pointer(integer(reparseData)+reparseInfo.SubstituteNameOffset);
      lstrcpynW(name2, reparseData2, reparseInfo.SubstituteNameLength);

      Target:= name2;
      LinkType:= slSymLink;
      Result:= true;
      end;

    IO_REPARSE_TAG_HSM:
      begin
      LinkType:= slHSM;
      Result:= true;
      end;

    IO_REPARSE_TAG_SIS:
      begin
      LinkType:= slSIS;
      Result:= true;
      end;

    IO_REPARSE_TAG_DFS:
      begin
      LinkType:= slDFS;
      Result:= true;
      end;
  end;
end;

//-------------------------------------------------------------
function FDriveSupportsSymlinks(const fn: WideString): boolean;
var
  disk: pchar;
  buf1, buf2: array[0..50] of char;
  Serial, NameLen, Flags: DWORD;
begin
  Result:= false;
  if (fn='') or (Pos(':\', fn)<>2) then Exit;
  disk:= pchar(fn[1]);
  FillChar(buf1, SizeOf(buf1), 0);
  FillChar(buf2, SizeOf(buf2), 0);
  if GetVolumeInformation(PChar(disk+':\'), @buf1, SizeOf(buf1),
    @Serial, NameLen, Flags, @buf2, SizeOf(buf2)) then
    Result:= (Flags and FILE_SUPPORTS_REPARSE_POINTS)<>0;
end;

end.
