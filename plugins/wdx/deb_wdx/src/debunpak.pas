unit debunpak;

{$mode delphi}{$H+}

interface

uses
  SysUtils, Classes;

// Extract 'control' from control.tar.*
function Deb_ExtractCtrlInfoFile(const DebFile: String; var DescFile: String): Boolean;

implementation

uses
  dpkg_deb, libtar, AbXz, ZStream;

var
  DebPkg: TDebianPackage;

const
  HEAD_CRC    = $02; { bit 1 set: header CRC present }
  EXTRA_FIELD = $04; { bit 2 set: extra field present }
  ORIG_NAME   = $08; { bit 3 set: original file name present }
  COMMENT     = $10; { bit 4 set: file comment present }

type
  TGzHeader = packed record
    ID1        : Byte;
    ID2        : Byte;
    Method     : Byte;
    Flags      : Byte;
    ModTime    : UInt32;
    XtraFlags  : Byte;
    OS         : Byte;
  end;

function ExtractGzip(InStream, OutStream: TStream): Boolean;
var
  ALength: Integer;
  AHeader: TGzHeader;
  ABuffer: array[Word] of Byte;
begin
  Result:= False;
  InStream.ReadBuffer(AHeader, SizeOf(TGzHeader));
  if (AHeader.ID1 = $1F) and (AHeader.ID2 = $8B) and (AHeader.Method = 8) then
  begin
    // Skip the extra field
    if (AHeader.Flags and EXTRA_FIELD <> 0) then
    begin
      ALength:= InStream.ReadWord;
      while ALength > 0 do
      begin
        InStream.ReadByte;
        Dec(ALength);
      end;
    end;
    // Skip the original file name
    if (AHeader.Flags and ORIG_NAME <> 0) then
    begin
      while (InStream.ReadByte > 0) do;
    end;
    // Skip the .gz file comment
    if (AHeader.Flags and COMMENT <> 0) then
    begin
      while (InStream.ReadByte > 0) do;
    end;
    // Skip the header crc
    if (AHeader.Flags and HEAD_CRC <> 0) then
    begin
      InStream.ReadWord;
    end;
    with TDecompressionStream.Create(InStream, True) do
    try
      while True do
      begin
        ALength:= Read(ABuffer[0], SizeOf(ABuffer));
        if (ALength = 0) then Break;
        OutStream.Write(ABuffer[0], ALength);
      end;
      Result:= True;
    finally
      Free;
    end;
  end;
end;

function ExtractXz(InStream, OutStream: TStream): Boolean;
begin
  with TLzmaDecompression.Create(InStream, OutStream) do
  try
    Result:= Code();
  finally
    Free;
  end;
end;

function UnpackDebFile(const DebFile: String; MemberIdx: Integer; OutStream: TStream): Boolean;
var
  Index: Integer;
  FileExt: String;
  TempStream: TMemoryStream;
begin
  Result:= False;
  if (MemberIdx in [MEMBER_CONTROL, MEMBER_DATA]) then
  try
    // a debian package must have control.tar.* and data.tar.*
    if DebPkg.ReadFromFile(DebFile) < 2 then Exit;

    // Check file type
    FileExt:= TrimRight(DebPkg.FMemberList[MemberIdx].ar_name);
    Index:= Pos(ExtensionSeparator, FileExt);
    if Index = 0 then Exit;
    FileExt:= Copy(FileExt, Index, MaxInt);

    if (FileExt = '.tar.xz') then
    begin
      TempStream:= TMemoryStream.Create;
      try
        if DebPkg.ExtractMemberToStream(MemberIdx, TempStream) then
        begin
          TempStream.Position:= 0;
          Result:= ExtractXz(TempStream, OutStream);
        end;
      finally
        TempStream.Free;
      end;
    end;

    if (FileExt = '.tar.gz') then
    begin
      TempStream:= TMemoryStream.Create;
      try
        if DebPkg.ExtractMemberToStream(MemberIdx, TempStream) then
        begin
          TempStream.Position:= 0;
          Result:= ExtractGzip(TempStream, OutStream);
        end;
      finally
        TempStream.Free;
      end;
    end;
  except
    Result:= False;
  end;
end;

function Deb_ExtractCtrlInfoFile(const DebFile: String; var DescFile: String): Boolean;
var
  TA: TTarArchive;
  DirRec: TTarDirRec;
  OutStream: TMemoryStream;
begin
  Result:= False;

  OutStream:= TMemoryStream.Create;
  try
    Result:= UnpackDebFile(DebFile, MEMBER_CONTROL, OutStream);
    if Result then
    try
      TA := TTarArchive.Create(OutStream);
      try
        while TA.FindNext(DirRec) do
        begin
          if (DirRec.Name = './control') or (DirRec.Name = '.\control') or (DirRec.Name = 'control') then
          begin
            DescFile:= TA.ReadFile;
            Result:= True;
            Break;
          end;
        end;
      finally
        TA.Free;
      end;
    except
      // Ignore
    end;
  finally
    OutStream.Free;
  end;
end;

initialization
  DebPkg := TDebianPackage.Create;

finalization
  DebPkg.Free;

end.

