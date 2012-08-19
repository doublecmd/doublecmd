{ **************************************************** }
{ MappingFile unit v1.0 for Delphi                     }
{ Copyright (c) Razumikhin Dmitry, 2005                }
{ E-mail: razumikhin_d@mail.ru                         }
{                                                      }
{ Use under terms LGPL license:                        }
{ http://www.gnu.org/copyleft/lesser.html              }
{ **************************************************** }
unit MappingFile;

interface
uses
  SysUtils, Windows, Classes, RTLConsts;

type
  EFileMappingError = class(Exception);

  TMappingFile = class
  private
    fHandle: Integer;        //file handle
    fFileName: string;       //file name
    fMode: Word;             //file open mode
    fMappingHandle: THandle; //handle of mapping file
    fBaseAddress: PChar;     //address of file image in memory
    fPos: Integer;           //current position
    fSize: Integer;          //size of real data
    fCapacity: Integer;      //size of allocated memory
    fExtraMem: Integer;       
    function GetChar(Index: Integer): Char;
    procedure SetSize(const Value: Integer);
    procedure SetChar(Index: Integer; const Value: Char);
    procedure TryMount;
    procedure ReMount;
    procedure SetCapacity(const Value: Integer);
    procedure SetPos(const Value: Integer);
  public
    property BaseAddress: PChar read fBaseAddress;
    property Size: Integer read fSize write SetSize;
    property Capacity: Integer read fCapacity write SetCapacity;
    property ExtraMem: Integer read fExtraMem write fExtraMem;
    property Ch[Index: Integer]: Char read GetChar write SetChar;
    property Position: Integer read fPos write SetPos;

    function Seek(Offset, Origin: Integer): Integer;
    //read functions
    function ReadCh(out Ch: Char): Boolean;
    function ReadStr(out Str: string; Len: Integer): Boolean; overload;
    function ReadStr(const Index, Len: Integer): string; overload;

    function Find(Ch: Char; StartIndex: Integer = 0): Integer;
    //write functions
    procedure WriteCh(const Ch: Char);
    procedure WriteStr(const Str: string); overload;
    procedure WriteStr(const Str: string; Index: Integer); overload;
    procedure WriteBuffer(const Buf: PChar; Count: Integer); overload;
    procedure WriteBuffer(const Buf: PChar; Index, Count: Integer); overload;

    //insert functions (expand + write)
    procedure InsertBuffer(const Buf: PChar; Count: Integer); overload;
    procedure InsertBuffer(const Buf: PChar; Index, Count: Integer); overload;
    procedure InsertStr(const Str: string); overload;
    procedure InsertStr(const Str: string; Index: Integer); overload;

    constructor Create(const FileName: string; Mode: Word);
    destructor Destroy; override;
  end;

function FileResize(Handle: THandle; Size: Integer): LongBool;
function FileMount(var MappingHandle: THandle; const FileHandle: Integer; ReadOnly: Boolean = True): Pointer;
procedure FileUmount(MappingHandle: THandle; BaseAddress: Pointer);

implementation

{ TMappingFile }

constructor TMappingFile.Create(const FileName: string; Mode: Word);
begin
  inherited Create;
  fFileName:=FileName;
  fMode:=Mode;
  fPos:=0;
  fSize:=0;
  fCapacity:=0;
  fExtraMem:=1024;
  fBaseAddress:=nil;
  if Mode = fmCreate then
    begin
    fHandle:=FileCreate(FileName);
    if fHandle < 0 then
      raise EFCreateError.CreateResFmt(@SFCreateErrorEx, [ExpandFileName(FileName), SysErrorMessage(GetLastError)]);
    end
  else
    begin
    fHandle:=FileOpen(FileName, Mode);
    if fHandle < 0 then
      raise EFOpenError.CreateResFmt(@SFOpenErrorEx, [ExpandFileName(FileName), SysErrorMessage(GetLastError)]);
    end;
  fSize:=GetFileSize(fHandle, nil);
  fCapacity:=fSize;
  TryMount;
end;

destructor TMappingFile.Destroy;
begin
  FileUmount(fMappingHandle, fBaseAddress);
  if fSize <> fCapacity then
    FileResize(fHandle, fSize);
  if fHandle >=0 then FileClose(fHandle);
  inherited;
end;

function TMappingFile.Find(Ch: Char; StartIndex: Integer): Integer;
var
  i: Integer;
begin
  for i:=StartIndex to fSize-1 do
    if Ch = PChar(fBaseAddress + i)^ then
      begin
      Result:=i;
      Exit;
      end;
  Result:=-1;
end;

function TMappingFile.GetChar(Index: Integer): Char;
begin
  Result:=PChar(fBaseAddress + Index)^; //Not control the bounds
end;

procedure TMappingFile.InsertBuffer(const Buf: PChar; Count: Integer);
begin
  InsertBuffer(Buf, fPos, Count);
  Inc(fPos, Count);
end;

procedure TMappingFile.InsertBuffer(const Buf: PChar; Index,
  Count: Integer);
var
  MoveCount: Integer;
begin
  if Count <> 0 then
    begin
    MoveCount:=fSize - Index;
    SetSize(fSize + Count);
    Move(PChar(fBaseAddress + Index)^, PChar(fBaseAddress + Index + Count)^, MoveCount);
    Move(Buf^, PChar(fBaseAddress + Index)^, Count);
    end;
end;

procedure TMappingFile.InsertStr(const Str: string);
begin
  InsertBuffer(PChar(Str), Length(Str));
end;

procedure TMappingFile.InsertStr(const Str: string; Index: Integer);
begin
  InsertBuffer(PChar(Str), Index, Length(Str));
end;

function TMappingFile.ReadCh(out Ch: Char): Boolean;
begin
  Result:=fPos < fSize;
  if Result then
    begin
    Ch:=PChar(fBaseAddress + fPos)^;
    Inc(fPos, SizeOf(Char));
    end
  else
    Ch:=#0;
end;

function TMappingFile.ReadStr(out Str: string; Len: Integer): Boolean;
begin
  Result:=(fPos + Len) <= fSize;
  SetLength(Str, Len);
  Move(PChar(fBaseAddress + fPos)^, Str[1], Len);
  Inc(fPos, Len);
end;

function TMappingFile.ReadStr(const Index, Len: Integer): string;
begin
  SetLength(Result, Len);
  Move(PChar(fBaseAddress + Index)^, Result[1], Len);
end;

procedure TMappingFile.Remount;
begin
  if Assigned(fBaseAddress) then
    FileUmount(fMappingHandle, fBaseAddress);
  TryMount;
end;

function TMappingFile.Seek(Offset, Origin: Integer): Integer;
var
  NewPos: Integer;
begin
  Result:=-1;
  case Origin of
    0:
      begin
      if Offset >= 0 then
        begin
        if (Offset > fSize) then
          SetSize(Offset);
        fPos:=Offset;
        Result:=Offset;
        end;
      end;
    1:
      begin
      NewPos:= fPos + Offset;
      if NewPos >=0 then
        begin
        if (NewPos > fSize) then
          SetSize(NewPos);
        fPos:=NewPos;
        Result:=NewPos;
        end;
      end;
    2:
      begin
      NewPos:=fSize - Offset - 1;
      if NewPos >=0 then
        begin
        if (NewPos > fSize) then
          SetSize(NewPos);
        fPos:=NewPos;
        Result:=NewPos;
        end;
      end;
  end;
end;

procedure TMappingFile.SetCapacity(const Value: Integer);
begin
  if fCapacity <> Value then
    begin
    fCapacity := Value;
    FileResize(fHandle, fCapacity);
    Remount;
    end;
end;

procedure TMappingFile.SetChar(Index: Integer; const Value: Char);
begin
  PChar(fBaseAddress + Index)^:=Value;  //Not control the bounds
end;

procedure TMappingFile.SetPos(const Value: Integer);
begin
  Seek(Value, 0);
end;

procedure TMappingFile.SetSize(const Value: Integer);
begin
  if fSize <> Value then
    begin
    fSize := Value;
    if fPos >= fSize then fPos:=fSize - 1;
    if fSize > fCapacity then
      SetCapacity(fSize + fExtraMem);
    end;
end;

procedure TMappingFile.TryMount;
begin
  if fSize > 0 then
    begin
    fBaseAddress:=FileMount(fMappingHandle, fHandle, fMode = fmOpenRead);
    if not Assigned(fBaseAddress) then
      raise EFileMappingError.CreateFmt('Could not mapped file ''%s''',[fFileName]);
    end;
end;

procedure TMappingFile.WriteBuffer(const Buf: PChar;
  Count: Integer);
begin
  if (fPos + Count) > fSize then
    SetSize(fPos + Count);
  Move(Buf^, PChar(fBaseAddress + fPos)^, Count);
  fPos:=fPos + Count;
end;

procedure TMappingFile.WriteBuffer(const Buf: PChar; Index,
  Count: Integer);
begin
  if (Index + Count) > fSize then
    SetSize(Index + Count);
  Move(Buf^, PChar(fBaseAddress + Index)^, Count);
end;

procedure TMappingFile.WriteCh(const Ch: Char);
begin
  WriteBuffer(@Ch, SizeOf(Char));
end;

procedure TMappingFile.WriteStr(const Str: string);
begin
  WriteBuffer(PChar(Str), Length(Str));
end;

procedure TMappingFile.WriteStr(const Str: string; Index: Integer);
begin
  WriteBuffer(PChar(Str), Index, Length(Str));
end;

//-----------------------------------------------------------------------

function FileMount(var MappingHandle: THandle; const FileHandle: Integer; ReadOnly: Boolean = True): Pointer;
var
  FileMappingMode,
  MapViewMode: DWORD;
begin
  if ReadOnly then
    begin
    FileMappingMode:=PAGE_READONLY;
    MapViewMode:=FILE_MAP_READ;
    end
  else
    begin
    FileMappingMode:=PAGE_READWRITE;
    MapViewMode:=FILE_MAP_READ + FILE_MAP_WRITE;
    end;

  MappingHandle:=CreateFileMapping(FileHandle, nil, FileMappingMode, 0, 0, nil);
  if MappingHandle <> 0 then
    begin
    Result:=MapViewOfFile(MappingHandle, MapViewMode, 0, 0, 0);
    end
  else
    Result:=nil;
end;

procedure FileUmount(MappingHandle: THandle; BaseAddress: Pointer);
begin
  if Assigned(BaseAddress) then
    UnmapViewOfFile(BaseAddress);
  if MappingHandle <> 0 then
    CloseHandle(MappingHandle);
end;

function FileResize(Handle: THandle; Size: Integer): LongBool;
begin
  FileSeek(Handle, Size, 0);
  Result:=SetEndOfFile(Handle);
end;

end.

