{
Seksi Commander
----------------------------
Licence  : GNU GPL v 2.0
Author   : radek.cervinka@centrum.cz

Thread for search in files (called from frmSearchDlg)

contributors:

Alexander Koblov (Alexx2000@mail.ru)

}


unit uFindThread;
{$mode objfpc}{$H+}

interface

uses
  Classes, StdCtrls, uDCUtils, SysUtils;

type

{ TFindThread }

TFindThread = class(TThread)
  private
    { Private declarations }
    FPathStart:String;
    FItems: TStrings;
    FFindInFiles:Boolean;
    FStatus: TLabel;
    FCurrent: TLabel;
    FCurrentFile:String;
    FFilesScaned:Integer;
    FFoundFile:String;

    FFileMask : String;
    FAttributes: Cardinal;
    FCaseSens:Boolean;
    {Date search}
    FIsDateFrom,
    FIsDateTo : Boolean;
    FDateFrom,
    FDateTo : TDateTime;
    (* File size search *)
    FIsFileSizeFrom,
    FIsFileSizeTo : Boolean;
    FFileSizeFrom,
    FFileSizeTo : Integer;
    
    FFindData:String;

    function CheckFileDate(Date : LongInt) : Boolean;
    function CheckFileSize(FileSize : LongInt) : Boolean;
    function CheckFile(const Folder : String; const sr : TSearchRec) : Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddFile;
    procedure WalkAdr(const sNewDir:String);
    procedure UpDateProgress;
    property FilterMask:String read FFileMask write FFileMask;
    property PathStart:String read FPathStart write FPathStart;
    property Items:TStrings write FItems;
    property FindInFiles:Boolean write FFindInFiles;
    property Status:TLabel read FStatus write FStatus;
    property Current:TLabel read FCurrent write FCurrent; // label current file
    property CaseSensitive:boolean read FCaseSens write FCaseSens;
    property FindData:String read FFindData write FFindData;
    (* Date search *)
    property IsDateFrom:Boolean read FIsDateFrom write FIsDateFrom;
    property IsDateTo:Boolean read FIsDateTo write FIsDateTo;
    property DateFrom:TDateTime read FDateFrom write FDateFrom;
    property DateTo:TDateTime read FDateTo write FDateTo;
    (* File size search *)
    property IsFileSizeFrom : Boolean read FIsFileSizeFrom write FIsFileSizeFrom;
    property IsFileSizeTo : Boolean read FIsFileSizeTo write FIsFileSizeTo;
    property FileSizeFrom : LongInt read FFileSizeFrom write FFileSizeFrom;
    property FileSizeTo : LongInt read FFileSizeTo write FFileSizeTo;
    
    property Attributes: Cardinal read FAttributes write FAttributes default
      (faArchive or faReadonly or faHidden or faSysFile or faDirectory);
  end;


implementation

uses
  Dialogs, uLng{$IFNDEF WIN32}, uFindMmap, BaseUnix{$ENDIF};
{ TFindThread }


constructor TFindThread.Create;
begin
  writeln('thread b');
  inherited Create(True);
  //FFilter:=TFilter.Create;
  FCaseSens:=True;
  FFilesScaned:=0;
  FilterMask:='*';
  {$IFDEF WIN32}
  FPathStart:='C:\';
  {$ELSE}
  FPathStart:='/';
  {$ENDIF}
  FItems:=Nil;
end;

destructor TFindThread.Destroy;
begin
  //if assigned(FFilter) then
    //FreeAndNil(FFilter)
end;

procedure TFindThread.Execute;
var
  sCurrDir:String;
begin
  try
    writeln('thread b2');
    assert(Assigned(FItems),'assert:FItems is empty');
    Synchronize(@UpDateProgress);
    if FPathStart[length(FPathStart)]='/' then
      Delete(FPathStart,length(FPathStart),1);
    sCurrDir:=GetCurrentDir;
    try
        writeln('thread b',FPathStart);
      WalkAdr(FPathStart);
    finally
      ChDir(sCurrDir);
    end;  
  //  MessageBeep(1000);
    writeln('thread end');

  except
    on E:Exception do
      ShowMessage(E.Message);
  end;
end;

procedure TFindThread.AddFile;
begin
  FItems.Add(FFoundFile);
end;

procedure TFindThread.UpDateProgress;
begin
  FStatus.Caption:=Format(lngGetString(clngFindScaned),[FFilesScaned]);
  FCurrent.Caption:=FCurrentFile;
end;


function FindInFile(const sFileName:String; sData: String; bCase:Boolean): Boolean;
{$IFDEF WIN32}
const
  BufferSize = 4096;
var
    fs: TFileStream;
    lastPos, sDataLength,
    OffsetPos: Cardinal;
    Buffer: array[0..BufferSize-1] of Char;

    Compare: function(Str1, Str2: PChar; MaxLen: SizeInt): SizeInt;

  begin
    Result := False;
    if sData = '' then Exit;

    if bCase then
     Compare := @StrLIComp
    else
     Compare := @StrLComp;

    sDataLength := Length(sData);

    try
     fs := TFileStream.Create(sFileName, fmOpenRead or fmShareDenyNone);
     try
      repeat
        OffsetPos := fs.Read(Buffer, BufferSize) - sDataLength;
        lastPos := 0;
        while (not Result) and (lastPos <= OffsetPos) do
          begin
            Result := (Compare(PChar(sData), @Buffer[lastPos], sDataLength) = 0);
            inc(lastPos);
          end;

      until fs.Position >= fs.Size;
     except
     end;
    finally
      fs.Free;
    end;
  end;

{$ELSE} // *nix
begin
Result := FindMmap(sFileName, sData, bCase);
end;
{$ENDIF}

function TFindThread.CheckFileDate(Date : LongInt) : Boolean;
var
  DateTime: TDateTime;
begin
   Result := True;
   DateTime := FileDateToDateTime(Date);

   if FIsDateFrom then
      Result := (DateTime >= FDateFrom);

   if (FIsDateTo and Result) then
      Result := (DateTime <= FDateTo);

end;

function TFindThread.CheckFileSize(FileSize: LongInt): Boolean;
begin
   Result := True;
   if FIsFileSizeFrom then
      Result := (FileSize >= FFileSizeFrom);

   if (FIsFileSizeTo and Result) then
      Result := (FileSize <= FFileSizeTo);
end;

function TFindThread.CheckFile(const Folder : String; const sr : TSearchRec) : Boolean;
begin
  Result := True;
{$IFDEF WIN32}
(* This is hack *)
if not FileMaskEquate(sr.Name, FFileMask) then
   begin
     Result := False;
     Exit;
   end;
{$ENDIF}
  if (FIsDateFrom or FIsDateTo) then
      Result := CheckFileDate(sr.Time);

  if (FIsFileSizeFrom or FIsFileSizeTo) and Result then
      Result := CheckFileSize(sr.Size);

  if (FFindInFiles and Result) then
     Result := FindInFile(Folder + PathDelim + sr.Name, FFindData, FCaseSens);

end;

procedure TFindThread.WalkAdr(const sNewDir:String);
var
  sr: TSearchRec;
  Path : String;
begin
  writeln(sNewDir);
  if not SetCurrentDir(sNewDir) then Exit;

  Path := sNewDir + PathDelim + FFileMask;
  //WriteLN('Path = ', Path);

  if FindFirst(Path, faAnyFile, sr)<>0 then Exit;
  repeat
    if (sr.Name='.') or (sr.Name='..') then Continue;
    inc(FFilesScaned);
    //writeln(sr.Name);

      if CheckFile(sNewDir, sr) then
      begin
        fFoundFile:=sNewDir + PathDelim + sr.Name;
        Synchronize(@AddFile);
      end;
      
    FCurrentFile:=sNewDir + PathDelim + sr.Name;
    Synchronize(@UpDateProgress);
  until (FindNext(sr)<>0)or terminated;
  FindClose(sr);

    {Search in sub folders}
    if not Terminated then
    begin
      Path := sNewDir + PathDelim + '*';
      WriteLN('Search in sub folders = ', Path);
      if not Terminated and (FindFirst(Path, faDirectory, sr) = 0) then
        repeat
          if (sr.Name[1] <> '.') then
            WalkAdr(sNewDir + PathDelim + sr.Name);
        until Terminated or (FindNext(sr) <> 0);
      FindClose(sr);
    end;

end;

end.
