{
Seksi Commander
----------------------------
Licence  : GNU GPL v 2.0
Author   : radek.cervinka@centrum.cz

Thread for search in files (called from frmSearchDlg)

contributors:

Alexander Koblov (Alexx2000@mail.ru)

}
{.$threading on}

unit uFindThread;
{$mode objfpc}{$H+}
{$DEFINE NOFAKETHREAD}
interface

uses
  Classes, StdCtrls, uFilter {$IFNDEF NOFAKETHREAD}, uFakeThread{$ENDIF};

type
{$IFDEF NOFAKETHREAD}
  TFindThread = class(TThread)
{$ELSE}
  TFindThread = class(TFakeThread)
{$ENDIF}
  
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
    FFindData:String;
    FCaseSens:Boolean;
    FFilter:TFilter; // filter mask object
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddFile;
    procedure WalkAdr(const sNewDir:String);
    procedure UpDateProgress;
    procedure SetFilterMask(const Val:String);
    function GetFilterMask:String;
    property FilterMask:String read GetFilterMask write SetFilterMask;
    property PathStart:String read FPathStart write FPathStart;
    property Items:TStrings write FItems;
    property FindInFiles:Boolean write FFindInFiles;
    property Status:TLabel read FStatus write FStatus;
    property Current:TLabel read FCurrent write FCurrent; // label current file
    property CaseSensitive:boolean read FCaseSens write FCaseSens;
    property FindData:String read FFindData write FFindData;
  end;


implementation

uses
  SysUtils, Dialogs, uLng{$IFNDEF WIN32}, uFindMmap, BaseUnix{$ENDIF};
{ TFindThread }


constructor TFindThread.Create;
begin
  writeln('thread b');
  inherited Create(True);
  FFilter:=TFilter.Create;
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
  if assigned(FFilter) then
    FreeAndNil(FFilter)
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
var
  fs:TFileStream;
  s:Char;
  i:Integer;
  lastPos:Integer;
begin
  Result:=False;
  if sData='' then Exit;
  fs:=TFileStream.Create(sFileName,fmOpenRead);
  if not bCase then
    sData:=AnsiUpperCase(sData);
  try
    try
      while fs.Position<=fs.Size do
      begin
        fs.Read(s,1);
        lastPos:=fs.Position;
        i:=1;
        if not bCase then
          s:=UpCase(s); // neni case sensitivni
        while (i<=length(sData)) and (s=sdata[i]) do
        begin
          fs.Read(s,1);
          inc(i);
        end;
        if i>length(sData) then
          Result:=True; // nalezeno
        if fs.Position<>lastPos then
          fs.Seek(lastPos,soFromBeginning);
      end; // while
    finally
      fs.Free;
    end;
  except
    On E:Exception do
      ShowMessage('Chyba (FindData):'+E.Message);
  end;
end;

{$ELSE} // *nix
begin
Result := FindMmap(sFileName, sData, bCase);
end;
{$ENDIF}

procedure TFindThread.WalkAdr(const sNewDir:String);
var
  sr: TSearchRec;
begin
  writeln(sNewDir);
  if not SetCurrentDir(sNewDir) then Exit;
  if FindFirst('*', faAnyFile, sr)<>0 then Exit;
  repeat
    if (sr.Name='.') or (sr.Name='..') then Continue;
    inc(FFilesScaned);
    writeln(sr.Name);
    if FFindInFiles then
    begin
     if FFilter.CheckFileMask(sr.Name) and  FindInFile(sNewDir+DirectorySeparator+sr.Name,FFindData,FCaseSens) then
      begin
        fFoundFile:=sNewDir+DirectorySeparator+sr.Name;
        Synchronize(@AddFile);
      end;
    end
    else
      if FFilter.CheckFileMask(sr.Name)then
      begin
        fFoundFile:=sNewDir+DirectorySeparator+sr.Name;
        Synchronize(@AddFile);
      end;
    FCurrentFile:=sNewDir+DirectorySeparator+sr.Name;
    Synchronize(@UpDateProgress);
    if (sr.Attr and faDirectory)>0 then
      WalkAdr(sNewDir+DirectorySeparator+sr.Name);
  until (FindNext(sr)<>0)or terminated;
  FindClose(sr);
end;

procedure TFindThread.SetFilterMask(const Val:String);
begin
  FFilter.FileMask:=Val;
end;

function TFindThread.GetFilterMask:String;
begin
  Result:=FFilter.FileMask;
end;

end.
