{
   Double Commander
   -------------------------------------------------------------------------
   Thread for search files (called from frmSearchDlg)

   Copyright (C) 2003-2004 Radek Cervinka (radek.cervinka@centrum.cz)
   Copyright (C) 2006-2007  Koblov Alexander (Alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

}

unit uFindThread;
{$mode objfpc}{$H+}

interface

uses
  Classes, StdCtrls, uDCUtils, SysUtils,udsxplugin;

type

{ TFindThread }

TFindThread = class(TThread)
  private
    { Private declarations }
    FPathStart:String;
    FItems: TStrings;
    FStatus: TLabel;
    FCurrent: TLabel;
    FCurrentFile:String;
    FFilesScaned:Integer;
    FFoundFile:String;
    FFileMask : String;
    FAttributes: Cardinal;
    FAttribStr : String;
    FCaseSens:Boolean;
    {Date search}
    FIsDateFrom,
    FIsDateTo : Boolean;
    FDateTimeFrom,
    FDateTimeTo : TDateTime;
    {Time search}
    FIsTimeFrom,
    FIsTimeTo : Boolean;
    (* File size search *)
    FIsFileSizeFrom,
    FIsFileSizeTo : Boolean;
    FFileSizeFrom,
    FFileSizeTo : Int64;
    (* Find text *)
    FIsNoThisText,
    FFindInFiles:Boolean;
    FFindData:String;
    (* Replace text *)
    FReplaceInFiles : Boolean;
    FReplaceData : String;


    function CheckFileDate(DT : LongInt) : Boolean;
    function CheckFileSize(FileSize : Int64) : Boolean;
    function CheckFile(const Folder : String; const sr : TSearchRec) : Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddFile;
    procedure WalkAdr(const sNewDir:String);
    procedure UpDateProgress;
    procedure FillSearchRecord(var Srec:TSearchAttrRecord);
    property FilterMask:String read FFileMask write FFileMask;
    property PathStart:String read FPathStart write FPathStart;
    property Items:TStrings write FItems;
    (* Find text *)
    property FindInFiles:Boolean write FFindInFiles;
    property IsNoThisText:Boolean write FIsNoThisText default False;
    property Status:TLabel read FStatus write FStatus;
    property Current:TLabel read FCurrent write FCurrent; // label current file
    property CaseSensitive:boolean read FCaseSens write FCaseSens;
    property FindData:String read FFindData write FFindData;
    (* Replace text *)
    property ReplaceInFiles:Boolean write FReplaceInFiles;
    property ReplaceData:String read FReplaceData write FReplaceData;
    (* Date search *)
    property IsDateFrom:Boolean read FIsDateFrom write FIsDateFrom;
    property IsDateTo:Boolean read FIsDateTo write FIsDateTo;
    property DateTimeFrom:TDateTime read FDateTimeFrom write FDateTimeFrom;
    property DateTimeTo:TDateTime read FDateTimeTo write FDateTimeTo;
    (* Time search *)
    property IsTimeFrom:Boolean read FIsTimeFrom write FIsTimeFrom;
    property IsTimeTo:Boolean read FIsTimeTo write FIsTimeTo;

    (* File size search *)
    property IsFileSizeFrom : Boolean read FIsFileSizeFrom write FIsFileSizeFrom;
    property IsFileSizeTo : Boolean read FIsFileSizeTo write FIsFileSizeTo;
    property FileSizeFrom : Int64 read FFileSizeFrom write FFileSizeFrom;
    property FileSizeTo : Int64 read FFileSizeTo write FFileSizeTo;
    
    property Attributes: Cardinal read FAttributes write FAttributes;
    property  AttribStr : String read FAttribStr write FAttribStr;
  end;


implementation

uses
  LCLProc, Dialogs, Masks, uLng, uClassesEx, uFindMmap, uFindEx, uGlobs, uShowMsg, uOSUtils;

{ TFindThread }

constructor TFindThread.Create;
begin
  DebugLn('thread b');
  inherited Create(True);
  FCaseSens:=True;
  FFilesScaned:=0;
  FilterMask:='*';
  GetDir(0, FPathStart);
  FItems:=nil;
  FIsDateFrom := False;
  FIsDateTo := False;
  FIsFileSizeFrom := False;
  FIsFileSizeTo := False;
  FAttributes := faAnyFile;
  FAttribStr := '?????????';
end;

destructor TFindThread.Destroy;
begin

end;

procedure TFindThread.Execute;
var
  sCurrDir:String;
begin
  try
    DebugLn('thread b2');
    assert(Assigned(FItems),'assert:FItems is empty');
    Synchronize(@UpDateProgress);
    if length(FPathStart)>1 then
    if FPathStart[length(FPathStart)] = PathDelim then
      Delete(FPathStart,length(FPathStart),1);
    sCurrDir:= mbGetCurrentDir;
    try
        DebugLn('thread b',FPathStart);
      WalkAdr(FPathStart);
    finally
      mbSetCurrentDir(sCurrDir);
    end;  
  //  MessageBeep(1000);
    DebugLn('thread end');

  except
    on E:Exception do
      msgError(E.Message, Self);
  end;
end;

procedure TFindThread.AddFile;
begin
  FItems.Add(FFoundFile);
end;

procedure TFindThread.UpDateProgress;
begin
  FStatus.Caption:=Format(rsFindScaned,[FFilesScaned]);
  FCurrent.Caption:=FCurrentFile;
end;


function FindInFile(const sFileName:String; sData: String; bCase:Boolean): Boolean;
const
  BufferSize = 4096;
var
    fs: TFileStreamEx;
    lastPos, sDataLength,
    OffsetPos: Cardinal;
    Buffer: array[0..BufferSize-1] of Char;

    Compare: function(Str1, Str2: PChar; MaxLen: SizeInt): SizeInt;

begin
  if gUseMmapInSearch then
    begin
      Result := FindMmap(sFileName, sData, bCase);
      Exit;
    end;
  
  Result := False;
  if sData = '' then Exit;

  if bCase then
    Compare := @StrLIComp
  else
    Compare := @StrLComp;

  sDataLength := Length(sData);

  try
    fs := TFileStreamEx.Create(sFileName, fmOpenRead or fmShareDenyNone);
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


procedure FileReplaceString(const FileName, SearchString, ReplaceString: string; bCase:Boolean);
var
  fs: TFileStreamEx;
  S: string;
  Flags : TReplaceFlags;
begin
  Include(Flags, rfReplaceAll);
  if not bCase then
    Include(Flags, rfIgnoreCase);
    
  fs := TFileStreamEx.Create(FileName, fmOpenread or fmShareDenyNone);
  try
    SetLength(S, fs.Size);
    fs.ReadBuffer(S[1], fs.Size);
  finally
    fs.Free;
  end;
  S  := StringReplace(S, SearchString, replaceString, Flags);
  fs := TFileStreamEx.Create(FileName, fmCreate);
  try
    fs.WriteBuffer(S[1], Length(S));
  finally
    fs.Free;
  end;
end;

function TFindThread.CheckFileDate(DT : LongInt) : Boolean;
var
  DateTime: TDateTime;
begin
   Result := True;
   DateTime := FileDateToDateTime(DT);

   (* Check date from *)
   if FIsDateFrom then
      Result := (Int(DateTime) >= Int(FDateTimeFrom));

   (* Check time to *)
   if (FIsDateTo and Result) then
      Result := (Int(DateTime) <= Int(FDateTimeTo));

   (* Check time from *)         //TODO Сделать секунды
   if (FIsTimeFrom and Result) then
      Result := ((Trunc(Frac(DateTime) * 10000000) / 10000000) >= (Trunc(Frac(FDateTimeFrom) * 1000) / 1000));
      
      DebugLn('Time From = ', FloatToStr(FDateTimeFrom), ' File time = ', FloatToStr(DateTime), ' Result = ', BoolToStr(Result));

   (* Check time to *)
   if (FIsTimeTo and Result) then
      Result := ((Trunc(Frac(DateTime) * 10000000) / 10000000) <= (Trunc(Frac(FDateTimeTo) * 1000) / 1000));

   //DebugLn('Time To = ', FloatToStr(FDateTimeTo), ' File time = ', FloatToStr(DateTime), ' Result = ', BoolToStr(Result));


end;

function TFindThread.CheckFileSize(FileSize: Int64): Boolean;
begin
   Result := True;
   if FIsFileSizeFrom then
      Result := (FileSize >= FFileSizeFrom);
      
      //DebugLn('After From', FileSize, '-',  FFileSizeFrom, BoolToStr(Result));
      
   if (FIsFileSizeTo and Result) then
      Result := (FileSize <= FFileSizeTo);
    //DebugLn('After To',  FileSize, '-',  FFileSizeTo, BoolToStr(Result));
end;

function TFindThread.CheckFile(const Folder : String; const sr : TSearchRec) : Boolean;
var
  Attrib : Cardinal;
begin
  Result := True;
{$IFDEF WIN32}
(* This is hack *)
//DebugLn('File = ', sr.Name);
if not MatchesMaskList(sr.Name, FFileMask) then
   begin
     Result := False;
     Exit;
   end;
{$ENDIF}
  if (FIsDateFrom or FIsDateTo or FIsTimeFrom or FIsTimeTo) then
      Result := CheckFileDate(sr.Time);

  if (FIsFileSizeFrom or FIsFileSizeTo) and Result then
      Result := CheckFileSize(sr.Size);
      
 // if Length(FAttribStr) <> 0 then
    begin
      Result := CheckAttrMask(FAttributes, FAttribStr, sr.Attr);
    end;

  if (FFindInFiles and Result) then
     begin
       if FPS_ISDIR(sr.Attr) then
         begin
           Result := False;
           Exit;
         end;
       Result := FindInFile(Folder + PathDelim + sr.Name, FFindData, FCaseSens);

       if (FReplaceInFiles and Result) then
         FileReplaceString(Folder + PathDelim + sr.Name, FFindData, FReplaceData, FCaseSens);

       if FIsNoThisText then
         Result := not Result;
     end;
end;

procedure TFindThread.FillSearchRecord(var Srec:TSearchAttrRecord);

begin
  with Srec do
  begin
    rFileMask:=pchar(FFileMask);
    rAttributes:=FAttributes;
    rAttribStr:=pchar(FAttribStr);
    rCaseSens:=FCaseSens;
    {Date search}
    rIsDateFrom:=FIsDateFrom;
    rIsDateTo:=FIsDateTo;
    rDateTimeFrom:=FDateTimeFrom;
    rDateTimeTo:=FDateTimeTo;
    {Time search}
    rIsTimeFrom:=FIsTimeFrom;
    rIsTimeTo:=FIsTimeTo;
    (* File size search *)
    rIsFileSizeFrom:=FIsFileSizeFrom;
    rIsFileSizeTo:=FIsFileSizeTo;
    rFileSizeFrom:=FFileSizeFrom;
    rFileSizeTo:=FFileSizeTo;
    (* Find text *)
    rIsNoThisText:=FIsNoThisText;
    rFindInFiles:=FFindInFiles;
    rFindData:= pchar(FFindData);
    (* Replace text *)
    rReplaceInFiles:=FReplaceInFiles;
    rReplaceData:=pchar(FReplaceData);
  end;
  
  
end;

procedure TFindThread.WalkAdr(const sNewDir:String);
var
  sr: TSearchRec;
  Path : String;
begin
  DebugLn(sNewDir);
  if not mbSetCurrentDir(sNewDir) then Exit;

  Path := sNewDir + PathDelim + FFileMask;
  //DebugLn('Path = ', Path);

  DebugLn('FAttributes == ' + IntToStr(FAttributes));

  if FindFirstEx(Path, FAttributes, sr) = 0 then
  repeat
    if (sr.Name='.') or (sr.Name='..') then Continue;
    inc(FFilesScaned);
    //DebugLn(sr.Name);

      if CheckFile(sNewDir, sr) then
      begin
        fFoundFile:=sNewDir + PathDelim + sr.Name;
        Synchronize(@AddFile);
      end;
      
    FCurrentFile:=sNewDir + PathDelim + sr.Name;
    Synchronize(@UpDateProgress);
  until (FindNextEx(sr)<>0)or terminated;
  FindClose(sr);

    {Search in sub folders}
    if not Terminated then
    begin
      Path := sNewDir + PathDelim + '*';
      DebugLn('Search in sub folders = ', Path);
      if not Terminated and (FindFirstEx(Path, faDirectory, sr) = 0) then
        repeat
          if (sr.Name[1] <> '.') then
            WalkAdr(sNewDir + PathDelim + sr.Name);
        until Terminated or (FindNextEx(sr) <> 0);
      FindClose(sr);
    end;

end;

end.
