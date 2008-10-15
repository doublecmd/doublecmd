{
   Seksi Commander
   ----------------------------
   Implementing of Generic File operation thread
   (copying, moving ... is inherited from this)

   Licence  : GNU GPL v 2.0
   Author   : radek.cervinka@centrum.cz

   contributors:
   
   Copyright (C) 2006-2008  Koblov Alexander (Alexx2000@mail.ru)
}

unit uFileOpThread;
{$mode objfpc}{$H+}
{$DEFINE NOFAKETHREAD}

interface

uses
  Classes, uFileList, fFileOpDlg, uTypes, uDescr, fMsg, uShowMsg {$IFNDEF NOFAKETHREAD}, uFakeThread{$ENDIF};

type
  { TFileOpThread }
{$IFDEF NOFAKETHREAD}
  TFileOpThread = class(TThread)
{$ELSE}
  TFileOpThread = class(TFakeThread)
{$ENDIF}
  private
    { Private declarations }
  protected
    FFileList: TFileList;  // input filelist (not rekursive walked)
    NewFileList: TFileList; // fill it with complete list of all files
    FFilesCount: Integer;
    FFilesSize: Int64;
    FDirCount :Integer;
    FBeginTime: TDateTime;
    FDownTo: Boolean; // browse list backward (for deleting)
    FReplaceAll:Boolean;
    FSkipAll:Boolean;
    FDstNameMask:String;
    FDstExtMask:String;
    FAppend: Boolean; // used mainly for pass information between move and copy
    FSymLinkAll, // process all symlinks
    FNotSymLinkAll : Boolean; // process all real files/folders
    FDescr: TDescription;

    procedure Execute; override;
    procedure MainExecute; virtual; abstract; // main loop for copy /delete ...
    procedure CreateForm;
    procedure FillAndCount;
    procedure FillAndCountRec(const srcPath, dstPath:String); // rekursive called
    procedure EstimateTime(iSizeCoped:Int64);
    function  GetCaptionLng:String; virtual;
    function CheckFile(FileRecItem: PFileRecItem): Boolean; virtual;
    procedure CorrectMask;
    function  CorrectDstName(const sName:String):String;
    function  CorrectDstExt(const sExt:String):String;

  public
    FFileOpDlg: TfrmFileOp; // progress window
    sDstPath: String;
    sDstMask: String;
    bDropReadOnlyFlag : Boolean; // for copy operation
    constructor Create(aFileList:TFileList);virtual;
    destructor Destroy; override;
    function UseForm:Boolean; virtual;
    function FreeAtEnd:Boolean; virtual;
    function DlgFileExist(const sMsg:String):Boolean; // result=true > rewrite file
    function DlgProcessSymLink(const sMsg:String):Boolean;
  end;
  
const
    FMyMsgButtons : array[0..5] of TMyMsgButton = (msmbRewrite, msmbNo, msmbSkip, msmbAppend, msmbRewriteAll, msmbSkipAll); //Alexx2000
    FSymLinkBtns : array[0..3] of TMyMsgButton = (msmbYes, msmbNo, msmbRewriteAll, msmbSkipAll); //Alexx2000

implementation

uses
  SysUtils, DateUtils, uLng, uFileProcs, uFileOp, Forms, uFindEx, uDCUtils, uOSUtils,
  LCLProc, uGlobs;

{ TFileOpThread }

constructor TFileOpThread.Create(aFileList:TFileList);
begin
  inherited Create(True); // create Suspended
  FFileList := aFileList;
  FreeOnTerminate:=FreeAtEnd;
  sDstMask:='*.*';
  FSymLinkAll := False;
  FNotSymLinkAll := False;
end;

destructor TFileOpThread.Destroy;
begin
  if assigned(FFileList) then
    FreeAndNil(FFileList);
end;

procedure TFileOpThread.FillAndCountRec(const srcPath, dstPath:String);
var
  sr:TSearchRec;
  fr:TFileRecItem;
begin
  if FindFirstEx(srcPath+'*',faAnyFile,sr)<>0 then
  begin
    FindClose(sr);
    Exit;
  end;
  repeat
    if (sr.Name='.') or (sr.Name='..') then Continue;
    fr.sName:=srcPath+sr.Name;
    //    write(fr.sName,': ');
    fr.sPath:=dstPath;
    fr.sNameNoExt:=sr.Name; // we use to save dstname
//    DebugLn(sr.Name);

    fr.iSize:= sr.Size;
    fr.iMode:= sr.Attr;
    fr.fTimeI:= FileDateToDateTime(sr.Time);

    fr.sTime:='';   // not interested
    
    fr.bIsLink:=FPS_ISLNK(fr.iMode);
    fr.sLinkTo:='';
    fr.bSelected:=False;
    fr.sModeStr:=''; // not interested
//    fr.sPath:=srcPath;

    // For process symlinks, read only files etc.
    CheckFile(@fr);

    NewFileList.AddItem(@fr);
    if fr.bIsLink then
      Continue;
    if FPS_ISDIR(fr.iMode) then
    begin
      inc(FDirCount);
      FillAndCountRec(srcPath+sr.Name+DirectorySeparator, dstPath+sr.Name+DirectorySeparator);
    end
    else
    begin
      inc(FFilesSize, fr.iSize);
      inc(FFilesCount);
    end;
  until FindNextEx(sr)<>0;
  FindClose(sr);
end;

procedure TFileOpThread.FillAndCount;
var
  I: Integer;
  ptr: PFileRecItem;
begin
  NewFileList.Clear;
  FFilesCount:= 0;
  FFilesSize:= 0;
  FDirCount:= 0;
  for I:= 0 to FFileList.Count-1 do
  begin
    ptr:= FFileList.GetItem(I);

    // For process symlinks, read only files etc.
    CheckFile(ptr);

    if FPS_ISDIR(ptr^.iMode) and (not ptr^.bLinkIsDir) then
    begin
      inc(FDirCount);
      NewFileList.AddItem(ptr); // add DIR to List
      FillAndCountRec(ptr^.sName+DirectorySeparator,ptr^.sNameNoExt+DirectorySeparator);  // rekursive browse child dir
    end
    else
    begin
      NewFileList.AddItem(ptr);
      inc(FFilesCount);
      inc(FFilesSize, ptr^.iSize); // in first level we know file size -> use it
    end;
  end;
end;

procedure TFileOpThread.Execute;
begin
// main thread code started here
try
  FReplaceAll:=False;
  FSkipAll:=False;
  NewFileList:=TFileList.Create;
  try
    FillAndCount; // gets full list of files (rekursive)

    if gProcessComments then
      FDescr:= TDescription.Create(True);

    FBeginTime:=Now;
    if UseForm then
    begin
      Synchronize(@CreateForm);  // create progress form in main thread
      FFileOpDlg.iProgress2Pos:= 0;
      FFileOpDlg.iProgress2Max:= 100;
      Synchronize(@FFileOpDlg.UpdateDlg);
    end;

    MainExecute; // main executive (virtual)

  finally
    if gProcessComments and Assigned(FDescr) then
      begin
        FDescr.SaveDescription;
        FreeAndNil(FDescr);
      end;
    if UseForm then
      begin
        Synchronize(@FFileOpDlg.Close);
        DebugLN('TFileOpThread finally');
      end;
    if Assigned(NewFileList) then
      FreeAndNil(NewFileList);
  end;
except
  on E:Exception do
    msgOK(Self, E.Message);
end;
end;

procedure TFileOpThread.CreateForm;
begin
  DebugLn('TFileOpThread.CreateForm');
  FFileOpDlg:= TfrmFileOp.Create(Application);
  FFileOpDlg.Thread:= TThread(Self);
  FFileOpDlg.Caption:= GetCaptionLng;
  FFileOpDlg.Show;
  FFileOpDlg.Update;
end;

function TFileOpThread.UseForm:Boolean;
begin
  Result:= True;
end;

function TFileOpThread.FreeAtEnd:Boolean;
begin
  Result:= True;
{ if we use WaitFor, we don't use FreeOnTerminate
  possible SIGSEV!!!!!!!!!!
}
 // Result:=False;
end;

procedure TFileOpThread.EstimateTime(iSizeCoped: Int64);
var
  iSec: Integer; // passed seconds
  iSpeed: Integer;  // operation speed
  dtEstimatedTime: TDateTime; // remained time
begin
  if not UseForm then Exit;

  iSec:= 0;
  iSpeed:= 0;

  with FFileOpDlg do
  begin
    if iSizeCoped = 0 then
      sEstimated:= '????'
    else
      begin
        iSec:= SecondsBetween(FBeginTime, Now);

        if iSec > 0 then
          iSpeed:= iSizeCoped div iSec;
        if iSpeed > 0 then
          dtEstimatedTime:= ((FFilesSize-iSizeCoped) div iSpeed) / SecsPerDay;

        sEstimated:= FormatDateTime('HH:MM:SS', dtEstimatedTime);
        sEstimated:= Format(rsDlgSpeedTime, [cnvFormatFileSize(iSpeed), sEstimated]);
      end;

    Synchronize(@FFileOpDlg.UpdateDlg);
  end;
end;

function TFileOpThread.DlgFileExist(const sMsg:String):Boolean; // result=true > rewrite file
begin
  FAppend:= False;
  Result:= False;

  case MsgBox(Self,sMsg, FMyMsgButtons, msmbYes, msmbNo) of
    mmrNo, mmrSkip:;
    mmrRewrite:
      begin
        Result:= True;
      end;
    mmrRewriteAll:
      begin
        FReplaceAll:=True;
        Result:= True;
      end;
    mmrAppend:
      begin
        FAppend:= True;
        Result:= True;
      end;
    mmrSkipAll:
      begin
        FSkipAll:= True;
      end;
    else
      Raise Exception.Create('bad handling msg result');
  end; //case
end;

{ Dialog for process symlink or real file/folder }

function TFileOpThread.DlgProcessSymLink(const sMsg:String):Boolean; // result=true > process symlink
begin
  FAppend:= False;
  Result:= False;

  case MsgBox(Self, sMsg, FSymLinkBtns, msmbYes, msmbNo) of
    mmrNo:;
    
    mmrYes:
      begin
        Result:=True;
      end;
    mmrRewriteAll:
      begin
        FSymLinkAll:=True;
        Result:=True;
      end;

    mmrSkipAll:
      begin
        FNotSymLinkAll:=True;
      end;
    else
      Raise Exception.Create('bad handling msg result');
  end; //case
end;

function TFileOpThread.GetCaptionLng:String;
begin
  Result:= '';
end;

function TFileOpThread.CheckFile(FileRecItem: PFileRecItem): Boolean;
var
  sRealName: String;
  sr: TSearchRec;
begin
  Result:= True;
  // For process symlink or real file/folder
  if FPS_ISLNK(FileRecItem^.iMode) then
  if (not FSymLinkAll) and (FNotSymLinkAll  or not DlgProcessSymLink('Process SymLink "' + FileRecItem^.sName +'"? Press "Yes" to copy or "No" for copy real file/folder')) then //TODO: Localize message
    begin
      sRealName:=ReadSymLink(FileRecItem^.sName);

      sRealName := GetAbsoluteFileName(ExtractFilePath(FileRecItem^.sName), sRealName);

      FindFirstEx(sRealName, faAnyFile, sr);
      with FileRecItem^ do
      begin
        iSize := sr.Size;
        sTime := DateTimeToStr(Trunc(FileDateToDateTime(sr.Time)));
        iMode := sr.Attr;
        sModeStr := AttrToStr(sr.Attr);
        bLinkIsDir:=False;
        bSelected:=False;
      end;
      DivFileName(sRealName, FileRecItem^.sNameNoExt, FileRecItem^.sExt);
      FileRecItem^.sNameNoExt := sr.Name;
      FileRecItem^.sName := sRealName;
    end;
  DebugLn('sNameNoExt == ' + FileRecItem^.sNameNoExt);
end;

procedure TFileOpThread.CorrectMask;
begin
  DivFileName(sDstMask,FDstNameMask,FDstExtMask);
  if FDstNameMask='' then
    FDstNameMask:='*';
  if FDstExtMask='' then
    FDstExtMask:='.*';
end;

function TFileOpThread.CorrectDstName(const sName:String):String;
var
  i:Integer;
begin
  Result:='';
  for i:=1 to length(FDstNameMask) do
  begin
    if FDstNameMask[i]= '?' then
      Result:=Result+sName[i]
    else
    if FDstNameMask[i]= '*' then
      Result:=Result+Copy(sName,i,length(sName)-i+1)
    else
      Result:=Result+FDstNameMask[i];
  end;
end;

function TFileOpThread.CorrectDstExt(const sExt:String):String;
var
  i:Integer;
begin
  Result:='';
  for i:=1 to length(FDstExtMask) do
  begin
    if FDstExtMask[i]= '?' then
      Result:=Result+sExt[i]
    else
    if FDstExtMask[i]= '*' then
      Result:=Result+Copy(sExt,i,length(sExt)-i+1)
    else
      Result:=Result+FDstExtMask[i];
  end;
end;

end.
