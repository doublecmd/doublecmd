{
Seksi Commander
----------------------------
Licence  : GNU GPL v 2.0
Author   : Pavel Letko (letcuv@centrum.cz)

File splitter

contributors:
  Radek Cervinka
}

unit fSplitter;

{$mode objfpc}{$H+}

interface

uses
  //Lazarus, Free-Pascal, etc.
  SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, Buttons, Menus,
  //DC
  uFileSource, uFile;

type
  { TfrmSplitter }
  TfrmSplitter = class(TForm)
    teNumberParts: TEdit;
    grbxFile: TGroupBox;
    edFileSource: TEdit;
    lblNumberParts: TLabel;
    lbFileSource: TLabel;
    edDirTarget: TEdit;
    lbDirTarget: TLabel;
    btnFTChoice: TButton;
    grbxSize: TGroupBox;
    cmbxSize: TComboBox;
    btnOK: TButton;
    btnCancel: TButton;
    rbtnKiloB: TRadioButton;
    rbtnMegaB: TRadioButton;
    rbtnGigaB: TRadioButton;
    rbtnByte: TRadioButton;
    btnRelativeFTChoice: TSpeedButton;
    cbRequireACRC32VerificationFile: TCheckBox;
    pmPathHelper: TPopupMenu;
    procedure btnFTChoiceClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SetNumberOfPart;
    procedure SetSizeOfPart;
    procedure cmbxSizeChange(Sender: TObject);
    procedure btnRelativeFTChoiceClick(Sender: TObject);
    procedure rbtnByteChange(Sender: TObject);
    procedure teNumberPartsChange(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    { Private declarations }
    MyModalResult: integer;
    function StrConvert(str: String): Int64;
  public
    { Public declarations }
    iVolumeNumber: Integer;
    iVolumeSize: int64;
  end;

{ ShowSplitterFileForm:
  "TMainCommands.cm_FileSpliter" function from "uMainCommands.pas" is calling this routine.}
function ShowSplitterFileForm(aFileSource: IFileSource; var aFile: TFile; const TargetPath: UTF8String): Boolean;

implementation

{$R *.lfm}

uses
  //Lazarus, Free-Pascal, etc.
  LCLType, LCLProc,
  //DC
  uLng, uFileProcs, uOperationsManager, uFileSourceSplitOperation, uShowMsg,
  DCOSUtils, uGlobs, uSpecialDir, uDCUtils;


{ ShowSplitterFileForm:
  "TMainCommands.cm_FileSpliter" function from "uMainCommands.pas" is calling this routine.}
function ShowSplitterFileForm(aFileSource: IFileSource; var aFile: TFile; const TargetPath: UTF8String): Boolean;
var
  Operation: TFileSourceSplitOperation = nil;
  frmSplitter:TfrmSplitter;
begin
  frmSplitter:=TfrmSplitter.Create(Application); //Did not use the "with..." here to make absolutely sure of what is referenced in the following.
  try
    frmSplitter.edFileSource.Text:= aFile.FullPath;
    frmSplitter.edDirTarget.Text:= TargetPath;
    frmSplitter.SetNumberOfPart;

    // Show form
    Result:= (frmSplitter.ShowModal = mrOK);

    if Result then
    begin
      try
        Operation:= aFileSource.CreateSplitOperation(aFile, frmSplitter.edDirTarget.Text) as TFileSourceSplitOperation;
        if Assigned(Operation) then
        begin
          Operation.VolumeSize:= frmSplitter.iVolumeSize;
          Operation.VolumeNumber:= frmSplitter.iVolumeNumber;
          Operation.RequireACRC32VerificationFile:= frmSplitter.cbRequireACRC32VerificationFile.Checked;
          Operation.AutomaticSplitMode:=(frmSplitter.cmbxSize.ItemIndex=0);
          OperationsManager.AddOperation(Operation);
        end;
      finally
        FreeThenNil(aFile);
      end;
    end;
  finally
    frmSplitter.Free;
  end;
end;

{ TfrmSplitter.SetNumberOfPart }
procedure TfrmSplitter.SetNumberOfPart;
begin
  if cmbxSize.ItemIndex<>0 then
    begin
      if StrConvert(cmbxSize.Text)>0 then
      begin
        if mbFileSize(edFileSource.Text) mod StrConvert(cmbxSize.Text)>0 then
          teNumberParts.Text:= IntToStr( (mbFileSize(edFileSource.Text) div StrConvert(cmbxSize.Text)) +1)
        else
          teNumberParts.Text:= IntToStr(mbFileSize(edFileSource.Text) div StrConvert(cmbxSize.Text));
      end
      else
      begin
        teNumberParts.Text:='Error';
      end;
    end
  else
    begin
      teNumberParts.Text:=rsMSgUndeterminedNumberOfFile;
    end;
end;

{ TfrmSplitter.SetSizeOfPart }
procedure TfrmSplitter.SetSizeOfPart;
begin
  if StrToInt64Def(teNumberParts.Text,0)>0 then
  begin
    if mbFileSize(edFileSource.Text) mod StrToInt64Def(teNumberParts.Text,0)>0 then
      cmbxSize.Text := IntToStr(mbFileSize(edFileSource.Text) div StrToInt64Def(teNumberParts.Text,0)+1)+'B'
    else
      cmbxSize.Text := IntToStr(mbFileSize(edFileSource.Text) div StrToInt64Def(teNumberParts.Text,0))+'B';
  end
  else
  begin
    cmbxSize.Text:='Error';
  end;
end;

{ TfrmSplitter.btnFTChoiceClick }
procedure TfrmSplitter.btnFTChoiceClick(Sender: TObject);
var
  sDir: string;
begin
  if SelectDirectory(rsSplitSelDir, edDirTarget.Text, sDir) then
  // Select directory:
  // must change on linux!!!
  begin
    edDirTarget.Text:= sDir;
  end;
end;

{ TfrmSplitter.StrConvert }
function TfrmSplitter.StrConvert(str:string):int64;
var iRet:int64;
    iPos,iMult:integer;
    sStr:string;
begin
  str:=UpperCase(str);
  iPos:=Pos('B',str);
  if iPos>1 then
  begin
    rbtnByte.Enabled:=false;
    rbtnKiloB.Enabled:=false;
    rbtnMegaB.Enabled:=false;
    rbtnGigaB.Enabled:=false;
    rbtnByte.Checked:=false;
    rbtnKiloB.Checked:=false;
    rbtnMegaB.Checked:=false;
    rbtnGigaB.Checked:=false;
    dec(iPos);
    case str[iPos] of
      'K':iMult:=1024; //Kilo
      'M':iMult:=1024*1024; //Mega
      'G':iMult:=1024*1024*1024; //Giga
    else
      iMult:=1;
      inc(iPos);
    end;
    dec(iPos);
    sStr:=Copy(str,1,iPos);
    iRet:=StrToInt64Def(sStr,0)*iMult;
  end
  else
  begin
    rbtnByte.Enabled:=true;
    rbtnKiloB.Enabled:=true;
    rbtnMegaB.Enabled:=true;
    rbtnGigaB.Enabled:=true;
    iMult:=1;
    if rbtnKiloB.Checked then iMult:=1024; //Kilo
    if rbtnMegaB.Checked then iMult:=1024*1024; //Mega
    if rbtnGigaB.Checked then iMult:=1024*1024*1024; //Giga
    iRet:=StrToInt64Def(Str,0)*iMult;
  end;
  Result:=iRet;
end;

{ TfrmSplitter.FormCreate }
procedure TfrmSplitter.FormCreate(Sender: TObject);
begin
  InitPropStorage(Self); // Initialize property storage
  rbtnByte.Enabled:= False;
  rbtnKiloB.Enabled:= False;
  rbtnMegaB.Enabled:= False;
  rbtnGigaB.Enabled:= False;
  MyModalResult:=mrCancel;
  gSpecialDirList.PopulateMenuWithSpecialDir(pmPathHelper,mp_PATHHELPER,nil);
end;

{ TfrmSplitter.rbtnByteChange }
procedure TfrmSplitter.rbtnByteChange(Sender: TObject);
begin
  SetNumberOfPart;
end;

{ TfrmSplitter.btnRelativeFTChoiceClick }
procedure TfrmSplitter.btnRelativeFTChoiceClick(Sender: TObject);
begin
  edDirTarget.SetFocus;
  gSpecialDirList.SetSpecialDirRecipientAndItsType(edDirTarget,pfPATH);
  pmPathHelper.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

{ TfrmSplitter.cmbxSizeChange }
procedure TfrmSplitter.cmbxSizeChange(Sender: TObject);
begin
  if cmbxSize.Focused then SetNumberOfPart; //Do the function ONLY-IF it's the result of someone typing in the field
end;

{ TfrmSplitter.teNumberPartsChange }
procedure TfrmSplitter.teNumberPartsChange(Sender: TObject);
begin
  if teNumberParts.Focused then SetSizeOfPart; //Do the function ONLY-IF it's the result of someone typing in the field
end;

{ TfrmSplitter.btnOKClick }
procedure TfrmSplitter.btnOKClick(Sender: TObject);
var
  iFileSize : Int64;
  Operation: TFileSourceSplitOperation = nil;
  WindowResult: ShortInt;
  isTooManyFiles: boolean;
begin
  if cmbxSize.ItemIndex<>0 then iVolumeSize:= StrConvert(cmbxSize.Text) else iVolumeSize:=0;
  if (iVolumeSize <= 0) AND (cmbxSize.ItemIndex<>0) then
  begin
    ShowMessageBox(rsSplitErrFileSize, 'Error!', MB_OK or MB_ICONERROR); //Incorrect file size format! (Used "ShowMessageBox" instead of "MsgError" 'cause with "MsgError", user can still click on the frmSplitter form and type in it).
  end
  else
  begin
    if not mbForceDirectory(IncludeTrailingPathDelimiter(mbExpandFileName(edDirTarget.Text))) then
    begin
      ShowMessageBox(rsSplitErrDirectory, 'Error!', MB_OK or MB_ICONERROR); //Unable to create target directory!
    end
    else
    begin
      if teNumberParts.Text <> rsMSgUndeterminedNumberOfFile then iVolumeNumber := StrToInt(teNumberParts.Text) else iVolumeNumber := 0;
      if (iVolumeNumber < 1) AND (teNumberParts.Text <> rsMSgUndeterminedNumberOfFile) then
      begin
        ShowMessageBox(rsSplitErrSplitFile, 'Error!', MB_OK or MB_ICONERROR); //Unable to split the file!
      end
      else
      begin
        isTooManyFiles:=(iVolumeNumber > 100);
        if isTooManyFiles then isTooManyFiles:=(MessageDlg(Caption, rsSplitMsgManyParts, mtWarning, mbYesNo, 0) <> mrYes);

        if not isTooManyFiles then
        begin
          MyModalResult:=mrOk;
          close;
        end; //if isTooManyFiles then
      end; //if (iVolumeNumber = 0) then
    end; //if not mbForceDirectory(edDirTarget.Text) then
  end; //if iVolumeSize <= 0 then
end;

{ TfrmSplitter.FormClose }
procedure TfrmSplitter.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  modalResult := MyModalResult;
end;

end.



