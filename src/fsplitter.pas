{
Seksi Commander
----------------------------
Licence  : GNU GPL v 2.0
Author   : Pavel Letko (letcuv@centrum.cz)

File split

contributors:
  Radek Cervinka
}

unit fSplitter;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls,
  uFileSource,
  uFile;

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
    procedure btnFTChoiceClick(Sender: TObject);
    procedure cmbxSizeCloseUp(Sender: TObject);
    procedure cmbxSizeKeyPress(Sender: TObject; var Key: char);
    procedure cmbxSizeKeyUp(Sender: TObject; var Key: char; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure rbtnKiloBChange(Sender: TObject);
    procedure SetNumberOfPart;
    procedure SetSizeOfPart;
    procedure teNumberPartsKeyPress(Sender: TObject; var Key: char);
    procedure teNumberPartsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    function StrConvert(str: String): Int64;
  public
    { Public declarations }
  end;

function ShowSplitterFileForm(aFileSource: IFileSource; var aFile: TFile; const TargetPath: UTF8String): Boolean;

implementation

{$R *.lfm}

uses
  LCLProc, uLng, uOSUtils, uFileProcs, uOperationsManager,
  uFileSourceSplitOperation, fFileOpDlg, uShowMsg;

function ShowSplitterFileForm(aFileSource: IFileSource; var aFile: TFile; const TargetPath: UTF8String): Boolean;
var
  iVolumeNumber: Integer;
  iFileSize, iVolumeSize: Int64;
  Operation: TFileSourceSplitOperation = nil;
  ProgressDialog: TfrmFileOp;
  OperationHandle: TOperationHandle;
begin
  with TfrmSplitter.Create(Application) do
  begin
    try
      edFileSource.Text:= aFile.FullPath;
      edDirTarget.Text:= TargetPath;
      SetNumberOfPart;

      // Show form
      Result:= (ShowModal = mrOK);

      if Result then
      begin
        iVolumeSize:= StrConvert(cmbxSize.Text);
        if iVolumeSize <= 0 then
        begin
          msgError(rsSplitErrFileSize); // Incorrect file size format!
          Exit;
        end;
        iFileSize:= mbFileSize(edFileSource.Text);
        if iVolumeSize >= iFileSize then
        begin
          msgError(rsSplitErrSplitFile); // Unable to split the file!
          Exit;
        end;
        if not mbForceDirectory(edDirTarget.Text) then
        begin
          msgError(rsSplitErrDirectory); // Unable to create target directory!
          Exit;
        end;
        iVolumeNumber:= StrToInt(teNumberParts.Text);
        if (iVolumeNumber = 0) then
        begin
          msgError(rsSplitErrSplitFile); // Unable to split the file!
          Exit;
        end;
        if iVolumeNumber > 100 then
        begin
          if MessageDlg(Caption, rsSplitMsgManyParts, mtWarning, mbYesNo, 0) <> mrYes then
            begin
              Exit; // Too many parts
            end;
        end;

        try
          Operation:= aFileSource.CreateSplitOperation(aFile, edDirTarget.Text) as TFileSourceSplitOperation;
          if Assigned(Operation) then
          begin
            Operation.VolumeSize:= iVolumeSize;
            Operation.VolumeNumber:= iVolumeNumber;
            OperationHandle:= OperationsManager.AddOperation(Operation, ossAutoStart);
            ProgressDialog:= TfrmFileOp.Create(OperationHandle);
            ProgressDialog.Show;
          end;
        finally
          FreeThenNil(aFile);
        end;
      end;
    finally
      Free;
    end;
  end;
end;

procedure TfrmSplitter.SetNumberOfPart;
begin
  if  not (cmbxSize.Text='') then
    begin
      if  mbFileSize(edFileSource.Text) mod StrConvert(cmbxSize.Text)>0 then
        teNumberParts.Text:= IntToStr(mbFileSize(edFileSource.Text)div StrConvert(cmbxSize.Text)+1)
      else
        teNumberParts.Text:= IntToStr(mbFileSize(edFileSource.Text)div StrConvert(cmbxSize.Text));
    end;
end;

procedure TfrmSplitter.SetSizeOfPart;
begin
  if not (teNumberParts.Text='') then
    begin
      if mbFileSize(edFileSource.Text) mod StrToInt64Def(teNumberParts.Text,0)>0 then
        cmbxSize.Text := IntToStr(mbFileSize(edFileSource.Text) div StrToInt64Def(teNumberParts.Text,0)+1)+'B'
      else
        cmbxSize.Text := IntToStr(mbFileSize(edFileSource.Text) div StrToInt64Def(teNumberParts.Text,0))+'B';
    end;
end;

procedure TfrmSplitter.teNumberPartsKeyPress(Sender: TObject; var Key: char);
begin
  if not (Key in ['0'.. '9', #8, #46]) then
    begin
      Key := #0;
      Exit;
    end;
end;

procedure TfrmSplitter.teNumberPartsKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  SetSizeOfPart;
end;

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

function TfrmSplitter.StrConvert(str:string):int64;
var iRet:int64;
    iPos,iMult:integer;
    sStr:string;
begin
  str:=UpperCase(str);
  iPos:=Pos('B',str);
  if iPos>1 then
  begin
    rbtnKiloB.Enabled:=false;
    rbtnMegaB.Enabled:=false;
    rbtnGigaB.Enabled:=false;
    rbtnKiloB.Checked:=false;
    rbtnMegaB.Checked:=false;
    rbtnGigaB.Checked:=false;
    dec(iPos);
    case str[iPos] of
      'K':iMult:=1024;       //Kilo
      'M':iMult:=1024*1024;    //Mega
      'G':iMult:=1024*1024*1024;//Giga
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
    rbtnKiloB.Enabled:=true;
    rbtnMegaB.Enabled:=true;
    rbtnGigaB.Enabled:=true;
    iMult:=1;
    if rbtnKiloB.Checked then iMult:=1024;          //Kilo
    if rbtnMegaB.Checked then iMult:=1024*1024;     //Mega
    if rbtnGigaB.Checked then iMult:=1024*1024*1024;//Giga
    iRet:=StrToInt64Def(Str,0)*iMult;
  end;
  Result:=iRet;
end;

procedure TfrmSplitter.cmbxSizeCloseUp(Sender: TObject);
begin
  SetNumberOfPart;
end;

procedure TfrmSplitter.cmbxSizeKeyPress(Sender: TObject; var Key: char);
begin
  if not (Key in ['0'.. '9', #8, #46,'G','g','M','m','K','k','B','b']) then
    begin
      Key := #0;
      Exit;
    end;
end;

procedure TfrmSplitter.cmbxSizeKeyUp(Sender: TObject; var Key: char;
  Shift: TShiftState);
begin
  if Key in ['0'.. '9','B','b'] then
    begin
      SetNumberOfPart;
      Exit;
    end;
  if Key in [#8, #46] then
    begin
      if (Pos ('M',UpperCase(cmbxSize.Text))>0) or
         (Pos ('K',UpperCase(cmbxSize.Text))>0) or
         (Pos ('G',UpperCase(cmbxSize.Text))>0) then Exit
      else SetNumberOfPart;
    end;
end;

procedure TfrmSplitter.FormCreate(Sender: TObject);
begin
  rbtnKiloB.Enabled:= False;
  rbtnMegaB.Enabled:= False;
  rbtnGigaB.Enabled:= False;
end;

procedure TfrmSplitter.rbtnKiloBChange(Sender: TObject);
begin
  SetNumberOfPart;
end;

end.
