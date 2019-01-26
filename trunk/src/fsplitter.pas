{
   Double Commander
   -------------------------------------------------------------------------
   Take a single file and split it in part based on few parameters.

   Copyright (C) 2007-2018  Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit fSplitter;

{$mode objfpc}{$H+}

interface

uses
  //Lazarus, Free-Pascal, etc.
  SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, Buttons, Menus,
  EditBtn, ExtCtrls,
  //DC
  fButtonForm, uFileSource, uFile, KASComboBox;

type
  { TfrmSplitter }
  TfrmSplitter = class(TfrmButtonForm)
    btnRelativeFTChoice: TSpeedButton;
    edDirTarget: TDirectoryEdit;
    lbDirTarget: TLabel;
    teNumberParts: TEdit;
    lblNumberParts: TLabel;
    grbxSize: TGroupBox;
    cmbxSize: TComboBoxAutoWidth;
    rbtnKiloB: TRadioButton;
    rbtnMegaB: TRadioButton;
    rbtnGigaB: TRadioButton;
    rbtnByte: TRadioButton;
    cbRequireACRC32VerificationFile: TCheckBox;
    pmPathHelper: TPopupMenu;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure SetNumberOfPart;
    procedure SetSizeOfPart;
    procedure cmbxSizeChange(Sender: TObject);
    procedure btnRelativeFTChoiceClick(Sender: TObject);
    procedure rbtnByteChange(Sender: TObject);
    procedure teNumberPartsChange(Sender: TObject);
  private
    FFileName: String;
    iVolumeSize: Int64;
    MyModalResult: integer;
    iVolumeNumber: Integer;
    function StrConvert(sExpression: String): Int64;
  public
    { Public declarations }
  end;

{ ShowSplitterFileForm:
  "TMainCommands.cm_FileSpliter" function from "uMainCommands.pas" is calling this routine.}
function ShowSplitterFileForm(TheOwner: TComponent; aFileSource: IFileSource; var aFile: TFile; const TargetPath: String): Boolean;

implementation

{$R *.lfm}

uses
  //Lazarus, Free-Pascal, etc.
  LazUTF8, LCLType, LCLProc,
  //DC
  uTypes, DCStrUtils, uLng, uFileProcs, uOperationsManager,
  uFileSourceSplitOperation, uShowMsg, DCOSUtils, uGlobs, uSpecialDir, uDCUtils;


{ ShowSplitterFileForm:
  "TMainCommands.cm_FileSpliter" function from "uMainCommands.pas" is calling this routine.}
function ShowSplitterFileForm(TheOwner: TComponent; aFileSource: IFileSource; var aFile: TFile; const TargetPath: String): Boolean;
var
  frmSplitter:TfrmSplitter;
  Operation: TFileSourceSplitOperation = nil;
begin
  frmSplitter:=TfrmSplitter.Create(TheOwner); //Did not use the "with..." here to make absolutely sure of what is referenced in the following.
  try
    frmSplitter.FFileName:= aFile.FullPath;
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
          OperationsManager.AddOperation(Operation, frmSplitter.QueueIdentifier, False);
        end;
      finally
        FreeAndNil(aFile);
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
        if mbFileSize(FFileName) mod StrConvert(cmbxSize.Text)>0 then
          teNumberParts.Text:= IntToStr( (mbFileSize(FFileName) div StrConvert(cmbxSize.Text)) +1)
        else
          teNumberParts.Text:= IntToStr(mbFileSize(FFileName) div StrConvert(cmbxSize.Text));
      end
      else
      begin
        teNumberParts.Text:=rsSimpleWordError;
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
    if mbFileSize(FFileName) mod StrToInt64Def(teNumberParts.Text,0)>0 then
      cmbxSize.Text := IntToStr(mbFileSize(FFileName) div StrToInt64Def(teNumberParts.Text, 0) + 1)
    else
      cmbxSize.Text := IntToStr(mbFileSize(FFileName) div StrToInt64Def(teNumberParts.Text, 0));
    rbtnByte.Checked := True;
    rbtnByte.Enabled := True;
    rbtnKiloB.Enabled := True;
    rbtnMegaB.Enabled := True;
    rbtnGigaB.Enabled := True;
  end
  else
  begin
    cmbxSize.Text:=rsSimpleWordError;
  end;
end;

{ TfrmSplitter.StrConvert }
//Let's do a basic conversion that maybe is not a full idiot-proof, but versatile and simple enough to fit in a few lines.
function TfrmSplitter.StrConvert(sExpression: String): Int64;
var
  iMult: int64 = 1;
  bUseRadioButtons: boolean = True;

  procedure CheckIfMemSizeAndSetMultiplicator(sExpressionToCheck:string; iMultiplicatorToSetIfAny:int64);
  var
    iSeekPos: integer;
  begin
    iSeekPos := pos(UTF8LowerCase(sExpressionToCheck), sExpression);
    if iSeekPos <> 0 then
    begin
      iMult := iMultiplicatorToSetIfAny;
      sExpression := UTF8LeftStr(sExpression, pred(iSeekPos));
      bUseRadioButtons := False;
    end;
  end;

begin
  //1.Let's place string in lowercase to avoid any later problem.
  sExpression := UTF8LowerCase(sExpression);

  //2.Let's check first if we have the personalized unit in the expression.
  //  We check first since they may include spaces and byte suffix.
  CheckIfMemSizeAndSetMultiplicator(gSizeDisplayUnits[fsfPersonalizedByte], 1);
  CheckIfMemSizeAndSetMultiplicator(gSizeDisplayUnits[fsfPersonalizedKilo], 1024);
  CheckIfMemSizeAndSetMultiplicator(gSizeDisplayUnits[fsfPersonalizedMega], 1024*1024);
  CheckIfMemSizeAndSetMultiplicator(gSizeDisplayUnits[fsfPersonalizedGiga], 1024*1024*1024);

  //4.Let's check if there are single letter multiplier or byte suffix.
  CheckIfMemSizeAndSetMultiplicator(rsLegacyOperationByteSuffixLetter, 1);
  CheckIfMemSizeAndSetMultiplicator(rsLegacyDisplaySizeSingleLetterKilo, 1024);
  CheckIfMemSizeAndSetMultiplicator(rsLegacyDisplaySizeSingleLetterMega, 1024*1024);
  CheckIfMemSizeAndSetMultiplicator(rsLegacyDisplaySizeSingleLetterGiga, 1024*1024*1024);

  //5. Well... It looks like the pre-defined disk size strings has not been translated in all languages so let's simplify with english values...
  //NO NEED TO TRANSLATE THESE ONES! Either translate all disk size strings and/or accept that english abbreviation always work here.
  CheckIfMemSizeAndSetMultiplicator('B', 1);
  CheckIfMemSizeAndSetMultiplicator('K', 1024);
  CheckIfMemSizeAndSetMultiplicator('M', 1024*1024);
  CheckIfMemSizeAndSetMultiplicator('G', 1024*1024*1024);

  //5.We remove the spaces since they are irrevelant.
  sExpression := UTF8StringReplace(sExpression, ' ', '', [rfReplaceAll]);

  //6.If we return a number here, let's disable the unit selector below.
  if cmbxSize.Focused then
  begin
    rbtnByte.Enabled := bUseRadioButtons;
    rbtnKiloB.Enabled := bUseRadioButtons;
    rbtnMegaB.Enabled := bUseRadioButtons;
    rbtnGigaB.Enabled := bUseRadioButtons;
  end;

  //7.If we return a number here, let's disable the unit selector below.
  if bUseRadioButtons then
  begin
    if rbtnKiloB.Checked then iMult:=1024;
    if rbtnMegaB.Checked then iMult:=1024*1024;
    if rbtnGigaB.Checked then iMult:=1024*1024*1024;
  end;

  //7.Since we're now supposed to have just numbers in our string, we should be ready to do our conversion.
  Result := StrToInt64Def(sExpression, 0) * iMult;
end;

{ TfrmSplitter.FormCreate }
procedure TfrmSplitter.FormCreate(Sender: TObject);
begin
  InitPropStorage(Self); // Initialize property storage
  MyModalResult:=mrCancel;
  gSpecialDirList.PopulateMenuWithSpecialDir(pmPathHelper,mp_PATHHELPER,nil);
  ParseLineToList(rsSplitPreDefinedSizes, cmbxSize.Items);
end;

{ TfrmSplitter.rbtnByteChange }
procedure TfrmSplitter.rbtnByteChange(Sender: TObject);
const
  sDigits:string='0123456789';
var
  iFirstNonDigit: integer = 0;
  iIndex: integer;
  sExpression, sSanitize: string;
begin
  if rbtnByte.focused OR rbtnKiloB.focused OR rbtnMegaB.focused OR rbtnGigaB.focused then
  begin
    if TRadioButton(Sender).Checked then
    begin
      sExpression := UTF8StringReplace(cmbxSize.Text, ' ', '', [rfIgnoreCase , rfReplaceAll]);
      sSanitize := '';
      iFirstNonDigit := 0;

      for iIndex := 1 to UTF8Length(sExpression) do
      begin
        if (UTF8Pos(UTF8Copy(sExpression, iIndex, 1), sDigits) = 0) then
        begin
          if iFirstNonDigit = 0 then
            iFirstNonDigit := iIndex;
        end
        else
        begin
          if iIndex=UTF8Length(sExpression) then iFirstNonDigit := succ(iIndex);
        end;
      end;

      if iFirstNonDigit <> 0 then sSanitize:=UTF8LeftStr(sExpression, pred(iFirstNonDigit));
      cmbxSize.Text := sSanitize;
      SetNumberOfPart;
    end;
  end;
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
  if cmbxSize.Focused then //Do the function ONLY-IF it's the result of someone typing in the field
  begin
    if cmbxSize.ItemIndex<>0 then
    begin
      SetNumberOfPart;
    end
    else
    begin
      teNumberParts.Text:='';
      if teNumberParts.CanFocus then teNumberParts.SetFocus;
      SetSizeOfPart;
    end;
  end;
end;

{ TfrmSplitter.teNumberPartsChange }
procedure TfrmSplitter.teNumberPartsChange(Sender: TObject);
begin
  if teNumberParts.Focused then SetSizeOfPart; //Do the function ONLY-IF it's the result of someone typing in the field
end;

{ TfrmSplitter.FormCloseQuery }
procedure TfrmSplitter.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  isTooManyFiles: boolean;
begin
  if (ModalResult <> mrCancel) then
  begin
    if cmbxSize.ItemIndex <> 0 then
      iVolumeSize:= StrConvert(cmbxSize.Text)
    else begin
      iVolumeSize:= 0;
    end;
    if (iVolumeSize <= 0) AND (cmbxSize.ItemIndex<>0) then
    begin
      ShowMessageBox(rsSplitErrFileSize, rsSimpleWordError+'!', MB_OK or MB_ICONERROR); //Incorrect file size format! (Used "ShowMessageBox" instead of "MsgError" 'cause with "MsgError", user can still click on the frmSplitter form and type in it).
    end
    else
    begin
      if not mbForceDirectory(IncludeTrailingPathDelimiter(mbExpandFileName(edDirTarget.Text))) then
      begin
        ShowMessageBox(rsSplitErrDirectory, rsSimpleWordError+'!', MB_OK or MB_ICONERROR); //Unable to create target directory!
      end
      else
      begin
        if teNumberParts.Text <> rsMSgUndeterminedNumberOfFile then iVolumeNumber := StrToInt(teNumberParts.Text) else iVolumeNumber := 0;
        if (iVolumeNumber < 1) AND (teNumberParts.Text <> rsMSgUndeterminedNumberOfFile) then
        begin
          ShowMessageBox(rsSplitErrSplitFile, rsSimpleWordError+'!', MB_OK or MB_ICONERROR); //Unable to split the file!
        end
        else
        begin
          isTooManyFiles:=(iVolumeNumber > 100);
          if isTooManyFiles then isTooManyFiles:=(MessageDlg(Caption, rsSplitMsgManyParts, mtWarning, mbYesNo, 0) <> mrYes);

          if not isTooManyFiles then
          begin
            MyModalResult:= mrOk;
          end; //if isTooManyFiles then
        end; //if (iVolumeNumber = 0) then
      end; //if not mbForceDirectory(edDirTarget.Text) then
    end; //if iVolumeSize <= 0 then
    CanClose:= (MyModalResult = mrOK);
  end;
  ModalResult := MyModalResult;
end;

end.

