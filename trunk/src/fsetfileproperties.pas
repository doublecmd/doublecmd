{
    Double Commander
    -------------------------------------------------------------------------
    Change file properties dialog

    Copyright (C) 2009-2015 Alexander Koblov (alexx2000@mail.ru)

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

unit fSetFileProperties;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Buttons,
  uFileSourceSetFilePropertyOperation, DCBasicTypes, DateTimePicker;

type

  { TfrmSetFileProperties }

  TfrmSetFileProperties = class(TForm)
    Bevel2: TBevel;
    Bevel1: TBevel;
    btnCancel: TBitBtn;
    btnCreationTime: TSpeedButton;
    btnLastAccessTime: TSpeedButton;
    btnLastWriteTime: TSpeedButton;
    btnOK: TBitBtn;
    cbExecGroup: TCheckBox;
    cbExecOther: TCheckBox;
    cbExecOwner: TCheckBox;
    cbReadGroup: TCheckBox;
    cbReadOther: TCheckBox;
    cbReadOwner: TCheckBox;
    cbSgid: TCheckBox;
    cbSticky: TCheckBox;
    cbSuid: TCheckBox;
    cbWriteGroup: TCheckBox;
    cbWriteOther: TCheckBox;
    cbWriteOwner: TCheckBox;
    chkArchive: TCheckBox;
    chkCreationTime: TCheckBox;
    chkHidden: TCheckBox;
    chkLastAccessTime: TCheckBox;
    chkLastWriteTime: TCheckBox;
    chkReadOnly: TCheckBox;
    chkRecursive: TCheckBox;
    chkSystem: TCheckBox;
    edtOctal: TEdit;
    gbTimeSamp: TGroupBox;
    gbWinAttributes: TGroupBox;
    gbUnixAttributes: TGroupBox;
    lblAttrBitsStr: TLabel;
    lblAttrGroupStr: TLabel;
    lblAttrInfo: TLabel;
    lblModeInfo: TLabel;
    lblAttrOtherStr: TLabel;
    lblAttrOwnerStr: TLabel;
    lblAttrText: TLabel;
    lblAttrTextStr: TLabel;
    lblExec: TLabel;
    lblOctal: TLabel;
    lblRead: TLabel;
    lblWrite: TLabel;
    DatesPanel: TPanel;
    ChecksPanel: TPanel;
    ZVCreationDateTime: TDateTimePicker;
    ZVLastWriteDateTime: TDateTimePicker;
    ZVLastAccessDateTime: TDateTimePicker;
    procedure btnCreationTimeClick(Sender: TObject);
    procedure btnLastAccessTimeClick(Sender: TObject);
    procedure btnLastWriteTimeClick(Sender: TObject);
    procedure SetOtherDateLikeThis(ReferenceZVDateTimePicker:TDateTimePicker);
    procedure btnOKClick(Sender: TObject);
    procedure cbChangeModeClick(Sender: TObject);
    procedure chkChangeAttrClick(Sender: TObject);
    procedure chkCreationTimeChange(Sender: TObject);
    procedure chkLastAccessTimeChange(Sender: TObject);
    procedure chkLastWriteTimeChange(Sender: TObject);
    procedure edtOctalKeyPress(Sender: TObject; var Key: char);
    procedure edtOctalKeyUp(Sender: TObject; var {%H-}Key: Word; {%H-}Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure ZVCreationDateTimeChange(Sender: TObject);
    procedure ZVLastAccessDateTimeChange(Sender: TObject);
    procedure ZVLastWriteDateTimeChange(Sender: TObject);
    procedure ZVCreationDateTimeClick(Sender: TObject);
    procedure ZVLastWriteDateTimeClick(Sender: TObject);
    procedure ZVLastAccessDateTimeClick(Sender: TObject);
  private
    FOperation: TFileSourceSetFilePropertyOperation;
    FChangeTriggersEnabled: Boolean;
    procedure ShowMode(Mode: TFileAttrs);
    procedure ShowAttr(Attr: TFileAttrs);
    procedure UpdateAllowGrayed(AllowGrayed: Boolean);
    function GetModeFromForm(out ExcludeAttrs: TFileAttrs): TFileAttrs;
    function GetAttrFromForm(out ExcludeAttrs: TFileAttrs): TFileAttrs;
  public
    constructor Create(aOwner: TComponent; const aOperation: TFileSourceSetFilePropertyOperation); reintroduce;
  end;

function ShowChangeFilePropertiesDialog(const aOperation: TFileSourceSetFilePropertyOperation): Boolean;

implementation

{$R *.lfm}

uses
  LCLType, DCFileAttributes, DCStrUtils, uDCUtils, uFileProperty, uKeyboard;

function ShowChangeFilePropertiesDialog(const aOperation: TFileSourceSetFilePropertyOperation): Boolean;
begin
  with TfrmSetFileProperties.Create(Application, aOperation) do
  try
    Result:= (ShowModal = mrOK);
  finally
    Free;
  end;
end;

{ TfrmSetFileProperties }

procedure TfrmSetFileProperties.btnOKClick(Sender: TObject);
var
  theNewProperties: TFileProperties;
begin
  with FOperation do
  begin
    theNewProperties:= NewProperties;
    if fpAttributes in SupportedProperties then
      begin
        if theNewProperties[fpAttributes] is TNtfsFileAttributesProperty then
          IncludeAttributes:= GetAttrFromForm(ExcludeAttributes);
        if theNewProperties[fpAttributes] is TUnixFileAttributesProperty then
          IncludeAttributes:= GetModeFromForm(ExcludeAttributes);
        // Nothing changed, clear new property
        if (IncludeAttributes = 0) and (ExcludeAttributes = 0) then
        begin
          theNewProperties[fpAttributes].Free;
          theNewProperties[fpAttributes]:= nil;
        end;
      end;
    if chkCreationTime.Checked then
      (theNewProperties[fpCreationTime] as TFileCreationDateTimeProperty).Value:= ZVCreationDateTime.DateTime
    else
      begin
        theNewProperties[fpCreationTime].Free;
        theNewProperties[fpCreationTime]:= nil;
      end;
    if chkLastWriteTime.Checked then
      (theNewProperties[fpModificationTime] as TFileModificationDateTimeProperty).Value:= ZVLastWriteDateTime.DateTime
    else
      begin
        theNewProperties[fpModificationTime].Free;
        theNewProperties[fpModificationTime]:= nil;
      end;
    if chkLastAccessTime.Checked then
      (theNewProperties[fpLastAccessTime] as TFileLastAccessDateTimeProperty).Value:= ZVLastAccessDateTime.DateTime
    else
      begin
        theNewProperties[fpLastAccessTime].Free;
        theNewProperties[fpLastAccessTime]:= nil;
      end;
    NewProperties:= theNewProperties;
    Recursive:= chkRecursive.Checked;
  end;
end;

procedure TfrmSetFileProperties.cbChangeModeClick(Sender: TObject);
var
  AMode, ExcludeAttrs: TFileAttrs;
  CheckBox: TCheckBox absolute Sender;
begin
  if FChangeTriggersEnabled then
  begin
    FChangeTriggersEnabled := False;
    if CheckBox.State = cbGrayed then
    begin
      edtOctal.Text:= EmptyStr;
      lblAttrText.Caption:= EmptyStr;
    end
    else begin
      AMode:= GetModeFromForm(ExcludeAttrs);
      edtOctal.Text:= DecToOct(AMode);
      lblAttrText.Caption:= FormatUnixAttributes(AMode);
    end;
    FChangeTriggersEnabled := True;
  end;
end;

procedure TfrmSetFileProperties.chkChangeAttrClick(Sender: TObject);
begin
  // Called after checking any windows-check
end;

procedure TfrmSetFileProperties.edtOctalKeyPress(Sender: TObject; var Key: char);
begin
  if not ((Key in ['0'..'7']) or (Key = Chr(VK_BACK)) or (Key = Chr(VK_DELETE))) then
    Key:= #0;
end;

procedure TfrmSetFileProperties.edtOctalKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if FChangeTriggersEnabled then
  begin
    FChangeTriggersEnabled := False;
    ShowMode(OctToDec(edtOctal.Text));
    FChangeTriggersEnabled := True;
  end;
end;

procedure TfrmSetFileProperties.FormCreate(Sender: TObject);
begin

end;

procedure TfrmSetFileProperties.ShowMode(Mode: TFileAttrs);
begin
  cbReadOwner.Checked:= ((Mode and S_IRUSR) = S_IRUSR);
  cbWriteOwner.Checked:= ((Mode and S_IWUSR) = S_IWUSR);
  cbExecOwner.Checked:= ((Mode and S_IXUSR) = S_IXUSR);

  cbReadGroup.Checked:= ((Mode and S_IRGRP) = S_IRGRP);
  cbWriteGroup.Checked:= ((Mode and S_IWGRP) = S_IWGRP);
  cbExecGroup.Checked:= ((Mode and S_IXGRP) = S_IXGRP);

  cbReadOther.Checked:= ((Mode and S_IROTH) = S_IROTH);
  cbWriteOther.Checked:= ((Mode and S_IWOTH) = S_IWOTH);
  cbExecOther.Checked:= ((Mode and S_IXOTH) = S_IXOTH);

  cbSuid.Checked:= ((Mode and S_ISUID) = S_ISUID);
  cbSgid.Checked:= ((Mode and S_ISGID) = S_ISGID);
  cbSticky.Checked:= ((Mode and S_ISVTX) = S_ISVTX);
end;

procedure TfrmSetFileProperties.ShowAttr(Attr: TFileAttrs);
begin
  chkArchive.Checked:= ((Attr and FILE_ATTRIBUTE_ARCHIVE) <> 0);
  chkReadOnly.Checked:= ((Attr and FILE_ATTRIBUTE_READONLY) <> 0);
  chkHidden.Checked:= ((Attr and FILE_ATTRIBUTE_HIDDEN) <> 0);
  chkSystem.Checked:= ((Attr and FILE_ATTRIBUTE_SYSTEM) <> 0);
end;

procedure TfrmSetFileProperties.UpdateAllowGrayed(AllowGrayed: Boolean);
var
  Index: Integer;
begin
  lblAttrInfo.Visible:= AllowGrayed;
  for Index:= 0 to gbWinAttributes.ControlCount - 1 do
  begin
    if gbWinAttributes.Controls[Index] is TCheckBox then
      TCheckBox(gbWinAttributes.Controls[Index]).AllowGrayed:= AllowGrayed;
  end;
  lblModeInfo.Visible:= AllowGrayed;
  for Index:= 0 to gbUnixAttributes.ControlCount - 1 do
  begin
    if gbUnixAttributes.Controls[Index] is TCheckBox then
      TCheckBox(gbUnixAttributes.Controls[Index]).AllowGrayed:= AllowGrayed;
  end;
end;

function TfrmSetFileProperties.GetModeFromForm(out ExcludeAttrs: TFileAttrs): TFileAttrs;
begin
  Result:= 0;
  ExcludeAttrs:= 0;
  case cbReadOwner.State of
    cbChecked:  Result:= (Result or S_IRUSR);
    cbUnchecked: ExcludeAttrs:= ExcludeAttrs or S_IRUSR;
  end;
  case cbWriteOwner.State of
    cbChecked: Result:= (Result or S_IWUSR);
    cbUnchecked: ExcludeAttrs:= ExcludeAttrs or S_IWUSR;
  end;
  case cbExecOwner.State of
    cbChecked: Result:= (Result or S_IXUSR);
    cbUnchecked: ExcludeAttrs:= ExcludeAttrs or S_IXUSR;
  end;
  case cbReadGroup.State of
    cbChecked: Result:= (Result or S_IRGRP);
    cbUnchecked: ExcludeAttrs:= ExcludeAttrs or S_IRGRP;
  end;
  case cbWriteGroup.State of
    cbChecked: Result:= (Result or S_IWGRP);
    cbUnchecked: ExcludeAttrs:= ExcludeAttrs or S_IWGRP;
  end;
  case cbExecGroup.State of
    cbChecked: Result:= (Result or S_IXGRP);
    cbUnchecked: ExcludeAttrs:= ExcludeAttrs or S_IXGRP;
  end;
  case cbReadOther.State of
    cbChecked: Result:= (Result or S_IROTH);
    cbUnchecked: ExcludeAttrs:= ExcludeAttrs or S_IROTH;
  end;
  case cbWriteOther.State of
    cbChecked: Result:= (Result or S_IWOTH);
    cbUnchecked: ExcludeAttrs:= ExcludeAttrs or S_IWOTH;
  end;
  case cbExecOther.State of
    cbChecked: Result:= (Result or S_IXOTH);
    cbUnchecked: ExcludeAttrs:= ExcludeAttrs or S_IXOTH;
  end;

  case cbSuid.State of
    cbChecked: Result:= (Result or S_ISUID);
    cbUnchecked: ExcludeAttrs:= ExcludeAttrs or S_ISUID;
  end;
  case cbSgid.State of
    cbChecked: Result:= (Result or S_ISGID);
    cbUnchecked: ExcludeAttrs:= ExcludeAttrs or S_ISGID;
  end;
  case cbSticky.State of
    cbChecked: Result:= (Result or S_ISVTX);
    cbUnchecked: ExcludeAttrs:= ExcludeAttrs or S_ISVTX;
  end;
end;

function TfrmSetFileProperties.GetAttrFromForm(out ExcludeAttrs: TFileAttrs): TFileAttrs;
begin
  Result:= 0;
  ExcludeAttrs:= 0;
  case chkArchive.State of
    cbChecked: Result:= (Result or FILE_ATTRIBUTE_ARCHIVE);
    cbUnchecked: ExcludeAttrs:= ExcludeAttrs or FILE_ATTRIBUTE_ARCHIVE;
  end;
  case chkReadOnly.State of
    cbChecked: Result:= (Result or FILE_ATTRIBUTE_READONLY);
    cbUnchecked: ExcludeAttrs:= ExcludeAttrs or FILE_ATTRIBUTE_READONLY;
  end;
  case chkHidden.State of
    cbChecked: Result:= (Result or FILE_ATTRIBUTE_HIDDEN);
    cbUnchecked: ExcludeAttrs:= ExcludeAttrs or FILE_ATTRIBUTE_HIDDEN;
  end;
  case chkSystem.State of
    cbChecked: Result:= (Result or FILE_ATTRIBUTE_SYSTEM);
    cbUnchecked: ExcludeAttrs:= ExcludeAttrs or FILE_ATTRIBUTE_SYSTEM;
  end;
end;

constructor TfrmSetFileProperties.Create(aOwner: TComponent; const aOperation: TFileSourceSetFilePropertyOperation);
begin
  inherited Create(aOwner);
  FOperation:= aOperation;
  FChangeTriggersEnabled:= True;
  ZVCreationDateTime.DateTime:= NullDate;
  ZVLastWriteDateTime.DateTime:= NullDate;
  ZVLastAccessDateTime.DateTime:= NullDate;
  // Enable only supported file properties
  with FOperation do
  begin
    if fpAttributes in SupportedProperties then
      begin
        UpdateAllowGrayed((TargetFiles.Count > 1) or TargetFiles[0].IsDirectory);
        if NewProperties[fpAttributes] is TNtfsFileAttributesProperty then
        begin
          if TargetFiles.Count = 1 then
            ShowAttr((NewProperties[fpAttributes] as TNtfsFileAttributesProperty).Value);
          gbWinAttributes.Show;
        end;
        if NewProperties[fpAttributes] is TUnixFileAttributesProperty then
        begin
          if TargetFiles.Count = 1 then
            ShowMode((NewProperties[fpAttributes] as TUnixFileAttributesProperty).Value);
          gbUnixAttributes.Show;
        end;
      end;
    if (fpCreationTime in SupportedProperties) and Assigned(NewProperties[fpCreationTime]) then
      begin
        ZVCreationDateTime.DateTime:= (NewProperties[fpCreationTime] as TFileCreationDateTimeProperty).Value;
        ZVCreationDateTime.Enabled:= True;
        chkCreationTime.Enabled:= True;
        btnCreationTime.Enabled:= True;
      end;
    if (fpModificationTime in SupportedProperties) and Assigned(NewProperties[fpModificationTime]) then
      begin
        ZVLastWriteDateTime.DateTime:= (NewProperties[fpModificationTime] as TFileModificationDateTimeProperty).Value;
        ZVLastWriteDateTime.Enabled:= True;
        chkLastWriteTime.Enabled:= True;
        btnLastWriteTime.Enabled:= True;
      end;
    if (fpLastAccessTime in SupportedProperties) and Assigned(NewProperties[fpLastAccessTime]) then
      begin
        ZVLastAccessDateTime.DateTime:= (NewProperties[fpLastAccessTime] as TFileLastAccessDateTimeProperty).Value;
        ZVLastAccessDateTime.Enabled:= True;
        chkLastAccessTime.Enabled:= True;
        btnLastAccessTime.Enabled:= True;
      end;
  end;
  chkCreationTime.Checked:=False;
  chkLastWriteTime.Checked:=False;
  chkLastAccessTime.Checked:=False;
end;

procedure TfrmSetFileProperties.btnCreationTimeClick(Sender: TObject);
begin
  ZVCreationDateTime.DateTime:= Now;
  if not chkCreationTime.Checked then chkCreationTime.Checked:=TRUE;
  if ssCtrl in GetKeyShiftStateEx then SetOtherDateLikeThis(ZVCreationDateTime);
end;

procedure TfrmSetFileProperties.btnLastWriteTimeClick(Sender: TObject);
begin
  ZVLastWriteDateTime.DateTime:= Now;
  if not chkLastWriteTime.Checked then chkLastWriteTime.Checked:=TRUE;
  if ssCtrl in GetKeyShiftStateEx then SetOtherDateLikeThis(ZVLastWriteDateTime);
end;

procedure TfrmSetFileProperties.btnLastAccessTimeClick(Sender: TObject);
begin
  ZVLastAccessDateTime.DateTime:= Now;
  if not chkLastAccessTime.Checked then chkLastAccessTime.Checked:=TRUE;
  if ssCtrl in GetKeyShiftStateEx then SetOtherDateLikeThis(ZVLastAccessDateTime);
end;

procedure TfrmSetFileProperties.SetOtherDateLikeThis(ReferenceZVDateTimePicker:TDateTimePicker);
begin
  if ReferenceZVDateTimePicker<>ZVCreationDateTime then
  begin
    ZVCreationDateTime.DateTime:=ReferenceZVDateTimePicker.DateTime;
    chkCreationTime.Checked:=TRUE;
  end;

  if ReferenceZVDateTimePicker<>ZVLastWriteDateTime then
  begin
    ZVLastWriteDateTime.DateTime:=ReferenceZVDateTimePicker.DateTime;
    chkLastWriteTime.Checked:=TRUE;
  end;

  if ReferenceZVDateTimePicker<>ZVLastAccessDateTime then
  begin
    ZVLastAccessDateTime.DateTime:=ReferenceZVDateTimePicker.DateTime;
    chkLastAccessTime.Checked:=TRUE;
   end;

  ReferenceZVDateTimePicker.SetFocus;
end;

procedure TfrmSetFileProperties.chkCreationTimeChange(Sender: TObject);
begin
  UpdateColor(ZVCreationDateTime, chkCreationTime.Checked);
  if (chkCreationTime.Checked and Visible) then ZVCreationDateTime.SetFocus;
end;

procedure TfrmSetFileProperties.chkLastAccessTimeChange(Sender: TObject);
begin
  UpdateColor(ZVLastAccessDateTime, chkLastAccessTime.Checked);
  if (chkLastAccessTime.Checked and Visible) then ZVLastAccessDateTime.SetFocus;
end;

procedure TfrmSetFileProperties.chkLastWriteTimeChange(Sender: TObject);
begin
  UpdateColor(ZVLastWriteDateTime, chkLastWriteTime.Checked);
  if (chkLastWriteTime.Checked and Visible) then ZVLastWriteDateTime.SetFocus;
end;

procedure TfrmSetFileProperties.ZVCreationDateTimeChange(Sender: TObject);
begin
  chkCreationTime.Checked:=True;
end;

procedure TfrmSetFileProperties.ZVLastAccessDateTimeChange(Sender: TObject);
begin
  chkLastAccessTime.Checked:=True;
end;

procedure TfrmSetFileProperties.ZVLastWriteDateTimeChange(Sender: TObject);
begin
  chkLastWriteTime.Checked:=True;
end;

procedure TfrmSetFileProperties.ZVCreationDateTimeClick(Sender: TObject);
begin
  if ssCtrl in GetKeyShiftStateEx then SetOtherDateLikeThis(ZVCreationDateTime);
end;

procedure TfrmSetFileProperties.ZVLastWriteDateTimeClick(Sender: TObject);
begin
  if ssCtrl in GetKeyShiftStateEx then SetOtherDateLikeThis(ZVLastWriteDateTime);
end;

procedure TfrmSetFileProperties.ZVLastAccessDateTimeClick(Sender: TObject);
begin
  if ssCtrl in GetKeyShiftStateEx then SetOtherDateLikeThis(ZVLastAccessDateTime);
end;

end.

