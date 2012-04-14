{
    Double Commander
    -------------------------------------------------------------------------
    Change file properties dialog

    Copyright (C) 2009  Koblov Alexander (Alexx2000@mail.ru)

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
  EditBtn, uFileSourceSetFilePropertyOperation, DCBasicTypes, ZVDateTimePicker;

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
    ZVCreationDateTime: TZVDateTimePicker;
    ZVLastWriteDateTime: TZVDateTimePicker;
    ZVLastAccessDateTime: TZVDateTimePicker;
    procedure btnCreationTimeClick(Sender: TObject);
    procedure btnLastAccessTimeClick(Sender: TObject);
    procedure btnLastWriteTimeClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure cbChangeModeClick(Sender: TObject);
    procedure chkChangeAttrClick(Sender: TObject);
    procedure chkCreationTimeChange(Sender: TObject);
    procedure chkLastAccessTimeChange(Sender: TObject);
    procedure chkLastWriteTimeChange(Sender: TObject);
    procedure edtOctalKeyPress(Sender: TObject; var Key: char);
    procedure edtOctalKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure ZVCreationDateTimeChange(Sender: TObject);
    procedure ZVLastAccessDateTimeChange(Sender: TObject);
    procedure ZVLastWriteDateTimeChange(Sender: TObject);
  private
    FOperation: TFileSourceSetFilePropertyOperation;
    FChangeTriggersEnabled: Boolean;
    procedure ShowMode(Mode: TFileAttrs);
    procedure ShowAttr(Attr: TFileAttrs);
    function GetModeFromForm: TFileAttrs;
    function GetAttrFromForm: TFileAttrs;
  public
    constructor Create(aOwner: TComponent; const aOperation: TFileSourceSetFilePropertyOperation); reintroduce;
  end;

function ShowChangeFilePropertiesDialog(const aOperation: TFileSourceSetFilePropertyOperation): Boolean;

implementation

{$R *.lfm}

uses
  LCLType, DCFileAttributes, DCStrUtils, uDCUtils, uFileProperty;

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
begin
  with FOperation do
  begin
    if fpAttributes in SupportedProperties then
      begin
        if NewProperties[fpAttributes] is TNtfsFileAttributesProperty then
          (NewProperties[fpAttributes] as TNtfsFileAttributesProperty).Value:= GetAttrFromForm;
        if NewProperties[fpAttributes] is TUnixFileAttributesProperty then
          (NewProperties[fpAttributes] as TUnixFileAttributesProperty).Value:= GetModeFromForm;
      end;
    if chkCreationTime.Checked then
      (NewProperties[fpCreationTime] as TFileCreationDateTimeProperty).Value:= ZVCreationDateTime.DateTime
    else
      begin
        NewProperties[fpCreationTime].Free;
        NewProperties[fpCreationTime]:= nil;
      end;
    if chkLastWriteTime.Checked then
      (NewProperties[fpModificationTime] as TFileModificationDateTimeProperty).Value:= ZVLastWriteDateTime.DateTime
    else
      begin
        NewProperties[fpModificationTime].Free;
        NewProperties[fpModificationTime]:= nil;
      end;
    if chkLastAccessTime.Checked then
      (NewProperties[fpLastAccessTime] as TFileLastAccessDateTimeProperty).Value:= ZVLastAccessDateTime.DateTime
    else
      begin
        NewProperties[fpLastAccessTime].Free;
        NewProperties[fpLastAccessTime]:= nil;
      end;
    Recursive:= chkRecursive.Checked;
  end;
end;

procedure TfrmSetFileProperties.cbChangeModeClick(Sender: TObject);
begin
  if FChangeTriggersEnabled then
  begin
    FChangeTriggersEnabled := False;
    edtOctal.Text:= DecToOct(GetModeFromForm);
    FChangeTriggersEnabled := True;
  end;
end;

procedure TfrmSetFileProperties.chkChangeAttrClick(Sender: TObject);
begin
  // Called after checking any windows-check
end;

procedure TfrmSetFileProperties.edtOctalKeyPress(Sender: TObject; var Key: char);
begin
  if not ((Key in ['0'..'7']) or (Key = Chr(VK_BACK))) then
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

function TfrmSetFileProperties.GetModeFromForm: TFileAttrs;
begin
  Result:= 0;
  if cbReadOwner.Checked then Result:= (Result or S_IRUSR);
  if cbWriteOwner.Checked then Result:= (Result or S_IWUSR);
  if cbExecOwner.Checked then Result:= (Result or S_IXUSR);
  if cbReadGroup.Checked then Result:= (Result or S_IRGRP);
  if cbWriteGroup.Checked then Result:= (Result or S_IWGRP);
  if cbExecGroup.Checked then Result:= (Result or S_IXGRP);
  if cbReadOther.Checked then Result:= (Result or S_IROTH);
  if cbWriteOther.Checked then Result:= (Result or S_IWOTH);
  if cbExecOther.Checked then Result:= (Result or S_IXOTH);

  if cbSuid.Checked then Result:= (Result or S_ISUID);
  if cbSgid.Checked then Result:= (Result or S_ISGID);
  if cbSticky.Checked then Result:= (Result or S_ISVTX);
end;

function TfrmSetFileProperties.GetAttrFromForm: TFileAttrs;
begin
  Result:= 0;
  if chkArchive.Checked then Result:= (Result or FILE_ATTRIBUTE_ARCHIVE);
  if chkReadOnly.Checked then Result:= (Result or FILE_ATTRIBUTE_READONLY);
  if chkHidden.Checked then Result:= (Result or FILE_ATTRIBUTE_HIDDEN);
  if chkSystem.Checked then Result:= (Result or FILE_ATTRIBUTE_SYSTEM);
end;

constructor TfrmSetFileProperties.Create(aOwner: TComponent; const aOperation: TFileSourceSetFilePropertyOperation);
begin
  inherited Create(aOwner);
  FOperation:= aOperation;
  FChangeTriggersEnabled:= True;
  // Enable only supported file properties
  with FOperation do
  begin
    if fpAttributes in SupportedProperties then
      begin
        if NewProperties[fpAttributes] is TNtfsFileAttributesProperty then
          begin
            ShowAttr((NewProperties[fpAttributes] as TNtfsFileAttributesProperty).Value);
            gbWinAttributes.Show;
          end;
        if NewProperties[fpAttributes] is TUnixFileAttributesProperty then
          begin
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
end;

procedure TfrmSetFileProperties.btnLastAccessTimeClick(Sender: TObject);
begin
  ZVLastAccessDateTime.DateTime:= Now;
end;

procedure TfrmSetFileProperties.btnLastWriteTimeClick(Sender: TObject);
begin
  ZVLastWriteDateTime.DateTime:= Now;
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

end.

