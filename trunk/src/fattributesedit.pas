{
   Double Commander
   -------------------------------------------------------------------------
   Graphic control that allows choosing file attributes.

   Copyright (C) 2010 Przemysław Nagay (cobines@gmail.com)

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

unit fAttributesEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, ExtCtrls, StdCtrls, Buttons;

type

  { TfrmAttributesEdit }

  TfrmAttributesEdit = class(TForm)
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    btnReset: TButton;
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
    cbDirectory: TCheckBox;
    cbSymlink: TCheckBox;
    cbCompressed: TCheckBox;
    cbEncrypted: TCheckBox;
    cbTemporary: TCheckBox;
    cbSparse: TCheckBox;
    cbArchive: TCheckBox;
    cbHidden: TCheckBox;
    cbReadOnly: TCheckBox;
    cbSystem: TCheckBox;
    edtTextAttrs: TEdit;
    gbWinGeneral: TGroupBox;
    gbNtfsAttributes: TGroupBox;
    lblAttrBitsStr: TLabel;
    lblAttrGroupStr: TLabel;
    lblAttrOtherStr: TLabel;
    lblAttrOwnerStr: TLabel;
    lblTextAttrs: TLabel;
    lblExec: TLabel;
    lblRead: TLabel;
    lblWrite: TLabel;
    pnlTextAttrs: TPanel;
    pnlTopAttrs: TPanel;
    pnlUnixAttrs: TPanel;
    pnlWinAttrs: TPanel;
    pnlButtons: TPanel;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure cbAttrCheckBoxChanged(Sender: TObject);
    procedure cbAttrCheckBoxClicked(Sender: TObject);

  private
    FOnOk: TNotifyEvent;
    FUpdatingControls: Boolean;

    function GetAttrsAsText: String;
    procedure UpdateText;
    function GetCheckStr(CheckBox: TCheckBox; AttrStr: String): String;

  public
    constructor Create(TheOwner: TComponent); override;

    procedure Reset;

    property OnOk: TNotifyEvent read FOnOk write FOnOk;
    property AttrsAsText: String read GetAttrsAsText;
  end; 

implementation

{$R *.lfm}

procedure TfrmAttributesEdit.btnOkClick(Sender: TObject);
begin
  if Assigned(FOnOk) then
    FOnOk(Self);
  Close;
end;

procedure TfrmAttributesEdit.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmAttributesEdit.btnResetClick(Sender: TObject);
begin
  Reset;
end;

procedure TfrmAttributesEdit.cbAttrCheckBoxChanged(Sender: TObject);
begin
  // Note: OnChange may work incorrectly with tri-state checkboxes,
  // so OnClick is also used.
  UpdateText;
end;

procedure TfrmAttributesEdit.cbAttrCheckBoxClicked(Sender: TObject);
begin
  UpdateText;
end;

constructor TfrmAttributesEdit.Create(TheOwner: TComponent);
begin
  FOnOk := nil;
  FUpdatingControls := False;
  inherited Create(TheOwner);
{$IF DEFINED(MSWINDOWS)}
  pnlWinAttrs.Visible := True;
{$ELSEIF DEFINED(UNIX)}
  pnlUnixAttrs.Visible := True;
{$ENDIF}
end;

procedure TfrmAttributesEdit.Reset;
begin
  FUpdatingControls := True;

  cbDirectory.State  := cbGrayed;
  cbSymlink.State    := cbGrayed;

  cbReadOwner.State  := cbGrayed;
  cbWriteOwner.State := cbGrayed;
  cbExecOwner.State  := cbGrayed;
  cbReadGroup.State  := cbGrayed;
  cbWriteGroup.State := cbGrayed;
  cbExecGroup.State  := cbGrayed;
  cbReadOther.State  := cbGrayed;
  cbWriteOther.State := cbGrayed;
  cbExecOther.State  := cbGrayed;
  cbSuid.State       := cbGrayed;
  cbSgid.State       := cbGrayed;
  cbSticky.State     := cbGrayed;

  cbArchive.State    := cbGrayed;
  cbReadOnly.State   := cbGrayed;
  cbHidden.State     := cbGrayed;
  cbSystem.State     := cbGrayed;
  cbCompressed.State := cbGrayed;
  cbEncrypted.State  := cbGrayed;
  cbTemporary.State  := cbGrayed;
  cbSparse.State     := cbGrayed;

  edtTextAttrs.Text := '';
  edtTextAttrs.Text  := '';

  FUpdatingControls := False;
end;

function TfrmAttributesEdit.GetAttrsAsText: String;
begin
  Result := edtTextAttrs.Text;
end;

procedure TfrmAttributesEdit.UpdateText;
var
  s: string = '';
begin
  if not FUpdatingControls then
  begin
    FUpdatingControls := True;

    s := s + GetCheckStr(cbDirectory, 'd');
    s := s + GetCheckStr(cbSymlink,   'l');

    if pnlUnixAttrs.Visible then
    begin
      s := s + GetCheckStr(cbReadOwner,  'ur');
      s := s + GetCheckStr(cbWriteOwner, 'uw');
      s := s + GetCheckStr(cbExecOwner,  'ux');
      s := s + GetCheckStr(cbReadGroup,  'gr');
      s := s + GetCheckStr(cbWriteGroup, 'gw');
      s := s + GetCheckStr(cbExecGroup,  'gx');
      s := s + GetCheckStr(cbReadOther,  'or');
      s := s + GetCheckStr(cbWriteOther, 'ow');
      s := s + GetCheckStr(cbExecOther,  'ox');
      s := s + GetCheckStr(cbSuid,       'us');
      s := s + GetCheckStr(cbSgid,       'gs');
      s := s + GetCheckStr(cbSticky,     'sb');
    end;

    if pnlWinAttrs.Visible then
    begin
      s := s + GetCheckStr(cbArchive,    'a');
      s := s + GetCheckStr(cbReadOnly,   'r');
      s := s + GetCheckStr(cbHidden,     'h');
      s := s + GetCheckStr(cbSystem,     's');
      s := s + GetCheckStr(cbCompressed, 'c');
      s := s + GetCheckStr(cbEncrypted,  'e');
      s := s + GetCheckStr(cbTemporary,  't');
      s := s + GetCheckStr(cbSparse,     's');
    end;

    edtTextAttrs.Text := s;

    FUpdatingControls := False;
  end;
end;

function TfrmAttributesEdit.GetCheckStr(CheckBox: TCheckBox; AttrStr: String): String;
begin
  case CheckBox.State of
    cbChecked:
      Result := AttrStr + '+';
    cbUnchecked:
      Result := AttrStr + '-';
    else
      Result := '';
  end;
end;

end.

