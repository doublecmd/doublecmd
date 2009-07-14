{
    Double Commander
    -------------------------------------------------------------------------
    Configuration Toolbar

    Copyright (C) 2006-2009  Koblov Alexander (Alexx2000@mail.ru)

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

unit fConfigToolBar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons, KASToolBar,
  ExtCtrls, KASBarFiles;

type

  { TfrmConfigToolBar }

  TfrmConfigToolBar = class(TForm)
    btnInsertButton: TButton;
    edtSmallIconSize: TEdit;
    lblButtonBar: TLabel;
    lblCommand: TLabel;
    btnCancel: TButton;
    gbGroupBox: TGroupBox;
    lblIcon: TLabel;
    lblIconFile: TLabel;
    btnAddButton: TButton;
    ktbBar: TKASToolBar;
    btnOpenBarFile: TButton;
    edtBarSize: TEdit;
    cbCommand: TComboBox;
    btnDeleteButton: TButton;
    btnOpenFile: TButton;
    btnAddSubBar: TButton;
    btnOpenIconFile: TButton;
    kedtIconFileName: TEdit;
    edtParams: TEdit;
    edtStartPath: TEdit;
    edtToolTip: TEdit;
    cbFlatIcons: TCheckBox;
    btnHelp: TButton;
    cbSmallIcons: TCheckBox;
    lblLabel: TLabel;
    btnOK: TButton;
    OpenDialog: TOpenDialog;
    lblParameters: TLabel;
    sbIconExample: TSpeedButton;
    pnlToolBarFileName: TPanel;
    tbScrollBox: TScrollBox;
    lblSize: TLabel;
    lblStartPath: TLabel;
    lblToolTip: TLabel;
    procedure btnInsertButtonClick(Sender: TObject);
    procedure btnOpenBarFileClick(Sender: TObject);
    procedure cbCommandSelect(Sender: TObject);
    procedure cbFlatIconsChange(Sender: TObject);
    procedure cbSmallIconsChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnAddButtonClick(Sender: TObject);
    function ktbBarLoadButtonGlyph(sIconFileName: String; iIconSize: Integer;
      clBackColor: TColor): TBitmap;
    procedure ktbBarToolButtonClick(Sender: TObject; NumberOfButton : Integer);
    procedure btnDeleteButtonClick(Sender: TObject);
    procedure btnOpenFileClick(Sender: TObject);
    procedure btnOpenIconFileClick(Sender: TObject);
    procedure sbIconExampleClick(Sender: TObject);

  private
    LastToolButton : Integer;

    procedure FillActionLists;
    procedure ClearControls;
    procedure LoadButton(NumberOfButton: Integer);
    procedure Save;
    function  GetSelectedButton: Integer;
    procedure InsertButton(InsertAt: Integer);

  public
    constructor Create(TheOwner: TComponent); override;
  end;

  procedure ShowConfigToolbar(iButtonIndex : Integer = -1);

implementation

uses
  ActnList, LCLProc, uClassesEx, fMain, uOSForms, uPixMapManager,
  uGlobsPaths, uGlobs, uDCUtils;

procedure ShowConfigToolbar(iButtonIndex : Integer = -1);
begin
  with TfrmConfigToolBar.Create(Application) do
  try
    ktbBar.Tag := iButtonIndex; // Selected button index
    ShowModal;
  finally
    Free;
  end;
end;

{ TfrmConfigToolBar }

constructor TfrmConfigToolBar.Create(TheOwner: TComponent);
begin
  LastToolButton := -1;
  inherited;
end;

procedure TfrmConfigToolBar.FillActionLists;
begin
  cbCommand.Items.AddStrings(Actions.CommandList);
  cbCommand.Sorted:=true;
end;

procedure TfrmConfigToolBar.FormShow(Sender: TObject);
var
  IniBarFile: TIniFileEx;
begin
  FillActionLists;
  edtBarSize.Text := IntToStr(gToolBarButtonSize);
  edtSmallIconSize.Text:= IntToStr(gToolBarIconSize);
  edtSmallIconSize.Enabled:= gToolBarSmallIcons;
  cbSmallIcons.Checked:= gToolBarSmallIcons;
  cbFlatIcons.Checked:= gToolBarFlat;
  sbIconExample.Flat:= gToolBarFlat;
  ktbBar.Flat:= gToolBarFlat;
  ktbBar.ChangePath:= gpExePath;
  ktbBar.EnvVar:= '%commander_path%';
  IniBarFile:= TIniFileEx.Create(gpIniDir + 'default.bar');
  ktbBar.LoadFromIniFile(IniBarFile);
  IniBarFile.Free;
  with pnlToolBarFileName do
  begin
    Caption:= MinimizeFilePath(gpIniDir + 'default.bar', Canvas, Width);
    Hint:= gpIniDir + 'default.bar';
  end;
  if ktbBar.Tag >= 0 then
    begin
      ktbBar.Buttons[ktbBar.Tag].Click;
      ktbBar.Buttons[ktbBar.Tag].Down := True;
    end
  else
    begin
      if ktbBar.ButtonCount>0 then
        begin
          ktbBar.Buttons[ktbBar.ButtonCount-1].Down := True;
          LoadButton(ktbBar.ButtonCount-1);
          LastToolButton := ktbBar.ButtonCount-1;
        end;
    end;
  Update;
  Height:= edtToolTip.Top + edtToolTip.Height + 18;
end;

procedure TfrmConfigToolBar.cbFlatIconsChange(Sender: TObject);
begin
  ktbBar.Flat := cbFlatIcons.Checked;
  sbIconExample.Flat := cbFlatIcons.Checked;
end;

procedure TfrmConfigToolBar.cbSmallIconsChange(Sender: TObject);
begin
  edtSmallIconSize.Enabled:= cbSmallIcons.Checked;
end;

procedure TfrmConfigToolBar.btnOpenBarFileClick(Sender: TObject);
var
  IniBarFile: TIniFileEx;
begin
  OpenDialog.FileName := pnlToolBarFileName.Hint;
  OpenDialog.Filter:= '*.bar|*.bar';
  if OpenDialog.Execute then
    begin
      IniBarFile:= TIniFileEx.Create(OpenDialog.FileName);
      ktbBar.LoadFromIniFile(IniBarFile);
      IniBarFile.Free;
      with pnlToolBarFileName do
      begin
        Caption:= MinimizeFilePath(OpenDialog.FileName, Canvas, Width);
        Hint:= OpenDialog.FileName;
      end;
    end;
end;

procedure TfrmConfigToolBar.btnInsertButtonClick(Sender: TObject);
var
  SelectedIndex: Integer = 0;
begin
  SelectedIndex := GetSelectedButton;
  if SelectedIndex = -1 then
    InsertButton(ktbBar.ButtonCount)
  else
    InsertButton(SelectedIndex);
end;

procedure TfrmConfigToolBar.cbCommandSelect(Sender: TObject);
begin
  edtToolTip.Text := Actions.GetCommandCaption(cbCommand.Items[cbCommand.ItemIndex]);
end;

procedure TfrmConfigToolBar.btnOKClick(Sender: TObject);
var
  IniBarFile: TIniFileEx;
begin
  Save;

  gToolBarButtonSize:= StrToIntDef(edtBarSize.Text, 20);
  gToolBarSmallIcons:= cbSmallIcons.Checked;
  gToolBarIconSize:= StrToIntDef(edtSmallIconSize.Text, 16);
  gToolBarFlat:= cbFlatIcons.Checked;

  if gToolBarSmallIcons then
    frmMain.MainToolBar.GlyphSize:= gToolBarIconSize
  else
    frmMain.MainToolBar.GlyphSize:= gToolBarButtonSize - 4;
  frmMain.MainToolBar.ButtonHeight:= gToolBarButtonSize;
  frmMain.MainToolBar.ButtonWidth:= gToolBarButtonSize;
  frmMain.MainToolBar.Flat:= gToolBarFlat;

  IniBarFile:= TIniFileEx.Create(gpIniDir + 'default.bar');
  try
    IniBarFile.CacheUpdates:= True;
    ktbBar.SaveToIniFile(IniBarFile);
    IniBarFile.UpdateFile;

    frmMain.MainToolBar.Clear;
    frmMain.MainToolBar.LoadFromIniFile(IniBarFile);

  finally
    FreeAndNil(IniBarFile);
  end;

  Close;
end;

(*Add new button on tool bar*)
procedure TfrmConfigToolBar.btnAddButtonClick(Sender: TObject);
begin
  InsertButton(ktbBar.ButtonCount);
end;

function TfrmConfigToolBar.ktbBarLoadButtonGlyph(sIconFileName: String;
  iIconSize: Integer; clBackColor: TColor): TBitmap;
begin
  Result := LoadBitmapFromFile(sIconFileName, iIconSize, clBackColor);
end;

(*Select button on panel*)
procedure TfrmConfigToolBar.ktbBarToolButtonClick(Sender: TObject; NumberOfButton : Integer);
begin
  Save;
  LoadButton(NumberOfButton);
  LastToolButton := NumberOfButton;
end;

procedure TfrmConfigToolBar.ClearControls;
begin
  cbCommand.Text := '';
  kedtIconFileName.Text := '';
  edtToolTip.Text := '';
  sbIconExample.Glyph := nil;
  edtParams.Text:= '';
  edtStartPath.Text:= '';
end;

procedure TfrmConfigToolBar.LoadButton(NumberOfButton: Integer);
begin
  // cbCommand.Text := ktbBar.Commands[NumberOfButton];
  cbCommand.Text := ktbBar.GetButtonX(NumberOfButton,CmdX);
  // kedtIconFileName.Text := ktbBar.Icons[NumberOfButton];
  kedtIconFileName.Text := ktbBar.GetButtonX(NumberOfButton,ButtonX);

  // edtToolTip.Text := ktbBar.Buttons[NumberOfButton].Hint;
  edtToolTip.Text := ktbBar.GetButtonX(NumberOfButton,MenuX);

  sbIconExample.Glyph := ktbBar.Buttons[NumberOfButton].Glyph;
  edtParams.Text:= ktbBar.GetButtonX(NumberOfButton,ParamX);
  edtStartPath.Text:= ktbBar.GetButtonX(NumberOfButton,PathX);
end;

(*Save current button*)
procedure TfrmConfigToolBar.Save;
begin
   if (LastToolButton >= 0) and (ktbBar.ButtonCount > 0) then
      begin
       //---------------------
       ktbBar.SetButtonX(LastToolButton,CmdX,cbCommand.Text);
       ktbBar.SetButtonX(LastToolButton,ParamX,edtParams.Text);
       ktbBar.SetButtonX(LastToolButton,PathX,edtStartPath.Text);
       ktbBar.SetButtonX(LastToolButton,ButtonX,kedtIconFileName.Text);
       ktbBar.SetButtonX(LastToolButton,MenuX,edtToolTip.Text);
       //---------------------
      end;
end;

(*Remove current button*)
procedure TfrmConfigToolBar.btnDeleteButtonClick(Sender: TObject);
begin
   if (LastToolButton >= 0) and (ktbBar.ButtonCount > 0) then
      begin
        ktbBar.RemoveButton(LastToolButton);
        ClearControls;

        if ktbBar.ButtonCount>0 then
        begin
          // Select next button or the last one.
          if (LastToolButton >= ktbBar.ButtonCount) then
            LastToolButton := ktbBar.ButtonCount - 1;

          ktbBar.Buttons[LastToolButton].Down := True;
          LoadButton(LastToolButton);
        end
        else
          LastToolButton := -1;
      end;
end;

procedure TfrmConfigToolBar.btnOpenFileClick(Sender: TObject);
begin
  OpenDialog.Filter:= '';
  if OpenDialog.Execute then
     cbCommand.Text := OpenDialog.FileName;
end;

procedure TfrmConfigToolBar.btnOpenIconFileClick(Sender: TObject);
var
  sFileName: String;
  Bitmap: TBitmap;
begin
  sFileName := kedtIconFileName.Text;
  if ShowOpenIconDialog(Self, sFileName) then
    begin
      kedtIconFileName.Text := sFileName;

      Bitmap := LoadBitmapFromFile(kedtIconFileName.Text, 32, Color);
      sbIconExample.Glyph := Bitmap;
      if Assigned(Bitmap) then
        FreeAndNil(Bitmap);

      // Refresh icon on the toolbar.
      ktbBar.SetButtonX(LastToolButton, ButtonX, kedtIconFileName.Text);
    end;
end;

procedure TfrmConfigToolBar.sbIconExampleClick(Sender: TObject);
begin
  btnOpenIconFileClick(Sender);
end;

function TfrmConfigToolBar.GetSelectedButton: Integer;
begin
  for Result := 0 to ktbBar.ButtonCount - 1 do
    if ktbBar.Buttons[Result].Down then
    begin
      Exit;
    end;
  Result := -1;
end;

procedure TfrmConfigToolBar.InsertButton(InsertAt: Integer);
begin
  Save;
  ClearControls;

  if (InsertAt >= 0) and (InsertAt < ktbBar.ButtonCount) then
  begin
    LastToolButton := ktbBar.InsertButtonX(InsertAt, '', '', '', '', '', '', '');
    ktbBar.Buttons[LastToolButton].Down := True;
  end
  else if (InsertAt = ktbBar.ButtonCount) then // insert at the end
  begin
    LastToolButton := ktbBar.AddButtonX('', '', '', '', '', '', '');
    ktbBar.Buttons[LastToolButton].Down := True;
  end
  else
    LastToolButton := -1;
end;

initialization
  {$I fconfigtoolbar.lrs}

end.

