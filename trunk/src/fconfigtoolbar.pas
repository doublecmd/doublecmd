{
    Double Commander
    -------------------------------------------------------------------------
    Configuration Toolbar

    Copyright (C) 2006-2010  Koblov Alexander (Alexx2000@mail.ru)

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
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Menus,
  StdCtrls, Buttons, KASToolBar, ExtCtrls, KASBarFiles;

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
    btnChangeButton: TButton;
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
    miAddSubMenu: TMenuItem;
    miInsertSeparator: TMenuItem;
    miAddSubBar: TMenuItem;
    OpenDialog: TOpenDialog;
    lblParameters: TLabel;
    pmChangeButton: TPopupMenu;
    sbIconExample: TSpeedButton;
    pnlToolBarFileName: TPanel;
    tbScrollBox: TScrollBox;
    lblSize: TLabel;
    lblStartPath: TLabel;
    lblToolTip: TLabel;
    procedure btnChangeButtonClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
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
    procedure miAddSubBarClick(Sender: TObject);
    procedure miAddSubMenuClick(Sender: TObject);
    procedure miInsertSeparatorClick(Sender: TObject);
    procedure sbIconExampleClick(Sender: TObject);

  private
    FBarFileName: UTF8String;
    LastToolButton : Integer;

    procedure FillActionLists;
    procedure ClearControls;
    procedure LoadButton(NumberOfButton: Integer);
    procedure Save;
    function  GetSelectedButton: Integer;
    procedure InsertButton(InsertAt: Integer);
    function AddSpecialButton(const sCommand: AnsiString; out aFileName: UTF8String): Boolean;

  public
    constructor Create(TheOwner: TComponent; const aBarFileName: UTF8String); reintroduce;
  end;

  function ShowConfigToolbar(const aBarFileName: UTF8String; iButtonIndex : Integer = -1): Boolean;

implementation

uses
  ActnList, LCLProc, HelpIntfs, uClassesEx, uOSForms, uPixMapManager,
  uGlobsPaths, uGlobs, uDCUtils, uOSUtils;

function ShowConfigToolbar(const aBarFileName: UTF8String; iButtonIndex : Integer = -1): Boolean;
begin
  with TfrmConfigToolBar.Create(Application, aBarFileName) do
  try
    ktbBar.Tag := iButtonIndex; // Selected button index
    Result:= (ShowModal = mrOK);
  finally
    Free;
  end;
end;

const
  cOpenBar = 'cm_OpenBar';
  cShowButtonMenu = 'cm_ShowButtonMenu';

{ TfrmConfigToolBar }

constructor TfrmConfigToolBar.Create(TheOwner: TComponent; const aBarFileName:UTF8String);
begin
  FBarFileName:= aBarFileName;
  LastToolButton := -1;
  inherited Create(TheOwner);
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
  try
    IniBarFile:= TIniFileEx.Create(FBarFileName);
    ktbBar.LoadFromIniFile(IniBarFile);
  finally
    FreeThenNil(IniBarFile);
  end;
  with pnlToolBarFileName do
  begin
    Caption:= MinimizeFilePath(FBarFileName, Canvas, Width);
    Hint:= FBarFileName;
  end;
  if ktbBar.Tag >= 0 then
    begin
      ktbBar.Buttons[ktbBar.Tag].Click;
      ktbBar.Buttons[ktbBar.Tag].Down := True;
    end
  else
    begin
      if ktbBar.ButtonCount > 0 then
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
  OpenDialog.DefaultExt:= '.bar';
  OpenDialog.Filter:= '*.bar|*.bar';
  if OpenDialog.Execute then
    begin
      try
        IniBarFile:= TIniFileEx.Create(OpenDialog.FileName);
        ktbBar.LoadFromIniFile(IniBarFile);
      finally
        FreeThenNil(IniBarFile);
      end;
      FBarFileName:= OpenDialog.FileName;
      with pnlToolBarFileName do
      begin
        Caption:= MinimizeFilePath(FBarFileName, Canvas, Width);
        Hint:= FBarFileName;
      end;
      ClearControls;
      if ktbBar.ButtonCount > 0 then
        begin
          ktbBar.Buttons[ktbBar.ButtonCount-1].Down := True;
          LoadButton(ktbBar.ButtonCount-1);
          LastToolButton := ktbBar.ButtonCount-1;
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

procedure TfrmConfigToolBar.btnChangeButtonClick(Sender: TObject);
var
  Point: TPoint;
begin
  with btnChangeButton do
  Point:= Classes.Point(Left, Top + Height);
  Point:= ClientToScreen(Point);
  pmChangeButton.PopUp(Point.X, Point.Y);
end;

procedure TfrmConfigToolBar.btnHelpClick(Sender: TObject);
begin
  ShowHelpOrErrorForKeyword('', '/toolbar.html');
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

  gToolBarFlat:= cbFlatIcons.Checked;
  gToolBarButtonSize:= StrToIntDef(edtBarSize.Text, 16);
  gToolBarSmallIcons:= cbSmallIcons.Checked;
  if gToolBarSmallIcons then
    gToolBarIconSize:= StrToIntDef(edtSmallIconSize.Text, 16);

  IniBarFile:= TIniFileEx.Create(FBarFileName);
  try
    IniBarFile.CacheUpdates:= True;
    ktbBar.SaveToIniFile(IniBarFile);
    IniBarFile.UpdateFile;
  finally
    FreeAndNil(IniBarFile);
  end;

  Close;

  ModalResult:= mrOK;
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
var
  Bitmap: TBitmap;
begin
  OpenDialog.DefaultExt:= EmptyStr;
  OpenDialog.Filter:= EmptyStr;
  if OpenDialog.Execute then
    begin
      cbCommand.Text := OpenDialog.FileName;
      edtStartPath.Text:= ExtractFilePath(OpenDialog.FileName);
      kedtIconFileName.Text:= OpenDialog.FileName;
      edtToolTip.Text:= ExtractOnlyFileName(OpenDialog.FileName);

      Bitmap := LoadBitmapFromFile(kedtIconFileName.Text, 32, Color);
      sbIconExample.Glyph.Assign(Bitmap);
      FreeThenNil(Bitmap);

      // Refresh icon on the toolbar.
      ktbBar.SetButtonX(LastToolButton, ButtonX, kedtIconFileName.Text);
    end;
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
      FreeThenNil(Bitmap);

      // Refresh icon on the toolbar.
      ktbBar.SetButtonX(LastToolButton, ButtonX, kedtIconFileName.Text);
    end;
end;

procedure TfrmConfigToolBar.miAddSubBarClick(Sender: TObject);
var
  sFileName: UTF8String;
begin
  if AddSpecialButton(cOpenBar, sFileName) then
    begin
      cbCommand.Text:= cOpenBar;
      edtParams.Text:= SetCmdDirAsEnvVar(sFileName);
      edtToolTip.Text:= Actions.GetCommandCaption(cOpenBar);
    end;
end;

procedure TfrmConfigToolBar.miAddSubMenuClick(Sender: TObject);
var
  sFileName: UTF8String;
begin
  if AddSpecialButton(cShowButtonMenu, sFileName) then
    begin
      cbCommand.Text:= cShowButtonMenu;
      edtParams.Text:= SetCmdDirAsEnvVar(sFileName);
      edtToolTip.Text:= Actions.GetCommandCaption(cShowButtonMenu);
    end;
end;

procedure TfrmConfigToolBar.miInsertSeparatorClick(Sender: TObject);
begin
  cbCommand.Text:= EmptyStr;
  edtParams.Text:= EmptyStr;
  edtStartPath.Text:= EmptyStr;
  kedtIconFileName.Text:= EmptyStr;
  edtToolTip.Text:= '-';
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

function TfrmConfigToolBar.AddSpecialButton(const sCommand: AnsiString; out aFileName: UTF8String): Boolean;
var
  IniBarFile: TIniFileEx;
  sIconFileName: UTF8String;
begin
  Result:= False;
  OpenDialog.DefaultExt:= '.bar';
  OpenDialog.Filter:= '*.bar|*.bar';
  if OpenDialog.Execute then
    begin
      aFileName:= OpenDialog.FileName;
      if not mbFileExists(aFileName) then
        try
          IniBarFile:= TIniFileEx.Create(aFileName);
          if SameText(sCommand, cOpenBar) then
            begin
              sIconFileName:= gpPixmapPath + IntToStr(gIconsSize) + 'x' + IntToStr(gIconsSize) + PathDelim + 'actions' + PathDelim + 'go-up.png';
              IniBarFile.WriteInteger('ButtonBar', 'ButtonCount', 1);
              IniBarFile.WriteString('ButtonBar', 'cmd1', cOpenBar);
              IniBarFile.WriteString('ButtonBar', 'param1', SetCmdDirAsEnvVar(FBarFileName));
              IniBarFile.WriteString('ButtonBar', 'menu1', Actions.GetCommandCaption(cOpenBar));
              IniBarFile.WriteString('ButtonBar', 'button1', SetCmdDirAsEnvVar(sIconFileName));
            end;
        finally
          FreeThenNil(IniBarFile);
        end;
      Result:= ShowConfigToolbar(aFileName);
    end;
end;

initialization
  {$I fconfigtoolbar.lrs}

end.

