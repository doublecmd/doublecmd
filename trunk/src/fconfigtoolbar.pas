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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus,
  StdCtrls, Buttons, KASToolBar, ExtCtrls, ComCtrls, KASBarFiles;

type

  { TfrmConfigToolBar }

  TfrmConfigToolBar = class(TForm)
    btnAppendButton: TButton;
    btnCloneButton: TButton;
    cbIsSeparator: TCheckBox;
    lblIconSize: TLabel;
    lblIconSizeValue: TLabel;
    lblBarSizeValue: TLabel;
    trbBarSize: TTrackBar;
    trbIconSize: TTrackBar;
    lblButtonBar: TLabel;
    lblCommand: TLabel;
    btnCancel: TButton;
    gbGroupBox: TGroupBox;
    lblIcon: TLabel;
    lblIconFile: TLabel;
    ktbBar: TKASToolBar;
    btnOpenBarFile: TButton;
    cbCommand: TComboBox;
    btnDeleteButton: TButton;
    btnOpenFile: TButton;
    btnAppendMore: TButton;
    kedtIconFileName: TEdit;
    edtParams: TEdit;
    edtStartPath: TEdit;
    edtToolTip: TEdit;
    cbFlatButtons: TCheckBox;
    btnHelp: TButton;
    lblLabel: TLabel;
    btnOK: TButton;
    miAddSubMenu: TMenuItem;
    miAddSubBar: TMenuItem;
    OpenDialog: TOpenDialog;
    lblParameters: TLabel;
    pmChangeButton: TPopupMenu;
    sbIconExample: TSpeedButton;
    pnlToolBarFileName: TPanel;
    tbScrollBox: TScrollBox;
    lblBarSize: TLabel;
    lblStartPath: TLabel;
    lblToolTip: TLabel;
    procedure btnAppendMoreClick(Sender: TObject);
    procedure btnCloneButtonClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure btnAppendButtonClick(Sender: TObject);
    procedure btnOpenBarFileClick(Sender: TObject);
    procedure cbCommandSelect(Sender: TObject);
    procedure cbFlatButtonsChange(Sender: TObject);
    procedure cbIsSeparatorChange(Sender: TObject);
    procedure edtToolTipChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure ktbBarClick(Sender: TObject);
    function ktbBarLoadButtonGlyph(sIconFileName: String; iIconSize: Integer;
      clBackColor: TColor): TBitmap;
    procedure ktbBarToolButtonClick(Sender: TObject; NumberOfButton : Integer);
    procedure btnDeleteButtonClick(Sender: TObject);
    procedure btnOpenFileClick(Sender: TObject);
    procedure ktbBarToolButtonDragDrop(Sender, Source: TObject; X, Y: Integer;
      NumberOfButton: Integer);
    procedure ktbBarToolButtonDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean; NumberOfButton: Integer);
    procedure ktbBarToolButtonEndDrag(Sender, Target: TObject; X, Y: Integer;
      NumberOfButton: Integer);
    procedure ktbBarToolButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; NumberOfButton: Integer);
    procedure ktbBarToolButtonMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer; NumberOfButton: Integer);
    procedure ktbBarToolButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; NumberOfButton: Integer);
    procedure miAddSubBarClick(Sender: TObject);
    procedure miAddSubMenuClick(Sender: TObject);
    procedure sbIconExampleClick(Sender: TObject);
    procedure tbScrollBoxClick(Sender: TObject);
    procedure trbBarSizeChange(Sender: TObject);
    procedure trbIconSizeChange(Sender: TObject);

  private
    FBarFileName: UTF8String;
    LastToolButton : Integer;
    DefaultEditBarsColor : TColor; // Because real color depends on compilation switches
    ToolButtonMouseX, ToolButtonMouseY, ToolDragButtonNumber: integer; // For dragging
    procedure FillActionLists;
    procedure WakeSleepControls();
    procedure ClearControls;
    procedure LoadButton(NumberOfButton: Integer);
    procedure CopyButton(SourceButton, DestinationButton: Integer);
    procedure Save;
    function  GetSelectedButton: Integer;
    procedure InsertButton(InsertAt: Integer);
    function AddSpecialButton(const sCommand: AnsiString; out aFileName: UTF8String): Boolean;

  public
    constructor Create(TheOwner: TComponent; const aBarFileName: UTF8String); reintroduce;
  end;

  function ShowConfigToolbar(const aBarFileName: UTF8String; iButtonIndex : Integer = -1): Boolean;

implementation

{$R *.lfm}

uses
  LCLProc, HelpIntfs, uClassesEx, uOSForms, uPixMapManager,
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
  DefaultEditBarsColor:= edtParams.Color;
  trbBarSize.Position := gToolBarButtonSize div 2;
  trbIconSize.Position:= gToolBarIconSize div 2;
  cbFlatButtons.Checked:= gToolBarFlat;
  // Flat buttons in this dialog don't have any sense. They decrease button
  // readability without giving any actual advantages.
//  sbIconExample.Flat:= gToolBarFlat;
//  ktbBar.Flat:= gToolBarFlat;
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
    end;

  ToolDragButtonNumber := -1;
  WakeSleepControls;
  ktbBar.GlyphSize := trbIconSize.Position*2;
  ktbBar.SetButtonSize(trbBarSize.Position*2,trbBarSize.Position*2);
  Update;
  Height:= sbIconExample.Top + sbIconExample.Height + 18;
end;

procedure TfrmConfigToolBar.cbFlatButtonsChange(Sender: TObject);
begin

end;

procedure TfrmConfigToolBar.cbIsSeparatorChange(Sender: TObject);
begin
  if cbIsSeparator.Checked then
    edtToolTip.Text:= '-'
  else if edtToolTip.Text= '-' then
    edtToolTip.Text:= EmptyStr;
end;

procedure TfrmConfigToolBar.edtToolTipChange(Sender: TObject);
begin
  cbIsSeparator.Checked:=(edtToolTip.Text='-');
  WakeSleepControls;
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
  WakeSleepControls;
end;

(*Add new button on tool bar*)
procedure TfrmConfigToolBar.btnAppendButtonClick(Sender: TObject);
var
  SelectedIndex: Integer = 0;
begin
  SelectedIndex := GetSelectedButton;
  if SelectedIndex = -1 then
    begin
      InsertButton(ktbBar.ButtonCount);
      WakeSleepControls;
    end
  else
    InsertButton(SelectedIndex);
end;

procedure TfrmConfigToolBar.btnAppendMoreClick(Sender: TObject);
var
  Point: TPoint;
begin
  with btnAppendMore do
  Point:= Classes.Point(Left, Top + Height);
  Point:= ClientToScreen(Point);
  pmChangeButton.PopUp(Point.X, Point.Y);
end;

(*Clone selected button on tool bar*)
procedure TfrmConfigToolBar.btnCloneButtonClick(Sender: TObject);
var
SelectedIndex: Integer = 0;
begin
  SelectedIndex := GetSelectedButton;
  if SelectedIndex > -1 then
    begin
    Save;
    CopyButton(SelectedIndex,SelectedIndex);
    LastToolButton := LastToolButton + 1;
    ktbBar.Buttons[LastToolButton].Down := True;
  end;
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

  gToolBarFlat:= cbFlatButtons.Checked;
  gToolBarButtonSize:= trbBarSize.Position*2;
  gToolBarIconSize:= trbIconSize.Position*2;
  { TODO : Maybe we should get rid of gToolBarSmallIcons, it's useless now. }
  gToolBarSmallIcons:= (gToolBarButtonSize<>gToolBarIconSize);

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

procedure TfrmConfigToolBar.ktbBarClick(Sender: TObject);
begin
   tbScrollBoxClick(Sender);
end;

function TfrmConfigToolBar.ktbBarLoadButtonGlyph(sIconFileName: String;
  iIconSize: Integer; clBackColor: TColor): TBitmap;
begin
  Result := PixMapManager.LoadBitmapEnhanced(sIconFileName, iIconSize, clBackColor);
end;

(*Select button on panel*)
procedure TfrmConfigToolBar.ktbBarToolButtonClick(Sender: TObject; NumberOfButton : Integer);
begin
  LoadButton(NumberOfButton);
  ktbBar.Buttons[NumberOfButton].Down:=True;
  LastToolButton := NumberOfButton;
  WakeSleepControls;
end;

(*Disables button controls if LastToolButton = -1.
Otherwise enables button controls.
For separators also disables most of button controls. *)
procedure TfrmConfigToolBar.WakeSleepControls();
var MakeEnabled: Boolean = True;
  // Value copied from Controls.pp troperty "Color"
  EditBarsColor: TColor = {$ifdef UseCLDefault}clDefault{$else}clWindow{$endif};
  AddButtonName: String = '&Insert new button';
begin
  EditBarsColor:=DefaultEditBarsColor;
  If (LastToolButton = -1) or (edtToolTip.Text='-') then
    begin
      MakeEnabled := False ;
      EditBarsColor := clInactiveCaptionText;
      AddButtonName := '&Add button to end'
    end;
  lblCommand.Enabled := MakeEnabled;
  lblParameters.Enabled := MakeEnabled;
  lblStartPath.Enabled := MakeEnabled;
  lblIconFile.Enabled := MakeEnabled;
  btnOpenFile.Enabled := MakeEnabled;
  cbCommand.Enabled := MakeEnabled;
  cbCommand.Color := EditBarsColor;
  edtParams.Enabled := MakeEnabled;
  edtParams.Color := EditBarsColor;
  edtStartPath.Enabled := MakeEnabled;
  edtStartPath.Color := EditBarsColor;
  kedtIconFileName.Enabled := MakeEnabled;
  kedtIconFileName.Color := EditBarsColor;
  sbIconExample.Enabled := MakeEnabled;
if edtToolTip.Text= '-' then
    begin
      MakeEnabled := True;
      EditBarsColor := {$ifdef UseCLDefault}clDefault{$else}clWindow{$endif};
      AddButtonName := '&Insert new button'
    end;
  lblToolTip.Enabled := MakeEnabled;
  edtToolTip.Enabled := MakeEnabled;
  edtToolTip.Color := EditBarsColor;
  cbIsSeparator.Enabled := MakeEnabled;
  btnCloneButton.Enabled := MakeEnabled;
  btnDeleteButton.Enabled := MakeEnabled;
  btnAppendButton.Caption:= AddButtonName;

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
  cbCommand.Text := ktbBar.GetButtonX(NumberOfButton,CmdX);
  kedtIconFileName.Text := ktbBar.GetButtonX(NumberOfButton,ButtonX);
  edtToolTip.Text := ktbBar.GetButtonX(NumberOfButton,MenuX);
  sbIconExample.Glyph := ktbBar.Buttons[NumberOfButton].Glyph;
  edtParams.Text:= ktbBar.GetButtonX(NumberOfButton,ParamX);
  edtStartPath.Text:= ktbBar.GetButtonX(NumberOfButton,PathX);
end;

procedure TfrmConfigToolBar.CopyButton(SourceButton, DestinationButton: Integer);
begin
  ktbBar.InsertButtonX(DestinationButton,
                        '',
                        ktbBar.GetButtonX(SourceButton,CmdX),
                        ktbBar.GetButtonX(SourceButton,ParamX),
                        ktbBar.GetButtonX(SourceButton,PathX),
                        ktbBar.GetButtonX(SourceButton,MenuX),
                        ktbBar.GetButtonX(SourceButton,MiskX),
                        ktbBar.GetButtonX(SourceButton,ButtonX));
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
  WakeSleepControls;
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

      Bitmap := PixMapManager.LoadBitmapEnhanced(kedtIconFileName.Text, 32, Color);
      sbIconExample.Glyph.Assign(Bitmap);
      FreeThenNil(Bitmap);

      // Refresh icon on the toolbar.
      ktbBar.SetButtonX(LastToolButton, ButtonX, kedtIconFileName.Text);
    end;
end;

procedure TfrmConfigToolBar.ktbBarToolButtonDragDrop(Sender, Source: TObject;
  X, Y: Integer; NumberOfButton: Integer);
begin
  ktbBar.MoveButton((Source as TSpeedButton).Tag, (Sender as TSpeedButton).Tag);
  tbScrollBoxClick(Sender);
  ktbBarToolButtonClick(Sender, NumberOfButton)
end;

procedure TfrmConfigToolBar.ktbBarToolButtonDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean; NumberOfButton: Integer);
begin
  // Some type checks to be here
  if ((Sender as TSpeedButton).Tag) <> ((Source as TSpeedButton).Tag) then
  Accept:=True;
end;

procedure TfrmConfigToolBar.ktbBarToolButtonEndDrag(Sender, Target: TObject; X,
  Y: Integer; NumberOfButton: Integer);
begin

end;

procedure TfrmConfigToolBar.ktbBarToolButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  NumberOfButton: Integer);
begin
  Save;
  ToolButtonMouseX:=X;
  ToolButtonMouseY:=Y;
end;

procedure TfrmConfigToolBar.ktbBarToolButtonMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; NumberOfButton: Integer);
begin
  if (ssLeft in Shift) and (ToolDragButtonNumber = -1) then
    if (abs(ToolButtonMouseX-X)>10) or (abs(ToolButtonMouseY-Y)>10) then
    begin
      ToolDragButtonNumber:=NumberOfButton;
      ktbBar.Buttons[NumberOfButton].BeginDrag(false,5);
    end;
end;

procedure TfrmConfigToolBar.ktbBarToolButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  NumberOfButton: Integer);
begin
  ToolDragButtonNumber := -1;
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

procedure TfrmConfigToolBar.sbIconExampleClick(Sender: TObject);
var
  sFileName: String;
  Bitmap: TBitmap;
begin
  sFileName := GetCmdDirFromEnvVar(kedtIconFileName.Text);
  if ShowOpenIconDialog(Self, sFileName) then
    begin
      kedtIconFileName.Text := sFileName;

      Bitmap := PixMapManager.LoadBitmapEnhanced(kedtIconFileName.Text, 32, Color);
      sbIconExample.Glyph := Bitmap;
      FreeThenNil(Bitmap);

      // Refresh icon on the toolbar.
      ktbBar.SetButtonX(LastToolButton, ButtonX, kedtIconFileName.Text);
    end;
end;

procedure TfrmConfigToolBar.tbScrollBoxClick(Sender: TObject);
begin
  ClearControls;
  LastToolButton := GetSelectedButton;
  if LastToolButton > -1 then
    begin
      ktbBar.Buttons[LastToolButton].Down:=False;
      LastToolButton := -1;
    end;
  WakeSleepControls;
end;

procedure TfrmConfigToolBar.trbBarSizeChange(Sender: TObject);
begin
  lblBarSizeValue.Caption:=IntToStr(trbBarSize.Position*2);
  trbIconSize.Position:= trbBarSize.Position - (trbBarSize.Position div 5);
  ktbBar.SetButtonSize(trbBarSize.Position*2,trbBarSize.Position*2);
end;

procedure TfrmConfigToolBar.trbIconSizeChange(Sender: TObject);
begin
  lblIconSizeValue.Caption := IntToStr(trbIconSize.Position*2);
  ktbBar.GlyphSize := trbIconSize.Position*2;
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
              IniBarFile.WriteInteger('ButtonBar', 'ButtonCount', 1);
              IniBarFile.WriteString('ButtonBar', 'cmd1', cOpenBar);
              IniBarFile.WriteString('ButtonBar', 'param1', SetCmdDirAsEnvVar(FBarFileName));
              IniBarFile.WriteString('ButtonBar', 'menu1', Actions.GetCommandCaption(cOpenBar));
              IniBarFile.WriteString('ButtonBar', 'button1', 'go-up');
            end;
        finally
          FreeThenNil(IniBarFile);
        end;
      Result:= ShowConfigToolbar(aFileName);
    end;
  WakeSleepControls;
end;

end.

