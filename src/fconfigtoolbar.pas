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
    btnClearHotKey: TButton;
    btnAppendButton: TButton;
    btnCloneButton: TButton;
    cbIsSeparator: TCheckBox;
    lblHotKeys: TLabel;
    edtHotKeys: TEdit;
    lblIconSize: TLabel;
    lblIconSizeValue: TLabel;
    lblBarSizeValue: TLabel;
    pnlDialogButtons: TPanel;
    pnlToolbarButtons: TPanel;
    trbBarSize: TTrackBar;
    trbIconSize: TTrackBar;
    lblButtonBar: TLabel;
    lblCommand: TLabel;
    btnCancel: TButton;
    gbGroupBox: TGroupBox;
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
    procedure btnClearHotKeyClick(Sender: TObject);
    procedure btnCloneButtonClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure btnAppendButtonClick(Sender: TObject);
    procedure btnOpenBarFileClick(Sender: TObject);
    procedure cbCommandSelect(Sender: TObject);
    procedure cbIsSeparatorChange(Sender: TObject);
    procedure edtHotKeysKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edtHotKeysKeyPress(Sender: TObject; var Key: char);
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
    ToolButtonMouseX, ToolButtonMouseY, ToolDragButtonNumber: integer; // For dragging
    HintWindow: THintWindow;
    FHotKeyList: TStringList;
    procedure FillActionLists;
    procedure WakeSleepControls();
    procedure ClearControls;
    procedure LoadButton(NumberOfButton: Integer);
    procedure CopyButton(SourceButton, DestinationButton: Integer);
    procedure Save;
    function  GetSelectedButton: Integer;
    procedure InsertButton(InsertAt: Integer);
    function AddSpecialButton(const sCommand: AnsiString; out aFileName: UTF8String): Boolean;
    procedure LoadHotKeyList;
    function GetHotKey (IndexButton:integer): string;
    procedure SetButtonHotKey;
    procedure ShowHint(Control: TControl; HintText: String);

  public
    constructor Create(TheOwner: TComponent; const aBarFileName: UTF8String); reintroduce;
    destructor Destroy; override;
  end;

  function ShowConfigToolbar(const aBarFileName: UTF8String; iButtonIndex : Integer = -1): Boolean;

implementation

{$R *.lfm}

uses
  LCLProc, HelpIntfs, uClassesEx, uOSForms, uPixMapManager, uLng,
  uGlobsPaths, uGlobs, uDCUtils, uOSUtils, uHotkeyManager, uKeyboard;

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
  cHotKeyCommand = 'cm_Int_RunCommandFromBarFile';

{ TfrmConfigToolBar }

constructor TfrmConfigToolBar.Create(TheOwner: TComponent; const aBarFileName:UTF8String);
begin
  FBarFileName:= aBarFileName;
  LastToolButton := -1;
  FHotKeyList:= TStringList.Create;
  HintWindow:= THintWindow.Create(Self);
  HintWindow.AutoHide:= True;
  inherited Create(TheOwner);
end;

destructor TfrmConfigToolBar.Destroy;
begin
  FreeThenNil(FHotKeyList);
  FreeThenNil(HintWindow);
  inherited Destroy;
end;

procedure TfrmConfigToolBar.FillActionLists;
var
  I: integer;
  sItem: String;
begin
  for I:= 0 to Actions.CommandList.Count - 1 do
    begin
      sItem:= Actions.CommandList.Strings[I];
      if (NumCountChars('_', sItem) = 1) then
        cbCommand.Items.Add(sItem);
    end;
  cbCommand.Sorted:= True;
end;

procedure TfrmConfigToolBar.LoadHotKeyList;
var
  i,j: Integer;
  lstl:TStringList;
begin
  lstl:=TStringList.Create;
  try
    for i:=0 to HotMan.HotkeyList.Count - 1 do
    begin
      HotMan.GetControlsListBy(HotMan.HotkeyList[i],lstl);
      for j:=0 to lstl.Count-1 do
      begin
        if Assigned(lstl.Objects[j]) then
          if THotkeyInfoClass(lstl.Objects[j]).ACommand = cHotKeyCommand then
            FHotKeyList.AddObject(HotMan.HotkeyList[i], lstl.Objects[j]);
      end; // for j
    end; // for i
  finally
    FreeAndNil(lstl);
  end;
end;

procedure TfrmConfigToolBar.FormShow(Sender: TObject);
var
  IniBarFile: TIniFileEx;
begin
  LoadHotKeyList;
  FillActionLists;
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
  Height:= edtHotKeys.Top + edtHotKeys.Height + 3;
end;

function TfrmConfigToolBar.GetHotKey(IndexButton: Integer): String;
var
  i: integer;
  sHotKey: string;
begin
  Result := '';
  sHotKey := ktbBar.GetButtonX(IndexButton, MiskX);
  for i:=0 to FHotKeyList.Count-1 do
    begin
      if THotkeyInfoClass(FHotKeyList.Objects[i]).AParams = sHotKey then
         Result := FHotKeyList.Strings[i];
    end;
end;

procedure TfrmConfigToolBar.cbIsSeparatorChange(Sender: TObject);
begin
  if cbIsSeparator.Checked then
    edtToolTip.Text:= '-'
  else if edtToolTip.Text= '-' then
    edtToolTip.Text:= EmptyStr;
  if LastToolButton > -1 then
    if (ktbBar.Buttons[LastToolButton].Down = True) and (kedtIconFileName.Caption = '') then
        ktbBar.Buttons[LastToolButton].Glyph.Assign(ktbBarLoadButtonGlyph(edtToolTip.Text, ktbBar.GlyphSize, Color));
end;

procedure TfrmConfigToolBar.edtToolTipChange(Sender: TObject);
begin
  cbIsSeparator.Checked:= (edtToolTip.Text='-');
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
  begin
    Point:= Classes.Point(Left, Top + Height);
    Point:= ClientToScreen(Point);
  end;
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
  IniBarFile: TIniFileEx = nil;
begin
  Save;

  gToolBarFlat:= cbFlatButtons.Checked;
  gToolBarButtonSize:= trbBarSize.Position*2;
  gToolBarIconSize:= trbIconSize.Position*2;
  { TODO : Maybe we should get rid of gToolBarSmallIcons, it's useless now. }
  gToolBarSmallIcons:= (gToolBarButtonSize<>gToolBarIconSize);

  try
    IniBarFile:= TIniFileEx.Create(FBarFileName);
    IniBarFile.CacheUpdates:= True;
    ktbBar.SaveToIniFile(IniBarFile);
    IniBarFile.UpdateFile;
  finally
    FreeThenNil(IniBarFile);
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
  Result := PixMapManager.LoadBitmapEnhanced(sIconFileName, iIconSize, True, clBackColor);

  if (sIconFileName = '-') then  // Paint 'separator' icon
    begin
      Result := TBitmap.Create;
      Result.SetSize(iIconSize,iIconSize);
      Result.Canvas.Brush.Color:= clBtnFace;
      Result.Canvas.FillRect(Rect(0,0,iIconSize,iIconSize));
      Result.Canvas.Brush.Color:= clBtnText;
      Result.Canvas.RoundRect(Rect(Round(iIconSize * 0.4), 2, Round(iIconSize * 0.6), iIconSize - 2),iIconSize div 8,iIconSize div 4);
    end;
end;

(*Select button on panel*)
procedure TfrmConfigToolBar.ktbBarToolButtonClick(Sender: TObject; NumberOfButton : Integer);
begin
  LastToolButton := NumberOfButton;
  LoadButton(NumberOfButton);
  ktbBar.Buttons[NumberOfButton].Down:=True;
  WakeSleepControls;
end;

(* Disables button controls if no toolbar button is selected.
Also disables most of button controls for separators.
Otherwise enables button controls. *)
procedure TfrmConfigToolBar.WakeSleepControls();
var
  MakeEnabled: Boolean = True;
  AddButtonName: String = '&Insert new button';
begin
  if (LastToolButton = -1) or (edtToolTip.Text='-') then
    begin
      MakeEnabled := False ;
      AddButtonName := '&Add button to end'
    end;
  lblCommand.Enabled := MakeEnabled;
  lblParameters.Enabled := MakeEnabled;
  lblStartPath.Enabled := MakeEnabled;
  lblIconFile.Enabled := MakeEnabled;
  lblHotKeys.Enabled := MakeEnabled;
  btnOpenFile.Enabled := MakeEnabled;
  EnableControl(cbCommand, MakeEnabled);
  EnableControl(edtParams, MakeEnabled);
  EnableControl(edtStartPath, MakeEnabled);
  EnableControl(kedtIconFileName, MakeEnabled);
  EnableControl(edtHotKeys, MakeEnabled);
  sbIconExample.Enabled := MakeEnabled;
  if edtToolTip.Text= '-' then
    begin
      MakeEnabled := True;
      AddButtonName := '&Insert new button'
    end;
  lblToolTip.Enabled := MakeEnabled;
  EnableControl(edtToolTip, MakeEnabled);
  cbIsSeparator.Checked := edtToolTip.Text='-';
  btnCloneButton.Enabled := MakeEnabled;
  btnDeleteButton.Enabled := MakeEnabled;
  btnAppendButton.Caption:= AddButtonName;
  btnClearHotKey.Enabled:= Length(edtHotKeys.Text) <> 0;
end;

procedure TfrmConfigToolBar.ClearControls;
begin
  cbCommand.Text := '';
  kedtIconFileName.Text := '';
  edtToolTip.Text := '';
  sbIconExample.Glyph := nil;
  edtParams.Text:= '';
  edtStartPath.Text:= '';
  edtHotKeys.Text:='';
  btnClearHotKey.Enabled:= False;
end;

procedure TfrmConfigToolBar.LoadButton(NumberOfButton: Integer);
begin
  cbCommand.Text := ktbBar.GetButtonX(NumberOfButton,CmdX);
  kedtIconFileName.Text := ktbBar.GetButtonX(NumberOfButton,ButtonX);
  edtToolTip.Text := ktbBar.GetButtonX(NumberOfButton,MenuX);
  edtParams.Text:= ktbBar.GetButtonX(NumberOfButton,ParamX);
  edtStartPath.Text:= ktbBar.GetButtonX(NumberOfButton,PathX);
  sbIconExample.Glyph := ktbBar.Buttons[NumberOfButton].Glyph;
  edtHotKeys.Text :=  GetHotKey(NumberOfButton);
  btnClearHotKey.Enabled:= Length(edtHotKeys.Text) <> 0;
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
       ///// save hotkey /////
       SetButtonHotKey;
       FHotKeyList.Clear;
       LoadHotKeyList;
       //---------------------
       ktbBar.SetButtonX(LastToolButton,MenuX,edtToolTip.Text);
       ktbBar.SetButtonX(LastToolButton,CmdX,cbCommand.Text);
       ktbBar.SetButtonX(LastToolButton,ParamX,edtParams.Text);
       ktbBar.SetButtonX(LastToolButton,PathX,edtStartPath.Text);
       ktbBar.SetButtonX(LastToolButton,ButtonX,kedtIconFileName.Text);
       //---------------------
      end;
end;

procedure TfrmConfigToolBar.btnClearHotKeyClick(Sender: TObject);
begin
  edtHotKeys.Text:= EmptyStr;
  btnClearHotKey.Enabled:= False;
  ktbBar.SetButtonX(LastToolButton, MiskX, EmptyStr);
  HotMan.DeleteHotKey(GetHotKey(LastToolButton), 'FrmMain', 'FrmMain');
end;

procedure TfrmConfigToolBar.edtHotKeysKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  sCommand: String;
  sShortCut: String;
begin
  sShortCut := ShortCutToTextEx(ShortCutEx(Key,GetKeyShiftStateEx));
  edtHotKeys.Text := sShortCut;
  Key := 0;
  HintWindow.Hide;
  if Length(sShortCut) > 0 then
  begin
    // Find hotkey
    sCommand:= HotMan.FindFirstCommand(sShortCut, 'FrmMain', 'FrmMain');
    if Length(sCommand) > 0 then
    begin
      ShowHint(edtHotKeys, Format(rsOptHotkeysShortCutUsedText1, [sShortCut, sCommand]));
    end;
  end;
  btnClearHotKey.Enabled:= Length(edtHotKeys.Text) <> 0;
end;

procedure TfrmConfigToolBar.edtHotKeysKeyPress(Sender: TObject; var Key: char);
begin
  Key := #0;
  edtHotKeys.Text := '';
end;

procedure TfrmConfigToolBar.SetButtonHotKey;
var
  sShortCut, sOldCommand: String;
  st: TStringList;

  //< local function for add hot key,
  procedure AddHotKeyButton;
  begin
    HotMan.AddHotKeyEx(sShortCut,
                       cHotKeyCommand,
                       sShortCut,
                       'FrmMain', 'FrmMain');
    ktbBar.SetButtonX(LastToolButton, MiskX, edtHotKeys.Text);
  end;

begin
   if Length(edtHotKeys.Text) = 0 then
     ktbBar.SetButtonX(LastToolButton, MiskX, edtHotKeys.Text)
   else
    begin
      sShortCut := edtHotKeys.Text;
      sOldCommand := HotMan.FindFirstCommand(sShortCut, 'FrmMain', 'FrmMain');
      if Length(sOldCommand) = 0 then
        AddHotKeyButton
      else
        begin
          if (sOldCommand = cHotKeyCommand) or
             (MessageDlg(rsOptHotkeysShortCutUsed,
                         Format(rsOptHotkeysShortCutUsedText1,
                                [sShortCut, sOldCommand]) + LineEnding +
                         Format(rsOptHotkeysShortCutUsedText2,
                                [cHotKeyCommand]),
                         mtConfirmation, mbYesNo, 0) = mrYes) then
            begin
              if HotMan.DeleteHotKey(sShortCut, 'FrmMain', 'FrmMain') then
                begin
                  AddHotKeyButton;
                end;
            end;
        end // Shortcut already used
    end  // Clear shortcut
end;

procedure TfrmConfigToolBar.ShowHint(Control: TControl; HintText: String);
var
  Rect: TRect;
  APoint: TPoint;
begin
  // Calculate hint position
  Rect:= HintWindow.CalcHintRect(400, HintText, nil);
  APoint:= Control.ClientOrigin;
  with Rect do
  begin
    Right:= APoint.X + 8 + Right;
    Bottom:= APoint.Y + Bottom - Control.Height;
    Left:= APoint.X + 8;
    Top:= APoint.Y - Control.Height;
  end;
  // Show hint
  HintWindow.ActivateHint(Rect, HintText);
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

      Bitmap := PixMapManager.LoadBitmapEnhanced(kedtIconFileName.Text, 32, True, Color);
      sbIconExample.Glyph.Assign(Bitmap);
      FreeThenNil(Bitmap);

      // Refresh icon on the toolbar.
      ktbBar.SetButtonX(LastToolButton, ButtonX, kedtIconFileName.Text);
    end;
end;

(* Select button after it is dragged*)
procedure TfrmConfigToolBar.ktbBarToolButtonDragDrop(Sender, Source: TObject;
  X, Y: Integer; NumberOfButton: Integer);
begin
  ktbBarToolButtonClick(Sender, NumberOfButton)
end;

(* Move button if it is dragged*)
procedure TfrmConfigToolBar.ktbBarToolButtonDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean; NumberOfButton: Integer);
begin
  if not (Source is TSpeedButton) then exit;
  if (ToolDragButtonNumber <>(Sender as TSpeedButton).Tag) then
    begin
      ktbBar.MoveButton((Source as TSpeedButton).Tag, (Sender as TSpeedButton).Tag);
      ToolDragButtonNumber := (Sender as TSpeedButton).Tag;
      Accept:=True;
    end;
end;

(* Do not start drag in here, because oterwise button wouldn't be pushed down*)
procedure TfrmConfigToolBar.ktbBarToolButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  NumberOfButton: Integer);
begin
  Save;
  ToolButtonMouseX:=X;
  ToolButtonMouseY:=Y;
end;

(* Start dragging only if mbLeft if pressed and mouse moved.*)
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

(* End button drag*)
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

      Bitmap := PixMapManager.LoadBitmapEnhanced(kedtIconFileName.Text, 32, True, Color);
      sbIconExample.Glyph := Bitmap;
      FreeThenNil(Bitmap);

      // Refresh icon on the toolbar.
      ktbBar.SetButtonX(LastToolButton, ButtonX, kedtIconFileName.Text);
    end;
end;

// Deselect any selected button
procedure TfrmConfigToolBar.tbScrollBoxClick(Sender: TObject);
begin
  LastToolButton := GetSelectedButton;
  if LastToolButton > -1 then
    begin
      Save;
      ktbBar.Buttons[LastToolButton].Down:=False;
      LastToolButton := -1;
    end;
  ClearControls;
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

