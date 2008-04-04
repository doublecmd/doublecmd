{
    Double Commander
    -------------------------------------------------------------------------
    Configuration Toolbar

    Copyright (C) 2006-2007  Koblov Alexander (Alexx2000@mail.ru)

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
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons, KASToolBar, KASEdit,
  ExtCtrls,KASBarFiles;

type

  { TfrmConfigToolBar }

  TfrmConfigToolBar = class(TForm)
    lblButtonBar: TLabel;
    lblCommand: TLabel;
    btnCancel: TButton;
    gbGroupBox: TGroupBox;
    lblIcon: TLabel;
    lblIconFile: TLabel;
    btnAddButton: TButton;
    ktbBar: TKASToolBar;
    btnOpenBarFile: TButton;
    kedtBarSize: TKASEdit;
    cbCommand: TComboBox;
    btnDeleteButton: TButton;
    btnOpenFile: TButton;
    btnAddSubBar: TButton;
    btnOpenIconFile: TButton;
    kedtIconFileName: TKASEdit;
    kedtParams: TKASEdit;
    kedtStartPath: TKASEdit;
    kedtToolTip: TKASEdit;
    cbFlatIcons: TCheckBox;
    btnHelp: TButton;
    cbSmallIcons: TCheckBox;
    lblLabel: TLabel;
    btnOK: TButton;
    OpenDialog: TOpenDialog;
    lblParameters: TLabel;
    sbIconExample: TSpeedButton;
    stToolBarFileName: TStaticText;
    tbScrollBox: TScrollBox;
    lblSize: TLabel;
    lblStartPath: TLabel;
    lblToolTip: TLabel;
    procedure btnAddSubBarClick(Sender: TObject);
    procedure btnOpenBarFileClick(Sender: TObject);
    procedure cbCommandSelect(Sender: TObject);
    procedure cbFlatIconsChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnAddButtonClick(Sender: TObject);
    function ktbBarLoadButtonGlyph(sIconFileName: String; iIconSize: Integer;
      clBackColor: TColor): TBitmap;
    procedure ktbBarToolButtonClick(Sender: TObject; NumberOfButton : Integer);
    procedure Save;
    procedure btnDeleteButtonClick(Sender: TObject);
    procedure btnOpenFileClick(Sender: TObject);
    procedure btnOpenIconFileClick(Sender: TObject);
  private
    { private declarations }
    procedure FillActionLists;
  public
    { public declarations }
  end; 

  procedure ShowConfigToolbar(iButtonIndex : Integer = -1);

var
  LastToolButton, NewToolButton : Integer;

implementation
uses ActnList, LCLProc, fMain, uOSForms, uPixMapManager, uGlobsPaths, uGlobs;

procedure ShowConfigToolbar(iButtonIndex : Integer = -1);
begin
  with TfrmConfigToolBar.Create(Application) do
  try
    LastToolButton := -1;
    NewToolButton := -1;
    ktbBar.InitBounds;
    ktbBar.Tag := iButtonIndex; // Selected button index
    ShowModal;
  finally
    Free;
  end;
end;

{ TfrmConfigToolBar }

procedure TfrmConfigToolBar.FillActionLists;
var
  vNum: integer;
  vActions: TAction;
begin
  for vNum := 0 to frmMain.actionLst.ActionCount -1 do
  begin
    vActions := frmMain.actionLst.Actions[vNum] as TAction;
    cbCommand.Items.AddObject(vActions.Name, vActions);
  end;
end;

procedure TfrmConfigToolBar.FormShow(Sender: TObject);
begin
  FillActionLists;
  kedtBarSize.Text := IntToStr(gToolBarIconSize);
  cbFlatIcons.Checked := gToolBarFlat;
  sbIconExample.Flat:= gToolBarFlat;
  ktbBar.FlatButtons := gToolBarFlat;
  ktbBar.ChangePath := gpExePath;
  ktbBar.EnvVar := '%commander_path%';
  ktbBar.LoadFromFile(gpIniDir + 'default.bar');
  stToolBarFileName.Caption := gpIniDir + 'default.bar';
  if ktbBar.Tag >= 0 then
    begin
      ktbBar.Buttons[ktbBar.Tag].Click;
      ktbBar.Buttons[ktbBar.Tag].Down := True;
    end
  else
    begin
      if ktbBar.ButtonCount>0 then
        begin
          ktbBar.Buttons[ktbBar.ButtonCount-1].Down:=true;
          ktbBarToolButtonClick(Sender,ktbBar.ButtonCount-1);
        end;
    end;
end;

procedure TfrmConfigToolBar.cbFlatIconsChange(Sender: TObject);
begin
  ktbBar.FlatButtons := cbFlatIcons.Checked;
  sbIconExample.Flat := cbFlatIcons.Checked;
end;

procedure TfrmConfigToolBar.btnOpenBarFileClick(Sender: TObject);
begin
  OpenDialog.FileName := stToolBarFileName.Caption;
  OpenDialog.Filter:= '*.bar|*.bar';
  if OpenDialog.Execute then
    begin
      ktbBar.LoadFromFile(OpenDialog.FileName);
      stToolBarFileName.Caption := OpenDialog.FileName;
    end;
end;

procedure TfrmConfigToolBar.btnAddSubBarClick(Sender: TObject);
begin

end;

procedure TfrmConfigToolBar.cbCommandSelect(Sender: TObject);
var
  vActions: TAction;
begin
   vActions := cbCommand.Items.Objects[cbCommand.ItemIndex] as TAction;
   kedtToolTip.Text := StringReplace(vActions.Caption, '&', '', [rfReplaceAll]);
end;

procedure TfrmConfigToolBar.btnOKClick(Sender: TObject);
begin
  Save;
  gToolBarIconSize := StrToIntDef(kedtBarSize.Text, 16);
  gToolBarFlat := cbFlatIcons.Checked;
  ktbBar.SaveToFile(gpIniDir + 'default.bar');
  frmMain.MainToolBar.ButtonGlyphSize := gToolBarIconSize;
  frmMain.MainToolBar.DeleteAllToolButtons;
  frmMain.MainToolBar.FlatButtons := gToolBarFlat;
  frmMain.MainToolBar.LoadFromFile(gpIniDir + 'default.bar');
  Close;
end;

(*Add new button on tool bar*)
procedure TfrmConfigToolBar.btnAddButtonClick(Sender: TObject);
begin
  Save;
 // WriteLn();
  ktbBar.AddX('','','','','');
  NewToolButton := ktbBar.AddButton('', '', '', '');

  ktbBar.Buttons[ktbBar.ButtonCount-1].Down:=true;
  ktbBarToolButtonClick(Sender,ktbBar.ButtonCount-1);
  //ShowMessage(IntToStr(NewToolButton));
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
// cbCommand.Text := ktbBar.Commands[NumberOfButton];
cbCommand.Text := ktbBar.GetButtonX(NumberOfButton,CmdX);
// kedtIconFileName.Text := ktbBar.Icons[NumberOfButton];
 kedtIconFileName.Text := ktbBar.GetButtonX(NumberOfButton,ButtonX);

// kedtToolTip.Text := ktbBar.Buttons[NumberOfButton].Hint;
 kedtToolTip.Text := ktbBar.GetButtonX(NumberOfButton,MenuX);

 sbIconExample.Glyph := ktbBar.Buttons[NumberOfButton].Glyph;
 kedtParams.Text:= ktbBar.GetButtonX(NumberOfButton,ParamX);
 kedtStartPath.Text:= ktbBar.GetButtonX(NumberOfButton,PathX);
 
 LastToolButton := NumberOfButton;


end;

(*Save current button*)
procedure TfrmConfigToolBar.Save;
begin
   if (LastToolButton >= 0) and (ktbBar.ButtonCount > 0) then
      begin
       //---------------------
       ktbBar.SetButtonX(LastToolButton,CmdX,cbCommand.Text);
       ktbBar.SetButtonX(LastToolButton,ParamX,kedtParams.Text);
       ktbBar.SetButtonX(LastToolButton,PathX,kedtStartPath.Text);
       ktbBar.SetButtonX(LastToolButton,ButtonX,kedtIconFileName.Text);
       ktbBar.SetButtonX(LastToolButton,MenuX,kedtToolTip.Text);
       //---------------------
//       ktbBar.Commands[LastToolButton] := cbCommand.Text;
//       ktbBar.Icons[LastToolButton] :=  kedtIconFileName.Text;
//       ktbBar.Buttons[LastToolButton].Hint := kedtToolTip.Text;
      end
   else   (*If only Append clicked*)
      if NewToolButton >= 0 then
         begin
            //ShowMessage(IntToStr(NewToolButton));
//            ktbBar.Commands[NewToolButton] := cbCommand.Text;
//            ktbBar.Icons[NewToolButton] :=  kedtIconFileName.Text;
//            ktbBar.Buttons[NewToolButton].Hint := kedtToolTip.Text;
           //---------------------
            ktbBar.SetButtonX(NewToolButton,CmdX,cbCommand.Text);
            ktbBar.SetButtonX(NewToolButton,ButtonX,kedtIconFileName.Text);
            ktbBar.SetButtonX(NewToolButton,MenuX,kedtToolTip.Text);
            ktbBar.SetButtonX(NewToolButton,ParamX,kedtParams.Text);
            ktbBar.SetButtonX(NewToolButton,PathX,kedtStartPath.Text);
           //---------------------

         end;
end;

(*Remove current button*)
procedure TfrmConfigToolBar.btnDeleteButtonClick(Sender: TObject);
begin
   if (LastToolButton >= 0) and (ktbBar.ButtonCount > 0) then
      begin
        ktbBar.RemoveButton(LastToolButton);
        cbCommand.Text := '';
        kedtIconFileName.Text := '';
        kedtToolTip.Text := '';
        LastToolButton := -1;
        NewToolButton := -1;
        if ktbBar.ButtonCount>0 then
        begin
          ktbBar.Buttons[ktbBar.ButtonCount-1].Down:=true;
          ktbBarToolButtonClick(Sender,ktbBar.ButtonCount-1);
        end;
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
begin
  sFileName := kedtIconFileName.Text;
  if ShowOpenIconDialog(Self, sFileName) then
    begin
      kedtIconFileName.Text := sFileName;
      sbIconExample.Glyph := LoadBitmapFromFile(kedtIconFileName.Text, 32, Color);
    end;
end;

initialization
  {$I fconfigtoolbar.lrs}

end.

