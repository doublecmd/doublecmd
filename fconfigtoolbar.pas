{
Double Commander
----------------------------
Configuration Toolbar

Licence  : GNU GPL v 2.0
Author   : Alexander Koblov (Alexx2000@mail.ru)

contributors:
}

unit fconfigtoolbar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons, KASToolBar, KASEdit;

type

  { TfrmButtonChangeDlg }

  TfrmButtonChangeDlg = class(TForm)
    lblButtonBar: TLabel;
    btnCancel: TButton;
    lblCommand: TLabel;
    gbGroupBox: TGroupBox;
    lblIconX: TLabel;
    lblIconfile: TLabel;
    btnAddButton: TButton;
    ktbBar: TKASToolBar;
    btnOpenBarFile: TButton;
    kedtBarSize: TKASEdit;
    cbCommand: TComboBox;
    btnDeleteButton: TButton;
    btnOpenFile: TButton;
    btnAddSubBar: TButton;
    lbIcons: TListBox;
    btnOpenIconFile: TButton;
    kedtIconFileName: TKASEdit;
    lblIconIndex: TLabel;
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
    tbScrollBox: TScrollBox;
    lblSize: TLabel;
    lblStartpath: TLabel;
    lblTooltip: TLabel;
    procedure cbFlatIconsChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnAddButtonClick(Sender: TObject);
    procedure ktbBarToolButtonClick(Sender: TObject; NumberOfButton : Integer);
    procedure Save;
    procedure btnDeleteButtonClick(Sender: TObject);
    procedure btnOpenFileClick(Sender: TObject);
    procedure btnOpenIconFileClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

  procedure ShowConfigToolbar;

var
  frmButtonChangeDlg: TfrmButtonChangeDlg;
  LastToolButton, NewToolButton : Integer;

implementation
uses fMain, uGlobsPaths, uGlobs;

procedure ShowConfigToolbar;
begin
    with TfrmButtonChangeDlg.Create(Application) do
  try
    LastToolButton := -1;
    NewToolButton := -1;
    ktbBar.InitBounds;
    ShowModal;
  finally
    Free;
  end;
end;

{ TfrmButtonChangeDlg }

procedure TfrmButtonChangeDlg.FormShow(Sender: TObject);
begin
  cbFlatIcons.Checked := gToolBarFlat;
  ktbBar.FlatButtons := gToolBarFlat;
  ktbBar.ChangePath := gpExePath;
  ktbBar.EnvVar := '%commander_path%';
  ktbBar.LoadFromFile(gpIniDir + 'default.bar');
end;

procedure TfrmButtonChangeDlg.cbFlatIconsChange(Sender: TObject);
begin
  ktbBar.FlatButtons := cbFlatIcons.Checked;
end;

procedure TfrmButtonChangeDlg.btnOKClick(Sender: TObject);
begin
  Save;
  gToolBarFlat := cbFlatIcons.Checked;
  ktbBar.SaveToFile(gpIniDir + 'default.bar');
  frmMain.MainToolBar.DeleteAllToolButtons;
  frmMain.MainToolBar.FlatButtons := gToolBarFlat;
  //frmMain.MainToolBar.CreateWnd;
  frmMain.MainToolBar.LoadFromFile(gpIniDir + 'default.bar');
  Close;
end;

(*Add new button on tool bar*)
procedure TfrmButtonChangeDlg.btnAddButtonClick(Sender: TObject);
begin
  Save;
  NewToolButton := ktbBar.AddButton('', '', '', '');
  //ShowMessage(IntToStr(NewToolButton));
end;

(*Select button on panel*)
procedure TfrmButtonChangeDlg.ktbBarToolButtonClick(Sender: TObject; NumberOfButton : Integer);
begin
 Save;
 cbCommand.Text := ktbBar.Commands[NumberOfButton];
 kedtIconFileName.Text := ktbBar.Icons[NumberOfButton];
 kedtToolTip.Text := ktbBar.Buttons[NumberOfButton].Hint;
 LastToolButton := NumberOfButton;
end;

(*Save current button*)
procedure TfrmButtonChangeDlg.Save;
begin
   if (LastToolButton >= 0) and (ktbBar.ButtonCount > 0) then
      begin
       ktbBar.Commands[LastToolButton] := cbCommand.Text;
       ktbBar.Icons[LastToolButton] :=  kedtIconFileName.Text;
       ktbBar.Buttons[LastToolButton].Hint := kedtToolTip.Text;
      end
   else   (*If only Append clicked*)
      if NewToolButton >= 0 then
         begin
            //ShowMessage(IntToStr(NewToolButton));
            ktbBar.Commands[NewToolButton] := cbCommand.Text;
            ktbBar.Icons[NewToolButton] :=  kedtIconFileName.Text;
            ktbBar.Buttons[NewToolButton].Hint := kedtToolTip.Text;
         end;
end;

(*Remove current button*)
procedure TfrmButtonChangeDlg.btnDeleteButtonClick(Sender: TObject);
begin
   if (LastToolButton >= 0) and (ktbBar.ButtonCount > 0) then
      begin
      ktbBar.RemoveButton(LastToolButton);
      cbCommand.Text := '';
      kedtIconFileName.Text := '';
      kedtToolTip.Text := '';
      LastToolButton := -1;
      NewToolButton := -1;
      end;
end;

procedure TfrmButtonChangeDlg.btnOpenFileClick(Sender: TObject);
begin
  if OpenDialog.Execute then
     cbCommand.Text := OpenDialog.FileName;
end;

procedure TfrmButtonChangeDlg.btnOpenIconFileClick(Sender: TObject);
var
  sDir: string;
begin
  if OpenDialog.Execute then
     kedtIconFileName.Text := OpenDialog.FileName;
end;

initialization
  {$I fconfigtoolbar.lrs}

end.

