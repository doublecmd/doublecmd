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

  { TButtonChangeDlg }

  TButtonChangeDlg = class(TForm)
    Buttonbar: TLabel;
    Cancel: TButton;
    Command: TLabel;
    GroupBox1: TGroupBox;
    IconX: TLabel;
    Iconfile: TLabel;
    id_btn_add: TButton;
    id_btn_assymbol: TCheckBox;
    id_btn_bar: TKASToolBar;
    id_btn_barfilesearch: TButton;
    id_btn_barsize: TKASEdit;
    id_btn_command: TComboBox;
    id_btn_delete: TButton;
    id_btn_find1: TButton;
    id_btn_findbar: TButton;
    id_btn_icon: TListBox;
    id_btn_iconfile: TButton;
    id_btn_iconfilename: TKASEdit;
    id_btn_iconindex: TLabel;
    id_btn_maximized: TCheckBox;
    id_btn_param: TKASEdit;
    id_btn_startpath: TKASEdit;
    id_btn_ToolTip: TKASEdit;
    id_FlatIcons: TCheckBox;
    id_Globalhelp: TButton;
    id_SmallIcons: TCheckBox;
    Label1: TLabel;
    Ok: TButton;
    OpenDialog: TOpenDialog;
    Parameters: TLabel;
    tbScrollBox: TScrollBox;
    Size: TLabel;
    Startpath: TLabel;
    Tooltip: TLabel;
    procedure FormShow(Sender: TObject);
    procedure OkClick(Sender: TObject);
    procedure id_btn_addClick(Sender: TObject);
    procedure id_btn_barToolButtonClick(NumberOfButton : Integer);
    procedure Save;
    procedure id_btn_deleteClick(Sender: TObject);
    procedure id_btn_find1Click(Sender: TObject);
    procedure id_btn_iconfileClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

  procedure ShowConfigToolbar;

var
  ButtonChangeDlg: TButtonChangeDlg;
  LastToolButton, NewToolButton : Integer;

implementation
uses fMain, uGlobsPaths;

procedure ShowConfigToolbar;
begin
    with TButtonChangeDlg.Create(Application) do
  try
    LastToolButton := -1;
    NewToolButton := -1;
    id_btn_bar.CreateWnd;
    ShowModal;
  finally
    Free;
  end;
end;

{ TButtonChangeDlg }

procedure TButtonChangeDlg.FormShow(Sender: TObject);
begin
  id_btn_bar.LoadFromFile(gpIniDir + 'default.bar');
end;

procedure TButtonChangeDlg.OkClick(Sender: TObject);
begin
  Save;
  id_btn_bar.SaveToFile(gpIniDir + 'default.bar');
  frmMain.MainToolBar.DeleteAllToolButtons;
  //frmMain.MainToolBar.CreateWnd;
  frmMain.MainToolBar.LoadFromFile(gpIniDir + 'default.bar');
  Close;
end;

(*Add new button on tool bar*)
procedure TButtonChangeDlg.id_btn_addClick(Sender: TObject);
begin
  Save;
  NewToolButton := id_btn_bar.AddButton('', '', '');
  //ShowMessage(IntToStr(NewToolButton));
end;

(*Select button on panel*)
procedure TButtonChangeDlg.id_btn_barToolButtonClick(NumberOfButton : Integer);
begin
 Save;
 id_btn_command.Text := id_btn_bar.Commands[NumberOfButton];
 id_btn_iconfilename.Text := id_btn_bar.Icons[NumberOfButton];
 id_btn_ToolTip.Text := id_btn_bar.Buttons[NumberOfButton].Hint;
 LastToolButton := NumberOfButton;
end;

(*Save current button*)
procedure TButtonChangeDlg.Save;
begin
   if (LastToolButton >= 0) and (id_btn_bar.ButtonCount > 0) then
      begin
       id_btn_bar.Commands[LastToolButton] := id_btn_command.Text;
       id_btn_bar.Icons[LastToolButton] :=  id_btn_iconfilename.Text;
       id_btn_bar.Buttons[LastToolButton].Hint := id_btn_ToolTip.Text;
      end
   else   (*If only Append clicked*)
      if NewToolButton >= 0 then
         begin
            //ShowMessage(IntToStr(NewToolButton));
            id_btn_bar.Commands[NewToolButton] := id_btn_command.Text;
            id_btn_bar.Icons[NewToolButton] :=  id_btn_iconfilename.Text;
            id_btn_bar.Buttons[NewToolButton].Hint := id_btn_ToolTip.Text;
         end;
end;

(*Remove current button*)
procedure TButtonChangeDlg.id_btn_deleteClick(Sender: TObject);
begin
   if (LastToolButton >= 0) and (id_btn_bar.ButtonCount > 0) then
      begin
      id_btn_bar.RemoveButton(LastToolButton);
      id_btn_command.Text := '';
      id_btn_iconfilename.Text := '';
      id_btn_ToolTip.Text := '';
      LastToolButton := -1;
      NewToolButton := -1;
      end;
end;

procedure TButtonChangeDlg.id_btn_find1Click(Sender: TObject);
begin
  if OpenDialog.Execute then
     id_btn_command.Text := OpenDialog.FileName;
end;

procedure TButtonChangeDlg.id_btn_iconfileClick(Sender: TObject);
var
  sDir: string;
begin
  if OpenDialog.Execute then
     id_btn_iconfilename.Text := OpenDialog.FileName;
end;

initialization
  {$I fconfigtoolbar.lrs}

end.

