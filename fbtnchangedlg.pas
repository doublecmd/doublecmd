{
Double Commander
----------------------------
Configuration Toolbar

Licence  : GNU GPL v 2.0
Author   : Alexander Koblov (Alexx2000@mail.ru)

contributors:
}

unit fbtnchangedlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls, KASEdit;

type

  { TOneButtonChangeDlg }

  TOneButtonChangeDlg = class(TForm)
    Cancel: TButton;
    Command: TLabel;
    IconX: TLabel;
    Iconfile: TLabel;
    id_btn_assymbol: TCheckBox;
    id_btn_command: TComboBox;
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
    id_Globalhelp: TButton;
    Ok: TButton;
    OpenDialog: TOpenDialog;
    Parameters: TLabel;
    Startpath: TLabel;
    Tooltip: TLabel;
    procedure CancelClick(Sender: TObject);
    procedure OkClick(Sender: TObject);
    procedure id_btn_find1Click(Sender: TObject);
    procedure id_btn_iconfileClick(Sender: TObject);

  private
    { private declarations }
  public
    { public declarations }
  end; 

  procedure ShowOneBtnChangeDlg(NumberOfButton : Integer);

var
  OneButtonChangeDlg: TOneButtonChangeDlg;
  LastToolButton : Integer;
implementation
 uses fMain, uGlobsPaths;
{ TOneButtonChangeDlg }


procedure ShowOneBtnChangeDlg(NumberOfButton : Integer);
begin
  with TOneButtonChangeDlg.Create(Application) do
  try
    id_btn_command.Text := frmMain.MainToolBar.Commands[NumberOfButton];
    id_btn_iconfilename.Text := frmMain.MainToolBar.Icons[NumberOfButton];
    id_btn_ToolTip.Text := frmMain.MainToolBar.Buttons[NumberOfButton].Hint;
    LastToolButton := NumberOfButton;
    ShowModal;
  finally
    Free;
  end;
end;

procedure TOneButtonChangeDlg.CancelClick(Sender: TObject);
begin
  Close;
end;

procedure TOneButtonChangeDlg.OkClick(Sender: TObject);
begin
  frmMain.MainToolBar.Commands[LastToolButton] := id_btn_command.Text;
  frmMain.MainToolBar.Icons[LastToolButton] :=  id_btn_iconfilename.Text;
  frmMain.MainToolBar.Buttons[LastToolButton].Hint := id_btn_ToolTip.Text;
  frmMain.MainToolBar.SaveToFile(gpIniDir + 'default.bar');
  Close;
end;

procedure TOneButtonChangeDlg.id_btn_find1Click(Sender: TObject);
begin
  if OpenDialog.Execute then
     id_btn_command.Text := OpenDialog.FileName;
end;

procedure TOneButtonChangeDlg.id_btn_iconfileClick(Sender: TObject);
begin
  if OpenDialog.Execute then
     id_btn_iconfilename.Text := OpenDialog.FileName;
end;


initialization
  {$I fbtnchangedlg.lrs}

end.

