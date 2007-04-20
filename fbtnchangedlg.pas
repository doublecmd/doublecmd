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
    btnCancel: TButton;
    lblCommand: TLabel;
    IconX: TLabel;
    lblIconfile: TLabel;
    lblRunMinimized: TCheckBox;
    cbCommand: TComboBox;
    btnOpenFile: TButton;
    btnAddSubBar: TButton;
    lblIcons: TListBox;
    btnOpenIconFile: TButton;
    kedtIconFileName: TKASEdit;
    lblIconIndex: TLabel;
    lblRunMaximized: TCheckBox;
    kedtParams: TKASEdit;
    kedtStartpath: TKASEdit;
    kedtToolTip: TKASEdit;
    btnHelp: TButton;
    btnOK: TButton;
    OpenDialog: TOpenDialog;
    lblParameters: TLabel;
    lblStartpath: TLabel;
    lblTooltip: TLabel;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnOpenFileClick(Sender: TObject);
    procedure btnOpenIconFileClick(Sender: TObject);

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
    cbCommand.Text := frmMain.MainToolBar.Commands[NumberOfButton];
    kedtIconFileName.Text := frmMain.MainToolBar.Icons[NumberOfButton];
    kedtToolTip.Text := frmMain.MainToolBar.Buttons[NumberOfButton].Hint;
    LastToolButton := NumberOfButton;
    ShowModal;
  finally
    Free;
  end;
end;

procedure TOneButtonChangeDlg.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TOneButtonChangeDlg.btnOKClick(Sender: TObject);
begin
  frmMain.MainToolBar.Commands[LastToolButton] := cbCommand.Text;
  frmMain.MainToolBar.Icons[LastToolButton] :=  kedtIconFileName.Text;
  frmMain.MainToolBar.Buttons[LastToolButton].Hint := kedtToolTip.Text;
  frmMain.MainToolBar.SaveToFile(gpIniDir + 'default.bar');
  Close;
end;

procedure TOneButtonChangeDlg.btnOpenFileClick(Sender: TObject);
begin
  if OpenDialog.Execute then
     cbCommand.Text := OpenDialog.FileName;
end;

procedure TOneButtonChangeDlg.btnOpenIconFileClick(Sender: TObject);
begin
  if OpenDialog.Execute then
     kedtIconFileName.Text := OpenDialog.FileName;
end;


initialization
  {$I fbtnchangedlg.lrs}

end.

