unit fSyncDirsPerformDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ButtonPanel;

type

  { TfrmSyncDirsPerformDlg }

  TfrmSyncDirsPerformDlg = class(TForm)
    Bevel1: TBevel;
    ButtonPanel1: TButtonPanel;
    chkDeleteLeft: TCheckBox;
    chkDeleteRight: TCheckBox;
    chkConfirmOverwrites: TCheckBox;
    chkLeftToRight: TCheckBox;
    chkRightToLeft: TCheckBox;
    edRightPath: TEdit;
    edLeftPath: TEdit;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmSyncDirsPerformDlg: TfrmSyncDirsPerformDlg;

implementation

{$R *.lfm}

end.

