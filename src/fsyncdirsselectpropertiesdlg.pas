unit fSyncDirsSelectPropertiesDlg;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel;

type

  { TfrmSyncDirsSelectPropertiesDlg }

  TfrmSyncDirsSelectPropertiesDlg = class(TForm)
    ButtonPanel1: TButtonPanel;
    chkTime: TCheckBox;
    chkAttributes: TCheckBox;
    chkPermissions: TCheckBox;
    chkXAttributes: TCheckBox;
    lblLabel: TLabel;
  end;

implementation

{$R *.lfm}

end.

