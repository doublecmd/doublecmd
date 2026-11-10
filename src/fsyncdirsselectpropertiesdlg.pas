unit fSyncDirsSelectPropertiesDlg;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel,
  fSelectFileProperties;

type

  { TfrmSyncDirsSelectPropertiesDlg }

  TfrmSyncDirsSelectPropertiesDlg = class(TForm)
    ButtonPanel1: TButtonPanel;
    frmProperties: TfrmSelectFileProperties;
    lblLabel: TLabel;
  end;

implementation

{$R *.lfm}

end.

