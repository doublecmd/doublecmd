unit dmDialogs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Dialogs; 

type
  TdmDlg = class(TDataModule)
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  dmDlg: TdmDlg;

implementation

initialization
  {$I dmDialogs.lrs}

end.

