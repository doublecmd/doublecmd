unit fSetFilePropertiesAdvLoad;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ButtonPanel, fSelectFileProperties;

type

  { TfrmSetFilePropertiesAdvLoad }

  TfrmSetFilePropertiesAdvLoad = class(TForm)
    ButtonPanel1: TButtonPanel;
    frmProperties: TfrmSelectFileProperties;
    gbLoadAttributes: TGroupBox;
    rgLoadFrom: TRadioGroup;
  end;

var
  frmSetFilePropertiesAdvLoad: TfrmSetFilePropertiesAdvLoad;

implementation

{$R *.lfm}

end.

