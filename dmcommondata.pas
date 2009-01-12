unit dmCommonData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Dialogs; 

type

  { TdmComData }

  TdmComData = class(TDataModule)
    ImageList: TImageList;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  dmComData: TdmComData;

implementation

initialization
  {$I dmcommondata.lrs}

end.

