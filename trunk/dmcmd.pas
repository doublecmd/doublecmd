unit dmCmd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Dialogs; 

type
  TdmCommander = class(TDataModule)
    imgList: TImageList;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  dmCommander: TdmCommander;

implementation

{ TdmCommander }

initialization
  {$I dmCmd.lrs}

end.

