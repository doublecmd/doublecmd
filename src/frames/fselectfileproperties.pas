unit fSelectFileProperties;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls;

type

  { TfrmSelectFileProperties }

  TfrmSelectFileProperties = class(TFrame)
    chkAttributes: TCheckBox;
    chkPermissions: TCheckBox;
    chkTime: TCheckBox;
    chkXAttributes: TCheckBox;
  end;

implementation

{$R *.lfm}

end.

