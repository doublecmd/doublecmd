{
Seksi Commander
----------------------------
Licence  : GNU GPL v 2.0
Author   : radek.cervinka@centrum.cz

contributors:

This is a generic language form.
It's public a virtual abstract method LoadLng, which
must be overloaded in descendant.
In this method we lokalize our GUI (via uLng).

In future maybe will have contained mechanism
for storing windows position in ini file

}


unit fLngForm;

interface

uses
  LResources,
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmLng = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure LoadLng; virtual; abstract;
  end;

implementation


uses
  uGlobs;

procedure TfrmLng.FormCreate(Sender: TObject);
begin
  LoadLng;
//  Color := clBtnFace;
//  Font.Assign(gMainFont);
//  writeln('Debug: Localised '+ClassName);
//  SetFocus;
//  PixelsPerInch:=0;
//  Scaled:=False;
end;

initialization
 {$I fLngForm.lrs}
end.
