unit fHackForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs; 

type

  { TfrmHackForm }

  TfrmHackForm = class(TForm)
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmHackForm: TfrmHackForm;

implementation

uses
  fMain;

{ TfrmHackForm }

procedure TfrmHackForm.FormShow(Sender: TObject);
begin
  Hide;
  frmMain.Show;
end;

initialization
  {$I fhackform.lrs}

end.

