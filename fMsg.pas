unit fMsg;

interface

uses
  LResources,
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

const
  cButtonWith=140;
  cButtonSpace=15;

type
  TfrmMsg = class(TForm)
    lblMsg: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure frmMsgShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Escape :Integer;
    iSelected:Integer;
    procedure ButtonClick(Sender:TObject);
  end;


implementation

procedure TfrmMsg.FormCreate(Sender: TObject);
begin
  iSelected:=-1;
end;

procedure TfrmMsg.ButtonClick(Sender:TObject);
begin
  iSelected:=(Sender as TButton).Tag;
  Close;
end;

procedure TfrmMsg.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key=#27) and (Escape>=0) then
  begin
    iSelected:=Escape;
    Close;
  end;
end;

procedure TfrmMsg.frmMsgShow(Sender: TObject);
var
  x:Integer;
  iWidth:Integer;
begin
  for x:=0 to ComponentCount-1 do
  begin
    if Components[x] is TButton then
    begin
      with Components[x] as TButton do
      begin
        if Tag=0 then SetFocus;
        Continue;
      end;
    end;
  end;
end;

initialization
 {$I fMsg.lrs}
end.
