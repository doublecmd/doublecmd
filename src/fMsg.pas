unit fMsg;

interface

uses
  SysUtils, Classes, Controls, Forms, StdCtrls, uOSForms;

var
  cButtonWidth: Integer = 90;
  cButtonSpace: Integer = 15;

type

  { TfrmMsg }

  TfrmMsg = class(TModalForm)
    lblMsg: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure frmMsgShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Escape :Integer;
    iSelected:Integer;
    procedure ButtonClick(Sender:TObject);
    procedure MouseUpEvent(Sender: TObject; Button: TMouseButton;
                             Shift: TShiftState; X, Y: Integer);
  end;


implementation

{$R *.lfm}

{$IF DEFINED(LCLGTK2)}
uses
  LCLType,
  LCLVersion;
{$ENDIF}

procedure TfrmMsg.FormCreate(Sender: TObject);
begin
  iSelected:=-1;
end;

procedure TfrmMsg.ButtonClick(Sender:TObject);
begin
  iSelected:=(Sender as TButton).Tag;
  Close;
end;

procedure TfrmMsg.MouseUpEvent(Sender: TObject; Button: TMouseButton;
                                 Shift: TShiftState; X, Y: Integer);
begin
{$IF DEFINED(LCLGTK) or DEFINED(LCLGTK2)}
  if (Button = mbLeft) and (Sender = FindLCLControl(Mouse.CursorPos)) then
  begin
    iSelected:=(Sender as TButton).Tag;
    Close;
  end;
{$ENDIF}
end;

procedure TfrmMsg.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key=#27) and (Escape>=0) then
  begin
    iSelected:=Escape;
    Close;
  end;
end;

procedure TfrmMsg.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  {$IF DEFINED(LCLGTK2) and (lcl_fullversion < 093100)}
  if Key = VK_RETURN then
    // Lazarus issue 0021483. ControlKeyUp not called after Enter pressed.
    Application.ControlKeyUp(ActiveControl, Key, Shift);
  {$ENDIF}
end;

procedure TfrmMsg.frmMsgShow(Sender: TObject);
var
  x:Integer;
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

end.
