unit fMsg;

interface

uses
  SysUtils, Classes, Controls, Forms, StdCtrls, ExtCtrls, Menus, uOSForms;

type

  { TfrmMsg }

  TfrmMsg = class(TModalForm)
    lblMsg: TLabel;
    pnlButtons: TPanel;
    mnuOther: TPopupMenu;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
    Escape: Integer;
    iSelected: Integer;
    procedure ButtonClick(Sender:TObject);
    procedure ButtonOtherClick(Sender:TObject);
    procedure MouseUpEvent(Sender: TObject; Button: TMouseButton;
                             Shift: TShiftState; X, Y: Integer);
  end;


implementation

{$R *.lfm}

uses
  LCLType;

procedure TfrmMsg.FormCreate(Sender: TObject);
begin
  iSelected:= -1;
end;

procedure TfrmMsg.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Index: Integer;
  NextOrder: Integer;
begin
  if (Key in [VK_UP, VK_DOWN]) and (ActiveControl is TButton) then
  begin
    NextOrder:= pnlButtons.ChildSizing.ControlsPerLine;
    if Key = VK_UP then NextOrder:= -NextOrder;
    NextOrder:= ActiveControl.TabOrder + NextOrder;
    for Index:= 0 to pnlButtons.ControlCount - 1 do
    begin
      if pnlButtons.Controls[Index] is TButton then
      begin
        if NextOrder = TButton(pnlButtons.Controls[Index]).TabOrder then
        begin
          ActiveControl:= TButton(pnlButtons.Controls[Index]);
          Key:= 0;
          Break;
        end;
      end;
    end;
  end;
end;

procedure TfrmMsg.ButtonClick(Sender: TObject);
begin
  iSelected:= (Sender as TComponent).Tag;
  Close;
end;

procedure TfrmMsg.MouseUpEvent(Sender: TObject; Button: TMouseButton;
                                 Shift: TShiftState; X, Y: Integer);
begin
{$IF DEFINED(LCLGTK) or DEFINED(LCLGTK2)}
  if (Button = mbLeft) and (Sender = FindLCLControl(Mouse.CursorPos)) then
  begin
    iSelected:= (Sender as TButton).Tag;
    Close;
  end;
{$ENDIF}
end;

procedure TfrmMsg.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #27) and (Escape >= 0) then
  begin
    iSelected:= Escape;
    Close;
  end;
end;

procedure TfrmMsg.ButtonOtherClick(Sender: TObject);
var
  Point: TPoint;
  Button: TButton absolute Sender;
begin
  Point.X:= Button.Left;
  Point.Y:= Button.Top + Button.Height;
  Point:= pnlButtons.ClientToScreen(Point);
  mnuOther.PopUp(Point.X, Point.Y);
end;

end.
