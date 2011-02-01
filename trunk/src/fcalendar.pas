unit fCalendar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Calendar, LCLType;

type

  { TfrmCalendar }

  TfrmCalendar = class(TForm)
    Calendar: TCalendar;
    procedure CalendarDblClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { private declarations }
  public
    { public declarations }
  end; 

function ShowCalendarDialog(ADate: String; APosition: TPoint): String;

implementation

{$R *.lfm}

function ShowCalendarDialog(ADate: String; APosition: TPoint): String;
var
  dtDate: TDateTime;
begin
  Result:= ADate;
  with TfrmCalendar.Create(Application) do
    try
      Left:= APosition.X;
      Top:= APosition.Y;

      if TryStrToDate(ADate, dtDate) then
        Calendar.DateTime:= dtDate
      else
        Calendar.DateTime:= Date;

      if ShowModal = mrOK then
       Result:= Calendar.Date;
    finally
      Free;
    end;
end;

{ TfrmCalendar }

procedure TfrmCalendar.CalendarDblClick(Sender: TObject);
begin
  if Calendar.HitTest(Calendar.ScreenToClient(Mouse.CursorPos)) in [cpDate, cpNoWhere] then
    begin
      Caption:= Calendar.Date;
      Close;
      ModalResult:= mrOK;
    end;
end;

procedure TfrmCalendar.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
  VK_ESCAPE:
    begin
      Key:= 0;
      Close;
      ModalResult:= mrCancel;
    end;
  VK_RETURN, VK_SPACE:
    begin
      Key:= 0;
      Close;
      ModalResult:= mrOK;
    end;
  end;
end;

end.

