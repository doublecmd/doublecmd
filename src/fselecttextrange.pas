unit fSelectTextRange;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, Buttons;

type

  { TfrmSelectTextRange }

  TfrmSelectTextRange = class(TForm)
    btpPanel: TButtonPanel;
    edtSelectText: TEdit;
    lblSelectText: TLabel;
    procedure edtSelectTextKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edtSelectTextMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FSelStart,
    FSelFinish: LongInt;
  public
    { public declarations }
  end; 

function ShowSelectTextRangeDlg(const ACaption, AText: UTF8String;
                                out iSelStart, iSelFinish: LongInt): Boolean;

implementation

{$R *.lfm}

function ShowSelectTextRangeDlg(const ACaption, AText: UTF8String;
                                out iSelStart, iSelFinish: LongInt): Boolean;
begin
  with TfrmSelectTextRange.Create(Application) do
  try
     Caption:= ACaption;
     edtSelectText.Text:= AText;

     Result:= (ShowModal = mrOK);

     if Result then
     begin
       iSelStart:= FSelStart;
       iSelFinish:= FSelFinish;
       Result:= (FSelFinish >= FSelStart);
     end;
  finally
    Free;
  end;
end;

{ TfrmSelectTextRange }

procedure TfrmSelectTextRange.edtSelectTextKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  FSelStart:= edtSelectText.SelStart + 1;
  FSelFinish:= edtSelectText.SelStart + edtSelectText.SelLength;
end;

procedure TfrmSelectTextRange.edtSelectTextMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FSelStart:= edtSelectText.SelStart + 1;
  FSelFinish:= edtSelectText.SelStart + edtSelectText.SelLength;
end;

end.

