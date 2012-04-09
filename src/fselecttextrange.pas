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
    procedure edtSelectTextExit(Sender: TObject);
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

procedure TfrmSelectTextRange.edtSelectTextExit(Sender: TObject);
begin
  FSelStart:= edtSelectText.SelStart + 1;
  FSelFinish:= edtSelectText.SelStart + edtSelectText.SelLength;
end;

end.

