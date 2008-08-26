{
   Double commander
   -------------------------------------------------------------------------
   Dialog for editing file comments.

   Copyright (C) 2008  Koblov Alexander (Alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit fDescrEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, uDescr;

type

  { TfrmDescrEdit }

  TfrmDescrEdit = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    cbEncoding: TComboBox;
    lblFileName: TLabel;
    lblEncoding: TLabel;
    lblEditCommentFor: TLabel;
    memDescr: TMemo;
    procedure cbEncodingChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FDescr: TDescription;
    FEncoding,
    FOriginalText: String;
  public
    { public declarations }
  end; 

function ShowDescrEditDlg(sFileName: String): Boolean;

implementation
uses
  LConvEncoding;

function ShowDescrEditDlg(sFileName: String): Boolean;
begin
  with TfrmDescrEdit.Create(Application) do
  begin
    FDescr:= TDescription.Create(False);
    lblFileName.Caption:= sFileName;
    // save original description
    FOriginalText:= FDescr.ReadDescription(sFileName);
    // try to guess encoding
    FEncoding:= GuessEncoding(FOriginalText);
    cbEncoding.Text:= FEncoding;
    memDescr.Lines.Text:= ConvertEncoding(FOriginalText, FEncoding, EncodingUTF8);
    if ShowModal = mrOK then
      begin
        FOriginalText:= ConvertEncoding(memDescr.Lines.Text, EncodingUTF8, FEncoding);
        FDescr.WriteDescription(sFileName, StringReplace(FOriginalText, LineEnding, #32, [rfReplaceAll]));
        FDescr.SaveDescription;
      end;
    FDescr.Free;
    Free;
  end;
end;

{ TfrmDescrEdit }

procedure TfrmDescrEdit.FormCreate(Sender: TObject);
var
  I: Integer;
begin
// fill encoding combobox
  cbEncoding.Clear;
  GetSupportedEncodings(cbEncoding.Items);
end;

procedure TfrmDescrEdit.cbEncodingChange(Sender: TObject);
begin
  FEncoding:= cbEncoding.Text;
  memDescr.Lines.Text:= ConvertEncoding(FOriginalText, FEncoding, EncodingUTF8);
end;

initialization
  {$I fdescredit.lrs}

end.

