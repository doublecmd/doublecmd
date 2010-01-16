{
   Double commander
   -------------------------------------------------------------------------
   Dialog for editing file comments.

   Copyright (C) 2008-2010  Koblov Alexander (Alexx2000@mail.ru)

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
    procedure DisplayEncoding;
  public
    { public declarations }
  end; 

function ShowDescrEditDlg(sFileName: String): Boolean;

implementation

uses
  LConvEncoding;

function ShowDescrEditDlg(sFileName: String): Boolean;
const
  nbsp = #194#160;
begin
  Result:= False;
  with TfrmDescrEdit.Create(Application) do
  begin
    FDescr:= TDescription.Create(False);
    lblFileName.Caption:= sFileName;
    // read description
    memDescr.Lines.Text:= StringReplace(FDescr.ReadDescription(sFileName), nbsp, LineEnding, [rfReplaceAll]);
    DisplayEncoding;
    if ShowModal = mrOK then
      begin
        FDescr.WriteDescription(sFileName, StringReplace(memDescr.Lines.Text, LineEnding, nbsp, [rfReplaceAll]));
        FDescr.SaveDescription;
        Result:= True;
      end;
    FDescr.Free;
    Free;
  end;
end;

{ TfrmDescrEdit }

procedure TfrmDescrEdit.FormCreate(Sender: TObject);
begin
  // fill encoding combobox
  cbEncoding.Clear;
  GetSupportedEncodings(cbEncoding.Items);
end;

procedure TfrmDescrEdit.DisplayEncoding;
var
  I: Integer;
begin
  for I:= 0 to cbEncoding.Items.Count - 1 do
    if SameText(NormalizeEncoding(cbEncoding.Items.Strings[I]), FDescr.Encoding) then
      begin
        cbEncoding.ItemIndex:= I;
        Break;
      end;
end;

procedure TfrmDescrEdit.cbEncodingChange(Sender: TObject);
begin
  FDescr.Encoding:= cbEncoding.Text;
  memDescr.Lines.Text:= FDescr.ReadDescription(lblFileName.Caption);
end;

initialization
  {$I fdescredit.lrs}

end.

