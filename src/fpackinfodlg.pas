{
   Double Commander
   -------------------------------------------------------------------------
   Packed file information window

   Copyright (C) 2008-2009  Koblov Alexander (Alexx2000@mail.ru)

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

unit fPackInfoDlg;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, LResources, Forms, StdCtrls, ExtCtrls, Controls,
  uFile, uArchiveFileSource, uFileSourceExecuteOperation;

type

  { TfrmPackInfoDlg }

  TfrmPackInfoDlg = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    btnClose: TButton;
    btnUnpackAllAndExec: TButton;
    btnUnpackAndExec: TButton;
    lblAttributes: TLabel;
    lblCompressionRatio: TLabel;
    lblDate: TLabel;
    lblMethod: TLabel;
    lblOriginalSize: TLabel;
    lblPackedFile: TLabel;
    lblPackedSize: TLabel;
    lblPacker: TLabel;
    lblTime: TLabel;
    nbProperties: TNotebook;
    lblPackedAttr: TLabel;
    lblPackedCompression: TLabel;
    lblPackedDate: TLabel;
    edtPackedFile: TEdit;
    lblPackedMethod: TLabel;
    lblPackedOrgSize: TLabel;
    lblPackedPackedSize: TLabel;
    lblPackedPacker: TLabel;
    lblPackedTime: TLabel;
    pgGeneral: TPage;
  private
    { private declarations }
  public
    constructor Create(TheOwner: TComponent; aFileSource: IArchiveFileSource; aFile: TFile); reintroduce;
  end; 

function ShowPackInfoDlg(aFileSource: IArchiveFileSource; aFile: TFile): TFileSourceExecuteOperationResult;

implementation

uses
  LCLType, uTypes, uFileProperty;

function ShowPackInfoDlg(aFileSource: IArchiveFileSource; aFile: TFile): TFileSourceExecuteOperationResult;
begin
  Result:= fseorSuccess;
  with TfrmPackInfoDlg.Create(Application, aFileSource, aFile) do
  begin
    case ShowModal of
    mrCancel:
      Result:= fseorSuccess;
    mrOK:
      Result:= fseorYourSelf;
    mrAll:
      Result:= fseorYourSelf;
    end;
    Free;
  end;
end;

{ TfrmPackInfoDlg }

constructor TfrmPackInfoDlg.Create(TheOwner: TComponent;
                                   aFileSource: IArchiveFileSource; aFile: TFile);
var
  sArcType: String;
begin
  inherited Create(TheOwner);

  edtPackedFile.Text:= aFile.FullPath;
  sArcType:= ExtractFileExt(aFileSource.ArchiveFileName);
  Delete(sArcType, 1, 1);
  lblPackedPacker.Caption:= sArcType;

  if not aFile.IsDirectory then
  begin
    lblPackedOrgSize.Caption:=  IntToStr(aFile.Size);
    lblPackedPackedSize.Caption:= IntToStr(aFile.CompressedSize);
    if aFile.Size > 0 then
      lblPackedCompression.Caption:= IntToStr(100 - (aFile.CompressedSize *100 div aFile.Size))+'%';
//      lblPackedMethod.Caption:= IntToStr(HeaderData.Method);
  end;

  // DateTime and Attributes
  lblPackedDate.Caption:= DateToStr(aFile.ModificationTime);
  lblPackedTime.Caption:= TimeToStr(aFile.ModificationTime);
  lblPackedAttr.Caption:= aFile.Properties[fpAttributes].AsString;
end;

initialization
  {$I fpackinfodlg.lrs}

end.

