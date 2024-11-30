{
   Double Commander
   -------------------------------------------------------------------------
   Packed file information window

   Copyright (C) 2008-2020 Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit fPackInfoDlg;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Forms, StdCtrls, ExtCtrls, Controls,
  uFile, KASCDEdit, uArchiveFileSource, uFileSourceExecuteOperation;

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
    lblPackedAttr: TKASCDEdit;
    lblPackedCompression: TKASCDEdit;
    lblPackedDate: TKASCDEdit;
    edtPackedFile: TEdit;
    lblPackedMethod: TKASCDEdit;
    lblPackedOrgSize: TKASCDEdit;
    lblPackedPackedSize: TKASCDEdit;
    lblPackedPacker: TKASCDEdit;
    lblPackedTime: TKASCDEdit;
    pnlInfoProperties: TPanel;
    pnlInfoFile: TPanel;
    pnlInfo: TPanel;
    pnlButtons: TPanel;
    procedure btnCloseKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { private declarations }
  public
    constructor Create(TheOwner: TComponent; aFileSource: IArchiveFileSource; aFile: TFile); reintroduce;
  end; 

function ShowPackInfoDlg(aFileSource: IArchiveFileSource; aFile: TFile): TFileSourceExecuteOperationResult;

implementation

{$R *.lfm}

uses
  {$IF DEFINED(LCLGTK2)}
  LCLType,
  LCLVersion,
  {$ENDIF}
  uDCUtils, uFileSourceOperationTypes;

function ShowPackInfoDlg(aFileSource: IArchiveFileSource; aFile: TFile): TFileSourceExecuteOperationResult;
begin
  Result:= fseorSuccess;
  with TfrmPackInfoDlg.Create(Application, aFileSource, aFile) do
  begin
    case ShowModal of
    mrCancel:
      Result:= fseorCancelled;
    mrOK:
      Result:= fseorYourSelf;
    mrAll:
      Result:= fseorWithAll;
    end;
    Free;
  end;
end;

{ TfrmPackInfoDlg }

procedure TfrmPackInfoDlg.btnCloseKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  {$IF DEFINED(LCLGTK2) and (lcl_fullversion < 093100)}
  if Key = VK_RETURN then
    // Lazarus issue 0021483. ControlKeyUp not called after Enter pressed.
    Application.ControlKeyUp(btnClose, Key, Shift);
  {$ENDIF}
end;

constructor TfrmPackInfoDlg.Create(TheOwner: TComponent;
                                   aFileSource: IArchiveFileSource; aFile: TFile);
var
  i: Integer;
  foundDividingControl: Boolean = False;
  upperInfoControls: array[0..4] of TControl;
begin
  inherited Create(TheOwner);

  btnUnpackAndExec.Enabled:= (fsoCopyOut in aFileSource.GetOperationsTypes);
  btnUnpackAllAndExec.Enabled:= ([fsoList, fsoCopyOut] * aFileSource.GetOperationsTypes = [fsoList, fsoCopyOut]);
  edtPackedFile.Text:= aFile.FullPath;
  lblPackedPacker.Caption:= aFileSource.Packer;

  lblPackedOrgSize.Visible := not aFile.IsDirectory;
  lblPackedPackedSize.Visible := not aFile.IsDirectory;
  lblPackedCompression.Visible := False;
  lblPackedMethod.Visible := False;

  if not aFile.IsDirectory then
  begin
    lblPackedOrgSize.Caption := IntToStrTS(aFile.Size);
    lblPackedPackedSize.Caption := IntToStrTS(aFile.CompressedSize);
    lblPackedOrgSize.Visible := aFile.SizeProperty.IsValid;
    lblPackedPackedSize.Visible := aFile.CompressedSizeProperty.IsValid;
    if (aFile.Size > 0) and aFile.CompressedSizeProperty.IsValid then
    begin
      lblPackedCompression.Caption := IntToStr(100 - (aFile.CompressedSize * 100 div aFile.Size)) + '%';
      lblPackedCompression.Visible := True;
    end;
  end;

  // DateTime and Attributes
  if not aFile.ModificationTimeProperty.IsValid then
  begin
    lblPackedDate.Visible:= False;
    lblPackedTime.Visible:= False;
  end
  else
  begin
    lblPackedDate.Caption:= DateToStr(aFile.ModificationTime);
    lblPackedTime.Caption:= TimeToStr(aFile.ModificationTime);
  end;
  lblPackedAttr.Caption:= aFile.AttributesProperty.AsString;

  // Hide labels for not visible values.
  lblOriginalSize.Visible     := lblPackedOrgSize.Visible;
  lblPackedSize.Visible       := lblPackedPackedSize.Visible;
  lblCompressionRatio.Visible := lblPackedCompression.Visible;
  lblMethod.Visible           := lblPackedMethod.Visible;
  lblDate.Visible             := lblPackedDate.Visible;
  lblTime.Visible             := lblPackedTime.Visible;

  // Controls from the dividing line to top.
  upperInfoControls[0] := lblMethod;
  upperInfoControls[1] := lblCompressionRatio;
  upperInfoControls[2] := lblPackedSize;
  upperInfoControls[3] := lblOriginalSize;
  upperInfoControls[4] := lblPacker;

  // Make space for the dividing line.
  for i := Low(upperInfoControls) to High(upperInfoControls) do
  begin
    if foundDividingControl then
      upperInfoControls[i].BorderSpacing.Bottom := 0
    else if upperInfoControls[i].Visible then
    begin
      foundDividingControl := True;
      upperInfoControls[i].BorderSpacing.Bottom := 12;
    end;
  end;
end;

end.

