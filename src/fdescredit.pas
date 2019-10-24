{
   Double commander
   -------------------------------------------------------------------------
   Dialog for editing file comments.

   Copyright (C) 2008-2019 Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit fDescrEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Buttons, ActnList, uDescr,
  uFormCommands, uFileView;

type

  { TfrmDescrEdit }

  TfrmDescrEdit = class(TForm, IFormCommands)
    actSaveDescription: TAction;
    ActionList: TActionList;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    cbEncoding: TStaticText;
    lblFileName: TLabel;
    lblEncoding: TLabel;
    lblEditCommentFor: TLabel;
    memDescr: TMemo;
    procedure actExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure memDescrKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
  private
    FDescr: TDescription;
    FCommands: TFormCommands;
    procedure DisplayEncoding;
    property Commands: TFormCommands read FCommands implements IFormCommands;
  public
    constructor Create(TheOwner: TComponent); override;
  published
    procedure cm_SaveDescription(const {%H-}Params: array of string);
  end; 

function ShowDescrEditDlg(const sFileName: String; FileView: TFileView): Boolean;

implementation

{$R *.lfm}

uses
  TypInfo, LCLType, LConvEncoding, DCStrUtils, uHotkeyManager, uLng, uGlobs,
  uFileSystemFileSource, uConvEncoding;

const
  HotkeysCategory = 'Edit Comment Dialog';

function ShowDescrEditDlg(const sFileName: String; FileView: TFileView): Boolean;
const
  nbsp = #194#160;
var
  FileSystem: Boolean;
begin
  Result:= False;
  FileSystem:= FileView.FileSource.IsClass(TFileSystemFileSource);
  with TfrmDescrEdit.Create(Application) do
  try
    if not FileSystem then
      FDescr:= TDescription.Create(False)
    else begin
      FDescr:= (FileView.FileSource as TFileSystemFileSource).Description;
      FDescr.Reset;
    end;
    lblFileName.Caption:= sFileName;
    // Read description
    memDescr.Lines.Text:= StringReplace(FDescr.ReadDescription(sFileName), nbsp, LineEnding, [rfReplaceAll]);
    DisplayEncoding;
    if ShowModal = mrOK then
    begin
      if Length(memDescr.Lines.Text) = 0 then
        FDescr.DeleteDescription(sFileName)
      else begin
        FDescr.WriteDescription(sFileName, StringReplace(memDescr.Lines.Text, LineEnding, nbsp, [rfReplaceAll]));
      end;
      FDescr.SaveDescription;
      FileView.Reload(True);
      Result:= True;
    end;
    if not FileSystem then FDescr.Free;
  finally
    Free;
  end;
end;

{ TfrmDescrEdit }

procedure TfrmDescrEdit.FormCreate(Sender: TObject);
var
  HMForm: THMForm;
  Hotkey: THotkey;
begin
  // Initialize property storage
  InitPropStorage(Self);

  HMForm := HotMan.Register(Self, HotkeysCategory);
  Hotkey := HMForm.Hotkeys.FindByCommand('cm_SaveDescription');

  if Assigned(Hotkey) then
    btnOK.Caption := btnOK.Caption + ' (' + ShortcutsToText(Hotkey.Shortcuts) + ')';
end;

procedure TfrmDescrEdit.FormDestroy(Sender: TObject);
begin
  HotMan.UnRegister(Self);
end;

procedure TfrmDescrEdit.memDescrKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then ModalResult:= btnCancel.ModalResult;
end;

procedure TfrmDescrEdit.DisplayEncoding;
begin
  cbEncoding.Caption:= Copy(GetEnumName(System.TypeInfo(TMacroEncoding), Ord(FDescr.Encoding)), 3 , MaxInt);
end;

constructor TfrmDescrEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FCommands := TFormCommands.Create(Self, actionList);
end;

procedure TfrmDescrEdit.cm_SaveDescription(const Params: array of string);
begin
  ModalResult:= btnOK.ModalResult;
end;

procedure TfrmDescrEdit.actExecute(Sender: TObject);
var
  cmd: string;
begin
  cmd := (Sender as TAction).Name;
  cmd := 'cm_' + Copy(cmd, 4, Length(cmd) - 3);
  Commands.ExecuteCommand(cmd, []);
end;

initialization
  TFormCommands.RegisterCommandsForm(TfrmDescrEdit, HotkeysCategory, @rsHotkeyCategoryEditCommentDialog);

end.

