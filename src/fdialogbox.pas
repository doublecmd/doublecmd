{
    Double Commander
    -------------------------------------------------------------------------
    This unit contains realization of Dialog API functions.

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

unit fDialogBox;

{$mode objfpc}{$H+}
{$include calling.inc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Types, Buttons, ExtCtrls, EditBtn, Extension, ComCtrls, DividerBevel;

type

  { TDialogBox }

  TDialogBox = class(TForm)
    DialogButton: TButton;
    DialogBitBtn: TBitBtn;
    DialogFileNameEdit: TFileNameEdit;
    DialogComboBox: TComboBox;
    DialogListBox: TListBox;
    DialogCheckBox: TCheckBox;
    DialogGroupBox: TGroupBox;
    DialogLabel: TLabel;
    DialogPanel: TPanel;
    DialogEdit: TEdit;
    DialogMemo: TMemo;
    DialogImage: TImage;
    DialogTabSheet: TTabSheet;
    DialogScrollBox: TScrollBox;
    DialogRadioGroup: TRadioGroup;
    DialogPageControl: TPageControl;
    DialogProgressBar: TProgressBar;
    DialogDividerBevel: TDividerBevel;
    // Dialog events
    procedure DialogBoxShow(Sender: TObject);
    // Button events
    procedure ButtonClick(Sender: TObject);
    procedure ButtonEnter(Sender: TObject);
    procedure ButtonExit(Sender: TObject);
    procedure ButtonKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ButtonKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    // ComboBox events
    procedure ComboBoxClick(Sender: TObject);
    procedure ComboBoxDblClick(Sender: TObject);
    procedure ComboBoxChange(Sender: TObject);
    procedure ComboBoxEnter(Sender: TObject);
    procedure ComboBoxExit(Sender: TObject);
    procedure ComboBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ComboBoxKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    // Edit events
    procedure EditClick(Sender: TObject);
    procedure EditDblClick(Sender: TObject);
    procedure EditChange(Sender: TObject);
    procedure EditEnter(Sender: TObject);
    procedure EditExit(Sender: TObject);
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    // ListBox events
    procedure ListBoxClick(Sender: TObject);
    procedure ListBoxDblClick(Sender: TObject);
    procedure ListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure ListBoxEnter(Sender: TObject);
    procedure ListBoxExit(Sender: TObject);
    procedure ListBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ListBoxKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    // CheckBox events
    procedure CheckBoxChange(Sender: TObject);
  private
    FRect: TRect;
    FText: String;
    FLRSData: String;
    FResult: LongBool;
    FDlgProc: TDlgProc;
    FTranslator: TAbstractTranslator;
  protected
    procedure ShowDialogBox;
    procedure ProcessResource; override;
    function InitResourceComponent(Instance: TComponent; RootAncestor: TClass): Boolean;
  public
    constructor Create(const LRSData: String; DlgProc: TDlgProc); reintroduce;
    destructor Destroy; override;
  end; 

function InputBox(Caption, Prompt: PAnsiChar; MaskInput: LongBool; Value: PAnsiChar; ValueMaxLen: Integer): LongBool; dcpcall;
function MessageBox(Text, Caption: PAnsiChar; Flags: Longint): Integer; dcpcall;
function DialogBoxLFM(LFMData: Pointer; DataSize: LongWord; DlgProc: TDlgProc): LongBool; dcpcall;
function DialogBoxLRS(LRSData: Pointer; DataSize: LongWord; DlgProc: TDlgProc): LongBool; dcpcall;
function DialogBoxLFMFile(lfmFileName: PAnsiChar; DlgProc: TDlgProc): LongBool; dcpcall;
function SendDlgMsg(pDlg: PtrUInt; DlgItemName: PAnsiChar; Msg, wParam, lParam: PtrInt): PtrInt; dcpcall;

implementation

uses
  LCLStrConsts, LazFileUtils, DCClassesUtf8, DCOSUtils, uShowMsg, uDebug,
  uTranslator, uGlobs;

function InputBox(Caption, Prompt: PAnsiChar; MaskInput: LongBool; Value: PAnsiChar; ValueMaxLen: Integer): LongBool; dcpcall;
var
  sValue: String;
begin
  sValue:= StrPas(Value);
  Result:= ShowInputQuery(Caption, Prompt, MaskInput, sValue);
  if Result then StrLCopy(Value, PAnsiChar(sValue), ValueMaxLen);
end;

function MessageBox(Text, Caption: PAnsiChar; Flags: Longint): Integer; dcpcall;
begin
  Result:= ShowMessageBox(Text, Caption, Flags);
end;

function LFMToLRS(const LFMData: String): String;
var
  LFMStream: TStringStream = nil;
  LRSStream: TStringStream = nil;
begin
  try
    LRSStream:= TStringStream.Create('');
    LFMStream:= TStringStream.Create(LFMData);
    LRSObjectTextToBinary(LFMStream, LRSStream);
    Result:= LRSStream.DataString;
  finally
    FreeAndNil(LFMStream);
    FreeAndNil(LRSStream);
  end;
end;

function DialogBox(const LRSData: String; DlgProc: TDlgProc): LongBool;
var
  Dialog: TDialogBox;
begin
  Dialog:= TDialogBox.Create(LRSData, DlgProc);
  try
    with Dialog do
    begin
      TThread.Synchronize(nil, @ShowDialogBox);
      Result:= FResult;
    end;
  finally
    FreeAndNil(Dialog);
  end;
end;

function DialogBoxLFM(LFMData: Pointer; DataSize: LongWord; DlgProc: TDlgProc): LongBool; dcpcall;
var
  DataString: String;
begin
  if Assigned(LFMData) and (DataSize > 0) then
  begin
    SetString(DataString, LFMData, DataSize);
    Result := DialogBox(LFMToLRS(DataString), DlgProc);
  end
  else
    Result := False;
end;

function DialogBoxLRS(LRSData: Pointer; DataSize: LongWord; DlgProc: TDlgProc): LongBool; dcpcall;
var
  DataString: String;
begin
  if Assigned(LRSData) and (DataSize > 0) then
  begin
    SetString(DataString, LRSData, DataSize);
    Result := DialogBox(DataString, DlgProc);
  end
  else
    Result := False;
end;

function DialogBoxLFMFile(lfmFileName: PAnsiChar; DlgProc: TDlgProc): LongBool; dcpcall;
var
  lfmStringList: TStringListEx;
begin
  if Assigned(lfmFileName) then
  begin
    lfmStringList:= TStringListEx.Create;
    try
      lfmStringList.LoadFromFile(lfmFileName);
      Result := DialogBox(LFMToLRS(lfmStringList.Text), DlgProc);
    finally
      FreeAndNil(lfmStringList);
    end;
  end
  else
    Result := False;
end;

function SendDlgMsg(pDlg: PtrUInt; DlgItemName: PAnsiChar; Msg, wParam, lParam: PtrInt): PtrInt; dcpcall;
var
  Key: Word;
  AText: String;
  Index: Integer;
  Control: TControl;
  lText: PAnsiChar absolute lParam;
  wText: PAnsiChar absolute wParam;
  pResult: Pointer absolute Result;
  DialogBox: TDialogBox absolute pDlg;
begin
  // find component by name
  for Index:= 0 to DialogBox.ComponentCount - 1 do
  begin
    Control:= TControl(DialogBox.Components[Index]);
    if CompareText(Control.Name, DlgItemName) = 0 then Break;
  end;
  // process message
  case Msg of
  DM_CLOSE:
    begin
      DialogBox.Close;
      if wParam <> -1 then
        DialogBox.ModalResult:= wParam;
    end;
  DM_ENABLE:
    begin
      Result:= PtrInt(Control.Enabled);
      if wParam <> -1 then
        Control.Enabled:= Boolean(wParam);
    end;
  DM_GETCHECK:
    begin
      if Control is TCheckBox then
        Result:= PtrInt((Control as TCheckBox).State);
      if Control is TRadioButton then
        Result := PtrInt((Control as TRadioButton).Checked);
    end;
  DM_GETDLGBOUNDS:
    begin
      with DialogBox do
      begin
        FRect.Left:= DialogBox.Left;
        FRect.Top:= DialogBox.Top;
        FRect.Right:= DialogBox.Left + DialogBox.Width;
        FRect.Bottom:= DialogBox.Top + DialogBox.Height;
        pResult:= @FRect;
      end;
    end;
  DM_GETDLGDATA:
    begin
      Result:= DialogBox.Tag;
    end;
  DM_GETDROPPEDDOWN:
    begin
      if Control is TComboBox then
        Result:= PtrInt((Control as TComboBox).DroppedDown);
    end;
  DM_GETITEMBOUNDS:
    begin
      with DialogBox do
      begin
        FRect.Left:= Control.Left;
        FRect.Top:= Control.Top;
        FRect.Right:= Control.Left + Control.Width;
        FRect.Bottom:= Control.Top + Control.Height;
        pResult:= @FRect;
      end;
    end;
  DM_GETITEMDATA:
    begin
      Result:= Control.Tag;
    end;
  DM_LISTADD:
    begin
      AText:= StrPas(wText);
      if Control is TComboBox then
        Result:= TComboBox(Control).Items.AddObject(AText, TObject(lText))
      else if Control is TListBox then
        Result:= TListBox(Control).Items.AddObject(AText, TObject(lText))
      else if Control is TMemo then
        Result:= TMemo(Control).Lines.AddObject(AText, TObject(lText));
    end;
  DM_LISTADDSTR:
    begin
      AText:= StrPas(wText);
      if Control is TComboBox then
        Result:= TComboBox(Control).Items.Add(AText)
      else if Control is TListBox then
        Result:= TListBox(Control).Items.Add(AText)
      else if Control is TMemo then
        Result:= TMemo(Control).Lines.Add(AText);
    end;
  DM_LISTDELETE:
    begin
      if Control is TComboBox then
        TComboBox(Control).Items.Delete(wParam)
      else if Control is TListBox then
        TListBox(Control).Items.Delete(wParam)
      else if Control is TMemo then
        TMemo(Control).Lines.Delete(wParam);
    end;
  DM_LISTINDEXOF:
    begin
      AText:= StrPas(lText);
      if Control is TComboBox then
        Result:= TComboBox(Control).Items.IndexOf(AText)
      else if Control is TListBox then
        Result:= TListBox(Control).Items.IndexOf(AText)
      else if Control is TMemo then
        Result:= TMemo(Control).Lines.IndexOf(AText);
    end;
  DM_LISTINSERT:
    begin
      AText:= StrPas(lText);
      if Control is TComboBox then
        TComboBox(Control).Items.Insert(wParam, AText)
      else if Control is TListBox then
        TListBox(Control).Items.Insert(wParam, AText)
      else if Control is TMemo then
        TMemo(Control).Lines.Insert(wParam, AText);
    end;
  DM_LISTGETCOUNT:
    begin
      if Control is TComboBox then
        Result:= TComboBox(Control).Items.Count
      else if Control is TListBox then
        Result:= TListBox(Control).Items.Count
      else if Control is TMemo then
        Result:= TMemo(Control).Lines.Count;
    end;
  DM_LISTGETDATA:
    begin
      if Control is TComboBox then
        Result:= PtrInt(TComboBox(Control).Items.Objects[wParam])
      else if Control is TListBox then
        Result:= PtrInt(TListBox(Control).Items.Objects[wParam])
      else if Control is TMemo then
        Result:= PtrInt(TMemo(Control).Lines.Objects[wParam]);
    end;
  DM_LISTGETITEM:
    begin
      with DialogBox do
      begin
        if Control is TComboBox then
          FText:= TComboBox(Control).Items[wParam]
        else if Control is TListBox then
          FText:= TListBox(Control).Items[wParam]
        else if Control is TMemo then
          FText:= TMemo(Control).Lines[wParam];
        pResult:= PAnsiChar(FText);
      end;
    end;
  DM_LISTGETITEMINDEX:
    begin
      Result:= -1;
      if Control is TComboBox then
        Result:= TComboBox(Control).ItemIndex
      else if Control is TListBox then
        Result:= TListBox(Control).ItemIndex
      else if Control is TRadioGroup then
        Result:= TRadioGroup(Control).ItemIndex;
    end;
  DM_LISTSETITEMINDEX:
    begin
      if Control is TComboBox then
        TComboBox(Control).ItemIndex:= wParam
      else if Control is TListBox then
        TListBox(Control).ItemIndex:= wParam
      else if Control is TRadioGroup then
        TRadioGroup(Control).ItemIndex:= wParam;
    end;
  DM_LISTUPDATE:
    begin
      AText:= StrPas(lText);
      if Control is TComboBox then
        TComboBox(Control).Items[wParam]:= AText
      else if Control is TListBox then
        TListBox(Control).Items[wParam]:= AText
      else if Control is TMemo then
        TMemo(Control).Lines[wParam]:= AText;
    end;
  DM_GETTEXT:
    begin
      with DialogBox do
      begin
        if Control is TButton then
          FText:= TButton(Control).Caption
        else if Control is TComboBox then
          FText:= TComboBox(Control).Text
        else if Control is TMemo then
          FText:= TMemo(Control).Text
        else if Control is TEdit then
          FText:= TEdit(Control).Text
        else if Control is TGroupBox then
          FText:= TGroupBox(Control).Caption
        else if Control is TLabel then
          FText:= TLabel(Control).Caption
        else if Control is TFileNameEdit then
          FText:= TFileNameEdit(Control).Text;
        pResult:= PAnsiChar(FText);
      end;
    end;
  DM_KEYDOWN:
    begin
      Key:= wParam;
      DialogBox.KeyDown(Key, GetKeyShiftState);
      Result:= Key;
    end;
  DM_KEYUP:
    begin
      Key:= wParam;
      DialogBox.KeyUp(Key, GetKeyShiftState);
      Result:= Key;
    end;
  DM_REDRAW:
    begin
      DialogBox.Repaint;
    end;
  DM_SETCHECK:
    begin
      if Control is TCheckBox then
        begin
          Result:= PtrInt((Control as TCheckBox).State);
          (Control as TCheckBox).State:= TCheckBoxState(wParam)
        end;
      if Control is TRadioButton then
        begin
          Result := PtrInt((Control as TRadioButton).Checked);
          (Control as TRadioButton).Checked:= Boolean(wParam);
        end;
    end;
  DM_LISTSETDATA:
    begin
      if Control is TComboBox then
        TComboBox(Control).Items.Objects[wParam]:= TObject(lText)
      else if Control is TListBox then
        TListBox(Control).Items.Objects[wParam]:= TObject(lText)
      else if Control is TMemo then
        TMemo(Control).Lines.Objects[wParam]:= TObject(lText);
    end;
  DM_SETDLGBOUNDS:
    begin
      with DialogBox do
      begin
        FRect:= PRect(wText)^;
        DialogBox.Left:= FRect.Left;
        DialogBox.Top:= FRect.Top;
        DialogBox.Width:= FRect.Right - FRect.Left;
        DialogBox.Height:= FRect.Bottom - FRect.Top;
      end;
    end;
  DM_SETDLGDATA:
    begin
      Result:= DialogBox.Tag;
      DialogBox.Tag:= wParam;
    end;
  DM_SETDROPPEDDOWN:
    begin
      if Control is TComboBox then
        (Control as TComboBox).DroppedDown:= Boolean(wParam);
    end;
  DM_SETFOCUS:
    begin
      if Control.Visible then
        (Control as TWinControl).SetFocus;
    end;
  DM_SETITEMBOUNDS:
    begin
      with DialogBox do
      begin
        FRect:= PRect(wText)^;
        Control.Left:= FRect.Left;
        Control.Top:= FRect.Top;
        Control.Width:= FRect.Right - FRect.Left;
        Control.Height:= FRect.Bottom - FRect.Top;
      end;
    end;
  DM_SETITEMDATA:
    begin
      Control.Tag:= wParam;
    end;
  DM_SETMAXTEXTLENGTH:
    begin
      Result:= -1;
      if Control is TComboBox then
        begin
          Result:= (Control as TComboBox).MaxLength;
          (Control as TComboBox).MaxLength:= wParam;
        end;
      if Control is TEdit then
        begin
          Result:= (Control as TEdit).MaxLength;
          (Control as TEdit).MaxLength:= wParam;
        end;
    end;
  DM_SETTEXT:
    begin
      AText:= StrPas(wText);
      if Control is TButton then
        TButton(Control).Caption:= AText
      else if Control is TComboBox then
        TComboBox(Control).Text:= AText
      else if Control is TMemo then
        TMemo(Control).Text:= AText
      else if Control is TEdit then
        TEdit(Control).Text:= AText
      else if Control is TGroupBox then
        TGroupBox(Control).Caption:= AText
      else if Control is TLabel then
        TLabel(Control).Caption:= AText
      else if Control is TFileNameEdit then
        TFileNameEdit(Control).Text:= AText;
    end;
  DM_SHOWDIALOG:
    begin
      if wParam = 0 then
        DialogBox.Hide;
      if wParam = 1 then
        DialogBox.Show;
    end;
  DM_SHOWITEM:
    begin
      Result:= PtrInt(Control.Visible);
      if wParam <> -1 then
        Control.Visible:= Boolean(wParam);
    end;
  DM_SETPROGRESSVALUE:
    begin
      if (Control is TProgressBar) then
      begin
        TProgressBar(Control).Position:= wParam;
      end;
    end;
  DM_SETPROGRESSSTYLE:
    begin
      if (Control is TProgressBar) then
      begin
        TProgressBar(Control).Style:= TProgressBarStyle(wParam);
      end;
    end;
  end;
end;

{ TDialogBox }

procedure TDialogBox.ShowDialogBox;
begin
  FResult:= (ShowModal = mrOK);
end;

procedure TDialogBox.ProcessResource;
begin
  if not InitResourceComponent(Self, TForm) then
    if RequireDerivedFormResource then
      raise EResNotFound.CreateFmt(rsFormResourceSNotFoundForResourcelessFormsCreateNew, [ClassName])
  else
    DCDebug(Format(rsFormResourceSNotFoundForResourcelessFormsCreateNew, [ClassName]));
end;

function TDialogBox.InitResourceComponent(Instance: TComponent; RootAncestor: TClass): Boolean;

  function InitComponent(ClassType: TClass): Boolean;
  var
    Stream: TStream;
    Reader: TReader;
    DestroyDriver: Boolean;
    Driver: TAbstractObjectReader;
  begin
    Result := False;
    if (ClassType = TComponent) or (ClassType = RootAncestor) then
      Exit;
    if Assigned(ClassType.ClassParent) then
      Result := InitComponent(ClassType.ClassParent);

    Stream := TStringStream.Create(FLRSData);

    try
      //DCDebug('Form Stream "', ClassType.ClassName, '"');
      DestroyDriver := False;
      Reader := CreateLRSReader(Stream, DestroyDriver);
      if Assigned(FTranslator) then begin
        Reader.OnReadStringProperty:= @FTranslator.TranslateStringProperty;
      end;
      try
        Reader.ReadRootComponent(Instance);
      finally
        Driver := Reader.Driver;
        Reader.Free;
        if DestroyDriver then
          Driver.Free;
      end;
    finally
      Stream.Free;
    end;
    Result := True;
  end;

begin
  if Instance.ComponentState * [csLoading, csInline] <> []
  then begin
    // global loading not needed
    Result := InitComponent(Instance.ClassType);
  end
  else try
    BeginGlobalLoading;
    Result := InitComponent(Instance.ClassType);
    NotifyGlobalLoading;
  finally
    EndGlobalLoading;
  end;
end;

constructor TDialogBox.Create(const LRSData: String; DlgProc: TDlgProc);
var
  Path: String;
  Language: String;
  FileName: String;
begin
  FLRSData:= LRSData;
  FDlgProc:= DlgProc;

  FileName:= mbGetModuleName(DlgProc);
  Path:= ExtractFilePath(FileName) + 'language' + PathDelim;
  Language:= ExtractFileExt(ExtractFileNameOnly(gPOFileName));
  FileName:= Path + ExtractFileNameOnly(FileName) + Language + '.po';
  if mbFileExists(FileName) then FTranslator:= TTranslator.Create(FileName);

  inherited Create(Screen.ActiveForm);
end;

destructor TDialogBox.Destroy;
begin
  inherited Destroy;
  FTranslator.Free;
end;

procedure TDialogBox.DialogBoxShow(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    fDlgProc(PtrUInt(Pointer(Self)), PAnsiChar((Sender as TControl).Name), DN_INITDIALOG,0,0);
end;

procedure TDialogBox.ButtonClick(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    fDlgProc(PtrUInt(Pointer(Self)), PAnsiChar((Sender as TControl).Name), DN_CLICK,0,0);
end;

procedure TDialogBox.ButtonEnter(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    fDlgProc(PtrUInt(Pointer(Self)), PAnsiChar((Sender as TControl).Name), DN_GOTFOCUS,0,0);
end;

procedure TDialogBox.ButtonExit(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    fDlgProc(PtrUInt(Pointer(Self)), PAnsiChar((Sender as TControl).Name), DN_KILLFOCUS,0,0);
end;

procedure TDialogBox.ButtonKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(fDlgProc) then
    fDlgProc(PtrUInt(Pointer(Self)), PAnsiChar((Sender as TControl).Name), DN_KEYDOWN, PtrInt(@Key), Integer(Shift));
end;

procedure TDialogBox.ButtonKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(fDlgProc) then
    fDlgProc(PtrUInt(Pointer(Self)), PAnsiChar((Sender as TControl).Name), DN_KEYUP, PtrInt(@Key), Integer(Shift));
end;

procedure TDialogBox.ComboBoxClick(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    fDlgProc(PtrUInt(Pointer(Self)), PAnsiChar((Sender as TControl).Name), DN_CLICK,PtrInt((Sender as TComboBox).ItemIndex),0);
end;

procedure TDialogBox.ComboBoxDblClick(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    fDlgProc(PtrUInt(Pointer(Self)), PAnsiChar((Sender as TControl).Name), DN_DBLCLICK,PtrInt((Sender as TComboBox).ItemIndex),0);
end;

procedure TDialogBox.ComboBoxChange(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    begin
      fDlgProc(PtrUInt(Pointer(Self)), PAnsiChar((Sender as TControl).Name), DN_CHANGE, PtrInt((Sender as TComboBox).ItemIndex),0);
    end;
end;

procedure TDialogBox.ComboBoxEnter(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    fDlgProc(PtrUInt(Pointer(Self)), PAnsiChar((Sender as TControl).Name), DN_GOTFOCUS,0,0);
end;

procedure TDialogBox.ComboBoxExit(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    fDlgProc(PtrUInt(Pointer(Self)), PAnsiChar((Sender as TControl).Name), DN_KILLFOCUS,0,0);
end;

procedure TDialogBox.ComboBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(fDlgProc) then
    fDlgProc(PtrUInt(Pointer(Self)), PAnsiChar((Sender as TControl).Name), DN_KEYDOWN, PtrInt(@Key), Integer(Shift));
end;

procedure TDialogBox.ComboBoxKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(fDlgProc) then
    fDlgProc(PtrUInt(Pointer(Self)), PAnsiChar((Sender as TControl).Name), DN_KEYUP, PtrInt(@Key), Integer(Shift));
end;

procedure TDialogBox.EditClick(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    fDlgProc(PtrUInt(Pointer(Self)), PAnsiChar((Sender as TControl).Name), DN_CLICK,0,0);
end;

procedure TDialogBox.EditDblClick(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    fDlgProc(PtrUInt(Pointer(Self)), PAnsiChar((Sender as TControl).Name), DN_DBLCLICK,0,0);
end;

procedure TDialogBox.EditChange(Sender: TObject);
var
  sText: String;
begin
  if Assigned(fDlgProc) then
    begin
      sText:= (Sender as TEdit).Text;
      fDlgProc(PtrUInt(Pointer(Self)), PAnsiChar((Sender as TControl).Name), DN_CHANGE, PtrInt(PAnsiChar(sText)), 0);
    end;
end;

procedure TDialogBox.EditEnter(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    fDlgProc(PtrUInt(Pointer(Self)), PAnsiChar((Sender as TControl).Name), DN_GOTFOCUS,0,0);
end;

procedure TDialogBox.EditExit(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    fDlgProc(PtrUInt(Pointer(Self)), PAnsiChar((Sender as TControl).Name), DN_KILLFOCUS,0,0);
end;

procedure TDialogBox.EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Assigned(fDlgProc) then
    fDlgProc(PtrUInt(Pointer(Self)), PAnsiChar((Sender as TControl).Name), DN_KEYDOWN, PtrInt(@Key), Integer(Shift));
end;

procedure TDialogBox.EditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Assigned(fDlgProc) then
    fDlgProc(PtrUInt(Pointer(Self)), PAnsiChar((Sender as TControl).Name), DN_KEYUP, PtrInt(@Key), Integer(Shift));
end;

procedure TDialogBox.ListBoxClick(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    begin
      fDlgProc(PtrUInt(Pointer(Self)), PAnsiChar((Sender as TControl).Name), DN_CLICK, PtrInt((Sender as TListBox).ItemIndex),0);
    end;
end;

procedure TDialogBox.ListBoxDblClick(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    begin
      fDlgProc(PtrUInt(Pointer(Self)), PAnsiChar((Sender as TControl).Name), DN_DBLCLICK, PtrInt((Sender as TListBox).ItemIndex),0);
    end;
end;

procedure TDialogBox.ListBoxSelectionChange(Sender: TObject; User: boolean);
begin
  if Assigned(fDlgProc) then
    begin
      fDlgProc(PtrUInt(Pointer(Self)), PAnsiChar((Sender as TControl).Name), DN_CHANGE, PtrInt((Sender as TListBox).ItemIndex),0);
    end;
end;

procedure TDialogBox.ListBoxEnter(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    fDlgProc(PtrUInt(Pointer(Self)), PAnsiChar((Sender as TControl).Name), DN_GOTFOCUS,0,0);
end;

procedure TDialogBox.ListBoxExit(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    fDlgProc(PtrUInt(Pointer(Self)), PAnsiChar((Sender as TControl).Name), DN_KILLFOCUS,0,0);
end;

procedure TDialogBox.ListBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(fDlgProc) then
    fDlgProc(PtrUInt(Pointer(Self)), PAnsiChar((Sender as TControl).Name), DN_KEYDOWN, PtrInt(@Key), Integer(Shift));
end;

procedure TDialogBox.ListBoxKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(fDlgProc) then
    fDlgProc(PtrUInt(Pointer(Self)), PAnsiChar((Sender as TControl).Name), DN_KEYUP, PtrInt(@Key), Integer(Shift));
end;

procedure TDialogBox.CheckBoxChange(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    begin
      fDlgProc(PtrUInt(Pointer(Self)), PAnsiChar((Sender as TControl).Name), DN_CHANGE, PtrInt((Sender as TCheckBox).Checked),0);
    end;
end;

initialization
  {.$I fdialogbox.lrs}

end.

