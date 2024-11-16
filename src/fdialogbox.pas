{
    Double Commander
    -------------------------------------------------------------------------
    This unit contains realization of Dialog API functions.

    Copyright (C) 2008-2024 Alexander Koblov (alexx2000@mail.ru)

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
  Types, Buttons, ExtCtrls, EditBtn, Extension, ComCtrls, DividerBevel, SynEdit,
  RttiUtils, TypInfo;

type

  { TDialogBox }

  TDialogBox = class(TForm)
    // Dialog events
    procedure DialogBoxShow(Sender: TObject);
    procedure DialogBoxClose(Sender: TObject; var CloseAction: TCloseAction);
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
    // Timer events
    procedure TimerTimer(Sender: TObject);
  private
    FRect: TRect;
    FText: String;
    FSelf: UIntPtr;
    FLRSData: String;
    FResult: LongBool;
    FDlgProc: TDlgProc;
    FPropValue: String;
    FInfoList: TStringList;
    FPropsStorage: TPropsStorage;
    FTranslator: TAbstractTranslator;
  private
    function GetComponent(const AName: PAnsiChar): TComponent;
    procedure WritePropValue(const ASection, Item, Value: String);
    function ReadPropValue(const ASection, Item, Default: String): String;
    function FindPropInfo(var AObject: TObject; const AName: String): PPropInfo;
    function SetProperty(AComponent: TComponent; const AName: String; AValue: Pointer; AType: Integer): Boolean;
    function GetProperty(AComponent: TComponent; const AName: String; AValue: Pointer; AType, ASize: Integer): Boolean;
  protected
    procedure ProcessResource; override;
    function InitResourceComponent(Instance: TComponent; RootAncestor: TClass): Boolean;
  public
    constructor Create(const LRSData: String; DlgProc: TDlgProc); reintroduce;
    destructor Destroy; override;
  end; 

  { TDialogBoxData }

  TDialogBoxData = class
  private
    FLRSData: String;
    FDlgProc: TDlgProc;
    FUserData: Pointer;
    DialogResult: LongBool;
    procedure ShowDialogBox;
  public
    constructor Create(const LRSData: String; DlgProc: TDlgProc; UserData: Pointer);
  end;

function InputBox(Caption, Prompt: PAnsiChar; MaskInput: LongBool; Value: PAnsiChar; ValueMaxLen: Integer): LongBool; dcpcall;
function MessageBox(Text, Caption: PAnsiChar; Flags: Longint): Integer; dcpcall;
function MsgChoiceBox(Text, Caption: PAnsiChar; Buttons: PPAnsiChar; BtnDef, BtnEsc: Integer): Integer; dcpcall;
function DialogBoxLFM(LFMData: Pointer; DataSize: LongWord; DlgProc: TDlgProc): LongBool; dcpcall;
function DialogBoxLRS(LRSData: Pointer; DataSize: LongWord; DlgProc: TDlgProc): LongBool; dcpcall;
function DialogBoxLFMFile(lfmFileName: PAnsiChar; DlgProc: TDlgProc): LongBool; dcpcall;
function DialogBoxParam(Data: Pointer; DataSize: LongWord; DlgProc: TDlgProc; Flags: UInt32; UserData, Reserved: Pointer): UIntPtr; dcpcall;
function SendDlgMsg(pDlg: PtrUInt; DlgItemName: PAnsiChar; Msg, wParam, lParam: PtrInt): PtrInt; dcpcall;
function SetProperty(pDlg: UIntPtr; DlgItemName, PropName: PAnsiChar; PropValue: Pointer; PropType: Integer): PtrInt; dcpcall;
function GetProperty(pDlg: UIntPtr; DlgItemName, PropName: PAnsiChar; PropValue: Pointer; PropType, PropSize: Integer): PtrInt; dcpcall;
function CreateComponent(pDlg: UIntPtr; Parent, DlgItemName, DlgItemClass: PAnsiChar; Reserved: Pointer): UIntPtr; dcpcall;

implementation

uses
  LCLStrConsts, LazFileUtils, DCOSUtils, DCStrUtils, uShowMsg,
  uDebug, uTranslator, uGlobs, uFileProcs;

type
  TControlProtected = class(TControl);

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

function MsgChoiceBox(Text, Caption: PAnsiChar; Buttons: PPAnsiChar; BtnDef, BtnEsc: Integer): Integer; dcpcall;
var
  AButtons: TStringArray;
begin
  AButtons:= Default(TStringArray);
  while (Buttons^ <> nil) do
  begin
    AddString(AButtons, Buttons^);
    Inc(Buttons);
  end;
  Result:= uShowMsg.MsgChoiceBox(nil, Text, Caption, AButtons, BtnDef, BtnEsc);
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

function DialogBox(const LRSData: String; DlgProc: TDlgProc; UserData: Pointer): LongBool;
var
  AData: TDialogBoxData;
begin
  AData:= TDialogBoxData.Create(LRSData, DlgProc, UserData);
  try
    TThread.Synchronize(nil, @AData.ShowDialogBox);
    Result:= AData.DialogResult;
  finally
    AData.Free;
  end;
end;

function DialogBoxLFM(LFMData: Pointer; DataSize: LongWord; DlgProc: TDlgProc): LongBool; dcpcall;
var
  DataString: String;
begin
  if Assigned(LFMData) and (DataSize > 0) then
  begin
    SetString(DataString, LFMData, DataSize);
    Result := DialogBox(LFMToLRS(DataString), DlgProc, nil);
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
    Result := DialogBox(DataString, DlgProc, nil);
  end
  else
    Result := False;
end;

function DialogBoxLFMFile(lfmFileName: PAnsiChar; DlgProc: TDlgProc): LongBool; dcpcall;
var
  DataString: String;
begin
  if (lfmFileName = nil) then
    Result := False
  else begin
    DataString := mbReadFileToString(lfmFileName);
    Result := DialogBox(LFMToLRS(DataString), DlgProc, nil);
  end;
end;

function DialogBoxParam(Data: Pointer; DataSize: LongWord;
  DlgProc: TDlgProc; Flags: UInt32; UserData, Reserved: Pointer): UIntPtr; dcpcall;
var
  DataString: String;
begin
  if (Data = nil) then Exit(0);
  if (DataSize = 0) then Exit(0);
  SetString(DataString, Data, DataSize);

  if (Flags and DB_FILENAME <> 0) then
  begin
    DataString:= LFMToLRS(mbReadFileToString(DataString));
  end
  else if (Flags and DB_LRS = 0) then
  begin
    DataString:= LFMToLRS(DataString);
  end;

  Result:= UIntPtr(DialogBox(DataString, DlgProc, UserData));
end;

function SendDlgMsg(pDlg: PtrUInt; DlgItemName: PAnsiChar; Msg, wParam, lParam: PtrInt): PtrInt; dcpcall;
var
  Key: Word;
  AText: String;
  Component: TComponent;
  lText: PAnsiChar absolute lParam;
  wText: PAnsiChar absolute wParam;
  pResult: Pointer absolute Result;
  DialogBox: TDialogBox absolute pDlg;
  Control: TControl absolute Component;
begin
  // find component by name
  Component:= DialogBox.GetComponent(DlgItemName);
  if (Component = nil) then Exit(-1);
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
      if (Component is TTimer) then
      begin
        Result:= PtrInt(TTimer(Component).Enabled);
        if wParam <> -1 then
          TTimer(Component).Enabled:= Boolean(wParam);
      end
      else begin
        Result:= PtrInt(Control.Enabled);
        if wParam <> -1 then
          Control.Enabled:= Boolean(wParam);
      end;
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
        Result:= TMemo(Control).Lines.AddObject(AText, TObject(lText))
      else if Control is TSynEdit then
        Result:= TSynEdit(Control).Lines.AddObject(AText, TObject(lText));
    end;
  DM_LISTADDSTR:
    begin
      AText:= StrPas(wText);
      if Control is TComboBox then
        Result:= TComboBox(Control).Items.Add(AText)
      else if Control is TListBox then
        Result:= TListBox(Control).Items.Add(AText)
      else if Control is TMemo then
        Result:= TMemo(Control).Lines.Add(AText)
      else if Control is TSynEdit then
        Result:= TSynEdit(Control).Lines.Add(AText);
    end;
  DM_LISTDELETE:
    begin
      if Control is TComboBox then
        TComboBox(Control).Items.Delete(wParam)
      else if Control is TListBox then
        TListBox(Control).Items.Delete(wParam)
      else if Control is TMemo then
        TMemo(Control).Lines.Delete(wParam)
      else if Control is TSynEdit then
        TSynEdit(Control).Lines.Delete(wParam);
    end;
  DM_LISTINDEXOF:
    begin
      AText:= StrPas(lText);
      if Control is TComboBox then
        Result:= TComboBox(Control).Items.IndexOf(AText)
      else if Control is TListBox then
        Result:= TListBox(Control).Items.IndexOf(AText)
      else if Control is TMemo then
        Result:= TMemo(Control).Lines.IndexOf(AText)
      else if Control is TSynEdit then
        Result:= TSynEdit(Control).Lines.IndexOf(AText);
    end;
  DM_LISTINSERT:
    begin
      AText:= StrPas(lText);
      if Control is TComboBox then
        TComboBox(Control).Items.Insert(wParam, AText)
      else if Control is TListBox then
        TListBox(Control).Items.Insert(wParam, AText)
      else if Control is TMemo then
        TMemo(Control).Lines.Insert(wParam, AText)
      else if Control is TSynEdit then
        TSynEdit(Control).Lines.Insert(wParam, AText);
    end;
  DM_LISTGETCOUNT:
    begin
      if Control is TComboBox then
        Result:= TComboBox(Control).Items.Count
      else if Control is TListBox then
        Result:= TListBox(Control).Items.Count
      else if Control is TMemo then
        Result:= TMemo(Control).Lines.Count
      else if Control is TSynEdit then
        Result:= TSynEdit(Control).Lines.Count;
    end;
  DM_LISTGETDATA:
    begin
      if Control is TComboBox then
        Result:= PtrInt(TComboBox(Control).Items.Objects[wParam])
      else if Control is TListBox then
        Result:= PtrInt(TListBox(Control).Items.Objects[wParam])
      else if Control is TMemo then
        Result:= PtrInt(TMemo(Control).Lines.Objects[wParam])
      else if Control is TSynEdit then
        Result:= PtrInt(TSynEdit(Control).Lines.Objects[wParam]);
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
          FText:= TMemo(Control).Lines[wParam]
        else if Control is TSynEdit then
          FText:= TSynEdit(Control).Lines[wParam];
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
        TMemo(Control).Lines[wParam]:= AText
      else if Control is TSynEdit then
        TSynEdit(Control).Lines[wParam]:= AText;
    end;
  DM_LISTCLEAR:
    begin
      if Control is TComboBox then
        TComboBox(Control).Clear
      else if Control is TListBox then
        TListBox(Control).Clear
      else if Control is TMemo then
        TMemo(Control).Clear
      else if Control is TSynEdit then
        TSynEdit(Control).Clear;
    end;
  DM_GETTEXT:
    begin
      with DialogBox do
      begin
        if Control is TButton then
          FText:= TButton(Control).Caption
        else if Control is TComboBox then
          FText:= TComboBox(Control).Text
        else if Control is TCheckBox then
          FText:= TCheckBox(Control).Caption
        else if Control is TMemo then
          FText:= TMemo(Control).Text
        else if Control is TEdit then
          FText:= TEdit(Control).Text
        else if Control is TGroupBox then
          FText:= TGroupBox(Control).Caption
        else if Control is TLabel then
          FText:= TLabel(Control).Caption
        else if Control is TFileNameEdit then
          FText:= TFileNameEdit(Control).Text
        else begin
          FText:= TControlProtected(Control).Text
        end;
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
        TMemo(Control).Lines.Objects[wParam]:= TObject(lText)
      else if Control is TSynEdit then
        TSynEdit(Control).Lines.Objects[wParam]:= TObject(lText);
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
      else if Control is TCheckBox then
        TCheckBox(Control).Caption:= AText
      else if Control is TMemo then
        TMemo(Control).Text:= AText
      else if Control is TEdit then
        TEdit(Control).Text:= AText
      else if Control is TGroupBox then
        TGroupBox(Control).Caption:= AText
      else if Control is TLabel then
        TLabel(Control).Caption:= AText
      else if Control is TFileNameEdit then
        TFileNameEdit(Control).Text:= AText
      else begin
        TControlProtected(Control).Text:= AText;
      end;
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
  DM_SETPASSWORDCHAR:
    begin
      if (Control is TCustomEdit) then
      begin
        TCustomEdit(Control).PasswordChar:= Char(wParam);
      end;
    end;
  DM_TIMERSETINTERVAL:
    begin
      if (Component is TTimer) then
      begin
        TTimer(Component).Interval:= wParam;
      end;
    end;
  end;
end;

function SetProperty(pDlg: UIntPtr; DlgItemName, PropName: PAnsiChar; PropValue: Pointer; PropType: Integer): PtrInt; dcpcall;
var
  Component: TComponent;
  DialogBox: TDialogBox absolute pDlg;
begin
  // find component by name
  Component:= DialogBox.GetComponent(DlgItemName);
  if (Component = nil) then
    Result:= -1
  else begin
    Result:= PtrInt(DialogBox.SetProperty(Component, PropName, PropValue, PropType));
  end;
end;

function GetProperty(pDlg: UIntPtr; DlgItemName, PropName: PAnsiChar; PropValue: Pointer; PropType, PropSize: Integer): PtrInt; dcpcall;
var
  Component: TComponent;
  DialogBox: TDialogBox absolute pDlg;
begin
  // find component by name
  Component:= DialogBox.GetComponent(DlgItemName);
  if (Component = nil) then
    Result:= -1
  else begin
    Result:= PtrInt(DialogBox.GetProperty(Component, PropName, PropValue, PropType, PropSize));
  end;
end;

function CreateComponent(pDlg: UIntPtr; Parent, DlgItemName, DlgItemClass: PAnsiChar; Reserved: Pointer): UIntPtr; dcpcall;
var
  AParent: TComponent;
  AClass: TPersistentClass;
  DialogBox: TDialogBox absolute pDlg;
  Component: TComponent absolute Result;
begin
  AClass:= GetClass(DlgItemClass);
  if (AClass = nil) then
    Result:= 0
  else begin
    Component:= TComponent(TComponentClass(AClass).Create(DialogBox));
    Component.Name:= DlgItemName;
    if Component is TControl then
    begin
      AParent:= DialogBox.GetComponent(Parent);
      if Assigned(AParent) and (AParent is TWinControl) then
      begin
        TControl(Component).Parent:= TWinControl(AParent);
      end;
    end;
  end;
end;

{ TDialogBox }

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
  FSelf:= UIntPtr(Self);

  FileName:= mbGetModuleName(DlgProc);
  Path:= ExtractFilePath(FileName) + 'language' + PathDelim;
  Language:= ExtractFileExt(ExtractFileNameOnly(gPOFileName));
  FileName:= Path + ExtractFileNameOnly(FileName) + Language + '.po';
  if mbFileExists(FileName) then FTranslator:= TTranslator.Create(FileName);

  FInfoList:= TStringList.Create;
  FInfoList.OwnsObjects:= True;
  FPropsStorage:= TPropsStorage.Create;
  FPropsStorage.OnReadString:= @ReadPropValue;
  FPropsStorage.OnWriteString:= @WritePropValue;

  inherited Create(Screen.ActiveForm);
end;

destructor TDialogBox.Destroy;
begin
  inherited Destroy;
  FPropsStorage.Free;
  FTranslator.Free;
  FInfoList.Free;
end;

procedure TDialogBox.WritePropValue(const ASection, Item, Value: String);
begin
  FPropValue:= Value;
end;

function TDialogBox.ReadPropValue(const ASection, Item, Default: String): String;
begin
  Result:= FPropValue;
end;

function TDialogBox.GetComponent(const AName: PAnsiChar): TComponent;
begin
  if (AName = nil) then
    Result:= Self
  else begin
    Result:= Self.FindComponent(AName);
  end;
end;

function TDialogBox.FindPropInfo(var AObject: TObject; const AName: String): PPropInfo;
var
  PropName: String;
  FullName: String;
  Index, J: Integer;
  Props: TPropInfoList;
  ANames: TStringArray;
begin
  FullName:= TComponent(AObject).Name;
  ANames:= AName.Split(['.'], TStringSplitOptions.ExcludeEmpty);
  for J:= 0 to High(ANames) do
  begin
    FPropsStorage.AObject:= AObject;
    Index:= FInfoList.IndexOf(FullName);
    if Index >= 0 then
      Props:= TPropInfoList(FInfoList.Objects[Index])
    else begin
      Props:= TPropInfoList.Create(AObject, tkAny - [tkUnknown]);
      FInfoList.AddObject(FullName, Props);
    end;
    PropName:= ANames[J];
    Result:= Props.Find(PropName);
    if (Result = nil) then Break;
    if (J < High(ANames)) then
    begin
      if Result^.PropType^.Kind <> tkClass then Exit(nil);
      AObject:= GetObjectProp(AObject, PropName);
      if (AObject = nil) then Exit(nil);
      FullName+= '.' + PropName;
    end;
  end;
end;

function TDialogBox.SetProperty(AComponent: TComponent; const AName: String;
  AValue: Pointer; AType: Integer): Boolean;
var
  Method: TMethod;
  AObject: TObject;
  PropInfo: PPropInfo;
  Address: CodePointer;
begin
  Result:= False;
  AObject:= AComponent;
  PropInfo:= FindPropInfo(AObject, AName);
  if Assigned(PropInfo) then
  begin
    case AType of
      TK_BOOL:
        begin
          Result:= (PropInfo^.PropType^.Kind = tkBool);
          if Result then SetOrdProp(AObject, PropInfo, PInt32(AValue)^);
        end;
      TK_INT32:
        begin
          Result:= (PropInfo^.PropType^.Kind = tkInteger);
          if Result then SetOrdProp(AObject, PropInfo, PInt32(AValue)^);
        end;
      TK_INT64:
        begin
          Result:= (PropInfo^.PropType^.Kind = tkInt64);
          if Result then SetOrdProp(AObject, PropInfo, PInt64(AValue)^);
        end;
      TK_FLOAT:
        begin
          Result:= (PropInfo^.PropType^.Kind = tkFloat);
          if Result then SetFloatProp(AObject, PropInfo, PDouble(AValue)^);
        end;
      TK_STRING:
        begin
          if (PropInfo^.PropType^.Kind = tkMethod) then
          begin
            Address:= MethodAddress(PAnsiChar(AValue));
            if Assigned(Address) then
            begin
              Result:= True;
              Method.Data:= Self;
              Method.Code:= Address;
              SetMethodProp(AObject, AName, Method);
            end;
          end
          else begin
            Result:= True;
            FPropValue:= StrPas(PAnsiChar(AValue));
            FPropsStorage.LoadAnyProperty(PropInfo);
          end;
        end;
    end;
  end;
end;

function TDialogBox.GetProperty(AComponent: TComponent; const AName: String;
  AValue: Pointer; AType, ASize: Integer): Boolean;
var
  Method: TMethod;
  AObject: TObject;
  PropInfo: PPropInfo;
  Props: TPropInfoList;
begin
  Result:= False;
  AObject:= AComponent;
  PropInfo:= FindPropInfo(AObject, AName);
  if Assigned(PropInfo) then
  begin
    case AType of
      TK_BOOL:
        begin
          Result:= (PropInfo^.PropType^.Kind = tkBool) and (ASize = SizeOf(Int32));
          if Result then PInt32(AValue)^:= Int32(GetOrdProp(AObject, PropInfo));
        end;
      TK_INT32:
        begin
          Result:= (PropInfo^.PropType^.Kind = tkInteger) and (ASize = SizeOf(Int32));
          if Result then PInt32(AValue)^:= Int32(GetOrdProp(AObject, PropInfo));
        end;
      TK_INT64:
        begin
          Result:= (PropInfo^.PropType^.Kind = tkInt64) and (ASize = SizeOf(Int64));
          if Result then PInt64(AValue)^:= GetOrdProp(AObject, PropInfo);
        end;
      TK_FLOAT:
        begin
          Result:= (PropInfo^.PropType^.Kind = tkFloat) and (ASize = SizeOf(Double));
          if Result then PDouble(AValue)^:= Double(GetFloatProp(AObject, PropInfo));
        end;
      TK_STRING:
        begin
          if (PropInfo^.PropType^.Kind = tkMethod) then
          begin
            Method:= GetMethodProp(AObject, PropInfo);
            FPropValue:= MethodName(Method.Code);
          end
          else begin
            FPropsStorage.StoreAnyProperty(PropInfo);
          end;
          Result:= True;
          StrPLCopy(PAnsiChar(AValue), FPropValue, ASize);
        end;
    end;
  end;
end;

procedure TDialogBox.DialogBoxShow(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    fDlgProc(FSelf, PAnsiChar((Sender as TControl).Name), DN_INITDIALOG,0,0);
end;

procedure TDialogBox.DialogBoxClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(fDlgProc) then
    fDlgProc(FSelf, PAnsiChar((Sender as TControl).Name), DN_CLOSE, 0, 0);
end;

procedure TDialogBox.ButtonClick(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    fDlgProc(FSelf, PAnsiChar((Sender as TControl).Name), DN_CLICK,0,0);
end;

procedure TDialogBox.ButtonEnter(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    fDlgProc(FSelf, PAnsiChar((Sender as TControl).Name), DN_GOTFOCUS,0,0);
end;

procedure TDialogBox.ButtonExit(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    fDlgProc(FSelf, PAnsiChar((Sender as TControl).Name), DN_KILLFOCUS,0,0);
end;

procedure TDialogBox.ButtonKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(fDlgProc) then
    fDlgProc(FSelf, PAnsiChar((Sender as TControl).Name), DN_KEYDOWN, PtrInt(@Key), Integer(Shift));
end;

procedure TDialogBox.ButtonKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(fDlgProc) then
    fDlgProc(FSelf, PAnsiChar((Sender as TControl).Name), DN_KEYUP, PtrInt(@Key), Integer(Shift));
end;

procedure TDialogBox.ComboBoxClick(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    fDlgProc(FSelf, PAnsiChar((Sender as TControl).Name), DN_CLICK,PtrInt((Sender as TComboBox).ItemIndex),0);
end;

procedure TDialogBox.ComboBoxDblClick(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    fDlgProc(FSelf, PAnsiChar((Sender as TControl).Name), DN_DBLCLICK,PtrInt((Sender as TComboBox).ItemIndex),0);
end;

procedure TDialogBox.ComboBoxChange(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    begin
      fDlgProc(FSelf, PAnsiChar((Sender as TControl).Name), DN_CHANGE, PtrInt((Sender as TComboBox).ItemIndex),0);
    end;
end;

procedure TDialogBox.ComboBoxEnter(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    fDlgProc(FSelf, PAnsiChar((Sender as TControl).Name), DN_GOTFOCUS,0,0);
end;

procedure TDialogBox.ComboBoxExit(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    fDlgProc(FSelf, PAnsiChar((Sender as TControl).Name), DN_KILLFOCUS,0,0);
end;

procedure TDialogBox.ComboBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(fDlgProc) then
    fDlgProc(FSelf, PAnsiChar((Sender as TControl).Name), DN_KEYDOWN, PtrInt(@Key), Integer(Shift));
end;

procedure TDialogBox.ComboBoxKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(fDlgProc) then
    fDlgProc(FSelf, PAnsiChar((Sender as TControl).Name), DN_KEYUP, PtrInt(@Key), Integer(Shift));
end;

procedure TDialogBox.EditClick(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    fDlgProc(FSelf, PAnsiChar((Sender as TControl).Name), DN_CLICK,0,0);
end;

procedure TDialogBox.EditDblClick(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    fDlgProc(FSelf, PAnsiChar((Sender as TControl).Name), DN_DBLCLICK,0,0);
end;

procedure TDialogBox.EditChange(Sender: TObject);
var
  sText: String;
begin
  if Assigned(fDlgProc) then
    begin
      sText:= (Sender as TEdit).Text;
      fDlgProc(FSelf, PAnsiChar((Sender as TControl).Name), DN_CHANGE, PtrInt(PAnsiChar(sText)), 0);
    end;
end;

procedure TDialogBox.EditEnter(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    fDlgProc(FSelf, PAnsiChar((Sender as TControl).Name), DN_GOTFOCUS,0,0);
end;

procedure TDialogBox.EditExit(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    fDlgProc(FSelf, PAnsiChar((Sender as TControl).Name), DN_KILLFOCUS,0,0);
end;

procedure TDialogBox.EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Assigned(fDlgProc) then
    fDlgProc(FSelf, PAnsiChar((Sender as TControl).Name), DN_KEYDOWN, PtrInt(@Key), Integer(Shift));
end;

procedure TDialogBox.EditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Assigned(fDlgProc) then
    fDlgProc(FSelf, PAnsiChar((Sender as TControl).Name), DN_KEYUP, PtrInt(@Key), Integer(Shift));
end;

procedure TDialogBox.ListBoxClick(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    begin
      fDlgProc(FSelf, PAnsiChar((Sender as TControl).Name), DN_CLICK, PtrInt((Sender as TListBox).ItemIndex),0);
    end;
end;

procedure TDialogBox.ListBoxDblClick(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    begin
      fDlgProc(FSelf, PAnsiChar((Sender as TControl).Name), DN_DBLCLICK, PtrInt((Sender as TListBox).ItemIndex),0);
    end;
end;

procedure TDialogBox.ListBoxSelectionChange(Sender: TObject; User: boolean);
begin
  if Assigned(fDlgProc) then
    begin
      fDlgProc(FSelf, PAnsiChar((Sender as TControl).Name), DN_CHANGE, PtrInt((Sender as TListBox).ItemIndex),0);
    end;
end;

procedure TDialogBox.ListBoxEnter(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    fDlgProc(FSelf, PAnsiChar((Sender as TControl).Name), DN_GOTFOCUS,0,0);
end;

procedure TDialogBox.ListBoxExit(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    fDlgProc(FSelf, PAnsiChar((Sender as TControl).Name), DN_KILLFOCUS,0,0);
end;

procedure TDialogBox.ListBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(fDlgProc) then
    fDlgProc(FSelf, PAnsiChar((Sender as TControl).Name), DN_KEYDOWN, PtrInt(@Key), Integer(Shift));
end;

procedure TDialogBox.ListBoxKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(fDlgProc) then
    fDlgProc(FSelf, PAnsiChar((Sender as TControl).Name), DN_KEYUP, PtrInt(@Key), Integer(Shift));
end;

procedure TDialogBox.CheckBoxChange(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    begin
      fDlgProc(FSelf, PAnsiChar((Sender as TControl).Name), DN_CHANGE, PtrInt((Sender as TCheckBox).Checked),0);
    end;
end;

procedure TDialogBox.TimerTimer(Sender: TObject);
begin
  if Assigned(fDlgProc) then
  begin
    fDlgProc(FSelf, PAnsiChar((Sender as TTimer).Name), DN_TIMER, 0, 0);
  end;
end;

{ TDialogBoxData }

procedure TDialogBoxData.ShowDialogBox;
var
  UserData: Pointer;
  Dialog: TDialogBox;
  TagData: PtrInt absolute UserData;
begin
  Dialog:= TDialogBox.Create(FLRSData, FDlgProc);
  try
    UserData:= FUserData;
    Dialog.Tag:= TagData;
    DialogResult:= (Dialog.ShowModal = mrOK);
  finally
    FreeAndNil(Dialog);
  end;
end;

constructor TDialogBoxData.Create(const LRSData: String; DlgProc: TDlgProc;
  UserData: Pointer);
begin
  inherited Create;
  FLRSData:= LRSData;
  FDlgProc:= DlgProc;
  FUserData:= UserData;
end;

initialization
  RegisterClasses([TTimer, TButton, TBitBtn, TFileNameEdit,
                   TDirectoryEdit, TComboBox, TListBox, TCheckBox,
                   TGroupBox, TLabel, TPanel, TEdit, TMemo, TImage,
                   TSynEdit, TTabSheet, TScrollBox, TRadioGroup,
                   TPageControl, TProgressBar, TDividerBevel]);

end.

