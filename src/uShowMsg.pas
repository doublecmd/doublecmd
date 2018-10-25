
{
   Double commander
   -------------------------------------------------------------------------
   Implementing of Showing messages with localization

   Copyright (C) 2007-2018 Alexander Koblov (alexx2000@mail.ru)

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
}

{
   Seksi Commander
   ----------------------------
   Implementing of Showing messages with localization

   Licence  : GNU GPL v 2.0
   Author   : radek.cervinka@centrum.cz

   contributors:

   Koblov Alexander (Alexx2000@mail.ru)
}

unit uShowMsg;

{$mode delphi}{$H+}

interface

uses
  Forms, Classes, DCBasicTypes;

type
  TMyMsgResult=(mmrOK, mmrNo, mmrYes, mmrCancel, mmrNone,
                mmrAppend, mmrResume, mmrCopyInto, mmrCopyIntoAll,
                mmrOverwrite, mmrOverwriteAll, mmrOverwriteOlder,
                mmrOverwriteSmaller, mmrOverwriteLarger, mmrAutoRenameSource, mmrRenameSource,
                mmrSkip, mmrSkipAll, mmrIgnore, mmrIgnoreAll, mmrAll, mmrRetry, mmrAbort, mmrRetryAdmin,
                mmrUnlock);

  TMyMsgButton=(msmbOK, msmbNo, msmbYes, msmbCancel, msmbNone,
                msmbAppend, msmbResume, msmbCopyInto, msmbCopyIntoAll,
                msmbOverwrite, msmbOverwriteAll, msmbOverwriteOlder,
                msmbOverwriteSmaller, msmbOverwriteLarger, msmbAutoRenameSource, msmbRenameSource,
                msmbSkip, msmbSkipAll, msmbIgnore, msmbIgnoreAll, msmbAll, msmbRetry, msmbAbort, msmbRetryAdmin,
                msmbUnlock,
                // Actions, they do not close the form and therefore have no corresponding result value:
                msmbCompare);

  TMyMsgActionButton = msmbCompare..High(TMyMsgButton);

  TMyMsgActionHandler = procedure(Button: TMyMsgActionButton) of object;

  { TDialogMainThread }

  TDialogMainThread = class
  private
    procedure SyncMsgBox;
    procedure SyncMessageBox;
    procedure SyncInputQuery;
    procedure SyncMessageChoiceBox;
  protected
    FThread: TThread;
    FCaption,
    FMessage,
    FValue: String;
    FMaskInput: Boolean;
    FFlags: Longint;
    FButtons: array of TMyMsgButton;
    FButDefault,
    FButEscape: TMyMsgButton;
    FInputQueryResult: Boolean;
    FMsgBoxResult: TMyMsgResult;
    FMessageBoxResult: LongInt;
    FChoices: TDynamicStringArray;
  public
    constructor Create(AThread: TThread);
    destructor Destroy;override;
    function ShowMsgBox(const sMsg: String; const Buttons: array of TMyMsgButton; ButDefault, ButEscape:TMyMsgButton) : TMyMsgResult;
    function ShowMessageBox(const AText, ACaption: String; Flags: LongInt): LongInt;
    function ShowMessageChoiceBox(const Message: String; Buttons: TDynamicStringArray): Integer;
    function ShowInputQuery(const ACaption, APrompt: String; MaskInput: Boolean; var Value: String) : Boolean;
  end;

function msgYesNo(const sMsg: String): Boolean; overload;
function msgYesNo(Thread: TThread; const sMsg: String): Boolean; overload;

function msgYesNoCancel(const sMsg: String): TMyMsgResult; overload;
function msgYesNoCancel(Thread: TThread; const sMsg: String): TMyMsgResult; overload;

procedure msgOK(const sMsg: String); overload;
procedure msgOK(Thread: TThread; const sMsg: String); overload;

procedure msgWarning(const sMsg: String); overload;
procedure msgWarning(Thread: TThread; const sMsg: String); overload;

procedure msgError(const sMsg: String); overload;
procedure msgError(Thread: TThread; const sMsg: String); overload;

function MsgBox(const sMsg: String; const Buttons: array of TMyMsgButton; ButDefault, ButEscape: TMyMsgButton; ActionHandler: TMyMsgActionHandler = nil): TMyMsgResult; overload;
function MsgBox(Thread: TThread; const sMsg: String; const Buttons: array of TMyMsgButton; ButDefault, ButEscape: TMyMsgButton): TMyMsgResult; overload;

function MsgTest:TMyMsgResult;

function MsgChoiceBox(const Message: String; Buttons: TDynamicStringArray): Integer; overload;
function MsgChoiceBox(Thread: TThread; const Message: String; Buttons: TDynamicStringArray): Integer; overload;

function ShowMessageBox(const AText, ACaption: String; Flags: LongInt): LongInt; overload;
function ShowMessageBox(Thread: TThread; const AText, ACaption: String; Flags: LongInt): LongInt; overload;

function ShowInputQuery(const ACaption, APrompt: String; MaskInput: Boolean; var Value: String): Boolean; overload;
function ShowInputQuery(Thread: TThread; const ACaption, APrompt: String; MaskInput: Boolean; var Value: String): Boolean; overload;

function ShowInputQuery(const ACaption, APrompt: String; var Value: String): Boolean; overload;
function ShowInputQuery(Thread: TThread; const ACaption, APrompt: String; var Value: String): Boolean; overload;

function ShowInputComboBox(const sCaption, sPrompt : String; slValueList : TStringList; var sValue : String) : Boolean;
function ShowInputListBox(const sCaption, sPrompt : String; slValueList : TStringList; var sValue : String; var SelectedChoice:integer) : Boolean;
function ShowInputMultiSelectListBox(const sCaption, sPrompt : String; slValueList, slOutputIndexSelected : TStringList) : Boolean;

procedure msgLoadLng;

implementation

uses
  LCLIntf, SysUtils, StdCtrls, Graphics, Math, typinfo, Menus,
  fMsg, uLng, Buttons, Controls, uLog, uGlobs, uDebug;

const
  cMsgName = 'Double Commander';

var
  cLngButton: array[TMyMsgButton] of String;

{ TDialogMainThread }

procedure TDialogMainThread.SyncMsgBox;
begin
  FMsgBoxResult:= MsgBox(FMessage, FButtons, FButDefault, FButEscape);
end;

procedure TDialogMainThread.SyncMessageBox;
begin
  FMessageBoxResult:= MessageBoxFunction(PAnsiChar(FMessage), PAnsiChar(FCaption), FFlags);
end;

procedure TDialogMainThread.SyncInputQuery;
begin
  FInputQueryResult := LCLIntf.RequestInput(FCaption, FMessage, FMaskInput, FValue);
end;

procedure TDialogMainThread.SyncMessageChoiceBox;
begin
  FMessageBoxResult:= MsgChoiceBox(FMessage, FChoices);
end;

constructor TDialogMainThread.Create(AThread : TThread);
begin
  FThread:= AThread;
end;

destructor TDialogMainThread.Destroy;
begin
  FButtons:= nil;
  inherited Destroy;
end;

function TDialogMainThread.ShowMsgBox(const sMsg: String;
                           const Buttons: array of TMyMsgButton; ButDefault,
                           ButEscape: TMyMsgButton) : TMyMsgResult;
var
  I : Integer;
begin
  FMessage := sMsg;

  SetLength(FButtons, Length(Buttons));
  for I := Low(Buttons) to High(Buttons) do
    FButtons[I] := Buttons[I];

  FButDefault := ButDefault;
  FButEscape := ButEscape;

  TThread.Synchronize(FThread, SyncMsgBox);

  Result := FMsgBoxResult;
end;

function TDialogMainThread.ShowMessageBox(const AText, ACaption: String; Flags: LongInt): LongInt;
begin
  FCaption:= ACaption;
  FMessage:= AText;
  FFlags:= Flags;

  TThread.Synchronize(FThread, SyncMessageBox);

  Result:= FMessageBoxResult;
end;

function TDialogMainThread.ShowMessageChoiceBox(const Message: String;
  Buttons: TDynamicStringArray): Integer;
begin
  FMessage:= Message;
  FChoices:= Buttons;

  TThread.Synchronize(FThread, SyncMessageChoiceBox);

  Result:= FMessageBoxResult;
end;

function TDialogMainThread.ShowInputQuery(const ACaption, APrompt: String;
  MaskInput: Boolean; var Value: String): Boolean;
begin
  FCaption:= ACaption;
  FMessage:= APrompt;
  FMaskInput:= MaskInput;
  FValue:= Value;

  TThread.Synchronize(FThread, SyncInputQuery);

  Value:= FValue;
  Result:= FInputQueryResult;
end;

procedure SetMsgBoxParams(var frmMsg: TfrmMsg; const sMsg: String;
                          const Buttons: array of TMyMsgButton; ButDefault, ButEscape: TMyMsgButton);
const
  cButtonCount = 8;
  cButtonSpace = 8;
var
  iIndex: Integer;
  iCount: Integer;
  MenuItem: TMenuItem;
  CaptionWidth: Integer;
  More: Boolean = False;
  MinButtonWidth: Integer;
  iIndexDefault : Integer = -1;
begin
  Assert(Assigned(frmMsg));
  frmMsg.Position:= poScreenCenter;
  frmMsg.BorderStyle:= bsSingle;
  frmMsg.BorderIcons:= [biSystemMenu];

  frmMsg.Caption:= cMsgName;
  frmMsg.lblMsg.Caption:= sMsg;

  // Get default button width
  with TButton.Create(nil) do
  begin
    MinButtonWidth:= GetDefaultWidth;
    Free;
  end;

  // Determine number of buttons
  iCount:= High(Buttons);
  if iCount > cButtonCount then
  begin
    More:= True;
    iCount:= cButtonCount - 1;
    CaptionWidth:= frmMsg.Canvas.TextWidth(rsDlgButtonOther);
    if CaptionWidth >= (MinButtonWidth - cButtonSpace) then
      MinButtonWidth:= CaptionWidth + cButtonSpace;
  end;

  // Calculate minimum button width
  for iIndex:= Low(Buttons) to iCount do
  begin
    CaptionWidth:= frmMsg.Canvas.TextWidth(cLngButton[Buttons[iIndex]]);
    if CaptionWidth >= (MinButtonWidth - cButtonSpace) then
      MinButtonWidth:= CaptionWidth + cButtonSpace;
  end;

  // Add first 9 items as buttons
  for iIndex:= Low(Buttons) to iCount do
  begin
    with TButton.Create(frmMsg) do
    begin
      AutoSize:= True;
      Caption:= cLngButton[Buttons[iIndex]];
      Parent:= frmMsg.pnlButtons;
      Constraints.MinWidth:= MinButtonWidth;
      if Buttons[iIndex] >= Low(TMyMsgActionButton) then
        Tag:= -2-iIndex
      else
        Tag:= iIndex;
      OnClick:= frmMsg.ButtonClick;
      OnMouseUp:= frmMsg.MouseUpEvent;
      if Buttons[iIndex] = ButDefault then
      begin
        Default:= True;
        iIndexDefault:=iIndex;
      end;
      if Buttons[iIndex] = ButEscape then
        frmMsg.Escape:= iIndex;
    end;
  end;

  //Once the buttons has been added, let's set the correct "TabOrder" in such way:
  //1o) The one with the default is "TabOrder=0"
  //2o) If we press "TAB" key, it keeps moving to the right
  //Let's determine what should be the "TabOrder" initial value so
  //1. The default button will have tab order 0
  //2. When moving with "tab" key, it will move from left to right
  //"TabOrder" need to be set *after* all the buttons are there
  if iIndexDefault<>-1 then
  begin
    for iIndex:= 0 to pred(frmMsg.ComponentCount) do
    begin
      if frmMsg.Components[iIndex] is TButton then
        with frmMsg.Components[iIndex] as TButton do
        begin
          if Tag >= 0 then
            TabOrder:= (Tag+(iCount+1)-iIndexDefault) mod (iCount+1) //Tricky but it does it, no "if", no negative after to check, etc.
          else
            TabOrder:= (-2-Tag+(iCount+1)-iIndexDefault) mod (iCount+1);
        end;
    end;
  end;

  // More add as popup menu
  if More then
  begin
    // Add button with popup menu
    with TButton.Create(frmMsg) do
    begin
      AutoSize:= True;
      Caption:= rsDlgButtonOther;
      Parent:= frmMsg.pnlButtons;
      Constraints.MinWidth:= MinButtonWidth;
      OnClick:= frmMsg.ButtonOtherClick;
    end;
    // Fill popup menu
    for iIndex:= cButtonCount to High(Buttons) do
    begin
      MenuItem:= TMenuItem.Create(frmMsg.mnuOther);
      with MenuItem do
      begin
        if Buttons[iIndex] >= Low(TMyMsgActionButton) then
          Tag:= -2-iIndex
        else
          Tag:= iIndex;
        Caption:= cLngButton[Buttons[iIndex]];
        OnClick:= frmMsg.ButtonClick;
        frmMsg.mnuOther.Items.Add(MenuItem);
      end;
    end;
  end;
end;

type TMsgBoxHelper = class
  Buttons: array of TMyMsgButton;
  ActionHandler: TMyMsgActionHandler;
  procedure MsgBoxActionHandler(Tag: PtrInt);
end;

procedure TMsgBoxHelper.MsgBoxActionHandler(Tag: PtrInt);
begin
  ActionHandler(Buttons[-Tag-2]);
end;

function MsgBox(const sMsg: String; const Buttons: array of TMyMsgButton; ButDefault, ButEscape: TMyMsgButton; ActionHandler: TMyMsgActionHandler = nil): TMyMsgResult;
var
  frmMsg:TfrmMsg;
  MsgBoxHelper: TMsgBoxHelper = nil;
  I: Integer;
begin
  frmMsg:=TfrmMsg.Create(Application);
  try
    MsgBoxHelper := TMsgBoxHelper.Create();
    SetLength(MsgBoxHelper.Buttons, Length(Buttons));
    for I := Low(Buttons) to High(Buttons) do
      MsgBoxHelper.Buttons[I] := Buttons[I];
    MsgBoxHelper.ActionHandler := ActionHandler;
    frmMsg.ActionHandler := MsgBoxHelper.MsgBoxActionHandler;
  
    SetMsgBoxParams(frmMsg, sMsg, Buttons, ButDefault, ButEscape);
  
    frmMsg.ShowModal;
    if (frmMsg.iSelected)=-1 then
      Result:=mmrNone
    else
      { TODO : not safe code because of direct typecast from one enumeration to another,
               better to use array lookup }
      Result:=TMyMsgResult(Buttons[frmMsg.iSelected]);
  finally
    frmMsg.Free;
    MsgBoxHelper.Free;
  end;
end;

function MsgBox(Thread: TThread; const sMsg: String;
                         const Buttons: array of TMyMsgButton; ButDefault,
                         ButEscape: TMyMsgButton): TMyMsgResult;
var
  DialogMainThread : TDialogMainThread;
begin
  Result := mmrNone;
  DialogMainThread := TDialogMainThread.Create(Thread);
  try
    Result := DialogMainThread.ShowMsgBox(sMsg, Buttons, ButDefault, ButEscape);
  finally
    DialogMainThread.Free;
  end;
end;

Function MsgTest:TMyMsgResult;
begin
  Result:= MsgBox('test language of msg subsystem'#10'Second line',[msmbOK, msmbNO, msmbYes, msmbCancel, msmbNone,
                       msmbAppend, msmbOverwrite, msmbOverwriteAll],msmbOK, msmbNO);
end;

function msgYesNo(const sMsg: String):Boolean;
begin
  Result:= MsgBox(nil, sMsg,[msmbYes, msmbNo], msmbYes, msmbNo )= mmrYes;
end;

function msgYesNo(Thread: TThread; const sMsg: String): Boolean;
begin
  Result:= MsgBox(Thread, sMsg,[msmbYes, msmbNo], msmbYes, msmbNo )= mmrYes;
end;

function msgYesNoCancel(const sMsg: String):TMyMsgResult;
begin
  Result:= MsgBox(sMsg,[msmbYes, msmbNo, msmbCancel], msmbYes, msmbCancel);
end;

function msgYesNoCancel(Thread: TThread; const sMsg: String): TMyMsgResult;
begin
  Result:= MsgBox(Thread, sMsg,[msmbYes, msmbNo, msmbCancel], msmbYes, msmbCancel);
end;

procedure msgOK(const sMsg: String);
begin
  MsgBox(sMsg,[msmbOK],msmbOK, msmbOK);
end;

procedure msgOK(Thread: TThread; const sMsg: String);
begin
  MsgBox(Thread, sMsg,[msmbOK],msmbOK, msmbOK);
end;

procedure msgError(const sMsg: String);
begin
  MsgBox(sMsg,[msmbOK],msmbOK, msmbOK);
end;

procedure msgError(Thread: TThread; const sMsg: String);
begin
  MsgBox(Thread, sMsg,[msmbOK],msmbOK, msmbOK)
end;

procedure msgWarning(const sMsg: String);
begin
  if gShowWarningMessages then
    MsgBox(sMsg,[msmbOK],msmbOK, msmbOK)
  else
    begin
      if gLogWindow then // if log window enabled then write error to it
        logWrite(sMsg, lmtError)
      else
        Beep;
    end;
end;

procedure msgWarning(Thread: TThread; const sMsg: String);
begin
  if gShowWarningMessages then
    MsgBox(Thread, sMsg,[msmbOK],msmbOK, msmbOK)
  else
    begin
      if gLogWindow then // if log window enabled then write error to it
        logWrite(Thread, sMsg, lmtError)
      else
        Beep;
    end;
end;

function ShowMessageBox(const AText, ACaption: String; Flags: LongInt): LongInt;
begin
  Result:= ShowMessageBox(nil, AText, ACaption, Flags);
end;

function ShowMessageBox(Thread: TThread; const AText, ACaption: String;
  Flags: LongInt): LongInt;
var
  DialogMainThread : TDialogMainThread;
begin
  Result:= 0;
  DialogMainThread:= TDialogMainThread.Create(Thread);
  try
    Result:= DialogMainThread.ShowMessageBox(AText, ACaption, Flags);
  finally
    DialogMainThread.Free;
  end;
end;

function ShowInputQuery(const ACaption, APrompt: String;
  MaskInput: Boolean; var Value: String): Boolean; overload;
begin
  Result:= ShowInputQuery(nil, ACaption, APrompt, MaskInput, Value);
end;

function ShowInputQuery(Thread: TThread; const ACaption, APrompt: String;
  MaskInput: Boolean; var Value: String): Boolean;
var
  DialogMainThread : TDialogMainThread;
begin
  Result := False;
  DialogMainThread:= TDialogMainThread.Create(Thread);
  try
    Result:= DialogMainThread.ShowInputQuery(ACaption, APrompt, MaskInput, Value);
  finally
    DialogMainThread.Free;
  end;
end;

function ShowInputQuery(const ACaption, APrompt: String;
  var Value: String): Boolean; overload;
begin
  Result:= ShowInputQuery(nil, ACaption, APrompt, False, Value);
end;

function ShowInputQuery(Thread: TThread; const ACaption, APrompt: String;
  var Value: String): Boolean;
begin
  Result:= ShowInputQuery(Thread, ACaption, APrompt, False, Value);
end;

function ShowInputComboBox(const sCaption, sPrompt : String; slValueList : TStringList;
                           var sValue : String) : Boolean;
var
  frmDialog : TForm;
  lblPrompt : TLabel;
  cbValue : TComboBox;
  bbtnOK,
  bbtnCancel : TBitBtn;
begin
  frmDialog := TForm.CreateNew(nil, 0);
  with frmDialog do
    try
      BorderStyle := bsDialog;
      Position := poScreenCenter;
      AutoSize := True;
      Height := 120;
      ChildSizing.TopBottomSpacing := 8;
      ChildSizing.LeftRightSpacing := 8;
      Caption := sCaption;
      lblPrompt := TLabel.Create(frmDialog);
      with lblPrompt do
        begin
          Parent := frmDialog;
          Caption := sPrompt;
          Top := 6;
          Left := 6;
        end;
      cbValue := TComboBox.Create(frmDialog);
      with cbValue do
        begin
          Parent := frmDialog;
          Items.Assign(slValueList);
          Text := sValue;
          Left := 6;
          AnchorToNeighbour(akTop, 6, lblPrompt);
          Constraints.MinWidth := max(280, Screen.Width div 4);
        end;
      bbtnCancel := TBitBtn.Create(frmDialog);
      with bbtnCancel do
        begin
          Parent := frmDialog;
          Kind := bkCancel;
          Cancel := True;
          Left := 6;
          Width:= 90;
          Anchors := [akTop, akRight];
          AnchorToNeighbour(akTop, 18, cbValue);
          AnchorSide[akRight].Control := cbValue;
          AnchorSide[akRight].Side := asrRight;
        end;
      bbtnOK := TBitBtn.Create(frmDialog);
      with bbtnOK do
        begin
          Parent := frmDialog;
          Kind := bkOk;
          Default := True;
          Width:= 90;
          Anchors := [akTop, akRight];
          AnchorToNeighbour(akTop, 18, cbValue);
          AnchorToNeighbour(akRight, 6, bbtnCancel);
        end;
      Result := (ShowModal = mrOK);
      if Result then
        begin
          if slValueList.IndexOf(cbValue.Text) < 0 then
            slValueList.Add(cbValue.Text);
          sValue := cbValue.Text;
        end;
    finally
      FreeAndNil(frmDialog);
    end; // with frmDialog
end;

type
  TProcedureHolder=class(TObject)
    public
      procedure ListBoxDblClick(Sender: TObject);
  end;

procedure TProcedureHolder.ListBoxDblClick(Sender: TObject);
begin
  TForm(TComponent(Sender).Owner).ModalResult:=mrOk;
end;

function InnerShowInputListBox(const sCaption, sPrompt: String; bMultiSelect:boolean; slValueList,slOutputIndexSelected:TStringList; var sValue: String; var SelectedChoice:integer) : Boolean;
var
  frmDialog : TForm;
  lblPrompt : TLabel;
  lbValue : TListBox;
  bbtnOK, bbtnCancel, bbtnSelectAll : TBitBtn;
  iIndex, iModalResult: integer;
  ProcedureHolder:TProcedureHolder;
begin
  SelectedChoice:=-1;
  ProcedureHolder:=TProcedureHolder.Create;
  try
  frmDialog := TForm.CreateNew(nil, 0);
  with frmDialog do
    try
      BorderStyle := bsDialog;
      Position := poScreenCenter;
      AutoSize := True;
      Height := 120;
      ChildSizing.TopBottomSpacing := 8;
      ChildSizing.LeftRightSpacing := 8;
      Caption := sCaption;
      lblPrompt := TLabel.Create(frmDialog);
      with lblPrompt do
        begin
          Parent := frmDialog;
          Caption := sPrompt;
          Top := 6;
          Left := 6;
        end;
      lbValue := TListBox.Create(frmDialog);
      with lbValue do
        begin
          Parent := frmDialog;
          Height := (slValueList.Count*15)+50;
          if height=0 then Height:=150 else
            if height > (screen.Height div 2) then
              height := (Screen.Height div 2);
          Items.Assign(slValueList);
          ItemIndex:=Items.IndexOf(sValue);
          lbValue.MultiSelect:=bMultiSelect;
          if (ItemIndex=-1) AND (Items.count>0) then ItemIndex:=0;
          Left := 6;
          AnchorToNeighbour(akTop, 6, lblPrompt);
          Constraints.MinWidth := max(280, Screen.Width div 4);
          OnDblClick:= ProcedureHolder.ListBoxDblClick;
        end;
      if bMultiSelect then
        begin
          bbtnSelectAll := TBitBtn.Create(frmDialog);
          with bbtnSelectAll do
            begin
              Parent := frmDialog;
              Kind := bkAll;
              Cancel := True;
              Left := 6;
              Width:= 90;
              Anchors := [akTop, akLeft];
              AnchorToNeighbour(akTop, 18, lbValue);
              AnchorSide[akLeft].Control := lbValue;
              AnchorSide[akLeft].Side := asrLeft;
            end;
        end;
      bbtnCancel := TBitBtn.Create(frmDialog);
      with bbtnCancel do
        begin
          Parent := frmDialog;
          Kind := bkCancel;
          Cancel := True;
          Left := 6;
          Width:= 90;
          Anchors := [akTop, akRight];
          AnchorToNeighbour(akTop, 18, lbValue);
          AnchorSide[akRight].Control := lbValue;
          AnchorSide[akRight].Side := asrRight;
        end;
      bbtnOK := TBitBtn.Create(frmDialog);
      with bbtnOK do
        begin
          Parent := frmDialog;
          Kind := bkOk;
          Default := True;
          Width:= 90;
          Anchors := [akTop, akRight];
          AnchorToNeighbour(akTop, 18, lbValue);
          AnchorToNeighbour(akRight, 6, bbtnCancel);
        end;
      iModalResult:=ShowModal;
      Result := (iModalResult = mrOK) AND (lbValue.ItemIndex<>-1);
      if (not Result) AND (bMultiSelect) AND (iModalResult = mrAll) then
        begin
          lbValue.SelectAll;
          Result:=True;
        end;
      if Result then
      begin
        sValue:=lbValue.Items.Strings[lbValue.ItemIndex];
        SelectedChoice:=lbValue.ItemIndex;
        if bMultiSelect then
          for iIndex:=0 to pred(lbValue.Items.count) do
            if lbValue.Selected[iIndex] then
              slOutputIndexSelected.Add(IntToStr(iIndex));
      end;
    finally
      FreeAndNil(frmDialog);
    end; // with frmDialog

  finally
    ProcedureHolder.Free;
  end;
end;

function ShowInputListBox(const sCaption, sPrompt : String; slValueList : TStringList; var sValue : String; var SelectedChoice:integer) : Boolean;
begin
  result := InnerShowInputListBox(sCaption, sPrompt, False, slValueList, nil, sValue, SelectedChoice);
end;

function ShowInputMultiSelectListBox(const sCaption, sPrompt : String; slValueList, slOutputIndexSelected : TStringList) : Boolean;
var
  sDummyValue:string;
  iDummySelectedChoice:integer;
begin
  if slValueList.Count>0 then sDummyValue := slValueList.Strings[0];
  result := InnerShowInputListBox(sCaption, sPrompt, True, slValueList, slOutputIndexSelected, sDummyValue, iDummySelectedChoice);
end;

function MsgChoiceBox(const Message: String; Buttons: TDynamicStringArray): Integer;
const
  cButtonSpace = 8;
var
  Index: Integer;
  frmMsg: TfrmMsg;
  CaptionWidth: Integer;
  MinButtonWidth: Integer;
begin
  frmMsg:= TfrmMsg.Create(Application);
  try
    frmMsg.BorderStyle:= bsSingle;
    frmMsg.Position:= poScreenCenter;
    frmMsg.BorderIcons:= [biSystemMenu];
    frmMsg.Caption:= Application.Title;

    frmMsg.lblMsg.WordWrap:= True;
    frmMsg.lblMsg.Caption:= Message;
    frmMsg.Constraints.MaxWidth:= 600;

    // Get default button width
    with TButton.Create(nil) do
    begin
      MinButtonWidth:= GetDefaultWidth;
      Free;
    end;

    // Calculate minimum button width
    for Index:= Low(Buttons) to High(Buttons) do
    begin
      CaptionWidth:= frmMsg.Canvas.TextWidth(Buttons[Index]);
      if CaptionWidth >= (MinButtonWidth - cButtonSpace) then
        MinButtonWidth:= CaptionWidth + cButtonSpace;
    end;

    // Add all buttons
    for Index:= Low(Buttons) to High(Buttons) do
    begin
      with TButton.Create(frmMsg) do
      begin
        Tag:= Index;
        AutoSize:= True;
        Caption:= Buttons[Index];
        Parent:= frmMsg.pnlButtons;
        OnClick:= frmMsg.ButtonClick;
        Constraints.MinWidth:= MinButtonWidth;
      end;
    end;

    frmMsg.ShowModal;
    Result:= frmMsg.iSelected;

  finally
    frmMsg.Free;
  end;
end;

function MsgChoiceBox(Thread: TThread; const Message: String;
  Buttons: TDynamicStringArray): Integer;
var
  DialogMainThread : TDialogMainThread;
begin
  Result := -1;
  DialogMainThread:= TDialogMainThread.Create(Thread);
  try
    Result:= DialogMainThread.ShowMessageChoiceBox(Message, Buttons);
  finally
    DialogMainThread.Free;
  end;
end;

procedure msgLoadLng;
var
  I: TMyMsgButton;
begin
  cLngButton[msmbOK]               := rsDlgButtonOK;
  cLngButton[msmbNo]               := rsDlgButtonNo;
  cLngButton[msmbYes]              := rsDlgButtonYes;
  cLngButton[msmbCancel]           := rsDlgButtonCancel;
  cLngButton[msmbNone]             := rsDlgButtonNone;
  cLngButton[msmbAppend]           := rsDlgButtonAppend;
  cLngButton[msmbResume]           := rsDlgButtonResume;
  cLngButton[msmbCopyInto]         := rsDlgButtonCopyInto;
  cLngButton[msmbCopyIntoAll]      := rsDlgButtonCopyIntoAll;
  cLngButton[msmbOverwrite]        := rsDlgButtonOverwrite;
  cLngButton[msmbOverwriteAll]     := rsDlgButtonOverwriteAll;
  cLngButton[msmbOverwriteOlder]   := rsDlgButtonOverwriteOlder;
  cLngButton[msmbOverwriteSmaller] := rsDlgButtonOverwriteSmaller;
  cLngButton[msmbOverwriteLarger]  := rsDlgButtonOverwriteLarger;
  cLngButton[msmbAutoRenameSource] := rsDlgButtonAutoRenameSource;
  cLngButton[msmbRenameSource]     := rsDlgButtonRename;
  cLngButton[msmbSkip]             := rsDlgButtonSkip;
  cLngButton[msmbSkipAll]          := rsDlgButtonSkipAll;
  cLngButton[msmbIgnore]           := rsDlgButtonIgnore;
  cLngButton[msmbIgnoreAll]        := rsDlgButtonIgnoreAll;
  cLngButton[msmbAll]              := rsDlgButtonAll;
  cLngButton[msmbRetry]            := rsDlgButtonRetry;
  cLngButton[msmbAbort]            := rsDlgButtonAbort;
  cLngButton[msmbRetryAdmin]       := rsDlgButtonRetryAdmin;
  cLngButton[msmbUnlock]           := rsDlgButtonUnlock;
  cLngButton[msmbCompare]          := rsDlgButtonCompare;

  for I:= Low(TMyMsgButton) to High(TMyMsgButton) do
  begin
    // A reminder in case someone forgots to assign text.
    if cLngButton[I] = EmptyStr then
      DCDebug('Warning: MsgBox button ' + GetEnumName(TypeInfo(TMyMsgButton), Integer(I)) + ' caption not set.');
  end;
end;

end.
