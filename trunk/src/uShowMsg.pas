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
  Forms, Classes;

type
  TMyMsgResult=(mmrOK, mmrNo, mmrYes, mmrCancel, mmrNone,
                mmrAppend, mmrResume, mmrCopyInto, mmrCopyIntoAll,
                mmrOverwrite, mmrOverwriteAll, mmrOverwriteOlder,
                mmrOverwriteSmaller, mmrOverwriteLarger, mmrSkip,
                mmrSkipAll, mmrIgnoreAll, mmrAll, mmrRetry, mmrAbort);

  TMyMsgButton=(msmbOK, msmbNo, msmbYes, msmbCancel, msmbNone,
                msmbAppend, msmbResume, msmbCopyInto, msmbCopyIntoAll,
                msmbOverwrite, msmbOverwriteAll, msmbOverwriteOlder,
                msmbOverwriteSmaller, msmbOverwriteLarger, msmbSkip,
                msmbSkipAll, msmbIgnoreAll, msmbAll, msmbRetry, msmbAbort);


  { TDialogMainThread }

  TDialogMainThread = class
  private
    procedure SyncMsgBox;
    procedure SyncMessageBox;
    procedure SyncInputQuery;
  protected
    FThread: TThread;
    FCaption,
    FMessage,
    FValue: UTF8String;
    FMaskInput: Boolean;
    FFlags: Longint;
    FButtons: array of TMyMsgButton;
    FButDefault,
    FButEscape: TMyMsgButton;
    FInputQueryResult: Boolean;
    FMsgBoxResult: TMyMsgResult;
    FMessageBoxResult: LongInt;
  public
    constructor Create(AThread: TThread);
    destructor Destroy;override;
    function ShowMsgBox(const sMsg: UTF8String; const Buttons: array of TMyMsgButton; ButDefault, ButEscape:TMyMsgButton) : TMyMsgResult;
    function ShowMessageBox(const AText, ACaption: UTF8String; Flags: LongInt): LongInt;
    function ShowInputQuery(const ACaption, APrompt: UTF8String; MaskInput: Boolean; var Value: UTF8String) : Boolean;
  end;

function msgYesNo(const sMsg: UTF8String): Boolean; overload;
function msgYesNo(Thread: TThread; const sMsg: UTF8String): Boolean; overload;

function msgYesNoCancel(const sMsg: UTF8String): TMyMsgResult; overload;
function msgYesNoCancel(Thread: TThread; const sMsg: UTF8String): TMyMsgResult; overload;

procedure msgOK(const sMsg: UTF8String); overload;
procedure msgOK(Thread: TThread; const sMsg: UTF8String); overload;

procedure msgWarning(const sMsg: UTF8String); overload;
procedure msgWarning(Thread: TThread; const sMsg: UTF8String); overload;

procedure msgError(const sMsg: UTF8String); overload;
procedure msgError(Thread: TThread; const sMsg: UTF8String); overload;

function MsgBox(const sMsg: UTF8String; const Buttons: array of TMyMsgButton; ButDefault, ButEscape: TMyMsgButton): TMyMsgResult; overload;
function MsgBox(Thread: TThread; const sMsg: UTF8String; const Buttons: array of TMyMsgButton; ButDefault, ButEscape: TMyMsgButton): TMyMsgResult; overload;

function MsgTest:TMyMsgResult;

function ShowMessageBox(const AText, ACaption: UTF8String; Flags: LongInt): LongInt; overload;
function ShowMessageBox(Thread: TThread; const AText, ACaption: UTF8String; Flags: LongInt): LongInt; overload;

function ShowInputQuery(const ACaption, APrompt: UTF8String; MaskInput: Boolean; var Value: UTF8String): Boolean; overload;
function ShowInputQuery(Thread: TThread; const ACaption, APrompt: UTF8String; MaskInput: Boolean; var Value: UTF8String): Boolean; overload;

function ShowInputQuery(const ACaption, APrompt: UTF8String; var Value: UTF8String): Boolean; overload;
function ShowInputQuery(Thread: TThread; const ACaption, APrompt: UTF8String; var Value: UTF8String): Boolean; overload;

function ShowInputComboBox(const sCaption, sPrompt : UTF8String; slValueList : TStringList; var sValue : UTF8String) : Boolean;

procedure msgLoadLng;

implementation

uses
  LCLIntf, SysUtils, StdCtrls, Graphics, Math, typinfo, Menus,
  fMsg, uLng, Buttons, Controls, uLog, uGlobs, uDebug;

const
  cMsgName = 'Double Commander';

var
  cLngButton: array[TMyMsgButton] of UTF8String;

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

constructor TDialogMainThread.Create(AThread : TThread);
begin
  FThread:= AThread;
end;

destructor TDialogMainThread.Destroy;
begin
  FButtons:= nil;
  inherited Destroy;
end;

function TDialogMainThread.ShowMsgBox(const sMsg: UTF8String;
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

function TDialogMainThread.ShowMessageBox(const AText, ACaption: UTF8String; Flags: LongInt): LongInt;
begin
  FCaption:= ACaption;
  FMessage:= AText;
  FFlags:= Flags;

  TThread.Synchronize(FThread, SyncMessageBox);

  Result:= FMessageBoxResult;
end;

function TDialogMainThread.ShowInputQuery(const ACaption, APrompt: UTF8String;
  MaskInput: Boolean; var Value: UTF8String): Boolean;
begin
  FCaption:= ACaption;
  FMessage:= APrompt;
  FMaskInput:= MaskInput;
  FValue:= Value;

  TThread.Synchronize(FThread, SyncInputQuery);

  Value:= FValue;
  Result:= FInputQueryResult;
end;

procedure SetMsgBoxParams(var frmMsg: TfrmMsg; const sMsg: UTF8String;
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
      Tag:= iIndex;
      OnClick:= frmMsg.ButtonClick;
      OnMouseUp:= frmMsg.MouseUpEvent;
      if Buttons[iIndex] = ButDefault then
        Default:= True;
      if Buttons[iIndex] = ButEscape then
        frmMsg.Escape:= iIndex;
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
        Tag:= iIndex;
        Caption:= cLngButton[Buttons[iIndex]];
        OnClick:= frmMsg.ButtonClick;
        frmMsg.mnuOther.Items.Add(MenuItem);
      end;
    end;
  end;
end;

function MsgBox(const sMsg: UTF8String; const Buttons: array of TMyMsgButton; ButDefault, ButEscape:TMyMsgButton):TMyMsgResult;
var
  frmMsg:TfrmMsg;
begin
  frmMsg:=TfrmMsg.Create(Application);
  try
  
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
  end;
end;

function MsgBox(Thread: TThread; const sMsg: UTF8String;
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

function msgYesNo(const sMsg: UTF8String):Boolean;
begin
  Result:= MsgBox(sMsg,[msmbYes, msmbNo], msmbYes, msmbNo )= mmrYes;
end;

function msgYesNo(Thread: TThread; const sMsg: UTF8String): Boolean;
begin
  Result:= MsgBox(Thread, sMsg,[msmbYes, msmbNo], msmbYes, msmbNo )= mmrYes;
end;

function msgYesNoCancel(const sMsg: UTF8String):TMyMsgResult;
begin
  Result:= MsgBox(sMsg,[msmbYes, msmbNo, msmbCancel], msmbYes, msmbCancel);
end;

function msgYesNoCancel(Thread: TThread; const sMsg: UTF8String): TMyMsgResult;
begin
  Result:= MsgBox(Thread, sMsg,[msmbYes, msmbNo, msmbCancel], msmbYes, msmbCancel);
end;

procedure msgOK(const sMsg: UTF8String);
begin
  MsgBox(sMsg,[msmbOK],msmbOK, msmbOK);
end;

procedure msgOK(Thread: TThread; const sMsg: UTF8String);
begin
  MsgBox(Thread, sMsg,[msmbOK],msmbOK, msmbOK);
end;

procedure msgError(const sMsg: UTF8String);
begin
  MsgBox(sMsg,[msmbOK],msmbOK, msmbOK);
end;

procedure msgError(Thread: TThread; const sMsg: UTF8String);
begin
  MsgBox(Thread, sMsg,[msmbOK],msmbOK, msmbOK)
end;

procedure msgWarning(const sMsg: UTF8String);
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

procedure msgWarning(Thread: TThread; const sMsg: UTF8String);
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

function ShowMessageBox(const AText, ACaption: UTF8String; Flags: LongInt): LongInt;
begin
  Result:= ShowMessageBox(nil, AText, ACaption, Flags);
end;

function ShowMessageBox(Thread: TThread; const AText, ACaption: UTF8String;
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

function ShowInputQuery(const ACaption, APrompt: UTF8String;
  MaskInput: Boolean; var Value: UTF8String): Boolean; overload;
begin
  Result:= ShowInputQuery(nil, ACaption, APrompt, MaskInput, Value);
end;

function ShowInputQuery(Thread: TThread; const ACaption, APrompt: UTF8String;
  MaskInput: Boolean; var Value: UTF8String): Boolean;
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

function ShowInputQuery(const ACaption, APrompt: UTF8String;
  var Value: UTF8String): Boolean; overload;
begin
  Result:= ShowInputQuery(nil, ACaption, APrompt, False, Value);
end;

function ShowInputQuery(Thread: TThread; const ACaption, APrompt: UTF8String;
  var Value: UTF8String): Boolean;
begin
  Result:= ShowInputQuery(Thread, ACaption, APrompt, False, Value);
end;

function ShowInputComboBox(const sCaption, sPrompt : UTF8String; slValueList : TStringList;
                           var sValue : UTF8String) : Boolean;
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
  cLngButton[msmbSkip]             := rsDlgButtonSkip;
  cLngButton[msmbSkipAll]          := rsDlgButtonSkipAll;
  cLngButton[msmbIgnoreAll]        := rsDlgButtonIgnoreAll;
  cLngButton[msmbAll]              := rsDlgButtonAll;
  cLngButton[msmbRetry]            := rsDlgButtonRetry;
  cLngButton[msmbAbort]            := rsDlgButtonAbort;

  for I:= Low(TMyMsgButton) to High(TMyMsgButton) do
  begin
    // A reminder in case someone forgots to assign text.
    if cLngButton[I] = EmptyStr then
      DCDebug('Warning: MsgBox button ' + GetEnumName(TypeInfo(TMyMsgButton), Integer(I)) + ' caption not set.');
  end;
end;

end.
