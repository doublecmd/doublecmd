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
                mmrAppend, mmrCopyInto, mmrOverwrite, mmrOverwriteAll,
                mmrSkip, mmrSkipAll, mmrAll, mmrRetry, mmrAbort);

  TMyMsgButton=(msmbOK, msmbNo, msmbYes, msmbCancel, msmbNone,
                msmbAppend, msmbCopyInto, msmbOverwrite, msmbOverwriteAll,
                msmbSkip, msmbSkipAll, msmbAll, msmbRetry, msmbAbort);


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
procedure InitDialogButtonWidth;

implementation

uses
  LCLIntf, SysUtils, StdCtrls, Graphics, Math, LCLProc, typinfo,
  fMsg, uLng, Buttons, Controls, uLog, uGlobs;

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

  SetLength(FButtons, SizeOf(Buttons));
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

{ This is workaround for autosize}
function MeasureText(Canvas:TCanvas; const sText: UTF8String):Integer;
var
  xEnter:Integer;
begin
  xEnter:=Pos(#10, sText);
  if xEnter>0 then
    Result:=Canvas.TextWidth(Copy(sText,1, xEnter))
  else
    Result:=Canvas.TextWidth(sText);
end;

procedure SetMsgBoxParams(var frmMsg : TfrmMsg; const sMsg: UTF8String;
                       const Buttons: array of TMyMsgButton; ButDefault, ButEscape:TMyMsgButton);
var
  iIndex:Integer;
begin
  frmMsg.Position:=poScreenCenter;
  frmMsg.BorderStyle := bsSingle;
  frmMsg.BorderIcons := [biSystemMenu, biMinimize];
  
  if (High(Buttons)+1)>=3 then
    frmMsg.Width:=(cButtonWidth+cButtonSpace)*3+cButtonSpace
  else
    frmMsg.Width:=(cButtonWidth+cButtonSpace)*(High(Buttons)+1)+cButtonSpace;
  frmMsg.Height:=(High(Buttons) div 3)*40+90;


    frmMsg.Caption:=cMsgName;
    with frmMsg.lblMsg do
    begin
      Caption:=sMsg;
      Top:=15;
      AutoSize:=True;
//      Anchors:=[akTop];
      Width:=MeasureText(frmMsg.Canvas, sMsg); // workaround
      if Width>frmMsg.Width then
        frmMsg.Width:=Width+2*cButtonSpace;
      Left:=(frmMsg.Width-Width) div 2;
    end;

    for iIndex:=0 to High(Buttons) do
    begin
      With TButton.Create(frmMsg) do
      begin
        Caption:=cLngButton[Buttons[iIndex]];
        Parent:=frmMsg;
        Width:=cButtonWidth;
        Height := 32;
        Tag:=iIndex;
        OnCLick:=frmMsg.ButtonClick;
        OnMouseUp:=frmMsg.MouseUpEvent;
        if (High(Buttons)+1)>=3 then
          Left:=(iIndex mod 3)*(cButtonWidth+cButtonSpace)+(frmMsg.Width-(3*cButtonWidth+2*cButtonSpace)) div 2
        else
          Left:=iIndex*(cButtonWidth+cButtonSpace)+(frmMsg.Width-((High(Buttons)+1)*cButtonWidth+High(Buttons)*cButtonSpace)) div 2;

        Top:=(iIndex div 3)*(Height+5)+50;
        if Buttons[iIndex]=ButDefault then
          Default:=True;
        if Buttons[iIndex]=ButEscape then
          frmMsg.Escape:=iIndex;
{        if iIndex=0 then
          SetFocus;  }
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
  try
    DialogMainThread := TDialogMainThread.Create(Thread);
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
  Result:= MsgBox(sMsg,[msmbYes, msmbNo, msmbCancel], msmbYes, msmbNo);
end;

function msgYesNoCancel(Thread: TThread; const sMsg: UTF8String): TMyMsgResult;
begin
  Result:= MsgBox(Thread, sMsg,[msmbYes, msmbNo, msmbCancel], msmbYes, msmbNo);
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
  try
    DialogMainThread:= TDialogMainThread.Create(Thread);
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
  try
    DialogMainThread:= TDialogMainThread.Create(Thread);
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
  Result := False;
  frmDialog := TForm.CreateNew(nil, 0);
  with frmDialog do
    begin
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
      ShowModal;
      if ModalResult = mrOK then
        begin
          if slValueList.IndexOf(cbValue.Text) < 0 then
            slValueList.Add(cbValue.Text);
          sValue := cbValue.Text;
          Result := True;
        end;
      Free;
    end; // with frmDialog
end;

procedure msgLoadLng;
begin
  cLngButton[msmbOK]           := rsDlgButtonOK;
  cLngButton[msmbNo]           := rsDlgButtonNo;
  cLngButton[msmbYes]          := rsDlgButtonYes;
  cLngButton[msmbCancel]       := rsDlgButtonCancel;
  cLngButton[msmbNone]         := rsDlgButtonNone;
  cLngButton[msmbAppend]       := rsDlgButtonAppend;
  cLngButton[msmbCopyInto]     := rsDlgButtonCopyInto;
  cLngButton[msmbOverwrite]    := rsDlgButtonOverwrite;
  cLngButton[msmbOverwriteAll] := rsDlgButtonOverwriteAll;
  cLngButton[msmbSkip]         := rsDlgButtonSkip;
  cLngButton[msmbSkipAll]      := rsDlgButtonSkipAll;
  cLngButton[msmbAll]          := rsDlgButtonAll;
  cLngButton[msmbRetry]        := rsDlgButtonRetry;
  cLngButton[msmbAbort]        := rsDlgButtonAbort;
end;

procedure InitDialogButtonWidth;
var
  I: TMyMsgButton;
begin
  for I:= Low(TMyMsgButton) to High(TMyMsgButton) do
  begin
    // A reminder in case someone forgots to assign text.
    if cLngButton[I] = EmptyStr then
      DebugLn('Warning: MsgBox button ' + GetEnumName(TypeInfo(TMyMsgButton), Integer(I)) + ' caption not set.');

    with Application.MainForm.Canvas do
    if TextWidth(cLngButton[I]) >= (cButtonWidth - 8) then
      cButtonWidth:= TextWidth(cLngButton[I]) + 8;
  end;
end;

end.
