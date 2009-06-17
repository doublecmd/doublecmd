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

{$MODE Delphi}{$H+}

interface
uses
  Forms, Classes;

type
  TMyMsgResult=(mmrOK, mmrNo, mmrYes, mmrCancel, mmrNone,
                mmrAppend, mmrRewrite, mmrRewriteAll, mmrSkip, mmrSkipAll, mmrAll );

  TMyMsgButton=(msmbOK, msmbNO, msmbYes, msmbCancel, msmbNone,
                msmbAppend, msmbRewrite, msmbRewriteAll, msmbSkip, msmbSkipAll, msmbAll);


  { TDlgOpThread }

  TDlgOpThread = class
  private
    procedure ShowInTheThread;
  protected
    FThread : TThread;
    FMsg : String;
    FButtons: array of TMyMsgButton;
    FButDefault,
    FButEscape : TMyMsgButton;
    FDlgResult : TMyMsgResult;
  public
    constructor Create(Thread : TThread);
    destructor Destroy;override;
    function Show(const sMsg:String; const Buttons: array of TMyMsgButton; ButDefault, ButEscape:TMyMsgButton) : TMyMsgResult;
  end;

function msgYesNo(const sMsg:String):Boolean; overload;
function msgYesNo(Thread: TThread; const sMsg:String):Boolean; overload;

function msgYesNoCancel(const sMsg:String):TMyMsgResult; overload;
function msgYesNoCancel(Thread: TThread; const sMsg:String):TMyMsgResult; overload;

procedure msgOK(const sMsg:String); overload;
procedure msgOK(Thread: TThread; const sMsg: String); overload;

function msgWarning(const sMsg: String): Boolean; overload;
function msgWarning(Thread: TThread; const sMsg: String): Boolean; overload;

procedure msgError(const sMsg: String); overload;
procedure msgError(Thread: TThread; const sMsg: String); overload;

function MsgBox(const sMsg: String; const Buttons: array of TMyMsgButton; ButDefault, ButEscape: TMyMsgButton): TMyMsgResult; overload;
function MsgBox(Thread: TThread;const sMsg: String; const Buttons: array of TMyMsgButton; ButDefault, ButEscape: TMyMsgButton): TMyMsgResult; overload;

function MsgTest:TMyMsgResult;
function ShowInputComboBox(const sCaption, sPrompt : String; var slValueList : TStringList;
                           var sValue : String) : Boolean;
procedure msgLoadLng;


implementation
uses
  SysUtils, StdCtrls, Graphics, math, fMsg, uLng, Buttons, Controls, uLog, uGlobs;

const
  cMsgName='Double Commander';

var
  cLngButton:Array[TMyMsgButton] of String;

{ TDlgOpThread }

procedure TDlgOpThread.ShowInTheThread;
begin
  FDlgResult := MsgBox(FMsg, FButtons, FButDefault, FButEscape);
end;

constructor TDlgOpThread.Create(Thread : TThread);
begin
  FThread := Thread;
end;

destructor TDlgOpThread.Destroy;
begin
  FButtons := nil;
  inherited Destroy;
end;

function TDlgOpThread.Show(const sMsg: String;
                           const Buttons: array of TMyMsgButton; ButDefault,
                           ButEscape: TMyMsgButton) : TMyMsgResult;
var
  I : Integer;
begin
  FMsg := sMsg;

  SetLength(FButtons, SizeOf(Buttons));
  for I := Low(Buttons) to High(Buttons) do
    FButtons[I] := Buttons[I];

  FButDefault := ButDefault;
  FButEscape := ButEscape;

  FThread.Synchronize(FThread, ShowInTheThread);

  Result := FDlgResult;
end;

{ This is workaround for autosize}
function MeasureText(Canvas:TCanvas; const sText:String):Integer;
var
  xEnter:Integer;
  x:Integer;
begin
  xEnter:=Pos(#10, sText);
  if xEnter>0 then
    Result:=Canvas.TextWidth(Copy(sText,1, xEnter))
  else
    Result:=Canvas.TextWidth(sText);
end;

procedure SetMsgBoxParams(var frmMsg : TfrmMsg; const sMsg:String;
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
        OnMouseDown:=frmMsg.MouseDownEvent;
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

function MsgBox(const sMsg:String; const Buttons: array of TMyMsgButton; ButDefault, ButEscape:TMyMsgButton):TMyMsgResult;
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

function MsgBox(Thread: TThread; const sMsg: String;
                         const Buttons: array of TMyMsgButton; ButDefault,
                         ButEscape: TMyMsgButton): TMyMsgResult;
var
  DlgOpThread : TDlgOpThread;
begin
  Result := mmrNone;
  try
    DlgOpThread := TDlgOpThread.Create(Thread);
    Result := DlgOpThread.Show(sMsg, Buttons, ButDefault, ButEscape);
  finally
    DlgOpThread.Free;
  end;
end;

Function MsgTest:TMyMsgResult;
begin
  Result:= MsgBox('test language of msg subsystem'#10'Second line',[msmbOK, msmbNO, msmbYes, msmbCancel, msmbNone,
                       msmbAppend, msmbRewrite, msmbRewriteAll],msmbOK, msmbNO);
end;

function msgYesNo(const sMsg:String):Boolean;
begin
  Result:= MsgBox(sMsg,[msmbYes, msmbNo], msmbYes, msmbNo )= mmrYes;
end;

function msgYesNo(Thread: TThread; const sMsg: String): Boolean;
begin
  Result:= MsgBox(Thread, sMsg,[msmbYes, msmbNo], msmbYes, msmbNo )= mmrYes;
end;

function msgYesNoCancel(const sMsg:String):TMyMsgResult;
begin
  Result:= MsgBox(sMsg,[msmbYes, msmbNo, msmbCancel], msmbYes, msmbNo);
end;

function msgYesNoCancel(Thread: TThread; const sMsg: String): TMyMsgResult;
begin
  Result:= MsgBox(Thread, sMsg,[msmbYes, msmbNo, msmbCancel], msmbYes, msmbNo);
end;

procedure msgOK(const sMsg:String);
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

function msgWarning(const sMsg: String): Boolean;
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

function msgWarning(Thread: TThread; const sMsg: String): Boolean;
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

function ShowInputComboBox(const sCaption, sPrompt : String; var slValueList : TStringList;
                           var sValue : String) : Boolean;
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
var
  I: TMyMsgButton;
  s: String;
  xPos: Integer;
begin
  s:= rsDlgButtons;
  for I:= Low(TMyMsgButton) to High(TMyMsgButton) do
  begin
    xPos:=Pos(';',s);
    cLngButton[I]:=Copy(s,1,xPos-1);
    with Application.MainForm.Canvas do
    if TextWidth(cLngButton[I]) >= (cButtonWidth - 8) then
      cButtonWidth:= TextWidth(cLngButton[I]) + 8;
    Delete(s,1,xPos);
  end;
end;

end.
