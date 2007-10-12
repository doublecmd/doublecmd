{
Seksi Commander
----------------------------
Implementing of Showing messages with lokalization

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

function msgYesNo(const sMsg:String):Boolean;
function msgYesNoCancel(const sMsg:String):TMyMsgResult;
procedure msgOK(const sMsg:String);

function msgWarning(const sMsg:String):Boolean;
procedure msgError(const sMsg:String);

function MsgBox(const sMsg:String; const Buttons: array of TMyMsgButton; ButDefault, ButEscape:TMyMsgButton):TMyMsgResult;
function MsgBoxForThread(Thread : TThread;const sMsg:String; const Buttons: array of TMyMsgButton; ButDefault, ButEscape:TMyMsgButton):TMyMsgResult;

function MsgTest:TMyMsgResult;
procedure msgLoadLng;


implementation
uses
  SysUtils, StdCtrls, Graphics, fMsg, uLng, Buttons, Controls;

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
    frmMsg.Width:=(cButtonWith+cButtonSpace)*3+cButtonSpace
  else
    frmMsg.Width:=(cButtonWith+cButtonSpace)*(High(Buttons)+1)+cButtonSpace;
  frmMsg.Height:=(High(Buttons) div 3)*40+80;


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
        Width:=cButtonWith;
        Height := 32;
        Tag:=iIndex;
        OnCLick:=frmMsg.ButtonClick;
        if (High(Buttons)+1)>=3 then
          Left:=(iIndex mod 3)*(cButtonWith+cButtonSpace)+(frmMsg.Width-(3*cButtonWith+2*cButtonSpace)) div 2
        else
          Left:=iIndex*(cButtonWith+cButtonSpace)+(frmMsg.Width-((High(Buttons)+1)*cButtonWith+High(Buttons)*cButtonSpace)) div 2;

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

function MsgBoxForThread(Thread: TThread; const sMsg: String;
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

function msgYesNoCancel(const sMsg:String):TMyMsgResult;
begin
  Result:= MsgBox(sMsg,[msmbYes, msmbNo, msmbCancel], msmbYes, msmbNo);
end;

procedure msgOK(const sMsg:String);
begin
  MsgBox(sMsg,[msmbOK],msmbOK, msmbOK);
end;

procedure msgError(const sMsg:String);
begin
  MsgBox(sMsg,[msmbOK],msmbOK, msmbOK);
end;

function msgWarning(const sMsg:String):Boolean;
begin
  Raise Exception.Create('Not implemented yet!');
end;

procedure msgLoadLng;
var
  i:TMyMsgButton;
  s:String;
  xPos:Integer;

begin
  s:=lngGetString(clngDlgButtons);
  for i:= Low(TMyMsgButton) to High(TMyMsgButton) do
  begin
    xPos:=Pos(';',s);
    cLngButton[i]:=Copy(s,1,xPos-1);
    Delete(s,1,xPos);
  end;
end;

end.
