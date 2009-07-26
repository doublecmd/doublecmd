unit uFileSourceOperationMessageBoxesUI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceOperationUI;

type

  {en
     We assume here the UI is used only from the GUI thread.
  }
  TFileSourceOperationMessageBoxesUI = class(TFileSourceOperationUI)
  public
    constructor Create; override;
    destructor Destroy; override;

    function AskQuestion(Msg: String; Question: String;
                         PossibleResponses: array of TFileSourceOperationUIResponse;
                         DefaultOKResponse: TFileSourceOperationUIResponse;
                         DefaultCancelResponse: TFileSourceOperationUIResponse
                        ) : TFileSourceOperationUIResponse; override;
  end;

implementation

uses
  uShowMsg;

const
  ResponseToButton: array[TFileSourceOperationUIResponse] of TMyMsgButton =
    (msmbOK, msmbNO, msmbYes, msmbCancel, msmbNone, msmbAppend,
     msmbRewrite, msmbRewriteAll, msmbSkip, msmbSkipAll, msmbAll, msmbRetry, msmbAbort);

  ResultToResponse: array[TMyMsgResult] of TFileSourceOperationUIResponse =
    (fsourInvalid, fsourNo, fsourYes, fsourCancel, fsourNone, fsourAppend,
     fsourRewrite, fsourRewriteAll, fsourSkip, fsourSkipAll, fsourAll, fsourRetry, fsourAbort);

constructor TFileSourceOperationMessageBoxesUI.Create;
begin
  inherited;
end;

destructor TFileSourceOperationMessageBoxesUI.Destroy;
begin
  inherited;
end;

function TFileSourceOperationMessageBoxesUI.AskQuestion(
             Msg: String; Question: String;
             PossibleResponses: array of TFileSourceOperationUIResponse;
             DefaultOKResponse: TFileSourceOperationUIResponse;
             DefaultCancelResponse: TFileSourceOperationUIResponse
         ) : TFileSourceOperationUIResponse;
var
  Buttons: array of TMyMsgButton;
  i: Integer;
  MsgResult: TMyMsgResult;
  TextMessage: String;
begin
  SetLength(Buttons, Length(PossibleResponses));
  for i := 0 to Length(PossibleResponses) - 1 do
    Buttons[i] := ResponseToButton[PossibleResponses[i]];

  TextMessage := Msg;
  if (Msg <> '') and (Question <> '') then
    TextMessage := TextMessage { + LineEnding} + ' ';
  TextMessage := TextMessage + Question;

  MsgResult := MsgBox(TextMessage,
                      Buttons,
                      ResponseToButton[DefaultOKResponse],
                      ResponseToButton[DefaultCancelResponse]);

  Result := ResultToResponse[MsgResult];
end;

end.

