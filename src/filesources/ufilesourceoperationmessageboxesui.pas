unit uFileSourceOperationMessageBoxesUI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceOperationUI,
  uShowMsg;

type

  {en
     We assume here the UI is used only from the GUI thread.
  }

  { TFileSourceOperationMessageBoxesUI }

  TFileSourceOperationMessageBoxesUI = class(TFileSourceOperationUI)
  private
    FUIActionHandler: TFileSourceOperationUIActionHandler;
  protected
    procedure QuestionActionHandler(Button: TMyMsgActionButton);
  public
    constructor Create; override;
    destructor Destroy; override;

    function AskQuestion(Msg: String; Question: String;
                         PossibleResponses: array of TFileSourceOperationUIResponse;
                         DefaultOKResponse: TFileSourceOperationUIResponse;
                         DefaultCancelResponse: TFileSourceOperationUIAnswer;
                         ActionHandler: TFileSourceOperationUIActionHandler = nil
                        ) : TFileSourceOperationUIAnswer; override;
  end;

implementation

const
  ResponseToButton: array[TFileSourceOperationUIResponse] of TMyMsgButton =
    (msmbOK, msmbOK, msmbNo, msmbYes, msmbCancel, msmbNone, msmbAppend, msmbResume,
     msmbCopyInto, msmbCopyIntoAll, msmbOverwrite, msmbOverwriteAll, msmbOverwriteOlder,
     msmbOverwriteSmaller, msmbOverwriteLarger, msmbAutoRenameSource, msmbRenameSource,
     msmbSkip, msmbSkipAll, msmbIgnore, msmbIgnoreAll, msmbAll, msmbRetry, msmbAbort,
     msmbRetryAdmin, msmbUnlock,
     // Actions:
     msmbCompare);

  ResultToResponse: array[TMyMsgResult] of TFileSourceOperationUIResponse =
    (fsourOk, fsourNo, fsourYes, fsourCancel, fsourNone, fsourAppend, fsourResume,
     fsourCopyInto, fsourCopyIntoAll, fsourOverwrite, fsourOverwriteAll, fsourOverwriteOlder,
     fsourOverwriteSmaller, fsourOverwriteLarger, fsourAutoRenameSource, fsourRenameSource,
     fsourSkip, fsourSkipAll, fsourIgnore, fsourIgnoreAll, fsourAll, fsourRetry, fsourAbort,
     fsourRetryAdmin, fsourUnlock);

  ButtonToUIAction: array[TMyMsgActionButton] of TFileSourceOperationUIAction =
    (fsouaCompare);

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
             DefaultCancelResponse: TFileSourceOperationUIAnswer;
             ActionHandler: TFileSourceOperationUIActionHandler = nil
         ) : TFileSourceOperationUIAnswer;
var
  Buttons: array of TMyMsgButton;
  i: Integer;
  MsgResult: TMyMsgResult;
  TextMessage: String;
begin
  FUIActionHandler := ActionHandler;

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
                      ResponseToButton[DefaultCancelResponse],
                      @QuestionActionHandler);

  Result := ResultToResponse[MsgResult];
end;

procedure TFileSourceOperationMessageBoxesUI.QuestionActionHandler(
  Button: TMyMsgActionButton);
begin
  if Assigned(FUIActionHandler) then
    FUIActionHandler(ButtonToUIAction[Button]);
end;

end.

