unit uFileSourceOperationUI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type

  TFileSourceOperationUIResponse =
    (fsourInvalid,
     fsourNo,
     fsourYes,
     fsourCancel,
     fsourNone,
     fsourAppend,
     fsourRewrite,
     fsourRewriteAll,
     fsourSkip,
     fsourSkipAll,
     fsourAll);
     //fsourAbort

  {en
     General interface for communication: operation <-> user.
  }
  TFileSourceOperationUI = class
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function AskQuestion(Msg: String; Question: String;
                         PossibleResponses: array of TFileSourceOperationUIResponse;
                         DefaultOKResponse: TFileSourceOperationUIResponse;
                         DefaultCancelResponse: TFileSourceOperationUIResponse
                        ) : TFileSourceOperationUIResponse; virtual abstract;

  end;

implementation

constructor TFileSourceOperationUI.Create;
begin
  inherited;
end;

destructor TFileSourceOperationUI.Destroy;
begin
  inherited;
end;

end.

