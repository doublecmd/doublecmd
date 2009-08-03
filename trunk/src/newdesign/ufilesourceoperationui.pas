unit uFileSourceOperationUI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type

  TFileSourceOperationUIResponse =
    (fsourInvalid,
     fsourOk,
     fsourNo,
     fsourYes,
     fsourCancel,
     fsourNone,
     fsourAppend,
     fsourRewrite,
     fsourRewriteAll,
     fsourSkip,
     fsourSkipAll,
     fsourAll,
     fsourRetry,
     fsourAbort);

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
    // Add possibility to display files properties (for example: to compare older - newer)
    // Add general option "remember this choice for all files of this type" (checkbox)
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

