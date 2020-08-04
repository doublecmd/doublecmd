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
     fsourAppend,      // for files
     fsourResume,      // for files
     fsourCopyInto,    // for directories
     fsourCopyIntoAll, // for directories
     fsourOverwrite,
     fsourOverwriteAll,
     fsourOverwriteOlder,
     fsourOverwriteSmaller,
     fsourOverwriteLarger,
     fsourAutoRenameSource,
     fsourAutoRenameTarget,
     fsourRenameSource,
     fsourSkip,
     fsourSkipAll,
     fsourIgnore,
     fsourIgnoreAll,
     fsourAll,
     fsourRetry,
     fsourAbort,
     fsourRetryAdmin,
     fsourUnlock,
     // Actions will never be returned since they do not close the window, handle them in ActionHandler.
     fsouaCompare); // The first action, hardcoded. Add new actions after this and new answers before this line.

  TFileSourceOperationUIAnswer = Low(TFileSourceOperationUIResponse)..Pred(fsouaCompare);

  TFileSourceOperationUIAction = fsouaCompare..High(TFileSourceOperationUIResponse);

  TFileSourceOperationUIActionHandler = procedure(Action: TFileSourceOperationUIAction) of object;

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
                         DefaultCancelResponse: TFileSourceOperationUIAnswer;
                         ActionHandler: TFileSourceOperationUIActionHandler = nil
                        ) : TFileSourceOperationUIAnswer; virtual abstract;
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

