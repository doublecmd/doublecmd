unit uFileSourceWatcher;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils
  {$IFDEF DARWIN}
  , uDarwinFSWatch
  {$ENDIF}
  ;

type
  TFSWatchFilter = set of (wfFileNameChange, wfAttributesChange);

  TFSWatcherEventType = (fswFileCreated,
                         fswFileChanged,
                         fswFileDeleted,
                         fswFileRenamed,
                         fswSelfDeleted,
                         fswUnknownChange);
  TFSWatcherEventTypes = set of TFSWatcherEventType;

  TFSWatcherEventData = record
    Path: String;
    EventType: TFSWatcherEventType;
    FileName: String;    // Valid for fswFileCreated, fswFileChanged, fswFileDeleted, fswFileRenamed
    NewFileName: String; // Valid for fswFileRenamed
    UserData: Pointer;
{$IFDEF DARWIN}
    OriginalEvent: TDarwinFSWatchEvent;
{$ENDIF}
  end;
  PFSWatcherEventData = ^TFSWatcherEventData;

  TFSWatcherEvent = procedure(const EventData: TFSWatcherEventData) of object;

  { TFileSourceWatcher }

  TFileSourceWatcher = class
    function canWatch( const path: String ): Boolean; virtual; abstract;
    function canWatch( const paths: array of String ): Boolean; virtual; abstract;
    procedure updateWatch; virtual; abstract;
    function addWatch( const path: String;
                      const filter: TFSWatchFilter;
                      const event: TFSWatcherEvent;
                      const UserData: Pointer = nil ): Boolean; virtual; abstract;
    procedure removeWatch( const path: String;
                          const event: TFSWatcherEvent); virtual; abstract;
  end;

  { TDefaultFileSourceWatcher }

  TDefaultFileSourceWatcher = class( TFileSourceWatcher )
    function canWatch( const path: String ): Boolean; override;
    function canWatch( const paths: array of String ): Boolean; override;
    procedure updateWatch; override;
    function addWatch(const path: String; const filter: TFSWatchFilter;
      const event: TFSWatcherEvent; const UserData: Pointer=nil): Boolean; override;
    procedure removeWatch(const path: String; const event: TFSWatcherEvent); override;
  end;

implementation

{ TFileSourceDefaultWatcher }

function TDefaultFileSourceWatcher.canWatch(const path: String): Boolean;
begin
  Result:= False;
end;

function TDefaultFileSourceWatcher.canWatch(const paths: array of String): Boolean;
var
  i: Integer;
begin
  for i:= Low(paths) to High(paths) do begin
    if NOT self.canWatch( paths[i] ) then
      Exit( False );
  end;
  Result:= True;
end;

procedure TDefaultFileSourceWatcher.updateWatch;
begin
end;

function TDefaultFileSourceWatcher.addWatch(const path: String;
  const filter: TFSWatchFilter; const event: TFSWatcherEvent;
  const UserData: Pointer): Boolean;
begin
  Result:= False;
end;

procedure TDefaultFileSourceWatcher.removeWatch(const path: String;
  const event: TFSWatcherEvent);
begin
end;

end.

