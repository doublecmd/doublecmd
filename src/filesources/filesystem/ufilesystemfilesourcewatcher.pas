unit uFileSystemFileSourceWatcher;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceWatcher, uFileSystemWatcher;

type

  { TFileSystemFileSourceWatcher }

  TFileSystemFileSourceWatcher = class( TFileSourceWatcher )
    function canWatch( const path: String ): Boolean; override;
    function canWatch( const paths: array of String ): Boolean; override;
    procedure updateWatch; override;
    function addWatch( const path: String;
                      const filter: TFSWatchFilter;
                      const event: TFSWatcherEvent;
                      const UserData: Pointer = nil ): Boolean; override;
    procedure removeWatch( const path: String;
                          const event: TFSWatcherEvent); override;
  end;

implementation

{ TFileSystemFileSourceWatcher }

function TFileSystemFileSourceWatcher.canWatch(const path: String): Boolean;
begin
  Result:= TFileSystemWatcher.CanWatch( path );
end;

function TFileSystemFileSourceWatcher.canWatch(const paths: array of String): Boolean;
begin
  Result:= TFileSystemWatcher.CanWatch( paths );
end;

procedure TFileSystemFileSourceWatcher.updateWatch;
begin
  {$IFDEF DARWIN}
  TFileSystemWatcher.UpdateWatch;
  {$ENDIF}
end;

function TFileSystemFileSourceWatcher.addWatch(const path: String;
  const filter: TFSWatchFilter; const event: TFSWatcherEvent;
  const UserData: Pointer): Boolean;
begin
  Result:= TFileSystemWatcher.AddWatch( path, filter, event, UserData );
end;

procedure TFileSystemFileSourceWatcher.removeWatch(const path: String;
  const event: TFSWatcherEvent);
begin
  TFileSystemWatcher.RemoveWatch( path, event );
end;

end.

