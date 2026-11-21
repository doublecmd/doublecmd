unit uDarwinSimpleFSWatch;

{$mode delphi}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils,
  uDarwinFSWatch;

type
  // MacOS Simple File Sytem Watcher (only one watchPath)

  { TSimpleDarwinFSWatcher }

  TSimpleDarwinFSWatcher = class( TThread )
  private
    _monitor: TDarwinFSWatcher;
    _callback: TDarwinFSWatchCallBack;
  protected
    procedure Execute; override;
    procedure handleEvent( event:TDarwinFSWatchEvent );
  public
    procedure stop();
    constructor Create( const path:String; const callback:TDarwinFSWatchCallBack );
    destructor Destroy; override;
  public
    property monitor: TDarwinFSWatcher read _monitor;
  end;

implementation

{ TSimpleDarwinFSWatcher }

procedure TSimpleDarwinFSWatcher.Execute;
begin
  _monitor.start();
end;

procedure TSimpleDarwinFSWatcher.handleEvent( event:TDarwinFSWatchEvent );
begin
  _callback( event );
end;

procedure TSimpleDarwinFSWatcher.stop();
begin
  _monitor.terminate();
end;

constructor TSimpleDarwinFSWatcher.Create(
  const path:String;
  const callback:TDarwinFSWatchCallBack );
begin
  Inherited Create( false );
  _callback:= callback;
  _monitor:= TDarwinFSWatcher.create( handleEvent );
  _monitor.addPath( path );
end;

destructor TSimpleDarwinFSWatcher.Destroy;
begin
  _monitor.terminate;
  FreeAndNil( _monitor );
  inherited;
end;

end.

