unit uWayland;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, wayland_protocol, wayland_client_core;

type

  { TWaylandScreen }

  TWaylandScreen = class(TWlOutput)
  private
    FId: UInt32;
    FName: String;
    FVersion: UInt32;
    FWidth, FHeight: UInt32;
  public
    property Id: UInt32 read FId;
    property Name: String read FName;
    property Width: UInt32 read FWidth;
    property Height: UInt32 read FHeight;
    property Version: UInt32 read FVersion;
  end;

  { TWaylandClient }

  TWaylandClient = class(IWlRegistryListener, IWlOutputListener)
  private
    FDisplay: TWLDisplay;
    FRegistry: TWlRegistry;
    FScreens: TFPObjectList;
    // IWlRegistryListener
    procedure wl_registry_global(AWlRegistry: TWlRegistry; AName: DWord; AInterface: String; AVersion: DWord);
    procedure wl_registry_global_remove(AWlRegistry: TWlRegistry; AName: DWord);
    // IWlOutputListener
    procedure wl_output_geometry(AWlOutput: TWlOutput; AX: LongInt; AY: LongInt; APhysicalWidth: LongInt; APhysicalHeight: LongInt; ASubpixel: LongInt; AMake: String; AModel: String; ATransform: LongInt);
    procedure wl_output_mode(AWlOutput: TWlOutput; AFlags: DWord; AWidth: LongInt; AHeight: LongInt; ARefresh: LongInt);
    procedure wl_output_done(AWlOutput: TWlOutput); {since: 2}
    procedure wl_output_scale(AWlOutput: TWlOutput; AFactor: LongInt); {since: 2}
    procedure wl_output_name(AWlOutput: TWlOutput; AName: String); {since: 4}
    procedure wl_output_description(AWlOutput: TWlOutput; ADescription: String); {since: 4}
  public
    constructor Create;
    destructor Destroy; override;
    property Screens: TFPObjectList read FScreens;
  end;

implementation

uses
  LazLoggerBase;

{ TWaylandClient }

procedure TWaylandClient.wl_registry_global(AWlRegistry: TWlRegistry;
  AName: DWord; AInterface: String; AVersion: DWord);
var
  AScreen: TWaylandScreen;
begin
  if AInterface = wl_output_interface.name then
  begin
    DebugLogger.DebugLn('Wayland: name ', IntToStr(AName), ', interface ', AInterface, ', version ', IntToStr(AVersion));

    if (AVersion >= wl_output_interface.version) then
    begin
      AScreen:= TWaylandScreen.Create(AWlRegistry.Bind(AName, @wl_output_interface, wl_output_interface.version), True);
      AScreen.FId:= AName;
      AScreen.AddListener(Self);
    end;
  end;
end;

procedure TWaylandClient.wl_registry_global_remove(AWlRegistry: TWlRegistry; AName: DWord);
var
  Index: Integer;
begin
  for Index:= FScreens.Count - 1 downto 0 do
  begin
    if (TWaylandScreen(FScreens[Index]).Id = AName) then
    begin
      FScreens.Delete(Index);
      Break;
    end;
  end;
end;

procedure TWaylandClient.wl_output_geometry(AWlOutput: TWlOutput; AX: LongInt;
  AY: LongInt; APhysicalWidth: LongInt; APhysicalHeight: LongInt;
  ASubpixel: LongInt; AMake: String; AModel: String; ATransform: LongInt);
begin

end;

procedure TWaylandClient.wl_output_mode(AWlOutput: TWlOutput; AFlags: DWord;
  AWidth: LongInt; AHeight: LongInt; ARefresh: LongInt);
begin
  if (AFlags and WL_OUTPUT_MODE_CURRENT <> 0) then
  begin
    TWaylandScreen(AWlOutput).FWidth:= AWidth;
    TWaylandScreen(AWlOutput).FHeight:= AHeight;
  end;
end;

procedure TWaylandClient.wl_output_done(AWlOutput: TWlOutput);
begin
  FScreens.Add(AWlOutput);
end;

procedure TWaylandClient.wl_output_scale(AWlOutput: TWlOutput; AFactor: LongInt);
begin

end;

procedure TWaylandClient.wl_output_name(AWlOutput: TWlOutput; AName: String);
begin
  if (Length(AName) > 0) then
  begin
    TWaylandScreen(AWlOutput).FName:= AName;
  end;
end;

procedure TWaylandClient.wl_output_description(AWlOutput: TWlOutput;
  ADescription: String);
begin

end;

constructor TWaylandClient.Create;
begin
  FScreens:= TFPObjectList.Create(True);

  FDisplay := TWlDisplay.Connect('') as TWLDisplay;
  if not Assigned(FDisplay) then
  begin
    raise Exception.Create('Wayland: failed to open display!');
  end;

  FRegistry := FDisplay.GetRegistry();
  FRegistry.AddListener(Self);
  FDisplay.Roundtrip;
  FDisplay.Roundtrip;
end;

destructor TWaylandClient.Destroy;
begin
  FScreens.Free;
  FDisplay.Free;
  FRegistry.Free;
end;

end.

