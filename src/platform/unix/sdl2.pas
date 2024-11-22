unit sdl2;

{$mode delphi}
{$packrecords c}

interface

uses
  Classes, SysUtils, CTypes;

const
  SDL_INIT_AUDIO = $00000010;

  SDL_AUDIO_ALLOW_FREQUENCY_CHANGE = $00000001;
  SDL_AUDIO_ALLOW_FORMAT_CHANGE    = $00000002;
  SDL_AUDIO_ALLOW_CHANNELS_CHANGE  = $00000004;
  SDL_AUDIO_ALLOW_SAMPLES_CHANGE   = $00000008;
  SDL_AUDIO_ALLOW_ANY_CHANGE       = (SDL_AUDIO_ALLOW_FREQUENCY_CHANGE or SDL_AUDIO_ALLOW_FORMAT_CHANGE or
                                      SDL_AUDIO_ALLOW_CHANNELS_CHANGE or SDL_AUDIO_ALLOW_SAMPLES_CHANGE);

type
  SDL_AudioFormat = type UInt16;
  SDL_AudioDeviceID = type UInt32;

  TSDL_RWops = record end;
  PSDL_RWops = ^TSDL_RWops;

  SDL_AudioCallback = procedure(userdata: Pointer; stream: PByte; len: cint); cdecl;

  SDL_AudioSpec = record
    freq: cint;
    format: SDL_AudioFormat;
    channels: UInt8;
    silence: UInt8;
    samples: UInt16;
    padding: UInt16;
    size: UInt32;
    callback: SDL_AudioCallback;
    userdata: Pointer;
  end;
  PSDL_AudioSpec = ^SDL_AudioSpec;

  TAudioData = record
    wavStart: PByte;
    wavLength: UInt32;
    wavSpec: SDL_AudioSpec;
  end;
  PAudioData = ^TAudioData;

var
  SDL_InitSubSystem: function(flags: UInt32): cint; cdecl;
  SDL_Delay: procedure(ms: UInt32); cdecl;
  SDL_GetError: function(): PAnsiChar; cdecl;
  SDL_RWFromFile: function(const file_name: PAnsiChar; const mode: PAnsiChar): PSDL_RWops; cdecl;
  SDL_LoadWAV_RW: function(src: PSDL_RWops; freesrc: cint; spec: PSDL_AudioSpec;
                           audio_buf: PPByte; audio_len: PUInt32): PSDL_AudioSpec; cdecl;
  SDL_FreeWAV: procedure(audio_buf: PByte); cdecl;
  SDL_QueueAudio: function(dev: SDL_AudioDeviceID; const data: Pointer; len: UInt32): cint; cdecl;
  SDL_GetQueuedAudioSize: function(dev: SDL_AudioDeviceID): UInt32; cdecl;
  SDL_OpenAudioDevice: function(const device: PAnsiChar; iscapture: cint;
                                const desired: PSDL_AudioSpec; obtained: PSDL_AudioSpec;
                                allowed_changes: cint): SDL_AudioDeviceID; cdecl;
  SDL_PauseAudioDevice: procedure(dev: SDL_AudioDeviceID; pause_on: cint); cdecl;
  SDL_CloseAudioDevice: procedure(dev: SDL_AudioDeviceID); cdecl;

function SDL_Initialize: Boolean;
function SDL_Play(const FileName: String): Boolean;

implementation

uses
  DCOSUtils, LazLogger;

function Play(Parameter: Pointer): PtrInt;
var
  audioDevice: SDL_AudioDeviceID;
  AudioData: PAudioData absolute parameter;
begin
  Result:= 0;
  try
    audioDevice:= SDL_OpenAudioDevice(nil, 0, @AudioData^.wavSpec, nil, SDL_AUDIO_ALLOW_ANY_CHANGE);

    if audioDevice = 0 then
    begin
      DebugLn('SDL_OpenAudioDevice: ', SDL_GetError());
      Exit(-1);
    end;

    SDL_QueueAudio(audioDevice, AudioData^.wavStart, AudioData^.wavLength);
    SDL_PauseAudioDevice(audioDevice, 0);

    while SDL_GetQueuedAudioSize(audioDevice) > 0 do
    begin
      SDL_Delay(100);
    end;

    SDL_CloseAudioDevice(audioDevice);
  finally
    SDL_FreeWAV(AudioData^.wavStart);
    Dispose(AudioData);
  end;
end;

function SDL_Play(const FileName: String): Boolean;
var
  RWops: PSDL_RWops;
  AudioData: PAudioData;
begin
  RWops:= SDL_RWFromFile(PAnsiChar(FileName), 'rb');
  if (RWops = nil) then
  begin
    DebugLn('SDL_RWFromFile: ', SDL_GetError());
    Exit(False);
  end;
  New(AudioData);
  with AudioData^ do
  begin
    if (SDL_LoadWAV_RW(RWops, 1, @wavSpec, @wavStart, @wavLength) = nil) then
    begin
      DebugLn('SDL_LoadWAV_RW: ', SDL_GetError());
      Dispose(AudioData);
      Exit(False);
    end;
  end;
  Result:= BeginThread(@Play, AudioData) > 0;
end;

const
  sdllib= 'libSDL2-2.0.so.0';

var
  libsdl: TLibHandle;

function SDL_Initialize: Boolean;
var
  AMsg: String;
begin
  libsdl:= SafeLoadLibrary(sdllib);
  Result:= (libsdl <> NilHandle);
  if Result then
  try
    SDL_InitSubSystem:= SafeGetProcAddress(libsdl, 'SDL_InitSubSystem');
    SDL_Delay:= SafeGetProcAddress(libsdl, 'SDL_Delay');
    SDL_GetError:= SafeGetProcAddress(libsdl, 'SDL_GetError');
    SDL_RWFromFile:= SafeGetProcAddress(libsdl, 'SDL_RWFromFile');
    SDL_LoadWAV_RW:= SafeGetProcAddress(libsdl, 'SDL_LoadWAV_RW');
    SDL_FreeWAV:= SafeGetProcAddress(libsdl, 'SDL_FreeWAV');
    SDL_QueueAudio:= SafeGetProcAddress(libsdl, 'SDL_QueueAudio');
    SDL_GetQueuedAudioSize:= SafeGetProcAddress(libsdl, 'SDL_GetQueuedAudioSize');
    SDL_OpenAudioDevice:= SafeGetProcAddress(libsdl, 'SDL_OpenAudioDevice');
    SDL_PauseAudioDevice:= SafeGetProcAddress(libsdl, 'SDL_PauseAudioDevice');
    SDL_CloseAudioDevice:= SafeGetProcAddress(libsdl, 'SDL_CloseAudioDevice');

    Result:= SDL_InitSubSystem(SDL_INIT_AUDIO) = 0;

    if not Result then
    begin
      AMsg:= SDL_GetError();
      raise Exception.Create(AMsg);
    end;
  except
    on E: Exception do
    begin
      Result:= False;
      DebugLn(E.Message);
      FreeLibrary(libsdl);
    end;
  end;
end;

end.

