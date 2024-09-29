unit uPlaySound;

{$mode objfpc}{$H+}
{$IF DEFINED(DARWIN)}
{$modeswitch objectivec1}
{$ENDIF}

interface

uses
  Classes, SysUtils
{$IF DEFINED(MSWINDOWS)}
  , MMSystem, LazUTF8
{$ELSEIF DEFINED(DARWIN)}
  , CocoaAll, uMyDarwin
{$ELSE}
  , LazLogger, sdl2
{$ENDIF}
  ;

function PlaySound(const FileName: String): Boolean;

implementation

{$IF DEFINED(DARWIN)}
type
  { NSSoundFinishedDelegate }
  SoundFinishedDelegate = objcclass(NSObject, NSSoundDelegateProtocol)
  public
    procedure sound_didFinishPlaying(Sound: NSSound; FinishedPlaying: Boolean); message 'sound:didFinishPlaying:';
  end;

var
  SoundDelegate: SoundFinishedDelegate;

{ NSSoundFinishedDelegate }

procedure SoundFinishedDelegate.sound_didFinishPlaying(Sound: NSSound; FinishedPlaying: Boolean);
begin
  if (FinishedPlaying) then
  begin
    Sound.Release;
    Sound:= nil;
    Sound.dealloc;
  end;
end;
{$ENDIF}

function PlaySound(const FileName: String): Boolean;
{$IF DEFINED(MSWINDOWS)}
begin
  Result:= sndPlaySoundW(PWideChar(UTF8ToUTF16(FileName)), SND_ASYNC or SND_NODEFAULT);
end;
{$ELSEIF DEFINED(DARWIN)}
var
  Sound: NSSound;
  audioFilePath: NSString;
begin
  audioFilePath:= StringToNSString(FileName);
  Sound:= NSSound.alloc.initWithContentsOfFile_byReference(audioFilePath, True);
  Sound.setDelegate(SoundDelegate);
  Result:= Sound.Play;
  if not Result then
  begin
    Sound.Release;
    Sound:= nil;
    Sound.dealloc;
  end;
end;
{$ELSE}
const
  Res: Integer = -1;
  First: Boolean = True;
begin
  if First then
  begin
    if SDL_Initialize then
    begin
      Res:= SDL_InitSubSystem(SDL_INIT_AUDIO);
      if (Res < 0) then
      begin
        DebugLn('SDL_InitSubSystem: ', SDL_GetError());
      end;
    end;
    First:= False;
  end;
  if (Res < 0) then
    Result:= False
  else begin
    Result:= SDL_Play(FileName);
  end;
end;
{$ENDIF}

{$IF DEFINED(DARWIN)}
initialization
  SoundDelegate:= SoundFinishedDelegate.alloc.init;
{$ENDIF}

end.
