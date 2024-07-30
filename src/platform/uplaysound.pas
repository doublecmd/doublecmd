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
begin
  Result:= False;
end;
{$ENDIF}

{$IF DEFINED(DARWIN)}
initialization
  SoundDelegate:= SoundFinishedDelegate.alloc.init;
{$ENDIF}

end.
