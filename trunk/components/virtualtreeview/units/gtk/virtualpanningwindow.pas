unit virtualpanningwindow;

{$mode objfpc}{$H+}

interface

uses
  LCLType, Graphics, Classes, SysUtils;
  
type

  { TVirtualPanningWindow }

  TVirtualPanningWindow = class
  private
    FHandle: THandle;
    FOwnerHandle: THandle;
    FImage: TBitmap;
    procedure HandlePaintMessage;
  public
    procedure Start(OwnerHandle: THandle; const Position: TPoint);
    procedure Stop;
    procedure Show(ClipRegion: HRGN);
    property Image: TBitmap read FImage;
    property Handle: THandle read FHandle;
  end;

implementation

{$ifdef DEBUG_VTV}
uses
  vtlogger;
{$endif}

{ TVirtualPanningWindow }

procedure TVirtualPanningWindow.HandlePaintMessage;
begin
end;

procedure TVirtualPanningWindow.Start(OwnerHandle: THandle; const Position: TPoint);
begin  
  FImage := TBitmap.Create;
end;

procedure TVirtualPanningWindow.Stop;
begin
  FImage.Free;
  FImage := nil;
end;

procedure TVirtualPanningWindow.Show(ClipRegion: HRGN);
begin
  {$ifdef DEBUG_VTV}Logger.SendBitmap([lcPanning],'Panning Image',FImage);{$endif}
end;

end.

