
{ This file is part of Delphi Compatibility Unit

  Copyright (C) 2007 Luiz Américo Pereira Câmara
  pascalive@bol.com.br

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

}

{$define HAS_INVERTRECT}
{$define HAS_DRAWTEXTW}
{$define HAS_GETCURRENTOBJECT}
{$define HAS_GETBKCOLOR}
{$define HAS_GETTEXTEXTENTEXPOINT}
{$define HAS_DRAWFRAMECONTROL}
{$define HAS_SCROLLWINDOW}

{$i ../generic/stubs.inc}
{$i ../generic/independentfunctions.inc}
{$i ../generic/unicodefunctions.inc}

function BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc,
  YSrc: Integer; Rop: DWORD): Boolean;
begin
  Result := GTKWidgetSet.StretchCopyArea(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, Width, Height,
   0, XSrc, YSrc, Rop);
end;

function DrawFrameControl(DC: HDC; const Rect: TRect; uType, uState: LongWord): Boolean;
begin
  Result := LCLIntf.DrawFrameControl(DC, Rect, uType, uState);
end;

function DrawTextW(hDC: HDC; lpString: PWideChar; nCount: Integer; var lpRect: TRect; uFormat: LongWord): Integer;
var
  TempStr: UTF8String;
  TempRect: TRect;
  TextHeight: Integer;
  TM: TTextMetric;
begin
  //Logger.Send('DrawTextW');
  TempRect := lpRect;
  //fix position under gtk (lcl bug 8565)
  if (uFormat and DT_VCENTER) > 0 then
  begin
    GetTextMetrics(hDC, TM);
    //gtk overestimate height
    TextHeight := TM.tmHeight - 2;
    TempRect.Top := (TempRect.Top + TempRect.Bottom - TextHeight) div 2;
  end;
  TempStr := UTF8Encode(WideString(lpString));
  Result := DrawText(hDC, PChar(TempStr), GetUTF8ByteCount(TempStr, nCount),
    TempRect, uFormat);
  //Logger.Send('Rect',TempRect);
end;

function GetBkColor(DC:HDC):COLORREF;
begin
  if GTKWidgetSet.IsValidDC(DC) then
    Result := TGtkDeviceContext(DC).CurrentBackColor.ColorRef
  else
    Result := CLR_INVALID;
end;

function GetCurrentObject(hdc: HDC; uObjectType: UINT): HGDIOBJ;
begin
  Result := 0;
  if GTKWidgetSet.IsValidDC(hdc) then
  with TGtkDeviceContext(hdc) do
  begin
    case uObjectType of
      OBJ_BITMAP: Result := HGDIOBJ(CurrentBitmap);
      OBJ_BRUSH: Result := HGDIOBJ(CurrentBrush);
      OBJ_FONT: Result := HGDIOBJ(CurrentFont);
      OBJ_PEN: Result := HGDIOBJ(CurrentPen);
    end;
  end;
end;

function GetTextExtentExPoint(DC: HDC; Str: PChar; Count, MaxWidth: Integer;
  MaxCount, PartialWidths: ObjPas.PInteger; var Size: TSize): BOOL;
var
  lbearing, rbearing, width, ascent,descent: LongInt;
  UseFont : PGDKFont;
  IsDBCSFont: Boolean;
  NewCount,Accumulator,i: Integer;
begin
  //based in lcl code
  Result := GTKWidgetSet.IsValidDC(DC);
  if Result then
  with TGtkDeviceContext(DC) do
  begin
    if (CurrentFont = nil) or (CurrentFont^.GDIFontObject = nil)
    then begin
      UseFont := GTKWidgetSet.GetDefaultGtkFont(false);
    end
    else begin
      UseFont := CurrentFont^.GDIFontObject;
    end;
    If UseFont = nil then
      DebugLn('WARNING: [TGtkWidgetSet.GetTextExtentPoint] Missing font')
    else begin
      descent:=0;
      {
      UpdateDCTextMetric(TDeviceContext(DC));
      IsDBCSFont:=TDeviceContext(DC).DCTextMetric.IsDoubleByteChar;

      if IsDBCSFont then begin
        NewCount:=Count*2;
        if FExtUTF8OutCacheSize<NewCount then begin
          ReAllocMem(FExtUTF8OutCache,NewCount);
          FExtUTF8OutCacheSize:=NewCount;
        end;
        NewCount:=UTF8ToDoubleByte(Str,Count,FExtUTF8OutCache)*2;
        gdk_text_extents(UseFont, FExtUTF8OutCache, NewCount,
                         @lbearing, @rBearing, @width, @ascent, @descent);
      end else begin
        gdk_text_extents(UseFont, Str, Count,
                         @lbearing, @rBearing, @width, @ascent, @descent);
      end;
      }
      gdk_text_extents(UseFont, Str, Count,@lbearing, @rBearing, @width, @ascent, @descent);
      Size.cX := Width;
      Size.cY := ascent+descent;
      if PartialWidths <> nil then
      begin
        Accumulator:=0;
        for i:= 0 to Count - 1 do
        begin
          Inc(Accumulator,gdk_char_width(UseFont,(Str+i)^));
          PartialWidths[i]:=Accumulator;
        end;
      end;
    end;
  end;
end;

function InvertRect(DC: HDC; const lprc: TRect): Boolean;
var
  DCOrigin: TPoint;
  Values: TGdkGCValues;
begin
  //todo: see the windows result when rect is invalid
  Result := GTKWidgetSet.IsValidDC(DC) and (lprc.Bottom > lprc.Top)
    and (lprc.Right > lprc.Left);
  if Result then
  begin
    with TGtkDeviceContext(DC) do
    begin
      DCOrigin := Offset;
      //todo: see if is necessary store old function
      gdk_gc_get_values(GC, @Values);
      gdk_gc_set_function(GC,GDK_INVERT);
      gdk_draw_rectangle(Drawable,GC,1,
        DCOrigin.X + lprc.Left, DCOrigin.Y + lprc.Top,
        lprc.Right - lprc.Left, lprc.Bottom - lprc.Top);
      gdk_gc_set_function(GC,Values.thefunction);
    end;
  end;
end;

function ScrollWindow(hWnd:THandle; XAmount, YAmount:longint;lpRect:PRECT; lpClipRect:PRECT):Boolean;
begin
  //gtk implementation does nothing if lpRect and lpClipRect are not nil
  Result := LCLIntf.ScrollWindowEx(hWnd, XAmount, YAmount, nil, nil, 0, nil, SW_INVALIDATE);
end;

var
  CachedUnicodeFormat: TClipboardFormat;
  
function CF_UNICODETEXT: TClipboardFormat;
begin
  //todo: see what mime type gtk expects for utf16
  if CachedUnicodeFormat = 0 then
    CachedUnicodeFormat:= gdk_atom_intern('text/utf16',GdkFalse);
  Result := CachedUnicodeFormat;
end;

type
  TTimerRecord = record
    Control: TControl;
    Notify: TTimerNotify;
    Id: LongWord;
    TimerHandle: guint;
  end;
  PTimerRecord = ^TTimerRecord;

  { TTimerList }

  TTimerList = class
  private
    FList: TMap;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(hWnd: THandle; ID: LongWord; NotifyFunc: TTimerNotify; WinControl: TControl):PTimerRecord;
    function GetTimerInfo(Handle: hWnd; idEvent:LongWord; out TimerInfo: TTimerRecord):Boolean;
    function GetTimerInfoPtr(Handle: hWnd; idEvent:LongWord): PTimerRecord;
  end;

var
  FTimerList: TTimerList;

function MakeQWord(d1, d2: dword): QWord; inline;
begin
  Result:=(QWord(d2) shl 32) or d1;
end;

{ TTimerList }

constructor TTimerList.Create;
begin
  //todo: see 64bit (itu16??)
  FList:=TMap.Create(itu8,SizeOf(TTimerRecord));
end;

destructor TTimerList.Destroy;
begin
  FList.Destroy;
  inherited Destroy;
end;

function TTimerList.Add(hWnd: THandle; ID: LongWord; NotifyFunc: TTimerNotify; WinControl: TControl):PTimerRecord;
var
  AID: QWord;
  ATimerRec: TTimerRecord;
begin
  ATimerRec.Notify := NotifyFunc;
  ATimerRec.Control := WinControl;
  ATimerRec.Id := ID;
  AId:=MakeQWord(hWnd,ID);
  with FList do
  begin
    if HasId(AID) then
      SetData(AID, ATimerRec)
    else
      Add(AID, ATimerRec);
    Result := GetDataPtr(AID);
  end;
end;

function TTimerList.GetTimerInfo(Handle: hWnd; idEvent: LongWord; out
  TimerInfo: TTimerRecord): Boolean;
begin
  Result:= FList.GetData(MakeQWord(Handle,idEvent),TimerInfo);
end;

function TTimerList.GetTimerInfoPtr(Handle: hWnd; idEvent: LongWord
  ): PTimerRecord;
begin
  Result := FList.GetDataPtr(MakeQWord(Handle,idEvent));
end;

function gtkTimerCB(Data: gPointer): {$IFDEF Gtk2}gBoolean{$ELSE}gint{$ENDIF}; cdecl;
begin
  Result := GdkFalse;  // assume: timer will stop
  with PTimerRecord(Data)^ do
  begin
    //DebugLn('gtkTimerCalled for TimerHandle: %d',[TimerHandle]);
    if TimerHandle <> 0 then
    begin
      if Notify <> nil then
      begin
        Notify(Id);
        Result := GdkTrue;
      end
      else
      begin
        if Control <> nil then
        begin
          LCLSendTimerMsg(Control,Id,0);
          Result := GdkTrue;
        end;
      end;
    end;
  end;
end;

function SetTimer(hWnd:THandle; nIDEvent:LongWord; uElapse:LongWord; lpTimerFunc:TTimerNotify):LongWord;
var
  TimerInfo: PTimerRecord;
  Control: TControl;
begin
  //todo: properly set Result
  //todo: make a custom GetLCLObject
  if hWnd <> 0 then
    Control := TControl(GetLCLObject(PGtkWidget(hWnd)))
  else
    Control := nil;
  TimerInfo := FTimerList.Add(hWnd, nIDEvent, lpTimerFunc, Control);
  TimerInfo^.TimerHandle := gtk_timeout_add(uElapse, @gtkTimerCB, TimerInfo);
  //DebugLn('SetTimer HWnd: %d ID: %d TimerHandle: %d',[hWnd,nIDEvent,TimerInfo^.TimerHandle]);
end;

function KillTimer(hWnd:THandle; nIDEvent: LongWord):Boolean;
var
  TimerInfo: PTimerRecord;
begin
  TimerInfo := FTimerList.GetTimerInfoPtr(hWnd,nIDEvent);
  if TimerInfo <> nil then
  begin
    //DebugLn('KillTimer HWnd: %d ID: %d TimerHandle: %d',[hWnd,nIDEvent,TimerInfo^.TimerHandle]);
    gtk_timeout_remove(TimerInfo^.TimerHandle);
    //next time gtkTimerCB be called the timeout will be destroied automatically
    //todo: see if is really necessary to set TimerHandle to 0 and check in gtkTimerCB
    TimerInfo^.TimerHandle := 0;
  end;
  //else
  //  DebugLn('KillTimer Could not find the timer info of HWnd: %d ID: %d',[hWnd,nIDEvent]);
end;


