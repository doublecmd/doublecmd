{ Copyright (C) 2007 Julian Schutsch

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU Lesser General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more
  details.

  A copy of the GNU Lesser General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/lgpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
  
  Changelog:
    9.27.2007 : Seperation from another Package, first Release under GPL
                Version 0.1
   10.02.2007 : Licence Changed to LGPL
                Added   : History
                Added   : Password Input mode
                Fixed   : Blank Screen when Resizing so that TopLine disappears.
                Added   : Fixed Prompt Description infront of Input, moves along with it
                Etc     : Functions, minor Bugs
                Missing : FreeLineWidth full support
                Version 0.2
   10.08.2007 : Removed : Fixed Line Width Support, Source now less complex
                Added   : Paste/Copy/Cut Ability, Select with Mouse and Shift/Keys
                Added   : TRTLCriticalsection called FLock to make Writeln/Write Threadsafe
                Fixed   : GTK 1/2 Linux support, several changes to make that work...
                Removed : LineWidth, can cause Property Loading Errors if used with old Apps !
                Workarn : GTK Font height gets 2 added on all plattforms, means, win32 have two extra dots unecessarily, can't solve that !
                Fixed   : Pos 1/End Key changes Scrollbar (Different GTK behaviour !)
                Version 0.3
   12.06.2008 : Optimized Color String output, still needs testing and PWD Strings are not
                changed yet. Improvement visible on Win32, but still to slow, any hacks?
   17.06.2008 : TColorString changed completly, now using Arrays instead of linked lists
   25.06.2008 : Fixed everything for Multispace support
                Added tabulator behaviour
                Caret type and Color now customizable
                Input Selection Colors published
                Speed improvement using precalculated Sum-Widths for TColorString
                Lots of minor UTF8 Bugs fixed
   26.06.2008 : Escape Codes for some sort of Graphical output (Tables, lines, etc)
                Better moving Input
                Bug fixes in MakeInputVisible
   27.06.2008 : Add FGraphicCharWidth
   28.06.2008 : New Escape Code preprocessor
                Support for different modes (ANSI color, CmdBox, None(ignore))
   29.06.2008 : FStringBuffer added,Works without WakeMainThread now as well
                Fixed LineOutAndFill
                Added AutoFollow
   
   Todo    : Input Masks
   Todo    : Docu

}
unit uCmdBox;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils,ExtCtrls,ComCtrls,Controls,Graphics,StdCtrls,Forms,LCLType,LCLIntf,
     lmessages,lresources,ClipBrd,LCLProc;
  
type TCaretType=(cartLine,cartSubBar,cartBigBar,cartUser);
     TEscapeCodeType=(esctCmdBox,esctAnsi,esctNone);
     TEscapeMode=(escmNone,escmOperation,escmData2,escmData1,escmAnsiOperation,escmAnsiSquare);
  
type TCmdBox=class;
type TColorstring=class;
  
type EOnCmdBoxInput=Procedure(ACmdBox:TCmdBox;Input:String) of object;
type EOnCmdBoxInputChange=Procedure(ACmdBox:TCmdBox;InputData:TColorstring) of object;

type

{ TCmdBox }

TCmdBox=class(TCustomControl)
      public
      constructor Create(AComponent:TComponent);override;
      destructor Destroy;override;
      protected
      procedure Paint;override;
      procedure Resize;override;
      procedure UTF8KeyPress(var Key:TUTF8Char);override;
      procedure KeyDown(var Key:Word;Shift:TShiftState);override;
      procedure CreateParams(var Params:TCreateParams);override;
      procedure CreateWnd;override;
      procedure WMVScroll(var message: TLMVScroll);message LM_VSCROLL;
      private
      FLock               : System.TRTLCriticalSection;
      FCaretTimer         : TTimer;
      FCaretVisible       : Boolean;
      FLineCount          : Integer;
      FLines              : array of TColorstring;
      FLineHeights        : array of Integer;
      FLineHeightSum      : array of Integer;
      FTopLine            : Integer;
      FPageHeight         : Integer;
      FVisibleLines       : Integer;
      FVSBVisible         : Boolean;
      FVSBPos             : Integer;
      FVSBWidth           : Integer;
      FClientWidth        : Integer;
      FClientHeight       : Integer;
      FCaretX             : Integer;
      FOutX,FOutY         : Integer;
      FInputX,FInputY     : Integer;
      FInputPos           : Integer;
      FCharHeight         : Integer;
      FLineOfTopLine      : Integer;
      FVisibleLineCount   : Integer;
      FInput              : Boolean;
      FInputBuffer        : TColorstring;
      FInputVisible       : Boolean;
      FInputMinPos        : Integer;
      FUTF8InputMinPos    : Integer;
      FOnInput            : EOnCmdBoxInput;
      FOnAny              : EOnCmdBoxInputChange;
      FOnInputChange      : EOnCmdBoxInputChange;
      FBackGroundColor    : TColor;
      FCurrentColor       : TColor;
      FCurrentBackGround  : TColor;
      FFont               : TFont;
      FPassWordChar       : TUTF8Char;
      FInputIsPassWord    : Boolean;
      FHistory            : Array of TColorstring;
      FHistoryLength      : Integer;
      FHistoryMax         : Integer;
      FHistoryPos         : Integer;
      FInputColor         : TColor;
      FInputBackground    : TColor;
      FInputSelColor      : TColor;
      FInputSelBackGround : TColor;
      FMouseDown          : Boolean;
      FSelStart,FSelEnd   : Integer;
      FMouseDownInputPos  : Integer;
      FCurrentString      : String;
      FCaretColor         : TColor;
      FCaretType          : TCaretType;
      FCaretWidth         : Integer;
      FCaretHeight        : Integer;
      FCaretYShift        : Integer;
      FTabWidth           : Integer;
      FGraphicCharWidth   : Integer;
      FEscapeCodeType     : TEscapeCodeType;
      FEscapeMode         : TEscapeMode;
      FEscapeData         : String;
      FStringBuffer       : TStringList;
      FAutoFollow         : Boolean;
      procedure CaretTimerExecute(Sender:TObject);
      procedure SetLineCount(c:Integer);
      procedure SetTopLine(Nr:Integer);
      procedure AdjustScrollBars;
      function AdjustLineHeight(i:Integer):Integer;
      procedure MakeInputVisible;
      procedure MakeOutVisible;
      procedure SetFont(F:TFont);
      procedure SetBackGroundColor(c:Tcolor);
      function GetSystemMetricsGapSize(const Index:Integer):Integer;
      procedure ScrollBarRange(Which: Integer; aRange,aPage: Integer);
      procedure ScrollBarPosition(Which,Value:Integer);
      function UpdateLineHeights:Integer;
      procedure TranslateScrollBarPosition;
      procedure ScrollUp;
      procedure SetHistoryMax(v:Integer);
      procedure InsertHistory;
      procedure SetHistoryPos(v:Integer);
      procedure EraseBackground(DC:HDC);override;
      function GetHistory(i:Integer):string;
      procedure DeleteHistoryEntry(i:Integer);
      procedure MakeFirstHistoryEntry(i:Integer);
      procedure MouseDown(Button:TMouseButton;Shift:TShiftState;x,y:Integer);override;
      procedure MouseUp(Button:TMouseButton;Shift:TShiftState;x,y:Integer);override;
      procedure MouseMove(Shift:TShiftState;x,y:Integer);override;
      function MoveInputCaretTo(x,y:Integer;chl:Boolean):Boolean;
      procedure SetSelection(Start,Ende:Integer);
      procedure LeftSelection(Start,Ende:Integer);
      procedure RightSelection(Start,Ende:Integer);
      procedure DeleteSelected;
      procedure SetOutY(v:Integer);
      procedure IntWrite;
      procedure MultiWrite;
      procedure SetCaretType(ACaretType:TCaretType);
      procedure SetCaretWidth(AValue:Integer);
      procedure SetCaretHeight(AValue:Integer);
      procedure SetCaretYShift(AValue:Integer);
      procedure SetTabWidth(AValue:Integer);
      function GetCaretInterval:Integer;
      procedure SetCaretInterval(AValue:Integer);

      public
      
      function HistoryHas(s:string):Boolean;
      function HistoryIndexOf(s:string):Integer;
      procedure ClearHistory;
      procedure TextColor(C:TColor);
      procedure TextBackground(C:TColor);
      procedure TextColors(FC,BC:TColor);
      procedure Write(s:String);
      procedure Writeln(s:String);
      procedure WriteStream(Stream:TStream);
      procedure Clear;
      procedure StartRead(DFC,DBC:TColor;const Desc:String;IFC,IBC:TColor);
      procedure StartReadPassWord(DFC,DBC:TColor;const Desc:String;IFC,IBC:TColor);
      procedure StopRead;
      procedure CopyToClipBoard;
      procedure PasteFromClipBoard;
      procedure CutToClipBoard;
      procedure ClearLine;
      property OutX               : Integer   read FOutX        write FOutX;
      property OutY               : Integer   read FOutY        write SetOutY;
      property TopLine            : Integer   read FTopLine     write SetTopLine;
      property History[i:Integer] : string    read GetHistory;
      property InputPos           : Integer   read FInputPos    write FInputPos;
      function HistoryCount       : Integer;
      published
      property Align;
      property Anchors;
      property ShowHint;
      property CaretColor              : TColor          read FCaretColor         write FCaretColor;
      property CaretType               : TCaretType      read FCaretType          write SetCaretType;
      property CaretWidth              : Integer         read FCaretWidth         write SetCaretWidth;
      property CaretHeight             : Integer         read FCaretHeight        write SetCaretHeight;
      property CaretYShift             : Integer         read FCaretYShift        write SetCaretYShift;
      property OnInput                 : EOnCmdBoxInput  read FOnInput            write FOnInput;
      property OnInputChange           : EOnCmdBoxInputChange read FOnInputChange write FOnInputChange;
      property OnAny                   : EOnCmdBoxInputChange read FOnAny         write FOnAny;
      property LineCount               : Integer         read FLineCount          write SetLineCount;
      property Font                    : TFont           read FFont               write SetFont;
      property BackGroundColor         : TColor          read FBackgroundColor    write SetBackGroundColor;
      property TabWidth                : Integer         read FTabWidth           write SetTabWidth;
      property PassWordChar            : TUTF8Char       read FPassWordChar       write FPassWordChar;
      property HistoryMax              : Integer         read FHistoryMax         write SetHistoryMax;
      property InputSelColor           : TColor          read FInputSelColor      write FInputSelColor;
      property InputSelBackGround      : TColor          read FInputSelBackGround write FInputSelBackGround;
      property CaretInterval           : Integer         read GetCaretInterval    write SetCaretInterval;
      property EscapeCodeType          : TEscapeCodeType read FEscapeCodeType     write FEscapeCodeType;
      property GraphicalCharacterWidth : Integer         read FGraphicCharWidth   write FGraphicCharWidth;
      property AutoFollow              : Boolean         read FAutoFollow         write FAutoFollow;
      property DoubleBuffered default true;
     end;
     
type TColorChar=record
      FChar        : TUTF8Char;
      FCharWidth   : Integer;
      FSumWidth    : Integer;
      FWordStart   : Integer;
      FFrontColor  : TColor;
      FBackColor   : TColor;
     end;
     
type TColorString=class
      private
      FChars         : array of TColorChar;
      FSumWidth      : Integer;
      FPassWordStart : Integer;
      FPassWordChar  : TUTF8Char;
      FTabWidth      : Integer;
      procedure MinimumLength(V:Integer;FC,BC:TColor);
      procedure MaximumLength(V:Integer);
      procedure UpdateSum;
      public
      constructor Create(AFont:TFont);
      destructor Destroy;override;
      procedure Clear;
      procedure OverWrite(S:String;Pos:Integer;FC,BC:TColor);
      procedure OverWriteChar(s:TUTF8Char;Pos,ADefWidth:Integer;FC,BC:TColor);
      procedure OverWrite(S:TColorstring;Pos:Integer);
      procedure OverWritePW(S:TColorstring;PWS,Pos:Integer;PWC:String);
      procedure PartOverWrite(S:TColorstring;Start,Ende,Pos:Integer);
      procedure LineOutAndFill(ACanvas:TCanvas;AX,AY,ALeftX,AWrapWidth,ACH,ACB,ACaretPos:Integer;ABC,ACC:TColor;ACaretHeight,ACaretWidth,ACaretYShift:Integer;ADrawCaret:Boolean);
      function Getstring:String;
      function GetPartstring(Start,Ende:Integer):String;
      procedure Delete(Index:Integer);
      procedure Delete(Index,Len:Integer);
      procedure Insert(Index:Integer;C:String;FC,BC:TColor);
      procedure BColorBlock(StartPos,EndPos:Integer;C:TColor);
      procedure ColorBlock(StartPos,EndPos:Integer;FC,BC:TColor);
      function LineCount(AWrapWidth,ACaretPos,ACaretWidth:Integer):Integer;
      function GetLength:Integer;
      function GetLineOfCaret(AWrapWidth,ACaretPos,ACaretWidth:Integer):Integer;
      function GetCharPosition(AWrapWidth,ALine,AXPos:Integer):Integer;
      private
      FFont              : TFont;
      FDefaultBackGround : TColor;
      public
      property TabWidth          : Integer read FTabWidth write FTabWidth;
      property PassWordChar      : TUTF8Char read FPassWordChar write FPassWordChar;
      property PassWordStart     : Integer read FPassWordStart write FPassWordStart;
      property Length            : Integer read GetLength;
      property DefaultBackGround : TColor read FDefaultBackground write FDefaultBackground;
      property Font              : TFont read FFont write FFont;
     end;

procedure Register;

implementation

procedure TColorString.UpdateSum;
var i             : Integer;
    LastWordStart : Integer;
    SumWidth      : Integer;
begin
 LastWordStart := 0;
 SumWidth      := 0;
 for i:=0 to High(FChars) do
 begin
  with FChars[i] do
  begin
   FWordStart := LastWordStart;
   case FChar[1] of
    #9:
    begin
     FCharWidth    := (SumWidth div FTabWidth+1)*FTabWidth-SumWidth;
     LastWordStart := i+1;
    end;
    #27:
    begin
     case FChar[2] of
      #9:
      begin
       FCharWidth    := (SumWidth div FTabWidth+1)*FTabWidth-SumWidth;
       LastWordStart := i+1;
      end;
      #10:LastWordStart:=i+1;
      #32,#46,#196,#205:
      begin
       FCharWidth    := Ord(FChar[3]);
       LastWordStart := i+1;
      end;
      #33,#47,#197,#206:
      begin
       FCharWidth    := (Ord(FChar[3])+Ord(FChar[4])*256)-SumWidth;
       if FCharWidth<0 then FCharWidth:=0;
       LastWordStart := i+1;
      end;
     end;
    end;
    else if FChar=' ' then LastWordStart:=i+1;
   end;
   SumWidth   := SumWidth+FCharWidth;
   FSumWidth  := SumWidth;
  end;
 end;
 FSumWidth:=SumWidth;
end;

function TColorString.GetLength:Integer;
begin
 Result:=System.Length(FChars);
end;

procedure TCmdBox.SetTabWidth(AValue:Integer);
var i:Integer;
begin
 FTabWidth:=AValue;
 for i:=0 to FLineCount-1 do FLines[i].TabWidth:=AValue;
 UpdateLineHeights;
 Invalidate;
end;

procedure TCmdBox.SetCaretWidth(AValue:Integer);
begin
 FCaretWidth := AValue;
 FCaretType  := cartUser;
end;

procedure TCmdBox.SetCaretHeight(AValue:Integer);
begin
 FCaretHeight := AValue;
 FCaretType   := cartUser;
end;

procedure TCmdBox.SetCaretYShift(AValue:Integer);
begin
 FCaretYShift := AValue;
 FCaretType   := cartUser;
end;

procedure TCmdBox.SetCaretType(ACaretType:TCaretType);
begin
 case ACaretType of
  cartLine:
  begin
   if HandleAllocated then FCaretHeight:=FFont.GetTextHeight('A')-3 else FCaretHeight:=-1;
   FCaretWidth  := 1;
   FCaretYShift := 3;
  end;
  cartSubBar:
  begin
   FCaretWidth  := -1;
   FCaretHeight := 3;
   FCaretYShift := 0;
  end;
  cartBigBar:
  begin
   if HandleAllocated then FCaretHeight:=FFont.GetTextHeight('A')-3 else FCaretHeight:=-1;
   FCaretWidth  := -1;
   FCaretYShift := 3;
  end;
 end;
 Invalidate;
 FCaretType:=ACaretType;
end;

// TOdo : Use string buffer instead of string (speed improvement expected)
procedure TColorString.LineOutAndFill(ACanvas:TCanvas;AX,AY,ALeftX,AWrapWidth,ACH,ACB,ACaretPos:Integer;ABC,ACC:TColor;ACaretHeight,ACaretWidth,ACaretYShift:Integer;ADrawCaret:Boolean);
var LineStart         : Integer;
    LineEnd           : Integer;
    MidWidth          : Integer;
    LineStartSumWidth : Integer;
    x                 : Integer;
    LastLineSumWidth  : Integer;
    ACHH              : Integer;
    ACBH              : Integer;

procedure DrawLine;
var SameColor      : String;
    SameForeColor  : TColor;
    SameBackColor  : TColor;
    SameColorX     : Integer;
    SameColorWidth : Integer;
    LP             : Integer;
    CaretX         : Integer;
    CaretW         : Integer;
    CW             : Integer;
    xp             : Integer;
begin
 if (AY<=-ACH) and (AY>ACanvas.Height) then
 begin
  Inc(AY,ACH);
  Ax:=ALeftx;
  Exit;
 end;
 SameColor:='';
 ACanvas.Brush.Style:=bsSolid;
 // TODO: Please try to reproduce this Ultra-Shit Error which couples GetTextWidth input to
 //       TextOut Output. I can't solve this...i am not Stupid Enough for such errors!
 // (Is in bug report, should be fixed somewhere in the future!)
 ACanvas.Font.GetTextWidth('%%%_$%_Hallo\\\\\\\\\32489738');
 // End of shit
 LP     := LineStart;
 CaretX := -1;
 while LineStart<>LineEnd+1 do
 begin
  with FChars[LineStart] do
  begin
   CW:=FCharWidth;
   if FChar=#9 then
   begin
    if SameColor<>'' then
    begin
     ACanvas.Font.Color  :=SameForeColor;
     ACanvas.Brush.Color :=SameBackColor;
     ACanvas.FillRect(SameColorX,AY,SameColorX+SameColorWidth,Ay+ACH);
     ACanvas.TextOut(SameColorX,AY,SameColor);
     Inc(SameColorX,SameColorWidth);
     SameColor:='';
    end else SameColorX:=AX;
    ACanvas.Brush.Color:=FBackColor;
    ACanvas.Fillrect(SameColorX,AY,SameColorX+FCharWidth,AY+ACH);
   end
   else
   if FChar[1]=#27 then
   begin
    if SameColor<>'' then
    begin
     ACanvas.Font.Color  :=SameForeColor;
     ACanvas.Brush.Color :=SameBackColor;
     ACanvas.FillRect(SameColorX,AY,SameColorX+SameColorWidth,Ay+ACH);
     ACanvas.TextOut(SameColorX,AY,SameColor);
     Inc(SameColorX,SameColorWidth);
     SameColor:='';
    end else SameColorX:=AX;
    case FChar[2] of
     #9:
     begin
      case FChar[3] of
       #46:
       begin
        ACanvas.Pen.Color:=FFrontColor;
        ACanvas.Pen.Style:=psDash;
        ACanvas.Brush.Color:=FBackColor;
        ACanvas.Fillrect(SameColorX,AY,SameColorX+FCharWidth,AY+ACH);
        xp:=SameColorX;
        if xp mod 2<>0 then Inc(xp);
        while xp<SameColorX+FCharWidth do
        begin
         ACanvas.Pixels[xp,AY+ACH-3]:=FFrontColor;
         Inc(xp,2);
        end;
       end;
       #196:
       begin
        ACanvas.Pen.Color   := FFrontColor;
        ACanvas.Pen.Style   := psSolid;
        ACanvas.Brush.Color := FBackColor;
        ACanvas.Fillrect(SameColorX,AY,SameColorX+FCharWidth,AY+ACH);
        ACanvas.Line(SameColorX,AY+ACHH,SameColorX+FCharWidth,AY+ACHH);
       end;
      end;
     end;
     #10:
     begin
      CW:=AWrapWidth-SameColorX;
      case FChar[3] of
       #179:
       begin
        ACanvas.Pen.Color   := FFrontColor;
        ACanvas.Brush.Color := FBackColor;
        ACanvas.Fillrect(SameColorX,AY,SameColorX+CW,AY+ACH);
        ACanvas.Line(SameColorX+CW-ACBH,AY,SameColorX+CW-ACBH,AY+ACH);
       end;
       #180:
       begin
        ACanvas.Pen.Color   := FFrontColor;
        ACanvas.Brush.Color := FBackColor;
        ACanvas.Fillrect(SameColorX,AY,SameColorX+CW,AY+ACH);
        ACanvas.Line(SameColorX+CW-ACBH,AY,SameColorX+CW-ACBH,AY+ACH);
        ACanvas.Line(SameColorX,AY+ACHH,SameColorX+CW-ACBH,AY+ACHH);
       end;
       #191:
       begin
        ACanvas.Pen.Color   := FFrontColor;
        ACanvas.Brush.Color := FBackColor;
        ACanvas.Fillrect(SameColorX,AY,SameColorX+CW,AY+ACH);
        ACanvas.MoveTo(SameColorX,AY+ACHH);
        ACanvas.LineTo(SameColorX+CW-ACBH,AY+ACHH);
        ACanvas.LineTo(SameColorX+CW-ACBH,AY+ACH);
       end;
       #196:
       begin
        ACanvas.Pen.Color   := FFrontColor;
        ACanvas.Brush.Color := FBackColor;
        ACanvas.Fillrect(SameColorX,AY,SameColorX+CW,AY+ACH);
        ACanvas.Line(SameColorX,AY+ACHH,SameColorX+CW,AY+ACHH);
       end;
       #205:
       begin
        ACanvas.Pen.Color   := FFrontColor;
        ACanvas.Brush.Color := FBackColor;
        ACanvas.Fillrect(SameColorX,AY,SameColorX+CW,AY+ACH);
        ACanvas.Line(SameColorX,AY+ACHH-1,SameColorX+CW,AY+ACHH-1);
        ACanvas.Line(SameColorX,AY+ACHH+1,SameColorX+CW,AY+ACHH+1);
       end;
       #217:
       begin
        ACanvas.Pen.Color   := FFrontColor;
        ACanvas.Brush.Color := FBackColor;
        ACanvas.Fillrect(SameColorX,AY,SameColorX+CW,AY+ACH);
        ACanvas.MoveTo(SameColorX,AY+ACHH);
        ACanvas.LineTo(SameColorX+CW-ACBH,AY+ACHH);
        ACanvas.LineTo(SameColorX+CW-ACBH,AY-1);
       end;
      end;
     end;
     #32,#33:
     begin
      ACanvas.Brush.Color:=FBackColor;
      ACanvas.Fillrect(SameColorX,AY,SameColorX+FCharWidth,AY+ACH);
     end;
     #46,#47:
     begin
      ACanvas.Pen.Color:=FFrontColor;
      ACanvas.Brush.Color:=FBackColor;
      ACanvas.Fillrect(SameColorX,AY,SameColorX+FCharWidth,AY+ACH);
      xp:=SameColorX;
      if xp mod 2<>0 then Inc(xp);
      while xp<SameColorX+FCharWidth do
      begin
       ACanvas.Pixels[xp,AY+ACH-3]:=FFrontColor;
       Inc(xp,2);
      end;
     end;
     #196,#197:
     begin
      ACanvas.Pen.Color   := FFrontColor;
      ACanvas.Brush.Color := FBackColor;
      ACanvas.Fillrect(SameColorX,AY,SameColorX+FCharWidth,AY+ACH);
      ACanvas.Line(SameColorX,AY+ACHH,SameColorX+FCharWidth,AY+ACHH);
     end;
     #179:
     begin
      ACanvas.Pen.Color   := FFrontColor;
      ACanvas.Brush.Color := FBackColor;
      ACanvas.Fillrect(SameColorX,AY,SameColorX+FCharWidth,AY+ACH);
      ACanvas.Line(SameColorX+ACBH,AY,SameColorX+ACBH,AY+ACH);
     end;
     #193:
     begin
      ACanvas.Pen.Color   := FFrontColor;
      ACanvas.Brush.Color := FBackColor;
      ACanvas.Fillrect(SameColorX,AY,SameColorX+FCharWidth,AY+ACH);
      ACanvas.Line(SameColorX,AY+ACHH,SameColorX+ACB,AY+ACHH);
      ACanvas.Line(SameColorX+ACBH,AY,SameColorX+ACBH,AY+ACHH);
     end;
     #194:
     begin
      ACanvas.Pen.Color   := FFrontColor;
      ACanvas.Brush.Color := FBackColor;
      ACanvas.Fillrect(SameColorX,AY,SameColorX+FCharWidth,AY+ACH);
      ACanvas.Line(SameColorX,AY+ACHH,SameColorX+ACB,AY+ACHH);
      ACanvas.Line(SameColorX+ACBH,AY+ACHH,SameColorX+ACBH,AY+ACH);
     end;
     #198:
     begin
      ACanvas.Pen.Color   := FFrontColor;
      ACanvas.Brush.Color := FBackColor;
      ACanvas.Fillrect(SameColorX,AY,SameColorX+FCharWidth,AY+ACH);
      ACanvas.Line(SameColorX,AY+ACHH,SameColorX+ACB,AY+ACHH);
      ACanvas.Line(SameColorX+ACBH,AY,SameColorX+ACBH,AY+ACH);
     end;
     #195:
     begin
      ACanvas.Pen.Color   := FFrontColor;
      ACanvas.Brush.Color := FBackColor;
      ACanvas.Fillrect(SameColorX,AY,SameColorX+FCharWidth,AY+ACH);
      ACanvas.Line(SameColorX+ACBH,AY,SameColorX+ACBH,AY+ACH);
      ACanvas.Line(SameColorX+ACBH,AY+ACHH,SameColorX+ACB,AY+ACHH);
     end;
     #217:
     begin
      ACanvas.Pen.Color   := FFrontColor;
      ACanvas.Brush.Color := FBackColor;
      ACanvas.Fillrect(SameColorX,AY,SameColorX+FCharWidth,AY+ACH);
      ACanvas.MoveTo(SameColorX+ACBH,AY);
      ACanvas.LineTo(SameColorX+ACBH,AY+ACHH);
      ACanvas.LineTo(SameColorX+ACB,AY+ACHH);
     end;
     #218:
     begin
      ACanvas.Pen.Color   := FFrontColor;
      ACanvas.Brush.Color := FBackColor;
      ACanvas.Fillrect(SameColorX,AY,SameColorX+FCharWidth,AY+ACH);
      ACanvas.MoveTo(SameColorX+ACBH,AY+ACH);
      ACanvas.LineTo(SameColorX+ACBH,AY+ACHH);
      ACanvas.LineTo(SameColorX+ACB,AY+ACHH);
     end;
    end;
   end
   else
   if SameColor='' then
   begin
    if (LP>=FPassWordStart) then
    begin
     SameColor      := FPassWordChar;
     SameColorWidth := FFont.GetTextWidth(FPassWordChar);
    end
    else
    begin
     SameColor      := FChar;
     SameColorWidth := FCharWidth;
    end;
    SameColorX    := AX;
    SameForeColor := FFrontColor;
    SameBackColor := FBackColor;
   end
   else
   begin
    if (SameForeColor=FFrontColor) and (SameBackColor=FBackColor) then
    begin
     if (LP>=FPassWordStart) then
     begin
      SameColor:=SameColor+FPassWordChar;
      Inc(SameColorWidth,FFont.GetTextWidth(FPassWordChar));
     end
     else
     begin
      SameColor:=SameColor+FChar;
      Inc(SameColorWidth,FCharWidth);
     end;
    end
    else
    begin
     ACanvas.Font.Color  :=SameForeColor;
     ACanvas.Brush.Color :=SameBackColor;
     ACanvas.FillRect(SameColorX,Ay,SameColorX+SameColorWidth,Ay+ACH);
     ACanvas.TextOut(SameColorX,AY,SameColor);
     if (LP>=FPassWordStart) then
     begin
      SameColor      := FPassWordChar;
      SameColorWidth := FFont.GetTextWidth(FPassWordChar);
     end
     else
     begin
      SameColor      := FChar;
      SameColorWidth := FCharWidth;
     end;
     SameForeColor  := FFrontColor;
     SameBackColor  := FBackColor;
     SameColorX     := AX;
    end;
   end;
   if LP=ACaretPos then
   begin
    CaretX:=AX;
    CaretW:=FCharWidth;
   end;
   Inc(AX,CW);
   Inc(LP);
  end;
  Inc(LineStart);
 end;
 if SameColor<>'' then
 begin
  ACanvas.Font.Color  := SameForeColor;
  ACanvas.Brush.Color := SameBackColor;
  ACanvas.FillRect(SameColorX,Ay,SameColorX+SameColorWidth,Ay+ACH);
  ACanvas.TextOut(SameColorX,AY,SameColor);
 end;
 ACanvas.FillRect(AX,AY,AWrapWidth,AY+ACH);
 AX:=ALeftX;
 Inc(AY,ACH);
 if ADrawCaret and (CaretX>=0) then
 begin
  ACanvas.Brush.Color:=ACC;
  if ACaretWidth>=0 then CaretW:=ACaretWidth;
  ACanvas.FillRect(CaretX,AY-ACaretHeight-ACaretYShift,CaretX+CaretW,AY-ACaretYShift);
 end;
end;

begin
 if AWrapWidth<0 then AWrapWidth:=0;
 if System.Length(FChars)=0 then
 begin
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := ABC;
  ACanvas.FillRect(AX,AY,AWrapWidth,AY+ACH);
  Exit;
 end;
 ACHH              := ACH div 2;
 ACBH              := ACB div 2;
 MidWidth          := FSumWidth div System.Length(FChars);
 LineStart         := 0;
 LineStartSumWidth := 0;
 LastLineSumWidth  := 0;
 x                 := 0;
 while LineStart<System.Length(FChars) do
 begin
  x:=LineStart+AWrapWidth div MidWidth;
  if x>High(FChars) then x:=High(FChars);
  while (x<High(FChars)) and (FChars[x].FSumWidth-LineStartSumWidth<AWrapWidth) do Inc(x);
  while (x>LineStart) and (FChars[x].FSumWidth-LineStartSumWidth>=AWrapWidth) do
   with FChars[x] do if (FChar<>' ') and (FWordStart>LineStart) then x:=FWordStart-1 else Dec(x);
  LineEnd:=x;
  DrawLine;
  LastLineSumWidth  := LineStartSumWidth;
  LineStartSumWidth := FChars[x].FSumWidth;
  LineStart         := x+1;
 end;
 if ACaretPos>=LineStart then
 begin
  if ACaretWidth>=0 then x:=ACaretWidth else x:=FFont.GetTextWidth('A');
  AX:=LineStartSumWidth-LastLineSumWidth+(ACaretPos-LineStart)*x;
  if Ax+x>AWrapWidth then
  begin
   Ax:=0;
   ACanvas.Brush.Color:=ABC;
   ACanvas.FillRect(0,AY,AWrapWidth,AY+ACH);
   Inc(Ay,ACH);
  end;
  if ADrawCaret then
  begin
   ACanvas.Brush.Color:=ACC;
   ACanvas.FillRect(AX,AY-ACaretHeight-ACaretYShift,AX+x,AY-ACaretYShift);
  end;
 end;
end;

function TColorString.GetCharPosition(AWrapWidth,ALine,AXPos:Integer):Integer;
var x,MidWidth,LineStart,LineStartSumWidth,LastLineSumWidth,LastLineStart:Integer;
begin
 if AWrapWidth<0 then AWrapWidth:=0;
 if System.Length(FChars)=0 then
 begin
  Result:=0;
  Exit;
 end;
 MidWidth := FSumWidth div System.Length(FChars);
 if MidWidth=0 then
 begin
  Result:=0;
  Exit;
 end;
 LineStart         := 0;
 LineStartSumWidth := 0;
 LastLineSumWidth  := 0;
 x:=0;
 while (LineStart<System.Length(FChars)) and (ALine>=0) do
 begin
  x:=LineStart+AWrapWidth div MidWidth;
  if x>High(FChars) then x:=High(FChars);
  while (x<High(FChars)) and (FChars[x].FSumWidth-LineStartSumWidth<AWrapWidth) do Inc(x);
  while (x>LineStart) and (FChars[x].FSumWidth-LineStartSumWidth>=AWrapWidth) do
   with FChars[x] do if (FChar<>' ') and (FWordStart>LineStart) then x:=FWordStart-1 else Dec(x);
  LastLineSumWidth  := LineStartSumWidth;
  LineStartSumWidth := FChars[x].FSumWidth;
  LastLineStart     := LineStart;
  LineStart         := x+1;
  Dec(ALine);
 end;
 Result:=LastLineStart;
 while (Result<LineStart) and (FChars[Result].FSumWidth-LastLineSumWidth<=AXPos) do Inc(Result);
end;

function TColorString.GetLineOfCaret(AWrapWidth,ACaretPos,ACaretWidth:Integer):Integer;
var x,MidWidth,LineStart,LineStartSumWidth,LastLineSumWidth:Integer;
begin
 if AWrapWidth<0 then AWrapWidth:=0;
 if System.Length(FChars)=0 then
 begin
  Result:=0;
  Exit;
 end;
 MidWidth := FSumWidth div System.Length(FChars);
 if MidWidth=0 then
 begin
  Result:=0;
  Exit;
 end;
 LineStart         := 0;
 LineStartSumWidth := 0;
 LastLineSumWidth  := 0;
 Result            := 0;
 x:=0;
 while LineStart<System.Length(FChars) do
 begin
  x:=LineStart+AWrapWidth div MidWidth;
  if x>High(FChars) then x:=High(FChars);
  while (x<High(FChars)) and (FChars[x].FSumWidth-LineStartSumWidth<AWrapWidth) do Inc(x);
  while (x>LineStart) and (FChars[x].FSumWidth-LineStartSumWidth>=AWrapWidth) do
   with FChars[x] do if (FChar<>' ') and (FWordStart>LineStart) then x:=FWordStart-1 else Dec(x);
  LastLineSumWidth  := LineStartSumWidth;
  LineStartSumWidth := FChars[x].FSumWidth;
  LineStart         := x+1;
  if ACaretPos<x then Exit;
  Inc(Result);
 end;
 if ACaretWidth>=0 then x:=ACaretWidth else x:=FFont.GetTextWidth('A');
 if (ACaretPos>LineStart) or (LineStartSumWidth-LastLineSumWidth+(ACaretPos-LineStart)*x+x<=AWrapWidth) then Dec(Result);
end;

function TColorString.LineCount(AWrapWidth,ACaretPos,ACaretWidth:Integer):Integer;
var x                 : Integer;
    MidWidth          : Integer;
    LineStart         : Integer;
    LineStartSumWidth : Integer;
    LastLineSumWidth  : Integer;
begin
 if AWrapWidth<0 then AWrapWidth:=0;
 if System.Length(FChars)=0 then
 begin
  Result:=1;
  Exit;
 end;
 MidWidth := FSumWidth div System.Length(FChars);
 if MidWidth=0 then
 begin
  Result:=1;
  Exit;
 end;
 LineStart         := 0;
 LineStartSumWidth := 0;
 LastLineSumWidth  := 0;
 Result:=0;
 x:=0;
 while LineStart<System.Length(FChars) do
 begin
  x:=LineStart+AWrapWidth div MidWidth;
  if x>High(FChars) then x:=High(FChars);
  while (x<High(FChars)) and (FChars[x].FSumWidth-LineStartSumWidth<AWrapWidth) do Inc(x);
  while (x>LineStart) and (FChars[x].FSumWidth-LineStartSumWidth>=AWrapWidth) do
   with FChars[x] do if (FChar<>' ') and (FWordStart>LineStart) then x:=FWordStart-1 else Dec(x);
  LastLineSumWidth  := LineStartSumWidth;
  LineStartSumWidth := FChars[x].FSumWidth;
  LineStart         := x+1;
  Inc(Result);
 end;
 if ACaretWidth>=0 then x:=ACaretWidth else x:=FFont.GetTextWidth('A');
 if (ACaretPos>=LineStart) and (LineStartSumWidth-LastLineSumWidth+(ACaretPos-LineStart)*x+x>AWrapWidth) then Inc(Result);
end;

constructor TColorString.Create(AFont:TFont);
begin
 inherited Create;
 FTabWidth      := 1;
 FFont          := AFont;
 FPassWordStart := MaxInt;
end;

procedure TColorstring.BColorBlock(StartPos,EndPos:Integer;C:TColor);
var i:Integer;
begin
 if StartPos<0 then StartPos:=0;
 if EndPos>High(FChars) then EndPos:=High(FChars);
 for i:=StartPos to EndPos do FChars[i].FBackColor:=C;
end;

procedure TColorstring.ColorBlock(StartPos,EndPos:Integer;FC,BC:TColor);
var i:Integer;
begin
 if StartPos<0 then StartPos:=0;
 if EndPos>High(FChars) then EndPos:=High(FChars);
 for i:=StartPos to EndPos do
 begin
  FChars[i].FFrontColor := FC;
  FChars[i].FBackColor  := BC;
 end;
end;

procedure TColorstring.Insert(Index:Integer;C:string;FC,BC:TColor);
var i      : Integer;
    l      : Integer;
    Pp     : Integer;
    OldLen : Integer;
    SLen   : Integer;
begin
 OldLen := System.Length(FChars);
 SLen   := UTF8Length(C);
 if OldLen<Index then MinimumLength(Index+SLen,FC,BC) else
 begin
  MinimumLength(SLen+OldLen,FC,BC);
  for i:=OldLen-1 downto Index do FChars[i+SLen]:=FChars[i];
 end;
 pp := 1;
 for i:=0 to SLen-1 do
 begin
  l:=UTF8CharacterLength(@C[Pp]);
  with FChars[Index+i] do
  begin
   FChar := Copy(C,Pp,l);
   if Index+i>=FPassWordStart then FCharWidth:=FFont.GetTextWidth(FPassWordChar) else FCharWidth:=FFont.GetTextWidth(FChar);
   FFrontColor := FC;
   FBackColor  := BC;
  end;
  Inc(pp,l);
 end;
 UpdateSum;
end;

procedure TColorstring.Delete(Index,Len:Integer);
var i:Integer;
begin
 if (Len=0) or (Index>=System.Length(FChars)) then Exit;
 if Index+Len>System.Length(FChars) then Len:=System.Length(FChars)-Index;
 for i:=Index to System.Length(FChars)-Len-1 do FChars[i]:=FChars[i+Len];
 SetLength(FChars,System.Length(FChars)-Len);
 UpdateSum;
end;

procedure TColorstring.Delete(Index:Integer);
var i:Integer;
begin
 if (Index>=System.Length(FChars)) then Exit;
 for i:=Index to System.Length(FChars)-2 do FChars[i]:=FChars[i+1];
 SetLength(FChars,System.Length(FChars)-1);
 UpdateSum;
end;

function TColorstring.GetPartstring(Start,Ende:Integer):string;
var i,n : Integer;
    Len : Integer;
begin
 if Start<0 then Start:=0;
 if Ende>High(FChars) then Ende:=High(FChars);
 Len:=0;
 for i:=Start to Ende do Inc(Len,System.Length(FChars[i].FChar));
 SetLength(Result,Len);
 Len:=1;
 for i:=Start to Ende do
 begin
  with FChars[i] do
  begin
   for n:=1 to System.Length(FChar) do
   begin
    Result[Len]:=FChar[n];
    Inc(Len);
   end;
  end;
 end;
end;

function TColorstring.Getstring:String;
var i,n : Integer;
    Len : Integer;
begin
 Len := 0;
 for i:=0 to High(FChars) do Inc(Len,System.Length(FChars[i].FChar));
 SetLength(Result,Len);
 Len := 1;
 for i:=0 to High(FChars) do
 begin
  with FChars[i] do
  begin
   for n:=1 to System.Length(FChar) do
   begin
    Result[Len]:=FChar[n];
    Inc(Len);
   end;
  end;
 end;
end;

procedure TColorstring.OverWritePW(S:TColorstring;PWS,Pos:Integer;PWC:String);
var i    : Integer;
    CPassWordStart:Integer;
begin
 MinimumLength(Pos+S.Length,CLSilver,S.FDefaultBackGround);
 CPassWordStart:=PWS;
 for i:=0 to S.Length-1 do
 begin
  FChars[i+Pos]:=S.FChars[i];
  if CPassWordStart<=0 then FChars[i+Pos].FChar:=PWC;
  Dec(CPassWordStart);
 end;
 UpdateSum;
end;

procedure TColorstring.OverWrite(S:TColorstring;Pos:Integer);
var i : Integer;
begin
 MinimumLength(Pos+S.Length,CLSilver,S.FDefaultBackGround);
 for i:=0 to S.Length-1 do FChars[i+Pos]:=S.FChars[i];
 UpdateSum;
end;

procedure TColorstring.PartOverWrite(S:TColorstring;Start,Ende,Pos:Integer);
var i : Integer;
begin
 MinimumLength(Pos+Ende-Start,CLSilver,S.FDefaultBackGround);
 for i:=0 to Ende-Start-1 do FChars[i+Pos]:=S.FChars[i+Start];
 UpdateSum;
end;

procedure TColorstring.OverWrite(s:String;Pos:Integer;FC,BC:TColor);
var i,Pp,l : Integer;
begin
 MinimumLength(Pos+UTF8Length(S),FC,BC);
 Pp:=1;
 for i:=0 to UTF8Length(S)-1 do
 begin
  l:=UTF8CharacterLength(@s[Pp]);
  with FChars[i+Pos] do
  begin
   FChar       := Copy(S,Pp,l);
   FCharWidth  := FFont.GetTextWidth(FChar);
   FFrontColor := FC;
   FBackColor  := BC;
  end;
  Inc(Pp,l);
 end;
 UpdateSum;
end;

procedure TColorstring.OverWriteChar(s:TUTF8Char;Pos,ADefWidth:Integer;FC,BC:TColor);
begin
 MinimumLength(Pos+1,FC,BC);
 with FChars[Pos] do
 begin
  FChar       := s;
  FCharWidth  := ADefWidth;
  FFrontColor := FC;
  FBackColor  := BC;
 end;
 UpdateSum;
end;

procedure TColorstring.MinimumLength(V:Integer;FC,BC:TColor);
var OldLen,i:Integer;
begin
 if System.Length(FChars)<V then
 begin
  OldLen:=System.Length(FChars);
  SetLength(FChars,V);
  for i:=OldLen to High(FChars) do
  begin
   with FChars[i] do
   begin
    FChar       := ' ';
    FCharWidth  := FFont.GetTextWidth(' ');
    FFrontColor := FC;
    FBackColor  := BC;
   end;
  end;
 end;
end;

procedure TColorstring.MaximumLength(V:Integer);
begin
 if System.Length(FChars)>V then SetLength(FChars,V);
end;

procedure TColorstring.Clear;
begin
 FChars:=Nil;
end;

procedure TCmdBox.ClearLine;
begin
 if FLines[FOutY].Length<>0 then
 begin
  FLines[FOutY].Clear;
  FOutX:=0;
  if FInput then FInputY:=FOutY;
  Invalidate;
 end;
end;

function TCmdBox.GetCaretInterval:Integer;
begin
 Result:=FCaretTimer.Interval;
end;

procedure TCmdBox.SetCaretInterval(AValue:Integer);
begin
 FCaretTimer.Interval:=AValue;
end;

procedure TCmdBox.MultiWrite;
var DoWrite:Boolean;
begin
 System.EnterCriticalSection(FLock);
 DoWrite:=FStringBuffer.Count<>0;
 if DoWrite then
 begin
  FCurrentString:=FStringBuffer[0];
  FStringBuffer.Delete(0);
 end;
 System.LeaveCriticalSection(FLock);
 if DoWrite then IntWrite;
end;

procedure TCmdBox.Write(S:String);
begin
 if ThreadID=MainThreadId then
 begin
  MultiWrite;
  FCurrentString:=S;
  IntWrite;
 end
 else
 begin
  System.EnterCriticalSection(FLock);
  FStringBuffer.Add(S);
  System.LeaveCriticalSection(FLock);
  if Assigned(WakeMainThread) then TThread.Synchronize(Nil,@MultiWrite);
 end;
end;

function TCmdBox.HistoryIndexOf(s:string):Integer;
begin
 for Result:=0 to HistoryCount-1 do if History[Result]=s then Exit;
 Result:=-1;
end;

function TCmdBox.HistoryHas(s:string):Boolean;
var i:Integer;
begin
 Result := True;
 for i:=0 to HistoryCount-1 do if History[i]=s then Exit;
 Result := False;
end;

function TCmdBox.HistoryCount:Integer;
begin
 HistoryCount:=FHistoryLength-Ord(FInput);
end;

function TCmdBox.GetHistory(i:Integer):string;
begin
 Inc(i,Ord(FInput));
 if (i>=0) and (i<FHistoryLength) then GetHistory:=FHistory[i].Getstring else GetHistory:='';
end;

procedure TCmdBox.EraseBackGround(DC:HDC);
begin
end;

procedure TCmdBox.ClearHistory;
begin
 FHistoryLength := Ord(FInput);
 FHistoryPos    := 0;
end;

procedure TCmdBox.SetHistoryMax(v:Integer);
var i:Integer;
begin
 if v<1 then v:=1;
 if v<>FHistoryMax then
 begin
  if FHistoryLength>v then FHistoryLength:=v;
  for i:=v to FHistoryMax-1 do FHistory[i].Free;
  SetLength(FHistory,v);
  for i:=FHistoryMax to v-1 do FHistory[i]:=TColorstring.Create(Canvas.Font);
  FHistoryMax:=v;
 end;
end;

procedure TCmdBox.WriteStream(Stream:TStream);
var c:wideString;
begin
 while Stream.Position<Stream.Size do
 begin
  // Not very efficient, but should work...
  Stream.Read(c,1);
  Write(c);
 end;
end;

procedure TCmdBox.LeftSelection(Start,Ende:Integer);
begin
 if FSelStart=-1 then
 begin
  SetSelection(Start,Ende);
 end
 else
 begin
  if FSelStart=Start then SetSelection(-1,0) else
  begin
   if FSelStart<Start then
   begin
    SetSelection(FSelStart,Start);
   end else SetSelection(Start,FSelEnd+1);
  end;
 end;
end;

procedure TCmdBox.RightSelection(Start,Ende:Integer);
begin
 if FSelStart=-1 then
 begin
  SetSelection(Start,Ende);
 end
 else
 begin
  if FSelEnd+1=Ende then SetSelection(-1,0) else
  begin
   if FSelstart<Start then
   begin
    SetSelection(FSelStart,Ende);
   end else SetSelection(Ende,FSelEnd+1);
  end;
 end;
end;

procedure TCmdBox.SetSelection(Start,Ende:Integer);
begin
 if FSelStart<>-1 then FInputBuffer.ColorBlock(FSelStart,FSelEnd,FInputColor,FInputBackGround);
 if Start=Ende then FSelStart:=-1 else
 begin
  if Start<Ende then
  begin
   FSelStart := Start;
   FSelEnd   := Ende-1;
  end
  else
  begin
   FSelStart := Ende;
   FSelEnd   := Start-1;
  end;
 end;
 if FSelStart<>-1 then FInputBuffer.ColorBlock(FSelStart,FSelEnd,FInputSelColor,FInputSelBackGround);
end;

procedure TCmdBox.CopyToClipBoard;
begin
 if FSelStart<>-1 then
 begin
  ClipBoard.AsText:=FInputBuffer.GetPartstring(FSelStart,FSelEnd);
 end;
end;

procedure TCmdBox.PasteFromClipBoard;
var s:widestring;
    l,Pp:Integer;
begin
 if ClipBoard.HasFormat(CF_TEXT) then
 begin
  s  := ClipBoard.AsText;
  Pp := 1;
  while pp<=Length(s) do
  begin
   l := UTF8CharacterLength(@S[Pp]);
   if (l=1) and (Byte(S[Pp])<32) then Delete(s,Pp,1) else inc(Pp,l);
  end;
  FInputBuffer.Insert(InputPos,s,FInputColor,FInputBackGround);
  InputPos := InputPos + UTF8Length(s);
  FCaretX:=FInputX+InputPos;
  AdjustScrollBars;
  MakeInputVisible;
  FHistoryPos:=0;
  if Assigned(FOnInputChange) then FOnInputChange(Self,FInputBuffer);
  if Assigned(FOnAny) then FOnAny(Self,FInputBuffer);
 end;
end;

procedure TCmdBox.DeleteSelected;
begin
 if FSelStart<>-1 then
 begin
  FInputBuffer.Delete(FSelStart,FSelEnd-FSelStart+1);
  FInputPos := FSelStart;
  FCaretX   := FInputX+FInputPos;
  FSelStart := -1;
 end;
end;

procedure TCmdBox.CutToClipBoard;
begin
 if FSelStart<>-1 then
 begin
  ClipBoard.AsText:=FInputBuffer.GetPartstring(FSelStart,FSelEnd);
  DeleteSelected;
 end;
end;

procedure TCmdBox.MouseMove(Shift:TShiftState;x,y:Integer);
begin
 if FMouseDown then
 begin
  if MoveInputCaretTo(x,y,false) then SetSelection(FMouseDownInputPos,FInputPos);
 end;
end;

function TCmdBox.MoveInputCaretTo(x,y:Integer;chl:Boolean):Boolean;
var h,sl,q:Integer;
begin
 if not FInput then Exit;
 y  := y div FCharHeight;
 h  := FLineHeightSum[FTopLine]+FLineOfTopLine+y;
 sl := FTopLine;
 while (sl<FLineCount-1) and (FLineHeightSum[sl+1]<=h) do inc(sl);
 if (sl=FInputY) or (not chl) then
 begin
  Dec(h,FLineHeightSum[FInputY]);
  q:=FInputBuffer.GetCharPosition(FClientWidth,h,x);
  if (q<FInputMinPos) then q:=FInputMinPos;
  if (q-FInputX>FInputBuffer.Length) then q:=FInputBuffer.Length-FInputX;
  FCaretX   := q;
  FInputPos := FCaretX-FInputX;
  if Assigned(FOnAny) then FOnAny(Self,FInputBuffer);
  Invalidate;
  Result:=True;
 end else Result:=False;
end;

procedure TCmdBox.MouseDown(Button:TMouseButton;Shift:TShiftState;x,y:Integer);
begin
 SetFocus;
 MoveInputCaretTo(x,y,True);
 FMouseDown:=True;
 SetSelection(-1,0);
 FMouseDownInputPos:=FInputPos;
 Invalidate;
end;

procedure TCmdBox.MouseUp(Button:TMouseButton;Shift:TShiftState;x,y:Integer);
begin
 FMouseDown:=False;
end;

destructor TColorstring.Destroy;
begin
 Clear;
 inherited Destroy;
end;

procedure TCmdBox.ScrollUp;
var n:Integer;
    Firstwidestring : TColorstring;
begin
 Firstwidestring:=FLines[0];
 for n:=0 to Length(FLines)-2 do Flines[n] := FLines[n+1];
 Firstwidestring.Clear;
 Firstwidestring.FDefaultBackGround := FBackGroundColor;
 Flines[High(Flines)]:=Firstwidestring;
end;

procedure TCmdBox.TextColors(FC,BC:TColor);
begin
 FCurrentColor      := FC;
 FCurrentBackGround := BC;
end;

procedure TCmdBox.TextColor(C:TColor);
begin
 FCurrentColor:=C;
end;

procedure TCmdBox.TextBackGround(C:TColor);
begin
 FCurrentBackGround:=C;
end;

procedure TCmdBox.TranslateScrollBarPosition;
var GLine,Line : Integer;
    He         :Integer;
begin
 if (FLineOfTopLine<FLineHeights[FTopLine]) and (FLineHeightSum[FTopLine]+FLineOfTopLine=FVSBPos) then exit;
 UpdateLineHeights;
 Line  := 0;
 GLine := 0;
 He:=FLineHeights[Line];
 while (Line<LineCount-1) and (Gline+He<=FVSBPos) do
 begin
  Inc(Line);
  Inc(Gline,He);
  He:=FLineHeights[Line];
 end;
 FTopLine       := Line;
 FLineOfTopLine := FVSBPos-GLine;
 Invalidate;
end;

procedure TCmdBox.WMVScroll(var message: TLMVScroll);
var CurrentPos :Integer;
begin
 CurrentPos:=FLineHeightSum[FTopLine]+FLineOfTopLine;
 case message.ScrollCode of
  SB_TOP:           CurrentPos := 0;
  SB_BOTTOM:        CurrentPos := FVisibleLineCount-FPageHeight;
  SB_LINEDOWN:      Inc(CurrentPos);
  SB_LINEUP:        Dec(CurrentPos);
  SB_PAGEDOWN:      Inc(CurrentPos,FPageHeight);
  SB_PAGEUP:        Dec(CurrentPos,FPageHeight);
  SB_THUMBPOSITION: CurrentPos := message.Pos;
  SB_THUMBTRACK:    CurrentPos := message.Pos;
  SB_ENDSCROLL:     Exit;
 end;
 
 if CurrentPos<0 then CurrentPos:=0 else if Currentpos>FVisibleLineCount-FPageHeight then CurrentPos:=FVisibleLineCount-FPageHeight;
 {$IFNDEF LCLGTK}
 ScrollBarPosition(SB_VERT, CurrentPos);
 {$ENDIF}

 FVSBPos:=CurrentPos;
 TranslateScrollBarPosition;
end;

procedure TCmdBox.ScrollBarRange(Which: Integer; aRange,aPage: Integer);
var ScrollInfo: TScrollInfo;
begin
 if HandleAllocated then
 begin
  FillChar(ScrollInfo, SizeOf(ScrollInfo), 0);
  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.fMask  := SIF_RANGE or SIF_PAGE or SIF_DISABLENOSCROLL;
  // Dont't know, someone told me to kick it...so i did:P
//  {$ifdef Unix}
{  ScrollInfo.fMask := ScrollInfo.fMask or SIF_UPDATEPOLICY;
  if goThumbTracking in Options then
   ScrollInfo.ntrackPos := SB_POLICY_CONTINUOUS
  else
   ScrollInfo.ntrackPos := SB_POLICY_DISCONTINUOUS;}
//  {$endif}}
  ScrollInfo.nMin := 0;
  ScrollInfo.nMax := ARange;
  if APage<0 then APage := 0;
  ScrollInfo.nPage := APage;
  SetScrollInfo(Handle, Which, ScrollInfo, True);
 end;
end;

procedure TCmdBox.ScrollBarPosition(Which, Value: integer);
var ScrollInfo : TScrollInfo;
    Vis        : Boolean;
begin
 if HandleAllocated then
 begin
  if Which = SB_VERT then Vis := FVSbVisible else
  FillChar(ScrollInfo, SizeOf(ScrollInfo), 0);
  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.fMask  := SIF_POS;
  ScrollInfo.nPos   := Value;
  SetScrollInfo(Handle, Which, ScrollInfo, Vis);
 end;
end;

function TCmdBox.GetSystemMetricsGapSize(const Index:Integer):Integer;
begin
 {$ifdef LCLWIN32}
 result:=0;
 {$else}
 result:=3;
 {$endif}
end;

procedure TCmdBox.SetBackGroundColor(c:TColor);
begin
 if c<>FBackGroundColor then
 begin
  FBackGroundColor:=c;
  Invalidate;
 end;
end;

procedure TCmdBox.SetFont(F:TFont);
var DC          : HDC;
    Save        : THandle;
    Metrics     : TTextMetric;
begin
 FFont.Assign(F);
 Canvas.Font := FFont;
 DC          := GetDC(0);
 Save        := SelectObject(DC,FFont.Handle);
 GetTextMetrics(DC, Metrics);
 SelectObject(DC, Save);
 ReleaseDC(0, DC);
 FCharHeight := Abs(Metrics.tmHeight)+2;
 Invalidate;
end;

// Still a Bug: Try having a cmdline with more lines than fit on screen : update doesn't work anymore...

procedure TCmdBox.MakeInputVisible;
var y : Integer;
begin
 if not FAutoFollow then Exit;
 UpdateLineHeights;
 y:=FLineHeightSum[FInputY]+FInputBuffer.GetLineOfCaret(FClientWidth,FCaretX,FCaretWidth);
 if y>=FLineHeightSum[FTopLine]+FLineOfTopLine+FPageHeight then
 begin
  While y>=FLineHeightSum[FTopLine]+FLineHeights[FTopLine]+FPageHeight-1 do Inc(FTopLine);
  FLineOfTopLine:=y-(FLineHeightSum[FTopLine]+FPageHeight)+1;
 end
 else if y<FLineHeightSum[FTopLine]+FLineOfTopLine then
 begin
  FLineOfTopLine:=0;
  While y<FLineHeightSum[FTopLine] do Dec(FTopLine);
  FLineOfTopLine:=y-FLineHeightSum[FTopLine];
 end;
 y:=FLineHeightSUm[FTopLine]+FLineOfTopLine;
 if y<>FVSBPos then
 begin
  FVSBPos:=y;
  if HandleAllocated then ScrollBarPosition(SB_Vert,y);
 end;
end;

procedure TCmdBox.MakeOutVisible;
var y : Integer;
begin
 if not FAutoFollow then Exit;
 UpdateLineHeights;
 y:=FLineHeightSum[FOutY]+FLines[FOutY].GetLineOfCaret(FClientWidth,FOutX,FCaretWidth);
 if y>=FLineHeightSum[FTopLine]+FLineOfTopLine+FPageHeight then
 begin
  While y>=FLineHeightSum[FTopLine]+FLineHeights[FTopLine]+FPageHeight-1 do Inc(FTopLine);
  FLineOfTopLine:=y-(FLineHeightSum[FTopLine]+FPageHeight)+1;
 end
 else if y<FLineHeightSum[FTopLine]+FLineOfTopLine then
 begin
  FLineOfTopLine:=0;
  While y<FLineHeightSum[FTopLine] do Dec(FTopLine);
  FLineOfTopLine:=y-FLineHeightSum[FTopLine];
 end;
 y:=FLineHeightSUm[FTopLine]+FLineOfTopLine;
 if y<>FVSBPos then
 begin
  FVSBPos:=y;
  if HandleAllocated then ScrollBarPosition(SB_Vert,y);
 end;
end;

procedure TCmdBox.SetHistoryPos(v:Integer);
begin
 if FInputIsPassWord then Exit;
 if v<0 then v:=FHistoryLength-1 else if v>=FHistoryLength then v:=0;
 if v<>FHistoryPos then
 begin
  if FHistoryPos=0 then
  begin
   FHistory[0].Clear;
   FHistory[0].PartOverWrite(FInputBuffer,FInputMinPos,FInputBuffer.Length,0);
  end;
  FInputBuffer.MaximumLength(FInputMinPos+FHistory[v].Length);
  FInputBuffer.OverWrite(FHistory[v],FInputMinPos);
  if FInputPos>FInputBuffer.Length then
  begin
   FInputPos:=FInputBuffer.Length;
   FCaretX:=FInputX+FInputPos;
  end;
  FHistoryPos:=v;
 end;
 if Assigned(FOnInputChange) then FOnInputChange(Self,FInputBuffer);
 MakeInputVisible;
 AdjustLineHeight(FInputY);
 AdjustScrollBars;
 Invalidate;
end;

procedure TCmdBox.UTF8KeyPress(var Key:TUTF8Char);
begin
 if not FInput then Exit;
 if key>=#32 then
 begin
  if FSelStart<>-1 then DeleteSelected;
  FInputBuffer.Insert(FInputPos,key,FInputColor,FInputBackGround);
  Inc(FInputPos);
  FCaretX:=FInputX+FInputPos;
  FHistoryPos:=0;
  if assigned(FOnInputChange) then FOnInputChange(Self,FInputBuffer);
 end;
 if Assigned(OnAny) then OnAny(Self,FInputBuffer);
 AdjustScrollBars;
 MakeInputVisible;
 If FInputVisible then Invalidate;
end;

procedure TCmdBox.KeyDown(var Key:Word;Shift:TShiftState);
var s : String;
    i : Integer;
begin
 if not FInput then Exit;
 case Key of
  VK_END:
  begin
   key:=0;
   if (not (ssAlt in Shift)) and FInput and (FInputPos<>FInputBuffer.Length) then
   begin
    if not (ssShift in Shift) then SetSelection(-1,0) else RightSelection(FInputPos,FInputBuffer.Length);
    FInputPos:=FInputBuffer.Length;
    FCaretX:=FInputX+FInputPos;
    MakeInputVisible;
    Invalidate;
   end;
  end;
  VK_HOME:
  begin
   key:=0;
   if (not (ssAlt in Shift)) and FInput and (FInputPos<>FInputMinPos) then
   begin
    if not (ssShift in Shift) then SetSelection(-1,0) else LeftSelection(FInputMinPos,FInputPos);
    FInputPos:=FInputMinPos;
    FCaretX:=FInputX+FInputPos;
    MakeInputVisible;
    Invalidate;
   end;
  end;
  VK_LEFT:
  begin
   if (not (ssAlt in Shift)) and (FInput and (FInputPos>FInputMinPos)) then
   begin
    if not (ssShift in Shift) then SetSelection(-1,0) else LeftSelection(FInputPos-1,FInputPos);
    Dec(FInputPos);
    FCaretX:=FInputX+FInputPos;
    MakeInputVisible;
    Invalidate;
   end;
  end;
  VK_UP:
  begin
   if (not (ssAlt in Shift)) and FInput then
   begin
    SetSelection(-1,0);
    SetHistoryPos(FHistoryPos+1);
   end;
  end;
  VK_DOWN:
  begin
   if (not (ssAlt in Shift)) and FInput then
   begin
    SetSelection(-1,0);
    SetHistoryPos(FHistoryPos-1);
   end;
  end;
  VK_RIGHT:
  begin
   if (not (ssAlt in Shift)) and FInput and (FInputPos<FInputBuffer.Length) then
   begin
    if not (ssShift in Shift) then SetSelection(-1,0) else RightSelection(FInputPos,FInputPos+1);
    Inc(FInputPos);
    FCaretX:=FInputX+FInputPos;
    MakeInputVisible;
    Invalidate;
   end;
  end;
  VK_DELETE:
  begin
   if FInput then
   begin
    if FSelStart<>-1 then DeleteSelected else FInputBuffer.Delete(FInputPos);
    FHistoryPos:=0;
    if assigned(FOnInputChange) then FOnInputChange(Self,FInputBuffer);
    MakeInputVisible;
    AdjustLineHeight(FInputY);
    AdjustScrollBars;
   end;
  end;
  VK_RETURN:
  begin
   if FInput then
   begin
    s := FInputBuffer.GetString;
    s := Copy(s,FUTF8InputMinPos+1,Length(s));
    if (FHistoryPos=0) then
    begin
     if (FInputBuffer.Length=FInputMinPos) or FInputIsPassWord then
     begin
      DeleteHistoryEntry(0);
     end
     else
     begin
      i:=HistoryIndexOf(s);
      if i>=0 then
      begin
       DeleteHistoryEntry(0);
       MakeFirstHistoryEntry(i);
      end
      else
      begin
       FHistory[0].Clear;
       FHistory[0].PartOverWrite(FInputBuffer,FInputMinPos,FInputBuffer.Length,0);
      end;
     end;
    end
    else
    begin
     DeleteHistoryEntry(0);
     MakeFirstHistoryEntry(FHistoryPos);
    end;
    FInput  := False;
    if FLines[FOutY].Length<>0 then
    begin
     if FOutY>=FLineCount-1 then
     begin
      ScrollUp;
      Dec(FOutY);
      FInputY:=FOutY;
      AdjustLineHeight(FOutY);
      UpdateLineHeights;
      TranslateScrollBarPosition;
     end;
     FLines[FOutY+1].Clear;
     FLines[FOutY+1].OverWrite(FLines[FOutY],0);
     FLines[FOutY].Clear;
     if FInputIsPassWord then
      FLines[FOutY].OverWritePW(FInputBuffer,FInputMinPos,FInputX,FPassWordChar)
     else FLines[FOutY].OverWrite(FInputBuffer,FInputX);
    end
    else
    begin
     if FInputIsPassWord then
      FLines[FOutY].OverWritePW(FInputBuffer,FInputMinPos,FInputX,FPassWordChar)
     else FLines[FOutY].OverWrite(FInputBuffer,FInputX);
    end;
    Inc(FOutY);
    if FOutY>=FLineCount then
    begin
     ScrollUp;
     Dec(FOutY);
     FInputY:=FOutY;
     AdjustLineHeight(FOutY);
     UpdateLineHeights;
     TranslateScrollBarPosition;
    end;
    FOutX   := 0;
    FCaretX := 0;
    FInputBuffer.Clear;
    if Assigned(OnInput) then OnInput(Self,s);
    if Assigned(OnAny) then OnAny(Self,FInputBuffer);
    AdjustScrollBars;
    Invalidate;
   end;
  end;
  VK_BACK:
  begin
   if FInput then
   begin
    if FSelStart<>-1 then DeleteSelected else
    begin
     If (FInputPos>FInputMinPos) then
     begin
      Dec(FInputPos);
      FInputBuffer.Delete(FInputPos);
      FCaretX:=FInputX+FInputPos;
     end;
    end;
    FHistoryPos:=0;
    if assigned(FOnInputChange) then FOnInputChange(Self,FInputBuffer);
    if Assigned(OnAny) then OnAny(Self,FInputBuffer);
    AdjustScrollBars;
    MakeInputVisible;
    If FInputVisible then Invalidate;
   end;
  end;
  VK_C:
  begin
   if (FInput) and (ssCtrl in Shift) then CopyToClipBoard;
  end;
  VK_V:
  begin
   if (FInput) and (ssCtrl in Shift) then PasteFromClipBoard;
  end;
  VK_X:
  begin
   if (FInput) and (ssCtrl in Shift) then CutToClipBoard;
  end;
  VK_A:
  begin
   if (FInput) and (ssCtrl in Shift) then
   begin
    SetSelection(FInputMinPos,FInputBuffer.Length);
    FInputPos:=FInputBuffer.Length;
    MakeInputVisible;
    if FInputVisible then Invalidate;
   end;
  end;
 end;
 if Assigned(OnAny) then OnAny(Self,FInputBuffer);
end;

procedure TCmdBox.InsertHistory;
var i : Integer;
    t : TColorstring;
begin
 t:=FHistory[FHistoryMax-1];
 for i:=FHistoryMax-2 downto 0 do
 begin
  FHistory[i+1]:=FHistory[i];
 end;
 FHistory[0] := t;
 FHistoryPos := 0;
 If FHistoryLength<FHistoryMax then Inc(FHistoryLength);
end;

procedure TCmdBox.StartRead(DFC,DBC:TColor;const Desc:String;IFC,IBC:TColor);
var Pp,i,l:Integer;
begin
 Inc(FCaretX,UTF8Length(Desc));
 FInputX             := FOutX;
 if FLines[FOutY].Length=0 then FInputY := FOutY else FInputY:=FOutY+1;
 FInputVisible       := True;
 FInput              := True;
 FUTF8InputMinPos    := Length(Desc);
 i:=0;
 Pp:=1;
 while Pp<=Length(Desc) do
 begin
  if Desc[Pp]=#27 then
  begin
   if Pp+1>Length(Desc) then Break;
   case Desc[Pp+1] of
    #9,#10,#32,#46,#196:
    begin
     if Pp+2>Length(Desc) then Break; //Incomplete Escape Seq...ignore
     l:=3;
    end;
    #33,#47,#197:
    begin
     if Pp+3>Length(Desc) then Break; //Incomplete Escape Seq...ignore
     l:=4;
    end;
    else
    begin
     l:=2;
    end;
   end;
  end else l:=UTF8CharacterLength(@Desc[PP]);
  FInputBuffer.OverWriteChar(Copy(Desc,Pp,l),i,FGraphicCharWidth,DFC,DBC);
  Inc(i);
  Inc(Pp,l);
 end;
 FInputPos           := i;
 FInputMinPos        := i;
// FInputBuffer.OverWrite(Desc,0,DFC,DBC);
 FInputIsPassWord    := False;
 FInputColor         := IFC;
 FInputBackground    := IBC;
 FInputBuffer.PassWordStart:=MaxInt;
 InsertHistory;
 MakeInputVisible;
end;

procedure TCmdBox.StartReadPassWord(DFC,DBC:TColor;const Desc:string;IFC,IBC:TColor);
begin
 StartRead(DFC,DBC,Desc,IFC,IBC);
 FInputBuffer.PassWordStart := UTF8Length(Desc);
 FInputBuffer.PassWordChar  := FPassWordChar;
 FInputIsPassWord           := True;
end;

procedure TCmdBox.StopRead;
begin
  FInput:=False;
end;

procedure TCmdBox.DeleteHistoryEntry(i:Integer);
var j:Integer;
    Temp:TColorstring;
begin
 Temp:=FHistory[i];
 for j:=i to FHistoryLength-2 do FHistory[j]:=FHistory[j+1];
 FHistory[FHistoryLength-1]:=Temp;
 Dec(FHistoryLength);
 if FHistoryPos>=i then Dec(FHistoryPos);
end;

procedure TCmdBox.MakeFirstHistoryEntry(i:Integer);
var Temp:TColorstring;
begin
 if FHistoryPos<>0 then
 begin
  Temp:=FHistory[i];
  for i:=i-1 downto 0 do FHistory[i+1]:=FHistory[i];
  FHistory[0]:=Temp;
 end;
end;

procedure TCmdBox.Clear;
var i:Integer;
begin
 for i:=0 to Length(FLines)-1 do Flines[i].Clear;
 FCaretX := 0;
 FInputY := 0;
 FOutX   := 0;
 FOutY   := 0;
 if FInput then FInputY:=0;
 Invalidate;
end;

procedure TCmdBox.Writeln(s:string);
begin
 write(s+#13#10);
end;

const AnsiColors:array['0'..'7'] of TColor=(clBlack,clRed,clGreen,clYellow,clBlue,clFuchsia,clAqua,clWhite);

procedure TCmdBox.IntWrite;
var Pp   : Integer;
    SLen : Integer;
    l    : Integer;
    s    : String;
    EscString  : String;
    EscPos     : Integer;
    EscSubMode : Integer;
begin
 S    := FCurrentString;
 SLen := UTF8Length(S);
 Pp   := 1;
 while Pp<=Length(S) do
 begin
  l:=1;
  case FEscapeMode of
   escmNone:
   begin
    if S[Pp]=#27 then
    begin
     case FEscapeCodeType of
      esctCmdBox:
      begin
       FEscapeMode := escmOperation;
       FEscapeData := '';
      end;
      esctAnsi:
      begin
       FEscapeMode := escmAnsiOperation;
       FEscapeData := '';
      end;
      esctNone:
      begin
       // Simply ignore it
      end;
     end;
    end
    else
    begin
     l:=UTF8CharacterLength(@S[Pp]);
     if l=1 then
     begin
      case s[Pp] of
       #13:FOutX:=0;
       #10:
       begin
        AdjustLineHeight(FOutY);
        if FLines[FOutY].Length=0 then FLines[FOutY].DefaultBackGround:=FCurrentBackGround;
        Inc(FOutY);
        if FOutY>=Length(FLines) then
        begin
         ScrollUp;
         Dec(FOutY);
         AdjustLineHeight(FOutY);
         UpdateLineHeights;
         TranslateScrollBarPosition;
        end;
       end;
       else
       begin
        FLines[FOutY].OverWrite(s[Pp],FOutX,FCurrentColor,FCurrentBackGround);
        Inc(FOutX);
       end;
      end;
     end
     else
     begin
      FLines[FOutY].OverWrite(Copy(s,Pp,l),FOutX,FCurrentColor,FCurrentBackGround);
      Inc(FOutX);
     end;
    end;
   end;
   escmOperation:
   begin
    case S[Pp] of
     #9,#10,#32,#46,#196:
     begin
      FEscapeData := S[Pp];
      FEscapeMode := escmData1;
     end;
     #33,#47,#197:
     begin
      FEscapeData := S[Pp];
      FEscapeMode := escmData2;
     end;
     else
     begin
      FLines[FOutY].OverWriteChar(#27+S[Pp],FOutX,FGraphicCharWidth,FCurrentColor,FCurrentBackGround);
      Inc(FOutX);
      FEscapeMode:=escmNone;
     end;
    end;
   end;
   escmData1:
   begin
    FLines[FOutY].OverWriteChar(#27+FEscapeData+S[Pp],FOutX,FGraphicCharWidth,FCurrentColor,FCurrentBackGround);
    Inc(FOutX);
    FEscapeMode:=escmNone;
   end;
   escmData2:
   begin
    FEscapeData := FEscapeData+S[Pp];
    FEscapeMode := escmData1;
   end;
   escmAnsiOperation:
   begin
    case S[Pp] of
     '[':FEscapeMode:=escmAnsiSquare;
     else FEscapeMode:=escmNone;
    end;
   end;
   escmAnsiSquare:
   begin
    case S[Pp] of
     'm':
     begin
      EscPos     := 1;
      EscSubMode := 0;
      while EscPos<=Length(FEscapeData) do
      begin
       case EscSubMode of
        0:
        begin
         case FEscapeData[EscPos] of
          '0':
          begin
           // No Reset Values know here...just assume
           FCurrentColor:=clSilver;
           FCurrentBackGround:=clBlack;
          end;
          '7':
          begin
           // Reverse? What now...
          end;
          '3':EscSubMode:=3;
          '4':EscSubMode:=4;
         end;
        end;
        1:
        begin
         // Just collect the expected ";", not sure what to do if it isn't there...
         EscSubMode:=0;
        end;
        3:
        begin
         if FEscapeData[EscPos] in ['0'..'7'] then FCurrentColor:=AnsiColors[FEscapeData[EscPos]];
         EscSubMode:=1;
        end;
        4:
        begin
         if FEscapeData[EscPos] in ['0'..'7'] then FCurrentBackGround:=AnsiColors[FEscapeData[EscPos]];
         EscSubMode:=1;
        end;
       end;
       Inc(EscPos);
      end;
      FEscapeMode:=escmNone;
     end;
     else
     begin
      FEscapeData:=FEscapeData+S[Pp];
     end;
    end;
   end;
  end;
  Inc(Pp,l);
 end;
 if FInput then
 begin
  if FLines[FOutY].Length=0 then
  begin
   if (FInputY<>FOutY) then FInputY:=FOutY;
  end
  else
  begin
   if FInputY<>FOutY+1 then FInputY:=FOutY+1;
  end;
  if FInputY>=FLineCount then
  begin
   ScrollUp;
   Dec(FOutY);
   Dec(FInputY);
   FInputY:=FOutY;
   AdjustLineHeight(FOutY);
   UpdateLineHeights;
   TranslateScrollBarPosition;
  end;
  MakeInputVisible;
 end else MakeOutVisible;
 AdjustLineHeight(FOutY);
 if not FInput then FCaretX:=FOutX;
 AdjustScrollBars;
end;

procedure TCmdBox.SetOutY(v:Integer);
begin
 if v>FLineCount-1 then v:=FLineCount-1;
 FOutY:=v;
end;

procedure TCmdBox.Resize;
begin
 inherited Resize;
 AdjustScrollBars;
 if FVSBPos>=FVisibleLineCount-FPageHeight then
 begin
  FVSBPos:=FVisibleLineCount-FPageHeight;
  if FVSBPos<0 then FVSBPos:=0;
 end;
 TranslateScrollBarPosition;
end;

function TCmdBox.AdjustLineHeight(i:Integer):Integer;
var LineC  : Integer;
    LineC2 : Integer;
begin
 if (FInputY=i) then
 begin
  LineC  := FLines[i].LineCount(FClientWidth,-1,FCaretWidth);
  LineC2 := FInputBuffer.LineCount(FClientWidth,FCaretX,FCaretWidth);
  if LineC2>LineC then LineC:=LineC2;
 end else LineC:=FLines[i].LineCount(FClientWidth,-1,FCaretWidth);
 Result          := LineC;
 FLineHeights[i] := Result;
end;

function TCmdBox.UpdateLineHeights:integer;
var i:integer;
begin
 Result:=0;
 for i:=0 to FLineCount-1 do
 begin
  FLineHeightSum[i]:=Result;
  Inc(Result,AdjustLineHeight(i));
 end;
end;

procedure TCmdBox.AdjustScrollBars;
var LH : Integer;
begin
 FClientWidth  := Width-FVSBWidth;
 FClientHeight := Height;
 FPageHeight   := FClientHeight div FCharHeight;
 FVisibleLines := FPageHeight+ord(FClientHeight mod FCharHeight<>0);
 LH            := UpdateLineHeights;
 if LH<>FVisibleLineCount then
 begin
  FVisibleLineCount:=LH;
  if FVisibleLineCount<=FVSBPos+FPageHeight then
  begin
   FVSBPos:=FVisibleLineCount-FPageHeight;
   if FVSBPos<0 then FVSBPos:=0;
   if HandleAllocated then ScrollBarPosition(SB_Vert,FVSBPos);
   TranslateScrollBarPosition;
  end;
 end;
 if FVisibleLineCount<FPageHeight then
 begin
  if HandleAllocated then
  begin
   ScrollBarPosition(SB_VERT,0);
   ScrollBarRange(SB_VERT,0,FPageHeight);
   ShowScrollBar(Handle,SB_VERT,True); { Disable the Scrollbar ! }
  end;
 end
 else
 begin
  if HandleAllocated then
  begin
   ScrollBarRange(SB_VERT,FVisibleLineCount,FPageHeight);
   ShowScrollBar(Handle,SB_VERT,True);
  end;
 end;
 Invalidate;
end;

procedure TCmdBox.SetTopLine(Nr:Integer);
begin
 if Nr<>FTopLine then
 begin
  FTopLine:=Nr;
  AdjustScrollBars;
 end;
end;

procedure TCmdBox.SetLineCount(c: Integer);
var i:Integer;
begin
 if c<1 then c:=1;
 if c<>FLineCount then
 begin
  for i:=0 to FLineCount-1 do FLines[i].Free;
  FLineCount:=c;
  SetLength(FLines,FLinecount);
  for i:=0 to FlineCount-1 do
  begin
   FLines[i]:=TColorstring.Create(Canvas.Font);
   FLines[i].DefaultBackGround:=FBackGroundColor;
   FLines[i].TabWidth:=FTabWidth;
  end;
  SetLength(FLineHeights,FLineCount);
  SetLength(FLineHeightSum,FLineCount);
  AdjustScrollBars;
 end;
end;

procedure TCmdBox.Paint;
Var y,m         : Integer;
    CurrentLine : Integer;
begin
 inherited Paint;
 with canvas do
 begin
  if (csdesigning in componentstate) then
  begin
   Brush.Style := bsSolid;
   Brush.Color := clBlack;
   FillRect(0,0,FClientWidth,FClientHeight);
   exit;
  end;
  Font        := FFont;
  Brush.Style := bsSolid;
  m           := FVisibleLines-1;
  y           := -FLineOfTopLine;
  CurrentLine := FTopLine;
  while (y<=m) and (CurrentLine<LineCount) do
  begin
   FLines[CurrentLine].LineOutAndFill(Canvas,0,y*FCharHeight,0,FClientWidth,FCharHeight,FGraphicCharWidth,-1,FBackGroundColor,FCaretColor,FCaretHeight,FCaretWidth,FCaretYShift,False);
   if (FInput) and (FInputY=CurrentLine) then
   begin
    if FInputIsPassWord then
    begin
     FInputBuffer.LineOutAndFill(Canvas,0,y*FCharHeight,0,FClientWidth,FCharHeight,FGraphicCharWidth,FCaretX,FBackGroundColor,FCaretColor,FCaretHeight,FCaretWidth,FCaretYShift,FCaretVisible and Focused);
    end
    else
    begin
     FInputBuffer.LineOutAndFill(Canvas,0,y*FCharHeight,0,FClientWidth,FCharHeight,FGraphicCharWidth,FCaretX,FBackGroundColor,FCaretColor,FCaretHeight,FCaretWidth,FCaretYShift,FCaretVisible and Focused);
    end;
   end;
   Inc(y,FLineHeights[CurrentLine]);
   Inc(CurrentLine);
  end;
  y:=y*FCharHeight;
  if y<FClientHeight then
  begin
   Brush.Color := FBackGroundColor;
   Brush.Style := bsSolid;
   FillRect(0,y,FClientWidth,FClientHeight);
  end;
 end;
end;

procedure TCmdBox.CaretTimerExecute(Sender:TObject);
begin
 if Focused then
 begin
  if not Assigned(WakeMainThread) then MultiWrite;
  FCaretVisible:=not FCaretVisible;
  Invalidate;
 end;
end;

procedure TCmdBox.CreateWnd;
begin
 inherited CreateWnd;
 FVSBWidth  := GetSystemMetrics(SM_CXVSCROLL) + GetSystemMetricsGapSize(SM_CXVSCROLL);
 SetFont(FFont);
 if FCaretHeight=-1 then FCaretHeight := FFont.GetTextHeight('A')-3;
 { Little Hack to prevent "grey bar" Scrollbar at StartUp }
 ShowScrollBar(Handle,SB_VERT,False);
 ShowScrollBar(Handle,SB_VERT,True);
 AdjustScrollBars;
end;

procedure TCmdBox.CreateParams(var Params:TCreateParams);
const ClassStylesOff = CS_VREDRAW or CS_HREDRAW;
begin
 inherited CreateParams(Params);
 with Params do
 begin
  WindowClass.Style:=WIndowClass.Style and DWORD(not ClassStylesOff);
  Style := Style or WS_VSCROLL or WS_HSCROLL or WS_CLIPCHILDREN;
 end;
end;

constructor TCmdBox.Create(AComponent:TComponent);
var i:integer;
begin
 inherited Create(AComponent);
 System.InitCriticalSection(FLock);
 FStringBuffer          := TStringList.Create;
 FCharHeight            := 15; // Just a random value to prevent stupid exceptions
 FSelStart              := -1;
 FLineCount             := 1000;
 FInputVisible          := False;
 FBackGroundColor       := clBlack;
 FGraphicCharWidth      := 10;
 FInputBuffer           := TColorstring.Create(Canvas.Font);
 FEscapeCodeType        := esctCmdBox;
 FAutoFollow            := True;
 SetLength(FLines,FLineCount);
 SetLength(FLineHeights,FLineCount);
 SetLength(FLineHeightSum,FLineCount);
 FTabWidth              := 60;
 for i:=0 to FLineCount-1 do
 begin
  FLines[i]                   := TColorstring.Create(Canvas.Font);
  FLines[i].DefaultBackGround := FBackGroundColor;
  FLines[i].TabWidth          := FTabWidth;
 end;
 FCaretTimer            := TTimer.Create(self);
 FCaretTimer.Interval   := 500;
 FCaretTimer.OnTimer    := @carettimerexecute;
 FCaretTimer.Enabled    := True;
 FCaretVisible          := True;
 FVSBVisible            := True;
 FFont                  := Canvas.Font;
 FCurrentColor          := clSilver;
 FCurrentBackground     := clBlack;
 DoubleBuffered         := True;
 FFont.Color            := ClSilver;
 FCaretColor            := clWhite;
 FCaretType             := cartLine;
 FCaretWidth            := 1;
 FCaretHeight           := -1;
 FCaretYShift           := 3;
 FInputSelBackground    := clWhite;
 FInputSelColor         := clBlue;
 FHistoryMax            := 10;
 FHistoryLength         := 0;
 SetBounds(0,0,200,200);
 SetLength(FHistory,FHistoryMax);
 for i:=0 to FHistoryMax-1 do FHistory[i]:=TColorstring.Create(Canvas.Font);
end;

destructor TCmdBox.Destroy;
begin
 FCaretTimer.Enabled := False;
 System.DoneCriticalSection(FLock);
 FStringBuffer.Free;
 inherited Destroy;
end;

procedure Register;
begin
 RegisterComponents('Other',[TCmdBox]);
end;

initialization
 {$I tcmdbox.lrs}
end.
