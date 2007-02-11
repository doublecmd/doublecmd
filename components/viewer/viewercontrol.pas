{
 Component ViewerControl (Free Pascal)
 show file in text (wraped or not) or bin or hex mode

 This is part of Seksi Commander

 To searching use uFindMmap,
 to movement call Upxxxx, Downxxxx, or set Position

 Realised under GNU GPL 2
 author Radek Cervinka (radek.cervinka@centrum.cz)

 changes:
 5.7. (RC)
   - selecting text with mouse
   - CopyToclipBoard, SelectAll
 ?.6. - LoadFromStdIn and loading first 64Kb of files with size=0 :) (/proc fs ..)  
 17.6. (RC)
   - mapfile (in error set FMappedFile=nil)
   - writetext TABs fixed (tab is replaced by 9 spaces)
   - set correct position for modes hex, bin (SetPosition)
 21.7
   - wrap text on 80 character lines works better now (by Radek Polak)
   - problems with function UpLine for specific lines:
     (lines of 80(=cTextWidth) character ended with ENTER (=#10)
 6.2. (RC)
   - ported to fpc for linux (CustomControl and gtk)
 7.2. (RC)
   - use temp to new implementation of LoadFromStdIn (and mmap temp file)
   - faster drawing of text (I hope)
   

}

unit viewercontrol;
{$mode objfpc}{$H+}
interface

uses
  SysUtils, Classes, controls, types, Graphics, LCLType;

type
  TViewerMode=(vmBin, vmHex, vmText, vmWrap);
  TDataAccess=(dtMmap, dtNothing);

  TViewerControl = class(TCustomControl)
//  TViewerControl = class(TGraphicControl)
  private
    { Private declarations }
  protected
    { Protected declarations }
    FTempName: String;
    
    FViewerMode:TViewerMode;
    FFileHandle:Integer;
    FFileSize:Integer;
    FMappedFile:PChar;
    FPosition: Integer;
    FLineList:TList;
    FBlockBeg:Integer;
    FBlockEnd:Integer;
    FMouseBlockBeg:Integer;
    FSelecting:Boolean;
    // this is broken ..., using constant
    FTextHeight, FTextWidth:Integer; // measured values of font, rec calc at font changed
//    FBitmap:TBitmap;
    procedure OutText(ARect:TRect; x,y:Integer; const sText:String);
    procedure WriteText(bWrap:Boolean);
    procedure WriteHex;
    procedure WriteBin;
    Procedure AddLineOffset(iOffset:Integer);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    function XYPos2Adr(x,y:Integer):Integer;
  public
    { Public declarations }
//    procedure EraseBackground(DC: HDC); override;


    procedure Paint; override;
    procedure Down;
    procedure Up;
    function UpLine:Boolean;
    function DownLine:Boolean;

    Function DownBy(iLines:Integer):Boolean;
    Function UpBy(iLines:Integer):Boolean;

    procedure PageUp;
    procedure PageDown;
    procedure GoHome;
    procedure GoEnd;

//    Function Find(const sText:String; bCase:Boolean):Boolean;
    procedure SetViewerMode(Value:TViewerMode);
    Function MapFile(const sFileName:String):Boolean;
    procedure UnMapFile;
    Function LoadFromStdin(const sCmd:String):Boolean;
    Function GetDataAdr:PChar;

    procedure SetPosition(Value:Integer);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SelectAll;
    procedure CopyToClipboard;
  published
    { Published declarations }
    property ViewerMode:TViewerMode read FViewerMode write SetViewerMode;
    property Position:Integer read FPosition write SetPosition;
    property FileSize:Integer read FFileSize;
    property OnMouseMove;
    property OnClick;
    property OnMouseDown;
    property OnMouseUp;
    property Font;
    property Align;
    property Color;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
  end;

procedure Register;

implementation
uses
  Clipbrd{$IFNDEF WIN32}, Libc, unix{$ENDIF};

const
  cTextWidth=80;  // wrap on 80 chars
  cMaxTextWidth=300; // maximum of chars on one line unwrapped text
  cHexWidth=16;
  cTabSpaces=9;  // tab stop

constructor TViewerControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color:=clWindow;
//  FBitmap:=TBitmap.Create;
  FViewerMode:=vmBin;
  FMappedFile:=nil;
  FPosition:=0;
  Width:=100;
  Height:=100;
  FTextWidth:=14; // dummy values, recalculated at the moment of assigning parent (broken)
  FTextHeight:=14;
  DoubleBuffered:=True;

  FLineList:=TList.Create;
//  MouseCapture:=True;  // lazarus shoter down :(, but working without
  Cursor:=crIBeam;
  Font.Name:='fixed';
  Font.Pitch:=fpFixed;
{  Font.Style:=[fsBold];}
  Font.Size:=14;
  FTempName:='';
end;

procedure TViewerControl.Paint;
var
  Rct: TRect;
begin

  FTextHeight:=Font.Size+2;  //TextHeight('0');
  with Canvas do
  begin
    Rct := ClientRect;
    Brush.Color := Self.Color;
    FillRect(Rct);
    Font := Self.Font;
    if not assigned(FMappedFile) then Exit;

    FLineList.Clear;
    FTextWidth:=TextWidth('0');
//    FTextHeight:=Font.Height;  //TextHeight('0');
    case FViewerMode of
      vmBin: WriteBin;
      vmHex: WriteHex;
      vmText: WriteText(False);
      vmWrap: WriteText(True);
    end;
  end;
end;

procedure TViewerControl.SetViewerMode(Value:TViewerMode);
begin
  FViewerMode:=Value;
  // for hex set correct position for line begin
  case Value of
    vmBin: FPosition:=(FPosition div cTextWidth)* cTextWidth;
    vmHex: FPosition:=(FPosition div cHexWidth)* cHexWidth;
  end;
  Invalidate
end;

procedure TViewerControl.Down;
begin
  if DownLine then
    Invalidate;
end;

function TViewerControl.UpLine:Boolean;
var
  i:Integer;
  iLastPos:Integer;
  iRemainder:Integer;

  procedure UpText(Wrap:Boolean);
  begin
    i:=FPosition;
    if (i>1) and (FMappedFile[i-1]=#10) then
      dec(i,2);
    While i>0 do
    begin
      if (FMappedFile[i]=#10) or (i=0) then
      begin
        if i>0 then inc(i); // we are after #10
        if Wrap then
        begin
          iRemainder:=(FPosition-i) mod cTextWidth;
          // RADEK: special case: line of length 80 ended with #10
          if (FMappedFile[FPosition-1]=#10) and (iRemainder=1) then
            FPosition:=FPosition-cTextWidth-1
          else
            if (iRemainder=0) then
              FPosition:=FPosition-cTextWidth
            else
              FPosition:=FPosition-iRemainder;
        end
        else
        begin
          // check for maximum of chars on one line
          if ((FPosition-i)>cMaxTextWidth) then
          begin
            iRemainder:=(FPosition-i) mod cMaxTextWidth;
            // RADEK: special case: line of max length ended with #10
            if (FMappedFile[FPosition-1]=#10) and (iRemainder=1) then
              FPosition:=FPosition-cMaxTextWidth-1
            else
              if ((FPosition-i) mod cMaxTextWidth)=0 then
                FPosition:=FPosition-cMaxTextWidth
              else
                FPosition:=FPosition-iRemainder;
          end
          else // maximum not reached
            FPosition:=i;

        end;
        Break;
      end;
      dec(i);
    end;
    if i=0 then FPosition:=0;
  end;

begin
  iLastPos:=FPosition;
  case FViewerMode of
    vmBin:
      begin
        if FPosition>=cTextWidth then
          dec(FPosition,cTextWidth);
      end;
    vmHex:
      begin
        if FPosition>=cHexWidth then
          dec(FPosition,cHexWidth);
      end;
    vmText: UpText(False);
    vmWrap: UpText(True);
  end;
  Result:= (iLastPos<>FPosition);
end;

Function TViewerControl.DownBy(iLines:Integer):Boolean;
var
  i:Integer;
begin
  Result:=False;
  for i:=1 to iLines do
  begin
    if DownLine then  // only one line and we need repaint
      Result:=True
    else
      Break;
  end;
  if Result then Invalidate;
end;

Function TViewerControl.UpBy(iLines:Integer):Boolean;
var
  i:Integer;
begin
  Result:=False;
  for i:=1 to iLines do
  begin
    if UpLine then  // only one line and we need repaint
      Result:=True
    else
      Break;
  end;
  if Result then Invalidate;
end;

function TViewerControl.DownLine:Boolean;
var
  i:Integer;
  iLastPos:Integer;

  procedure DownText(Wrap:Boolean);
  begin
    i:=FPosition;
    While i<FFileSize do
    begin
      if (FMappedFile[i]=#10)then
      begin
        FPosition:=i+1;
        Break;
      end;
      if Wrap and ((i-FPosition)>=cTextWidth) then
      begin
        FPosition:=i;
        Break;
      end;
      // this is a workaround, max of unwrapped text on one line
      if not Wrap and ((i-FPosition)>=cMaxTextWidth) then
      begin
        FPosition:=i;
        Break;
      end;
      inc(i);
    end;
    if FPosition>=FFileSize then
      FPosition:=iLastPos;
  end;

begin
  iLastPos:=FPosition;
  case FViewerMode of
    vmBin:
      begin
        if FPosition+cTextWidth<=FFileSize then
          inc(FPosition,cTextWidth);
      end;
    vmHex:
      begin
        if FPosition+cHexWidth<=FFileSize then
          inc(FPosition,cHexWidth);
      end;
    vmText:DownText(False);
    vmWrap:DownText(True);
  end;
  Result:= iLastPos<>FPosition;
end;


procedure TViewerControl.Up;
begin
  if UpLine then
    Invalidate;
end;

procedure TViewerControl.PageUp;
var
  H:Integer;
begin
  H:= Self.Height div FTextHeight-1;
  if H<=0 then H:=1;
  UpBy(H);
end;



procedure TViewerControl.PageDown;
var
  H:Integer;

begin
  H:= Self.Height div FTextHeight-1;
  if H<=0 then H:=1;
  DownBy(H);
end;

procedure TViewerControl.GoHome;
begin
  FPosition:=0;
  Invalidate;
end;

procedure TViewerControl.GoEnd;
begin
  FPosition:=FFileSize-1;
  if FPosition<0 then FPosition:=0;
  Up;
end;



Function TViewerControl.MapFile(const sFileName:String):Boolean;
{$IFDEF WIN32}
begin

end;
{$ELSE}
var
  stat:TStatBuf;
begin
  Result:=False;
  if assigned(FMappedFile) then
    UnMapFile; // if needed
  FFileHandle:=Libc.open(PChar(sFileName), O_RDONLY);
  writeln('Trying map:'+sFileName);
  if FFileHandle=-1 then Exit;
  if fstat(FFileHandle, stat) <> 0 then
  begin
    Libc.__close(FFileHandle);
    Exit;
  end;

  FFileSize := stat.st_size;
  FMappedFile:=mmap(nil,FFileSize,PROT_READ, MAP_PRIVATE{SHARED},FFileHandle,0 );
  if Integer(FMappedFile)=-1 then
  begin
    FMappedFile:=nil;
    Libc.__close(FFileHandle);
    writeln('failed > try throught cat+stdin');
    if FTempName<>'' then
    begin
      writeln('Circular mmaping, aborting.');
      UnMapFile;
      Exit;
    end;
    LoadFromStdin('cat '+sFileName);
    Exit;
  end;
  writeln('Mmaped succesfully');
  FPosition:=0;
  Invalidate;
  Result:=True;
end;
{$ENDIF}

procedure TViewerControl.UnMapFile;
{$IFDEF WIN32}
begin

end;
{$ELSE}
begin
  writeln('Unmap file:',FTempName);
  FPosition:=0;
  if FTempName<>'' then
  begin
    DeleteFile(FTempName);  // delete temp file
    FTempName:='';
  end;
  if not assigned(FMappedFile) then Exit;
  Libc.__close(FFileHandle);
  munmap(FMappedFile,FFileSize);
  FMappedFile:=nil;
  writeln('Unmap file done');
end;
{$ENDIF}

procedure TViewerControl.WriteText(bWrap:Boolean);
var
  xIndex, yIndex:Integer;
//  H:Integer;
  c:Char;
  Rct: TRect;
  iPos:Integer;
  s:String;
begin
  iPos:=FPosition;
  with Canvas do
  begin
    Rct := GetClientRect;
//    H:= TextHeight('0');
    for yIndex:=0 to Rct.Bottom div FTextHeight do
    begin
      s:='';
      xIndex:=0;
      AddLineOffset(iPos);
      while ((xIndex<cMaxTextWidth) and not bWrap) or (xIndex<cTextWidth) do
      begin
        inc(xIndex);
        if ipos>=FFileSize then Break;
        c:=FMappedFile[iPos];
        inc(iPos);
        if (c=#13) then Continue;
        if (c=#10) then Break;
        if c=#9 then
           s:=s+ StringOfChar(' ',cTabSpaces-xIndex mod cTabSpaces)
        else
        begin
          if c<' ' then c:=' ';
          if c>#$F0 then c:='.';
          s:=s+c;
        end;
      end;
      // RADEK: if wrapped text ends with #10 we dont want extra empty line
      if ((xIndex=cTextWidth) or (xIndex=cMaxTextWidth)) then
      begin
        c := FMappedFile[iPos];
        if (c=#10) then
          inc(iPos);
      end;

      if s<>'' then
        OutText(Rct, 0, yIndex*FTextHeight,s);
    end;
  end;
end;

{function ConvertByte(b:Byte):Char;
begin
  if b in [32..255] then
    Result:=Chr(b)
  else
    Result:='.';
end;
}
Function LineFormat(const sHex, sAscii:String; iOffset:Integer):String;
var
  sDummy:String;
begin
  sDummy:='';
  if length(sHex)<(cHexWidth*3) then
    sDummy:=StringOfChar(' ',cHexWidth*3-length(sHex));
  Result:=Format('%s: %s%s  %s',[IntToHex(iOffset,8), sHex,sDummy,sAscii]);;
end;


procedure TViewerControl.WriteHex;
var
  xIndex, yIndex:Integer;
  c:Char;
  Rct: TRect;
  iPos, iLineBeg:Integer;
  sStr,sHex:String;
begin
  iPos:=FPosition;
  with Canvas do
  begin
    Rct := GetClientRect;
//    Rct := ClipRect;
//    s:='';
    for yIndex:=0 to Rct.Bottom div FTextHeight do
    begin
//      s:='';
      sStr:='';
      sHex:='';
      iLineBeg:=iPos;
      AddLineOffset(iPos);
      for xIndex:=0 to cHexWidth -1 do
      begin
        if ipos>=FFileSize then Break;
        c:=FMappedFile[ipos];
        if c<' ' then
          sStr:=sStr+'.'
        else
          sStr:=sStr+c;
        sHex:=sHex+IntToHex(ord(c),2);
        if xIndex=7 then
          sHex:=sHex+'|'
        else
          sHex:=sHex+' ';
        inc(iPos);
      end;
      if sStr<>'' then
        OutText(Rct, 0, yIndex*FTextHeight,LineFormat(sHex,sStr,iLineBeg));
//        TextRect(Rect(Rct.Left,yIndex*h,rct.Right,yIndex*h+H), 0, yIndex*h,LineFormat(sHex,sStr,iLineBeg));
    end;
  end;
end;

procedure TViewerControl.WriteBin;
var
  xIndex, yIndex:Integer;
  c:Char;
  Rct: TRect;
  iPos:Integer;
  s:String;
begin
  iPos:=FPosition;
  with Canvas do
  begin
    Rct := GetClientRect;
    s:='';
    for yIndex:=0 to Rct.Bottom div FTextHeight do
    begin
      s:='';
      AddLineOffset(iPos);
      for xIndex:=0 to cTextWidth -1 do
      begin
        if ipos>=FFileSize then Break;
        c:=FMappedFile[ipos];
        if c<' ' then c:='.';
        s:=s+c;
        inc(iPos);
      end;
      if s<>'' then
        OutText(Rct, 0, yIndex*FTextHeight,s);
    end;
  end;
end;

destructor TViewerControl.Destroy;
begin
  UnMapFile;

  if Assigned(FLineList) Then
    FreeAndNil(FLineList);
  inherited
end;

Function TViewerControl.GetDataAdr:PChar;
begin
  Result:=FMappedFile;
end;

procedure TViewerControl.SetPosition(Value:Integer);
begin
  if not assigned(FMappedFile) then Exit;
  if (Value<FFileSize) and (Value>=0) then
  begin
    case FViewerMode of
      vmBin: FPosition:=(Value div cTextWidth)* cTextWidth;
      vmHex: FPosition:=(Value div cHexWidth)* cHexWidth;
      vmText, vmWrap: FPosition:=Value;
    end;
    Invalidate;
  end;
end;

Function TViewerControl.LoadFromStdin(const sCmd:String):Boolean;
{$IFDEF WIN32}
begin

end;
{$ELSE}
var
  fFile:TextFile;
begin
  Result:=False;
  UnMapFile;
  FFileSize:=0;
  FPosition:=0;
  FTempName:='/tmp/view.tmp';// later something different
  popen(fFile,sCmd+' >'+FTempName,'w');
  close(fFile);
  
  MapFile(FTempName);
end;
{$ENDIF}

procedure TViewerControl.OutText(aRect:TRect; x,y:Integer; const sText:String);
var
  pBegLine, pEndLine:Integer;
  iBegDrawIndex:Integer;
  iEndDrawIndex:Integer;

begin
  pBegLine:=Integer(FLineList.Items[y div FTextHeight]);
  pEndLine:=pBegLine+length(sText);
  if ((FBlockEnd-FBlockBeg)=0) or
     ((FBlockBeg<pBegLine) and (FBlockEnd<pBegLine)) or // before
     ((FBlockBeg>pEndLine) and (FBlockEnd>pEndLine)) then //after
  begin
    // out of selection, draw normal
//    Canvas.Font.Color:=clYellow; // test
    Canvas.Font.Color:=clText;
//    Canvas.TextRect(ARect, x, y,sText); //!!!
    Canvas.TextOut(x, y,sText);
    Exit;
  end;

  if (FBlockBeg-pBegLine)>0 then
  begin
// begin line, not selected
//    Canvas.Font.Color:=clBlue; // test
    Canvas.Font.Color:=clText;
//    Canvas.TextRect(ARect, x, y,Copy(sText,1,FBlockBeg-pBegLine-1)); //!!!
    Canvas.TextOut(x, y,Copy(sText,1,FBlockBeg-pBegLine-1));
  end;

  // selected ?
  if (FBlockBeg<=pBegLine) then
    iBegDrawIndex:=pBegLine
  else
    iBegDrawIndex:=FBlockBeg;
  if (FBlockEnd<pEndLine) then
    iEndDrawIndex:=FBlockEnd
  else
    iEndDrawIndex:=pEndLine;

  Canvas.Brush.Style:=bsSolid;
  Canvas.Brush.Color:=clHighlight;
  if iBegDrawIndex<>pBegLine then
  begin

    Canvas.FillRect(Rect(x+(iBegDrawIndex-pBegLine-1)*FTextWidth, y, x+(iEndDrawIndex-pBegLine)*FTextWidth, y+FTextHeight));
//   Canvas.Font.Color:=clRed; // test
    Canvas.Font.Color:=clLight;
//    Canvas.TextRect(ARect, x+(iBegDrawIndex-pBegLine-1)*FTextWidth, y,Copy(sText,iBegDrawIndex-pBegLine,iEndDrawIndex-iBegDrawIndex+1));!!!
    Canvas.TextOut(x+(iBegDrawIndex-pBegLine-1)*FTextWidth, y,Copy(sText,iBegDrawIndex-pBegLine,iEndDrawIndex-iBegDrawIndex+1));
  end
  else
  begin
    Canvas.FillRect(Rect(x+(iBegDrawIndex-pBegLine)*FTextWidth, y, x+(iEndDrawIndex-pBegLine)*FTextWidth, y+FTextHeight));
//    Canvas.Font.Color:=clMaroon; // test
    Canvas.Font.Color:=clLight;
//    Canvas.TextRect(ARect, x+(iBegDrawIndex-pBegLine)*FTextWidth, y,Copy(sText,iBegDrawIndex-pBegLine,iEndDrawIndex-iBegDrawIndex));
    Canvas.TextOut(x+(iBegDrawIndex-pBegLine)*FTextWidth, y,Copy(sText,iBegDrawIndex-pBegLine,iEndDrawIndex-iBegDrawIndex));
  end;
  if (pEndLine-FBlockEnd)>0 then
  begin
// end of line, not selected
//    Canvas.Font.Color:=clGreen; // test
    Canvas.Font.Color:=clText;
//    Canvas.TextRect(ARect, x+(FBlockEnd-pBegLine)*FTextWidth, y,Copy(sText,FBlockEnd-pBegLine+1,pEndLine-FBlockEnd));
    Canvas.TextOut(x+(FBlockEnd-pBegLine)*FTextWidth, y,Copy(sText,FBlockEnd-pBegLine+1,pEndLine-FBlockEnd));

  end;
end;

Procedure TViewerControl.AddLineOffset(iOffset:Integer);
begin
  FLineList.Add(Pointer(iOffset));
end;

procedure TViewerControl.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if not assigned(FMappedFile) then Exit;
  inherited;
  if (Button=mbLeft) then
  begin
    FBlockBeg:=XYPos2Adr(x,y);
    FMouseBlockBeg:=FBlockBeg;
    FBlockEnd:=FBlockBeg;
    FSelecting:=True;
  end;
end;

procedure TViewerControl.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  iTemp:Integer;
begin
  inherited;
  if FSelecting and ((y mod FTextHeight=0) or (x mod FTextWidth=0)) Then
  begin
    if y<10 then
    begin
      if UpLine then
       if UpLine then
         UpLine;
    end;
    if y>Height-10 then
    begin
      if DownLine then
        if DownLine then
          DownLine;
    end;
    iTemp:=XYPos2Adr(x,y);
    if iTemp<FMouseBlockBeg then
    begin
      FBlockBeg:=iTemp;
      FBlockEnd:=FMouseBlockBeg;
    end
    else
    begin
      FBlockBeg:=FMouseBlockBeg;
      FBlockEnd:=iTemp;
    end;
    Invalidate;
  end;
end;

procedure TViewerControl.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  iTemp:Integer;
begin
  inherited;
  if not FSelecting then Exit;
  iTemp:=XYPos2Adr(x,y);
  if iTemp<FMouseBlockBeg then
  begin
    FBlockBeg:=iTemp;
    FBlockEnd:=FMouseBlockBeg;
  end
  else
  begin
    FBlockBeg:=FMouseBlockBeg;
    FBlockEnd:=iTemp;
  end;
  FSelecting:=False;
  CopyToClipboard;  // copy selection to clipboard
  Invalidate;
end;

function TViewerControl.XYPos2Adr(x,y:Integer):Integer;
var
  yIndex:Integer;
begin
  yIndex:=y div FTextHeight;
  if yIndex>=FLineList.Count then
    yIndex:=FLineList.Count-1;
  if yIndex<0 then
    yIndex:=0;  
  Result:=Integer(FLineList.Items[yIndex]);
  //(FTextWidth div 2) is half of char
  inc(Result, (X+FTextWidth div 2) div FTextWidth);
  if yIndex<FLineList.Count-1 then
  begin
    if Result>=Integer(FLineList.Items[yIndex+1]) then
      Result:=Integer(FLineList.Items[yIndex+1])//-1
  end;
//  writeln(Format('%d %d %d %d %d',[x,y,Integer(FLineList.Items[yIndex]),yIndex,Result]));
end;


procedure TViewerControl.SelectAll;
begin
  FBlockBeg:=0;
  FBlockEnd:=FFileSize;
  Invalidate;
end;

procedure TViewerControl.CopyToClipboard;
begin
  if (FBlockEnd-FBlockBeg)=0 then Exit;
  Clipboard.Clear;   // prevent multiple formats in Clipboard (specially synedit)
  Clipboard.AsText:=Copy(FMappedFile,FBlockBeg,FBlockEnd-FBlockBeg+1);
//  writeln(Clipboard.AsText);
end;

procedure Register;
begin
  RegisterComponents('SeksiCmd', [TViewerControl]);
end;

end.
