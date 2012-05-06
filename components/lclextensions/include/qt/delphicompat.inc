
{
  Qt Interface
  
  Initial implementation by Zeljan Rikalo
}

{$define HAS_GETBKCOLOR}
{$define HAS_GETTEXTEXTENTEXPOINT}
{$define HAS_GETTEXTALIGN}
{$define HAS_GETWINDOWDC}
{$define HAS_OFFSETRGN}
{$define HAS_SETBRUSHORGEX}


{$i ../generic/stubs.inc}
{$i ../generic/independentfunctions.inc}
{$i ../generic/unicodefunctions.inc}

function GetBkColor(DC:HDC):COLORREF;
var
  Color: PQColor;
begin
  if QtWidgetSet.IsValidDC(DC) then
  begin
    Color := TQtDeviceContext(DC).BackgroundBrush.getColor;
    TQColorToColorRef(Color^, Result);
  end else
    Result := CLR_INVALID;
end;

function GetTextExtentExPoint(DC: HDC; Str: PChar;
  Count, MaxWidth: Integer; MaxCount, PartialWidths: PInteger;
  var Size: TSize): BOOL;
var
  lbearing, rbearing, ascent, descent: LongInt;
  IsDBCSFont: Boolean;
  NewCount, Accumulator, i: Integer;
  AStr: WideString;
begin
  //based in lcl code
  Result := QtWidgetSet.IsValidDC(DC);
  if Result then
  with TQtDeviceContext(DC) do
  begin
    AStr := WideString(Str);
    Ascent := Font.Metrics.ascent;
    Descent := Font.Metrics.descent;
    Size.cX := Font.Metrics.width(@AStr, Count);
    Size.cY := Ascent + Descent;
    if PartialWidths <> nil then
    begin
      Accumulator := 0;
      for i:= 0 to Count - 1 do
      begin
        Inc(Accumulator, QFontMetrics_width(Font.Metrics.FHandle, PWideChar((Str+i))));
        PartialWidths[i] := Accumulator;
      end;
    end;
  end;
end;

function GetTextAlign(hDC:HDC): LongWord;
var
  QtDC: TQtDeviceContext;
  QtFontMetrics: QFontMetricsH;
  QtFont: QFontH;
begin
  Result := 0;
  if not QtWidgetSet.IsValidDC(hdC) then
    Exit;
  QtDC := TQtDeviceContext(hDC);
  QtFont := QtDC.vFont.FHandle;
  QtFontMetrics := QFontMetrics_create(QtFont);
  try
  {TODO: FIXME we should save somehow text flags into QtDC
   cause we don't have any function which returns current flags !}
  finally
    QFontMetrics_destroy(QtFontMetrics);
  end;
end;

function GetWindowDC(hWnd:HWND): HDC;
begin
  Result := LCLIntf.GetDC(hWnd);
end;

function OffsetRgn(hrgn:HRGN; nxOffset, nYOffset:longint):longint;
var
  Region: TQtRegion;
begin
  Region := TQtRegion(hrgn);
  QRegion_translate(Region.FHandle, nxOffset, nYOffset);
  Result := Region.GetRegionType;
end;

function SetBrushOrgEx(DC:HDC; nXOrg, nYOrg:longint; lppt:PPOINT):Boolean;
var
  QtDC: TQtDeviceContext;
begin
  Result := False;
  if not QtWidgetSet.IsValidDC(DC) then
    Exit;
  QtDC := TQtDeviceContext(DC);
  if lppt <> nil then
    QtDC.getBrushOrigin(lppt);
  QtDC.setBrushOrigin(nXorg, nYOrg);
  Result := True;
end;
