
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
begin
  Result := QtWidgetSet.GetTextExtentExPoint(DC, Str, Count, MaxWidth, MaxCount, PartialWidths, Size);
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
