

{$ifndef HAS_GETDOUBLECLICKTIME}
function GetDoubleClickTime: UINT;
begin
  //todo: see if gtk has a value. Use Windows default for now
  Result := 500;
end;
{$endif}

{$ifndef HAS_REDRAWWINDOW}
function RedrawWindow(hWnd:THandle; lprcUpdate:PRECT; hrgnUpdate:HRGN; flags:LongWord):BOOLEAN;
begin
  //todo: see if there's a better way of doing this
  Result := LCLIntf.InvalidateRect(hWnd, lprcUpdate, (RDW_ERASE and flags) > 0);
end;
{$endif}
