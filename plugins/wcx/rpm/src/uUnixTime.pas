unit uUnixTime;

interface

function UnixTimeToDosTime(mtime : Longint; usedaylight : Boolean) : Longint;

implementation
{$IFDEF MSWINDOWS}
uses
  Windows;
{$ENDIF}

function UnixTimeToDosTime(mtime : Longint; usedaylight : Boolean) : Longint;
{$IFDEF MSWINDOWS}
const
  monthdays:array[1..12] of byte=(31,28,31,30,31,30,31,31,30,31,30,31);
var
  ft, ftStart, ftStop    : TFileTime;
  st                     : TSystemTime;
  tzinfo                 : TTimeZoneInformation;
  hw, lw, month1, month2 : Word;
  bias, day1, day2       : Integer;
  summer                 : Boolean;
begin
  ft.dwLowDateTime  := $D53E8000;
  ft.dwHighDateTime := $019DB1DE;
  comp(ft):=comp(ft) + 10000000 * (mtime);
  FillChar(tzinfo, sizeof(tzinfo), #0);
  GetTimeZoneInformation(tzinfo);
  bias := tzinfo.bias;
  comp(ft) := comp(ft) - 10000000 * 60 * bias;
  FileTimeToSystemTime(ft, st);
  if (tzinfo.StandardDate.wmonth <> 0) and usedaylight then begin
    if tzinfo.StandardDate.wYear = 0 then begin
      month1 := tzinfo.DaylightDate.wMonth;
      month2 := tzinfo.StandardDate.wMonth;
      if (st.wmonth < month1) or (st.wmonth > month2) then summer := False else
      if (st.wmonth > month1) and (st.wmonth < month2) then summer := True else
      if (st.wmonth = month1) then begin
        day1 := (st.wDayOfWeek - ((st.wday - 1) mod 7) + 7) mod 7;
        day2 := (tzinfo.DaylightDate.wDayOfWeek - day1 + 7) mod 7 + 1;
        day2 := day2 + 7 * (tzinfo.DaylightDate.wDay - 1);
        if day2 > monthdays[month1] then dec(day2, 7);
        if st.wDay < day2 then summer := False else
        if st.wDay > day2 then summer := True else
          if st.whour >= tzinfo.DaylightDate.whour
            then summer := True
            else summer := False;
      end
      else begin
        day1 := (st.wDayOfWeek - ((st.wday - 1) mod 7) + 7) mod 7;
        day2 := (tzinfo.StandardDate.wDayOfWeek - day1 + 7) mod 7 + 1;
        day2 := day2 + 7 * (tzinfo.StandardDate.wDay - 1);
        if day2 > monthdays[month1] then Dec(day2, 7);
        if st.wDay < day2 then summer := True else
        if st.wDay > day2 then summer := False else
          if st.whour >= tzinfo.StandardDate.whour
            then summer := False
            else summer := True;
      end;
    end else begin
      SystemTimeToFileTime(tzinfo.DaylightDate, ftStart);
      SystemTimeToFileTime(tzinfo.StandardDate, ftStop);
      summer := (comp(ft) > comp(ftStart)) and (comp(ft) <= comp(ftStop));
    end;
    if summer
      then bias := tzinfo.DaylightBias
      else bias := tzinfo.StandardBias;
    comp(ft) := comp(ft) - 10000000 * 60 * bias;
  end;
  FileTimeToDosDateTime(ft, hw, lw);
  Result := 65536 * hw + lw;
end;
{$ELSE}
begin
  Result := mtime;
end;
{$ENDIF}

end.
