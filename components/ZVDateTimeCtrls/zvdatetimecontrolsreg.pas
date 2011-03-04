{
ZVDateTimeControlsReg
- - - - - - - - - - - - - - - - - -
Author: Zoran Vučenović, January and February 2010
        Зоран Вученовић, јануар и фебруар 2010.

   This unit is part of ZVDateTimeCtrls package for Lazarus.

-----------------------------------------------------------
LICENCE
- - - -
   Modified LGPL -- see COPYING.TXT.

-----------------------------------------------------------
NO WARRANTY
- - - - - -
   There is no warranty whatsoever.

-----------------------------------------------------------
BEST REGARDS TO LAZARUS COMMUNITY!
- - - - - - - - - - - - - - - - - -
   I do hope the ZVDateTimeCtrls package will be useful.
}
unit ZVDateTimeControlsReg;

{$mode objfpc}{$H+}

interface

procedure Register;

implementation

uses
  Classes, ZVDateTimePicker, DBZVDateTimePicker, LResources;

procedure Register;
begin
  RegisterComponents('Date and Time Ctrls', [
                          TZVDateTimePicker,
                          TDBZVDateTimePicker
                     ]);
end;

initialization
{$i zvdatetimectrls.lrs}

end.

