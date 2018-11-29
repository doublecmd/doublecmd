{
   Double commander
   -------------------------------------------------------------------------
   Definitions of some common types.

   Copyright (C) 2012 Przemyslaw Nagay (cobines@gmail.com)
   Copyright (C) 2018 Alexander Koblov (Alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit uTypes;

// This unit should depend on as little other units as possible.

interface

type
  TCaseSensitivity = (
    cstNotSensitive,
    // According to locale collation specs. Usually it means linguistic sorting
    // of character case "aAbBcC" taking numbers into consideration (aa1, aa2, aa10, aA1, aA2, aA10, ...).
    cstLocale,
    // Depending on character value, direct comparison of bytes, so usually ABCabc.
    // Might not work correctly for Unicode, just for Ansi.
    cstCharValue);

  TRange = record
    First: Integer;
    Last: Integer;
  end;

  //Note: If we add a format here, don't forget to update also "FILE_SIZE" string table in "uFileFunctions".
  TFileSizeFormat = (fsfFloat, fsfByte, fsfKilo, fsfMega, fsfGiga, fsfTera,
                     fsfPersonalizedFloat, fsfPersonalizedByte, fsfPersonalizedKilo, fsfPersonalizedMega, fsfPersonalizedGiga, fsfPersonalizedTera);

implementation

end.
