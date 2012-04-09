{
   Double commander
   -------------------------------------------------------------------------
   Definitions of some common types.

   Copyright (C) 2012 Przemyslaw Nagay (cobines@gmail.com)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit uTypes;

// This unit should depend on as little other units as possible.

interface

type
// plugin types
  TPluginType = (ptDSX, ptWCX, ptWDX, ptWFX, ptWLX);

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

implementation

end.
