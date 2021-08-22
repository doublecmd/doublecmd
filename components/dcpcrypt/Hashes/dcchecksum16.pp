{
    Double Commander
    -------------------------------------------------------------------------
    Simple 16-bits checksum algrotithm

    Copyright (C) 2021 Alexander Koblov (alexx2000@mail.ru)

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

unit dcchecksum16;

{$mode objfpc}{$H+}

interface

function checksum16_bytes(data: PByte; length: longword; previousChecksum: WORD = 0): WORD;

implementation
{$R-}{$Q-}

{ checksum16_bytes }
function checksum16_bytes(data: PByte; length: longword; previousChecksum: WORD = 0): WORD;
var
  iIndex: longword;
begin
  result := previousChecksum;

  for iIndex := 1 to length do
  begin
    result := result + data^;
    inc(data);
  end;
end;

end.

