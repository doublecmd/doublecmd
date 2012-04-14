{
   Double commander
   -------------------------------------------------------------------------
   Definitions of basic types.

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

unit DCBasicTypes;

interface

type
  TLibHandle = PtrInt;

  TDynamicStringArray = array of String;
  TCharSet = set of Char;

  TFileAttrs = Cardinal;     // file attributes type regardless of system

  TWinFileTime = QWord;      // NTFS time (UTC) (2 x DWORD)
  TDosFileTime = LongInt;    // MS-DOS time (local)

{$IFDEF MSWINDOWS}
  TFileTime = TWinFileTime;
{$ELSE}
  // Unix time (UTC).
  // Unix defines time_t as signed integer,
  // but we define it as unsigned because sign is not needed.
  {$IFDEF cpu64}
  TFileTime = QWord;
  {$ELSE}
  TFileTime = DWord;
  {$ENDIF}
{$ENDIF}

  TUnixFileTime = TFileTime;

  PFileTime = ^TFileTime;
  PWinFileTime = ^TWinFileTime;

implementation

end.
