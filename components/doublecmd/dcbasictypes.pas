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

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

type
  TDynamicStringArray = array of String;
  TCharSet = set of Char;

  TFileAttrs = Cardinal;     // file attributes type regardless of system

  TWinFileTime = QWord;      // NTFS time (UTC) (2 x DWORD)
  TDosFileTime = LongInt;    // MS-DOS time (local)

{$IFDEF MSWINDOWS}
  TFileTime = TWinFileTime;
  TFileTimeEx = TFileTime;
{$ELSE}
  TFileTime = Int64;

  TFileTimeEx = record
    public
      sec: int64;
      nanosec: int64;
    public
      constructor create( aSec:int64; aNanosec:int64=0 );
      class operator =(l,r : TFileTimeEx): Boolean;
  end;
{$ENDIF}

  TUnixFileTime = TFileTime;

  PFileTime = ^TFileTime;
  PWinFileTime = ^TWinFileTime;

const
  TFileTimeExNull: TFileTimeEx = {$IFDEF MSWINDOWS} 0 {$ELSE} (sec:0; nanosec:-1) {$ENDIF};

implementation

{$IF not DEFINED(MSWINDOWS)}
constructor TFileTimeEx.create( aSec:int64; aNanosec:int64 );
begin
  self.sec:= aSec;
  self.nanosec:= aNanosec;
end;

class operator TFileTimeEx.=(l,r : TFileTimeEx): Boolean;
begin
  Result:= (l.sec=r.sec) and (l.nanosec=r.nanosec);
end;
{$ENDIF}

end.
