{
   Double commander
   -------------------------------------------------------------------------
   svn_wdx is a content plugin that displays some information from subversion

   Copyright (C) 2011 Koblov Alexander (Alexx2000@mail.ru)

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as 
   published by the Free Software Foundation, either version 3 of the 
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
}

unit svn_def;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

const
  // Information offset relative to file name index
  SVN_CHECKSUM = 7;
  SVN_REVISION = 9;
  SVN_AUTHOR   = 10;

implementation

end.

