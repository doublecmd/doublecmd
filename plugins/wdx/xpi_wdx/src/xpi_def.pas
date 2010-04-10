{
   Double commander
   -------------------------------------------------------------------------
   xpi_wdx is destined to obtainin the information from xpi-files
   in which extensions and themes for Gecko-based applications are distributed.

   Copyright (C) 2010 Koblov Alexander (Alexx2000@mail.ru)

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
}

unit xpi_def;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type
  TTargetApplication = record
    ID,
    MinVersion,
    MaxVersion: AnsiString;
  end;

  TInstallManifest = record
    ID: AnsiString;
    Name: AnsiString;
    Version: AnsiString;
    Description: AnsiString;
    Creator: AnsiString;
    Developer: AnsiString;
    Contributor,
    Translator,
    HomePageURL,
    UpdateURL,
    TargetPlatform: AnsiString;
  end;
  PInstallManifest = ^TInstallManifest;

implementation

end.

