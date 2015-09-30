{
  Double Commander
  -------------------------------------------------------------------------
  SevenZip archiver plugin, language support

  Copyright (C) 2014-2015 Alexander Koblov (alexx2000@mail.ru)

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
}

unit SevenZipLng;

{$mode delphi}

interface

uses
  Classes, SysUtils;

resourcestring
  rsSevenZipLoadError = 'Failed to load 7z.dll';
  rsSevenZipSfxNotFound = 'Cannot find specified SFX module';

resourcestring
  rsCompressionLevelStore = 'Store';
  rsCompressionLevelFastest = 'Fastest';
  rsCompressionLevelFast = 'Fast';
  rsCompressionLevelNormal = 'Normal';
  rsCompressionLevelMaximum = 'Maximum';
  rsCompressionLevelUltra = 'Ultra';

  rsSolidBlockSolid = 'Solid';
  rsSolidBlockNonSolid = 'Non-solid';

implementation

end.

