{
   Double Commander
   -------------------------------------------------------------------------
   This unit contains specific DARWIN functions.

   Copyright (C) 2016-2024 Alexander Koblov (alexx2000@mail.ru)

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

unit uMyDarwin;

{$mode delphi}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils, UnixType,
  InterfaceBase, Menus, Controls, Forms,
  uFileProperty, uDisplayFile, uFileView, uColumnsFileView,
  uLng,
  MacOSAll, CocoaAll, QuickLookUI,
  CocoaUtils, CocoaInt, CocoaPrivate, CocoaConst, CocoaMenus, Cocoa_Extra,
  uDarwinApplication, uDarwinFSWatch, uDarwinFinder, uDarwinFinderModel, uDarwinUtil;

implementation

uses
  DynLibs;

end.
