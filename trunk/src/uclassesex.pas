{
   Double commander
   -------------------------------------------------------------------------
   This module contains additional or extended classes.

   Copyright (C) 2008-2014 Alexander Koblov (alexx2000@mail.ru)

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

unit uClassesEx;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniPropStorage, SynEdit;

type

  { TBlobStream }

  TBlobStream = class(TCustomMemoryStream)
  public
    constructor Create(Ptr: Pointer; ASize: PtrInt);
  end;

  { TIniPropStorageEx }

  TIniPropStorageEx = class(TCustomIniPropStorage)
  private
    function ChangeIdent(const Ident: String): String;
  protected
    function IniFileClass: TIniFileClass; override;
  public
    procedure Restore; override;
    function DoReadString(const Section, Ident, default: string): string; override;
    procedure DoWriteString(const Section, Ident, Value: string); override;
  end;

  { TSynEditHelper }

  TSynEditHelper = class helper for TSynEdit
  public
    procedure FixDefaultKeystrokes;
  end;

implementation

uses
  LCLType, Forms, Controls, SynEditKeyCmds, DCStrUtils, DCClassesUtf8;

{ TBlobStream }

constructor TBlobStream.Create(Ptr: Pointer; ASize: PtrInt);
begin
  inherited Create;
  SetPointer(Ptr, ASize);
end;

{ TIniPropStorageEx }

function TIniPropStorageEx.IniFileClass: TIniFileClass;
begin
  Result:= TIniFileEx;
end;

procedure TIniPropStorageEx.Restore;
var
  AMonitor: TMonitor;
begin
  inherited Restore;

  if Self.Owner is TCustomForm then
  begin
    with TCustomForm(Self.Owner) do
    begin
      // Refresh monitor list
      Screen.UpdateMonitors;

      AMonitor:= Screen.MonitorFromPoint(Classes.Point(Left, Top));
      if Assigned(AMonitor) then MakeFullyVisible(AMonitor, True);

      // Workaround for bug: http://bugs.freepascal.org/view.php?id=18514
      if WindowState = wsMinimized then WindowState:= wsNormal;
    end;
  end;
end;

function TIniPropStorageEx.DoReadString(const Section, Ident, default: string): string;
begin
  Result := inherited DoReadString(Section, ChangeIdent(Ident), default);
end;

procedure TIniPropStorageEx.DoWriteString(const Section, Ident, Value: string);
begin
  inherited DoWriteString(Section, ChangeIdent(Ident), Value);
end;

function TIniPropStorageEx.ChangeIdent(const Ident: String): String;
begin
  // Change component name to class name.
  if StrBegins(Ident, Owner.Name) then
    Result := Owner.ClassName + Copy(Ident, 1 + Length(Owner.Name), MaxInt)
  else
    Result := Ident;
end;

{ TSynEditHelper }

procedure TSynEditHelper.FixDefaultKeystrokes;

  procedure AddKey(const ACmd: TSynEditorCommand; const AKey: Word;
     const AShift: TShiftState; const AShiftMask: TShiftState = []);
  begin
    with Keystrokes.Add do
    begin
      Key       := AKey;
      Shift     := AShift;
      ShiftMask := AShiftMask;
      Command   := ACmd;
    end;
  end;

begin
  AddKey(ecCopy, VK_C, [ssModifier]);
  AddKey(ecSelectAll, VK_A, [ssModifier]);
end;

end.
