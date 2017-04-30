{
   Double commander
   -------------------------------------------------------------------------
   This module contains additional or extended classes.

   Copyright (C) 2008-2017 Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.
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
    FPixelsPerInch: Integer;
    function ChangeIdent(const Ident: String): String;
  protected
    procedure SaveProperties; override;
    function IniFileClass: TIniFileClass; override;
  public
    procedure Restore; override;
    function DoReadString(const Section, Ident, Default: string): string; override;
    procedure DoWriteString(const Section, Ident, Value: string); override;
  end;

  { TSynEditHelper }

  TSynEditHelper = class helper for TSynEdit
  public
    procedure FixDefaultKeystrokes;
  end;

implementation

uses
  LCLType, Forms, Controls, LCLVersion, SynEditKeyCmds, DCStrUtils, DCClassesUtf8;

{ TBlobStream }

constructor TBlobStream.Create(Ptr: Pointer; ASize: PtrInt);
begin
  inherited Create;
  SetPointer(Ptr, ASize);
end;

{ TIniPropStorageEx }

procedure TIniPropStorageEx.SaveProperties;
begin
  inherited SaveProperties;
  IniFile.WriteInteger(IniSection, 'Screen_PixelsPerInch', Screen.PixelsPerInch);
end;

function TIniPropStorageEx.IniFileClass: TIniFileClass;
begin
  Result:= TIniFileEx;
end;

procedure TIniPropStorageEx.Restore;
var
  AMonitor: TMonitor;
begin
  StorageNeeded(True);
  try
    FPixelsPerInch := IniFile.ReadInteger(IniSection, 'Screen_PixelsPerInch', Screen.PixelsPerInch);
    inherited Restore;
  finally
    FreeStorage;
  end;

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

function TIniPropStorageEx.DoReadString(const Section, Ident, Default: string): string;
var
  Value: Integer;
  Form: TCustomForm;
begin
  Result := inherited DoReadString(Section, ChangeIdent(Ident), Default);
{$if lcl_fullversion >= 1070000}
  // Workaround for bug: http://bugs.freepascal.org/view.php?id=31526
  if (Self.Owner is TCustomForm) and (TCustomForm(Self.Owner).Scaled) then
  begin
    Form := TCustomForm(Self.Owner);
    if (Form.DesignTimePPI <> FPixelsPerInch) then
    begin
      if StrEnds(Ident, '_Width') or StrEnds(Ident, '_Height') then
      begin
        if TryStrToInt(Result, Value) then
        begin
          Result := IntToStr(MulDiv(Value, Form.DesignTimePPI, FPixelsPerInch));
        end;
      end;
    end;
  end;
{$endif}
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
