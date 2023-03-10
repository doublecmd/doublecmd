{
   Double Commander
   -------------------------------------------------------------------------
   This unit contains specific DARWIN FSEvent functions.

   Copyright (C) 2023 Alexander Koblov (alexx2000@mail.ru)
   Copyright (C) 2023 Rich Chang (rich2014.git@outlook.com)

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.

   Notes:
   1. TCocoaWSCustomComboBoxEx disalbe AutoComplete currently.
      it is a workaround for the bug of Lazarus related IME of Cocoa.
      related codes can be removed after Lazarus merges related Patches.
      see also:
      https://gitlab.com/freepascal.org/lazarus/lazarus/-/issues/40008
   2. TCocoaWSCustomComboBoxEx.FreeItems is a workaround for the bug of Lazarus
      related IME of Cocoa too.
      see also:
      https://gitlab.com/freepascal.org/lazarus/lazarus/-/merge_requests/117
}

unit uCocoaWidgetSetFix;

{$mode delphi}
{$modeswitch objectivec1}

interface

procedure Initialize;

implementation

uses
  Classes, SysUtils,
  LCLType, Controls, StdCtrls, WSLCLClasses, WSStdCtrls,
  CocoaAll, CocoaWSStdCtrls, CocoaTextEdits;

type

 { TCocoaWSCustomComboBoxEx }

 TCocoaWSCustomComboBoxEx = class(TCocoaWSCustomComboBox)
 private
   class function getNSText(const ACustomComboBox: TCustomComboBox): NSText;
 published
   class function CreateHandle(const AWinControl: TWinControl;
     const AParams: TCreateParams): TLCLIntfHandle; override;
   class procedure FreeItems(var AItems: TStrings); override;
   class procedure DestroyHandle(const AWinControl: TWinControl); override;

   class function  GetSelStart(const ACustomComboBox: TCustomComboBox): integer; override;
   class function  GetSelLength(const ACustomComboBox: TCustomComboBox): integer; override;
   class procedure SetSelStart(const ACustomComboBox: TCustomComboBox; NewStart: integer); override;
   class procedure SetSelLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); override;
 end;


class function TCocoaWSCustomComboBoxEx.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
  TCustomComboBox(AWinControl).AutoComplete:= false;
  Result:= Inherited;
end;


class procedure TCocoaWSCustomComboBoxEx.FreeItems(var AItems: TStrings);
begin
  // TCocoaComboBox.list should be released later (in DestroyHandle),
  // to avoid invalid TCocoaComboBox.list in TCocoaComboBox.comboBox_indexOfItemWithStringValue().
  // which will be called even in TCocoaWSWinControl.DestoryWnd(),
  // when TCocoaComboBox hasMarkedText (in IME state).
end;

class procedure TCocoaWSCustomComboBoxEx.DestroyHandle(const AWinControl: TWinControl);
var
  txt: NSText;
  list: TStrings;
  cocoaObj: NSObject;
begin
  txt:= getNSText( TCustomComboBox(AWinControl) );
  if not Assigned(txt) then exit;

  // close IME first
  {%H-}NSTextInputClientProtocol(txt).unmarktext;
  Inherited;

  // and then free the TCocoaComboBox.list
  list:= nil;
  cocoaObj:= NSObject( AWinControl.Handle );
  if cocoaObj.isKindOfClass(TCocoaComboBox.classClass) then
    list:= TCocoaComboBox(AWinControl.Handle).list
  else if cocoaObj.isKindOfClass(TCocoaReadOnlyComboBox.classClass) then
    list:= TCocoaReadOnlyComboBox(AWinControl.Handle).list;
  if Assigned(list) then
    Inherited FreeItems( list );
end;

class function TCocoaWSCustomComboBoxEx.getNSText(const ACustomComboBox: TCustomComboBox): NSText;
var
  control: NSControl;
begin
  Result:= nil;
  if not Assigned(ACustomComboBox) or (not ACustomComboBox.HandleAllocated) or (ACustomComboBox.Handle=0) then
    exit;
  control:= NSControl( ACustomComboBox.Handle );
  Result:= control.currentEditor;
end;

class function TCocoaWSCustomComboBoxEx.GetSelStart(
  const ACustomComboBox: TCustomComboBox): integer;
var
  txt: NSText;
begin
  Result:= 0;
  txt:= getNSText( ACustomComboBox );
  if Assigned(txt) then
    Result:= txt.selectedRange.location;
end;

class function TCocoaWSCustomComboBoxEx.GetSelLength(
  const ACustomComboBox: TCustomComboBox): integer;
var
  txt: NSText;
begin
  Result:= 0;
  txt:= getNSText( ACustomComboBox );
  if Assigned(txt) then
    Result:= txt.selectedRange.length;
end;

class procedure TCocoaWSCustomComboBoxEx.SetSelStart(
  const ACustomComboBox: TCustomComboBox; NewStart: integer);
var
  txt: NSText;
  range: NSRange;
begin
  txt:= getNSText( ACustomComboBox );
  if not Assigned(txt) then
    exit;
  range:= txt.selectedRange;
  range.location:= NewStart;
  txt.setSelectedRange( range );
end;

class procedure TCocoaWSCustomComboBoxEx.SetSelLength(
 const ACustomComboBox: TCustomComboBox; NewLength: integer);
var
  txt: NSText;
  range: NSRange;
begin
  txt:= getNSText( ACustomComboBox );
  if not Assigned(txt) then
    exit;
  range:= txt.selectedRange;
  range.length:= NewLength;
  txt.setSelectedRange( range );
end;

procedure Initialize;
begin
 // Replace TCustomComboBox widgetset class
  WSStdCtrls.RegisterCustomComboBox;
  RegisterWSComponent(TCustomComboBox, TCocoaWSCustomComboBoxEx);
end;

end.
