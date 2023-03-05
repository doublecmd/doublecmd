unit uCocoaWidgetSetFix;

{$mode delphi}
{$modeswitch objectivec1}

interface

procedure Initialize;

implementation

uses
  Classes, SysUtils,
  StdCtrls, WSLCLClasses, WSStdCtrls,
  CocoaAll, CocoaWSStdCtrls;

type

 { TCocoaWSCustomComboBoxEx }

 TCocoaWSCustomComboBoxEx = class(TCocoaWSCustomComboBox)
 private
   class function getNSText(const ACustomComboBox: TCustomComboBox): NSText;
 published
   class function  GetSelStart(const ACustomComboBox: TCustomComboBox): integer; override;
   class function  GetSelLength(const ACustomComboBox: TCustomComboBox): integer; override;
   class procedure SetSelStart(const ACustomComboBox: TCustomComboBox; NewStart: integer); override;
   class procedure SetSelLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); override;
 end;

class function TCocoaWSCustomComboBoxEx.getNSText(const ACustomComboBox: TCustomComboBox): NSText;
var
  control: NSControl;
begin
  Result:= nil;
  if not Assigned(ACustomComboBox) or (not ACustomComboBox.HandleAllocated) or (ACustomComboBox.Handle=0) then
    exit;
  control:= NSControl( ACustomComboBox.Handle );
  Result:= NSText( control.currentEditor );
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
