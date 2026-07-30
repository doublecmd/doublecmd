unit uCocoaModernFormConfig;

{$if NOT defined(DisableCocoaModernForm)}

{$include ucocoamodernformconfig.inc}

{$else}

interface

type

  { TDCCocoaModernFormUtils }

  TDCCocoaModernFormUtils = class
    class procedure initConfig;
    class function isEnabled: Boolean;
    class procedure checkAndSetPrivilegeItem;
  end;

implementation

class procedure TDCCocoaModernFormUtils.initConfig;
begin
end;

class function TDCCocoaModernFormUtils.isEnabled: Boolean;
begin
  Result:= False;
end;

class procedure TDCCocoaModernFormUtils.checkAndSetPrivilegeItem;
begin
end;

{$endif}

end.
