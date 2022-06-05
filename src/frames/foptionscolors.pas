unit fOptionsColors;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, Dialogs,
  fOptionsFrame, fOptionsGroups;

type

  { TfrmOptionsColors }

  TfrmOptionsColors = class(TOptionsColorsGroup)
    rgDarkMode: TRadioGroup;
  private
    FAppMode: Integer;
  protected
    procedure Init; override;
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  public
    class function IsEmpty: Boolean; override;
  end;

resourcestring
  rsDarkModeOptions = 'Auto;Enabled;Disabled';

implementation

{$R *.lfm}

uses
  DCStrUtils, uEarlyConfig, uDarkStyle;

{ TfrmOptionsColors }

procedure TfrmOptionsColors.Init;
begin
  FAppMode:= gAppMode;
  ParseLineToList(rsDarkModeOptions, rgDarkMode.Items);
end;

procedure TfrmOptionsColors.Load;
begin
  case FAppMode of
    1: rgDarkMode.ItemIndex:= 0;
    2: rgDarkMode.ItemIndex:= 1;
    3: rgDarkMode.ItemIndex:= 2;
  end;
end;

function TfrmOptionsColors.Save: TOptionsEditorSaveFlags;
begin
  Result:= [];
  case rgDarkMode.ItemIndex of
    0: gAppMode:= 1;
    1: gAppMode:= 2;
    2: gAppMode:= 3;
  end;
  if gAppMode <> FAppMode then
  try
    SaveEarlyConfig;
    Result:= [oesfNeedsRestart];
  except
    on E: Exception do MessageDlg(E.Message, mtError, [mbOK], 0);
  end;
end;

class function TfrmOptionsColors.IsEmpty: Boolean;
begin
  Result:= not g_darkModeSupported;
end;

end.

