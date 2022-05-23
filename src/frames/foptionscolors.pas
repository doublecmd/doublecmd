unit fOptionsColors;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, DCClassesUtf8, fOptionsFrame,
  fOptionsGroups;

type

  { TfrmOptionsColors }

  TfrmOptionsColors = class(TOptionsColorsGroup)
    rgDarkMode: TRadioGroup;
  private
    FMode: Integer;
    FConfig: TIniFileEx;
  protected
    procedure Init; override;
    procedure Load; override;
    procedure Done; override;
    function Save: TOptionsEditorSaveFlags; override;
  public
    class function IsEmpty: Boolean; override;
  end;

resourcestring
  rsDarkModeOptions = 'Auto;Enabled;Disabled';

implementation

{$R *.lfm}

uses
  DCStrUtils, uShowMsg, uGlobsPaths, uDarkStyle;

{ TfrmOptionsColors }

procedure TfrmOptionsColors.Init;
begin
  try
    FConfig:= TIniFileEx.Create(gpCfgDir + 'doublecmd.ini');
  except
    on E: Exception do msgError(E.Message);
  end;
  ParseLineToList(rsDarkModeOptions, rgDarkMode.Items);
end;

procedure TfrmOptionsColors.Load;
begin
  FMode:= FConfig.ReadInteger('General', 'DarkMode', 1);
  case FMode of
    1: rgDarkMode.ItemIndex:= 0;
    2: rgDarkMode.ItemIndex:= 1;
    3: rgDarkMode.ItemIndex:= 2;
  end;
end;

procedure TfrmOptionsColors.Done;
begin
  FConfig.Free;
end;

function TfrmOptionsColors.Save: TOptionsEditorSaveFlags;
var
  AMode: Integer;
begin
  Result:= [];
  case rgDarkMode.ItemIndex of
    0: AMode:= 1;
    1: AMode:= 2;
    2: AMode:= 3;
  end;
  if FMode <> AMode then
  try
    FConfig.WriteInteger('General', 'DarkMode', AMode);
    FConfig.UpdateFile;
    Result:= [oesfNeedsRestart];
  except
    on E: Exception do msgError(E.Message);
  end;
end;

class function TfrmOptionsColors.IsEmpty: Boolean;
begin
  Result:= not g_darkModeSupported;
end;

end.

