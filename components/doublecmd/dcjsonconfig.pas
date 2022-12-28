unit DCJsonConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpJson;

type

  { TJsonConfig }

  TJsonConfig = class
  private
    FRoot: TJSONObject;
  public
    procedure SaveToFile(const FileName: String);
    procedure LoadFromFile(const FileName: String);
    property Root: TJSONObject read FRoot;
  end;

implementation

uses
  DCClassesUtf8;

{ TJsonConfig }

procedure TJsonConfig.SaveToFile(const FileName: String);
begin
  with TStringListEx.Create do
  try
    Text:= FRoot.FormatJSON();
    SaveToFile(FileName);
  finally
    Free;
  end;
end;

procedure TJsonConfig.LoadFromFile(const FileName: String);
var
  AStream: TFileStreamEx;
begin
  AStream:= TFileStreamEx.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    FRoot:= GetJSON(AStream, True) as TJSONObject;
  finally
    AStream.Free;
  end;
end;

end.

