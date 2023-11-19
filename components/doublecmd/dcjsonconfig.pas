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
    constructor Create;
    destructor Destroy; override;
    procedure SaveToFile(const FileName: String);
    procedure LoadFromFile(const FileName: String);
    property Root: TJSONObject read FRoot;
  end;

implementation

uses
  DCClassesUtf8;

{ TJsonConfig }

constructor TJsonConfig.Create;
begin
  FRoot:= TJSONObject.Create;
end;

destructor TJsonConfig.Destroy;
begin
  inherited Destroy;
  FRoot.Free;
end;

procedure TJsonConfig.SaveToFile(const FileName: String);
begin
  with TStringListEx.Create do
  try
    Text:= FRoot.FormatJSON([foDoNotQuoteMembers]);
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
    FreeAndNil(FRoot);
    FRoot:= GetJSON(AStream, True) as TJSONObject;
  finally
    AStream.Free;
  end;
end;

end.

