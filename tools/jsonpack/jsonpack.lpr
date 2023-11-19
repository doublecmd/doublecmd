program jsonpack;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils,
  Classes,
  JsonParser,
  fpJson;

var
  AFileName: String;
  AConfig: TJSONData;
  AStream: TFileStream;
  AOptions: TFormatOptions;
begin
  if ParamStr(1) = '-c' then
  begin
    AOptions:= AsCompactJSON;
  end
  else if ParamStr(1) = '-d' then
  begin
    AOptions:= [foDoNotQuoteMembers]
  end
  else begin
    WriteLn;
    WriteLn(ExtractFileName(ParamStr(0)), ' <Options> <json-file>');
    WriteLn;
    WriteLn('Options:');
    WriteLn(' -c                  compress json-file');
    WriteLn(' -d                  decompress json-file');
    WriteLn;
    Exit;
  end;
  AFileName:= ParamStr(2);

  AStream:= TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    AConfig:= GetJSON(AStream, True);
  finally
    AStream.Free;
  end;
  try
    with TStringList.Create do
    try
      Text:= AConfig.FormatJSON(AOptions);
      SaveToFile(AFileName);
    finally
      Free;
    end;
  finally
    AConfig.Free;
  end;
end.

