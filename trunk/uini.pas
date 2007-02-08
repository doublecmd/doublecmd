{
Seksi Commander
----------------------------
Licence  : GNU GPL v 2.0
Author   : radek.cervinka@centrum.cz

very simple implementation of ini file

contributors:

}


unit uIni;

interface
uses
  Classes;
type
  TIni=Class
  private
    fIniFile:TStringList;
    fChanged:Boolean;
    fIniName:String;
  public
    Constructor Create(const sIniName:String);
    Destructor Destroy; override;
    function GetValue(const sKey:String):String;
    procedure SetValue(const sKey, sValue:String);
    procedure Save;
    property Value[const sKey: string]:String read GetValue write SetValue;
  end;

implementation
uses
  SysUtils;
Constructor TIni.Create(const sIniName:String);
begin
  fIniFile:=TStringList.Create;
  fIniName:=ExpandFileName(sIniName);
  fIniFile.LoadFromFile(sIniName);
  fChanged:=False;
end;

Destructor TIni.Destroy;
begin
  if assigned(fIniFile) then
  begin
{    try
      Save;
    finally}
      FreeAndNil(fIniFile);
//    end;
  end;
end;

function TIni.GetValue(const sKey:String):String;
begin
  Result:=fIniFile.Values[sKey];
end;

procedure TIni.SetValue(const sKey, sValue:String);
begin
  if sValue<>GetValue(sKey) then
  begin
    fIniFile.Values[sKey]:=sValue;
    fChanged:=True;
  end;
end;

procedure TIni.Save;
begin
  if fChanged then fIniFile.SaveToFile(fIniName);
end;

end.

