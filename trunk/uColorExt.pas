{
Seksi Commander
----------------------------
Licence  : GNU GPL v 2.0
Author   : radek.cervinka@centrum.cz

Authors:
  Radek Cervinka, radek.cervinka@centrum.cz

format of file:

ext color
ext2 color
where color is like $00rrggbb rr=hex red value ...
}

unit uColorExt;

interface
uses
  Classes, Graphics;
type
  TColorExt=Class
  protected
    lsExts:TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    function ColorByExt(const sExt:String):TColor;
    procedure LoadFromFile(const sFileName:String);
    procedure SaveToFile(const sFileName:String);
  end;

implementation

uses
  SysUtils;

constructor TColorExt.Create;
begin
  inherited;
  lsExts:=TStringList.Create;
end;

destructor TColorExt.Destroy;
begin
  if assigned(lsExts) then
    FreeAndNil(lsExts);
end;

function TColorExt.ColorByExt(const sExt:String):TColor;
var
  iIndex:Integer;
begin
  Result:=0; //$0000ff00;
  if sExt='' then Exit;
  if sExt[1]='.' then
    iIndex:= lsExts.IndexOf(UpperCase(Copy(sExt,2, Length(sExt)-1)))
   else
    iIndex:= lsExts.IndexOf(UpperCase(sExt));
  if iIndex=-1 then Exit;
  Result:=TColor(lsExts.Objects[iIndex]);
end;

procedure TColorExt.LoadFromFile(const sFileName:String);
var
  f:TextFile;
  sExt, sLine:String;
  sColor:String;
  iColor:Integer;
begin
  lsExts.Clear;
  if not FileExists(sFileName) then
    Exit;
  assign(f,sFileName);
  reset(f);
  try
    while not eof(f) do
    begin
      readln(f,sLine);
      if sLine='' then Continue;
      if sLine[1]='#' then Continue;
      sExt:=Copy(sLine,1,Pos(':',sLine)-1);
      sExt:=Trim(Uppercase(sExt));
      sColor:=Trim(Copy(sLine,Pos(':',sLine)+1, Length(sLine)));
      if sExt='' then Continue;
      if sExt[1]='.' then
        Delete(sExt,1,1);
      iColor:=StrToIntDef(sColor, Integer(clText));
      lsExts.AddObject(sExt,TObject(iColor));
    end;
  finally
    closefile(f);
  end;  

end;

procedure TColorExt.SaveToFile(const sFileName:String);
begin


end;

end.
