{
Seksi Commander
----------------------------
Licence  : GNU GPL v 2.0
Author   : radek.cervinka@centrum.cz

check file name with wildcards

contributors:

}

unit uFilter;
{$mode objfpc}{$H+}
interface
type
  TFilter=Class

  protected
    FMaskFileName:String;
    FMaskFileExt:String;
    FbNameHasWild:Boolean;
    FbExtHasWild:Boolean;
    procedure SetFileMask(const Value:String);
    function GetFileMask:String;
  public
    constructor Create;
    Function CheckFileMask(const s:String):Boolean;
    property FileMask:String read GetFileMask write SetFileMask;
  end;

procedure DivFileName(const sFileName:String; var n,e:String);
  

implementation
uses
  SysUtils;

procedure DivFileName(const sFileName:String; var n,e:String);
var
  i:Integer;
begin
  for i:= length(sFileName) downto 1 do
    if sFileName[i]='.' then
    begin
//      if i>1 then // hidden files??
      e:=Copy(sFileName,i,Length(sFileName)-i+1);
      n:=Copy(sFileName,1,i-1);
      Exit;
    end;
  e:='';
  n:=sFileName;
end;

constructor TFilter.Create;
begin
  inherited;
  FileMask:='*.*';
end;

procedure TFilter.SetFileMask(const Value:String);
begin
  DivFileName(Value,FMaskFileName, FMaskFileExt);
  FbNameHasWild:=pos('*',FMaskFileName)>0;
  FbExtHasWild:=pos('*',FMaskFileExt)>0;
  if FbNameHasWild then
    FMaskFileName:=StringReplace(FMaskFileName,'*','',[rfReplaceAll]);
  if FbExtHasWild then
    FMaskFileExt:=StringReplace(FMaskFileExt,'*','',[rfReplaceAll]);
end;

function TFilter.GetFileMask:String;
begin
  Result:=FMaskFileName+FMaskFileExt;
end;

Function TFilter.CheckFileMask(const s:String):Boolean;
var
  sName, sExt:String;
  bNameOK, bExtOK:Boolean;
begin
  DivFileName(s,sName, sExt);
// only needed tests are performed

// First we check if mask exists or only *.*
  bNameOK:=(FMaskFileName='');
  bExtOK:=(FMaskFileExt='.');
  Result:=bNameOK AND bExtOK;
  if Result then Exit;

  // check if strings is =
  if not bNameOK then
  begin
    if FMaskFileName=sName then
      bNameOK:=True;
  end;
  if not bExtOK then
  begin
    if FMaskFileExt=sExt then
      bExtOK:=True;
  end;
  Result:=bNameOK AND bExtOK;
  if Result then Exit;

  // now examine *bla*  -- case sensitive
  if FbNameHasWild and not bNameOK then
  begin
    if Pos(FMaskFileName,s)>0 then
      bNameOK:=True;
  end;
  if FbExtHasWild and not bExtOK then
  begin
    if Pos(FMaskFileExt,s)>0 then
      bExtOK:=True;
  end;
  Result:=bNameOK AND bExtOK;
  if Result then Exit;

 //
end;

end.
