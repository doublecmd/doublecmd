{
  Part of Seksi Commander
  author: Radek Cervinka, radek.cervinka@centrum.cz

  Store path alias, for archive, symlink, ftp support
  (for return to previous directory)

  It's work as stack

  realised under GNU GPL 2
}

unit uPathHistory;

{$mode objfpc}{$H+}

interface
uses
  contnrs;

type
  TAliasObject= Class
    Path:String;
    Alias:String;
  end;

  TPathHistory= Class
  protected
    FAliasList:TObjectList;
  public
    constructor Create;
    function AddAlias(const sPath, sAlias: String):Integer;
    function IsEmpty:Boolean;
    function GetLastPath(var sPath:String):Boolean; overload;
    function GetLastPath:String; overload;
    function GetLastPathRemove(var sPath:String):Boolean;
    function GetLastAlias(var sAlias:String):Boolean;
    function GetLastAliasRemove(var sAlias:String):Boolean;

    procedure DeleteLast;
    procedure ClearPaths;
    destructor Destroy; override;
  end;
implementation

uses
  SysUtils;
{ TPathHistory }

function TPathHistory.AddAlias(const sPath, sAlias: String):Integer;
var
  ao:TAliasObject;
begin
  ao:=TAliasObject.Create;
  ao.Path:=sPath;
  ao.Alias:=sAlias;
  Result:=FAliasList.Add(ao);
end;

constructor TPathHistory.Create;
begin
  FAliasList:=TObjectList.Create;
  FAliasList.OwnsObjects:=True; // object list free memory of object if used delete
end;

procedure TPathHistory.DeleteLast;
begin
  if not IsEmpty then
    FAliasList.Delete(FAliasList.Count-1);
end;

destructor TPathHistory.Destroy;
begin
  if assigned(FAliasList) then
    FreeAndNil(FAliasList);
  inherited;
end;

function TPathHistory.GetLastPath(var sPath: String): Boolean;
begin
  Result:=False;
  if IsEmpty then Exit;
  sPath:=TAliasObject(FAliasList.Items[FAliasList.Count-1]).Path;
  Result:=True;
end;

function TPathHistory.GetLastPath: String;
begin
  Result:='';
  if IsEmpty then Exit;
  Result:=TAliasObject(FAliasList.Items[FAliasList.Count-1]).Path;
end;


function TPathHistory.GetLastAlias(var sAlias: String): Boolean;
begin
  Result:=False;
  if IsEmpty then Exit;
  sAlias:=TAliasObject(FAliasList.Items[FAliasList.Count-1]).Alias;
  Result:=True;
end;


function TPathHistory.GetLastPathRemove(var sPath: String): Boolean;
begin
  Result:=False;
  if IsEmpty then Exit;
  sPath:=TAliasObject(FAliasList.Items[FAliasList.Count-1]).Path;
  DeleteLast;
  Result:=True;
end;

function TPathHistory.GetLastAliasRemove(var sAlias: String): Boolean;
begin
  Result:=False;
  if IsEmpty then Exit;
  sAlias:=TAliasObject(FAliasList.Items[FAliasList.Count-1]).Alias;
  DeleteLast;
  Result:=True;
end;


function TPathHistory.IsEmpty: Boolean;
begin
  Result:=FAliasList.Count=0;
end;

procedure TPathHistory.ClearPaths;
begin
  FAliasList.Clear;
end;


end.
