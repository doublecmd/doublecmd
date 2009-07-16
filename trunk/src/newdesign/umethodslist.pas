unit uMethodsList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StringHashList;

type

  {en
     Stores association between method name and its address.

     StringHashList is used for this purpose, which may be faster
     than linear scanning by using MethodAddress on the given object.
  }
  TMethodsList = class
  private
    FMethods: TStringHashList;
    FInstanceObject: TObject;

    procedure GetMethodsList;

  public
    {en
       Creates methods list.
       @param(AnObject is an object of which we want the list of methods.)
    }
    constructor Create(AnObject: TObject); virtual reintroduce;
    destructor Destroy; override;

    function GetMethod(Name: String): TMethod;
  end;

implementation

constructor TMethodsList.Create(AnObject: TObject);
begin
  inherited Create;

  FInstanceObject := AnObject;
  FMethods := TStringHashList.Create(False); // False = not case-sensitive
  GetMethodsList;
end;

destructor TMethodsList.Destroy;
begin
  FreeAndNil(FMethods);
  inherited;
end;

procedure TMethodsList.GetMethodsList;
type
   pmethodnamerec = ^tmethodnamerec;
   tmethodnamerec = packed record
      name : pshortstring;
      addr : pointer;
   end;

   tmethodnametable = packed record
     count : dword;
     entries : tmethodnamerec; // first entry
     // subsequent tmethodnamerec records follow
   end;

   pmethodnametable =  ^tmethodnametable;

var
 methodtable : pmethodnametable;
 i : dword;
 vmt : tclass;
 pentry: pmethodnamerec;

begin
   vmt := FInstanceObject.ClassType;
   while assigned(vmt) do
     begin
        methodtable := pmethodnametable((Pointer(vmt)+vmtMethodTable)^);
        if assigned(methodtable) then
        begin
          pentry := @methodtable^.entries;
          for i := 0 to methodtable^.count - 1 do
            FMethods.Add(pentry[i].name^, pentry[i].addr);
        end;
        vmt := pclass(pointer(vmt) + vmtParent)^;
     end;
end;

function TMethodsList.GetMethod(Name: String): TMethod;
var
  Index: Integer;
begin
  Index := FMethods.Find(Name);
  if Index = -1 then
  begin
    Result.Code := nil;
    Result.Data := nil;
  end
  else
  begin
    Result.Code := FMethods.List[Index]^.Data;  // address of method
    Result.Data := FInstanceObject;             // pointer to instance
  end;
end;

end.

