unit uActs;

{$mode objfpc}{$H+}{$M+}

interface

uses
  Classes, SysUtils,Dialogs,typinfo;

const cf_Null=0;
      cf_Error=-1;
  
  type

   TIntFunc=procedure(param:string; var Result:integer) of object;

  { TActs }

  TActs=class
  private
   FCmdList:TStrings;
//   function Methods(AClass:TClass) : TStringList;
   function GetList:TStrings;
  public
   constructor Create;
   destructor Destroy;override;
   function Execute(Cmd,param:string):integer;
   function GetIndex(Cmd:string):integer;
  function Methods(AClass:TClass) : TStringList;
  published
  //Only published functions and procedures can by found by MethodAddress
   procedure SomeFunction (param:string; var Result:integer);
   procedure SomeProcedure(param:string);
   procedure Mess(param:string);
   //---------------------
   property CommandList:TStrings read FCmdList; //be careful with these list's objects.
  end;

implementation


{ TActs }

function TActs.Methods(AClass:TClass): TStringList;
//------------------------------------------------------
    type
       tmethodnamerec = packed record
          name : pshortstring;
          addr : pointer;
       end;

       tmethodnametable = packed record
         count : dword;
         entries : packed array[0..0] of tmethodnamerec;
       end;

       pmethodnametable =  ^tmethodnametable;

var
 methodtable : pmethodnametable;
 i : dword;
 vmt : tclass;

begin
   Result:=TStringList.Create;
   vmt:=AClass;
   while assigned(vmt) do
     begin
        methodtable:=pmethodnametable((Pointer(vmt)+vmtMethodTable)^);
        if assigned(methodtable) then
          for i:=0 to methodtable^.count-1 do
            Result.AddObject(methodtable^.entries[i].name^,Tobject(AClass));
        vmt:=pclass(pointer(vmt)+vmtParent)^;
     end;
end;
//------------------------------------------------------


function TActs.GetList: TStrings;
begin
  Result:=Methods(Self.ClassType);
end;


constructor TActs.Create;
begin
  FCmdList:=GetList;
end;

destructor TActs.Destroy;
begin
  if Assigned(FCmdList) then FreeAndNil(FCmdList);
  inherited Destroy;
end;


function TActs.Execute(Cmd, param: string): integer;
var t:TMethod; ind:integer;
begin
    Result:=cf_Error;
    ind:={FCmdList.IndexOf(cmd);}GetIndex(Cmd);
    if ind=-1 then exit;
    if not Assigned(FCmdList.Objects[ind]) then exit;
    t.Data:=TClass(FCmdList.Objects[ind]).ClassInfo;
    t.Code:=TClass(FCmdList.Objects[ind]).MethodAddress(cmd);
    if Assigned(t.code) then
    begin
     Result:=cf_Null;
     TIntFunc(t)(param,Result);
    end;
end;


function TActs.GetIndex(Cmd: string): integer;
//------------------------------------------------------
    Function DoCompareText(const s1,s2 : string) : PtrInt;
      begin
        result:=CompareText(upcase(s1),upcase(s2));
      end;
    //---------------------
begin
  Result:=0;
  with FCmdList do
  begin
    While (Result<Count) and (DoCompareText(Strings[Result],Cmd)<>0) do Result:=Result+1;
    if Result=Count then Result:=-1;
  end;
end;


//------------------------------------------------------
//Published methods
//------------------------------------------------------

procedure TActs.SomeFunction(param: string; var Result: integer);
begin
  ShowMessage('SomeFunction: param='+param);
  result:=100;
end;

procedure TActs.SomeProcedure(param:string);
begin
  ShowMessage('SomeProcedure: param='+param);
end;

procedure TActs.Mess(param: string);
begin
  ShowMessage(param);
end;


{procedure TActs.FillIntCommands(const Slist:TStrings);
begin
  //Gets Unit Name
  //  Slist.Add(GetTypeData(PTypeInfo(Self.ClassInfo))^.UnitName+':'+

  //Gets Class.Name
  Slist.Add(PTypeInfo(Self.ClassInfo)^.Name);

  // Slist.Add('----------------------');

  //Add list of functions and procedures
  Slist.AddStrings(Methods(Self.ClassType));
end;}

end.

