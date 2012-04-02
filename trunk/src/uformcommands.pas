{
   Double Commander
   -------------------------------------------------------------------------
   Implements custom commands for a component

   Copyright (C) 2011-2012  Przemyslaw Nagay (cobines@gmail.com)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit uFormCommands;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StringHashList, ActnList, uTypes;

type
  TCommandFuncResult = (cfrSuccess, cfrDisabled, cfrNotFound);
  TCommandFunc = procedure(const Params: array of string) of object;
  TCommandCaptionType = (cctShort, cctLong);

  (*
    The commands are 'user' functions which can be assigned to toolbar
    button, hotkey, menu item, executed by scripts, etc.
    Only published functions and procedures can by found by MethodAddress.

    How to set up a form to handle hotkeys:
    1. Specify that the form class implements IFormCommands (class (TForm, IFormCommands)).
    2. Add private FCommands: TFormCommands that will implement the interface.
    3. Add property that will specify that FCommands implements the interface in place of the form.
       property Commands: TFormCommands read FCommands{$IF FPC_FULLVERSION >= 020501} implements IFormCommands{$ENDIF};
    4. Make sure a default constructor Create(TheOwner: TComponent) is present which
       will create the FCommands on demand when it is needed to read the hotkeys
       when the form is not currently created.
    5. For FPC < 2.5.1 "implements" does not work correctly so the form must
       implement the interface itself. For example see fViewer.
        {$IF FPC_FULLVERSION < 020501}
        // "implements" does not work in FPC < 2.5.1
        function ExecuteCommand(Command: string; Param: String=''): TCommandFuncResult;
        function GetCommandCaption(Command: String; CaptionType: TCommandCaptionType): String;
        procedure GetCommandsList(List: TStrings);
        {$ENDIF}
  *)

  { IFormCommands }

  {$interfaces corba}
  // If a form/object implements this interface then it can execute custom
  // commands with parameters.
  IFormCommands = interface
    ['{0464B1C0-BA98-4258-A286-F0F726FF66C4}']
    function ExecuteCommand(Command: String; const Params: array of string): TCommandFuncResult;
    function GetCommandCaption(Command: String; CaptionType: TCommandCaptionType = cctShort): String;
    procedure GetCommandsList(List: TStrings);
  end;
  {$interfaces default}

  // Used to filter out commands. If this function returns True the command
  // is not included in the commands list.
  TCommandFilterFunc = function (Command: String): Boolean of object;

  TCommandRec = record
    Address: Pointer;  //<en Address of the command function in the class.
    Action: TAction;   //<en If a TAction is assigned to a named action it is cached here.
  end;
  PCommandRec = ^TCommandRec;

  {en
     Stores association between method name and its address.

     StringHashList is used for this purpose, which may be faster
     than linear scanning by using MethodAddress on the given object.
  }

  { TFormCommands }

  TFormCommands = class(TComponent, IFormCommands)
  private
    FFilterFunc: TCommandFilterFunc;
    FInstanceObject: TObject;
    FMethods: TStringHashList;

    class procedure GetMethodsList(Instance: TObject; MethodsList: TStringHashList; ActionList: TActionList);

  public
    {en
       Creates methods list.
       @param(TheOwner
              Object of which we want the list of methods.
              It will also be the owner component.)
       @param(ActionList
              Optional. If contains actions corresponding to commands names
              (but prefixed with "act" instead of "cm_") then actions hints or
              captions will be used as descriptions for the commands.)
    }
    constructor Create(TheOwner: TComponent; ActionList: TActionList = nil); reintroduce;
    destructor Destroy; override;

    function ExecuteCommand(Command: string; const Params: array of string): TCommandFuncResult;

    {en
       Enables/disables command.
       @param(CommandName
              Name of the command. Include prefix if exists, like 'cm_'.)
       @param(Enable
              Whether to enable or disable the command.)
    }
    procedure EnableCommand(Command: String; Enable: Boolean);

    function GetCommandCaption(Command: String; CaptionType: TCommandCaptionType): String;
    function GetCommandName(Index: Integer): String;
    function GetCommandRec(Command: String): PCommandRec;
    procedure GetCommandsList(List: TStrings);

    class procedure GetCategoriesList(List: TStrings; Translated: TStrings);
    class function GetCommandsForm(CategoryName: String): TComponentClass;
    class procedure RegisterCommandsForm(AClass: TClass; CategoryName: String; TranslatedName: PResStringRec);

    property FilterFunc: TCommandFilterFunc read FFilterFunc write FFilterFunc;
  end;

  function GetDefaultParam(const Params: array of String): String;
  {en
     Searches for parameters starting with "Key=" and sets Value to the
     the rest of the parameter string (Key=Value).
     If the key is not found it sets Value to empty string and returns @false.
     @returns(@true if the key was found, @false if it was not found)
  }
  function GetParamValue(const Params: array of String; Key: String; out Value: String): Boolean;
  function GetParamValue(const Param: String; Key: String; out Value: String): Boolean;
  {en
     If StrValue matches any value that can be translated into boolean then
     it returns @true and sets Value appropriately. Otherwise returns @false.
  }
  function GetBoolValue(StrValue: string; out BoolValue: Boolean): Boolean;
  {en
     Replaces old value of Key or adds a new Key=NewValue string to the array.
  }
  procedure SetValue(var anArray: TDynamicStringArray; Key, NewValue: String);

implementation

uses
  uDCUtils;

type
  TCommandsFormRec = record
    AClass: TComponentClass;
    Name: String;
    TranslatedName: PResStringRec; // Until FPC 2.7.1 resource strings translation
                                   // is not applied after assining so pointer is used here.
                                   // It is OK because the address doesn't change after translation.
  end;

var
  CommandsForms: array of TCommandsFormRec;

constructor TFormCommands.Create(TheOwner: TComponent; ActionList: TActionList);
begin
  inherited Create(TheOwner);
  FInstanceObject := TheOwner;
  FMethods := TStringHashList.Create(False); // False = not case-sensitive
  GetMethodsList(FInstanceObject, FMethods, ActionList);
end;

destructor TFormCommands.Destroy;
var
  Index: Integer;
begin
  for Index := 0 to FMethods.Count - 1 do
    Dispose(PCommandRec(FMethods.List[Index]^.Data));
  FreeAndNil(FMethods);
  inherited;
end;

function TFormCommands.ExecuteCommand(Command: String; const Params: array of string): TCommandFuncResult;
var
  Method: TMethod;
  CommandRec: PCommandRec;
begin
  CommandRec := GetCommandRec(Command);
  if Assigned(CommandRec) then
  begin
    if Assigned(CommandRec^.Action) and not CommandRec^.Action.Enabled then
      Result := cfrDisabled
    else
    begin
      Method.Code := CommandRec^.Address;  // address of method
      Method.Data := FInstanceObject;      // pointer to instance
      TCommandFunc(Method)(Params);
      Result := cfrSuccess;
    end;
  end
  else
    Result := cfrNotFound;
end;

procedure TFormCommands.EnableCommand(Command: String; Enable: Boolean);
var
  CommandRec: PCommandRec;
begin
  CommandRec := GetCommandRec(Command);
  if Assigned(CommandRec) then
  begin
    if Assigned(CommandRec^.Action) then
      CommandRec^.Action.Enabled := Enable;
  end
  else
    raise Exception.Create('Invalid command name: ' + Command);
end;

function TFormCommands.GetCommandCaption(Command: String; CaptionType: TCommandCaptionType): String;
var
  CommandRec: PCommandRec;
begin
  CommandRec := GetCommandRec(Command);
  if Assigned(CommandRec) and Assigned(CommandRec^.Action) then
  begin
    if (CaptionType = cctLong) and (CommandRec^.Action.Hint <> EmptyStr) then
      Result := CommandRec^.Action.Hint
    else
      Result := StringReplace(CommandRec^.Action.Caption, '&', '', [rfReplaceAll]);
  end
  else
    Result:= '';
end;

function TFormCommands.GetCommandName(Index: Integer): String;
begin
  if (Index >= 0) and (Index < FMethods.Count) then
    Result := FMethods.List[Index]^.Key
  else
    raise ERangeError.Create('Invalid command index');
end;

function TFormCommands.GetCommandRec(Command: String): PCommandRec;
var
  Index: Integer;
begin
  Index := FMethods.Find(Command);
  if Index = -1 then
    Result := nil
  else
    Result := PCommandRec(FMethods.List[Index]^.Data);
end;

procedure TFormCommands.GetCommandsList(List: TStrings);
var
  Index: Integer;
  Command: String;
begin
  List.Clear;
  for Index := 0 to FMethods.Count - 1 do
  begin
    Command := FMethods.List[Index]^.Key;
    if not (Assigned(FilterFunc) and FilterFunc(Command)) then
      List.Add(Command);
  end;
end;

class procedure TFormCommands.GetMethodsList(Instance: TObject; MethodsList: TStringHashList; ActionList: TActionList);
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

  pmethodnametable = ^tmethodnametable;

var
  methodtable : pmethodnametable;
  i : dword;
  vmt : tclass;
  pentry: pmethodnamerec;
  CommandRec: PCommandRec;
  Command: String;
  Action: TContainedAction;
begin
  vmt := Instance.ClassType;
  while assigned(vmt) do
  begin
    methodtable := pmethodnametable((Pointer(vmt)+vmtMethodTable)^);
    if assigned(methodtable) then
    begin
      pentry := @methodtable^.entries;
      for i := 0 to methodtable^.count - 1 do
      begin
        Command := pentry[i].name^;
        if StrBegins(Command, 'cm_') then
        // TODO: Match functions parameter too.
        begin
          New(CommandRec);
          MethodsList.Add(Command, CommandRec);
          CommandRec^.Address := pentry[i].addr;
          CommandRec^.Action  := nil;
          if Assigned(ActionList) then
          begin
            Action := ActionList.ActionByName('act' + Copy(Command, 4, Length(Command) - 3));
            if Action is TAction then
              CommandRec^.Action := TAction(Action);
          end;
        end;
      end;
    end;
    vmt := pclass(pointer(vmt) + vmtParent)^;
  end;
end;

class procedure TFormCommands.GetCategoriesList(List: TStrings; Translated: TStrings);
var
  i: Integer;
begin
  List.Clear;
  Translated.Clear;
  for i := Low(CommandsForms) to High(CommandsForms) do
    begin
      List.Add(CommandsForms[i].Name);
      Translated.Add(CommandsForms[i].TranslatedName^);
    end;
end;

class function TFormCommands.GetCommandsForm(CategoryName: String): TComponentClass;
var
  i: Integer;
begin
  for i := Low(CommandsForms) to High(CommandsForms) do
    if CommandsForms[i].Name = CategoryName then
    begin
      Exit(CommandsForms[i].AClass);
    end;
  Result := nil;
end;

class procedure TFormCommands.RegisterCommandsForm(AClass: TClass; CategoryName: String; TranslatedName: PResStringRec);
begin
  SetLength(CommandsForms, Length(CommandsForms) + 1);
  CommandsForms[High(CommandsForms)].AClass := TComponentClass(AClass);
  CommandsForms[High(CommandsForms)].Name   := CategoryName;
  CommandsForms[High(CommandsForms)].TranslatedName := TranslatedName;
end;

function GetDefaultParam(const Params: array of String): String;
begin
  if Length(Params) > 0 then
    Result := Params[0]
  else
    Result := '';
end;

function GetParamValue(const Params: array of String; Key: String; out Value: String): Boolean;
var
  Param: String;
begin
  Key := Key + '=';
  for Param in Params do
    if StrBegins(Param, Key) then
    begin
      Value := Copy(Param, Length(Key) + 1, MaxInt);
      Exit(True);
    end;
  Value := '';
  Result := False;
end;

function GetParamValue(const Param: String; Key: String; out Value: String): Boolean;
begin
  Key := Key + '=';
  if StrBegins(Param, Key) then
  begin
    Value := Copy(Param, Length(Key) + 1, MaxInt);
    Exit(True);
  end;
  Value := '';
  Result := False;
end;

function GetBoolValue(StrValue: string; out BoolValue: Boolean): Boolean;
begin
  StrValue := upcase(StrValue);
  if (StrValue = 'TRUE') or
     (StrValue = 'YES') or
     (StrValue = 'ON') or
     (StrValue = '1') then
  begin
    BoolValue := True;
    Result := True;
  end
  else
  if (StrValue = 'FALSE') or
     (StrValue = 'NO') or
     (StrValue = 'OFF') or
     (StrValue = '0') then
  begin
    BoolValue := False;
    Result := True;
  end
  else
    Result := False;
end;

procedure SetValue(var anArray: TDynamicStringArray; Key, NewValue: String);
var
  i: Integer;
begin
  Key := Key + '=';
  for i := Low(anArray) to High(anArray) do
    if StrBegins(anArray[i], Key) then
    begin
      anArray[i] := Key + NewValue;
      Exit;
    end;
  AddString(anArray, Key + NewValue);
end;

end.

