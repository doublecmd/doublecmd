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
  Classes, SysUtils, StringHashList, ActnList, Menus;

type
  TCommandFuncResult = (cfrSuccess, cfrDisabled, cfrNotFound);
  TCommandFunc = procedure(const Params: array of string) of object;
  TCommandCaptionType = (cctShort, cctLong);
  TCommandCategorySortOrder = (ccsLegacy, ccsAlphabetical);
  TCommandSortOrder = (csLegacy, csAlphabetical);

  (*
    The commands are 'user' functions which can be assigned to toolbar
    button, hotkey, menu item, executed by scripts, etc.
    Only published functions and procedures can by found by MethodAddress.

    How to set up a form to handle hotkeys:
    1. Specify that the form class implements IFormCommands (class (TForm, IFormCommands)).
    2. Add private FCommands: TFormCommands that will implement the interface.
    3. Add property that will specify that FCommands implements the interface in place of the form.
       property Commands: TFormCommands read FCommands{$IF FPC_FULLVERSION >= 020501} implements IFormCommands{$ENDIF};
       For FPC < 2.5.1 "implements" does not work correctly so the form must
       implement the interface itself. For example see fViewer.
        {$IF FPC_FULLVERSION < 020501}
        // "implements" does not work in FPC < 2.5.1
        function ExecuteCommand(Command: string; Param: String=''): TCommandFuncResult;
        function GetCommandCaption(Command: String; CaptionType: TCommandCaptionType): String;
        procedure GetCommandsList(List: TStrings);
        {$ENDIF}
    4. Make sure a default constructor Create(TheOwner: TComponent) is present which
       will create the FCommands on demand when it is needed to read the hotkeys
       when the form is not currently created.
    5. Register the form and action list in HotkeyManager somewhere in constructor:
       const
         HotkeysCategory = <unique_name>;
         HMForm := HotMan.Register(Self, HotkeysCategory);
         HMForm.RegisterActionList(actionList);
       And unregister in destructor:
         HotMan.UnRegister(Self);
    6. Register form as commands form so that it is displayed in Options:
       initialization
         TFormCommands.RegisterCommandsForm(Tfrm..., HotkeysCategory, @rsHotkeyCategory...);


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
    procedure GetCommandCategoriesList(List: TStringList; CommandCategorySortOrder:TCommandCategorySortOrder);
    procedure GetCommandsListForACommandCategory(List: TStringList; sCategoryName:String; CommandSortOrder: TCommandSortOrder);
    procedure ExtractCommandFields(ItemInList: string; var sCategory:string; var sCommand: string; var sHint: string; var sHotKey: string; var FlagCategoryTitle: boolean);
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
    FTranslatableCommandCategory: TStringList;

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
    procedure GetCommandCategoriesList(List: TStringList; CommandCategorySortOrder:TCommandCategorySortOrder);
    procedure GetCommandsListForACommandCategory(List: TStringList; sCategoryName:String; CommandSortOrder: TCommandSortOrder);
    procedure ExtractCommandFields(ItemInList: string; var sCategory: string; var sCommand: string; var sHint: string; var sHotKey: string; var FlagCategoryTitle: boolean);

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
  function GetParamBoolValue(const Param: String; Key: String; out BoolValue: Boolean): Boolean;
  {en
     If StrValue matches any value that can be translated into boolean then
     it returns @true and sets Value appropriately. Otherwise returns @false.
  }
  function GetBoolValue(StrValue: string; out BoolValue: Boolean): Boolean;
  function CloneMainAction(AMainAction:TAction; ATargetActionList:TActionList; AMenuToInsert:TMenuItem=nil; APositionToInsert:integer=-1):TAction;


implementation

uses
  uGlobs, uHotkeyManager, DCStrUtils, uLng;

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
  FTranslatableCommandCategory:=TStringList.Create;
  ParseLineToList(rsCmdCategoryListInOrder, FTranslatableCommandCategory);
end;

destructor TFormCommands.Destroy;
var
  Index: Integer;
begin
  for Index := 0 to FMethods.Count - 1 do
    Dispose(PCommandRec(FMethods.List[Index]^.Data));
  FreeAndNil(FMethods);
  FTranslatableCommandCategory.Free;
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
  List.BeginUpdate;
  try
    for Index := 0 to FMethods.Count - 1 do
    begin
      Command := FMethods.List[Index]^.Key;
      if not (Assigned(FilterFunc) and FilterFunc(Command)) then
        List.Add(Command);
    end;
  finally
    List.EndUpdate;
  end;
end;

procedure TFormCommands.GetCommandCategoriesList(List: TStringList; CommandCategorySortOrder:TCommandCategorySortOrder);
var
  Index: Integer;
  Command, Category: String;
begin
  List.Clear;
  List.BeginUpdate;
  try
    for Index := 0 to FMethods.Count - 1 do
    begin
      Command := FMethods.List[Index]^.Key;
      if not (Assigned(FilterFunc) and FilterFunc(Command)) then
      begin
        if TCommandRec(FMethods.List[Index]^.Data^).Action.Tag <> 0 then
        begin
          case CommandCategorySortOrder of
            ccsLegacy: Category:=Format('%2.2d',[TCommandRec(FMethods.List[Index]^.Data^).Action.Tag])+FTranslatableCommandCategory.Strings[TCommandRec(FMethods.List[Index]^.Data^).Action.Tag];
            else Category:=FTranslatableCommandCategory.Strings[TCommandRec(FMethods.List[Index]^.Data^).Action.Tag];
          end;
          if List.IndexOf(Category)=-1 then
            List.Add(Category);
        end;
      end;
    end;

    List.Sort;
    if CommandCategorySortOrder=ccsLegacy then
      for Index:=0 to pred(List.count) do List.Strings[Index]:=RightStr(List.Strings[Index],length(List.Strings[Index])-2);
    List.Insert(0,'('+rsSimpleWordAll+')');
  finally
    List.EndUpdate;
  end;
end;

{ TFormCommands.GetCommandsListForACommandCategory }
{
  Routine is in fact going through all the commands present in the main form.
  They will store them into a list passed in parameter "List".
  Each item of the list will be a string with information separate between pipe "|" symbol.
  These info will be command name|shortcut|hint|category number.
  For example: cm_ChangeDirToHome|Ctrl+Alt+H|Change directory to home|14
  While building the list, if the wanted sort method is "csLegacy", each item will preceeded with category index on two digt and by command index on three digits.
  This is to help to sort the element. We'll simply sort calling "TStringList.Sort" since the beginning of the string have the legacy reference order.
  For example: 14106cm_ChangeDirToHome|Ctrl+Alt+H|Change directory to home|14
  At the end, when exiting, these 5 digits which help to sort will simply be removed.
  ALSO, the routine has the parameter "sCategoryName" to determine the command from which category should be in the list OR if all the commands from ALL the catagory must be returned.
  When the commands from ALL the category are requested, category header will be inserted in the returned list.
  These command will have the prefix for the command index set to '000' to make sure it appear at the beginning of the category command name.
  For these category identifier, the other fields are empty so that's why pipe are following with nothing between.
  Example: 14000Navigation||||
  No special "class" has been created for all this. It seem simple like that.
}
procedure TFormCommands.GetCommandsListForACommandCategory(List: TStringList; sCategoryName:String; CommandSortOrder: TCommandSortOrder);
var
  Index, iHotKey, iControl: Integer;
  Command, Category, sHotKey, LocalHint, HeaderSortedHelper, HeaderCategorySortedHelper: String;
  HMForm: THMForm;
  HMControl: THMControl;
  hotkey: THotkey;
begin
  List.Clear;
  List.BeginUpdate;
  try
    HeaderSortedHelper:='';
    HMForm := HotMan.Forms.Find('main');

    for Index := 0 to FMethods.Count - 1 do
    begin
      Command := FMethods.List[Index]^.Key;
      if not (Assigned(FilterFunc) and FilterFunc(Command)) then
      begin
        Category:=FTranslatableCommandCategory.Strings[TCommandRec(FMethods.List[Index]^.Data^).Action.Tag];

        if (Category = sCategoryName) OR (sCategoryName=('('+rsSimpleWordAll+')'))  then
        begin
          sHotKey := '';

          iHotKey := 0;
          while (iHotKey < HMForm.Hotkeys.Count) and (sHotKey = '') do
          begin
            hotkey := HMForm.Hotkeys[iHotKey];
            if hotkey.Command = Command then
              sHotKey := ShortcutsToText(hotkey.Shortcuts);
            Inc(iHotKey);
          end;

          if sHotKey='' then
          begin
            iControl:=0;
            while (iControl<HMForm.Controls.Count) and (sHotKey='') do
            begin
              HMControl := HMForm.Controls[iControl];
              iHotKey:=0;
              while (iHotKey < HMControl.Hotkeys.Count) and (sHotKey = '') do
              begin
                hotkey := HMControl.Hotkeys[iHotKey];
                if hotkey.Command = Command then
                  sHotKey := ShortcutsToText(hotkey.Shortcuts);
                Inc(iHotKey);
              end;
              inc(iControl);
            end;
          end;

          if CommandSortOrder=csLegacy then
          begin
            HeaderSortedHelper:=Format('%2.2d',[TCommandRec(FMethods.List[Index]^.Data^).Action.Tag])+
                                Format('%3.3d',[TCommandRec(FMethods.List[Index]^.Data^).Action.Index+1]);

            if sCategoryName=('('+rsSimpleWordAll+')') then
            begin
              HeaderCategorySortedHelper:=Format('%2.2d',[TCommandRec(FMethods.List[Index]^.Data^).Action.Tag])+'000';
              if List.IndexOf(HeaderCategorySortedHelper+Category+'||||')=-1 then
                List.Add(HeaderCategorySortedHelper+Category+'||||');
            end;
          end;

          if TCommandRec(FMethods.List[Index]^.Data^).Action.Hint <> EmptyStr then
            LocalHint := TCommandRec(FMethods.List[Index]^.Data^).Action.Hint
          else
            LocalHint := StringReplace(TCommandRec(FMethods.List[Index]^.Data^).Action.Caption, '&', '', [rfReplaceAll]);

          if LocalHint<>EmptyStr then Command:=Command+'|'+sHotKey+'|'+LocalHint+'|'+Format('%2.2d',[TCommandRec(FMethods.List[Index]^.Data^).Action.Tag]);

          List.Add(HeaderSortedHelper+Command);
        end;
      end;
    end;

    List.Sort;
    if CommandSortOrder=csLegacy then
      for Index:=0 to pred(List.count) do List.Strings[Index]:=RightStr(List.Strings[Index],length(List.Strings[Index])-(2+3));
  finally
    List.EndUpdate;
  end;
end;

procedure TFormCommands.ExtractCommandFields(ItemInList: string; var sCategory: string; var sCommand: string; var sHint: string; var sHotKey: string; var FlagCategoryTitle: boolean);
var
  PosPipe: longint;
  sWorkingString: String;
begin
  FlagCategoryTitle := False;
  sCommand := '';
  sHint := '';
  sHotKey := '';
  sCategory := '';
  PosPipe := Pos('|', ItemInList);
  if PosPipe <> 0 then
  begin
    if pos('||||', ItemInList) = 0 then
    begin
      sCommand := Copy(ItemInList, 1, pred(PosPipe));
      sWorkingString := RightStr(ItemInList, length(ItemInList) - PosPipe);
      PosPipe := pos('|', sWorkingString);
      if PosPipe <> 0 then
      begin
        sHotKey := copy(sWorkingString, 1, pred(PosPipe));
        sWorkingString := rightStr(sWorkingString, length(sWorkingString) - PosPipe);
        PosPipe := pos('|', sWorkingString);
        if PosPipe <> 0 then
        begin
          sHint := copy(sWorkingString, 1, pred(PosPipe));
          sCategory := rightStr(sWorkingString, length(sWorkingString) - PosPipe);
          sCategory := FTranslatableCommandCategory.Strings[StrToIntDef(sCategory,0)];
        end;
      end;
    end
    else
    begin
      sCommand := Copy(ItemInList, 1, pred(PosPipe));
      FlagCategoryTitle := True;
    end;
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
  vmt : PVmt;
  pentry: pmethodnamerec;
  CommandRec: PCommandRec;
  Command: String;
  Action: TContainedAction;
begin
  vmt := PVmt(Instance.ClassType);
  while assigned(vmt) do
  begin
    methodtable := pmethodnametable(vmt^.vMethodTable);
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
    vmt := vmt^.vParent;
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

function GetParamBoolValue(const Param: String; Key: String; out BoolValue: Boolean): Boolean;
var
  sValue: String;
begin
  Result := GetParamValue(Param, Key, sValue) and
            GetBoolValue(sValue, BoolValue);
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

{ CloneMainAction }
// Useful to implement an action in sub window form that will invoke a main action
function CloneMainAction(AMainAction:TAction; ATargetActionList:TActionList; AMenuToInsert:TMenuItem=nil; APositionToInsert:integer=-1):TAction;
var
  AMenuItem:TMenuItem;
begin
  result:= TAction.Create(ATargetActionList);
  result.Name := AMainAction.Name;
  result.Caption := AMainAction.Caption;
  result.Hint := AMainAction.Hint;
  result.Category := AMainAction.Category;
  result.GroupIndex := AMainAction.GroupIndex;
  result.ShortCut:= AMainAction.ShortCut;
  result.Enabled := AMainAction.Enabled;
  result.ActionList := ATargetActionList;
  result.OnExecute := AMainAction.OnExecute;

  if AMenuToInsert<>nil then
  begin
    AMenuItem:=TMenuItem.Create(AMenuToInsert);
    AMenuItem.Action:=result;
    if APositionToInsert=-1 then
      AMenuToInsert.Add(AMenuItem)
    else
      AMenuToInsert.Insert(APositionToInsert,AMenuItem);
  end;
end;

end.

