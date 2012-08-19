 {
    Double Commander
    -------------------------------------------------------------------------
    HotKey Manager.
    Allow to set it's own bindings to each TWinControl on form.

    Copyright (C) 2008      Dmitry Kolomiets (B4rr4cuda@rambler.ru)
    Copyright (C) 2011-2012 Przemyslaw Nagay (cobines@gmail.com)

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

unit uHotkeyManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, LCLProc, LCLType, LCLIntf, Forms, ActnList,
  DCClassesUtf8, fgl, contnrs, DCXmlConfig, DCBasicTypes;

type
  generic THMObjectInstance<InstanceClass> = class
    Instance: InstanceClass;
    KeyDownProc: TKeyEvent;
  end;

  THMFormInstance = specialize THMObjectInstance<TCustomForm>;
  THMControlInstance = specialize THMObjectInstance<TWinControl>;

  { THotkey }

  THotkey = class
    Shortcuts: array of String;
    Command: String;
    Params: array of String;
    procedure Assign(Hotkey: THotkey);
    function Clone: THotkey;
    function HasParam(const aParam: String): Boolean; overload;
    function HasParam(const aParams: array of String): Boolean; overload;
    function SameAs(Hotkey: THotkey): Boolean;
    function SameParams(const aParams: array of String): Boolean;
    function SameShortcuts(const aShortcuts: array of String): Boolean;
  end;

  TBaseHotkeysList = specialize TFPGObjectList<THotkey>;

  { TFreeNotifier }

  TFreeNotifier = class(TComponent)
  private
    FFreeEvent: TNotifyEvent;
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    property OnFree: TNotifyEvent read FFreeEvent write FFreeEvent;
  end;

  THotkeyOperation = (hopAdd, hopRemove, hopClear, hopUpdate);
  THotkeyEvent = procedure (hotkey: THotkey; operation: THotkeyOperation) of object;

  { THotkeys }

  THotkeys = class(TBaseHotkeysList)
  private
    FOnChange: THotkeyEvent;
    procedure DoOnChange(hotkey: THotkey; operation: THotkeyOperation);
  public
    constructor Create(AFreeObjects: Boolean = True); reintroduce;
    function Add(const Shortcuts, Params: array of String; Command: String): THotkey; overload;
    function AddIfNotExists(const Shortcuts, Params: array of String; Command: String): THotkey; overload;
    {en
       Adds multiple shortcuts to the same command.
       @param(ShortcutsWithParams
              Array of shortcuts followed by any number of parameters.
              Each shortcuts array must end with an empty string,
              and similarly each parameters must end with an empty string.
                [Shortcut1A, Shortcut1B, '', S1ParamA, '',
                 Shortcut2, '', S2ParamA, S2ParamB, '', ...])
       @param(Command
              Command to which the shortcuts should be added.)
       @param(OldShortcuts, OldParams
              Adds new shortcuts even if old shortcut exists.
              If a different shortcuts exists however then doesn't add new one.)
    }
    procedure AddIfNotExists(const ShortcutsWithParams: array of String; Command: String;
                             const OldShortcuts, OldParams: array of String); overload;
    procedure AddIfNotExists(const ShortcutsWithParams: array of String; Command: String); overload;
    procedure Clear; reintroduce;
    procedure Remove(var hotkey: THotkey); reintroduce;
    function Find(const Shortcuts: TDynamicStringArray): THotkey;
    {en
       Find hotkey which shortcuts begin with Shortcuts parameter.
       If BothWays=@true then also looks for shortcuts which are the beginning
       of Shortcuts parameter.
    }
    function FindByBeginning(const Shortcuts: TDynamicStringArray; BothWays: Boolean): THotkey;
    function FindByCommand(Command: String): THotkey;
    function FindByContents(Hotkey: THotkey): THotkey;
    {en
       Should be called whenever a hotkey has shortcut updated to update the
       shortcuts in ActionLists.
    }
    procedure UpdateHotkey(Hotkey: THotkey);
    property OnChange: THotkeyEvent read FOnChange write FOnChange;
  end;

  { THMBaseObject }

  generic THMBaseObject<InstanceClass, InstanceInfoClass> = class
  private
    FObjects: TFPObjectList;
    FHotkeys: THotkeys;
    FName: String;
  public
    constructor Create(AName: String); virtual;
    destructor Destroy; override;
    function Add(AInstanceInfo: InstanceInfoClass): Integer;
    procedure Delete(AInstance: InstanceClass);
    function Find(AInstance: InstanceClass): InstanceInfoClass;
    property Hotkeys: THotkeys read FHotkeys;
    property Name: String read FName;
  end;

  THMControl = specialize THMBaseObject<TWinControl, THMControlInstance>;
  THMBaseControls = specialize TFPGObjectList<THMControl>;

  { THMControls }

  THMControls = class(THMBaseControls)
    procedure Delete(AName: String); overload;
    function Find(AName: String): THMControl;
    function Find(AControl: TWinControl): THMControl;
    function FindOrCreate(AName: String): THMControl;
  end;

  THMBaseForm = specialize THMBaseObject<TCustomForm, THMFormInstance>;
  TActionLists = specialize TFPGObjectList<TActionList>;

  { THMForm }

  THMForm = class(THMBaseForm)
  private
    {en
       Used for notifying when an ActionList is destroyed.
    }
    FFreeNotifier: TFreeNotifier;
    FActionLists: TActionLists;
    function GetActionByCommand(ActionList: TActionList; Command: String): TAction;
    procedure OnActionListFree(Sender: TObject);
    procedure OnHotkeyEvent(hotkey: THotkey; operation: THotkeyOperation);
    procedure RemoveActionShortcut(hotkey: THotkey; AssignNextShortcut: Boolean);
    procedure SetActionShortcut(hotkey: THotkey; OverridePrevious: Boolean);
  public
    Controls: THMControls;
    constructor Create(AName: String); override;
    destructor Destroy; override;
    procedure RegisterActionList(ActionList: TActionList);
    procedure UnregisterActionList(ActionList: TActionList);
  end;
  TBaseForms = specialize TFPGObjectList<THMForm>;

  { THMForms }

  THMForms = class(TBaseForms)
    procedure Delete(AName: String); overload;
    function Find(AName: String): THMForm;
    function Find(AForm: TCustomForm): THMForm;
    function FindOrCreate(AName: String): THMForm;
  end;

  { THotKeyManager }

  THotKeyManager = class
  private
    FForms: THMForms;
    FLastShortcutTime: Double; // When last shortcut was received (used for sequences of shortcuts)
    FSequenceStep: Integer;    // Which hotkey we are waiting for (from 0)
    FShortcutsSequence: TDynamicStringArray; // Sequence of shortcuts that has been processed since last key event
    FVersion: Integer;
    //---------------------
    procedure ClearAllHotkeys;
    //Hotkey Handler
    procedure KeyDownHandler(Sender: TObject; var Key: Word; Shift: TShiftState);
    //---------------------
    //This function is called from KeyDownHandler to find registered hotkey and execute assigned action
    function HotKeyEvent(Form: TCustomForm; Hotkeys: THotkeys): Boolean;
    //---------------------
    function RegisterForm(AFormName: String): THMForm;
    function RegisterControl(AFormName: String; AControlName: String): THMControl;
    //---------------------
    procedure Save(Config: TXmlConfig; Root: TXmlNode);
    procedure Load(Config: TXmlConfig; Root: TXmlNode);
    procedure LoadIni(FileName: String);
    //---------------------
    function IsShortcutConflictingWithOS(Shortcut: String): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    //---------------------
    procedure Save(FileName: String);
    procedure Load(FileName: String);
    //---------------------
    function Register(AForm: TCustomForm; AFormName: String): THMForm;
    function Register(AControl: TWinControl; AControlName: String): THMControl;
    procedure UnRegister(AForm: TCustomForm);
    procedure UnRegister(AControl: TWinControl);
    //---------------------
    property Forms: THMForms read FForms;
    property Version: Integer read FVersion;
  end;

implementation

uses
  XMLRead, uKeyboard, uGlobs, uDebug, uDCVersion, uFormCommands,
  DCOSUtils, DCStrUtils;

const
  MaxShortcutSequenceInterval = 1000; // in ms

{ THotkey }

procedure THotkey.Assign(Hotkey: THotkey);
begin
  Shortcuts := Copy(Hotkey.Shortcuts);
  Params    := Copy(Hotkey.Params);
  Command   := Hotkey.Command;
end;

function THotkey.Clone: THotkey;
begin
  Result := THotkey.Create;
  Result.Assign(Self);
end;

function THotkey.HasParam(const aParams: array of String): Boolean;
begin
  Result := ContainsOneOf(Params, aParams);
end;

function THotkey.HasParam(const aParam: String): Boolean;
begin
  Result := Contains(Params, aParam);
end;

function THotkey.SameAs(Hotkey: THotkey): Boolean;
begin
  Result := (Command = Hotkey.Command) and
            (SameShortcuts(Hotkey.Shortcuts)) and
            (SameParams(Hotkey.Params));
end;

function THotkey.SameParams(const aParams: array of String): Boolean;
begin
  Result := Compare(Params, aParams);
end;

function THotkey.SameShortcuts(const aShortcuts: array of String): Boolean;
begin
  Result := Compare(Shortcuts, aShortcuts);
end;

{ TFreeNotifier }

procedure TFreeNotifier.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and Assigned(FFreeEvent) then
    FFreeEvent(AComponent);
  inherited Notification(AComponent, Operation);
end;

{ THotkeys }

constructor THotkeys.Create(AFreeObjects: Boolean);
begin
  FOnChange := nil;
  inherited Create(AFreeObjects);
end;

function THotkeys.Add(const Shortcuts, Params: array of String; Command: String): THotkey;
begin
  if (Command <> EmptyStr) and (Length(Shortcuts) > 0) then
  begin
    Result           := THotkey.Create;
    Result.Shortcuts := CopyArray(Shortcuts);
    Result.Params    := CopyArray(Params);
    Result.Command   := Command;
    Add(Result);
    DoOnChange(Result, hopAdd);
  end
  else
    Result := nil;
end;

function THotkeys.AddIfNotExists(const Shortcuts, Params: array of String; Command: String): THotkey;
var
  i: Integer;
begin
  // Check if the shortcuts aren't already assigned to a different command
  // or if a different shortcut isn't already assigned to the command.
  // Also check if the shortucts aren't a partial match to another shortcuts.
  for i := 0 to Count - 1 do
  begin
    if ArrBegins(Items[i].Shortcuts, Shortcuts, True) or (Items[i].Command = Command) then
      Exit(nil);
  end;
  Result := Add(Shortcuts, Params, Command);
end;

procedure THotkeys.AddIfNotExists(const ShortcutsWithParams: array of String; Command: String);
begin
  AddIfNotExists(ShortcutsWithParams, Command, [], []);
end;

procedure THotkeys.AddIfNotExists(const ShortcutsWithParams: array of String; Command: String;
                                  const OldShortcuts, OldParams: array of String);
var
  s: String;
  StartIndex: Integer;

  function GetArray: TDynamicStringArray;
  var
    i: Integer;
  begin
    Result := nil;
    for i := StartIndex to High(ShortcutsWithParams) do
    begin
      s := ShortcutsWithParams[i];
      if s <> '' then
        AddString(Result, s)
      else
        Break;
    end;
    StartIndex := i + 1;
  end;
  function CheckIfOldOrEmpty: Boolean;
  var
    i: Integer;
  begin
    for i := 0 to Count - 1 do
      if Items[i].Command = Command then
      begin
        if not (Items[i].SameShortcuts(OldShortcuts) and
                Items[i].SameParams(OldParams)) then
          Exit(False);
      end;
    Result := True;
  end;
var
  Shortcuts, Params: array of String;
begin
  // Check if a different shortcut isn't already assigned to the command.
  // If there is only the old shortcut then allow adding new one.
  if not CheckIfOldOrEmpty then
    Exit;

  StartIndex := Low(ShortcutsWithParams);
  while True do
  begin
    Shortcuts := GetArray;
    Params := GetArray;

    if Length(Shortcuts) > 0 then
    begin
      // Check if the shortcuts aren't already assigned to a different command.
      if not Assigned(FindByBeginning(Shortcuts, True)) then
        Add(Shortcuts, Params, Command);
    end
    else
     Break;
  end;
end;

procedure THotkeys.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    DoOnChange(Items[0], hopClear);
    inherited Delete(0);
  end;
end;

procedure THotkeys.Remove(var hotkey: THotkey);
begin
  if Assigned(hotkey) then
  begin
    DoOnChange(hotkey, hopRemove);
    inherited Remove(hotkey);
    if FreeObjects then
      hotkey := nil;
  end;
end;

procedure THotkeys.UpdateHotkey(Hotkey: THotkey);
begin
  DoOnChange(Hotkey, hopUpdate);
end;

function THotkeys.Find(const Shortcuts: TDynamicStringArray): THotkey;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].SameShortcuts(Shortcuts) then
      Exit(Items[i]);
  Result := nil;
end;

function THotkeys.FindByBeginning(const Shortcuts: TDynamicStringArray; BothWays: Boolean): THotkey;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if ArrBegins(Items[i].Shortcuts, Shortcuts, BothWays) then
      Exit(Items[i]);
  Result := nil;
end;

function THotkeys.FindByCommand(Command: String): THotkey;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].Command = Command then
      Exit(Items[i]);
  Result := nil;
end;

function THotkeys.FindByContents(Hotkey: THotkey): THotkey;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := Items[i];
    if Result.SameAs(Hotkey) then
      Exit;
  end;
  Result := nil;
end;

procedure THotkeys.DoOnChange(hotkey: THotkey; operation: THotkeyOperation);
begin
  if Assigned(FOnChange) then
    FOnChange(hotkey, operation);
end;

{ THMForm }

constructor THMForm.Create(AName: String);
begin
  FFreeNotifier := nil;
  inherited;
  Controls     := THMControls.Create(True);
  FActionLists := TActionLists.Create(False);
end;

destructor THMForm.Destroy;
begin
  inherited;
  Controls.Free;
  FActionLists.Free;
  FFreeNotifier.Free;
end;

procedure THMForm.RegisterActionList(ActionList: TActionList);
var
  i: Integer;
begin
  if FActionLists.IndexOf(ActionList) < 0 then
  begin
    FActionLists.Add(ActionList);

    Hotkeys.OnChange := @OnHotkeyEvent;

    if not Assigned(FFreeNotifier) then
    begin
      FFreeNotifier := TFreeNotifier.Create(nil);
      FFreeNotifier.OnFree := @OnActionListFree;
    end;
    ActionList.FreeNotification(FFreeNotifier);

    // Initialize actionlist with shortcuts.
    for i := 0 to hotkeys.Count - 1 do
      SetActionShortcut(hotkeys[i], False);
  end;
end;

procedure THMForm.UnregisterActionList(ActionList: TActionList);
begin
  if FActionLists.Remove(ActionList) >= 0 then
    ActionList.RemoveFreeNotification(FFreeNotifier);
end;

function THMForm.GetActionByCommand(ActionList: TActionList; Command: String): TAction;
var
  action: TContainedAction;
begin
  action := ActionList.ActionByName('act' + Copy(Command, 4, Length(Command) - 3));
  if action is TAction then
    Result := action as TAction
  else
    Result := nil;
end;

procedure THMForm.OnActionListFree(Sender: TObject);
begin
  if Sender is TActionList then
    UnregisterActionList(Sender as TActionList);
end;

procedure THMForm.OnHotkeyEvent(hotkey: THotkey; operation: THotkeyOperation);
begin
  case operation of
    hopAdd:
      SetActionShortcut(hotkey, False);
    hopRemove:
      RemoveActionShortcut(hotkey, True);
    hopClear:
      RemoveActionShortcut(hotkey, False);
    hopUpdate:
      SetActionShortcut(hotkey, True);
  end;
end;

procedure THMForm.RemoveActionShortcut(hotkey: THotkey; AssignNextShortcut: Boolean);
var
  action: TAction;
  i, j: Integer;
  shortcut, newShortcut: TShortCut;
begin
  shortcut := TextToShortCutEx(hotkey.Shortcuts[0]);
  for i := 0 to FActionLists.Count - 1 do
  begin
    action := GetActionByCommand(FActionLists[i], hotkey.Command);
    if Assigned(action) then
    begin
      if action.Shortcut = shortcut then
      begin
        newShortcut := VK_UNKNOWN;

        if AssignNextShortcut then
        begin
          // Search for another possible hotkey assigned for the same command.
          for j := 0 to hotkeys.Count - 1 do
            if (hotkeys[j].Command = hotkey.Command) and (hotkeys[j] <> hotkey) then
            begin
              newShortcut := TextToShortCutEx(hotkeys[j].Shortcuts[0]);
              Break;
            end;
        end;

        action.ShortCut := newShortcut;
      end;
    end;
  end;
end;

procedure THMForm.SetActionShortcut(hotkey: THotkey; OverridePrevious: Boolean);
var
  action: TAction;
  i: Integer;
  shortcut: TShortCut;
begin
  shortcut := TextToShortCutEx(hotkey.Shortcuts[0]);
  for i := 0 to FActionLists.Count - 1 do
  begin
    action := GetActionByCommand(FActionLists[i], hotkey.Command);
    if Assigned(action) then
    begin
      if OverridePrevious or (action.Shortcut = VK_UNKNOWN) then
        action.ShortCut := shortcut;
    end;
  end;
end;

{ THMBaseObject }

constructor THMBaseObject.Create(AName: String);
begin
  FName    := AName;
  FHotkeys := THotkeys.Create(True);
  FObjects := TFPObjectList.Create(True);
end;

destructor THMBaseObject.Destroy;
begin
  inherited Destroy;
  FHotkeys.Free;
  FObjects.Free;
end;

function THMBaseObject.Add(AInstanceInfo: InstanceInfoClass): Integer;
begin
  Result := FObjects.Add(AInstanceInfo);
end;

procedure THMBaseObject.Delete(AInstance: InstanceClass);
var
  i: Integer;
begin
  for i := 0 to FObjects.Count - 1 do
    if InstanceInfoClass(FObjects[i]).Instance = AInstance then
    begin
      FObjects.Delete(i);
      Exit;
    end;
end;

function THMBaseObject.Find(AInstance: InstanceClass): InstanceInfoClass;
var
  i: Integer;
begin
  for i := 0 to FObjects.Count - 1 do
  begin
    if InstanceInfoClass(FObjects[i]).Instance = AInstance then
      Exit(InstanceInfoClass(FObjects[i]));
  end;
  Result := nil;
end;

{ THMControls }

procedure THMControls.Delete(AName: String);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if SameText(Items[i].Name, AName) then
    begin
      Delete(i);
      Exit;
    end;
end;

function THMControls.Find(AName: String): THMControl;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if SameText(Items[i].Name, AName) then
      Exit(Items[i]);
  Result := nil;
end;

function THMControls.Find(AControl: TWinControl): THMControl;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    if Assigned(Items[i].Find(AControl)) then
      Exit(Items[i]);
  end;
  Result := nil;
end;

function THMControls.FindOrCreate(AName: String): THMControl;
begin
  Result := Find(AName);
  if not Assigned(Result) then
  begin
    Result := THMControl.Create(AName);
    Add(Result);
  end;
end;

{ THMForms }

procedure THMForms.Delete(AName: String);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if SameText(Items[i].Name, AName) then
    begin
      Delete(i);
      Exit;
    end;
end;

function THMForms.Find(AName: String): THMForm;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    if SameText(Items[i].Name, AName) then
      Exit(Items[i]);
  end;
  Result := nil;
end;

function THMForms.Find(AForm: TCustomForm): THMForm;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    if Assigned(Items[i].Find(AForm)) then
      Exit(Items[i]);
  end;
  Result := nil;
end;

function THMForms.FindOrCreate(AName: String): THMForm;
begin
  Result := Find(AName);
  if not Assigned(Result) then
  begin
    Result := THMForm.Create(AName);
    Add(Result);
  end;
end;

{ THotKeyManager }

constructor THotKeyManager.Create;
begin
  FForms := THMForms.Create(True);
  FSequenceStep := 0;
end;

destructor THotKeyManager.Destroy;
begin
  inherited Destroy;
  FForms.Free;
end;

procedure THotKeyManager.Save(FileName: String);
var
  Config: TXmlConfig = nil;
begin
  try
    Config := TXmlConfig.Create(FileName);
    Config.SetAttr(Config.RootNode, 'DCVersion', dcVersion);
    Save(Config, Config.RootNode);
    Config.Save;
  finally
    Config.Free;
  end;
end;

procedure THotKeyManager.Load(FileName: String);
var
  Config: TXmlConfig = nil;
  NotAnXML: Boolean = False;
begin
  try
    try
      Config := TXmlConfig.Create(FileName);
      Load(Config, Config.RootNode);
    except
      on EXMLReadError do
        NotAnXML := True;
    end;
  finally
    Config.Free;
  end;

  if NotAnXML then
  begin
    LoadIni(FileName);
    // Immediately save as xml so that configuration isn't lost.
    if mbRenameFile(FileName, FileName + '.ini.obsolete') then
      Save(FileName);
  end;
end;

procedure THotKeyManager.Save(Config: TXmlConfig; Root: TXmlNode);

var
  SavedHotkeys: THotkeys;

  procedure SaveHotkeys(Form: THMForm; Hotkeys: THotkeys; ControlIndex: Integer; Node: TXmlNode);
  var
    i, j: Integer;
    HotkeyNode, ControlNode: TXmlNode;
    Control: THMControl;

    procedure AddControl(AName: String);
    begin
      ControlNode := Config.AddNode(HotkeyNode, 'Control');
      Config.SetContent(ControlNode, AName);
    end;

  begin
    for i := 0 to Hotkeys.Count - 1 do
    begin
      // Save Form's hotkeys and hotkeys which have not been saved yet.
      if (ControlIndex < 0) or (not Assigned(SavedHotkeys.FindByContents(Hotkeys[i]))) then
      begin
        HotkeyNode := Config.AddNode(Node, 'Hotkey');

        for j := Low(Hotkeys[i].Shortcuts) to High(Hotkeys[i].Shortcuts) do
          Config.AddValue(HotkeyNode, 'Shortcut', Hotkeys[i].Shortcuts[j]);
        Config.AddValue(HotkeyNode, 'Command', Hotkeys[i].Command);
        for j := Low(Hotkeys[i].Params) to High(Hotkeys[i].Params) do
          Config.AddValue(HotkeyNode, 'Param', Hotkeys[i].Params[j]);

        if ControlIndex >= 0 then
          AddControl(Form.Controls[ControlIndex].Name);

        // Search all successive controls for the same hotkey.
        for j := Succ(ControlIndex) to Form.Controls.Count - 1 do
        begin
          Control := Form.Controls[j];
          if Assigned(Control.Hotkeys.FindByContents(Hotkeys[i])) then
            AddControl(Control.Name);
        end;

        SavedHotkeys.Add(Hotkeys[i]);
      end;
    end;
  end;

var
  i, j: Integer;
  FormNode: TXmlNode;
  Form: THMForm;
begin
  Root := Config.FindNode(Root, 'Hotkeys', True);
  Config.ClearNode(Root);
  Config.SetAttr(Root, 'Version', hkVersion);

  SavedHotkeys := THotkeys.Create(False);
  try
    for i := 0 to FForms.Count - 1 do
    begin
      Form := FForms[i];
      FormNode := Config.AddNode(Root, 'Form');
      Config.SetAttr(FormNode, 'Name', Form.Name);
      SaveHotkeys(Form, Form.Hotkeys, -1, FormNode);

      for j := 0 to Form.Controls.Count - 1 do
        SaveHotkeys(Form, Form.Controls[j].Hotkeys, j, FormNode);
    end;
  finally
    SavedHotkeys.Free;
  end;
end;

procedure THotKeyManager.Load(Config: TXmlConfig; Root: TXmlNode);
var
  Form: THMForm;

  procedure AddIfNotEmpty(var Arr: TDynamicStringArray; const Value: String);
  begin
    if Value <> '' then
      AddString(Arr, Value);
  end;
  procedure LoadHotkey(Hotkeys: THotkeys; Node: TXmlNode);
  var
    Shortcut, Command, Param: String;
    Shortcuts: array of String = nil;
    Params: array of String = nil;
    Controls: array of String = nil;
    HMControl: THMControl;
    i: Integer;
  begin
    // These checks for version may be removed after 0.5.5 release because
    // the XML format for hotkeys has only been added in development version 0.5.5.
    // Only Command needs to be retrieved here.
    if FVersion <= 1 then
      Command := Config.GetAttr(Node, 'Command', '')
    else
      Command := Config.GetValue(Node, 'Command', ''); // Leave only this
    if FVersion <= 1 then
      Param := Config.GetAttr(Node, 'Params', '')
    else if FVersion < 9 then
      Param := Config.GetValue(Node, 'Params', '');
    if FVersion < 10 then
    begin
      Shortcut := Config.GetAttr(Node, 'Key', '');
      if Shortcut <> '' then
      begin
        Shortcut := NormalizeModifiers(Shortcut);
        AddIfNotEmpty(Shortcuts, Shortcut);
      end;
    end;
    if (FVersion < 9) then
      AddIfNotEmpty(Params, Param);
    // Up to here may be deleted after 0.5.5 release.

    Node := Node.FirstChild;
    while Assigned(Node) do
    begin
      if Node.CompareName('Shortcut') = 0 then
        AddIfNotEmpty(Shortcuts, NormalizeModifiers(Config.GetContent(Node)))
      else if Node.CompareName('Control') = 0 then
        AddIfNotEmpty(Controls, Config.GetContent(Node))
      else if Node.CompareName('Param') = 0 then
        AddIfNotEmpty(Params, Config.GetContent(Node));
      Node := Node.NextSibling;
    end;

    if Length(Shortcuts) > 0 then
    begin
      if Length(Controls) = 0 then
      begin
        // This "if" block may also be deleted after 0.5.5 release.
        if (FVersion <= 3) and IsShortcutConflictingWithOS(Shortcuts[0]) then
        begin
          HMControl := Form.Controls.FindOrCreate('Files Panel');
          HMControl.Hotkeys.AddIfNotExists(Shortcuts, Params, Command);
        end
        else
          Hotkeys.Add(Shortcuts, Params, Command); // Leave only this
      end
      else
      begin
        for i := Low(Controls) to High(Controls) do
        begin
          HMControl := Form.Controls.FindOrCreate(Controls[i]);
          HMControl.Hotkeys.Add(Shortcuts, Params, Command);
        end;
      end;
    end;
  end;

var
  FormNode, HotkeyNode: TXmlNode;
  AName: String;
begin
  ClearAllHotkeys;

  Root := Config.FindNode(Root, 'Hotkeys');
  if Assigned(Root) then
  begin
    FVersion := Config.GetAttr(Root, 'Version', hkVersion);
    FormNode := Root.FirstChild;
    while Assigned(FormNode) do
    begin
      if (FormNode.CompareName('Form') = 0) and
         (Config.TryGetAttr(FormNode, 'Name', AName)) and
         (AName <> EmptyStr) then
      begin
        Form := FForms.FindOrCreate(AName);

        HotkeyNode := FormNode.FirstChild;
        while Assigned(HotkeyNode) do
        begin
          if HotkeyNode.CompareName('Hotkey') = 0 then
            LoadHotkey(Form.Hotkeys, HotkeyNode);
          HotkeyNode := HotkeyNode.NextSibling;
        end;
      end;

      FormNode := FormNode.NextSibling;
    end;
  end;
end;

procedure THotKeyManager.LoadIni(FileName: String);
var
  st:       TStringList;
  ini:      TIniFileEx;
  i, j:     Integer;
  section:  String;
  shortCut: String;
  hotkeys:  THotkeys;
  form:     THMForm;
  control:  THMControl;
  Command, Param, FormName, ControlName: String;
  Params: array of String = nil;

  procedure RemoveFrmPrexif(var s: String);
  begin
    if SameText(Copy(s, 1, 3), 'Frm') then
      Delete(s, 1, 3);
  end;
begin
  ClearAllHotkeys;

  st       := TStringList.Create;
  ini      := TIniFileEx.Create(FileName);
  ini.ReadSections(st);

  for i := 0 to st.Count - 1 do
  begin
    section  := st[i];
    shortCut := NormalizeModifiers(section);
    if shortCut <> '' then
    begin
      j := 0;
      while ini.ValueExists(section, 'Command' + IntToStr(j)) do
      begin
        Command     := ini.ReadString(section, 'Command' + IntToStr(j), '');
        Param       := ini.ReadString(section, 'Param' + IntToStr(j), '');
        ControlName := ini.ReadString(section, 'Object' + IntToStr(j), '');
        FormName    := ini.ReadString(section, 'Form' + IntToStr(j), '');

        RemoveFrmPrexif(FormName);
        RemoveFrmPrexif(ControlName);

        form := FForms.FindOrCreate(FormName);

        if IsShortcutConflictingWithOS(shortCut) then
          ControlName := 'Files Panel';

        // Old config had FormName=ControlName for main form.
        if SameText(FormName, ControlName) then
        begin
          hotkeys := form.Hotkeys;
        end
        else
        begin
          control := form.Controls.FindOrCreate(ControlName);
          hotkeys := control.Hotkeys;
        end;

        if Param <> '' then
        begin
          SetLength(Params, 1);
          Params[0] := Param;
        end
        else
          Params := nil;

        hotkeys.Add([shortcut], Params, Command);

        j := j + 1;
      end;
    end;
  end;

  FreeAndNil(st);
  FreeAndNil(ini);
end;

function THotKeyManager.IsShortcutConflictingWithOS(Shortcut: String): Boolean;
const
  ConflictingShortcuts: array [0..27] of String =
    (SmkcBkSp,                           // Delete previous character
     SmkcDel,                            // Delete next character
     SmkcLeft,                           // Move cursor left
     SmkcRight,                          // Move cursor right
     SmkcSpace,                          // Space
     SmkcWin,                            // Context menu
     SmkcShift + 'F10',                  // Context menu
     SmkcShift + SmkcDel,                // Cut text
     SmkcShift + SmkcIns,                // Paste text
     SmkcShift + SmkcHome,               // Select to beginning
     SmkcShift + SmkcEnd,                // Select to end
     SmkcShift + SmkcLeft,               // Select previous character
     SmkcShift + SmkcRight,              // Select next character
     SmkcCtrl + 'A',                     // Select all
     SmkcCtrl + 'C',                     // Copy text
     SmkcCtrl + 'V',                     // Paste text
     SmkcCtrl + 'X',                     // Cut text
     SmkcCtrl + 'Z',                     // Undo
     SmkcCtrl + SmkcBkSp,                // Delete previous word
     SmkcCtrl + SmkcDel,                 // Delete next word
     SmkcCtrl + SmkcIns,                 // Copy text
     SmkcCtrl + SmkcHome,                // Move to beginning
     SmkcCtrl + SmkcEnd,                 // Move to end
     SmkcCtrl + SmkcLeft,                // Move to beginning of word
     SmkcCtrl + SmkcRight,               // Move to end of word
     SmkcCtrl + SmkcShift + 'Z',         // Redo
     SmkcCtrl + SmkcShift + SmkcLeft,    // Select to beginning of word
     SmkcCtrl + SmkcShift + SmkcRight);  // Select to end of word
var
  i: Integer;
begin
  for i := Low(ConflictingShortcuts) to High(ConflictingShortcuts) do
    if Shortcut = ConflictingShortcuts[i] then
      Exit(True);
  Result := False;
end;

function THotKeyManager.Register(AForm: TCustomForm; AFormName: String): THMForm;
var
  formInstance: THMFormInstance;
begin
  Result := RegisterForm(AFormName);
  formInstance := Result.Find(AForm);
  if not Assigned(formInstance) then
  begin
    formInstance             := THMFormInstance.Create;
    formInstance.Instance    := AForm;
    formInstance.KeyDownProc := AForm.OnKeyDown;
    Result.Add(formInstance);

    AForm.OnKeyDown := @KeyDownHandler;
    AForm.KeyPreview := True;
  end;
end;

function THotKeyManager.Register(AControl: TWinControl; AControlName: String): THMControl;
var
  ParentForm: TCustomForm;
  form: THMForm;
  controlInstance: THMControlInstance;
begin
  ParentForm := GetParentForm(AControl);
  if Assigned(ParentForm) then
  begin
    form := FForms.Find(ParentForm);
    if not Assigned(form) then
    begin
      DCDebug('HotMan: Failed registering ' + AControlName + ': Form ' +
        ParentForm.ClassName + ':' + ParentForm.Name + ' not registered.');
      Exit(nil);
    end;
    Result := form.Controls.Find(AControlName);
    if not Assigned(Result) then
    begin
      Result := THMControl.Create(AControlName);
      form.Controls.Add(Result);
    end;

    controlInstance := Result.Find(AControl);
    if not Assigned(controlInstance) then
    begin
      controlInstance             := THMControlInstance.Create;
      controlInstance.Instance    := AControl;
      controlInstance.KeyDownProc := AControl.OnKeyDown;
      Result.Add(controlInstance);

      //AControl.OnKeyDown := @KeyDownHandler;
    end;
  end;
end;

function THotKeyManager.RegisterForm(AFormName: String): THMForm;
begin
  Result := FForms.Find(AFormName);
  if not Assigned(Result) then
  begin
    Result := THMForm.Create(AFormName);
    FForms.Add(Result);
  end;
end;

function THotKeyManager.RegisterControl(AFormName: String; AControlName: String): THMControl;
var
  form: THMForm;
begin
  form := RegisterForm(AFormName);
  Result := form.Controls.Find(AControlName);
  if not Assigned(Result) then
  begin
    Result := THMControl.Create(AControlName);
    form.Controls.Add(Result);
  end;
end;

procedure THotKeyManager.UnRegister(AForm: TCustomForm);
var
  form: THMForm;
  formInstance: THMFormInstance;
begin
  form := FForms.Find(AForm);
  if Assigned(form) then
  begin
    formInstance := form.Find(AForm);
    AForm.OnKeyDown := formInstance.KeyDownProc;
    form.Delete(AForm);
  end;
end;

procedure THotKeyManager.UnRegister(AControl: TWinControl);
var
  ParentForm: TCustomForm;
  form: THMForm;
  control: THMControl;
  i: Integer;
begin
  ParentForm := GetParentForm(AControl);
  if Assigned(ParentForm) then
  begin
    form := FForms.Find(ParentForm);
    if Assigned(form) then
    begin
      control := form.Controls.Find(AControl);
      if Assigned(control) then
        control.Delete(AControl);
    end;
  end
  else
  begin
    // control lost its parent, find through all forms
    for i := 0 to FForms.Count - 1 do
    begin
      form := FForms[i];

      control := form.Controls.Find(AControl);
      if Assigned(control) then
        control.Delete(AControl);
    end;
  end;
end;

function THotKeyManager.HotKeyEvent(Form: TCustomForm; Hotkeys: THotkeys): Boolean;
var
  hotkey: THotkey;
  FormCommands: IFormCommands;
begin
  hotkey := Hotkeys.FindByBeginning(FShortcutsSequence, False);
  if Assigned(hotkey) then
  begin
    if High(hotkey.Shortcuts) > FSequenceStep then
    begin
      // There are more shortcuts to match.
      FLastShortcutTime := SysUtils.Now;
      Inc(FSequenceStep);
      Result := True;
    end
    else
    begin
      FSequenceStep := 0;
      FormCommands := Form as IFormCommands;
      Result := Assigned(FormCommands) and
                (FormCommands.ExecuteCommand(hotkey.Command, hotkey.Params) = cfrSuccess);
    end;
  end
  else
    Result := False;
end;

procedure THotKeyManager.ClearAllHotkeys;
var
  i, j: Integer;
  Form: THMForm;
begin
  for i := 0 to FForms.Count - 1 do
  begin
    Form := FForms[i];
    Form.Hotkeys.Clear;
    for j := 0 to Form.Controls.Count - 1 do
      Form.Controls[j].Hotkeys.Clear;
  end;
end;

procedure THotKeyManager.KeyDownHandler(Sender: TObject; var Key: Word; Shift: TShiftState);
//------------------------------------------------------
var
  i:                 Integer;
  Shortcut:          TShortCut;
  TextShortcut:      String;
  Form:              TCustomForm;
  Control:           TWinControl;
  HMForm:            THMForm;
  HMControl:         THMControl;
  HMFormInstance:    THMFormInstance;
  HMControlInstance: THMControlInstance;
  ShiftEx:           TShiftState;

  function OrigKeyDown(AKeyDownProc: TKeyEvent): Boolean;
  begin
    if Assigned(AKeyDownProc) then
    begin
      AKeyDownProc(Sender, Key, ShiftEx);
      Result := True;
    end
    else
      Result := False;
  end;

begin
  Form := GetParentForm(Sender as TWinControl);
  HMForm := FForms.Find(Form);
  if not Assigned(HMForm) then
    Exit;

  ShiftEx      := GetKeyShiftStateEx;
  Shortcut     := KeyToShortCutEx(Key, ShiftEx);
  TextShortcut := ShortCutToTextEx(Shortcut);
  Control      := Form.ActiveControl;

  // Don't execute hotkeys that coincide with key typing actions.
  if (TextShortcut <> '') and
     ((FSequenceStep > 0) or
     (not (((GetKeyTypingAction(ShiftEx) <> ktaNone)
{$IFDEF MSWINDOWS}
      // Don't execute hotkeys with Ctrl+Alt = AltGr on Windows.
      or (HasKeyboardAltGrKey and
          (ShiftEx * KeyModifiersShortcutNoText = [ssCtrl, ssAlt]) and
          (gKeyTyping[ktmNone] <> ktaNone))
      // Don't execute hotkeys with AltGr on Windows.
      or (ShiftEx = [ssAltGr])
{$ENDIF}
      ) and (Key in [VK_0..VK_9, VK_A..VK_Z])))) then
    begin
      // If too much time has passed reset sequence.
      if (FSequenceStep > 0) and (DateTimeToTimeStamp(SysUtils.Now - FLastShortcutTime).Time > MaxShortcutSequenceInterval) then
        FSequenceStep := 0;

      // Add shortcut to sequence.
      if Length(FShortcutsSequence) <> FSequenceStep + 1 then
        SetLength(FShortcutsSequence, FSequenceStep + 1);
      FShortcutsSequence[FSequenceStep] := TextShortcut;

      if Assigned(Control) then
      begin
        for i := 0 to HMForm.Controls.Count - 1 do
        begin
          HMControl := HMForm.Controls[i];
          HMControlInstance := HMControl.Find(Control);
          if Assigned(HMControlInstance) then
          begin
            if HotKeyEvent(Form, HMControl.Hotkeys) then
            begin
              Key := VK_UNKNOWN;
              Exit;
            end
            else
              Break;
          end;
        end;
      end;

      // Hotkey for the whole form
      if (Key <> VK_UNKNOWN) and HotKeyEvent(Form, HMForm.Hotkeys) then
      begin
        Key := VK_UNKNOWN;
        Exit;
      end;

      FSequenceStep := 0; // Hotkey was not matched - reset sequence.
    end;

  if Key <> VK_UNKNOWN then
  begin
    HMFormInstance := HMForm.Find(Form);
    OrigKeyDown(HMFormInstance.KeyDownProc);
  end;
end;

end.

