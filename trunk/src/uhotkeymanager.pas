 {
    Double Commander
    -------------------------------------------------------------------------
    HotKey Manager.
    Allow to set it's own bindings to each TWinControl on form.

    Copyright (C) 2008  Dmitry Kolomiets (B4rr4cuda@rambler.ru)
    Copyright (C) 2011  Przemyslaw Nagay (cobines@gmail.com)

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
  uClassesEx, fgl, contnrs, uXmlConfig;

type
  generic THMObjectInstance<InstanceClass> = class
    Instance: InstanceClass;
    KeyDownProc: TKeyEvent;
  end;

  THMFormInstance = specialize THMObjectInstance<TCustomForm>;
  THMControlInstance = specialize THMObjectInstance<TWinControl>;

  THotkey = class
    Shortcut: String;
    Command: String;
    Params: String;
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

  THotkeyOperation = (hopAdd, hopRemove, hopClear);
  THotkeyEvent = procedure (hotkey: THotkey; operation: THotkeyOperation) of object;

  { THotkeys }

  THotkeys = class(TBaseHotkeysList)
  private
    FOnChange: THotkeyEvent;
    procedure DoOnChange(hotkey: THotkey; operation: THotkeyOperation);
  public
    constructor Create(AFreeObjects: Boolean = True); reintroduce;
    function Add(Shortcut: String; Command, Params: String): THotkey; overload;
    function AddIfNotExists(Shortcut: String; Command, Params: String): THotkey; overload;
    {en
       Adds multiple shortcuts to the same command.
       @param(ShortcutsWithParams
              Array of shortcuts with their respective parameters:
                [Shortcut1, Param1, Shortcut2, Param2, ...])
       @param(Command
              Command to which the shortcuts should be added.)
    }
    procedure AddIfNotExists(ShortcutsWithParams: array of String; Command: String);
    procedure Clear;
    procedure Delete(Shortcut: String); reintroduce;
    procedure Remove(var hotkey: THotkey); reintroduce;
    function Find(Shortcut: String): THotkey; overload;
    function FindByContents(Hotkey: THotkey): THotkey;
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
    procedure SetActionShortcut(hotkey: THotkey);
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
    FVersion: Integer;
    //---------------------
    procedure ClearAllHotkeys;
    //Hotkey Handler
    procedure KeyDownHandler(Sender: TObject; var Key: Word; Shift: TShiftState);
    //---------------------
    //This function is called from KeyDownHandler to find registered hotkey and execute assigned action
    function HotKeyEvent(Form: TCustomForm; Shortcut: String; Hotkeys: THotkeys): Boolean;
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
  uKeyboard, uGlobs, uDebug, uOSUtils, uDCVersion, XMLRead, uFormCommands;


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

function THotkeys.Add(Shortcut: String; Command, Params: String): THotkey;
begin
  Result          := THotkey.Create;
  Result.Shortcut := Shortcut;
  Result.Command  := Command;
  Result.Params   := Params;
  Add(Result);
  DoOnChange(Result, hopAdd);
end;

function THotkeys.AddIfNotExists(Shortcut: String; Command, Params: String): THotkey;
var
  i: Integer;
begin
  // Check if the shortcut isn't already assigned to a different command
  // or if a different shortcut isn't already assigned to the command.
  for i := 0 to Count - 1 do
  begin
    if (Items[i].Shortcut = Shortcut) or (Items[i].Command = Command) then
      Exit(nil);
  end;
  Result := Add(Shortcut, Command, Params);
end;

procedure THotkeys.AddIfNotExists(ShortcutsWithParams: array of String; Command: String);
var
  i, j: Integer;
  AddShortcut: array of Boolean;
  NrOfShortcuts: Integer;
begin
  Assert(Length(ShortcutsWithParams) mod 2 = 0);

  NrOfShortcuts := Length(ShortcutsWithParams) div 2;
  SetLength(AddShortcut, NrOfShortcuts);
  for j := 0 to NrOfShortcuts - 1 do
    AddShortcut[j] := True;

  // Check if the shortcut isn't already assigned to a different command
  // or if a different shortcut isn't already assigned to the command.
  for i := 0 to Count - 1 do
  begin
    for j := 0 to NrOfShortcuts - 1 do
      if Items[i].Shortcut = ShortcutsWithParams[j*2] then
        AddShortcut[j] := False;
    if Items[i].Command = Command then
      Exit;
  end;

  for j := 0 to NrOfShortcuts - 1 do
    if AddShortcut[j] then
      Add(ShortcutsWithParams[j*2], Command, ShortcutsWithParams[j*2 + 1]);
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

procedure THotkeys.Delete(Shortcut: String);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].ShortCut = Shortcut then
    begin
      DoOnChange(Items[i], hopRemove);
      inherited Delete(i);
      Exit;
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

function THotkeys.Find(Shortcut: String): THotkey;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].ShortCut = Shortcut then
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
    if (Result.ShortCut = Hotkey.Shortcut) and
       (Result.Command  = Hotkey.Command) and
       (Result.Params   = Hotkey.Params) then
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
      SetActionShortcut(hotkeys[i]);
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
      SetActionShortcut(hotkey);
    hopRemove:
      RemoveActionShortcut(hotkey, True);
    hopClear:
      RemoveActionShortcut(hotkey, False);
  end;
end;

procedure THMForm.RemoveActionShortcut(hotkey: THotkey; AssignNextShortcut: Boolean);
var
  action: TAction;
  i, j: Integer;
  shortcut, newShortcut: TShortCut;
begin
  shortcut := TextToShortCutEx(hotkey.Shortcut);
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
              newShortcut := TextToShortCutEx(hotkeys[j].Shortcut);
              Break;
            end;
        end;

        action.ShortCut := newShortcut;
      end;
    end;
  end;
end;

procedure THMForm.SetActionShortcut(hotkey: THotkey);
var
  action: TAction;
  i: Integer;
  shortcut: TShortCut;
begin
  shortcut := TextToShortCutEx(hotkey.Shortcut);
  for i := 0 to FActionLists.Count - 1 do
  begin
    action := GetActionByCommand(FActionLists[i], hotkey.Command);
    if Assigned(action) then
    begin
      // Don't override previous shortcut.
      if action.Shortcut = VK_UNKNOWN then
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

        Config.SetAttr(HotkeyNode, 'Key', Hotkeys[i].Shortcut);
        Config.SetValue(HotkeyNode, 'Command', Hotkeys[i].Command);
        if Hotkeys[i].Params <> EmptyStr then
          Config.SetValue(HotkeyNode, 'Params', Hotkeys[i].Params);

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

  procedure LoadHotkey(Hotkeys: THotkeys; Node: TXmlNode);
  var
    Shortcut, Command, Params: String;
    HMControl: THMControl;
    IsFormHotkey: Boolean = True;
  begin
    if (Config.TryGetAttr(Node, 'Key', Shortcut)) and
       (((FVersion <= 1) and Config.TryGetAttr(Node, 'Command', Command)) or
        Config.TryGetValue(Node, 'Command', Command)) and
       (Shortcut <> EmptyStr) and
       (Command <> EmptyStr) then
    begin
      if FVersion <= 1 then
        Params := Config.GetAttr(Node, 'Params', '')
      else
        Params := Config.GetValue(Node, 'Params', '');

      Shortcut := NormalizeModifiers(Shortcut);

      Node := Node.FirstChild;
      while Assigned(Node) do
      begin
        if Node.CompareName('Control') = 0 then
        begin
          IsFormHotkey := False;
          HMControl := Form.Controls.FindOrCreate(Config.GetContent(Node));
          HMControl.Hotkeys.Add(Shortcut, Command, Params);
        end;
        Node := Node.NextSibling;
      end;

      if IsFormHotkey then
      begin
        if (FVersion <= 3) and IsShortcutConflictingWithOS(Shortcut) then
        begin
          HMControl := Form.Controls.FindOrCreate('Files Panel');
          HMControl.Hotkeys.AddIfNotExists(Shortcut, Command, Params);
        end
        else
          Hotkeys.Add(Shortcut, Command, Params);
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
  Command, Params, FormName, ControlName: String;

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
    j := 0;
    while ini.ValueExists(section, 'Command' + IntToStr(j)) do
    begin
      begin
        Command     := ini.ReadString(section, 'Command' + IntToStr(j), '');
        Params      := ini.ReadString(section, 'Param' + IntToStr(j), '');
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

        hotkeys.Add(shortcut, Command, Params);
      end;

      j := j + 1;
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

function THotKeyManager.HotKeyEvent(Form: TCustomForm; Shortcut: String; Hotkeys: THotkeys): Boolean;
var
  hotkey: THotkey;
  FormCommands: IFormCommands;
begin
  hotkey := Hotkeys.Find(Shortcut);
  if Assigned(hotkey) then
  begin
    FormCommands := Form as IFormCommands;
    Result := Assigned(FormCommands) and
              (FormCommands.ExecuteCommand(hotkey.Command, hotkey.Params) = cfrSuccess);
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
  if not (((GetKeyTypingAction(ShiftEx) <> ktaNone)
{$IFDEF MSWINDOWS}
      // Don't execute hotkeys with Ctrl+Alt = AltGr on Windows.
      or ((ShiftEx * KeyModifiersShortcutNoText = [ssCtrl, ssAlt]) and
          (gKeyTyping[ktmNone] <> ktaNone))
      // Don't execute hotkeys with AltGr on Windows.
      or (ShiftEx = [ssAltGr])
{$ENDIF}
      ) and (Key in [VK_0..VK_9, VK_A..VK_Z])) then
    begin
      if Assigned(Control) then
      begin
        for i := 0 to HMForm.Controls.Count - 1 do
        begin
          HMControl := HMForm.Controls[i];
          HMControlInstance := HMControl.Find(Control);
          if Assigned(HMControlInstance) then
          begin
            if HotKeyEvent(Form, TextShortcut, HMControl.Hotkeys) then
            begin
              Key := VK_UNKNOWN;
              Exit;
            end
            else
              // Original OnKeyDown of the control
              OrigKeyDown(HMControlInstance.KeyDownProc);
          end;
        end;
      end;

      // Hotkey for the whole form
      if HotKeyEvent(Form, TextShortcut, HMForm.Hotkeys) then
      begin
        Key := VK_UNKNOWN;
        Exit;
      end;
    end;

  if Key <> VK_UNKNOWN then
  begin
    HMFormInstance := HMForm.Find(Form);
    OrigKeyDown(HMFormInstance.KeyDownProc);
  end;
end;

end.

