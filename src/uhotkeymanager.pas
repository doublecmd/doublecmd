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
  uClassesEx, fgl, contnrs, EpikTimer;

type
  generic THMObjectInstance<InstanceClass> = class
    Instance: InstanceClass;
    KeyDownProc: TKeyEvent;
  end;

  THMFormInstance = specialize THMObjectInstance<TCustomForm>;
  THMControlInstance = specialize THMObjectInstance<TWinControl>;

  THotkey = class
    Shortcut: TShortCut;
    Command: String;
    Params: String;
  end;

  TBaseHotkeysList = specialize TFPGObjectList<THotkey>;

  { THotkeys }

  THotkeys = class(TBaseHotkeysList)
  public
    function Add(Shortcut: TShortCut; Command, Params: String): THotkey; overload;
    function Add(sShortcut: String; Command, Params: String): THotkey; overload;
    function AddIfNotExists(sShortcut: String; Command, Params: String): THotkey; overload;
    procedure Delete(Shortcut: TShortCut); overload;
    procedure Delete(sShortcut: String); overload;
    function Find(Shortcut: TShortCut): THotkey; overload;
    function Find(sShortcut: String): THotkey; overload;
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

  { THMForm }

  THMForm = class(THMBaseForm)
    Controls: THMControls;
    constructor Create(AName: String); override;
    destructor Destroy; override;
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
    FVersion: String;
    //---------------------
    //Hotkey Handler
    procedure KeyDownHandler(Sender: TObject; var Key: Word; Shift: TShiftState);
    //---------------------
    //This function is called from KeyDownHandler to find registered hotkey and execute assigned action
    function HotKeyEvent(Shortcut: TShortCut; Hotkeys: THotkeys): Boolean;
    //---------------------
    function RegisterForm(AFormName: String): THMForm;
    function RegisterControl(AFormName: String; AControlName: String): THMControl;
    //---------------------
  public
    constructor Create;
    destructor Destroy; override;
    //---------------------
    procedure LoadShortCutToActionList(ActionList: TActionList; AFormName: String);
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
    property Version: String read FVersion;
  end;

//Helpers
//------------------------------------------------------
function KeyToShortCutEx(Key: Word; Shift: TShiftState): TShortCut;
function ShortCutToTextEx(ShortCut: TShortCut): String;
function TextToShortCutEx(const ShortCutText: String): TShortCut;
function KeyToText(Akey: Word): String;

implementation

uses
  uKeyboard, uGlobs, uOSUtils, uDebug, uActs;

const
  scWin = $1000;

{ THotKeyManager }

function TextToShortCutEx(const ShortCutText: String): TShortCut;

  function CompareFront(var StartPos: Integer; const Front: String): Boolean;
  begin
    if (Front <> '') and (StartPos + length(Front) - 1 <= length(ShortCutText)) and
      (AnsiStrLIComp(@ShortCutText[StartPos], PChar(Front), Length(Front)) = 0) then
    begin
      Result := True;
      Inc(StartPos, length(Front));
    end
    else
      Result := False;
  end;

var
  Key:      TShortCut;
  Shift:    TShortCut;
  StartPos: Integer;
  Name:     String;
begin
  Result   := 0;
  Shift    := 0;
  StartPos := 1;
  while True do
  begin
    if CompareFront(StartPos, MenuKeyCaps[mkcShift]) then
      Shift := Shift or scShift
    else if CompareFront(StartPos, '^') then
      Shift := Shift or scCtrl
    else if CompareFront(StartPos, MenuKeyCaps[mkcCtrl]) then
      Shift := Shift or scCtrl
    else if CompareFront(StartPos, MenuKeyCaps[mkcAlt]) then
      Shift := Shift or scAlt
    else if CompareFront(StartPos, MenuKeyCaps[mkcWin]) then
      Shift := Shift or scWin
    else
      Break;
  end;

  // Get text for the key if anything left in the string.
  if StartPos <= Length(ShortCutText) then
  begin
    { Copy range from table in ShortCutToText }
    for Key := $08 to $FF do
    begin
      Name := VirtualKeyToText(Key);
      if (Name <> '') and (length(Name) = length(ShortCutText) - StartPos + 1) and
        (AnsiStrLIComp(@ShortCutText[StartPos], PChar(Name), length(Name)) = 0) then
      begin
        Result := Key or Shift;
        Exit;
      end;
    end;
  end;
end;

function KeyToText(Akey: Word): String;
begin
  Result := ShortCutToTextEx(KeyToShortCutEx(AKey, GetKeyShiftStateEx));
end;

function ShortCutToTextEx(ShortCut: TShortCut): String;
var
  ShiftState: TShiftState = [];
begin
  if ShortCut and scShift <> 0 then
    Include(ShiftState, ssShift);
  if ShortCut and scCtrl <> 0 then
    Include(ShiftState, ssCtrl);
  if ShortCut and scAlt <> 0 then
    Include(ShiftState, ssAlt);
  if ShortCut and scWin <> 0 then
    Include(ShiftState, ssSuper);

  Result := VirtualKeyToText(Byte(ShortCut and $FF), ShiftState);
end;

function KeyToShortCutEx(Key: Word; Shift: TShiftState): TShortCut;
begin
  Result := Key;
  if ssShift in Shift then
    Inc(Result, scShift);
  if ssCtrl in Shift then
    Inc(Result, scCtrl);
  if ssAlt in Shift then
    Inc(Result, scAlt);
  if ssSuper in Shift then
    Inc(Result, scWin);
end;

{ THotkeys }

function THotkeys.Add(Shortcut: TShortCut; Command, Params: String): THotkey;
begin
  Result          := THotkey.Create;
  Result.Shortcut := Shortcut;
  Result.Command  := Command;
  Result.Params   := Params;
  Add(Result);
end;

function THotkeys.Add(sShortcut: String; Command, Params: String): THotkey;
begin
  Result := Add(TextToShortCutEx(sShortcut), Command, Params);
end;

function THotkeys.AddIfNotExists(sShortcut: String; Command, Params: String): THotkey;
var
  Shortcut: TShortCut;
begin
  Shortcut := TextToShortCutEx(sShortcut);
  Result := Find(Shortcut);
  if not Assigned(Result) then
    Result := Add(Shortcut, Command, Params);
end;

procedure THotkeys.Delete(Shortcut: TShortCut);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].ShortCut = Shortcut then
    begin
      Delete(i);
      Exit;
    end;
end;

procedure THotkeys.Delete(sShortcut: String);
var
  Shortcut: TShortCut;
begin
  Shortcut := TextToShortCutEx(sShortcut);
  Delete(Shortcut);
end;

function THotkeys.Find(Shortcut: TShortCut): THotkey;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].ShortCut = Shortcut then
      Exit(Items[i]);
  Result := nil;
end;

function THotkeys.Find(sShortcut: String): THotkey;
var
  Shortcut: TShortCut;
begin
  Shortcut := TextToShortCutEx(sShortcut);
  Result := Find(Shortcut);
end;

{ THMForm }

constructor THMForm.Create(AName: String);
begin
  inherited;
  Controls := THMControls.Create(True);
end;

destructor THMForm.Destroy;
begin
  inherited;
  Controls.Free;
end;

{ THMBaseObject }

constructor THMBaseObject.Create(AName: String);
begin
  FName    := AName;
  FHotkeys := THotkeys.Create(True);
  FObjects := TFPObjectList.Create(False);
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

procedure THotKeyManager.LoadShortCutToActionList(ActionList: TActionList; AFormName: String);
var
  form: THMForm;
  i: Integer;
  hotkey: THotkey;
  action: TContainedAction;
begin
  // Only set shortcuts of hotkeys for the form itself.
  form := FForms.Find(AFormName);
  if Assigned(form) then
  begin
    for i := 0 to form.Hotkeys.Count - 1 do
    begin
      hotkey := form.Hotkeys[i];
      action := ActionList.ActionByName(
        'act' + Copy(hotkey.Command, 4, Length(hotkey.Command) - 3));
      if action is TAction then
        (action as TAction).ShortCut := hotkey.Shortcut;
    end;
  end;
end;

procedure THotKeyManager.Save(FileName: String);
var
  i, j, k:  Integer;
  ini:      TIniFileEx;
  fst, cst: TStringList;
begin
  {if FForms.Count > 0 then
  begin
    if mbFileExists(FileName) then
      mbDeleteFile(FileName);
    ini := TIniFileEx.Create(FileName);
    ini.WriteString('Configuration', 'Version', hkVersion);
    for i := 0 to FForms.Count - 1 do
    begin
      fst := TStringList(FHotList.Objects[i]);
      if Assigned(fst) and (fst.Count > 0) then
      begin
        for j := 0 to fst.Count - 1 do
        begin
          cst := TStringList(fst.Objects[j]);
          if Assigned(cst) and (cst.Count > 0) then
          begin
            for k := 0 to cst.Count - 1 do
            begin
              TH := THotkeyInfoClass(cst.Objects[k]);
              ini.WriteString(FHotList[i], 'Command' + IntToStr(j), TH.ACommand);
              ini.WriteString(FHotList[i], 'Param' + IntToStr(j), TH.AParams);
              ini.WriteString(FHotList[i], 'Object' + IntToStr(j), Cst[k]);
              ini.WriteString(FHotList[i], 'Form' + IntToStr(j), fst[j]);
            end;
          end;
        end;
      end;
    end;
    ini.Free;
  end;}
end;

procedure THotKeyManager.Load(FileName: String);
var
  st:       TStringList;
  ini:      TIniFileEx;
  i, j:     Integer;
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
  FForms.Clear;

  st       := TStringList.Create;
  ini      := TIniFileEx.Create(FileName);
  FVersion := ini.ReadString('Configuration', 'Version', EmptyStr);
  ini.ReadSections(st);

  for i := 0 to st.Count - 1 do
  begin
    shortcut := st[i];
    j := 0;
    while ini.ValueExists(shortcut, 'Command' + IntToStr(j)) do
    begin
      begin
        Command     := ini.ReadString(shortcut, 'Command' + IntToStr(j), '');
        Params      := ini.ReadString(shortcut, 'Param' + IntToStr(j), '');
        ControlName := ini.ReadString(shortcut, 'Object' + IntToStr(j), '');
        FormName    := ini.ReadString(shortcut, 'Form' + IntToStr(j), '');

        RemoveFrmPrexif(FormName);
        RemoveFrmPrexif(ControlName);

        form := FForms.FindOrCreate(FormName);

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
  end;
end;

function THotKeyManager.HotKeyEvent(Shortcut: TShortCut; Hotkeys: THotkeys): Boolean;
var
  hotkey: THotkey;
begin
  hotkey := Hotkeys.Find(Shortcut);
  Result := Assigned(hotkey) and
            Actions.IsActionEnabled(Copy(hotkey.Command, 4, Length(hotkey.Command) - 3)) and
            (Actions.Execute(hotkey.Command, hotkey.Params) <> cf_Error);
end;

procedure THotKeyManager.KeyDownHandler(Sender: TObject; var Key: Word; Shift: TShiftState);
//------------------------------------------------------
var
  i:                 Integer;
  shortcut:          TShortCut;
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

  ShiftEx := GetKeyShiftStateEx;
  shortcut := KeyToShortCutEx(Key, ShiftEx);
  Control := Form.ActiveControl;

  // Don't execute hotkeys that coincide with quick search/filter combination
  if not ((Shift <> []) and ((Shift = gQuickSearchMode) or (Shift = gQuickFilterMode)) and
    (Key in [VK_0..VK_9, VK_A..VK_Z])) then
{$IFDEF MSWINDOWS}
    // Don't execute hotkeys with AltGr on Windows.
    if not (ShiftEx = [ssAltGr]) then
{$ENDIF}
    begin
      if Assigned(Control) then
      begin
        for i := 0 to HMForm.Controls.Count - 1 do
        begin
          HMControl := HMForm.Controls[i];
          HMControlInstance := HMControl.Find(Control);
          if Assigned(HMControlInstance) then
          begin
            if HotKeyEvent(shortcut, HMControl.Hotkeys) then
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
      if HotKeyEvent(shortcut, HMForm.Hotkeys) then
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

