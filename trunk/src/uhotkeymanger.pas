 {
    Double Commander
    -------------------------------------------------------------------------
    HotKey Manager.
    Allow to set it's own bindings to each TWinControl on form.

    Copyright (C) 2008  Dmitry Kolomiets (B4rr4cuda@rambler.ru)

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

unit uhotkeymanger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,Controls,LCLProc,LCLType,LCLIntf,Forms,ActnList,uActs,uClassesEx;

const
  scWin=$1000;

type
   { TObjInfoClass }
   //Object information for forms and controls
   TObjInfoClass=class
      AKeyDownProc :TKeyEvent;
      AObject      :TWinControl;
      AChilds:TStringList; //list of form's registered controls. For controls this is nil.
      destructor Destroy; override;
   end;

   //TODO: Всю тягомотину Hot->Forms->Controls->HotInfo выгести в отдельные классы
   
   THotkeyInfoClass=class
//     AShortCut:TShortCut;
     ACommand,
     AParams,
     AObjectName,
     AObjectFormName :string;
   end;
   

  { THotKeyManager }

   THotKeyManager= class
   private
    //Main listы
    FHotList:TStringList;
    FFormsList:TStringList;
    //---------------------
    //Hotkey Handler
    procedure KeyDownHandler(Sender: TObject; var Key: Word; Shift: TShiftState);
    //---------------------
    //This function is called from KeyDownHandler to find registered hotkey and execute assigned action
    function HotKeyEvent(sShortcut:string; ObjInfo:TObjInfoClass):boolean;
   //---------------------
    //Form registration
    procedure RegisterManagerForF(AObject:TCustomForm);
    //TWinControl Registration
    procedure RegisterManagerForW(AObject: TWinControl);
    //Unregistration procs
    procedure UnRegisterManagerForF(AObject:TCustomForm);
    procedure UnRegisterManagerForW(AObject: TWinControl);
   //---------------------
   public
     constructor Create;
     destructor Destroy; override;
     //---------------------
     procedure ClearHotKeys;
     //---------------------
     function AddHotKey(AHotKey,ACommand,AParams:string; AObject:TWinControl):integer;
     function AddHotKey(AHotKey,ACommand,AParams,AObjectName,AObjectFormName:string):integer;
     function DeleteHotKey(AHotKey,AObjectName,AObjectFormName:string):boolean;
     function DeleteHotKey(AHotKey:string; AObject:TWinControl):boolean;
     function ReplaceHotkey(AOldHotkey,ANewHotKey:string):integer;
     //---------------------
     //Index of hotkey in FHotList
     function GetHotKeyIndex(Hotkey:string; FromI:integer=0):integer;
     function GetFormsListBy(Hotkey: string; List: TStringList): integer;
     function GetControlsListBy(Hotkey: string; List: TStringList): integer;
     function GetCommandsListBy(Hotkey: string; List: TStringList): integer;
     //---------------------
     procedure LoadShortCutToActionList(ActionList: TActionList);
     //---------------------
     procedure Save(FileName:string);
     procedure Load(FileName:string);
     //---------------------
     procedure RegisterHotkeyManager(AObject:TWinControl);
     procedure UnRegisterHotkeyManager(AObject: TWinControl);
     //---------------------
     property HotkeyList:TStringList read FHotList;
   end;

//Helpers
//------------------------------------------------------
 function ShortCutEx(Key: Word; Shift: TShiftState): TShortCut;
 function ShortCutToTextEx(ShortCut: TShortCut): string;
 function TextToShortCutEx(const ShortCutText: string): TShortCut;
 function KeyToText(Akey:Word):string;
 
 
implementation

uses
  uKeyboard, uGlobs;

{ THotKeyManager }

function TextToShortCutEx(const ShortCutText: string): TShortCut;

  function CompareFront(var StartPos: integer; const Front: string): Boolean;
  begin
    if (Front<>'') and (StartPos+length(Front)-1<=length(ShortCutText))
    and (AnsiStrLIComp(@ShortCutText[StartPos], PChar(Front), Length(Front))= 0)
    then begin
      Result:=true;
      inc(StartPos,length(Front));
    end else
      Result:=false;
  end;

var
  Key: TShortCut;
  Shift: TShortCut;
  StartPos: integer;
  Name: string;
begin
  Result := 0;
  Shift := 0;
  StartPos:=1;
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
  if Copy(ShortCutText, StartPos, Length(ShortCutText) - StartPos + 1) <> '' then
  begin
    for Key := $08 to $FF do begin { Copy range from table in ShortCutToText }
      Name:=VirtualKeyToText(Key);
      if (Name<>'') and (length(Name)=length(ShortCutText)-StartPos+1)
      and (AnsiStrLIComp(@ShortCutText[StartPos], PChar(Name), length(Name)) = 0)
      then begin
        Result := Key or Shift;
        Exit;
      end;
    end;
  end;
end;

function KeyToText(Akey: Word): string;
begin
  result:=ShortCutToTextEx(ShortCutEx(AKey,GetKeyShiftStateEx))
end;


function ShortCutToTextEx(ShortCut: TShortCut): string;
var
  ShiftState: TShiftState = [];
begin
  if ShortCut and scShift <> 0 then Include(ShiftState, ssShift);
  if ShortCut and scCtrl  <> 0 then Include(ShiftState, ssCtrl);
  if ShortCut and scAlt   <> 0 then Include(ShiftState, ssAlt);
  if ShortCut and scWin   <> 0 then Include(ShiftState, ssSuper);

  Result := VirtualKeyToText(Byte(ShortCut and $FF), ShiftState);
end;

function ShortCutEx(Key: Word; Shift: TShiftState): TShortCut;
begin
  Result := Key;
  if ssShift in Shift then Inc(Result,scShift);
  if ssCtrl in Shift then Inc(Result,scCtrl);
  if ssAlt in Shift then Inc(Result,scAlt);
  if ssSuper in Shift then Inc(Result,scWin);
end;

constructor THotKeyManager.Create;
begin
  FHotList:=TStringList.Create;
  FFormsList:=TStringList.Create;
end;

destructor THotKeyManager.Destroy;
begin
 if assigned(FHotList) then
   begin
     Self.ClearHotKeyS;
     FreeAndNil(FHotList);
   end;
 //---------------------

 if assigned(FFormsList) then
 begin
   while FFormsList.Count>0 do
     begin
       if Assigned(FFormsList.Objects[0]) then
         begin
           TObjInfoClass(FFormsList.Objects[0]).Free;
           FFormsList.Delete(0);
         end;
     end;
   FreeAndNil(FFormsList);
 end;

 //---------------------

 inherited Destroy;
end;

procedure THotKeyManager.ClearHotKeys;
begin
 if not assigned(FHotList) then exit;
 //hotkeys
 while FHotList.Count>0 do
   begin
     if Assigned(FHotList.Objects[0]) then
       begin
        //forms
        while TStringList(FHotList.Objects[0]).Count>0 do
          begin
           if Assigned(TStringList(FHotList.Objects[0]).Objects[0]) then
             begin
               //Controls
               while TStringList(TStringList(FHotList.Objects[0]).Objects[0]).Count>0 do
                 begin
                   if Assigned(TStringList(TStringList(FHotList.Objects[0]).Objects[0]).Objects[0]) then
                      TStringList(TStringList(FHotList.Objects[0]).Objects[0]).Objects[0].Free;
                   TStringList(TStringList(FHotList.Objects[0]).Objects[0]).Delete(0);
                 end;
               TStringList(TStringList(FHotList.Objects[0]).Objects[0]).Free;
             end;
           TStringList(FHotList.Objects[0]).Delete(0);
          end;
        FHotList.Objects[0].Free;
       end;
     FHotList.Delete(0);
   end;

end;

function THotKeyManager.AddHotKey(AHotKey,ACommand,AParams:string; AObject: TWinControl): integer;
var
  par:TWinControl;
  TH:THotkeyInfoClass;
  i,j,k:integer;
  st:TStringList;
  shortCut: string;
begin
  shortCut := ShortCutToTextEx(TextToShortCutEx(AHotKey));
  // Omit invalid shortcuts.
  if shortCut <> '' then
  begin
   //Find control's parent form
   par:=AObject;
   while assigned(Par) and (not (par is TCustomForm)) do
    Par:=Par.Parent;

   if par is TCustomForm then
     begin
      i:=FHotList.IndexOf(shortCut);
      if i=-1 then
        i:=FHotList.AddObject(shortCut,TStringList.Create);
      result:=i;
      st:=TStringList(FHotList.Objects[i]); //form list
      //---------------------
      //find form and add it in form list
      j:=st.IndexOf(par.Name);
      if j=-1 then
        j:=st.AddObject(par.Name,TStringList.Create);

      st:=TStringList(st.Objects[j]); //controls list
      //---------------------
      k:=st.AddObject(AObject.Name,THotkeyInfoClass.Create);
      TH:=THotkeyInfoClass(st.Objects[k]);
      Th.ACommand:=ACommand;
      TH.AParams:=AParams;
      TH.AObjectName:=AObject.Name;
      TH.AObjectFormName:=Par.Name;
     end;
  end;
end;

function THotKeyManager.AddHotKey(AHotKey, ACommand, AParams, AObjectName,
  AObjectFormName: string): integer;
var
  tmp,k:integer;
  TH:THotkeyInfoClass;
  shortCut: string;
begin
  shortCut := ShortCutToTextEx(TextToShortCutEx(AHotKey));
  // Omit invalid shortcuts.
  if shortCut <> '' then
  begin
    Th:=THotkeyInfoClass.Create;
    th.ACommand:=ACommand;
    th.AParams:=AParams;
    th.AObjectName:=AObjectName;
    th.AObjectFormName:=AObjectFormName;

    tmp:=FHotList.IndexOf(shortCut);
    if tmp=-1 then
    tmp:=FHotList.AddObject(shortCut,TStringList.Create);

    //find form and add it in form list
    k:=TStringList(FHotList.Objects[tmp]).IndexOf(th.AObjectFormName);
    if k=-1 then
      k:=TStringList(FHotList.Objects[tmp]).AddObject(th.AObjectFormName,TStringList.Create);

    TStringList(TStringList(FHotList.Objects[tmp]).Objects[k]).AddObject(th.AObjectName,th);
    result:=tmp;
  end
  else
    Result := -1;
end;

function THotKeyManager.DeleteHotKey(AHotKey, AObjectName,
  AObjectFormName: string): boolean;
var i,j,k:integer;
begin
  result:=false;
  i:=GetHotKeyIndex(ShortCutToTextEx(TextToShortCutEx(AHotKey)));
  if i=-1 then exit;
  j:=TStringList(FHotList.Objects[i]).IndexOf(AObjectFormName);
  if j=-1 then exit;
  k:=TStringList(TStringList(FHotList.Objects[i]).Objects[j]).IndexOf(AObjectName);
  if k=-1 then exit;
  TStringList(TStringList(FHotList.Objects[i]).Objects[j]).Objects[k].Free;
  TStringList(TStringList(FHotList.Objects[i]).Objects[j]).Delete(k);
  result:=true;
  //TODO: по идее необходимы проверки, типа последний ли это элемент, но особого влияния их отсутствие\наличие не создаст.
end;

function THotKeyManager.DeleteHotKey(AHotKey: string; AObject: TWinControl
  ): boolean;
  var par:TWinControl; i,j,k:integer;
begin
  Result:=false;
  par:=AObject;
   while assigned(Par) and (not (par is TCustomForm)) do
    Par:=Par.Parent;

   if par is TCustomForm then
     begin
      i:=GetHotKeyIndex(ShortCutToTextEx(TextToShortCutEx(AHotKey)));
      if i=-1 then exit;
      j:=TStringList(FHotList.Objects[i]).IndexOf(par.Name);
      if j=-1 then exit;
      k:=TStringList(TStringList(FHotList.Objects[i]).Objects[j]).IndexOf(AObject.Name);
      if k=-1 then exit;
      TStringList(TStringList(FHotList.Objects[i]).Objects[j]).Objects[k].Free;
      TStringList(TStringList(FHotList.Objects[i]).Objects[j]).Delete(k);
      Result:=true;
     end;
end;

function THotKeyManager.ReplaceHotkey(AOldHotkey, ANewHotKey: string): integer;
begin
  Result:=GetHotKeyIndex(ShortCutToTextEx(TextToShortCutEx(AOldHotkey)));
  FHotList.Strings[Result]:=ShortCutToTextEx(TextToShortCutEx(ANewHotKey));
end;

procedure THotKeyManager.LoadShortCutToActionList(ActionList: TActionList);
var
  I: Integer;
  slActionList: TStringList;
  sCmd: String;
begin
  slActionList:= TStringList.Create;
  for I:= 0 to HotkeyList.Count - 1 do
    begin
      if GetCommandsListBy(HotkeyList[I], slActionList) > 0 then
        begin
          sCmd:= slActionList.ValueFromIndex[0]; // get first action
          sCmd:= 'act' + Copy(sCmd, 4, Length(sCmd) - 3);
          if Assigned(ActionList.ActionByName(sCmd)) then
            (ActionList.ActionByName(sCmd) as TAction).ShortCut:= TextToShortCutEx(HotkeyList[I]);
        end;
    end;
  FreeAndNil(slActionList);
end;

procedure THotKeyManager.Save(FileName: string);
var i,j,k:integer; ini:TIniFileEx; fst,cst:TStringList; TH:THotkeyInfoClass;
begin
  if FHotList.Count>0 then
  begin
    if FileExists(FileName) then DeleteFile(FileName);
    ini:=TIniFileEx.Create(FileName);
    for i:=0 to FHotList.Count-1 do
       begin
         fst:=TStringList(FHotList.Objects[i]);
         if Assigned(fst) and (fst.Count>0) then
           begin
             For j:=0 to fst.Count-1 do
               begin
                 cst:=TStringList(fst.Objects[j]);
                 if Assigned(cst) and (cst.Count>0) then
                   begin
                    for k:=0 to cst.Count-1 do
                      begin
                        TH:=THotkeyInfoClass(cst.Objects[k]);
                        ini.WriteString(FHotList[i],'Command'+IntToStr(k),TH.ACommand);
                        ini.WriteString(FHotList[i],'Param'+IntToStr(k),TH.AParams);
                        ini.WriteString(FHotList[i],'Object'+IntToStr(k),Cst[k]);
                        ini.WriteString(FHotList[i],'Form'+IntToStr(k),fst[j]);
                      end;
                   end;
               end;
           end;
       end;
    ini.Free;
  end;
end;

procedure THotKeyManager.Load(FileName: string);
var st:TStringList;
    ini:TIniFileEx;
    i,j,k,tmp:integer;
    sec, shortCut:string;
    Th:THotkeyInfoClass;
begin
  //первый элемент листа контролов - сама форма
  Self.ClearHotKeyS;
  st:=TStringList.Create;
  ini:=TIniFileEx.Create(FileName);
  ini.ReadSections(st);

  for i:=0 to st.Count-1 do
    begin
      sec:=st[i];
      j:=0;
      while ini.ValueExists(sec,'Command'+inttostr(j)) do
        begin
          shortCut := ShortCutToTextEx(TextToShortCutEx(sec));
          // Omit invalid shortcuts.
          if shortCut <> '' then
          begin
            Th:=THotkeyInfoClass.Create;
            if Assigned(th) then
             begin
              th.ACommand:=ini.ReadString(sec,'Command'+inttostr(j),'');
              th.AParams:=ini.ReadString(sec,'Param'+inttostr(j),'');
              th.AObjectName:=ini.ReadString(sec,'Object'+inttostr(j),'');
              th.AObjectFormName:=ini.ReadString(sec,'Form'+inttostr(j),'');

              tmp:=FHotList.IndexOf(shortCut);
              if tmp=-1 then
                tmp:=FHotList.AddObject(shortCut,TStringList.Create);
              k:=TStringList(FHotList.Objects[tmp]).IndexOf(th.AObjectFormName);
              if k=-1 then
                k:=TStringList(FHotList.Objects[tmp]).AddObject(th.AObjectFormName,TStringList.Create);
             //TODO:Тут тоже по идее надо заменять если существует
              TStringList(TStringList(FHotList.Objects[tmp]).Objects[k]).AddObject(th.AObjectName,th)
             end;
          end;

          j:=j+1;
        end;
    end;
  
  FreeAndNil(st);
  FreeAndNil(ini);
end;

procedure THotKeyManager.RegisterManagerForF(AObject: TCustomForm);
var T:TObjInfoClass;
begin
      AObject.KeyPreview:=true;
      
      t:=TObjInfoClass.Create;
      t.AObject:=AObject;
      t.AChilds:=TStringList.Create;
      //Save sender's OnKeyDown proc
      if Assigned(AObject.OnKeyDown) then
        t.AKeyDownProc:=AObject.OnKeyDown;

      AObject.OnKeyDown:=@KeyDownHandler;
        
    FFormsList.AddObject(AObject.Name,T);
end;

procedure THotKeyManager.RegisterManagerForW(AObject: TWinControl);
var T:TObjInfoClass;  Par:TWinControl; i:integer;
begin

      t:=TObjInfoClass.Create;
      t.AObject:=AObject;
      //Save sender's OnKeyDown proc
      if Assigned(AObject.OnKeyDown) then
        t.AKeyDownProc:=AObject.OnKeyDown;
      t.AChilds:=nil;

      //find component's parent form
       par:=AObject;
       while assigned(Par) and (not (par is TCustomForm)) do
        Par:=Par.Parent;
       if par is TCustomForm then
         begin
           i:=FFormsList.IndexOf(par.Name);
           if i=-1 then
             begin
               {register form}
               RegisterManagerForF(Par as TCustomForm);
               i:=FFormsList.IndexOf(par.Name);
               if i=-1 then exit;
             end;
           TObjInfoClass(FFormsList.Objects[i]).AChilds.AddObject(AObject.Name,T);
         end;

end;

procedure THotKeyManager.UnRegisterManagerForF(AObject: TCustomForm);
 var i:integer; T:TObjInfoClass;
begin

    i:=FFormsList.IndexOf(AObject.Name);
    if i=-1 then exit;
    
    T:=TObjInfoClass(FFormsList.Objects[i]);
    
    if Assigned(T.AKeyDownProc) then
      AObject.OnKeyDown:=T.AKeyDownProc;

      T.Free;
      FFormsList.Delete(i);

end;

procedure THotKeyManager.UnRegisterManagerForW(AObject: TWinControl);
 var i:integer; par:TWinControl;  t:TObjInfoClass;

begin
  if Assigned(AObject.OnKeyDown) then
   begin
       //find parent form
       par:=AObject;
       while assigned(Par) and (not (par is TCustomForm)) do
        Par:=Par.Parent;
       if par is TCustomForm then
         begin
           i:=FFormsList.IndexOf(par.Name);
           if i=-1 then exit;
           t:=TObjInfoClass(FFormsList.Objects[i]);
           i:=T.AChilds.IndexOf(AObject.Name);
           if i=-1 then exit;
           TObjInfoClass(T.AChilds.Objects[i]).free;
           T.AChilds.Delete(i);
         end;
  end;
end;


function THotKeyManager.HotKeyEvent(sShortcut: string; ObjInfo:TObjInfoClass):boolean;
var hi,tmp:integer;
    par:TWinControl;
    TH:THotkeyInfoClass;
    st:TStringList;

begin

 Result:=false;

 //HotKey index in list
 hi:=GetHotKeyIndex(sShortcut);
 if hi=-1 then exit;
 
      //find parent form
       par:=ObjInfo.AObject;
       while assigned(Par) and (not (par is TCustomForm)) do
        Par:=Par.Parent;

       if par is TCustomForm then
         begin
           //form's list
           //---------------------
           if not assigned(FHotList.Objects[hi]) then exit;
           st:=TStringList(FHotList.Objects[hi]);
           tmp:=st.IndexOf(Par.Name);
           if tmp=-1 then exit;

           //control's list
           //---------------------
           if not assigned(st.Objects[tmp]) then exit;
           st:=TStringList(st.Objects[tmp]);
           tmp:=st.IndexOf(ObjInfo.AObject.Name);
           if tmp=-1 then exit;
           TH:=THotkeyInfoClass(st.Objects[tmp]);
           
           //---------------------
           {if (TH.AObjectName=ObjInfo.AObject.Name) and
           ((ObjInfo.AObject is TCustomForm) or (TH.AObjectFormName=Par.Name)) then}
           if (CompareText(TH.AObjectName,ObjInfo.AObject.Name)=0) then
             begin
               // Check if the action is enabled.
               if Actions.IsActionEnabled(Copy(TH.ACommand, 4, Length(TH.ACommand) - 3)) then
               begin
                 Result:=true;
                 Actions.Execute(TH.ACommand,TH.AParams);
               end;
             end;
         end;

end;

function THotKeyManager.GetHotKeyIndex(Hotkey: string; FromI:integer=0): integer;
//------------------------------------------------------
    Function DoCompareText(const s1,s2 : string) : PtrInt;
      begin
        result:=CompareText(upcase(s1),upcase(s2));
      end;
//---------------------
begin
  Result:=FromI;
  with FHotList do
  begin
    While (Result<Count) and (DoCompareText(Strings[Result],Hotkey)<>0) do Result:=Result+1;
    if Result=Count then Result:=-1;
  end;
end;

function THotKeyManager.GetFormsListBy(Hotkey: string; List: TStringList):integer;
var i:integer;
begin
  i:=GetHotKeyIndex(Hotkey);
  if i=-1 then
    begin
      Result:=0;
      Exit;
    end
  else
    begin
      List.Clear;
      List.AddStrings(TStringList(FHotList.Objects[i]));
      Result:=list.Count;
    end;
end;

function THotKeyManager.GetControlsListBy(Hotkey: string; List: TStringList
  ): integer;
var i,j:integer; st:TStringList;
begin
  i:=GetHotKeyIndex(Hotkey);
  if i=-1 then
    begin
      Result:=0;
      Exit;
    end
  else
    begin
    List.Clear;
      //List.AddStrings(TStringList(FHotList.Objects[i]));
      st:=TStringList.Create;
      for j:=0 to TStringList(FHotList.Objects[i]).Count-1 do
        begin
          st.AddStrings(TStringList(TStringList(FHotList.Objects[i]).Objects[j]));
        end;
      List.AddStrings(st);
      st.free;
      Result:=list.Count;
    end;
end;

function THotKeyManager.GetCommandsListBy(Hotkey: string; List: TStringList
  ): integer;
var i,j:integer; st:TStringList;
begin
  i:=GetHotKeyIndex(Hotkey);
  if i=-1 then
    begin
      Result:=0;
      Exit;
    end
  else
    begin
      st:=TStringList.Create;
      List.Clear;
      if GetControlsListBy(Hotkey,st)>0 then
        for j:=0 to st.Count-1 do
          begin
            if Assigned(st.Objects[j]) then
             list.Add(st.Strings[j]+'='+THotkeyInfoClass(st.Objects[j]).ACommand);
          end;
       st.Free;
       Result:=list.Count;
    end;
end;


procedure THotKeyManager.KeyDownHandler(Sender: TObject; var Key: Word; Shift: TShiftState);
 //------------------------------------------------------

 function OrigControlKeyDown(ObjInfo:TObjInfoClass):boolean;
   begin
     {Вызов оригинального KeyDown}
     if Assigned(ObjInfo.AKeyDownProc) then
      begin
       ObjInfo.AKeyDownProc(Sender,Key,GetKeyShiftStateEx);
       Result:=true;
      end else Result:=false;
   end;

 function OrigFormKeyDown(ObjInfo:TObjInfoClass):boolean;
   begin
      if Assigned(ObjInfo.AKeyDownProc) then
 	begin
           ObjInfo.AKeyDownProc(Sender,Key,GetKeyShiftStateEx);
           Result:=true;
        end
         else Result:=false;
   end;

  var
      Sinfo:TObjInfoClass;
      i,j:integer;
      Handled:boolean;
      sk:string;
begin

 {предварительная проверка - зарегистрирован ли хоткей вообще,
 чтобы не тратить время на вычисления  и вызов оригинальных обработчиков}
 //if GetHotKeyIndex(KeyToText(Key))=-1 then exit;

 Handled:=false;
 sk:=KeyToText(Key);

 i:=FFormsList.IndexOf((Sender as TWinControl).Name);
 if i=-1 then exit;
 Sinfo:=TObjInfoClass(FFormsList.Objects[i]);

{$IFDEF MSWINDOWS}
  // Don't execute hotkeys with AltGr on Windows.
  if not (GetKeyShiftStateEx = [ssAltGr]) then
{$ENDIF}
  begin
    if (Assigned(Sinfo.AChilds)) and (Sinfo.AChilds.Count>0) then
     begin
      for j:=0 to Sinfo.AChilds.Count-1 do
        if Assigned(Sinfo.AChilds.Objects[j]) then
        if (TObjInfoClass(Sinfo.AChilds.Objects[j]).AObject as TWinControl).Focused then
          begin
             Handled:=HotKeyEvent(sk,TObjInfoClass(Sinfo.AChilds.Objects[j]));
             if Handled then
              begin
               key:=0;
               exit;
              end
           else
             //Оригинальный onKeyDown контрола
             OrigControlKeyDown(TObjInfoClass(Sinfo.AChilds.Objects[j]));
          end;
     end;

    //Наш глобальный хоткей
    Handled:=HotKeyEvent(sk,Sinfo);
  end;

  //Оригинальный OnKeyDown
  if not Handled then
    Handled:=OrigFormKeyDown(Sinfo) else Key:=0;
end;

procedure THotKeyManager.RegisterHotkeyManager(AObject: TWinControl);
begin
  If AObject is TCustomForm then
    RegisterManagerForF(AObject as TCustomForm)
  else
  If AObject is TWinControl then
    RegisterManagerForW(AObject);
end;

procedure THotKeyManager.UnRegisterHotkeyManager(AObject: TWinControl);
begin
If AObject is TCustomForm then
    UnRegisterManagerForF(AObject as TCustomForm)
  else
  If AObject is TWinControl then
    UnRegisterManagerForW(AObject);
end;

{ TObjInfoClass }

destructor TObjInfoClass.Destroy;
begin
  if Assigned(AChilds) then
    begin
      while AChilds.Count>0 do
      begin
       if Assigned(AChilds.Objects[0]) then
        AChilds.Objects[0].Free;
        AChilds.Delete(0);
      end;

      FreeAndNil(AChilds);
    end;

  inherited Destroy;
end;

end.

