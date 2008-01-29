{
   File name: kasbarfiles.pas

   Author:    Dmitry Kolomiets (B4rr4cuda@rambler.ru)
   Class working with *.bar files.

   Based on KASToolBar functions
   Copyright (C) 2006-2007  Koblov Alexander (Alexx2000@mail.ru)
    
   contributors:



   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   in a file called COPYING along with this program; if not, write to
   the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
   02139, USA.
}

unit KASBarFiles;


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,IniFiles;

type

  //Button property's type
  //------------------------------------------------------
    TInfor=(ButtonX,
            CmdX,
            ParamX,
            PathX,
            MenuX,
            IconicX
           );
  //------------------------------------------------------

 //Class of button
 //---------------------------------
 TKButton=class
          ButtonX:string; //Icon
          CmdX:string;    //Command or path
          ParamX:string;  //parameters
          PathX:string;
          MenuX:string;   //Description
          IconicX:Integer; //-1 0 1 full default minimized ( as TC use)
         end;
 //---------------------------------

 { TBarClass }

 TBarClass=class
             CurrentBar:string;
             
           private
             XButtons:Tlist;
             FEnvVar : String;
             FChangePath : String;
             function GetButton(Index: Integer): TKButton;
             function GetButtonCount: Integer;
             function GetCmdDirFromEnvVar(sPath: String): String;
             function SetCmdDirAsEnvVar(sPath: String): String;
             procedure SetButton(Index:Integer; const AValue: TKButton);
           //------------------------------------------------------

           public

             Constructor Create;
             destructor Destroy; override;
             //---------------------
             function GetButtonX(Index:integer; What:TInfor):string;
             function AddButtonX(ButtonX, CmdX, ParamX, PathX, MenuX:string ):integer;
             //---------------------
             procedure RemoveButton(Index: Integer);
             procedure DeleteAllButtons;
             procedure SetButtonX(Index:integer; What:Tinfor;Value: string);
             procedure LoadFromFile(FileName : String);
             procedure SaveToFile(FileName : String);
             //---------------------
             property ButtonCount: Integer read GetButtonCount;
             property Buttons[Index:Integer]: TKButton read GetButton write SetButton;
             property EnvVar : String read FEnvVar write FEnvVar;
             property ChangePath : String read FChangePath write FChangePath;
           //------------------------------------------------------
           end;
           
implementation
{ TBarClass }

constructor TBarClass.Create;
begin
  XButtons:=TList.Create;
end;

destructor TBarClass.Destroy;
var i:integer;
begin
    if Assigned(XButtons) then
    begin
      if XButtons.Count>0 then
        for I := 0 to XButtons.Count - 1 do
          TKButton(XButtons.Items[I]).Free;
      FreeAndNil(XButtons);
    end;

  inherited Destroy;
end;

procedure TBarClass.SetButtonX(Index: integer; What: Tinfor; Value: string);
begin
If Index>=XButtons.Count then XButtons.Add(TKButton.Create);

 case What of
  ButtonX: TKButton(XButtons.Items[Index]).ButtonX:=Value;
  cmdX:    TKButton(XButtons.Items[Index]).cmdX:=Value;
  paramX:  TKButton(XButtons.Items[Index]).paramX:=Value;
  pathX:   TKButton(XButtons.Items[Index]).pathX:=Value;
  MenuX:   TKButton(XButtons.Items[Index]).menuX:=Value;
  iconicX: begin
             if Value='' then
               TKButton(XButtons.Items[Index]).iconicX:=0
             else
               TKButton(XButtons.Items[Index]).iconicX:=StrToInt(Value);
           end;
 end;

end;

function TBarClass.GetButtonX(Index: integer; What: TInfor): string;
begin
if (index>=XButtons.Count) or (Index<0) then Exit;
      case What of
         ButtonX: Result := TKButton(XButtons.Items[Index]).ButtonX;
         cmdX:    Result := TKButton(XButtons.Items[Index]).CmdX;
         paramX:  Result := TKButton(XButtons.Items[Index]).ParamX;
         pathX:   Result := TKButton(XButtons.Items[Index]).PathX;
         menuX:   Result := TKButton(XButtons.Items[Index]).MenuX;
         iconicX: Result := IntToStr(TKButton(XButtons.Items[Index]).IconicX);
      end;
end;

function TBarClass.AddButtonX(ButtonX, CmdX, ParamX, PathX, MenuX: string
  ): integer;
begin
  Result:=XButtons.Add(TKButton.Create);
  TKButton(XButtons[Result]).CmdX:=CmdX;
  TKButton(XButtons[Result]).ButtonX:=ButtonX;
  TKButton(XButtons[Result]).ParamX:=ParamX;
  TKButton(XButtons[Result]).PathX:=PathX;
  TKButton(XButtons[Result]).MenuX:=MenuX;
end;

procedure TBarClass.LoadFromFile(FileName: String);
var
  IniFile : Tinifile;
  BtnCount, I : Integer;
begin
  DeleteAllButtons;
  IniFile := Tinifile.Create(FileName);
  BtnCount := IniFile.ReadInteger('Buttonbar', 'Buttoncount', 0);
  CurrentBar:=FileName;
  for I := 1 to BtnCount do
    begin
       XButtons.Add(TKButton.Create);
           TKButton(XButtons[I-1]).ButtonX :=GetCmdDirFromEnvVar(IniFile.ReadString('Buttonbar', 'button' + IntToStr(I), ''));
           TKButton(XButtons[I-1]).CmdX := IniFile.ReadString('Buttonbar', 'cmd' + IntToStr(I), '');
           TKButton(XButtons[I-1]).ParamX := IniFile.ReadString('Buttonbar', 'param' + IntToStr(I), '');
           TKButton(XButtons[I-1]).PathX := IniFile.ReadString('Buttonbar', 'path' + IntToStr(I), '');
           TKButton(XButtons[I-1]).MenuX := IniFile.ReadString('Buttonbar', 'menu' + IntToStr(I), '');
           TKButton(XButtons[I-1]).IconicX := IniFile.ReadInteger('Buttonbar', 'icon' + IntToStr(I),0);

    end;
  IniFile.Free;
end;

procedure TBarClass.SaveToFile(FileName: String);
var
  IniFile : Tinifile;
  I : Integer;
begin
  //For cleaning. Without this saved file will contain removed buttons
if FileExists(FileName) then  DeleteFile(FileName);

  IniFile := Tinifile.Create(FileName);
  IniFile.WriteInteger('Buttonbar', 'Buttoncount', XButtons.Count);

  for I := 0 to XButtons.Count - 1 do
    begin
      IniFile.WriteString('Buttonbar', 'button' + IntToStr(I + 1), SetCmdDirAsEnvVar(GetButtonX(I,ButtonX)));
      IniFile.WriteString('Buttonbar', 'cmd' + IntToStr(I + 1), SetCmdDirAsEnvVar(GetButtonX(I,CmdX)));
      IniFile.WriteString('Buttonbar', 'param' + IntToStr(I + 1), GetButtonX(I,ParamX) );
      IniFile.WriteString('Buttonbar', 'path' + IntToStr(I + 1), GetButtonX(I,PathX) );
      IniFile.WriteString('Buttonbar', 'menu' + IntToStr(I + 1),GetButtonX(I,MenuX) );
    end;
  IniFile.Free;
end;

procedure TBarClass.RemoveButton(Index: Integer);
begin
    TKButton(XButtons[Index]).Free;
    XButtons.Delete(Index);
end;

procedure TBarClass.DeleteAllButtons;
begin
    while XButtons.Count>0 do
      begin
        TKButton(XButtons[0]).Free;
        XButtons.Delete(0);
      end;
end;

function TBarClass.GetButtonCount: Integer;
begin
Result := XButtons.Count;
end;

function TBarClass.GetButton(Index:Integer): TKButton;
begin
  Result:=TKButton(XButtons[Index]);
end;

function TBarClass.GetCmdDirFromEnvVar(sPath: String): String;
begin
  DoDirSeparators(sPath);
  if Pos(FEnvVar, sPath) <> 0 then
    Result := StringReplace(sPath, FEnvVar, ExcludeTrailingPathDelimiter(FChangePath), [rfIgnoreCase])
  else
    Result := sPath;
end;

procedure TBarClass.SetButton(Index:Integer; const AValue: TKButton);
begin
 TKButton(XButtons[Index]):=AValue;
end;

function TBarClass.SetCmdDirAsEnvVar(sPath: String): String;
begin
  DoDirSeparators(sPath);
  if Pos(FChangePath, sPath) <> 0 then
    Result := StringReplace(sPath, ExcludeTrailingPathDelimiter(FChangePath), FEnvVar, [rfIgnoreCase])
  else
    Result := sPath;
end;

end.

