{
   Double Commander components
   -------------------------------------------------------------------------
   Toolbar panel class

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



unit KAStoolBar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, Buttons, IniFiles, FileUtil;

type

  TOnToolButtonClick = procedure (Sender: TObject; NumberOfButton : Integer) of object;
  TChangeLineCount   = procedure (AddSize : Integer) of object;
  TOnLoadButtonGlyph   = function (sIconFileName : String; iIconSize : Integer; clBackColor : TColor) : TBitmap of object;

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
          //---------------
          ButtonX:string; //Icon
          CmdX:string;    //Command or path
          ParamX:string;  //parameters
          PathX:string;
          MenuX:string;   //Description
          IconicX:Integer; //-1 0 1 full default minimized ( as TC use)
         end;
 //---------------------------------


  { TKAStoolBar }

  TKAStoolBar = class(TPanel)
  private
    FButtonsList: TList;
//    FCmdList,
 //   FIconList : TStringList;
    FPositionX : Integer;
    FPositionY : Integer;
    FIconSize,
    FButtonSize : Integer;
    FNeedMore : Boolean;
    FOnToolButtonClick : TOnToolButtonClick;
    FChangeLineCount : TChangeLineCount;
    FOnLoadButtonGlyph : TOnLoadButtonGlyph;
    FTotalBevelWidth : Integer;
    FCheckToolButton : Boolean;
    FFlatButtons: Boolean;
    FDiskPanel: Boolean;
    FChangePath : String;
    FEnvVar : String;
    FOldWidth : Integer;
    FMustResize,
    FLockResize : Boolean;
    //---------------------
    XButtons:Tlist;
    CurrentBar:string;
    //---------------------
    function LoadBtnIcon(IconPath : String) : TBitMap;
    function GetButton(Index: Integer): TSpeedButton;
    function GetButtonCount: Integer;
    function GetCommand(Index: Integer): String;
//    function GetIconPath(Index: Integer): String;
    procedure SetButton(Index : Integer; Value : TSpeedButton);
    procedure SetCommand(Index: Integer; const AValue: String);
//    procedure SetIconPath(Index: Integer; const AValue: String);
    procedure SetFlatButtons(const AValue : Boolean);
    procedure ToolButtonClick(Sender: TObject);
    procedure UpdateButtonsTag;

  protected
    { Protected declarations }
    procedure CreateWnd; override;
    procedure Resize; override;
    function GetCmdDirFromEnvVar(sPath: String): String;
    function SetCmdDirAsEnvVar(sPath: String): String;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure InitBounds;

    //------------------------------------------------------
    function AddX(ButtonX, CmdX, ParamX, PathX, MenuX:string ):integer;
    procedure SetButtonX(Index:integer; What:Tinfor;Value: string);
    function GetButtonX(Index:integer; What:TInfor):string;
    //------------------------------------------------------

    procedure LoadFromFile(FileName : String);
    procedure SaveToFile(FileName : String);
    function AddButton(sCaption, Cmd, BtnHint, IconPath : String) : Integer;
    procedure RemoveButton(Index: Integer);
    procedure DeleteAllToolButtons;
    property ButtonCount: Integer read GetButtonCount;
    property Buttons[Index: Integer]: TSpeedButton read GetButton write SetButton;
    property Commands[Index: Integer]: String read GetCommand write SetCommand;
//    property Icons[Index: Integer]: String read GetIconPath write SetIconPath;
    property ButtonList: TList read FButtonsList;

  published
    { Published declarations }
    property OnToolButtonClick: TOnToolButtonClick read FOnToolButtonClick write FOnToolButtonClick;
    property OnChangeLineCount : TChangeLineCount read FChangeLineCount write FChangeLineCount;
    property OnLoadButtonGlyph : TOnLoadButtonGlyph read FOnLoadButtonGlyph write FOnLoadButtonGlyph;
    property CheckToolButton : Boolean read FCheckToolButton write FCheckToolButton default False;
    property FlatButtons : Boolean read FFlatButtons write SetFlatButtons default False;
    property IsDiskPanel : Boolean read FDiskPanel write FDiskPanel default False;
    property ButtonGlyphSize : Integer read FIconSize write FIconSize;

    property ChangePath : String read FChangePath write FChangePath;
    property EnvVar : String read FEnvVar write FEnvVar;
  end;


procedure Register;

implementation

uses GraphType;

function TKAStoolBar.GetCmdDirFromEnvVar(sPath: String): String;
begin
  DoDirSeparators(sPath);
  if Pos(FEnvVar, sPath) <> 0 then
    Result := StringReplace(sPath, FEnvVar, ExcludeTrailingPathDelimiter(FChangePath), [rfIgnoreCase])
  else
    Result := sPath;
end;

function TKAStoolBar.SetCmdDirAsEnvVar(sPath: String): String;
begin
  DoDirSeparators(sPath);
  if Pos(FChangePath, sPath) <> 0 then
    Result := StringReplace(sPath, ExcludeTrailingPathDelimiter(FChangePath), FEnvVar, [rfIgnoreCase])
  else
    Result := sPath;
end;

procedure Register;
begin
  RegisterComponents('KASComponents',[TKAStoolBar]);
end;

procedure TKAStoolBar.InitBounds;
begin
  Caption := '';
  if (BevelInner <> bvNone) and (BevelOuter <> bvNone) then
  FTotalBevelWidth := BevelWidth * 2
  else
  FTotalBevelWidth := BevelWidth;

  // change panel size
  FLockResize := True;
  Height := FIconSize + (FTotalBevelWidth * 2) + 6;
  FLockResize := False;


  FButtonSize := Height - FTotalBevelWidth * 2;
//  writeln('FButtonSize = ' + IntToStr(FButtonSize));
  if Width < Height then
     Width := Height;

  FPositionX := FTotalBevelWidth;
  FPositionY := FTotalBevelWidth;
end;

function TKAStoolBar.AddX(ButtonX, CmdX, ParamX, PathX, MenuX: string ): integer;
begin
Result:=XButtons.Add(TKButton.Create);
TKButton(XButtons[Result]).CmdX:=CmdX;
TKButton(XButtons[Result]).ButtonX:=ButtonX;
TKButton(XButtons[Result]).ParamX:=ParamX;
TKButton(XButtons[Result]).PathX:=PathX;
TKButton(XButtons[Result]).MenuX:=MenuX;
end;


function TKAStoolBar.GetButtonX(Index: integer; What: TInfor): string;
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

procedure TKAStoolBar.SetButtonX(Index: integer; What: Tinfor; Value: string);
var
  PNG : TPortableNetworkGraphic;
begin
//if Index<0 then Exit;

If Index>=XButtons.Count then XButtons.Add(TKButton.Create);

 case What of
  ButtonX: begin
            // SetButtonX(Index,ButtonX,Value);
             with TSpeedButton(FButtonsList.Items[Index]) do
               if Assigned(FOnLoadButtonGlyph) then
                 Glyph := FOnLoadButtonGlyph(Value, FIconSize, Color)
               else
                 Glyph := LoadBtnIcon(Value);
             TKButton(XButtons.Items[Index]).ButtonX:=Value;
//             ChangedIco:=True;
           end;
  cmdX:TKButton(XButtons.Items[Index]).cmdX:=Value;
  paramX:TKButton(XButtons.Items[Index]).paramX:=Value;
  pathX:TKButton(XButtons.Items[Index]).pathX:=Value;
  MenuX:TKButton(XButtons.Items[Index]).menuX:=Value;
  {iconicX: begin
             if Value='' then
               TKButton(XButtons.Items[Index]).iconicX:=0
             else
               TKButton(XButtons.Items[Index]).iconicX:=StrToInt(Value);
//             Exit;
           end; }
 end;
// Changes:=True;

end;

procedure TKAStoolBar.Resize;
var
  I, Count, NewHeight : Integer;
  ToolButton : TSpeedButton;
begin
  inherited Resize;

  if FOldWidth = 0 then
    FOldWidth := Width;
    
  if (((FOldWidth <> Width) and not FLockResize) or FMustResize) and (FButtonsList.Count > 0) then
    begin
      // lock on resize handler
      FLockResize := True;
      
      NewHeight := FButtonSize + FTotalBevelWidth * 2;

      if (BevelInner <> bvNone) and (BevelOuter <> bvNone) then
        FTotalBevelWidth := BevelWidth * 2
      else
        FTotalBevelWidth := BevelWidth;

      FButtonSize := NewHeight - FTotalBevelWidth * 2;
      if Width < NewHeight then
        Self.SetBounds(Left, Top, NewHeight, NewHeight);


      FPositionX := FTotalBevelWidth;
      FPositionY := FTotalBevelWidth;
      //*****************
      FNeedMore := False;

      Count := FButtonsList.Count - 1;
      for I := 0 to Count do
        begin
          ToolButton := TSpeedButton(FButtonsList.Items[I]);

          ToolButton.SetBounds(FPositionX, FPositionY, ToolButton.Width, ToolButton.Height );
          //ToolButton.Left:=FPositionX;
          //ToolButton.Top := FPositionY;
          ToolButton.Height := FButtonSize;

          FPositionX:= FPositionX + ToolButton.Width;

          if FNeedMore then
            begin
              NewHeight := NewHeight + FButtonSize;
              FNeedMore := False;
             end;

            if (I <> Count) and ((FPositionX + TSpeedButton(FButtonsList.Items[I + 1]).Width) > Width) then
              begin
                FPositionY:= FPositionY + ToolButton.Height;
                FPositionX := FTotalBevelWidth;
                FNeedMore := True;
             end;
        end;
      FOldWidth := Width;
      FMustResize := False;
      if Assigned(FChangeLineCount) then
        FChangeLineCount(NewHeight - Height);

      Self.SetBounds(Left, Top, Width, NewHeight);

      // unlock on resize handler
      FLockResize := False;
    end;

end;

function TKAStoolBar.LoadBtnIcon(IconPath: String): TBitMap;
var
  PNG : TPortableNetworkGraphic;
begin
  Result := nil;
  if IconPath <> '' then
  if FileExists(IconPath) then
   begin
   if CompareFileExt(IconPath, 'png', false) = 0 then
      begin
        PNG := TPortableNetworkGraphic.Create;
        PNG.LoadFromFile(IconPath);
        Result := TBitMap(PNG);
      end
   else
      begin
         Result := TBitMap.Create;
         Result.LoadFromFile(IconPath);
      end;
   end;
end;

function TKAStoolBar.GetButton(Index: Integer): TSpeedButton;
begin
  Result := TSpeedButton(FButtonsList.Items[Index]);
end;

procedure TKAStoolBar.SetButton(Index : Integer; Value : TSpeedButton);
begin
 TSpeedButton(FButtonsList.Items[Index]) := Value;
end;

procedure TKAStoolBar.SetCommand(Index: Integer; const AValue: String);
begin
//  FCmdList[Index] := AValue;
SetButtonX(Index,CmdX,AValue);
end;

{procedure TKAStoolBar.SetIconPath(Index: Integer; const AValue: String);
var
  PNG : TPortableNetworkGraphic;
begin
//  FIconList[Index] := AValue;
 SetButtonX(Index,ButtonX,AValue);
  with TSpeedButton(FButtonsList.Items[Index]) do
  if Assigned(FOnLoadButtonGlyph) then
    Glyph := FOnLoadButtonGlyph(AValue, FIconSize, Color)
  else
    Glyph := LoadBtnIcon(AValue);
end;
}
procedure TKAStoolBar.SetFlatButtons(const AValue: Boolean);
var
  I :Integer;
begin
  FFlatButtons := AValue;
  for I := 0 to FButtonsList.Count - 1 do
    TSpeedButton(FButtonsList.Items[I]).Flat := FFlatButtons;
end;

procedure TKAStoolBar.ToolButtonClick(Sender: TObject);
begin
  inherited Click;
  if Assigned(FOnToolButtonClick) then
     FOnToolButtonClick(Self, (Sender as TSpeedButton).Tag);
end;

procedure TKAStoolBar.UpdateButtonsTag;
var
  I :Integer;
begin
  for I := 0 to FButtonsList.Count - 1 do
    TSpeedButton(FButtonsList.Items[I]).Tag := I;
end;

procedure TKAStoolBar.DeleteAllToolButtons;
var
  BtnCount,
  I: Integer;
begin
  // lock on resize handler
  FLockResize := True;
      
  BtnCount := FButtonsList.Count - 1;
  for I := 0 to BtnCount do
    begin

      TSpeedButton(FButtonsList.Items[0]).Free;
      FButtonsList.Delete(0);

     TKButton(XButtons[0]).Free;
     XButtons.Delete(0);
//      FCmdList.Delete(0);
//      FIconList.Delete(0);
  end;
  // Assign to BtnCount new toolbar height
  BtnCount := FButtonSize + FTotalBevelWidth * 2;
  // Assign to I old toolbar height
  I := Height;
  // set new toolbar height
  Self.SetBounds(Left, Top, Width, BtnCount);
  
  if Assigned(FChangeLineCount) then
    FChangeLineCount(BtnCount - I);
  
  FNeedMore := False;
  InitBounds;

  // unlock on resize handler
  FLockResize := False;
end;

function TKAStoolBar.GetButtonCount: Integer;
begin
  Result := FButtonsList.Count;
end;

function TKAStoolBar.GetCommand(Index: Integer): String;
begin
//  Result := FCmdList[Index];
 Result := GetButtonX(Index,CmdX);
end;

{function TKAStoolBar.GetIconPath(Index: Integer): String;
begin
//  Result := FIconList[Index];
 Result := GetButtonX(Index,ButtonX);
end;
}
constructor TKAStoolBar.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FButtonsList := TList.Create;
//---------------------
  XButtons := TList.Create;
//---------------------
 // FCmdList := TStringList.Create;
//  FIconList := TStringList.Create;
  FNeedMore := False;
  FOldWidth := Width;
  FMustResize := False;
  FLockResize := False;
  FIconSize := 16; // default
end;

destructor TKAStoolBar.Destroy;
var
  I: Integer;
begin
  for I := 0 to FButtonsList.Count - 1 do
    if TControl(FButtonsList[I]) is TSpeedButton then
      TSpeedButton(FButtonsList.Items[I]).Free;

 //---------------------
  if Assigned(XButtons) then
    begin
      if XButtons.Count>0 then
        for I := 0 to XButtons.Count - 1 do
          TKButton(XButtons.Items[I]).Free;
      FreeAndNil(XButtons);
    end;
 //---------------------

  FreeAndNil(FButtonsList);
//  FreeAndNil(FCmdList);
//  FreeAndNil(FIconList);
  inherited Destroy;
end;

procedure TKAStoolBar.CreateWnd;
begin
  inherited CreateWnd;
  InitBounds;
end;

procedure TKAStoolBar.LoadFromFile(FileName: String);
var
  IniFile : Tinifile;
  BtnCount, I : Integer;
begin
  DeleteAllToolButtons;
  FPositionX := FTotalBevelWidth;
  FPositionY := FTotalBevelWidth;
  IniFile := Tinifile.Create(FileName);
  BtnCount := IniFile.ReadInteger('Buttonbar', 'Buttoncount', 0);
  //---------------------
  CurrentBar:=FileName;
  //---------------------
  for I := 1 to BtnCount do
    begin
      AddButton('', GetCmdDirFromEnvVar(IniFile.ReadString('Buttonbar', 'cmd' + IntToStr(I), '')),
                IniFile.ReadString('Buttonbar', 'menu' + IntToStr(I), ''),
                GetCmdDirFromEnvVar(IniFile.ReadString('Buttonbar', 'button' + IntToStr(I), '')));
      //---------------------
       XButtons.Add(TKButton.Create);
           TKButton(XButtons[I-1]).ButtonX :=GetCmdDirFromEnvVar(IniFile.ReadString('Buttonbar', 'button' + IntToStr(I), ''));
           TKButton(XButtons[I-1]).CmdX := IniFile.ReadString('Buttonbar', 'cmd' + IntToStr(I), '');
           TKButton(XButtons[I-1]).ParamX := IniFile.ReadString('Buttonbar', 'param' + IntToStr(I), '');
           TKButton(XButtons[I-1]).PathX := IniFile.ReadString('Buttonbar', 'path' + IntToStr(I), '');
           TKButton(XButtons[I-1]).MenuX := IniFile.ReadString('Buttonbar', 'menu' + IntToStr(I), '')
         //IconicX := IniFile.ReadInteger('Buttonbar', 'icon' + IntToStr(I),0);

      //---------------------
    end;
  IniFile.Free;
end;

procedure TKAStoolBar.SaveToFile(FileName: String);
var
  IniFile : Tinifile;
  I : Integer;
begin
//------------------------------------------------------
  //For cleaning. Without this saved file will contain removed buttons
  DeleteFile(FileName);
//------------------------------------------------------

  IniFile := Tinifile.Create(FileName);
  IniFile.WriteInteger('Buttonbar', 'Buttoncount', FButtonsList.Count);

  for I := 0 to FButtonsList.Count - 1 do
    begin
//      IniFile.WriteString('Buttonbar', 'button' + IntToStr(I + 1), SetCmdDirAsEnvVar(FIconList[I]));
//      IniFile.WriteString('Buttonbar', 'cmd' + IntToStr(I + 1), SetCmdDirAsEnvVar(FCmdList[I]));
      //------------------------------------------------------
      IniFile.WriteString('Buttonbar', 'button' + IntToStr(I + 1), SetCmdDirAsEnvVar(GetButtonX(I,ButtonX)));
      IniFile.WriteString('Buttonbar', 'cmd' + IntToStr(I + 1), SetCmdDirAsEnvVar(GetButtonX(I,CmdX)));
      IniFile.WriteString('Buttonbar', 'param' + IntToStr(I + 1), GetButtonX(I,ParamX) );
      IniFile.WriteString('Buttonbar', 'path' + IntToStr(I + 1), GetButtonX(I,PathX) );
      IniFile.WriteString('Buttonbar', 'menu' + IntToStr(I + 1),GetButtonX(I,MenuX) );
      //------------------------------------------------------
//      IniFile.WriteString('Buttonbar', 'menu' + IntToStr(I + 1), TSpeedButton(FButtonsList.Items[I]).Hint);
    end;
  IniFile.Free;
end;

function TKAStoolBar.AddButton(sCaption, Cmd, BtnHint, IconPath : String) : Integer;
var
  ToolButton: TSpeedButton;
  I:Integer;
begin
  // lock on resize handler
  FLockResize := True;

  ToolButton:= TSpeedButton.Create(Self);
  //Include(ToolButton.ComponentStyle, csSubComponent);
  ToolButton.Parent:=Self;
  ToolButton.Visible := True;

  ToolButton.Height := FButtonSize;
  ToolButton.ParentShowHint := False;
  ToolButton.Caption := sCaption;
  ToolButton.ShowHint := True;
  ToolButton.Hint := BtnHint;

  if FDiskPanel then
    begin
      ToolButton.Width := ToolButton.Canvas.TextWidth(sCaption) + ToolButton.Glyph.Width + 32;
    end
  else
    ToolButton.Width := FButtonSize;

  if ((FPositionX + ToolButton.Width) > Width) then
    begin
      FPositionY:= FPositionY + ToolButton.Height;
      FPositionX := FTotalBevelWidth;
      if Assigned(FChangeLineCount) then
        FChangeLineCount(FButtonSize);
      Height := Height + FButtonSize;
    end;

  ToolButton.Left:= FPositionX;
  ToolButton.Top := FPositionY;

  //WriteLN('ToolButton.Left == ' + IntToStr(ToolButton.Left));

  if Assigned(OnMouseUp) then
    ToolButton.OnMouseUp := OnMouseUp;

  if FCheckToolButton then
    ToolButton.GroupIndex := 1;

  ToolButton.Flat := FFlatButtons;

  with ToolButton do
  if Assigned(FOnLoadButtonGlyph) then
    Glyph := FOnLoadButtonGlyph(IconPath, FIconSize, Color)
  else
    Glyph := LoadBtnIcon(IconPath);

  ToolButton.OnClick:=TNotifyEvent(@ToolButtonClick);

  FPositionX:= FPositionX + ToolButton.Width;

  ToolButton.Tag := FButtonsList.Add(ToolButton);
//  FCmdList.Add(Cmd);
//  FIconList.Add(IconPath);
  
  if FDiskPanel then
    AddX(sCaption,Cmd,'','','');
    
  // unlock on resize handler
  FLockResize := False;
  
  Result := ToolButton.Tag;
end;

procedure TKAStoolBar.RemoveButton(Index: Integer);
var
  I, OldLeft, PrevLeft,
  OldTop, PrevTop : integer;
begin
  try
    TSpeedButton(FButtonsList.Items[Index]).Visible := False;
    TSpeedButton(FButtonsList.Items[Index]).Free;
    FButtonsList.Delete(Index);
    UpdateButtonsTag;
    //FCmdList.Delete(Index);
    //FIconList.Delete(Index);
    //---------------------
    TKButton(XButtons[Index]).Free;
    XButtons.Delete(Index);
    //---------------------
    FMustResize := True;
    Resize;

  finally
    Repaint;
  end;
end;

end.
