{
   File name: uPixMapManager.pas
   Date:      2004/04/xx
   Author:    Radek Cervinka  <radek.cervinka@centrum.cz>

   Fast pixmap memory manager a loader

   Copyright (C) 2004
   
   contributors:
   
   Koblov Alexander (Alexx2000@mail.ru)

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


unit uPixMapManager;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, uTypes, contnrs, Graphics;


type
  TPixMapManager=class
  
  private
    FExtList:TStringList;
    FPixmapName:TStringList;
    FimgList: TObjectList;
    FiDirIconID: Integer;
    FiLinkIconID: Integer;
    FiUpDirIconID: Integer;
    FiDefaultIconID: Integer;
    {$IFDEF WIN32}
    SysImgList : Cardinal;
    {$ENDIF}
  protected
    function CheckAddPixmap(const sName:String):Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load(const sFileName:String);
    function GetBitmap(iIndex:Integer; BkColor : TColor):TBitmap;
    Function GetIconByFile(fi:PFileRecItem):Integer;
  end;
  
var
  PixMapManager:TPixMapManager = nil;

procedure LoadPixMapManager;


implementation
uses
  uGlobsPaths, uOSUtils{$IFDEF WIN32}, ShellAPI, Windows{$ENDIF};
{ TPixMapManager }

{$IFDEF WIN32}
function GetRGBColor(Value: TColor): DWORD;
begin
  Result := ColorToRGB(Value);
  case Result of
    clNone: Result := CLR_NONE;
    clDefault: Result := CLR_DEFAULT;
  end;
end;
{$ENDIF}

function TPixMapManager.CheckAddPixmap(const sName: String): Integer;
begin
  Result:=-1;
  if not FileExists(gpPixmapPath+sName) then
  begin
    writeln(Format('Warning: pixmap [%s] not exists!',[gpPixmapPath+sName]));
    Exit;
  end;
  // determine: known this file?
  Result:=FPixmapName.IndexOf(sName);
  if Result<0 then // no
    Result:=FPixmapName.Add(sName); // add to list
end;

constructor TPixMapManager.Create;
{$IFDEF WIN32}
var
  FileInfo : TSHFileInfo;
{$ENDIF}
begin
  FExtList:=TStringList.Create;
  FimgList:=TObjectList.Create;
  FPixmapName:=TStringList.Create;
  {$IFDEF WIN32}
    SysImgList := SHGetFileInfo('C:\',
                           0,
                           FileInfo,
                           SizeOf(FileInfo),
                           SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  {$ENDIF}
end;

destructor TPixMapManager.Destroy;
begin
  if assigned(FPixmapName) then
    FreeAndNil(FPixmapName);
  if assigned(FimgList) then
    FreeAndNil(FimgList);
  if assigned(FExtList) then
    FreeAndNil(FExtList);
  {$IFDEF WIN32}
   ImageList_Destroy(SysImgList);
  {$ENDIF}
  inherited Destroy;
end;

procedure TPixMapManager.Load(const sFileName: String);
var
  f:TextFile;
  s:String;
  sExt, sPixMap:String;
  iekv:integer;
  iPixMap:Integer;
  x:Integer;
  png:TPortableNetworkGraphic;

begin
  if FileExists(sFileName) then
  begin
    assignFile(f,sFileName);
    reset(f);
    try
      while not eof(f) do
      begin
        readln(f,s);
        s:=Trim(lowercase(s));
        iekv:=Pos('=',s);
        if iekv=0 then
          Continue;
        sExt:=Copy(s,1, iekv-1);
        sPixMap:=Copy(s, iekv+1, length(s)-iekv);
        iPixMap:=CheckAddPixmap(sPixMap);
        if iPixMap<0 then
          Continue;

        if FExtList.IndexOf(sExt)<0 then
          FExtList.AddObject(sExt, TObject(iPixMap));
      end;
    finally
      CloseFile(f);
    end;
  end;
  // add some standard icons
  FiDirIconID:=CheckAddPixmap('fdir.png');
  FiLinkIconID:=CheckAddPixmap('flink.png');
  FiUpDirIconID:=CheckAddPixmap('fupdir.png');
  FiDefaultIconID:=CheckAddPixmap('fblank.png');

  // now fill imagelist by FPixMap

  for x:=0 to FPixmapName.Count-1 do
  begin
//    writeln('Loading:',x,' ',FExtList[x],': ',gpPixmapPath+FPixmapName[x]);
    png:=TPortableNetworkGraphic.Create;
    png.LoadFromFile(gpPixmapPath+FPixmapName[x]);
    png.Transparent:=True;
//    bmp.TransparentMode:=tmFixed;
//    writeln(bmp.Width,' ',bmp.Height);
    FimgList.Add(png);
  end;

end;

function TPixMapManager.GetBitmap(iIndex: Integer; BkColor : TColor): Graphics.TBitmap;
begin
  if iIndex<FimgList.Count then
    Result:=Graphics.TBitmap(FimgList.Items[iIndex])
  else
{$IFDEF WIN32}
  if iIndex >= $1000 then
  begin
  Result := Graphics.TBitmap.Create;

  Result.Width := GetSystemMetrics( SM_CXSMICON );
  Result.Height := GetSystemMetrics( SM_CYSMICON );

  try
   (*For pseudo transparent*)
   ImageList_DrawEx(SysImgList, iIndex - $1000, Result.Canvas.Handle, 0, 0, 0, 0, GetRGBColor(BkColor), clNone, ILD_NORMAL);
  except
   Result:=nil;
  end;
  end;

{$ELSE}
    Result:=nil;
{$ENDIF}
end;

function TPixMapManager.GetIconByFile(fi: PFileRecItem): Integer;
var
Ext : String;
{$IFDEF WIN32}
    FileInfo : TSHFileInfo;
{$ENDIF}
begin
  Result:=-1;
  if not assigned(fi) then Exit;


  with fi^ do
  begin
//    writeln(sExt);
    if sName='..' then
    begin
      Result:=FiUpDirIconID;
      Exit;
    end;
    if FPS_ISDIR(iMode) then
    begin
      Result:=FiDirIconID;
      Exit;
    end;
    if FPS_ISLNK(iMode) then
    begin
      Result:=FiLinkIconID;
      Exit;
    end;
    if sExt='' then
    begin
      Result:=FiDefaultIconID;
      Exit;
    end;
    Ext := lowercase(copy(sExt,2,length(sExt)));
    Result:= FExtList.IndexOf(Ext); // ignore .
    if Result<0 then
    begin
    {$IFDEF WIN32}
    SHGetFileInfo(PCHar(sName),
                           0,
                           FileInfo,
                           SizeOf(FileInfo),
                           SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
       Result := FileInfo.iIcon + $1000;
       if (FExtList.IndexOf(Ext)<0) and (Ext <> 'exe') then
        FExtList.AddObject(Ext, TObject(Result));
    {$ELSE}
      Result:=FiDefaultIconID;
    {$ENDIF}
      Exit;
    end;
    Result:=Integer(FExtList.Objects[Result]);
//    writeln(Result);
  end;
end;

procedure LoadPixMapManager;
begin
  PixMapManager:=TPixMapManager.Create;
  PixMapManager.Load(gpExePath+'pixmaps.txt');
end;

initialization

finalization

  if assigned(PixMapManager) then
    FreeAndNil(PixMapManager);

end.

