{
   File name: uPixMapManager.pas
   Date:      2004/04/xx
   Author:    Radek Cervinka  <radek.cervinka@centrum.cz>

   Fast pixmap memory manager a loader

   Copyright (C) 2004
   
   contributors:
   
   Copyright (C) 2006-2007  Koblov Alexander (Alexx2000@mail.ru)

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

  { TPixMapManager }

  TPixMapManager=class
  
  private
    FExtList:TStringList;
    FPixmapName:TStringList;
    FimgList: TObjectList;
    FiDirIconID: Integer;
    FiDirLinkIconID: Integer;
    FiLinkIconID: Integer;
    FiUpDirIconID: Integer;
    FiDefaultIconID: Integer;
    FiArcIconID : Integer;
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
    function DrawBitmap(iIndex: Integer; Canvas : TCanvas; Rect : TRect) : Boolean;
    Function GetIconByFile(fi:PFileRecItem; PanelMode: TPanelMode):Integer;
  end;
  
var
  PixMapManager:TPixMapManager = nil;

procedure LoadPixMapManager;


implementation
uses
  uGlobsPaths, uOSUtils, uWCXhead, uGlobs{$IFDEF WIN32}, ShellAPI, Windows{$ENDIF};
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
  iIconSize : Integer;
{$ENDIF}
begin
  FExtList:=TStringList.Create;
  FimgList:=TObjectList.Create;
  FPixmapName:=TStringList.Create;
  {$IFDEF WIN32}
    if gIconsSize < 32 then
      iIconSize := SHGFI_SMALLICON
    else
      iIconSize := SHGFI_LARGEICON;
      
    SysImgList := SHGetFileInfo('C:\',
                           0,
                           FileInfo,
                           SizeOf(FileInfo),
                           SHGFI_SYSICONINDEX or iIconSize);
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
  I : Integer;
  png:TPortableNetworkGraphic;
  Plugins : TStringList;
  sCurrentPlugin : String;
  iCurPlugCaps : Integer;
begin
  // add some standard icons
  FiDefaultIconID:=CheckAddPixmap('mimetypes' + PathDelim + 'empty.png');
  FiDirIconID:=CheckAddPixmap('filesystems' + PathDelim + 'folder.png');
  FiDirLinkIconID:=CheckAddPixmap('filesystems' + PathDelim + 'folder-link.png');
  FiLinkIconID:=CheckAddPixmap('filesystems' + PathDelim + 'link.png');
  FiUpDirIconID:=CheckAddPixmap('actions' + PathDelim + 'go-up.png');
  FiArcIconID := CheckAddPixmap('filesystems' + PathDelim + 'archive.png');

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
        iPixMap:=CheckAddPixmap('mimetypes' + PathDelim + sPixMap);
        if iPixMap<0 then
          Continue;

        if FExtList.IndexOf(sExt)<0 then
          FExtList.AddObject(sExt, TObject(iPixMap));
      end;
    finally
      CloseFile(f);
    end;
  end;

  (* Set archive icons *)
  
  Plugins := TStringList.Create;
  gIni.ReadSectionRaw('PackerPlugins', Plugins);
  
  for I:=0 to Plugins.Count - 1 do
        begin
          sCurrentPlugin := Plugins.ValueFromIndex[I];
          iCurPlugCaps := StrToInt(Copy(sCurrentPlugin, 1, Pos(',',sCurrentPlugin) - 1));
          if (iCurPlugCaps and PK_CAPS_HIDE) <> PK_CAPS_HIDE then
            begin
              if FExtList.IndexOf(Plugins.Names[I]) < 0 then
                FExtList.AddObject(Plugins.Names[I], TObject(FiArcIconID));
            end;
        end; //for
  Plugins.Free;
  
  (* /Set archive icons *)

  // now fill imagelist by FPixMap

  for I := 0 to FPixmapName.Count - 1 do
  begin
//    writeln('Loading:',I,' ',FExtList[I],': ',gpPixmapPath+FPixmapName[I]);
    png:=TPortableNetworkGraphic.Create;
    png.LoadFromFile(gpPixmapPath+FPixmapName[I]);
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
{$IFDEF MSWINDOWS}
  if iIndex >= $1000 then
  begin
  Result := Graphics.TBitmap.Create;
  if gIconsSize < 32 then
    begin
      Result.Width := GetSystemMetrics( SM_CXSMICON );
      Result.Height := GetSystemMetrics( SM_CYSMICON );
    end
  else
    begin
      Result.Width := GetSystemMetrics( SM_CXICON );
      Result.Height := GetSystemMetrics( SM_CYICON );
    end;

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


function TPixMapManager.DrawBitmap(iIndex: Integer; Canvas: TCanvas; Rect: TRect): Boolean;
begin
  Result := True;
  if iIndex < FimgList.Count then
    Canvas.Draw(Rect.Left, Rect.Top ,Graphics.TBitmap(FimgList.Items[iIndex]))
  else
{$IFDEF MSWINDOWS}
  if iIndex >= $1000 then
    try
      (*For transparent*)
      ImageList_Draw(SysImgList, iIndex - $1000, Canvas.Handle, Rect.Left, Rect.Top, ILD_TRANSPARENT);
    except
      Result:= False;
    end;

{$ELSE}
    Result:= False;
{$ENDIF}
end;

function TPixMapManager.GetIconByFile(fi: PFileRecItem; PanelMode: TPanelMode): Integer;
var
  Ext : String;
{$IFDEF MSWINDOWS}
    FileInfo : TSHFileInfo;
    _para2 : DWORD;
    _para5 : UINT;
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
    if bLinkIsDir then
    begin
      Result:=FiDirLinkIconID;
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
    {$IFDEF MSWINDOWS}
    
    if PanelMode = pmDirectory then
      begin
        _para2 := 0;
        _para5 := SHGFI_SYSICONINDEX;
      end
    else
      begin
        _para2 := FILE_ATTRIBUTE_NORMAL;
        _para5 := SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES;
      end;

    if gIconsSize < 32 then
      _para5 := _para5 or SHGFI_SMALLICON
    else
      _para5 := _para5 or SHGFI_LARGEICON;

    //WriteLN('Icon for file == ' + sName);
          
    SHGetFileInfo(PCHar(sName),
                           _para2,
                           FileInfo,
                           SizeOf(FileInfo),
                           _para5);
       Result := FileInfo.iIcon + $1000;
       
       //WriteLN('FileInfo.iIcon == ' + IntToStr(FileInfo.iIcon));
       
       if (FExtList.IndexOf(Ext)<0) and (Ext <> 'exe') and (Ext <> 'ico') and (Ext <> 'lnk') then
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

