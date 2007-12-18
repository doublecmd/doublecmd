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
  Classes, SysUtils, uTypes, contnrs, Graphics, uOSUtils;


type
  TDriveIcons = record
    bmMediaFloppy,
    bmDriveHardDisk,
    bmMediaFlash,
    bmMediaOptical : TBitmap;
  end;
  PDriveIcons = ^TDriveIcons;
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
    FFirstIconSize,
    FSecondIconSize,
    FThirdIconSize : TDriveIcons;
    FPixmapSize : String;
    {$IFDEF WIN32}
    SysImgList : Cardinal;
    {$ENDIF}
  protected
    function CheckLoadPixmap(const sName:String) : TBitmap;
    function CheckAddPixmap(const sName:String):Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load(const sFileName:String);
    function GetBitmap(iIndex:Integer; BkColor : TColor):TBitmap;
    function GetStretchBitmap(iIndex: Integer; BkColor : TColor; iSize : Integer): TBitmap;
    function DrawBitmap(iIndex: Integer; Canvas : TCanvas; Rect : TRect) : Boolean;
    Function GetIconByFile(fi:PFileRecItem; PanelMode: TPanelMode):Integer;
    function GetDriveIcon(Drive : PDrive; IconSize : Integer; clBackColor : TColor) : Graphics.TBitmap;
  end;

function StretchBitmap(bmBitmap : Graphics.TBitmap; iIconSize : Integer;
                       clBackColor : TColor; bFreeAtEnd : Boolean = False) : Graphics.TBitmap;
function LoadBitmapFromFile(sFileName : String; iIconSize : Integer; clBackColor : TColor) : Graphics.TBitmap;
  
var
  PixMapManager:TPixMapManager = nil;

procedure LoadPixMapManager;


implementation
uses
  FileUtil, uGlobsPaths, uWCXhead, uGlobs{$IFDEF MSWINDOWS}, ShellAPI, Windows, uIcoFiles{$ENDIF};

{$IFDEF MSWINDOWS}
function GetRGBColor(Value: TColor): DWORD;
begin
  Result := ColorToRGB(Value);
  case Result of
    clNone: Result := CLR_NONE;
    clDefault: Result := CLR_DEFAULT;
  end;
end;
{$ENDIF}

function StretchBitmap(bmBitmap : Graphics.TBitmap; iIconSize : Integer;
                       clBackColor : TColor; bFreeAtEnd : Boolean = False) : Graphics.TBitmap;
var
  bmStretchBitmap : Graphics.TBitMap;
  memstream: TMemoryStream;
begin
  bmStretchBitmap:= Graphics.TBitMap.Create;
    with bmStretchBitmap do
      begin
        Width := iIconSize;
        Height := iIconSize;

        Canvas.Brush.Color := clBackColor;
        Canvas.FillRect(Canvas.ClipRect);
        Canvas.StretchDraw(Canvas.ClipRect, bmBitmap);
        { For drawing color transparent bitmaps }
        memstream := TMemoryStream.Create;
        try
          SaveToStream(memstream);
          memstream.position := 0;
          LoadFromStream(memstream);
        finally
          memstream.free;
        end;
        Transparent := True;
        TransparentColor := clBackColor;
        if bFreeAtEnd then bmBitmap.Free;
        Result := bmStretchBitmap;
      end; //  with
end;

function LoadBitmapFromFile(sFileName : String; iIconSize : Integer; clBackColor : TColor) : Graphics.TBitmap;
var
{$IFDEF MSWINDOWS}
  iPos,
  iIconIndex : Integer;
  phicon,
  phiconLarge,
  phiconSmall : HIcon;
{$ENDIF}
  bmStandartBitmap : Graphics.TBitMap;
  PNG : TPortableNetworkGraphic;
begin
{$IFDEF MSWINDOWS}
  iIconIndex := -1;
  iPos :=Pos(',', sFileName);
  if iPos <> 0 then
    begin
      iIconIndex := StrToIntDef(Copy(sFileName, iPos + 1, Length(sFileName) - iPos), 0);
      sFileName := Copy(sFileName, 1, iPos - 1);
    end;

  if FileIsExeLib(sFileName) then
    begin
      if iIconIndex < 0 then iIconIndex := 0;
      ExtractIconEx(PChar(sFileName), iIconIndex, phiconLarge, phiconSmall, 1);
      case iIconSize of
        16:  // Small icon
          Result := CreateIconFromHandle(phiconSmall);
        32:  // Large icon
          Result := CreateIconFromHandle(phiconLarge);
        else
          begin
            { Convert TIcon to TBitMap  }
            bmStandartBitmap := Graphics.TBitMap.Create;
            if iIconSize > 16 then  // Large icon
              begin
                bmStandartBitmap.Width := GetSystemMetrics(SM_CXICON);
                bmStandartBitmap.Height := GetSystemMetrics(SM_CYICON);
                phicon := phiconLarge;
              end
            else  // Small icon
              begin
                bmStandartBitmap.Width := GetSystemMetrics(SM_CXSMICON);
                bmStandartBitmap.Height := GetSystemMetrics(SM_CYSMICON);
                phicon := phiconSmall;
              end;

            bmStandartBitmap.Canvas.Brush.Color := clBackColor;
            bmStandartBitmap.Canvas.FillRect(bmStandartBitmap.Canvas.ClipRect);
            Windows.DrawIcon(bmStandartBitmap.Canvas.Handle, 0, 0, phicon);
            Result := StretchBitmap(bmStandartBitmap, iIconSize, clBackColor, True);
        end;  // non standart size
      end;  // case
    end  // IsExecutable
  else
{$ENDIF}
    begin
      if FileExists(sFileName) then
        begin
          if CompareFileExt(sFileName, 'png', false) = 0 then
            begin
              PNG := TPortableNetworkGraphic.Create;
              PNG.LoadFromFile(sFileName);
              bmStandartBitmap := Graphics.TBitMap(PNG);
            end
          else
            begin
              bmStandartBitmap := Graphics.TBitMap.Create;
              bmStandartBitmap.LoadFromFile(sFileName);
            end;

          // if need stretch icon
          if  (iIconSize <> bmStandartBitmap.Height) or (iIconSize <> bmStandartBitmap.Width) then
            Result := StretchBitmap(bmStandartBitmap, iIconSize, clBackColor, True)
          else
            Result := bmStandartBitmap;
        end  // FileExists
      else
        Result := nil;  // file not found
    end;  // IsExecutable else
end;

{ TPixMapManager }

function TPixMapManager.CheckLoadPixmap(const sName: String): Graphics.TBitmap;
var
  png : TPortableNetworkGraphic;
begin
  Result:= nil;
  if not FileExists(gpPixmapPath+FPixmapSize+sName) then
  begin
    writeln(Format('Warning: pixmap [%s] not exists!',[gpPixmapPath+FPixmapSize+sName]));
    Exit;
  end;
  png:=TPortableNetworkGraphic.Create;
  png.LoadFromFile(gpPixmapPath + FPixmapSize+ sName);
  png.Transparent:=True;
  Result := Graphics.TBitmap(png);
end;

function TPixMapManager.CheckAddPixmap(const sName: String): Integer;
begin
  Result:=-1;
  if not FileExists(gpPixmapPath+FPixmapSize+sName) then
  begin
    writeln(Format('Warning: pixmap [%s] not exists!',[gpPixmapPath+FPixmapSize+sName]));
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
  with FFirstIconSize do
  begin
    if Assigned(bmMediaFloppy) then FreeAndNil(bmMediaFloppy);
    if Assigned(bmDriveHardDisk) then FreeAndNil(bmDriveHardDisk);
    if Assigned(bmMediaFlash) then FreeAndNil(bmMediaFlash);
    if Assigned(bmMediaOptical) then FreeAndNil(bmMediaOptical);
  end;
  with FSecondIconSize do
  begin
    if Assigned(bmMediaFloppy) then FreeAndNil(bmMediaFloppy);
    if Assigned(bmDriveHardDisk) then FreeAndNil(bmDriveHardDisk);
    if Assigned(bmMediaFlash) then FreeAndNil(bmMediaFlash);
    if Assigned(bmMediaOptical) then FreeAndNil(bmMediaOptical);
  end;
  with FThirdIconSize do
  begin
    if Assigned(bmMediaFloppy) then FreeAndNil(bmMediaFloppy);
    if Assigned(bmDriveHardDisk) then FreeAndNil(bmDriveHardDisk);
    if Assigned(bmMediaFlash) then FreeAndNil(bmMediaFlash);
    if Assigned(bmMediaOptical) then FreeAndNil(bmMediaOptical);
  end;
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
  sPixMapSize : String;
  I : Integer;
  png:TPortableNetworkGraphic;
  Plugins : TStringList;
  sCurrentPlugin : String;
  iCurPlugCaps : Integer;
begin
  //  load all drive icons
  sPixMapSize := FPixmapSize;  // save icon size path
  FPixmapSize := '16x16' + PathDelim;
  with FFirstIconSize do
  begin
    bmMediaFloppy := CheckLoadPixmap('devices' + PathDelim + 'media-floppy.png');
    bmDriveHardDisk := CheckLoadPixmap('devices' + PathDelim + 'drive-harddisk.png');
    bmMediaFlash := CheckLoadPixmap('devices' + PathDelim + 'media-flash.png');
    bmMediaOptical := CheckLoadPixmap('devices' + PathDelim + 'media-optical.png');
  end;
  FPixmapSize := '22x22' + PathDelim;
  with FSecondIconSize do
  begin
    bmMediaFloppy := CheckLoadPixmap('devices' + PathDelim + 'media-floppy.png');
    bmDriveHardDisk := CheckLoadPixmap('devices' + PathDelim + 'drive-harddisk.png');
    bmMediaFlash := CheckLoadPixmap('devices' + PathDelim + 'media-flash.png');
    bmMediaOptical := CheckLoadPixmap('devices' + PathDelim + 'media-optical.png');
  end;
  FPixmapSize := '32x32' + PathDelim;
  with FThirdIconSize do
  begin
    bmMediaFloppy := CheckLoadPixmap('devices' + PathDelim + 'media-floppy.png');
    bmDriveHardDisk := CheckLoadPixmap('devices' + PathDelim + 'drive-harddisk.png');
    bmMediaFlash := CheckLoadPixmap('devices' + PathDelim + 'media-flash.png');
    bmMediaOptical := CheckLoadPixmap('devices' + PathDelim + 'media-optical.png');
  end;
    FPixmapSize := sPixMapSize;  // restore icon size path
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
//    writeln('Loading:',I,' ',FExtList[I],': ',gpPixmapPath+ FPixmapSize+FPixmapName[I]);
    png:=TPortableNetworkGraphic.Create;
    png.LoadFromFile(gpPixmapPath+FPixmapSize+FPixmapName[I]);
    png.Transparent:=True;
//    bmp.TransparentMode:=tmFixed;
//    writeln(bmp.Width,' ',bmp.Height);
    FimgList.Add(png);
  end;

end;

function TPixMapManager.GetBitmap(iIndex: Integer; BkColor : TColor): Graphics.TBitmap;
{$IFDEF MSWINDOWS}
var
  memstream: TMemoryStream;
{$ENDIF}
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
    { For drawing color transparent bitmaps }
    memstream := TMemoryStream.create;
    try
      Result.SaveToStream(memstream);
      memstream.position := 0;
      Result.LoadFromStream(memstream);
    finally
      memstream.free;
    end;

    Result.Transparent := True;
    Result.TransparentColor := BkColor;
  except
    Result:=nil;
  end;
  end;

{$ELSE}
    Result:=nil;
{$ENDIF}
end;

function TPixMapManager.GetStretchBitmap(iIndex: Integer; BkColor: TColor;
  iSize: Integer): Graphics.TBitmap;
begin
  Result := Graphics.TBitMap.Create;
  with Result do
  begin
    Width := iSize;
    Height := iSize;

    Canvas.Brush.Color := BkColor;
    Canvas.FillRect(Canvas.ClipRect);
    Canvas.StretchDraw(Canvas.ClipRect, GetBitmap(iIndex, BkColor));
  end;
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
      {$IFDEF MSWINDOWS}
      if not FileExists(sName + '\desktop.ini') then
      {$ENDIF}
    begin
      Result:=FiDirIconID;
      Exit;
    end;
    if FPS_ISLNK(iMode) then
    begin
      Result:=FiLinkIconID;
      Exit;
    end;
    if (sExt = '') and (not FPS_ISDIR(iMode)) then
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
       
       if (FExtList.IndexOf(Ext)<0) and (Ext <> 'exe') and (Ext <> 'ico') and (Ext <> 'lnk')  and (not FPS_ISDIR(iMode)) then
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

function TPixMapManager.GetDriveIcon(Drive : PDrive; IconSize : Integer; clBackColor : TColor) : Graphics.TBitmap;
var
  DriveIcons : PDriveIcons;
{$IFDEF MSWINDOWS}
  SFI: TSHFileInfo;
{$ENDIF}
begin
  Result := nil;
{$IFDEF MSWINDOWS}
  if not gCustomDriveIcons then
    begin
      SHGetFileInfo(PChar(Drive^.Path), 0, SFI, SizeOf(SFI), SHGFI_ICON);
      SFI.hIcon := 0;
      case IconSize of
      16: // Standart icon size
        begin
          SHGetFileInfo(PChar(Drive^.Path), 0, SFI, SizeOf(SFI), SHGFI_ICON or SHGFI_SMALLICON);
          if SFI.hIcon <> 0 then
            Result := CreateIconFromHandle(SFI.hIcon);
        end;
      32:  // Standart icon size
        begin
          SHGetFileInfo(PChar(Drive^.Path), 0, SFI, SizeOf(SFI), SHGFI_ICON or SHGFI_LARGEICON);
          if SFI.hIcon <> 0 then
            Result := CreateIconFromHandle(SFI.hIcon);
        end;
      else  // for non standart icon size we Convert HIcon to TBitMap
        begin
          SHGetFileInfo(PChar(Drive^.Path), 0, SFI, SizeOf(SFI), SHGFI_ICON);
          Result := Graphics.TBitMap.Create;
          Result.Width := GetSystemMetrics(SM_CXICON);
          Result.Height := GetSystemMetrics(SM_CYICON);
          Result.Canvas.Brush.Color := clBackColor;
          Result.Canvas.FillRect(Result.Canvas.ClipRect);
          Windows.DrawIcon(Result.Canvas.Handle,0,0,SFI.hIcon);

          Result := StretchBitmap(Result, IconSize, clBackColor, True);
        end;
      end;  //  case
    end // not gCustomDriveIcons
  else
{$ENDIF}
    begin
      case IconSize of
      16: // Standart 16x16 icon size
        DriveIcons := @FFirstIconSize;
      22:  // Standart 22x22 icon size
        DriveIcons := @FSecondIconSize;
      32:  // Standart 32x32 icon size
        DriveIcons := @FThirdIconSize;
      else  // for non standart icon size use more large icon for stretch
        DriveIcons := @FThirdIconSize;
      end;
      case Drive^.DriveType of
      dtFloppy:
        Result :=  DriveIcons^.bmMediaFloppy;
      dtFixed:
        Result :=  DriveIcons^.bmDriveHardDisk;
      dtFlash:
        Result :=  DriveIcons^.bmMediaFlash;
      dtCDROM:
        Result :=  DriveIcons^.bmMediaOptical;
      else
        Result :=  DriveIcons^.bmDriveHardDisk;
      end;
      //  if need stretch icon
      if (IconSize <> 16) and (IconSize <> 22) and (IconSize <> 32) then
        begin
          Result := StretchBitmap(Result, IconSize, clBackColor);
        end;
    end;  //
end;

procedure LoadPixMapManager;
begin
  PixMapManager:=TPixMapManager.Create;
  PixMapManager.FPixmapSize:= IntToStr(gIconsSize) + 'x' + IntToStr(gIconsSize) + PathDelim;
  PixMapManager.Load(gpExePath+'pixmaps.txt');
end;

initialization

finalization

  if assigned(PixMapManager) then
    FreeAndNil(PixMapManager);

end.

