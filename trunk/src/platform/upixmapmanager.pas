{
   File name: uPixMapManager.pas
   Date:      2004/04/xx
   Author:    Radek Cervinka  <radek.cervinka@centrum.cz>

   Fast pixmap memory manager a loader

   Copyright (C) 2004
   
   contributors:
   
   Copyright (C) 2006-2009  Koblov Alexander (Alexx2000@mail.ru)

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
  Classes, SysUtils, uTypes, Graphics, uOSUtils, uFileSorting,
  uFile, uColumnsFileViewFiles
  {$IF DEFINED(UNIX)}
  , uClassesEx
  {$ENDIF};

type
  TDriveIconList = class
    bmMediaFloppy,
    bmDriveHardDisk,
    bmMediaFlash,
    bmMediaOptical : TBitmap;
  end;

  { TPixMapManager }

  TPixMapManager = class
  
  private
    FExtList : TStringList;
    FPixmapList : TStringList;
    FDriveIconList : TStringList;
    FiDirIconID : PtrInt;
    FiDirLinkIconID : PtrInt;
    FiLinkIconID : PtrInt;
    FiUpDirIconID : PtrInt;
    FiDefaultIconID : PtrInt;
    FiExeIconID : PtrInt;
    FiArcIconID : PtrInt;
    FiSortAscID : PtrInt;
    FiSortDescID : PtrInt;
    FPixmapSize : String;
    {$IFDEF MSWINDOWS}
    FSysImgList : THandle;
    {$ELSE}
    FIconTheme: Pointer;
    {$ENDIF}
    {$IFDEF LCLGTK2}
    FPixbufList : TStringList;
    {$ENDIF}
  protected
    function LoadBitmap(AIconFileNAme: String; out ABitmap: TBitmap): Boolean;
    function CheckLoadPixmap(const sName : String; bUsePixmapPath : Boolean = True) : TBitmap;
    function CheckAddPixmap(const sName : String; bUsePixmapPath : Boolean = True):Integer;
  {$IF DEFINED(UNIX)}
    procedure CreateIconTheme;
    procedure DestroyIconTheme;
    function LoadIconThemeIcon(AFileExt, AIconName: String; AIconSize: Integer): Boolean;
    procedure LoadMimeIcons;
    function GetGenericIcons(const slGenericIcons: TStringListEx): Boolean;
  {$ENDIF}
    function GetBuiltInDriveIcon(Drive : PDrive; IconSize : Integer; clBackColor : TColor) : Graphics.TBitmap;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load(const sFileName : String);
    function GetBitmap(iIndex : Integer; BkColor : TColor) : TBitmap; // Always returns new copy.
//    function GetStretchBitmap(iIndex: Integer; BkColor : TColor; iSize : Integer): TBitmap;
    function DrawBitmap(iIndex: Integer; Canvas : TCanvas; Rect : TRect) : Boolean;
    function DrawBitmap(AFile: TColumnsViewFile; Canvas : TCanvas; Rect : TRect) : Boolean;
    function GetIconBySortingDirection(SortingDirection: TSortDirection): PtrInt;
    function GetIconByFile(AFile: TFile; DirectAccess: Boolean):PtrInt;
    function GetDriveIcon(Drive : PDrive; IconSize : Integer; clBackColor : TColor) : Graphics.TBitmap;
    function GetDefaultDriveIcon(IconSize : Integer; clBackColor : TColor) : Graphics.TBitmap;
  end;

function StretchBitmap(var bmBitmap : Graphics.TBitmap; iIconSize : Integer;
                       clBackColor : TColor; bFreeAtEnd : Boolean = False) : Graphics.TBitmap;
function LoadBitmapFromFile(sFileName : String; iIconSize : Integer; clBackColor : TColor) : Graphics.TBitmap;

var
  PixMapManager:TPixMapManager = nil;

procedure LoadPixMapManager;

implementation

uses
  GraphType, LCLIntf, LCLType, LCLProc, Forms, FileUtil, uGlobsPaths, uWCXhead,
  uGlobs, uDCUtils, uFileSystemFile
  {$IFDEF LCLGTK2}
    , gtkdef, gtk2, gdk2pixbuf, gdk2, glib2
  {$ENDIF}
  {$IFDEF MSWINDOWS}
    , CommCtrl, ShellAPI, Windows, uIcoFiles, uGdiPlus, IntfGraphics, uShlObjAdditional
  {$ELSE}
    , StrUtils
    {$IFNDEF LCLGTK2}
    , uIconTheme, uMyIconTheme
    {$ENDIF}
  {$ENDIF}
  ;

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

{$IFDEF LCLGTK2}
procedure DrawPixbufAtCanvas(Canvas: TCanvas; Pixbuf : PGdkPixbuf; SrcX, SrcY, DstX, DstY, Width, Height: Integer);
var
  gdkDrawable : PGdkDrawable;
  gdkGC : PGdkGC;
  gtkDC : TGtkDeviceContext;
begin
  gtkDC := TGtkDeviceContext(Canvas.Handle);
  gdkDrawable := gtkDC.Drawable;
  gdkGC := gdk_gc_new(gdkDrawable);
  gdk_draw_pixbuf(gdkDrawable, gdkGC, Pixbuf, SrcX, SrcY, DstX, DstY, Width, Height, GDK_RGB_DITHER_NONE, 0, 0);
  g_object_unref(gdkGC)
end;
{$ENDIF}

function StretchBitmap(var bmBitmap : Graphics.TBitmap; iIconSize : Integer;
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
        if bFreeAtEnd then
          FreeAndNil(bmBitmap);
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
  IntfImage: TLazIntfImage = nil;
{$ENDIF}
  AFile: TFileSystemFile;
  iIndex : Integer;
  sExtFilter,
  sGraphicFilter : String;
  bFreeAtEnd : Boolean;
  bmStandartBitmap : Graphics.TBitMap = nil;
  {$IFNDEF LCLGTK2}
  PNG : TPortableNetworkGraphic;
  Icon : TIcon = nil;
  {$ENDIF}
  {$IFDEF LCLGTK2}
  pbPicture : PGdkPixbuf;
  iPixbufWidth : Integer;
  iPixbufHeight : Integer;
  {$ENDIF}
begin
  sFileName:= mbExpandFileName(sFileName);
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
      ExtractIconExW(PWChar(UTF8Decode(sFileName)), iIconIndex, phiconLarge, phiconSmall, 1);
      case iIconSize of
        16, 32:
          try
            Result:= Graphics.TBitMap.Create;
            if iIconSize = 16 then
              Icon:= CreateIconFromHandle(phiconSmall)    // Small icon
            else
              Icon:= CreateIconFromHandle(phiconLarge);   // Large icon
            IntfImage := Icon.CreateIntfImage;
            Result.LoadFromIntfImage(IntfImage);
          finally
            if Assigned(Icon) then
              FreeAndNil(Icon);
            if Assigned(IntfImage) then
              FreeAndNil(IntfImage);
          end;
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
      bFreeAtEnd := True;
      sExtFilter := ExtractFileExt(sFileName) + ';';
      sGraphicFilter := GraphicFilter(TGraphic);
      // if file is graphic
      if (Pos(sExtFilter, sGraphicFilter) <> 0) and (mbFileExists(sFileName)) then
      begin
        {$IFDEF LCLGTK2}
        bmStandartBitmap := TBitMap.Create;
        pbPicture := gdk_pixbuf_new_from_file(PChar(sFileName), nil);
        if pbPicture <> nil then
        begin
          iPixbufWidth := gdk_pixbuf_get_width(pbPicture);
          iPixbufHeight := gdk_pixbuf_get_height(pbPicture);

          bmStandartBitmap.SetSize(iPixbufWidth, iPixbufHeight);
          bmStandartBitmap.Canvas.Brush.Color := clBackColor;
          bmStandartBitmap.Canvas.FillRect(0, 0, iPixbufWidth, iPixbufHeight);

          DrawPixbufAtCanvas(bmStandartBitmap.Canvas, pbPicture, 0, 0, 0, 0, iPixbufWidth, iPixbufHeight);
          gdk_pixmap_unref(pbPicture);
        end;
        {$ELSE}
        if CompareFileExt(sFileName, 'png', false) = 0 then
          begin
            PNG := TPortableNetworkGraphic.Create;
            try
              PNG.LoadFromFile(sFileName);
              bmStandartBitmap := Graphics.TBitmap.Create;
              bmStandartBitmap.Assign(PNG);
            finally
              FreeAndNil(PNG);
            end;
          end
        else if CompareFileExt(sFileName, 'ico', false) = 0 then
          begin
            Icon := TIcon.Create;
            try
              Icon.LoadFromFile(sFileName);
              bmStandartBitmap := Graphics.TBitmap.Create;
              bmStandartBitmap.Assign(Icon);
            finally
              FreeAndNil(Icon);
            end;
          end
        else
          begin
            bmStandartBitmap := Graphics.TBitMap.Create;
            bmStandartBitmap.LoadFromFile(sFileName);
          end
        {$ENDIF}
      end
      else // get file icon by ext
        begin
          if mbFileExists(sFileName) or mbDirectoryExists(sFileName) then
            begin
              AFile := TFileSystemFile.Create(sFileName);
              iIndex := PixMapManager.GetIconByFile(AFile, True);
              bmStandartBitmap := PixMapManager.GetBitmap(iIndex, clBackColor);
              FreeAndNil(AFile);
            end
          else  // file not found
            begin
              Exit(nil);
            end;
        end;
      // if need stretch icon
      if  (iIconSize <> bmStandartBitmap.Height) or (iIconSize <> bmStandartBitmap.Width) then
        Result := StretchBitmap(bmStandartBitmap, iIconSize, clBackColor, bFreeAtEnd)
      else
        Result := bmStandartBitmap;
    end;  // IsExecutable else
end;

{ TPixMapManager }

function TPixMapManager.LoadBitmap(AIconFileNAme: String; out ABitmap: Graphics.TBitmap): Boolean;
var
  pngBitmap: TPortableNetworkGraphic;
begin
  Result:= False;
  ABitmap:= nil;
  if CompareFileExt(AIconFileNAme, 'png', False) = 0 then
    begin
      pngBitmap:= TPortableNetworkGraphic.Create;
      try
        pngBitmap.LoadFromFile(AIconFileNAme);
        // if unsupported BitsPerPixel then exit
        if pngBitmap.RawImage.Description.BitsPerPixel > 32 then
          Exit;
        pngBitmap.Transparent:= True;
        ABitmap:= Graphics.TBitmap.Create;
        ABitmap.Assign(pngBitmap);
        Result:= True;
      finally
        FreeAndNil(pngBitmap);
      end;
    end
  else
    begin
      ABitmap:= Graphics.TBitMap.Create;
      ABitmap.LoadFromFile(AIconFileNAme);
      Result:= True;
    end;
end;

function TPixMapManager.CheckLoadPixmap(const sName: String; bUsePixmapPath : Boolean = True): Graphics.TBitmap;
var
  sFileName: String;
begin
  Result:= nil;

  if bUsePixmapPath then
    sFileName:= gpPixmapPath+FPixmapSize+sName
  else
    sFileName:= sName;
    
  if not mbFileExists(sFileName) then
    begin
      DebugLn(Format('Warning: pixmap [%s] not exists!',[sFileName]));
      Exit;
    end;
  LoadBitmap(sFileName, Result);
end;

function TPixMapManager.CheckAddPixmap(const sName: String; bUsePixmapPath : Boolean = True): Integer;
var
  sFileName : String;
  {$IFDEF LCLGTK2}
  pbPicture : PGdkPixbuf;
  {$ELSE}
  bmpBitmap: Graphics.TBitmap;
  {$ENDIF}
begin
  Result:= -1;
  
  if bUsePixmapPath then
    sFileName:= gpPixmapPath+FPixmapSize+sName
  else
    sFileName:= sName;
  
  if not mbFileExists(sFileName) then
    begin
      DebugLn(Format('Warning: pixmap [%s] not exists!',[sFileName]));
      Exit;
    end;
  // determine: known this file?
  {$IFDEF LCLGTK2}
  Result:= FPixbufList.IndexOf(sName);
  if Result < 0 then
  begin
    pbPicture := gdk_pixbuf_new_from_file(PChar(sFileName), nil);
    if pbPicture = nil then
    begin
      DebugLn(Format('Error: pixmap [%s] not loaded!', [sFileName]));
      Exit;
    end;
    Result := FPixbufList.AddObject(sName, TObject(pbPicture));
  end;
  {$ELSE}
  Result:= FPixmapList.IndexOf(sName);
  if Result < 0 then // no
    begin
      if LoadBitmap(sFileName, bmpBitmap) then
        Result:= FPixmapList.AddObject(sName, bmpBitmap); // add to list
    end;
  {$ENDIF}
end;

{$IF DEFINED(UNIX)}

procedure TPixMapManager.CreateIconTheme;
{$IFDEF LCLGTK2}
var
  GtkIconTheme: PGtkIconTheme;
begin
  // get current gtk theme
  GtkIconTheme:= gtk_icon_theme_get_for_screen(gdk_screen_get_default);
  { // load custom theme
  GtkIconTheme:= gtk_icon_theme_new;
  gtk_icon_theme_set_custom_theme(GtkIconTheme, 'oxygen');
  }
  FIconTheme:= GtkIconTheme;
end;
{$ELSE}
var
  IconTheme: TIconTheme;
begin
  FIconTheme:= nil;
  IconTheme:= TIconTheme.Create(GetCurrentIconTheme);
  if IconTheme.Load then
    FIconTheme:= Pointer(IconTheme);
end;
{$ENDIF}

procedure TPixMapManager.DestroyIconTheme;
{$IFDEF LCLGTK2}
begin
  FIconTheme:= nil;
end;
{$ELSE}
var
  IconTheme: TIconTheme;
begin
  IconTheme:= TIconTheme(FIconTheme);
  FreeThenNil(IconTheme);
end;
{$ENDIF}

function TPixMapManager.LoadIconThemeIcon(AFileExt, AIconName: String; AIconSize: Integer): Boolean;
{$IFDEF LCLGTK2}
var
  GtkIconTheme: PGtkIconTheme;
  pbPicture: PGdkPixbuf;
  pgcIconName: Pgchar;
  iPixMap: PtrInt;
begin
  Result:= False;
  GtkIconTheme:= PGtkIconTheme(FIconTheme);
  pgcIconName:= Pgchar(AIconName);
  pbPicture:= gtk_icon_theme_load_icon(GtkIconTheme, pgcIconName, AIconSize, GTK_ICON_LOOKUP_NO_SVG, nil);
  if pbPicture <> nil then
    begin
     iPixMap:= FPixbufList.AddObject(AFileExt, TObject(pbPicture));
     FExtList.AddObject(AFileExt, TObject(iPixMap));
     Result:= True;
    end;
end;
{$ELSE}
var
  IconTheme: TIconTheme;
  sIconFileName: String;
  bmpBitmap: TBitmap;
  iPixMap: PtrInt;
begin
  Result:= False;
  IconTheme:= TIconTheme(FIconTheme);
  sIconFileName:= IconTheme.FindIcon(AIconName, AIconSize);
  if sIconFileName <> EmptyStr then
  begin
    iPixMap:= FPixmapList.IndexOf(sIconFileName);
    if (iPixMap >= 0) then
      begin
        FExtList.AddObject(AFileExt, TObject(iPixMap));
        Result:= True;
      end
    else if LoadBitmap(sIconFileName, bmpBitmap) then
      begin
        iPixMap:= FPixmapList.AddObject(sIconFileName, bmpBitmap);
        FExtList.AddObject(AFileExt, TObject(iPixMap));
        Result:= True;
      end;
  end;
end;
{$ENDIF}

procedure TPixMapManager.LoadMimeIcons;
const
  // From update-mime-database.c
  media_types: array[0..10] of String = (
        'text', 'application', 'image', 'audio',
        'inode', 'video', 'message', 'model', 'multipart',
        'x-content', 'x-epoc');
var
  slGenericIcons: TStringListEx;
  I, J: Integer;
  iPixMap: PtrInt;
  scIconName: String;
  sExt, sIconName: String;
  bResult: Boolean;
begin
  slGenericIcons:= TStringListEx.Create;
  //slGenericIcons.LoadFromFile(gpIniDir + 'mimetypes.txt');
  if GetGenericIcons(slGenericIcons) then
    begin
      // get current icon theme
      CreateIconTheme;
      // load theme icons
      for I:= 0 to slGenericIcons.Count - 1 do
        begin
          sExt:= slGenericIcons.Names[I];
          if FExtList.IndexOf(sExt) >= 0 then Continue;
          sIconName:= slGenericIcons.ValueFromIndex[I];
          // try to load mime icon
          scIconName:= Copy2SymbDel(sIconName, ':');
          bResult:= LoadIconThemeIcon(sExt, scIconName, gIconsSize);
          // if icon not found then try to load generic icon
          if (bResult = False) and (sIconName <> '') then
            begin
              scIconName:= sIconName;
              bResult:= LoadIconThemeIcon(sExt, scIconName, gIconsSize);
            end;
          // Shared-mime-info spec says:
          // "If [generic-icon] is not specified then the mimetype is used to generate the
          // generic icon by using the top-level media type (e.g. "video" in "video/ogg")
          // and appending "-x-generic" (i.e. "video-x-generic" in the previous example)."
          if bResult = False then
            begin
              sIconName:= slGenericIcons.ValueFromIndex[I];
              for J:= Low(media_types) to High(media_types) do
                begin
                  if Pos(media_types[J], sIconName) = 1 then
                    begin
                      scIconName:= media_types[J] + '-x-generic';
                      bResult:= LoadIconThemeIcon(sExt, scIconName, gIconsSize);
                      if bResult = True then Break;
                    end; // if
                end; // for
            end; // if
        end;
    end;
  slGenericIcons.Free;
end;

function TPixMapManager.GetGenericIcons(const slGenericIcons: TStringListEx): Boolean;
const
  mime_globs = '/usr/share/mime/globs';
  mime_generic_icons = '/usr/share/mime/generic-icons';
var
  globs,
  generic_icons: TStringListEx;
  I: Integer;
  sMimeIconName,
  sGenericIconName: String;
begin
  try
    Result:= False;
    globs:= nil;
    generic_icons:= nil;
    // load mime types list
    globs:= TStringListEx.Create;
    globs.NameValueSeparator:= ':';
    globs.LoadFromFile(mime_globs);
    // try to load generic icons list
    if mbFileExists(mime_generic_icons) then
      begin
        generic_icons:= TStringListEx.Create;
        generic_icons.NameValueSeparator:= ':';
        generic_icons.LoadFromFile(mime_generic_icons);
      end;
    // fill icons list (format "extension=mimeiconname:genericiconname")
    if Assigned(generic_icons) then
      for I:= 0 to globs.Count - 1 do
        begin
          sGenericIconName:= generic_icons.Values[globs.Names[I]];
          sMimeIconName:= StringReplace(globs.Names[I], '/', '-', []);
          slGenericIcons.Add(PChar(globs.ValueFromIndex[I])+2 + '=' + sMimeIconName + ':' + sGenericIconName);
        end
    else
      for I:= 0 to globs.Count - 1 do
        begin
          sMimeIconName:= StringReplace(globs.Names[I], '/', '-', []);
          slGenericIcons.Add(PChar(globs.ValueFromIndex[I])+2 + '=' + sMimeIconName);
        end;
    Result:= True;
  finally
    if Assigned(globs) then
      FreeAndNil(globs);
    if Assigned(generic_icons) then
      FreeAndNil(generic_icons);
  end;
end;

{$ENDIF} // Unix

constructor TPixMapManager.Create;
{$IFDEF MSWINDOWS}
var
  FileInfo : TSHFileInfoW;
  iIconSize : Integer;
{$ENDIF}
begin
  FExtList:=TStringList.Create;
  FPixmapList:=TStringList.Create;
  FDriveIconList:= TStringList.Create;

  {$IFDEF LCLGTK2}
  FPixbufList := TStringList.Create;
  {$ENDIF}

  {$IFDEF MSWINDOWS}
    if gIconsSize = 16 then
      iIconSize := SHGFI_SMALLICON
    else
      iIconSize := SHGFI_LARGEICON;
      
    FSysImgList := SHGetFileInfoW(PWideChar(UTF8Decode(mbGetCurrentDir)),
                           0,
                           FileInfo,
                           SizeOf(FileInfo),
                           SHGFI_SYSICONINDEX or iIconSize);
  {$ENDIF}
end;

destructor TPixMapManager.Destroy;
var
  I : Integer;
begin
  if Assigned(FPixmapList) then
  begin
    for I := 0 to FPixmapList.Count - 1 do
      if Assigned(FPixmapList.Objects[I]) then
        Graphics.TBitmap(FPixmapList.Objects[I]).Free;
    FreeAndNil(FPixmapList);
  end;

  if Assigned(FExtList) then
    FreeAndNil(FExtList);

  if Assigned(FDriveIconList) then
  begin
    for I := 0 to FDriveIconList.Count - 1 do
      with FDriveIconList.Objects[I] as TDriveIconList do
      begin
        if Assigned(bmMediaFloppy) then FreeAndNil(bmMediaFloppy);
        if Assigned(bmDriveHardDisk) then FreeAndNil(bmDriveHardDisk);
        if Assigned(bmMediaFlash) then FreeAndNil(bmMediaFlash);
        if Assigned(bmMediaOptical) then FreeAndNil(bmMediaOptical);
        Free;
      end;
    FreeAndNil(FDriveIconList);
  end;

  {$IFDEF LCLGTK2}
  if Assigned(FPixbufList) then
  begin
    for I := 0 to FPixbufList.Count - 1 do
      g_object_unref(PGdkPixbuf(FPixbufList.Objects[I]));

    FreeAndNil(FPixbufList);
  end;
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  ImageList_Destroy(FSysImgList);
  {$ELSE}
  DestroyIconTheme;
  {$ENDIF}
  inherited Destroy;
end;

procedure TPixMapManager.Load(const sFileName: String);
var
  slPixmapList: TStringList;
  s:String;
  sExt, sPixMap:String;
  iekv:integer;
  iPixMap:PtrInt;
  sPixMapSize : String;
  I : Integer;
  Plugins : TStringList;
  sCurrentPlugin : String;
  iCurPlugCaps : Integer;
begin
  //  load all drive icons
  FDriveIconList.AddObject('16x16', TDriveIconList.Create);
  FDriveIconList.AddObject('22x22', TDriveIconList.Create);
  FDriveIconList.AddObject('32x32', TDriveIconList.Create);

  sPixMapSize := FPixmapSize;  // save icon size path
  for I:= 0 to FDriveIconList.Count - 1 do
    with FDriveIconList.Objects[I] as TDriveIconList do
    begin
      FPixmapSize := FDriveIconList.Strings[I] + PathDelim;
      bmMediaFloppy := CheckLoadPixmap('devices' + PathDelim + 'media-floppy.png');
      bmDriveHardDisk := CheckLoadPixmap('devices' + PathDelim + 'drive-harddisk.png');
      bmMediaFlash := CheckLoadPixmap('devices' + PathDelim + 'media-flash.png');
      bmMediaOptical := CheckLoadPixmap('devices' + PathDelim + 'media-optical.png');
    end;
  FPixmapSize := sPixMapSize;  // restore icon size path

  // add some standard icons
  FiDefaultIconID:=CheckAddPixmap('mimetypes' + PathDelim + 'unknown.png');
  FiDirIconID:=CheckAddPixmap('filesystems' + PathDelim + 'folder.png');
  FiDirLinkIconID:=CheckAddPixmap('filesystems' + PathDelim + 'folder-link.png');
  FiLinkIconID:=CheckAddPixmap('filesystems' + PathDelim + 'link.png');
  FiUpDirIconID:=CheckAddPixmap('actions' + PathDelim + 'go-up.png');
  FiArcIconID := CheckAddPixmap('mimetypes' + PathDelim + 'package-x-generic.png');
  FiExeIconID:= CheckAddPixmap('mimetypes' + PathDelim + 'application-x-executable.png');
  FiSortAscID := CheckAddPixmap('actions' + PathDelim + 'view-sort-ascending.png');
  FiSortDescID := CheckAddPixmap('actions' + PathDelim + 'view-sort-descending.png');

  { Load icons from doublecmd.ext }
  for I := 0 to gExts.Count - 1 do
    begin
      sPixMap := gExts.Items[I].Icon;
      if mbFileExists(sPixMap) then
        begin
          iPixMap:= CheckAddPixmap(sPixMap, False);
          if iPixMap < 0 then Continue;
          gExts.Items[I].IconIndex:= iPixMap;
          //DebugLn('sPixMap = ',sPixMap, ' Index = ', IntToStr(iPixMap));

          // set pixmap index for all extensions
          for iekv := 0 to gExts.Items[I].Extensions.Count - 1 do
            begin
              sExt := gExts.Items[I].Extensions[iekv];
              if FExtList.IndexOf(sExt) < 0 then
                FExtList.AddObject(sExt, TObject(iPixMap));
            end;
        end;
    end;
  {/ Load icons from doublecmd.ext }  
  
  if FileExists(sFileName) then
  begin
    slPixmapList:= TStringList.Create;
    slPixmapList.LoadFromFile(sFileName);
    try
      for I:= 0 to slPixmapList.Count - 1 do
      begin
        s:= slPixmapList.Strings[I];
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
      slPixmapList.Free;
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

  {$IF DEFINED(UNIX)}
  if (gShowIcons <> sim_none) and (gShowIcons <> sim_standart) then
    LoadMimeIcons;
  {$ENDIF}
end;

function TPixMapManager.GetBitmap(iIndex: Integer; BkColor : TColor): Graphics.TBitmap;
{$IFDEF MSWINDOWS}
var
  memstream: TMemoryStream;
{$ENDIF}
begin
  if iIndex<FPixmapList.Count then
  begin
    // Make a new copy.
    Result := Graphics.TBitmap.Create;
    Result.Assign(Graphics.TBitmap(FPixmapList.Objects[iIndex]));
  end
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
    ImageList_DrawEx(FSysImgList, iIndex - $1000, Result.Canvas.Handle, 0, 0, 0, 0, GetRGBColor(BkColor), clNone, ILD_NORMAL);
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

{
function TPixMapManager.GetStretchBitmap(iIndex: Integer; BkColor: TColor;
  iSize: Integer): Graphics.TBitmap;
var
  BitmapTmp: TBitmap;
begin
  Result := Graphics.TBitMap.Create;
  with Result do
  begin
    Width := iSize;
    Height := iSize;

    Canvas.Brush.Color := BkColor;
    Canvas.FillRect(Canvas.ClipRect);
    BitmapTmp := GetBitmap(iIndex, BkColor);
    Canvas.StretchDraw(Canvas.ClipRect, BitmapTmp);
    FreeAndNil(BitmapTmp);
  end;
end;
}

function TPixMapManager.DrawBitmap(iIndex: Integer; Canvas: TCanvas; Rect: TRect): Boolean;
  {$IFDEF MSWINDOWS}
var
  hicn: HICON;
  {$ENDIF}

  {$IFDEF LCLGTK2}
var
  pbPicture : PGdkPixbuf;
  iPixbufWidth : Integer;
  iPixbufHeight : Integer;
  {$ENDIF}
begin
  Result := True;
  {$IFDEF LCLGTK2}
  if iIndex < FPixbufList.Count then
  begin
    pbPicture := PGdkPixbuf(FPixbufList.Objects[iIndex]);
    iPixbufWidth :=  gdk_pixbuf_get_width(pbPicture);
    iPixbufHeight :=  gdk_pixbuf_get_height(pbPicture);

    DrawPixbufAtCanvas(Canvas, pbPicture, 0, 0, Rect.Left, Rect.Top, iPixbufWidth, iPixbufHeight);
  end
  else
  {$ELSE}
  if iIndex < FPixmapList.Count then
    Canvas.Draw(Rect.Left, Rect.Top ,Graphics.TBitmap(FPixmapList.Objects[iIndex]))
  else
  {$ENDIF}
{$IFDEF MSWINDOWS}
  if iIndex >= $1000 then
    try
      if gIconsSize in [16, 32] then
        // for transparent
        ImageList_Draw(FSysImgList, iIndex - $1000, Canvas.Handle, Rect.Left, Rect.Top, ILD_TRANSPARENT)
      else
        try
          hicn:= ImageList_GetIcon(FSysImgList, iIndex - $1000, 0);
          if IsGdiPlusLoaded then
            Result:= GdiPlusStretchDraw(hicn, Canvas.Handle, Rect.Left, Rect.Top, gIconsSize, gIconsSize)
          else
            Result:= DrawIconEx(Canvas.Handle, Rect.Left, Rect.Top, hicn, gIconsSize, gIconsSize, 0, 0, DI_NORMAL);
        finally
          DestroyIcon(hicn);
        end;
    except
      Result:= False;
    end;

{$ELSE}
    Result:= False;
{$ENDIF}
end;

function TPixMapManager.DrawBitmap(AFile: TColumnsViewFile; Canvas: TCanvas; Rect: TRect): Boolean;
var
  I: Integer;
begin
  Result:= DrawBitmap(AFile.IconID, Canvas, Rect);
  {$IFDEF MSWINDOWS}
  if gIconOverlays then
    begin
      I:= SHGetOverlayIconIndex(AFile.TheFile.Path + AFile.TheFile.Name);
      if I >= 0 then
        Result:= DrawBitmap(I + $1000, Canvas, Rect);
    end;
  {$ENDIF}
end;

function TPixMapManager.GetIconBySortingDirection(SortingDirection: TSortDirection): PtrInt;
begin
  case SortingDirection of
    sdDescending:
      begin
        Result := FiSortDescID;
      end;
    sdAscending:
      begin
        Result := FiSortAscID;
      end;
  end;
end;

function TPixMapManager.GetIconByFile(AFile: TFile; DirectAccess: Boolean): PtrInt;
var
  Ext: String;
  sFileName: String;
{$IFDEF MSWINDOWS}
    FileInfo: TSHFileInfoW;
    _para2: DWORD;
    _para5: UINT;
{$ENDIF}
begin
  Result := -1;
  if not Assigned(AFile) then Exit;

  with AFile do
  begin
//    writeln(sExt);
    if Name = '..' then
    begin
      Result := FiUpDirIconID;
      Exit;
    end;

    if IsLinkToDirectory then
    begin
      Result := FiDirLinkIconID;
      Exit;
    end;

    if IsDirectory then
      {$IFDEF MSWINDOWS}
      if (gShowIcons = sim_standart) or
         ((not mbFileExists(Path + Name + '\desktop.ini')) and
         (GetDeviceCaps(Application.MainForm.Canvas.Handle, BITSPIXEL) > 16)) then
      {$ENDIF}
    begin
      Result := FiDirIconID;
      Exit;
    end;

    if IsLink then
    begin
      Result := FiLinkIconID;
      Exit;
    end;

    if (Extension = '') and (not IsDirectory) then
    begin
      Result := FiDefaultIconID;
      Exit;
    end;

    Ext := UTF8LowerCase(Extension);

    {$IFDEF MSWINDOWS}
    if gShowIcons <> sim_all_and_exe then
      begin
        if Ext = 'exe' then
          Exit(FiExeIconID)
        else if Ext = 'lnk' then
          Exit(FiLinkIconID)
        else if Ext = 'ico' then
          Exit(FiDefaultIconID)
      end;
    {$ENDIF}

    Result:= FExtList.IndexOf(Ext);
    if Result < 0 then
    begin
    {$IFDEF MSWINDOWS}

    if gShowIcons = sim_standart then
      Exit(FiDefaultIconID);

    if DirectAccess then
      begin
        _para2 := 0;
        _para5 := SHGFI_SYSICONINDEX;
        sFileName := Path + Name;
      end
    else
      begin
        _para2 := FILE_ATTRIBUTE_NORMAL;
        _para5 := SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES;
        sFileName := Name;
      end;

    if gIconsSize = 16 then
      _para5 := _para5 or SHGFI_SMALLICON
    else
      _para5 := _para5 or SHGFI_LARGEICON;

    //WriteLN('Icon for file == ' + sName);
          
    SHGetFileInfoW(PWideChar(UTF8Decode(sFileName)),
                           _para2,
                           FileInfo,
                           SizeOf(FileInfo),
                           _para5);
       Result := FileInfo.iIcon + $1000;
       
       //WriteLN('FileInfo.iIcon == ' + IntToStr(FileInfo.iIcon));
       
       if (FExtList.IndexOf(Ext) < 0) and (Ext <> 'exe') and
          (Ext <> 'ico') and (Ext <> 'lnk') and (not IsDirectory) then
        FExtList.AddObject(Ext, TObject(Result));
    {$ELSE}
      Result := FiDefaultIconID;
    {$ENDIF}
      Exit;
    end;
    Result := PtrInt(FExtList.Objects[Result]);
//    writeln(Result);
  end;
end;

function TPixMapManager.GetDriveIcon(Drive : PDrive; IconSize : Integer; clBackColor : TColor) : Graphics.TBitmap;
{$IFDEF MSWINDOWS}
var
  SFI: TSHFileInfo;
  Icon: TIcon = nil;
  IntfImage: TLazIntfImage = nil;
  _para5 : UINT;
{$ENDIF}
begin
  Result := nil;
{$IFDEF MSWINDOWS}
  if GetDeviceCaps(Application.MainForm.Canvas.Handle, BITSPIXEL) < 15 then Exit;
  if (not gCustomDriveIcons) and (GetDeviceCaps(Application.MainForm.Canvas.Handle, BITSPIXEL) > 16) then
    begin
      SHGetFileInfo(PChar(Drive^.Path), 0, SFI, SizeOf(SFI), SHGFI_ICON);
      SFI.hIcon := 0;
      Result := Graphics.TBitMap.Create;
      case IconSize of
      16, 32: // Standart icon size
        begin
          if IconSize = 16 then
            _para5 := SHGFI_SMALLICON
          else
            _para5 := SHGFI_LARGEICON;

          SHGetFileInfo(PChar(Drive^.Path), 0, SFI, SizeOf(SFI), _para5 or SHGFI_ICON);

          if SFI.hIcon <> 0 then
            try
              Icon := CreateIconFromHandle(SFI.hIcon);
              IntfImage := Icon.CreateIntfImage;
              Result.LoadFromIntfImage(IntfImage);
            finally
              if Assigned(Icon) then
                FreeAndNil(Icon);
              if Assigned(IntfImage) then
                FreeAndNil(IntfImage);
            end;
        end;
      else  // for non standart icon size we Convert HIcon to TBitMap
        begin
          SHGetFileInfo(PChar(Drive^.Path), 0, SFI, SizeOf(SFI), SHGFI_ICON);
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
      Result := GetBuiltInDriveIcon(Drive, IconSize, clBackColor);
    end;
end;

function TPixMapManager.GetBuiltInDriveIcon(Drive : PDrive; IconSize : Integer; clBackColor : TColor) : Graphics.TBitmap;
var
  DriveIconListIndex: Integer;
  Bitmap: Graphics.TBitmap;
begin
{$IFDEF MSWINDOWS}
  if GetDeviceCaps(Application.MainForm.Canvas.Handle, BITSPIXEL) < 15 then Exit;
{$ENDIF}
  case IconSize of
  16: // Standart 16x16 icon size
    DriveIconListIndex := 0;
  22:  // Standart 22x22 icon size
    DriveIconListIndex := 1;
  32:  // Standart 32x32 icon size
    DriveIconListIndex := 2;
  else  // for non standart icon size use more large icon for stretch
    DriveIconListIndex := 2;
  end;
  with FDriveIconList.Objects[DriveIconListIndex] as TDriveIconList do
  case Drive^.DriveType of
  dtFloppy:
    Bitmap := bmMediaFloppy;
  dtFixed:
    Bitmap := bmDriveHardDisk;
  dtFlash:
    Bitmap := bmMediaFlash;
  dtCDROM:
    Bitmap := bmMediaOptical;
  else
    Bitmap := bmDriveHardDisk;
  end;
  //  if need stretch icon
  if (IconSize <> 16) and (IconSize <> 22) and (IconSize <> 32) then
    begin
      Result := StretchBitmap(Bitmap, IconSize, clBackColor, False);
    end
  else
    begin
      Result := Graphics.TBitmap.Create;
      Result.Assign(Bitmap);
    end;
  // 'Bitmap' should not be freed, because it only points to DriveIconList.
end;

function TPixMapManager.GetDefaultDriveIcon(IconSize : Integer; clBackColor : TColor) : Graphics.TBitmap;
var
  Drive: TDrive = (Name: ''; Path: ''; DriveLabel: ''; DriveType: dtFixed);
begin
  Result := GetBuiltInDriveIcon(@Drive, IconSize, clBackColor);
end;

procedure LoadPixMapManager;
begin
  PixMapManager:=TPixMapManager.Create;
  PixMapManager.FPixmapSize:= IntToStr(gIconsSize) + 'x' + IntToStr(gIconsSize) + PathDelim;
  PixMapManager.Load(gpIniDir+'pixmaps.txt');
end;

initialization

finalization

  if Assigned(PixMapManager) then
    FreeAndNil(PixMapManager);

end.

