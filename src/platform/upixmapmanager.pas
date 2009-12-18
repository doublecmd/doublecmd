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

{
  GTK2 is used directly in PixmapManager, because FPC/Lazarus draws bitmaps
  without alpha channel under GTK2, so bitmaps looks ugly.
  If this problem will be fixed then GTK2 specific code could be dropped.
}

uses
  Classes, SysUtils, Graphics, uOSUtils, uFileSorting, StringHashList,
  uFile
  {$IF DEFINED(UNIX)}
  , uClassesEx
    {$IF NOT DEFINED(DARWIN)}
    , contnrs
      {$IFDEF LCLGTK2}
      , gtk2
      {$ELSE}
      , uIconTheme, uMyIconTheme
      {$ENDIF}
    {$ENDIF}
  {$ENDIF};

type
  TDriveIconList = class
    bmMediaFloppy,
    bmDriveHardDisk,
    bmMediaFlash,
    bmMediaOptical,
    bmDriveNetwork: TBitmap;
  end;

  { TPixMapManager }

  TPixMapManager = class
  
  private
    {en
       Maps file extension to index of bitmap (in FPixmapList) for this file extension.
    }
    FExtList : TStringHashList;
    {en
       Maps icon filename to index of bitmap (in FPixmapList) for this icon.
       Uses absolute file names.
    }
    FPixmapsFileNames : TStringHashList;
    {en
       A list of loaded bitmaps.
       Stores TBitmap objects (on GTK2 it stores PGdkPixbuf pointers).
    }
    FPixmapList : TFPList;
    FDriveIconList : TStringList;
    FiDirIconID : PtrInt;
    FiDirLinkIconID : PtrInt;
    FiLinkIconID : PtrInt;
    FiEmblemLinkID: PtrInt;
    FiUpDirIconID : PtrInt;
    FiDefaultIconID : PtrInt;
    FiExeIconID : PtrInt;
    FiArcIconID : PtrInt;
    FiSortAscID : PtrInt;
    FiSortDescID : PtrInt;
    FPixmapSize : String;
    {$IF DEFINED(MSWINDOWS)}
    FSysImgList : THandle;
    {$ELSEIF DEFINED(UNIX) AND NOT DEFINED(DARWIN)}
    {en
       Maps file extension to MIME icon name(s).
    }
    FExtToMimeIconName: TFPDataHashTable;
    {en
       Maps mime icon name to index of bitmap (in FPixmapList) for this icon.
    }
    FThemePixmapsFileNames: TStringHashList;
    {$IFDEF LCLGTK2}
    FIconTheme: PGtkIconTheme;
    {$ELSE}
    FIconTheme: TIconTheme;
    {$ENDIF}
    {$ENDIF}
  protected
    function LoadBitmap(AIconFileName: String; out ABitmap: TBitmap): Boolean;
    function CheckLoadPixmap(const AIconName: String; bUsePixmapPath : Boolean = True) : TBitmap;
    function CheckAddPixmap(AIconName: String; IconSize : Integer = 0; bUsePixmapPath : Boolean = True): PtrInt;
  {$IF DEFINED(UNIX) AND NOT DEFINED(DARWIN)}
    procedure CreateIconTheme;
    procedure DestroyIconTheme;
    procedure LoadMimeIconNames;
    function LoadIconThemeIcon(AIconName: String; AIconSize: Integer): PtrInt;
    function GetMimeIcon(AFileExt: String; AIconSize: Integer): PtrInt;
    function GetIconByDesktopFile(sFileName: UTF8String; iDefaultIcon: PtrInt): PtrInt;
  {$ENDIF}
    function GetBuiltInDriveIcon(Drive : PDrive; IconSize : Integer; clBackColor : TColor) : Graphics.TBitmap;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load(const sFileName : String);
    function GetBitmap(iIndex : PtrInt; BkColor : TColor) : TBitmap; // Always returns new copy.
    function DrawBitmap(iIndex: PtrInt; Canvas : TCanvas; X, Y: Integer) : Boolean;
    {en
       Draws bitmap stretching it if needed to Width x Height.
       If Width is 0 then full bitmap width is used.
       If Height is 0 then full bitmap height is used.
       @param(iIndex
              Index of pixmap manager's bitmap.)
    }
    function DrawBitmap(iIndex: PtrInt; Canvas : TCanvas; X, Y, Width, Height: Integer) : Boolean;
    function DrawBitmap(iIndex: PtrInt; AFile: TFile; DirectAccess: Boolean; Canvas : TCanvas; X, Y: Integer) : Boolean;
    function GetIconBySortingDirection(SortingDirection: TSortDirection): PtrInt;
    function GetIconByFile(AFile: TFile; DirectAccess: Boolean): PtrInt;
    function GetIconByName(const AIconName: UTF8String): PtrInt;
    function GetDriveIcon(Drive : PDrive; IconSize : Integer; clBackColor : TColor) : Graphics.TBitmap;
    function GetDefaultDriveIcon(IconSize : Integer; clBackColor : TColor) : Graphics.TBitmap;
    function GetArchiveIcon(IconSize: Integer; clBackColor : TColor) : Graphics.TBitmap;
  end;

function StretchBitmap(var bmBitmap : Graphics.TBitmap; iIconSize : Integer;
                       clBackColor : TColor; bFreeAtEnd : Boolean = False) : Graphics.TBitmap;
function LoadBitmapFromFile(sFileName : String; iIconSize : Integer; clBackColor : TColor) : Graphics.TBitmap;

var
  PixMapManager:TPixMapManager = nil;

procedure LoadPixMapManager;

implementation

uses
  GraphType, LCLIntf, LCLType, LCLProc, Forms, FileUtil, uGlobsPaths, WcxPlugin,
  uGlobs, uDCUtils, uFileSystemFile
  {$IFDEF LCLGTK2}
    , uPixMapGtk, gtkdef, gdk2pixbuf, gdk2, glib2
  {$ENDIF}
  {$IFDEF MSWINDOWS}
    , CommCtrl, ShellAPI, Windows, uIcoFiles, uGdiPlus, IntfGraphics, uShlObjAdditional
  {$ELSE}
    , StrUtils
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

function StretchBitmap(var bmBitmap : Graphics.TBitmap; iIconSize : Integer;
                       clBackColor : TColor; bFreeAtEnd : Boolean = False) : Graphics.TBitmap;
var
  memstream: TMemoryStream;
begin
  Result := Graphics.TBitMap.Create;
    with Result do
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
  Icon : TIcon = nil;
{$ENDIF}
  AFile: TFileSystemFile;
  iIndex : PtrInt;
  sExtFilter,
  sGraphicFilter : String;
  bFreeAtEnd : Boolean;
  bmStandartBitmap : Graphics.TBitMap = nil;
  {$IFDEF LCLGTK2}
  pbPicture : PGdkPixbuf;
  {$ENDIF}
begin
  Result := nil;

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
      sExtFilter := UTF8LowerCase(ExtractFileExt(sFileName)) + ';';
      sGraphicFilter := GraphicFilter(TGraphic);
      // if file is graphic
      if (Pos(sExtFilter, sGraphicFilter) <> 0) and mbFileExists(sFileName) then
      begin
        {$IFDEF LCLGTK2}
        pbPicture := gdk_pixbuf_new_from_file(PChar(sFileName), nil);
        if pbPicture <> nil then
        begin
          bmStandartBitmap:= PixBufToBitmap(pbPicture);
          gdk_pixmap_unref(pbPicture);
        end
        else // Try loading the standard way.
        {$ELSE}
        if not PixMapManager.LoadBitmap(sFileName, bmStandartBitmap) then
          Exit;
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

function TPixMapManager.LoadBitmap(AIconFileName: String; out ABitmap: Graphics.TBitmap): Boolean;
var
  Picture: TPicture;
begin
  Result:= False;
  ABitmap:= nil;
  try
    Picture := TPicture.Create;
    try
      Picture.LoadFromFile(AIconFileName);
      //Picture.Graphic.Transparent := True;

      ABitmap := Graphics.TBitmap.Create;
      ABitmap.Assign(Picture.Bitmap);

      // if unsupported BitsPerPixel then exit
      if ABitmap.RawImage.Description.BitsPerPixel > 32 then
        raise EInvalidGraphic.Create('Unsupported bits per pixel');

      Result:= True;
    finally
      FreeAndNil(Picture);
    end;
  except
    on e: Exception do
      begin
        if Assigned(ABitmap) then
          FreeAndNil(ABitmap);
        DebugLn(Format('Error: Cannot load pixmap [%s] : %s',[AIconFileName, e.Message]));
      end;
  end;
end;

function TPixMapManager.CheckLoadPixmap(const AIconName: String; bUsePixmapPath : Boolean = True): Graphics.TBitmap;
var
  sFileName: String;
begin
  Result:= nil;

  if bUsePixmapPath then
    sFileName:= gpPixmapPath + FPixmapSize + AIconName
  else
    sFileName:= AIconName;
    
  if not mbFileExists(sFileName) then
    begin
      DebugLn(Format('Warning: pixmap [%s] not exists!',[sFileName]));
      Exit;
    end;
  LoadBitmap(sFileName, Result);
end;

function TPixMapManager.CheckAddPixmap(AIconName: String; IconSize : Integer; bUsePixmapPath : Boolean): PtrInt;
var
  fileIndex: PtrInt;
  {$IFDEF LCLGTK2}
  pbPicture : PGdkPixbuf;
  {$ELSE}
  bmpBitmap: Graphics.TBitmap;
  {$ENDIF}
begin
  Result:= -1;
  if AIconName = EmptyStr then Exit;

  if IconSize = 0 then IconSize:= gIconsSize;

  if bUsePixmapPath or (GetPathType(AIconName) = ptAbsolute) then
    begin
      if bUsePixmapPath then
        AIconName := gpPixmapPath + FPixmapSize + AIconName;

      // Determine if this file is already loaded.
      fileIndex := FPixmapsFileNames.Find(AIconName);
      if fileIndex < 0 then
        begin
          if not mbFileExists(AIconName) then
            begin
              DebugLn(Format('Warning: pixmap [%s] not exists!', [AIconName]));
              Exit;
            end;
      {$IFDEF LCLGTK2}
          pbPicture := gdk_pixbuf_new_from_file_at_size(PChar(AIconName), IconSize, IconSize, nil);
          if Assigned(pbPicture) then
            begin
              Result := FPixmapList.Add(pbPicture);
              FPixmapsFileNames.Add(AIconName, Pointer(Result));
            end
          else
            DebugLn(Format('Error: pixmap [%s] not loaded!', [AIconName]));
      {$ELSE}
          if LoadBitmap(AIconName, bmpBitmap) then
          begin
            // Shrink big bitmaps before putting them into PixmapManager,
            // to speed up later drawing.
            //
            // Note: Transparent bitmaps may lose transparency, because
            // they must drawn onto a background, so we allow smaller bitmaps
            // up to 48x48 (icons for example) to load in full size and they
            // are resized upon drawing.
            //
            // TODO:
            // This should resize any non-transparent,
            // non-alpha channel bitmaps to gIconsSize
            // (so if Width<>gIconsSize or Height<>gIconsSize then Resize).
            if (bmpBitmap.Width > 48) or (bmpBitmap.Height > 48) then
            begin
              bmpBitmap := StretchBitmap(bmpBitmap, IconSize, clBlack, True);
            end;
            Result := FPixmapList.Add(bmpBitmap);
            FPixmapsFileNames.Add(AIconName, Pointer(Result));
          end;
      {$ENDIF}
        end
      else
        begin
          Result:= PtrInt(FPixmapsFileNames.List[fileIndex]^.Data);
        end;
    end
{$IF DEFINED(UNIX) and NOT DEFINED(DARWIN)}
  else  // Load theme icon
    begin
      Result := LoadIconThemeIcon(AIconName, IconSize);
    end;
{$ENDIF}
end;

{$IF DEFINED(UNIX) AND NOT DEFINED(DARWIN)}

procedure TPixMapManager.CreateIconTheme;
{$IFDEF LCLGTK2}
begin
  // get current gtk theme
  FIconTheme:= gtk_icon_theme_get_for_screen(gdk_screen_get_default);
  { // load custom theme
  FIconTheme:= gtk_icon_theme_new;
  gtk_icon_theme_set_custom_theme(FIconTheme, 'oxygen');
  }
end;
{$ELSE}
begin
  FIconTheme:= TIconTheme.Create(GetCurrentIconTheme);
end;
{$ENDIF}

procedure TPixMapManager.DestroyIconTheme;
{$IFDEF LCLGTK2}
begin
  FIconTheme:= nil;
end;
{$ELSE}
begin
  if Assigned(FIconTheme) then
    FreeAndNil(FIconTheme);
end;
{$ENDIF}

procedure TPixMapManager.LoadMimeIconNames;
const
  mime_globs = '/usr/share/mime/globs';
  mime_generic_icons = '/usr/share/mime/generic-icons';
  pixmaps_cache = 'pixmaps.cache';
  cache_signature: DWord = $44435043; // 'DCPC'
  cache_version: DWord = 1;
var
  I, J, K: Integer;
  globs: TStringListEx = nil;
  generic_icons: TStringListEx = nil;
  cache: TFileStreamEx = nil;
  mTime: LongInt;
  sMimeType,
  sMimeIconName,
  sExtension: String;
  node: THTDataNode = nil;
  iconsList: TStringList;
  EntriesCount, IconsCount: Cardinal;
  nodeList: TFPObjectList;
begin
  try
    // Try to load from cache.
    mTime:= mbFileAge(mime_globs);
    if (mbFileAge(gpIniDir + pixmaps_cache) = mTime) and
       (mbFileAccess(gpIniDir + pixmaps_cache, fmOpenRead)) and
       (mbFileSize(gpIniDir + pixmaps_cache) > SizeOf(DWord) * 2) then
    begin
      cache := TFileStreamEx.Create(gpIniDir + pixmaps_cache, fmOpenRead);
      if (cache.ReadDWord <> NtoBE(cache_signature)) or
         (cache.ReadDWord <> cache_version) then
      begin
        FreeAndNil(cache);
      end;
    end;

    if Assigned(cache) then
      begin
        EntriesCount := cache.ReadDWord;
        FExtToMimeIconName.HashTableSize := EntriesCount;

        // Each entry is a file extension with a list of icon names.
        for I := 0 to EntriesCount - 1 do
        begin
          sExtension := cache.ReadAnsiString;
          IconsCount := cache.ReadDWord;
          iconsList := TStringList.Create;
          FExtToMimeIconName.Add(sExtension, iconsList);
          iconsList.Capacity := IconsCount;
          for J := 0 to IconsCount - 1 do
            iconsList.Add(cache.ReadAnsiString);
        end;
      end
    else if mbFileAccess(mime_globs, fmOpenRead) then
      begin
        // Load mapping: MIME type -> file extension.
        globs:= TStringListEx.Create;
        globs.NameValueSeparator:= ':';
        globs.LoadFromFile(mime_globs);

        // Try to load mapping: MIME type -> generic MIME icon name.
        if mbFileExists(mime_generic_icons) then
          begin
            generic_icons:= TStringListEx.Create;
            generic_icons.NameValueSeparator:= ':';
            generic_icons.LoadFromFile(mime_generic_icons);
          end;

        EntriesCount := 0;
        // Create mapping: file extension -> list of MIME icon names.
        for I:= 0 to globs.Count - 1 do
          if (globs.Strings[I]    <> '') and   // bypass empty lines
             (globs.Strings[I][1] <> '#') then // and comments
          begin
            sMimeType := globs.Names[I];
            sMimeIconName:= StringReplace(sMimeType, '/', '-', []);
            sExtension:= ExtractFileExt(globs.ValueFromIndex[I]);

            // Support only extensions, not full file name masks.
            if (sExtension <> '') and (sExtension <> '.*') then
            begin
              Delete(sExtension, 1, 1);

              node := THTDataNode(FExtToMimeIconName.Find(sExtension));
              if not Assigned(node) then
                begin
                  iconsList := TStringList.Create;
                  FExtToMimeIconName.Add(sExtension, iconsList);
                  Inc(EntriesCount);
                end
              else
                iconsList := TStringList(node.Data);

              if iconsList.IndexOf(sMimeIconName) < 0 then
                iconsList.Add(sMimeIconName);

              // Shared-mime-info spec says:
              // "If [generic-icon] is not specified then the mimetype is used to generate the
              // generic icon by using the top-level media type (e.g. "video" in "video/ogg")
              // and appending "-x-generic" (i.e. "video-x-generic" in the previous example)."
              if Assigned(generic_icons) then
                begin
                  J := generic_icons.IndexOfName(sMimeType);
                  if J <> -1 then
                    sMimeIconName := generic_icons.ValueFromIndex[J] // found generic icon
                  else
                    sMimeIconName := Copy2Symb(sMimeIconName, '-') + '-x-generic';
                end
              else
                sMimeIconName := Copy2Symb(sMimeIconName, '-') + '-x-generic';

              if iconsList.IndexOf(sMimeIconName) < 0 then
                iconsList.Add(sMimeIconName);
            end;
          end;

        // save to cache
        cache := TFileStreamEx.Create(gpIniDir + pixmaps_cache, fmCreate);
        cache.WriteDWord(NtoBE(cache_signature));
        cache.WriteDWord(cache_version);
        cache.WriteDWord(EntriesCount);
        for I := 0 to FExtToMimeIconName.HashTable.Count - 1 do
          begin
            nodeList := TFPObjectList(FExtToMimeIconName.HashTable.Items[I]);
            if Assigned(nodeList) then
              for J := 0 to nodeList.Count - 1 do
                begin
                  node := THtDataNode(nodeList.Items[J]);
                  iconsList := TStringList(node.Data);
                  cache.WriteAnsiString(node.Key);
                  cache.WriteDWord(iconsList.Count);
                  for K := 0 to iconsList.Count - 1 do
                    cache.WriteAnsiString(iconsList.Strings[K]);
                end;
          end;
        FreeAndNil(cache); // Close file
        mbFileSetTime(gpIniDir + pixmaps_cache, mTime, 0, 0);
      end;

  finally
    if Assigned(globs) then
      FreeAndNil(globs);
    if Assigned(generic_icons) then
      FreeAndNil(generic_icons);
    if Assigned(cache) then
      FreeAndNil(cache);
  end;
end;

function TPixMapManager.GetMimeIcon(AFileExt: String; AIconSize: Integer): PtrInt;
var
  I: Integer;
  node: THTDataNode;
  iconList: TStringList;
begin
  Result := -1;

  // Search for an icon for this file extension.
  node := THTDataNode(FExtToMimeIconName.Find(AFileExt));
  if Assigned(node) then
    begin
      iconList := TStringList(node.Data);
      // Try to load one of the icons in the list.
      for I := 0 to iconList.Count - 1 do
        begin
          Result := LoadIconThemeIcon(iconList.Strings[I], AIconSize);
          if Result <> -1 then break;
        end;
    end;
end;

function TPixMapManager.LoadIconThemeIcon(AIconName: String; AIconSize: Integer): PtrInt;
var
  fileIndex: PtrInt;
{$IFDEF LCLGTK2}
  pbPicture: PGdkPixbuf;
  pgcIconName: Pgchar;
{$ELSE}
  sIconFileName: UTF8String;
  bmpBitmap: Graphics.TBitmap;
{$ENDIF}
begin
  fileIndex := FThemePixmapsFileNames.Find(AIconName);
  if fileIndex < 0 then
    begin
{$IFDEF LCLGTK2}
      pgcIconName:= Pgchar(AIconName);
      pbPicture:= gtk_icon_theme_load_icon(FIconTheme, pgcIconName, AIconSize, GTK_ICON_LOOKUP_NO_SVG, nil);
      if pbPicture <> nil then
        begin
          Result := FPixmapList.Add(pbPicture);
          FThemePixmapsFileNames.Add(AIconName, Pointer(Result));
        end
      else
        Result := -1;
{$ELSE}
      sIconFileName:= FIconTheme.FindIcon(AIconName, AIconSize);
      if sIconFileName <> EmptyStr then
        begin
          bmpBitmap := CheckLoadPixmap(sIconFileName, False);
          if Assigned(bmpBitmap) then
            begin
              Result := FPixmapList.Add(bmpBitmap); // add to list
              FThemePixmapsFileNames.Add(AIconName, Pointer(Result));
            end;
        end
      else
        Result := -1;
{$ENDIF}
    end
  else
    Result := PtrInt(FThemePixmapsFileNames.List[fileIndex]^.Data);
end;

function TPixMapManager.GetIconByDesktopFile(sFileName: UTF8String; iDefaultIcon: PtrInt): PtrInt;
var
  I: PtrInt;
  iniDesktop: TIniFileEx;
  sIconName: UTF8String;
begin
  iniDesktop:= TIniFileEx.Create(sFileName, fmOpenRead);
  try
    sIconName:= iniDesktop.ReadString('Desktop Entry', 'Icon', EmptyStr);
  finally
    FreeThenNil(iniDesktop);
  end;

  I:= GetIconByName(sIconName);
  if I < 0 then
    Result:= iDefaultIcon
  else
    Result:= I;
end;

{$ENDIF} // Unix

constructor TPixMapManager.Create;
{$IFDEF MSWINDOWS}
var
  FileInfo : TSHFileInfoW;
  iIconSize : Integer;
{$ENDIF}
begin
  FExtList := TStringHashList.Create(True);
  FPixmapsFileNames := TStringHashList.Create(True);
  FPixmapList := TFPList.Create;
  FDriveIconList := TStringList.Create;

  {$IF DEFINED(UNIX) AND NOT DEFINED(DARWIN)}
  FExtToMimeIconName := TFPDataHashTable.Create;
  FThemePixmapsFileNames := TStringHashList.Create(True);
  CreateIconTheme;
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
{$IF DEFINED(UNIX) AND NOT DEFINED(DARWIN)}
  J : Integer;
  nodeList: TFPObjectList;
{$ENDIF}
begin
  if Assigned(FPixmapList) then
  begin
    for I := 0 to FPixmapList.Count - 1 do
      if Assigned(FPixmapList.Items[I]) then
  {$IFDEF LCLGTK2}
        g_object_unref(PGdkPixbuf(FPixmapList.Items[I]));
  {$ELSE}
        Graphics.TBitmap(FPixmapList.Items[I]).Free;
  {$ENDIF}
    FreeAndNil(FPixmapList);
  end;

  if Assigned(FExtList) then
    FreeAndNil(FExtList);
  if Assigned(FPixmapsFileNames) then
    FreeAndNil(FPixmapsFileNames);

  if Assigned(FDriveIconList) then
  begin
    for I := 0 to FDriveIconList.Count - 1 do
      with FDriveIconList.Objects[I] as TDriveIconList do
      begin
        if Assigned(bmMediaFloppy) then FreeAndNil(bmMediaFloppy);
        if Assigned(bmDriveHardDisk) then FreeAndNil(bmDriveHardDisk);
        if Assigned(bmMediaFlash) then FreeAndNil(bmMediaFlash);
        if Assigned(bmMediaOptical) then FreeAndNil(bmMediaOptical);
        if Assigned(bmDriveNetwork) then FreeAndNil(bmDriveNetwork);
        Free;
      end;
    FreeAndNil(FDriveIconList);
  end;

  {$IF DEFINED(MSWINDOWS)}
  ImageList_Destroy(FSysImgList);
  {$ELSEIF DEFINED(UNIX) AND NOT DEFINED(DARWIN)}
  DestroyIconTheme;

  for I := 0 to FExtToMimeIconName.HashTable.Count - 1 do
    begin
      nodeList := TFPObjectList(FExtToMimeIconName.HashTable.Items[I]);
      if Assigned(nodeList) then
        for J := 0 to nodeList.Count - 1 do
          TStringList(THtDataNode(nodeList.Items[J]).Data).Free;
    end;

  FreeThenNil(FExtToMimeIconName);
  FreeThenNil(FThemePixmapsFileNames);
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
      bmDriveNetwork:= CheckLoadPixmap('devices' + PathDelim + 'network-wired.png');
    end;
  // load emblems
  if gIconsSize = 22 then
    I:= 16
  else
    I:= gIconsSize div 2;
  FPixmapSize := IntToStr(I) + 'x' + IntToStr(I) + PathDelim;
  FiEmblemLinkID:= CheckAddPixmap('emblems' + PathDelim + 'emblem-symbolic-link.png', gIconsSize div 2);
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
      gExts.Items[I].IconIndex:= FiDefaultIconID;
      sPixMap := gExts.Items[I].Icon;
      if mbFileExists(sPixMap) then
        begin
          iPixMap:= CheckAddPixmap(sPixMap, gIconsSize, False);
          if iPixMap < 0 then Continue;
          gExts.Items[I].IconIndex:= iPixMap;
          //DebugLn('sPixMap = ',sPixMap, ' Index = ', IntToStr(iPixMap));

          // set pixmap index for all extensions
          for iekv := 0 to gExts.Items[I].Extensions.Count - 1 do
            begin
              sExt := gExts.Items[I].Extensions[iekv];
              if FExtList.Find(sExt) < 0 then
                FExtList.Add(sExt, TObject(iPixMap));
            end;
        end;
    end;
  {/ Load icons from doublecmd.ext }  
  
  if mbFileExists(sFileName) then
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

        if FExtList.Find(sExt)<0 then
          FExtList.Add(sExt, TObject(iPixMap));
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
          sExt := Plugins.Names[I];
          if (Length(sExt) > 0) and (sExt[1] <> '#') then // if plugin not disabled
            begin
          iCurPlugCaps := StrToInt(Copy(sCurrentPlugin, 1, Pos(',',sCurrentPlugin) - 1));
          if (iCurPlugCaps and PK_CAPS_HIDE) <> PK_CAPS_HIDE then
            begin
                if FExtList.Find(sExt) < 0 then
                  FExtList.Add(sExt, TObject(FiArcIconID));
            end;
            end;
        end; //for
  Plugins.Free;
  
  (* /Set archive icons *)

  {$IF DEFINED(UNIX) AND NOT DEFINED(DARWIN)}
  if (gShowIcons <> sim_none) and (gShowIcons <> sim_standart) then
    begin
      LoadMimeIconNames;
      {$IFNDEF LCLGTK2}
      FIconTheme.Load;
      {$ENDIF}
    end;
  {$ENDIF}
end;

function TPixMapManager.GetBitmap(iIndex: PtrInt; BkColor : TColor): Graphics.TBitmap;
{$IFDEF MSWINDOWS}
var
  memstream: TMemoryStream;
{$ENDIF}
begin
  if (iIndex >= 0) and (iIndex < FPixmapList.Count) then
  begin
{$IFDEF LCLGTK2}
    Result:= PixBufToBitmap(PGdkPixbuf(FPixmapList[iIndex]));
{$ELSE}
    // Make a new copy.
    Result := Graphics.TBitmap.Create;
    Result.Assign(Graphics.TBitmap(FPixmapList[iIndex]));
{$ENDIF}
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

function TPixMapManager.DrawBitmap(iIndex: PtrInt; Canvas : TCanvas; X, Y: Integer) : Boolean;
begin
  Result := DrawBitmap(iIndex, Canvas, X, Y, gIconsSize, gIconsSize); // X, Y, 0, 0 - No bitmap stretching.
end;

function TPixMapManager.DrawBitmap(iIndex: PtrInt; Canvas: TCanvas; X, Y, Width, Height: Integer): Boolean;

  procedure TrySetSize(aWidth, aHeight: Integer);
  begin
    if Width = 0 then
      Width := aWidth;
    if Height = 0 then
      Height := aHeight;
  end;

  {$IFDEF MSWINDOWS}
var
  hicn: HICON;
  cx, cy: Integer;
  {$ENDIF}

  {$IFDEF LCLGTK2}
var
  pbPicture : PGdkPixbuf;
  iPixbufWidth : Integer;
  iPixbufHeight : Integer;
  {$ELSE}
var
  Bitmap: Graphics.TBitmap;
  aRect: TRect;
  {$ENDIF}
begin
  Result := True;
  if (iIndex >= 0) and (iIndex < FPixmapList.Count) then
  begin
  {$IFDEF LCLGTK2}
    pbPicture := PGdkPixbuf(FPixmapList[iIndex]);
    iPixbufWidth :=  gdk_pixbuf_get_width(pbPicture);
    iPixbufHeight :=  gdk_pixbuf_get_height(pbPicture);
    TrySetSize(iPixbufWidth, iPixbufHeight);
    DrawPixbufAtCanvas(Canvas, pbPicture, 0, 0, X, Y, Width, Height);
  {$ELSE}
    Bitmap := Graphics.TBitmap(FPixmapList[iIndex]);
    TrySetSize(Bitmap.Width, Bitmap.Height);
    aRect := Classes.Bounds(X, Y, Width, Height);
    Canvas.StretchDraw(aRect, Bitmap);
  {$ENDIF}
  end
  else
  {$IFDEF MSWINDOWS}
  if iIndex >= $1000 then
    try
      if ImageList_GetIconSize(FSysImgList, @cx, @cy) then
        TrySetSize(cx, cy)
      else
        TrySetSize(gIconsSize, gIconsSize);

      if Height in [16, 32] then
        // for transparent
        ImageList_Draw(FSysImgList, iIndex - $1000, Canvas.Handle, X, Y, ILD_TRANSPARENT)
      else
      begin
        hicn:= ImageList_GetIcon(FSysImgList, iIndex - $1000, 0);
        try
          if IsGdiPlusLoaded then
            Result:= GdiPlusStretchDraw(hicn, Canvas.Handle, X, Y, Width, Height)
          else
            Result:= DrawIconEx(Canvas.Handle, X, Y, hicn, Width, Height, 0, 0, DI_NORMAL);
        finally
          DestroyIcon(hicn);
        end;
      end;
    except
      Result:= False;
    end;

  {$ELSE}
    Result:= False;
  {$ENDIF}
end;

function TPixMapManager.DrawBitmap(iIndex: PtrInt; AFile: TFile; DirectAccess: Boolean; Canvas: TCanvas; X, Y: Integer): Boolean;
var
  I: Integer;
begin
  Result:= DrawBitmap(iIndex, Canvas, X, Y);

  if gIconOverlays then
    begin
    {$IFDEF MSWINDOWS}
      if DirectAccess then
      begin
        I:= SHGetOverlayIconIndex(AFile.Path, AFile.Name);
        if I >= 0 then
          Result:= DrawBitmap(I + $1000, Canvas, X, Y);
      end;
    {$ELSE}
      if AFile.IsLink then
        begin
          I:= gIconsSize div 2;
          Result:= DrawBitmap(FiEmblemLinkID, Canvas, X, Y + I, I, I);
        end;
    {$ENDIF}
    end;
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
    else
      Result := -1;
  end;
end;

function TPixMapManager.GetIconByFile(AFile: TFile; DirectAccess: Boolean): PtrInt;
var
  Ext: String;
{$IFDEF MSWINDOWS}
  sFileName: String;
    FileInfo: TSHFileInfoW;
    _para2: DWORD;
    _para5: UINT;
{$ENDIF}
begin
  Result := -1;
  if not Assigned(AFile) then Exit;

  with AFile do
  begin
    if Name = '..' then
    begin
      Result := FiUpDirIconID;
      Exit;
    end;

    if IsLinkToDirectory then
    begin
    {$IFDEF UNIX}
      if gIconOverlays then
        Result:= FiDirIconID
      else
    {$ENDIF}
      Result := FiDirLinkIconID;
      Exit;
    end;

    if IsDirectory then
    begin
      {$IF DEFINED(MSWINDOWS)}
      if ((gShowIcons = sim_standart) or
         (not (DirectAccess and mbFileExists(Path + Name + '\desktop.ini')))) and
         (GetDeviceCaps(Application.MainForm.Canvas.Handle, BITSPIXEL) > 16) then
      {$ELSEIF DEFINED(UNIX) AND NOT DEFINED(DARWIN)}
      if (gShowIcons = sim_all_and_exe) and
         (DirectAccess and mbFileExists(Path + Name + '/.directory')) then
        begin
          Result:= GetIconByDesktopFile(Path + Name + '/.directory', FiDirIconID);
          Exit;
        end
      else
      {$ENDIF}
        begin
          Result := FiDirIconID;
          Exit;
        end;
    end;

    if IsLink and not gIconOverlays then
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

    {$IF DEFINED(MSWINDOWS)}
    if gShowIcons <> sim_all_and_exe then
      begin
        if Ext = 'exe' then
          Exit(FiExeIconID)
        else if Ext = 'lnk' then
          Exit(FiLinkIconID)
        else if Ext = 'ico' then
          Exit(FiDefaultIconID)
      end;
    {$ELSEIF DEFINED(UNIX) AND NOT DEFINED(DARWIN)}
    if gShowIcons = sim_all_and_exe then
      begin
        if DirectAccess and (Ext = 'desktop') then
          begin
            Result:= GetIconByDesktopFile(Path + Name, FiDefaultIconID);
            Exit;
          end;
      end;
    {$ENDIF}

    Result:= FExtList.Find(Ext);
    if Result >= 0 then
      Result := PtrInt(FExtList.List[Result]^.Data)
    else
    begin
      if gShowIcons = sim_standart then
        Exit(FiDefaultIconID);

      {$IF DEFINED(MSWINDOWS)}

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
       
      if (Ext <> 'exe') and
         (Ext <> 'ico') and
         (Ext <> 'lnk') and (not IsDirectory) then
      begin
        FExtList.Add(Ext, TObject(Result));
      end;

      {$ELSEIF DEFINED(UNIX) AND NOT DEFINED(DARWIN)}

      Result := GetMimeIcon(Ext, gIconsSize);
      if Result < 0 then
        Result := FiDefaultIconID;

      // Default icon should also be associated with the extension
      // because it will be faster to find next time.
      FExtList.Add(Ext, Pointer(Result));

      {$ENDIF}
    end;
  end;
end;

function TPixMapManager.GetIconByName(const AIconName: UTF8String): PtrInt;
begin
  Result := CheckAddPixmap(AIconName, gIconsSize, False);
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
      SFI.hIcon := 0;
      Result := Graphics.TBitMap.Create;
      case IconSize of
      16, 32: // Standart icon size
        begin
          if IconSize = 16 then
            _para5 := SHGFI_SMALLICON
          else
            _para5 := SHGFI_LARGEICON;

          if (SHGetFileInfo(PChar(Drive^.Path), 0, SFI, SizeOf(SFI), _para5 or SHGFI_ICON) <> 0) and
             (SFI.hIcon <> 0) then
            try
              Icon := CreateIconFromHandle(SFI.hIcon);
              IntfImage := Icon.CreateIntfImage;
              Result.LoadFromIntfImage(IntfImage);
            finally
              if Assigned(Icon) then
                FreeAndNil(Icon);
              if Assigned(IntfImage) then
                FreeAndNil(IntfImage);
              DestroyIcon(SFI.hIcon);
            end;
        end;
      else  // for non standart icon size we Convert HIcon to TBitMap
        begin
          if (SHGetFileInfo(PChar(Drive^.Path), 0, SFI, SizeOf(SFI), SHGFI_ICON) <> 0) and
             (SFI.hIcon <> 0) then
          try
            Result.Width := GetSystemMetrics(SM_CXICON);
            Result.Height := GetSystemMetrics(SM_CYICON);
            Result.Canvas.Brush.Color := clBackColor;
            Result.Canvas.FillRect(Result.Canvas.ClipRect);
            Windows.DrawIcon(Result.Canvas.Handle,0,0,SFI.hIcon);

            Result := StretchBitmap(Result, IconSize, clBackColor, True);
          finally
            DestroyIcon(SFI.hIcon);
          end;
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
  if GetDeviceCaps(Application.MainForm.Canvas.Handle, BITSPIXEL) < 15 then Exit(nil);
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
  dtNetwork:
    Bitmap := bmDriveNetwork;
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

function TPixMapManager.GetArchiveIcon(IconSize: Integer; clBackColor : TColor) : Graphics.TBitmap;
begin
  Result := GetBitmap(FiArcIconID, clBackColor);
  if Assigned(Result) then
  begin
    //  if need stretch icon
    if (IconSize <> gIconsSize) then
      begin
        Result := StretchBitmap(Result, IconSize, clBackColor, True);
      end;
  end;
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

