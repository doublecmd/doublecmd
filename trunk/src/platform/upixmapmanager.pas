{
   File name: uPixMapManager.pas
   Date:      2004/04/xx
   Author:    Radek Cervinka  <radek.cervinka@centrum.cz>

   Fast pixmap memory manager a loader

   Copyright (C) 2004
   
   contributors:
   
   Copyright (C) 2006-2019 Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit uPixMapManager;

{$mode objfpc}{$H+}

{$IFDEF DARWIN}
{$modeswitch objectivec1}
{$ENDIF}

interface

{
  GTK2 is used directly in PixmapManager, because FPC/Lazarus draws bitmaps
  without alpha channel under GTK2, so bitmaps looks ugly.
  If this problem will be fixed then GTK2 specific code could be dropped.
}

uses
  Classes, SysUtils, Graphics, syncobjs, uFileSorting, DCStringHashListUtf8,
  uFile, uIconTheme, uDrive, uDisplayFile, uGlobs, uDCReadPSD, uOSUtils
  {$IF DEFINED(MSWINDOWS) and DEFINED(LCLQT5)}
  , fgl
  {$ELSEIF DEFINED(UNIX)}
  , DCFileAttributes
    {$IF DEFINED(DARWIN)}
      {$IF (FPC_FULLVERSION >= 30000)}
      , uDCTiffImage
      {$ENDIF}
    {$ELSE}
    , contnrs, uDCReadSVG, uGio
      {$IFDEF LCLGTK2}
      , gtk2
      {$ELSE}
      , uUnixIconTheme
      {$ENDIF}
    {$ENDIF}
  {$ENDIF};

const
  DC_THEME_NAME = 'dctheme';

type
  TDriveIconList = record
    Size: Integer;
    Bitmap: array[TDriveType] of TBitmap;
  end;

  { TfromWhatBitmapWasLoaded }
  //Used to indicate from where the icon was loaded from.
  //Useful when exporting to TC for example which cannot used "as is" the same icon file in some circumstances.
  TfromWhatBitmapWasLoaded = (fwbwlNotLoaded, fwbwlIconThemeBitmap, fwbwlResourceFileExtracted, fwbwlGraphicFile, fwbwlGraphicFileNotSupportedByTC, fwbwlFileIconByExtension, fwbwlFiDefaultIconID);
  PTfromWhatBitmapWasLoaded = ^TfromWhatBitmapWasLoaded;

  { TPixMapManager }

  TPixMapManager = class
  
  private
    {en
       Maps file extension to index of bitmap (in FPixmapList) for this file extension.
    }
    FExtList : TStringHashListUtf8;
    {en
       Maps icon filename to index of bitmap (in FPixmapList) for this icon.
       Uses absolute file names.
    }
    FPixmapsFileNames : TStringHashListUtf8;
    {en
       A list of loaded bitmaps.
       Stores TBitmap objects (on GTK2 it stores PGdkPixbuf pointers).
    }
    FPixmapList : TFPList;
    {en
       Lock used to synchronize access to PixmapManager storage.
    }
    FPixmapsLock: TCriticalSection;

    FDriveIconList : array[0..2] of TDriveIconList;
    FiDirIconID : PtrInt;
    FiDirLinkIconID : PtrInt;
    FiDirLinkBrokenIconID : PtrInt;
    FiLinkIconID : PtrInt;
    FiLinkBrokenIconID : PtrInt;
    FiEmblemLinkID: PtrInt;
    FiEmblemUnreadableID: PtrInt;
    FiUpDirIconID : PtrInt;
    FiDefaultIconID : PtrInt;
    FiExeIconID : PtrInt;
    FiArcIconID : PtrInt;
    FiSortAscID : PtrInt;
    FiSortDescID : PtrInt;
    FiHashIconID : PtrInt;
    {$IF DEFINED(MSWINDOWS)}
    FSysImgList : THandle;
    FiSysDirIconID : PtrInt;
    {$ELSEIF DEFINED(DARWIN)}
    FUseSystemTheme: Boolean;
    {$ELSEIF DEFINED(UNIX)}
    {en
       Maps file extension to MIME icon name(s).
    }
    FExtToMimeIconName: TFPDataHashTable;
    {$IFDEF LCLGTK2}
    FIconTheme: PGtkIconTheme;
    {$ELSE}
    FIconTheme: TIconTheme;
    {$ENDIF}
    FHomeFolder: String;
    {$ENDIF}
    {en
       Maps theme icon name to index of bitmap (in FPixmapList) for this icon.
    }
    FThemePixmapsFileNames: TStringHashListUtf8;
    FDCIconTheme: TIconTheme;
    {$IF DEFINED(MSWINDOWS) and DEFINED(LCLQT5)}
    type
      TPtrIntMap = specialize TFPGMap<PtrInt, PtrInt>;
    var
      FSystemIndexList: TPtrIntMap;
    {$ENDIF}

    procedure CreateIconTheme;
    procedure DestroyIconTheme;
    {en
       Same as LoadBitmap but displays a warning if pixmap file doesn't exist.
    }
    function CheckLoadPixmapFromFile(const AIconName: String) : TBitmap;
    {en
       If path is absolute tries to load bitmap and add to storage.
       If path is relative it tries to load theme icon and add to storage.
    }
    function CheckAddPixmap(AIconName: String; AIconSize : Integer = 0): PtrInt;
    {en
       Loads a theme icon and adds it to storage.
       This function should only be called under FPixmapLock.
    }
    function CheckAddThemePixmapLocked(AIconName: String; AIconSize: Integer): PtrInt;
    {en
       Loads a theme icon and adds it to storage.
       Safe to call without a lock.
    }
    function CheckAddThemePixmap(const AIconName: String; AIconSize: Integer = 0) : PtrInt;
    {en
       Loads a theme icon. Returns TBitmap (on GTK2 convert GdkPixbuf to TBitmap).
       This function should only be called under FPixmapLock.
    }
    function LoadIconThemeBitmapLocked(AIconName: String; AIconSize: Integer): TBitmap;
    {en
       Loads a plugin icon.
    }
    function GetPluginIcon(const AIconName: String; ADefaultIcon: PtrInt): PtrInt;

  {$IF DEFINED(MSWINDOWS) and DEFINED(LCLQT5)}
    function CheckAddSystemIcon(ASystemIndex: PtrInt): PtrInt;
  {$ENDIF}

  {$IF DEFINED(WINDOWS)}
    {en
       Checks if the AIconName points to an icon resource in a library, executable, etc.
       @param(AIconName
              Full path to the file with the icon with appended "," and icon index.)
       @param(IconFile
              Returns the full path to the file containing the icon resource.)
       @param(IconIndex
              Returns the index of the icon in the file.)
       @returns(@true if AIconName points to an icon resource, @false otherwise.)
    }
    function GetIconResourceIndex(const IconPath: String; out IconFile: String; out IconIndex: PtrInt): Boolean;
    function GetSystemFolderIcon: PtrInt;
    function GetSystemArchiveIcon: PtrInt;
    function GetSystemExecutableIcon: PtrInt;
  {$ENDIF}
  {$IF DEFINED(UNIX) AND NOT DEFINED(DARWIN)}
    {en
       Loads MIME icons names and creates a mapping: file extension -> MIME icon name.
       Doesn't need to be synchronized as long as it's only called from Load().
    }
    procedure LoadMimeIconNames;
    {en
       Retrieves index of a theme icon based on file extension
       using Extension->MIME map.
       Loads the icon and adds it into PixmapManager, if not yet added.
       This function should only be called under FPixmapLock.
    }
    function GetMimeIcon(AFileExt: String; AIconSize: Integer): PtrInt;
    {en
       It is synchronized in GetIconByName->CheckAddPixmap.
    }
    function GetIconByDesktopFile(sFileName: String; iDefaultIcon: PtrInt): PtrInt;
  {$ENDIF}
  {$IF DEFINED(DARWIN)}
    function GetSystemFolderIcon: PtrInt;
    function GetMimeIcon(AFileExt: String; AIconSize: Integer): PtrInt;
  {$ENDIF}
    function GetBuiltInDriveIcon(Drive : PDrive; IconSize : Integer; clBackColor : TColor) : Graphics.TBitmap;

{$IF NOT DEFINED(DARWIN)}
    procedure LoadApplicationThemeIcon;
{$ENDIF}

  public
    constructor Create;
    destructor Destroy; override;
    procedure Load(const sFileName : String);
    {en
       Loads a graphical file (if supported) to a bitmap.
       @param(AIconFileName must be a full path to the graphical file.)
       @param(ABitmap receives a new bitmap object.)
       @returns(@true if bitmap has been loaded, @false otherwise.)
    }
    function LoadBitmapFromFile(AIconFileName: String; out ABitmap: TBitmap): Boolean;
    {en
       Loads a graphical file as a bitmap if filename is full path.
       Environment variables in the filename are supported.
       If filename is not graphic file it tries to load some bitmap associated
       with the file (by extension, attributes, etc.).
       Loads an icon from a file's resources if filename ends with ",Nr" (on Windows).
       Loads a theme icon if filename is not a full path.
       Performs resize of the bitmap to <iIconSize>x<iIconSize> if Stretch = @true.
       If Stretch = @false then clBackColor is ignored.
    }
    function LoadBitmapEnhanced(sFileName : String; iIconSize : Integer; Stretch: Boolean; clBackColor : TColor; fromWhatItWasLoaded:PTfromWhatBitmapWasLoaded = nil) : Graphics.TBitmap;
    {en
       Loads a theme icon as bitmap.
       @param(AIconName is a MIME type name.)
    }
    function LoadIconThemeBitmap(AIconName: String; AIconSize: Integer): TBitmap;
    {en
       Retrieves a bitmap stored in PixmapManager by index (always returns a new copy).
       On Windows if iIndex points to system icon list it creates a new bitmap
       by loading system icon and drawing onto the bitmap.
    }
    function GetBitmap(iIndex : PtrInt) : TBitmap;
    function DrawBitmap(iIndex: PtrInt; Canvas : TCanvas; X, Y: Integer) : Boolean;
    function DrawBitmapAlpha(iIndex: PtrInt; Canvas : TCanvas; X, Y: Integer) : Boolean;
    {en
       Draws bitmap stretching it if needed to Width x Height.
       If Width is 0 then full bitmap width is used.
       If Height is 0 then full bitmap height is used.
       @param(iIndex
              Index of pixmap manager's bitmap.)
    }
    function DrawBitmap(iIndex: PtrInt; Canvas : TCanvas; X, Y, Width, Height: Integer) : Boolean;
    {en
       Draws overlay bitmap for a file.
       @param(AFile
              File for which is needed to draw the overlay icon.)
       @param(DirectAccess
              Whether the file is on a directly accessible file source.)
    }
    function DrawBitmapOverlay(AFile: TDisplayFile; DirectAccess: Boolean; Canvas : TCanvas; X, Y: Integer) : Boolean;
    function GetIconBySortingDirection(SortingDirection: TSortDirection): PtrInt;
    {en
       Retrieves icon index in FPixmapList table for a file.

       @param(AFile
              File for which to retrieve the icon.)
       @param(DirectAccess
              Whether the file is on a directly accessible file source.)
       @param(LoadIcon
              Only used when an icon for a file does not yet exist in FPixmapsList.
              If @true then it loads the icon into FPixmapsList table and returns
              the index of the loaded icon.
              If @false then it returns -1 to notify that an icon for the file
              does not exist in FPixmapsList.
              If the icon already exists for the file the function returns
              its index regardless of LoadIcon parameter.)
       @param(IconsMode
              Whether to retrieve only standard icon, also from file resources, etc.)
       @param(GetIconWithLink
              If the file is a link and GetLinkIcon is @true it retrieves icon
              with embedded link bitmap. If @false it only retrieves the file icon itself.)
    }
    function GetIconByFile(AFile: TFile; DirectAccess: Boolean; LoadIcon: Boolean;
                           IconsMode: TShowIconsMode; GetIconWithLink: Boolean): PtrInt;
    {$IF DEFINED(MSWINDOWS) OR DEFINED(RabbitVCS)}
    {en
       Retrieves overlay icon index for a file.

       @param(AFile
              File for which to retrieve the overlay icon.)
       @param(DirectAccess
              Whether the file is on a directly accessible file source.)
    }
    function GetIconOverlayByFile(AFile: TFile; DirectAccess: Boolean): PtrInt;
    {$ELSEIF DEFINED(DARWIN)}
    function GetApplicationBundleIcon(sFileName: String; iDefaultIcon: PtrInt): PtrInt;
    {$ENDIF}
    function GetIconByName(const AIconName: String): PtrInt;
    function GetThemeIcon(const AIconName: String; AIconSize: Integer) : Graphics.TBitmap;
    function GetDriveIcon(Drive : PDrive; IconSize : Integer; clBackColor : TColor) : Graphics.TBitmap;
    function GetDefaultDriveIcon(IconSize : Integer; clBackColor : TColor) : Graphics.TBitmap;
    function GetArchiveIcon(IconSize: Integer; clBackColor : TColor) : Graphics.TBitmap;
    function GetFolderIcon(IconSize: Integer; clBackColor : TColor) : Graphics.TBitmap;
    {en
       Returns default icon for a file.
       For example default folder icon for folder, default executable icon for *.exe, etc.
    }
    function GetDefaultIcon(AFile: TFile): PtrInt;
  end;

var
  PixMapManager: TPixMapManager = nil;

var
  ICON_SIZES: array [0..3] of Integer = (16, 24, 32, 48);

procedure LoadPixMapManager;

function AdjustIconSize(ASize: Integer; APixelsPerInch: Integer): Integer;

function StretchBitmap(var bmBitmap : Graphics.TBitmap; iIconSize : Integer;
                       clBackColor : TColor; bFreeAtEnd : Boolean = False) : Graphics.TBitmap;


implementation

uses
  GraphType, LCLIntf, LCLType, LCLProc, Forms, uGlobsPaths, WcxPlugin,
  DCStrUtils, uDCUtils, uFileSystemFileSource, uReSample, uDebug,
  DCOSUtils, DCClassesUtf8, LazUTF8, uGraphics, uHash
  {$IFDEF LCLGTK2}
    , uPixMapGtk, gdk2pixbuf, gdk2, glib2
  {$ENDIF}
  {$IFDEF MSWINDOWS}
    , CommCtrl, ShellAPI, Windows, DCFileAttributes, uBitmap, uGdiPlus,
      IntfGraphics, uShlObjAdditional
  {$ELSE}
    , StrUtils, Types, DCBasicTypes
  {$ENDIF}
  {$IFDEF DARWIN}
    , CocoaAll, MacOSAll, uClassesEx
  {$ENDIF}
  {$IFDEF RabbitVCS}
  , Math, uRabbitVCS
  {$ENDIF}
  ;

{$IF DEFINED(MSWINDOWS) OR DEFINED(RabbitVCS)}
const
  SystemIconIndexStart: PtrInt = High(PtrInt) div 2;
{$ENDIF}

function AdjustIconSize(ASize: Integer; APixelsPerInch: Integer): Integer;
begin
{$IF DEFINED(MSWINDOWS)}
  if (APixelsPerInch = Screen.PixelsPerInch) then
    Result:= ASize
  else begin
    Result:= MulDiv(ASize, Screen.PixelsPerInch, APixelsPerInch);
  end;
{$ELSE}
  Result:= ASize;
{$ENDIF}
end;

function StretchBitmap(var bmBitmap : Graphics.TBitmap; iIconSize : Integer;
                       clBackColor : TColor; bFreeAtEnd : Boolean = False) : Graphics.TBitmap;
var
  memstream: TMemoryStream;
  {$IFDEF MSWINDOWS}
  liiSource: TLazIntfImage = nil;
  liiDestination: TLazIntfImage = nil;
  ImgFormatDescription:TRawImageDescription;
  {$ENDIF}

begin
  {$IFDEF MSWINDOWS}
  //Let's make sure we're working with 32-bits bitmap
  if bmBitmap.PixelFormat<>pf32bit then
    begin
      liiSource:=bmBitmap.CreateIntfImage;
      liiDestination:=TLazIntfImage.Create(bmBitmap.Width,bmBitmap.Height);
	  try
        ImgFormatDescription.Init_BPP32_B8G8R8A8_BIO_TTB(bmBitmap.Width,bmBitmap.Height);
        liiDestination.DataDescription:=ImgFormatDescription;
        liiDestination.CopyPixels(liiSource);
        bmBitmap.FreeImage;
        bmBitmap.PixelFormat:=pf32bit;
        bmBitmap.LoadFromIntfImage(liiDestination);
	  finally
        liiDestination.Free;
        liiSource.Free;
	  end;
    end;
  {$ENDIF}

  if (iIconSize <> bmBitmap.Height) or (iIconSize <> bmBitmap.Width) then
  begin
    Result := Graphics.TBitMap.Create;
    try
      Result.SetSize(iIconSize, iIconSize);
      if bmBitmap.RawImage.Description.AlphaPrec <> 0 then // if bitmap has alpha channel
        Stretch(bmBitmap, Result, ResampleFilters[2].Filter, ResampleFilters[2].Width)
      else
        with Result do
        begin
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
          if bmBitmap.RawImage.Description.MaskBitsPerPixel = 0 then
            TransparentColor := clBackColor;
        end; //  with
      if bFreeAtEnd then
        FreeAndNil(bmBitmap);
    except
      FreeAndNil(Result);
      raise;
    end;
  end
  else // Don't need to stretch.
  begin
    if bFreeAtEnd then
    begin
      Result := bmBitmap;
      bmBitmap := nil;
    end
    else
    begin
      Result := Graphics.TBitMap.Create;
      try
        Result.Assign(bmBitmap);
      except
        FreeAndNil(Result);
        raise;
      end;
    end;
  end;
end;

{ TPixMapManager }

{ TPixMapManager.LoadBitmapFromFile }
function TPixMapManager.LoadBitmapFromFile(AIconFileName: String; out ABitmap: Graphics.TBitmap): Boolean;
var
  {$IFDEF LCLGTK2}
  pbPicture : PGdkPixbuf;
  {$ELSE}
  Picture: TPicture;
  {$ENDIF}
begin
  Result:= False;

  {$IFDEF LCLGTK2}
  pbPicture := gdk_pixbuf_new_from_file(PChar(AIconFileName), nil);
  if pbPicture <> nil then
  begin
    ABitmap := PixBufToBitmap(pbPicture);
    gdk_pixmap_unref(pbPicture);

    // if unsupported BitsPerPixel then exit
    if (ABitmap = nil) or (ABitmap.RawImage.Description.BitsPerPixel > 32) then
      raise EInvalidGraphic.Create('Unsupported bits per pixel');

    Result:= True;
  end;

  {$ELSE}
  Picture := TPicture.Create;
  try
    ABitmap := Graphics.TBitmap.Create;
    try
      Picture.LoadFromFile(AIconFileName);
      //Picture.Graphic.Transparent := True;
      ABitmap.Assign(Picture.Bitmap);

      // if unsupported BitsPerPixel then exit
      if ABitmap.RawImage.Description.BitsPerPixel > 32 then
        raise EInvalidGraphic.Create('Unsupported bits per pixel');

      Result:= True;
    except
      on E: Exception do
      begin
        FreeAndNil(ABitmap);
        DCDebug(Format('Error: Cannot load pixmap [%s] : %s',[AIconFileName, e.Message]));
      end;
    end;
  finally
    FreeAndNil(Picture);
  end;
  {$ENDIF}
end;

function TPixMapManager.LoadBitmapEnhanced(sFileName : String; iIconSize : Integer; Stretch: Boolean; clBackColor : TColor; fromWhatItWasLoaded:PTfromWhatBitmapWasLoaded) : Graphics.TBitmap;
var
{$IFDEF MSWINDOWS}
  iIconIndex: PtrInt;
  iIconSmall: Integer;
  phIcon: HICON = INVALID_HANDLE_VALUE;
  phIconLarge : HICON = 0;
  phIconSmall : HICON = 0;
  IconFileName: String;
{$ENDIF}
  AFile: TFile;
  iIndex : PtrInt;
  sExtFilter,
  sGraphicFilter : String;
  bmStandartBitmap : Graphics.TBitMap = nil;
begin
  Result := nil;
  if fromWhatItWasLoaded<> nil then fromWhatItWasLoaded^ := fwbwlNotLoaded;

  sFileName:= ReplaceEnvVars(sFileName);
  sFileName:= ExpandAbsolutePath(sFileName);

  // If the name is not full path then treat it as MIME type.
  if GetPathType(sFileName) = ptNone then
  begin
    bmStandartBitmap := LoadIconThemeBitmap(sFileName, iIconSize);
    if fromWhatItWasLoaded<> nil then fromWhatItWasLoaded^ := fwbwlIconThemeBitmap;
  end
  else
{$IFDEF MSWINDOWS}
  if GetIconResourceIndex(sFileName, IconFileName, iIconIndex) then
    begin
      if ExtractIconExW(PWChar(UTF8Decode(IconFileName)), iIconIndex, phIconLarge, phIconSmall, 1) = 2 then // if extracted both icons
        begin
          // Get system metrics
          iIconSmall:= GetSystemMetrics(SM_CXSMICON);
          if iIconSize <= iIconSmall then
            phIcon:= phIconSmall    // Use small icon
          else begin
            phIcon:= phIconLarge    // Use large icon
          end;

          if phIcon <> INVALID_HANDLE_VALUE then
          begin
            bmStandartBitmap := BitmapCreateFromHICON(phIcon);
            bmStandartBitmap.Masked := True; // Need to explicitly set Masked=True, Lazarus issue #0019747
            if fromWhatItWasLoaded<> nil then fromWhatItWasLoaded^ := fwbwlResourceFileExtracted;
          end;
          DestroyIcon(phIconLarge);
          DestroyIcon(phIconSmall);
        end;
    end  // GetIconResourceIndex
  else
{$ENDIF}
    begin
      sExtFilter := UTF8LowerCase(ExtractFileExt(sFileName)) + ';';
      sGraphicFilter := GraphicFilter(TGraphic);
      // if file is graphic
      if (Length(sExtFilter) > 1) and (Pos(sExtFilter, sGraphicFilter) <> 0) and mbFileExists(sFileName) then
      begin
        LoadBitmapFromFile(sFileName, bmStandartBitmap);
        if fromWhatItWasLoaded<> nil then fromWhatItWasLoaded^ := fwbwlGraphicFile;
      end;
    end;

  if not Assigned(bmStandartBitmap) then // get file icon by ext
  begin
    if mbFileSystemEntryExists(sFileName) then
      begin
        AFile := TFileSystemFileSource.CreateFileFromFile(sFileName);
        try
          iIndex := GetIconByFile(AFile, True, True, sim_all_and_exe, False);
          bmStandartBitmap := GetBitmap(iIndex);
          if fromWhatItWasLoaded<> nil then fromWhatItWasLoaded^ := fwbwlFileIconByExtension;
        finally
          FreeAndNil(AFile);
        end;
      end
    else  // file not found
      begin
        bmStandartBitmap := GetBitmap(FiDefaultIconID);
        if fromWhatItWasLoaded<> nil then fromWhatItWasLoaded^ := fwbwlFiDefaultIconID;
      end;
  end;

  if Stretch and Assigned(bmStandartBitmap) then
    Result := StretchBitmap(bmStandartBitmap, iIconSize, clBackColor, True)
  else
    Result := bmStandartBitmap;
end;

function TPixMapManager.LoadIconThemeBitmap(AIconName: String; AIconSize: Integer): Graphics.TBitmap;
begin
  FPixmapsLock.Acquire;
  try
    Result := LoadIconThemeBitmapLocked(AIconName, AIconSize);
  finally
    FPixmapsLock.Release;
  end;
end;

function TPixMapManager.CheckLoadPixmapFromFile(const AIconName: String): Graphics.TBitmap;
begin
  if not mbFileExists(AIconName) then
    begin
      DCDebug(Format('Warning: pixmap [%s] not exists!',[AIconName]));
      Exit(nil);
    end;

  LoadBitmapFromFile(AIconName, Result);
end;

function TPixMapManager.CheckAddThemePixmap(const AIconName: String; AIconSize : Integer) : PtrInt;
begin
  if AIconSize = 0 then
    AIconSize := gIconsSize;

  FPixmapsLock.Acquire;
  try
    Result := CheckAddThemePixmapLocked(AIconName, AIconSize);
  finally
    FPixmapsLock.Release;
  end;
end;

function TPixMapManager.CheckAddPixmap(AIconName: String; AIconSize : Integer): PtrInt;
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

  if AIconSize = 0 then
    AIconSize := gIconsSize;

  AIconName := ReplaceEnvVars(AIconName);

  if GetPathType(AIconName) = ptAbsolute then
    begin
      FPixmapsLock.Acquire;
      try
        // Determine if this file is already loaded.
        fileIndex := FPixmapsFileNames.Find(AIconName);
        if fileIndex < 0 then
          begin
        {$IFDEF LCLGTK2}
            if not mbFileExists(AIconName) then
              begin
                DCDebug(Format('Warning: pixmap [%s] not exists!', [AIconName]));
                Exit;
              end;
            pbPicture := gdk_pixbuf_new_from_file_at_size(PChar(AIconName), AIconSize, AIconSize, nil);
            if Assigned(pbPicture) then
              begin
                Result := FPixmapList.Add(pbPicture);
                FPixmapsFileNames.Add(AIconName, Pointer(Result));
              end
            else
              DCDebug(Format('Error: pixmap [%s] not loaded!', [AIconName]));
        {$ELSE}
            bmpBitmap := LoadBitmapEnhanced(AIconName, AIconSize, False, clNone, nil);
            if Assigned(bmpBitmap) then
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
                bmpBitmap := StretchBitmap(bmpBitmap, AIconSize, clBlack, True);
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
      finally
        FPixmapsLock.Release;
      end;
    end
  else
    begin
      Result := CheckAddThemePixmap(AIconName, AIconSize);
    end;
end;

procedure TPixMapManager.CreateIconTheme;
var
  DirList: array of string;
begin
{$IF DEFINED(UNIX) AND NOT DEFINED(DARWIN)}
  {$IFDEF LCLGTK2}
  // get current gtk theme
  FIconTheme:= gtk_icon_theme_get_for_screen(gdk_screen_get_default);
  { // load custom theme
  FIconTheme:= gtk_icon_theme_new;
  gtk_icon_theme_set_custom_theme(FIconTheme, 'oxygen');
  }
  {$ELSE}
  FIconTheme:= TIconTheme.Create(GetCurrentIconTheme,
                                 GetUnixIconThemeBaseDirList,
                                 GetUnixDefaultTheme);
  {$ENDIF}
{$ENDIF}

  // Create DC theme.
  if not gUseConfigInProgramDir then begin
    AddString(DirList, IncludeTrailingBackslash(GetAppDataDir) + 'pixmaps');
  end;
  AddString(DirList, ExcludeTrailingPathDelimiter(gpPixmapPath));
  FDCIconTheme := TIconTheme.Create(gIconTheme, DirList, DC_THEME_NAME);
end;

procedure TPixMapManager.DestroyIconTheme;
begin
{$IF DEFINED(UNIX) AND NOT DEFINED(DARWIN)}
  {$IFDEF LCLGTK2}
  FIconTheme:= nil;
  {$ELSE}
  if Assigned(FIconTheme) then
    FreeAndNil(FIconTheme);
  {$ENDIF}
{$ENDIF}
  FreeThenNil(FDCIconTheme);
end;

{$IF DEFINED(UNIX) AND NOT DEFINED(DARWIN)}

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
  mTime: TFileTime;
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
    if (mbFileAge(gpCfgDir + pixmaps_cache) = mTime) and
       (mbFileAccess(gpCfgDir + pixmaps_cache, fmOpenRead)) and
       (mbFileSize(gpCfgDir + pixmaps_cache) > SizeOf(DWord) * 2) then
    try
      cache := TFileStreamEx.Create(gpCfgDir + pixmaps_cache, fmOpenRead or fmShareDenyWrite);
      if (cache.ReadDWord <> NtoBE(cache_signature)) or
         (cache.ReadDWord <> cache_version) then
      begin
        FreeAndNil(cache);
      end;
    except
      cache:= nil;
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
        try
          cache := TFileStreamEx.Create(gpCfgDir + pixmaps_cache, fmCreate or fmShareDenyWrite);
          try
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
          finally
            FreeAndNil(cache); // Close file
          end;
          mbFileSetTime(gpCfgDir + pixmaps_cache, mTime, 0, 0);
        except
          on E: Exception do
            DCDebug(Format('Error: Cannot save pixmaps cache [%s] : %s',[gpCfgDir + pixmaps_cache, E.Message]));
        end;
      end;

  finally
    FreeAndNil(globs);
    FreeAndNil(generic_icons);
    FreeAndNil(cache);
  end;
end;

function TPixMapManager.GetMimeIcon(AFileExt: String; AIconSize: Integer): PtrInt;
var
  I: Integer;
  node: THTDataNode;
  iconList: TStringList;
begin
  // This function must be called under FPixmapsLock.

  Result := -1;

  // Search for an icon for this file extension.
  node := THTDataNode(FExtToMimeIconName.Find(AFileExt));
  if Assigned(node) then
    begin
      iconList := TStringList(node.Data);
      // Try to load one of the icons in the list.
      for I := 0 to iconList.Count - 1 do
        begin
          Result := CheckAddThemePixmapLocked(iconList.Strings[I], AIconSize);
          if Result <> -1 then break;
        end;
    end;
end;

function TPixMapManager.GetIconByDesktopFile(sFileName: String; iDefaultIcon: PtrInt): PtrInt;
var
  I: PtrInt;
  iniDesktop: TIniFileEx = nil;
  sIconName: String;
begin
  try
    iniDesktop:= TIniFileEx.Create(sFileName, fmOpenRead);
    try
      sIconName:= iniDesktop.ReadString('Desktop Entry', 'Icon', EmptyStr);
    finally
      FreeAndNil(iniDesktop);
    end;
  except
    Exit(iDefaultIcon);
  end;

  {
    Some icon names in .desktop files are specified with an extension,
    even though it is not allowed by the standard unless an absolute path
    to the icon is supplied. We delete this extension here.
  }
  if GetPathType(sIconName) = ptNone then
    sIconName := TIconTheme.CutTrailingExtension(sIconName);

  I:= GetIconByName(sIconName);
  if I < 0 then
    Result:= iDefaultIcon
  else
    Result:= I;
end;

{$ELSEIF DEFINED(DARWIN)}

function TPixMapManager.GetApplicationBundleIcon(sFileName: String;
  iDefaultIcon: PtrInt): PtrInt;
var
  I, J: PtrInt;
  slInfoFile: TStringListEx = nil;
  sTemp,
  sIconName: String;
begin
  Result:= iDefaultIcon;
  slInfoFile:= TStringListEx.Create;
  try
    try
      slInfoFile.LoadFromFile(sFileName + '/Contents/Info.plist');
      sTemp:= slInfoFile.Text;
      I:= Pos('CFBundleIconFile', sTemp);
      if I <= 0 then Exit;
      I:= PosEx('<string>', sTemp, I) + 8;
      J:= PosEx('</string>', sTemp, I);
      sIconName:= Copy(sTemp, I, J - I);
      if not StrEnds(sIconName, '.icns') then
        sIconName:= sIconName + '.icns';
      sIconName:= sFileName + '/Contents/Resources/' + sIconName;
    except
      Exit;
    end;
  finally
    slInfoFile.Free;
  end;

  I:= GetIconByName(sIconName);
  if I >= 0 then Result:= I;
end;

{$ENDIF} // Unix

function TPixMapManager.CheckAddThemePixmapLocked(AIconName: String; AIconSize: Integer): PtrInt;
var
  fileIndex: PtrInt;
{$IFDEF LCLGTK2}
  pbPicture: PGdkPixbuf = nil;
  sIconFileName: String;
{$ELSE}
  bmpBitmap: Graphics.TBitmap;
{$ENDIF}
begin
  // This function must be called under FPixmapsLock.

  fileIndex := FThemePixmapsFileNames.Find(AIconName);
  if fileIndex < 0 then
    begin
    {$IF DEFINED(LCLGTK2) AND DEFINED(UNIX) AND NOT DEFINED(DARWIN)}
      if gShowIcons > sim_standart then
        begin
          pbPicture:= gtk_icon_theme_load_icon(FIconTheme, Pgchar(AIconName),
                                               AIconSize, GTK_ICON_LOOKUP_USE_BUILTIN, nil);
        end;
      // If not found in system theme or using of system theme is disabled look in DC theme.
      if not Assigned(pbPicture) then
        begin
          sIconFileName := FDCIconTheme.FindIcon(AIconName, AIconSize);
          if sIconFileName <> EmptyStr then
            pbPicture := gdk_pixbuf_new_from_file_at_size(
                PChar(sIconFileName), AIconSize, AIconSize, nil);
        end;

      if Assigned(pbPicture) then
        begin
          Result := FPixmapList.Add(pbPicture);
          FThemePixmapsFileNames.Add(AIconName, Pointer(Result));
        end
      else
        Result := -1;
    {$ELSE}
      bmpBitmap := LoadIconThemeBitmapLocked(AIconName, AIconSize);
      if Assigned(bmpBitmap) then
        begin
          Result := FPixmapList.Add(bmpBitmap); // add to list
          FThemePixmapsFileNames.Add(AIconName, Pointer(Result));
        end
      else
        Result := -1;
    {$ENDIF}
    end
  else
    Result := PtrInt(FThemePixmapsFileNames.List[fileIndex]^.Data);
end;

function TPixMapManager.LoadIconThemeBitmapLocked(AIconName: String; AIconSize: Integer): Graphics.TBitmap;
var
  sIconFileName: String;
{$IFDEF LCLGTK2}
  pbPicture: PGdkPixbuf = nil;
{$ENDIF}
begin
  // This function must be called under FPixmapsLock.

{$IF DEFINED(UNIX) AND NOT DEFINED(DARWIN)}
  Result := nil;
  {$IFDEF LCLGTK2}
  if gShowIcons > sim_standart then
    begin
      pbPicture:= gtk_icon_theme_load_icon(FIconTheme, Pgchar(PChar(AIconName)),
                                           AIconSize, GTK_ICON_LOOKUP_USE_BUILTIN, nil);
      if pbPicture <> nil then
        Result := PixBufToBitmap(pbPicture);
    end;
  {$ELSE}
  sIconFileName:= FIconTheme.FindIcon(AIconName, AIconSize);
  if sIconFileName <> EmptyStr then
  begin
    if TScalableVectorGraphics.IsFileExtensionSupported(ExtractFileExt(sIconFileName)) then
      Result := BitmapLoadFromScalable(sIconFileName, AIconSize, AIconSize)
    else
      Result := CheckLoadPixmapFromFile(sIconFileName);
  end;
  {$ENDIF}
  if not Assigned(Result) then
{$ENDIF}
    begin
      sIconFileName:= FDCIconTheme.FindIcon(AIconName, AIconSize);
      if sIconFileName <> EmptyStr then
        Result := CheckLoadPixmapFromFile(sIconFileName)
      else
        Result := nil;
    end;
end;

function TPixMapManager.GetPluginIcon(const AIconName: String; ADefaultIcon: PtrInt): PtrInt;
{$IF DEFINED(MSWINDOWS)}
var
  phIcon: HICON;
  fileIndex: PtrInt;
  AIconSize: Integer;
  phIconLarge : HICON = 0;
  phIconSmall : HICON = 0;
begin
  FPixmapsLock.Acquire;
  try
    // Determine if this file is already loaded.
    fileIndex := FPixmapsFileNames.Find(AIconName);
    if fileIndex >= 0 then
      Result:= PtrInt(FPixmapsFileNames.List[fileIndex]^.Data)
    else begin
      if ExtractIconExW(PWChar(UTF8Decode(AIconName)), 0, phIconLarge, phIconSmall, 1) = 0 then
        Result:= ADefaultIcon
      else begin
        if not ImageList_GetIconSize(FSysImgList, @AIconSize, @AIconSize) then
          AIconSize:= gIconsSize;
        // Get system metrics
        if AIconSize <= GetSystemMetrics(SM_CXSMICON) then
          phIcon:= phIconSmall // Use small icon
        else begin
          phIcon:= phIconLarge // Use large icon
        end;
        if phIcon = 0 then
          Result:= ADefaultIcon
        else begin
          Result:= ImageList_AddIcon(FSysImgList, phIcon) + SystemIconIndexStart;
{$IF DEFINED(LCLQT5)}
          Result:= CheckAddSystemIcon(Result);
{$ENDIF}
        end;
        if (phIconLarge <> 0) then DestroyIcon(phIconLarge);
        if (phIconSmall <> 0) then DestroyIcon(phIconSmall);
      end;
      FPixmapsFileNames.Add(AIconName, Pointer(Result));
    end;
  finally
    FPixmapsLock.Release;
  end;
end;
{$ELSE}
var
  AIcon: TIcon;
  ABitmap: TBitmap;
  AFileName: String;
  AResult: Pointer absolute Result;
begin
  AFileName:= ChangeFileExt(AIconName, '.ico');
  if not mbFileExists(AFileName) then Exit(ADefaultIcon);

  FPixmapsLock.Acquire;
  try
    Result:= FPixmapsFileNames.Find(AFileName);

    if Result >= 0 then
      AResult:= FPixmapsFileNames.List[Result]^.Data
    else begin
{$IF DEFINED(LCLGTK2)}
      AResult := gdk_pixbuf_new_from_file_at_size(PChar(AFileName), gIconsSize, gIconsSize, nil);
      if (AResult = nil) then Exit(ADefaultIcon);
      Result := FPixmapList.Add(AResult);
      FPixmapsFileNames.Add(AFileName, AResult);
{$ELSE}
      AIcon:= TIcon.Create;
      try
        AIcon.LoadFromFile(AFileName);
        AIcon.Current:= AIcon.GetBestIndexForSize(TSize.Create(gIconsSize, gIconsSize));
        ABitmap:= TBitmap.Create;
        try
          BitmapAssign(ABitmap, AIcon);
          Result := FPixmapList.Add(ABitmap);
          FPixmapsFileNames.Add(AFileName, AResult);
        except
          FreeAndNil(ABitmap);
        end;
      except
        Result:= ADefaultIcon;
      end;
      AIcon.Free;
{$ENDIF}
    end;
  finally
    FPixmapsLock.Release;
  end;
end;
{$ENDIF}

{$IFDEF DARWIN}
function TPixMapManager.GetSystemFolderIcon: PtrInt;
var
  FileType: String;
begin
  FileType:= NSFileTypeForHFSTypeCode(kGenericFolderIcon).UTF8String;
  Result:= GetMimeIcon(FileType, gIconsSize);
end;

function TPixMapManager.GetMimeIcon(AFileExt: String; AIconSize: Integer): PtrInt;
var
  I: Integer;
  nData: NSData;
  nImage: NSImage;
  bestRect: NSRect;
  nRepresentations: NSArray;
  nImageRep: NSImageRep;
  WorkStream: TBlobStream;
  tfBitmap: TTiffImage;
  bmBitmap: TBitmap;
begin
  Result:= -1;
  if not FUseSystemTheme then Exit;
  if AIconSize = 24 then AIconSize:= 32;
  nImage:= NSWorkspace.sharedWorkspace.iconForFileType(NSSTR(PChar(AFileExt)));
  // Try to find best representation for requested icon size
  bestRect.origin.x:= 0;
  bestRect.origin.y:= 0;
  bestRect.size.width:= AIconSize;
  bestRect.size.height:= AIconSize;
  nImageRep:= nImage.bestRepresentationForRect_context_hints(bestRect, nil, nil);
  if Assigned(nImageRep) then
  begin
    nImage:= NSImage.Alloc.InitWithSize(nImageRep.Size);
    nImage.AddRepresentation(nImageRep);
  end
  // Try old method
  else begin
    nRepresentations:= nImage.Representations;
    for I:= nRepresentations.Count - 1 downto 0 do
    begin
      nImageRep:= NSImageRep(nRepresentations.objectAtIndex(I));
      if (AIconSize <> nImageRep.Size.Width) then
        nImage.removeRepresentation(nImageRep);
    end;
    if nImage.Representations.Count = 0 then Exit;
  end;
  nData:= nImage.TIFFRepresentation;
  tfBitmap:= TTiffImage.Create;
  WorkStream:= TBlobStream.Create(nData.Bytes, nData.Length);
  try
    tfBitmap.LoadFromStream(WorkStream);
    bmBitmap:= TBitmap.Create;
    try
      bmBitmap.Assign(tfBitmap);
      Result:= FPixmapList.Add(bmBitmap);
    except
      bmBitmap.Free;
    end;
  finally
    tfBitmap.Free;
    nImage.Release;
    WorkStream.Free;
  end;
end;
{$ENDIF}

{$IF DEFINED(MSWINDOWS) and DEFINED(LCLQT5)}
function TPixMapManager.CheckAddSystemIcon(ASystemIndex: PtrInt): PtrInt;
var
  AIcon: HICON;
  ABitmap: Graphics.TBitmap;
begin
  if not FSystemIndexList.TryGetData(ASystemIndex, Result) then
  begin
    Result:= -1;
    AIcon:= ImageList_GetIcon(FSysImgList, ASystemIndex - SystemIconIndexStart, ILD_NORMAL);
    if AIcon <> 0 then
    try
      ABitmap := BitmapCreateFromHICON(AIcon);
      ABitmap.Masked := True; // Need to explicitly set Masked=True, Lazarus issue #0019747
      if (ABitmap.Width <> gIconsSize) or (ABitmap.Height <> gIconsSize) then
        ABitmap:= StretchBitmap(ABitmap, gIconsSize, clWhite, True);
      Result := FPixmapList.Add(ABitmap);
      FSystemIndexList.Add(ASystemIndex, Result);
    finally
      DestroyIcon(AIcon);
    end
  end;
end;
{$ENDIF}

{$IFDEF WINDOWS}
function TPixMapManager.GetIconResourceIndex(const IconPath: String; out IconFile: String; out IconIndex: PtrInt): Boolean;
var
  iPos, iIndex: Integer;
begin
  iPos := Pos(',', IconPath);
  if iPos <> 0 then
    begin
      if TryStrToInt(Copy(IconPath, iPos + 1, Length(IconPath) - iPos), iIndex) and (iIndex >= 0) then
        begin
          IconIndex := iIndex;
          IconFile := Copy(IconPath, 1, iPos - 1);
          Result := FileIsExeLib(IconFile);
        end
      else
        Result := False;
    end
  else
    begin
      IconIndex := 0;
      IconFile := IconPath;
      Result := FileIsExeLib(IconFile);
    end;
end;

function TPixMapManager.GetSystemFolderIcon: PtrInt;
var
  FileInfo: TSHFileInfo;
begin
  if (SHGetFileInfo('nil',
                    FILE_ATTRIBUTE_DIRECTORY,
                    FileInfo,
                    SizeOf(FileInfo),
                    SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES) = 0) then
    Result := -1
  else begin
    Result := FileInfo.iIcon + SystemIconIndexStart;
{$IF DEFINED(LCLQT5)}
    Result := CheckAddSystemIcon(Result);
{$ENDIF}
  end;
end;

function TPixMapManager.GetSystemArchiveIcon: PtrInt;
const
  SIID_ZIPFILE = 105;
var
  psii: TSHStockIconInfo;
  SHGetStockIconInfo: function(siid: Int32; uFlags: UINT; var psii: TSHStockIconInfo): HRESULT; stdcall;
begin
  Result:= -1;
  if (Win32MajorVersion > 5) then
  begin
    Pointer(SHGetStockIconInfo):= GetProcAddress(GetModuleHandle(Shell32), 'SHGetStockIconInfo');
    if Assigned(SHGetStockIconInfo) then
    begin
      psii.cbSize:= SizeOf(TSHStockIconInfo);
      if SHGetStockIconInfo(SIID_ZIPFILE, SHGFI_SYSICONINDEX, psii) = S_OK then
      begin
        Result:= psii.iSysImageIndex + SystemIconIndexStart;
{$IF DEFINED(LCLQT5)}
        Result := CheckAddSystemIcon(Result);
{$ENDIF}
      end;
    end;
  end;
end;

function TPixMapManager.GetSystemExecutableIcon: PtrInt;
var
  FileInfo: TSHFileInfo;
begin
  if (SHGetFileInfo(PAnsiChar('a.exe'),    // Ansi version is enough.
                    FILE_ATTRIBUTE_NORMAL,
                    FileInfo,
                    SizeOf(FileInfo),
                    SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES) = 0) then
    Result := -1
  else begin
    Result := FileInfo.iIcon + SystemIconIndexStart;
{$IF DEFINED(LCLQT5)}
    Result := CheckAddSystemIcon(Result);
{$ENDIF}
  end;
end;

{$ENDIF}

constructor TPixMapManager.Create;
{$IF DEFINED(DARWIN)}
var
  systemVersion: SInt32;
{$ELSEIF DEFINED(MSWINDOWS)}
var
  iIconSize : Integer;
{$ENDIF}
begin
  FExtList := TStringHashListUtf8.Create(True);
  FPixmapsFileNames := TStringHashListUtf8.Create(True);
  FPixmapList := TFPList.Create;

  {$IF DEFINED(DARWIN)}
  FUseSystemTheme:= NSAppKitVersionNumber >= 1038;
  {$ELSEIF DEFINED(UNIX)}
  FExtToMimeIconName := TFPDataHashTable.Create;
  FHomeFolder := IncludeTrailingBackslash(GetHomeDir);
  {$ENDIF}

  FThemePixmapsFileNames := TStringHashListUtf8.Create(True);
  CreateIconTheme;

  {$IFDEF MSWINDOWS}
  for iIconSize:= Low(ICON_SIZES) to High(ICON_SIZES) do
    ICON_SIZES[iIconSize]:= AdjustIconSize(ICON_SIZES[iIconSize], 96);

  if gIconsSize <= ICON_SIZES[0] then
    iIconSize := SHIL_SMALL
  else if gIconsSize <= ICON_SIZES[2] then
    iIconSize := SHIL_LARGE
  else begin
    iIconSize := SHIL_EXTRALARGE;
  end;

  FSysImgList := SHGetSystemImageList(iIconSize);
  {$ENDIF}

  {$IF DEFINED(MSWINDOWS) and DEFINED(LCLQT5)}
  FSystemIndexList:= TPtrIntMap.Create;
  FSystemIndexList.Sorted:= True;
  {$ENDIF}

  FPixmapsLock := syncobjs.TCriticalSection.Create;
end;

destructor TPixMapManager.Destroy;
var
  I : Integer;
  K: TDriveType;
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

  for I := Low(FDriveIconList) to High(FDriveIconList) do
  begin
    with FDriveIconList[I] do
    begin
      for K:= Low(Bitmap) to High(Bitmap) do
        FreeAndNil(Bitmap[K]);
    end;
  end;

  {$IF DEFINED(MSWINDOWS)}
  ImageList_Destroy(FSysImgList);
  {$ELSEIF DEFINED(UNIX) AND NOT DEFINED(DARWIN)}
  for I := 0 to FExtToMimeIconName.HashTable.Count - 1 do
    begin
      nodeList := TFPObjectList(FExtToMimeIconName.HashTable.Items[I]);
      if Assigned(nodeList) then
        for J := 0 to nodeList.Count - 1 do
          TStringList(THtDataNode(nodeList.Items[J]).Data).Free;
    end;

  FreeThenNil(FExtToMimeIconName);
  {$ENDIF}

  {$IF DEFINED(MSWINDOWS) and DEFINED(LCLQT5)}
  FSystemIndexList.Free;
  {$ENDIF}

  DestroyIconTheme;
  FreeThenNil(FThemePixmapsFileNames);
  FreeThenNil(FPixmapsLock);

  inherited Destroy;
end;

procedure TPixMapManager.Load(const sFileName: String);
var
  slPixmapList: TStringListEx;
  s:String;
  sExt, sPixMap:String;
  iekv:integer;
  iPixMap:PtrInt;
  I : Integer;
  iPixmapSize: Integer;
begin
  // This function doesn't need to be synchronized
  // as long as it is called before creating the main form
  // (via LoadPixMapManager in doublecmd.lpr).

  // Load icon themes.
  {$IF DEFINED(UNIX) AND NOT DEFINED(DARWIN)}
  if gShowIcons > sim_standart then
    begin
      LoadMimeIconNames; // For use with GetMimeIcon
  {$IFNDEF LCLGTK2}
      FIconTheme.Load; // Load system icon theme.
  {$ENDIF}
    end;
  {$ENDIF}
  FDCIconTheme.Load; // Load DC theme.

  //  load all drive icons
  FDriveIconList[0].Size := 16;
  FDriveIconList[1].Size := 24;
  FDriveIconList[2].Size := 32;

  for I:= Low(FDriveIconList) to High(FDriveIconList) do
    with FDriveIconList[I] do
    begin
      iPixmapSize := FDriveIconList[I].Size;
      Bitmap[dtFloppy] := LoadIconThemeBitmapLocked('media-floppy', iPixmapSize);
      Bitmap[dtHardDisk] := LoadIconThemeBitmapLocked('drive-harddisk', iPixmapSize);
      Bitmap[dtFlash] := LoadIconThemeBitmapLocked('media-flash', iPixmapSize);
      Bitmap[dtOptical] := LoadIconThemeBitmapLocked('media-optical', iPixmapSize);
      Bitmap[dtNetwork] := LoadIconThemeBitmapLocked('network-wired', iPixmapSize);
      Bitmap[dtVirtual] := LoadIconThemeBitmapLocked('drive-virtual', iPixmapSize);
      Bitmap[dtRemovable] := LoadIconThemeBitmapLocked('drive-removable-media', iPixmapSize);
      Bitmap[dtRemovableUsb] := LoadIconThemeBitmapLocked('drive-removable-media-usb', iPixmapSize);
    end;

  // load emblems
  if gIconsSize = 24 then
    I:= 16
  else
    I:= gIconsSize div 2;
  FiEmblemLinkID:= CheckAddThemePixmap('emblem-symbolic-link', I);
  FiEmblemUnreadableID:= CheckAddThemePixmap('emblem-unreadable', I);

  // add some standard icons
  FiDefaultIconID:=CheckAddThemePixmap('unknown');
  {$IF DEFINED(MSWINDOWS)}
  FiSysDirIconID := GetSystemFolderIcon;
  {$ENDIF}
  {$IF DEFINED(MSWINDOWS) or DEFINED(DARWIN)}
  FiDirIconID := -1;
  if (gShowIcons > sim_standart) and (not (cimFolder in gCustomIcons)) then
    FiDirIconID := GetSystemFolderIcon;
  if FiDirIconID = -1 then
  {$ENDIF}
  FiDirIconID:=CheckAddThemePixmap('folder');
  FiDirLinkIconID:=CheckAddThemePixmap('folder-link');
  FiDirLinkBrokenIconID:=CheckAddThemePixmap('folder-link-broken');
  FiLinkIconID:=CheckAddThemePixmap('link');
  FiLinkBrokenIconID:=CheckAddThemePixmap('link-broken');
  FiUpDirIconID:=CheckAddThemePixmap('go-up');
  {$IFDEF MSWINDOWS}
  FiArcIconID := -1;
  if (gShowIcons > sim_standart) and (not (cimArchive in gCustomIcons)) then
    FiArcIconID := GetSystemArchiveIcon;
  if FiArcIconID = -1 then
  {$ENDIF}
  FiArcIconID := CheckAddThemePixmap('package-x-generic');
  {$IFDEF MSWINDOWS}
  FiExeIconID := -1;
  if gShowIcons > sim_standart then
    FiExeIconID := GetSystemExecutableIcon;
  if FiExeIconID = -1 then
  {$ENDIF}
  FiExeIconID:= CheckAddThemePixmap('application-x-executable');
  FiSortAscID := CheckAddThemePixmap('view-sort-ascending');
  FiSortDescID := CheckAddThemePixmap('view-sort-descending');
  FiHashIconID := CheckAddThemePixmap('text-x-hash');
  gFiOwnDCIcon := CheckAddPixmap(ParamStr(0), gIconsSize);

  { Load icons from "extassoc.xml" }
  for I := 0 to gExts.Count - 1 do
    begin
      iPixMap := CheckAddPixmap(gExts.Items[I].Icon, gIconsSize);
      if iPixMap >= 0 then
        begin
          // set pixmap index for all extensions
          for iekv := 0 to gExts.Items[I].Extensions.Count - 1 do
            begin
              sExt := LowerCase(gExts.Items[I].Extensions[iekv]);
              if FExtList.Find(sExt) < 0 then
                FExtList.Add(sExt, TObject(iPixMap));
            end;
        end
      else
        iPixMap:= FiDefaultIconID;

      gExts.Items[I].IconIndex:= iPixMap;
    end;
  {/ Load icons from "extassoc.xml" }

  // Load icons from pixmaps.txt only if "Only standart icons" enabled
  if (gShowIcons = sim_standart) and mbFileExists(sFileName) then
  try
    slPixmapList:= TStringListEx.Create;
    try
      slPixmapList.LoadFromFile(sFileName);
      for I:= 0 to slPixmapList.Count - 1 do
      begin
        s:= Trim(slPixmapList.Strings[I]);
        iekv := Pos('=',s);
        if iekv = 0 then
          Continue;
        sPixMap := Copy(s, iekv+1, length(s)-iekv);

        // Since DC 0.4.6 filename without path is treated as a MIME type
        // and it shouldn't have an extension. Cut any extension here.
        // Only '.png' were used in previous versions of pixmaps.txt.
        if (GetPathType(sPixMap) = ptNone) and StrEnds(sPixMap, '.png') then
          Delete(sPixMap, Length(sPixMap) - 3, 4);

        iPixMap := CheckAddPixmap(sPixMap);
        if iPixMap >= 0 then
        begin
          sExt := Copy(s, 1, iekv-1);
          if FExtList.Find(sExt) < 0 then
            FExtList.Add(sExt, TObject(iPixMap));
        end;
      end;
    except
      on E: Exception do
      with Application do
      MessageBox(PAnsiChar(E.Message), PAnsiChar(Title), MB_OK or MB_ICONERROR);
    end;
  finally
    slPixmapList.Free;
  end;

  for sExt in HashFileExt do
  begin
    FExtList.Add(sExt, TObject(FiHashIconID));
  end;

  (* Set archive icons *)
  
  for I:=0 to gWCXPlugins.Count - 1 do
    begin
      if gWCXPlugins.Enabled[I] and ((gWCXPlugins.Flags[I] and PK_CAPS_HIDE) <> PK_CAPS_HIDE) then
        begin
          sExt := gWCXPlugins.Ext[I];
          if (Length(sExt) > 0) and (FExtList.Find(sExt) < 0) then
            FExtList.Add(sExt, TObject(FiArcIconID));
        end;
    end; //for

  for I:= 0 to gMultiArcList.Count - 1 do
    begin
      if gMultiArcList.Items[I].FEnabled then
        begin
          sExt := gMultiArcList.Items[I].FExtension;
          if (Length(sExt) > 0) and (FExtList.Find(sExt) < 0) then
            FExtList.Add(sExt, TObject(FiArcIconID));
        end;
    end;

  (* /Set archive icons *)

{$IF NOT DEFINED(DARWIN)}
  LoadApplicationThemeIcon;
{$ENDIF}
end;

function TPixMapManager.GetBitmap(iIndex: PtrInt): Graphics.TBitmap;
var
  PPixmap: Pointer;
  PixmapFromList: Boolean = False;
{$IFDEF LCLWIN32}
  AIcon: HICON;
{$ENDIF}
begin
  FPixmapsLock.Acquire;
  try
    if (iIndex >= 0) and (iIndex < FPixmapList.Count) then
    begin
      PPixmap := FPixmapList[iIndex];
      PixmapFromList := True;
    end;
  finally
    FPixmapsLock.Release;
  end;

  if PixmapFromList then
  begin
{$IFDEF LCLGTK2}
    Result:= PixBufToBitmap(PGdkPixbuf(PPixmap));
{$ELSE}
    // Make a new copy.
    Result := Graphics.TBitmap.Create;
    Result.Assign(Graphics.TBitmap(PPixmap));
{$ENDIF}
  end
  else
{$IFDEF LCLWIN32}
  if iIndex >= SystemIconIndexStart then
    begin
      Result:= nil;
      AIcon:= ImageList_GetIcon(FSysImgList, iIndex - SystemIconIndexStart, ILD_NORMAL);
      if AIcon <> 0 then
      try
        Result := BitmapCreateFromHICON(AIcon);
        Result.Masked := True; // Need to explicitly set Masked=True, Lazarus issue #0019747
      finally
        DestroyIcon(AIcon);
      end
    end
  else
{$ENDIF}
  Result:= nil;
end;

function TPixMapManager.DrawBitmap(iIndex: PtrInt; Canvas : TCanvas; X, Y: Integer) : Boolean;
begin
  Result := DrawBitmap(iIndex, Canvas, X, Y, gIconsSize, gIconsSize); // No bitmap stretching.
end;

function TPixMapManager.DrawBitmapAlpha(iIndex: PtrInt; Canvas: TCanvas; X, Y: Integer): Boolean;
var
  ARect: TRect;
  ABitmap: Graphics.TBitmap;
begin
  ABitmap:= GetBitmap(iIndex);
  Result := Assigned(ABitmap);
  if Result then
  begin
    BitmapAlpha(ABitmap, 0.5);
    ARect := Classes.Bounds(X, Y, gIconsSize, gIconsSize);
    Canvas.StretchDraw(aRect, ABitmap);
    ABitmap.Free;
  end;
end;

function TPixMapManager.DrawBitmap(iIndex: PtrInt; Canvas: TCanvas; X, Y, Width, Height: Integer): Boolean;

  procedure TrySetSize(aWidth, aHeight: Integer);
  begin
    if Width = 0 then
      Width := aWidth;
    if Height = 0 then
      Height := aHeight;
  end;

var
  PPixmap: Pointer;
  PixmapFromList: Boolean = False;
{$IFDEF MSWINDOWS}
  hicn: HICON;
  cx, cy: Integer;
{$ENDIF}
{$IFDEF LCLGTK2}
  pbPicture : PGdkPixbuf;
  iPixbufWidth : Integer;
  iPixbufHeight : Integer;
{$ELSE}
  Bitmap: Graphics.TBitmap;
  aRect: TRect;
{$ENDIF}
begin
  Result := True;

  FPixmapsLock.Acquire;
  try
    if (iIndex >= 0) and (iIndex < FPixmapList.Count) then
    begin
      PPixmap := FPixmapList[iIndex];
      PixmapFromList := True;
    end;
  finally
    FPixmapsLock.Release;
  end;

  if PixmapFromList then
  begin
  {$IFDEF LCLGTK2}
    pbPicture := PGdkPixbuf(PPixmap);
    iPixbufWidth :=  gdk_pixbuf_get_width(pbPicture);
    iPixbufHeight :=  gdk_pixbuf_get_height(pbPicture);
    TrySetSize(iPixbufWidth, iPixbufHeight);
    DrawPixbufAtCanvas(Canvas, pbPicture, 0, 0, X, Y, Width, Height);
  {$ELSE}
    Bitmap := Graphics.TBitmap(PPixmap);
    TrySetSize(Bitmap.Width, Bitmap.Height);
    aRect := Classes.Bounds(X, Y, Width, Height);
    Canvas.StretchDraw(aRect, Bitmap);
  {$ENDIF}
  end
  else
  {$IFDEF LCLWIN32}
  if iIndex >= SystemIconIndexStart then
    try
      if ImageList_GetIconSize(FSysImgList, @cx, @cy) then
        TrySetSize(cx, cy)
      else
        TrySetSize(gIconsSize, gIconsSize);

      if (cx = Width) and (cy = Height) then
        ImageList_Draw(FSysImgList, iIndex - SystemIconIndexStart, Canvas.Handle, X, Y, ILD_TRANSPARENT)
      else
      begin
        hicn:= ImageList_GetIcon(FSysImgList, iIndex - SystemIconIndexStart, ILD_NORMAL);
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

function TPixMapManager.DrawBitmapOverlay(AFile: TDisplayFile; DirectAccess: Boolean; Canvas: TCanvas; X, Y: Integer): Boolean;
var
  I: Integer;
begin
  if AFile.FSFile.IsLink then
    begin
      I:= gIconsSize div 2;
      Result:= DrawBitmap(FiEmblemLinkID, Canvas, X, Y + I, I, I);
      if Assigned(AFile.FSFile.LinkProperty) then
      begin
        if not AFile.FSFile.LinkProperty.IsValid then
          Result:= DrawBitmap(FiEmblemUnreadableID, Canvas, X + I, Y + I, I, I);
      end;
    end
  {$IF DEFINED(MSWINDOWS) OR DEFINED(RabbitVCS)}
  else
    // Windows XP doesn't draw link overlay icon for soft links (don't know about Vista or 7).
    if DirectAccess then
    begin
      if AFile.IconOverlayID >= SystemIconIndexStart then
        Result:= DrawBitmap(AFile.IconOverlayID
                            {$IFDEF RabbitVCS} - SystemIconIndexStart {$ENDIF},
                            Canvas, X, Y);
    end;
  {$ENDIF}
    ;
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

function TPixMapManager.GetIconByFile(AFile: TFile; DirectAccess: Boolean; LoadIcon: Boolean;
  IconsMode: TShowIconsMode; GetIconWithLink: Boolean): PtrInt;
var
  Ext: String;
{$IFDEF MSWINDOWS}
  sFileName: String;
  FileInfo: TSHFileInfoW;
  dwFileAttributes: DWORD;
  uFlags: UINT;
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
      if GetIconWithLink then
      begin
        if (LinkProperty = nil) or LinkProperty.IsValid then
          Result := FiDirLinkIconID
        else
          Result := FiDirLinkBrokenIconID;
        Exit;
      end;
    end;

    if (DirectAccess = False) and (AFile.Attributes = (FILE_ATTRIBUTE_NORMAL or FILE_ATTRIBUTE_VIRTUAL)) and Assigned(AFile.LinkProperty) then
    begin
      if not LoadIcon then
        Result := -1
      else begin
        Result := GetPluginIcon(AFile.LinkProperty.LinkTo, FiDirIconID);
      end;
      Exit;
    end;

    if IsDirectory or IsLinkToDirectory then
    begin
      {$IF DEFINED(MSWINDOWS)}
      if (IconsMode = sim_standart) or
         // Directory has special icon only if it has "read only" or "system" attributes
         // and contains desktop.ini file
         (not (DirectAccess and (IsSysFile or FileIsReadOnly(Attributes)) and mbFileExists(FullPath + '\desktop.ini'))) or
         (ScreenInfo.ColorDepth < 16) then
      {$ELSEIF DEFINED(UNIX) AND NOT DEFINED(DARWIN)}
      if (IconsMode = sim_all_and_exe) and (DirectAccess) then
      begin
        if mbFileAccess(Path + Name + '/.directory', fmOpenRead) then
        begin
          if LoadIcon then
            Result := GetIconByDesktopFile(Path + Name + '/.directory', FiDirIconID)
          else
            Result := -1;
          Exit;
        end
        else if (FHomeFolder = Path) then
        begin
          if LoadIcon then
            Result := CheckAddThemePixmap(GioFileGetIcon(FullPath))
          else
            Result := -1;
          Exit;
        end
        else Exit(FiDirIconID);
      end
      else
      {$ELSEIF DEFINED(DARWIN)}
      if (IconsMode = sim_all_and_exe) and
         (DirectAccess and (ExtractFileExt(FullPath) = '.app')) then
        begin
          if LoadIcon then
            Result := GetApplicationBundleIcon(FullPath, FiDirIconID)
          else
            Result := -1;
          Exit;
        end
      else
      {$ENDIF}
        begin
          Exit(FiDirIconID);
        end;
    end
    else // not directory
    begin
      if IsLink and GetIconWithLink then
      begin
        if (LinkProperty = nil) or LinkProperty.IsValid then
          Exit(FiLinkIconID)
        else
          Exit(FiLinkBrokenIconID);
      end;

      if (Extension = '') then
      begin
        {$IF DEFINED(UNIX) AND NOT DEFINED(DARWIN)}
        if IconsMode = sim_all_and_exe then
        begin
          if DirectAccess and (Attributes and S_IXUGO <> 0) then
          begin
            if not LoadIcon then
              Result := -1
            else begin
              Ext := GioFileGetIcon(FullPath);
              if Ext = 'application-x-sharedlib' then
                Result := FiExeIconID
              else
                Result := CheckAddThemePixmap(Ext);
            end;
            Exit;
          end;
        end;
        {$ENDIF}
        Exit(FiDefaultIconID);
      end;

      Ext := UTF8LowerCase(Extension);

      {$IF DEFINED(MSWINDOWS)}
      if IconsMode <> sim_all_and_exe then
        begin
          if Ext = 'exe' then
            Exit(FiExeIconID)
          else if Ext = 'lnk' then
            Exit(FiLinkIconID)
          else if Ext = 'url' then
            Exit(FiLinkIconID)
          else if Ext = 'ico' then
            Exit(FiDefaultIconID)
        end;
      {$ELSEIF DEFINED(UNIX) AND NOT DEFINED(DARWIN)}
      if IconsMode = sim_all_and_exe then
        begin
          if DirectAccess and ((Ext = 'desktop') or (Ext = 'directory')) then
            begin
              if LoadIcon then
                Result := GetIconByDesktopFile(Path + Name, FiDefaultIconID)
              else
                Result := -1;
              Exit;
            end;
        end;
      {$ENDIF}

      FPixmapsLock.Acquire;
      try
        Result := FExtList.Find(Ext);
        if Result >= 0 then
          Exit(PtrInt(PtrUInt(FExtList.List[Result]^.Data)));

        if IconsMode <= sim_standart then
          Exit(FiDefaultIconID);

        {$IF DEFINED(UNIX)}

        if LoadIcon = False then
          Exit(-1);

        Result := GetMimeIcon(Ext, gIconsSize);
        if Result < 0 then
          Result := FiDefaultIconID;

        // Default icon should also be associated with the extension
        // because it will be faster to find next time.
        FExtList.Add(Ext, Pointer(Result));

        {$ENDIF}

      finally
        FPixmapsLock.Release;
      end;
    end;

    {$IF DEFINED(MSWINDOWS)}

    if DirectAccess then
      begin
        if LoadIcon = False then
          Exit(-1);

        dwFileAttributes := 0;
        uFlags := SHGFI_SYSICONINDEX;
        sFileName := FullPath;
      end
    else
      begin
        // This is fast, so do it even if LoadIcon is false.
        dwFileAttributes := FILE_ATTRIBUTE_NORMAL;
        uFlags := SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES;
        sFileName := Name;
      end;

    if (SHGetFileInfoW(PWideChar(UTF8Decode(sFileName)),
                       dwFileAttributes,
                       FileInfo,
                       SizeOf(FileInfo),
                       uFlags) = 0) then
    begin
      // Could not retrieve icon.
      if IsDirectory then
        Result := FiDirIconID
      else
        Result := FiDefaultIconID;
    end
    else
    begin
      Result := FileInfo.iIcon + SystemIconIndexStart;

{$IF DEFINED(LCLQT5)}
      FPixmapsLock.Acquire;
      try
        Result := CheckAddSystemIcon(Result);
      finally
        FPixmapsLock.Release;
      end;
{$ENDIF}

      if IsDirectory then
      begin
        // In the fact the folder does not have a special icon
        if (cimFolder in gCustomIcons) and (Result = FiSysDirIconID) then
          Result := FiDirIconID;
      end
      else if (Ext <> 'exe') and
        (Ext <> 'ico') and
        (Ext <> 'ani') and
        (Ext <> 'cur') and
        (Ext <> 'lnk') and
        (Ext <> 'url') then
      begin
        FPixmapsLock.Acquire;
        try
          FExtList.Add(Ext, Pointer(Result));
        finally
          FPixmapsLock.Release;
        end;
      end;
    end;

    {$ENDIF}
  end;
end;

{$IF DEFINED(MSWINDOWS)}
function TPixMapManager.GetIconOverlayByFile(AFile: TFile; DirectAccess: Boolean): PtrInt;
begin
  if DirectAccess then
    Result:= SHGetOverlayIconIndex(AFile.Path, AFile.Name) + SystemIconIndexStart
  else
    Result:= -1;
end;
{$ELSEIF DEFINED(RabbitVCS)}
function TPixMapManager.GetIconOverlayByFile(AFile: TFile; DirectAccess: Boolean): PtrInt;
var
  Emblem: String;
begin
  if RabbitVCS and DirectAccess then
  begin
    Emblem:= CheckStatus(AFile.FullPath);
    if Length(Emblem) = 0 then Exit(0);
    Result:= CheckAddThemePixmap(Emblem);
    Result:= IfThen(Result < 0, 0, Result + SystemIconIndexStart);
  end
  else
    Result:= 0;
end;
{$ENDIF}

function TPixMapManager.GetIconByName(const AIconName: String): PtrInt;
begin
  Result := CheckAddPixmap(AIconName, gIconsSize);
end;

function TPixMapManager.GetThemeIcon(const AIconName: String; AIconSize: Integer): Graphics.TBitmap;
var
  ABitmap: Graphics.TBitmap;
begin
  Result:= LoadIconThemeBitmap(AIconName, AIconSize);
  if Assigned(Result) then
  begin
    if (Result.Width > AIconSize) or (Result.Height > AIconSize) then
    begin
      ABitmap:= Graphics.TBitmap.Create;
      ABitmap.SetSize(AIconSize, AIconSize);
      Stretch(Result, ABitmap, ResampleFilters[2].Filter, ResampleFilters[2].Width);
      Result.Free; Result:= ABitmap;
    end;
  end;
end;

function TPixMapManager.GetDriveIcon(Drive : PDrive; IconSize : Integer; clBackColor : TColor) : Graphics.TBitmap;
{$IFDEF MSWINDOWS}
var
  SFI: TSHFileInfoW;
  uFlags: UINT;
  iIconSmall,
  iIconLarge: Integer;
{$ENDIF}
begin
  if Drive^.DriveType = dtVirtual then
  begin
    Result := GetBuiltInDriveIcon(Drive, IconSize, clBackColor);
    Exit;
  end;
  Result := nil;
{$IFDEF MSWINDOWS}
  if ScreenInfo.ColorDepth < 15 then Exit;
  if (not (cimDrive in gCustomIcons)) and (ScreenInfo.ColorDepth > 16) then
    begin
      SFI.hIcon := 0;
      Result := Graphics.TBitMap.Create;
      iIconLarge:= GetSystemMetrics(SM_CXICON);
      iIconSmall:= GetSystemMetrics(SM_CXSMICON);

      if (IconSize <= iIconSmall) then
        uFlags := SHGFI_SMALLICON  // Use small icon
      else begin
        uFlags := SHGFI_LARGEICON; // Use large icon
      end;

      if (SHGetFileInfoW(PWideChar(UTF8Decode(Drive^.Path)), 0, SFI,
                         SizeOf(SFI), uFlags or SHGFI_ICON) <> 0) then
      begin
        if (SFI.hIcon <> 0) then
        try
          Result:= BitmapCreateFromHICON(SFI.hIcon);
          Result.Masked := True; // Need to explicitly set Masked=True, Lazarus issue #0019747
          if (IconSize <> iIconSmall) and (IconSize <> iIconLarge) then // non standart icon size
            Result := StretchBitmap(Result, IconSize, clBackColor, True);
        finally
          DestroyIcon(SFI.hIcon);
        end;
      end;
    end // not gCustomDriveIcons
  else
{$ENDIF}
    begin
      Result := GetBuiltInDriveIcon(Drive, IconSize, clBackColor);
    end;

  if Assigned(Result) and (gDiskIconsAlpha in [1..99]) and (not Drive^.IsMounted) then
  begin
    BitmapAlpha(Result, gDiskIconsAlpha / 100);
  end;
end;

function TPixMapManager.GetBuiltInDriveIcon(Drive : PDrive; IconSize : Integer; clBackColor : TColor) : Graphics.TBitmap;
var
  DriveIconListIndex: Integer;
  ABitmap: Graphics.TBitmap;
begin
{$IFDEF MSWINDOWS}
  if ScreenInfo.ColorDepth < 15 then Exit(nil);
{$ENDIF}
  case IconSize of
  16: // Standart 16x16 icon size
    DriveIconListIndex := 0;
  24:  // Standart 24x24 icon size
    DriveIconListIndex := 1;
  32:  // Standart 32x32 icon size
    DriveIconListIndex := 2;
  else  // for non standart icon size use more large icon for stretch
    DriveIconListIndex := 2;
  end;
  with FDriveIconList[DriveIconListIndex] do
  begin
    if Assigned(Bitmap[Drive^.DriveType]) then
      ABitmap:= Bitmap[Drive^.DriveType]
    else begin
      ABitmap:= Bitmap[dtHardDisk];
    end;
  end;
  //  if need stretch icon
  if (IconSize <> 16) and (IconSize <> 24) and (IconSize <> 32) then
    begin
      Result := StretchBitmap(ABitmap, IconSize, clBackColor, False);
    end
  else
    begin
      Result := Graphics.TBitmap.Create;
      Result.Assign(ABitmap);
    end;
  // 'Bitmap' should not be freed, because it only points to DriveIconList.
end;

{$IF NOT DEFINED(DARWIN)}

procedure TPixMapManager.LoadApplicationThemeIcon;
var
  AIcon: TIcon;
  SmallIcon, LargeIcon: Graphics.TBitmap;
begin
  LargeIcon:= LoadIconThemeBitmapLocked('doublecmd', GetSystemMetrics(SM_CXICON));
  SmallIcon:= LoadIconThemeBitmapLocked('doublecmd', GetSystemMetrics(SM_CXSMICON));
  if Assigned(LargeIcon) or Assigned(SmallIcon) then
  begin
    AIcon:= TIcon.Create;
    if Assigned(SmallIcon) then
    begin
      AIcon.Add(pf32bit, SmallIcon.Height, SmallIcon.Width);
      AIcon.AssignImage(SmallIcon);
      SmallIcon.Free;
    end;
    if Assigned(LargeIcon) then
    begin
      AIcon.Add(pf32bit, LargeIcon.Height, LargeIcon.Width);
      AIcon.Current := AIcon.Current + 1;
      AIcon.AssignImage(LargeIcon);
      LargeIcon.Free;
    end;
    Application.Icon.Assign(AIcon);
    AIcon.Free;
  end;
end;

{$ENDIF}

function TPixMapManager.GetDefaultDriveIcon(IconSize : Integer; clBackColor : TColor) : Graphics.TBitmap;
var
  Drive: TDrive = (DisplayName: ''; Path: ''; DriveLabel: ''; DeviceId: '';
                   DriveType: dtHardDisk; DriveSize: 0; FileSystem: '';
                   IsMediaAvailable: True; IsMediaEjectable: False;
                   IsMediaRemovable: False; IsMounted: True; AutoMount: True);
begin
  Result := GetBuiltInDriveIcon(@Drive, IconSize, clBackColor);
end;

function TPixMapManager.GetArchiveIcon(IconSize: Integer; clBackColor : TColor) : Graphics.TBitmap;
begin
  Result := GetBitmap(FiArcIconID);
  if Assigned(Result) then
  begin
    //  if need stretch icon
    if (IconSize <> gIconsSize) then
      begin
        Result := StretchBitmap(Result, IconSize, clBackColor, True);
      end;
  end;
end;

function TPixMapManager.GetFolderIcon(IconSize: Integer; clBackColor: TColor): Graphics.TBitmap;
begin
  Result := GetBitmap(FiDirIconID);
  if Assigned(Result) then
  begin
    //  if need stretch icon
    if (IconSize <> gIconsSize) then
      begin
        Result := StretchBitmap(Result, IconSize, clBackColor, True);
      end;
  end;
end;

function TPixMapManager.GetDefaultIcon(AFile: TFile): PtrInt;
begin
  if AFile.IsDirectory then
    Result := FiDirIconID
  else if UTF8LowerCase(AFile.Extension) = 'exe' then
    Result := FiExeIconID
  else
    Result := FiDefaultIconID;
end;

procedure LoadPixMapManager;
begin
  DCDebug('Creating PixmapManager');
  PixMapManager:=TPixMapManager.Create;
  PixMapManager.Load(gpCfgDir + 'pixmaps.txt');
end;

initialization

finalization

  if Assigned(PixMapManager) then
  begin
    DCDebug('Shutting down PixmapManager');
    FreeAndNil(PixMapManager);
  end;

end.

