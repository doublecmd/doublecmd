{
   Double Commander
   -------------------------------------------------------------------------
   Fast pixmap memory manager and loader

   Copyright (C) 2004 Radek Cervinka (radek.cervinka@centrum.cz)
   Copyright (C) 2006-2025 Alexander Koblov (alexx2000@mail.ru)

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
{$IF DEFINED(LCLGTK2) AND DEFINED(UNIX) AND NOT (DEFINED(DARWIN) OR DEFINED(HAIKU))}
  {$DEFINE GTK2_FIX}
{$ENDIF}

// Use freedesktop.org specifications
{$IF DEFINED(UNIX) AND NOT (DEFINED(DARWIN) OR DEFINED(HAIKU))}
  {$DEFINE XDG}
{$ENDIF}

uses
  Classes, SysUtils,
  Graphics, ImgList, Controls, ExtCtrls, Buttons, syncobjs, uFileSorting, DCStringHashListUtf8,
  uFile, uIconTheme, uDrive, uDisplayFile, uGlobs, uDCReadPSD, uOSUtils, FPImage,
  LCLVersion, uVectorImage, uMultiArc, uFileSource, WfxPlugin
  {$IF DEFINED(MSWINDOWS)}
  , ShlObj
  {$ELSEIF DEFINED(MSWINDOWS) and DEFINED(LCLQT5)}
  , fgl
  {$ELSEIF DEFINED(UNIX)}
  , DCFileAttributes
    {$IF DEFINED(DARWIN)}
    , CocoaAll, MacOSAll, CocoaUtils, uDarwinUtil, uMyDarwin
    {$ELSEIF NOT DEFINED(HAIKU)}
    , Math, Contnrs, uGio, uXdg
      {$IFDEF GTK2_FIX}
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
    FiDirLinkBrokenIconID : PtrInt;
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
    FiEmblemPinned: PtrInt;
    FiEmblemOnline: PtrInt;
    FiEmblemOffline: PtrInt;
    FiShortcutIconID: PtrInt;
    FOneDrivePath: TStringList;
    {$ELSEIF DEFINED(DARWIN)}
    FUseSystemTheme: Boolean;
    {$ELSEIF DEFINED(UNIX) AND NOT DEFINED(HAIKU)}
    {en
       Maps file extension to MIME icon name(s).
    }
    FExtToMimeIconName: TFPDataHashTable;
    {$IFDEF GTK2_FIX}
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

    function AddSpecial(ALow, AHigh: PtrInt): PtrInt;

    {en
       Same as LoadBitmap but displays a warning if pixmap file doesn't exist.
    }
    function CheckLoadPixmapFromFile(const AIconName: String) : TBitmap;
    {en
       If path is absolute tries to load bitmap and add to storage.
       If path is relative it tries to load theme icon and add to storage.
       This function should only be called under FPixmapLock.
    }
    function CheckAddPixmapLocked(AIconName: String; AIconSize : Integer): PtrInt;
    {en
       If path is absolute tries to load bitmap and add to storage.
       If path is relative it tries to load theme icon and add to storage.
       Safe to call without a lock.
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
       Loads an icon from default theme (DCTheme) and adds it to storage.
    }
    function AddDefaultThemePixmap(const AIconName: String; AIconSize: Integer = 0) : PtrInt;
    {en
       Loads an icon from the theme
    }
    function LoadThemeIcon(AIconTheme: TIconTheme; const AIconName: String; AIconSize: Integer): TBitmap;
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
    function GetShellFolderIcon(AFile: TFile): PtrInt;
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
    function GetSystemFileIcon(const FileName: String; dwFileAttributes: DWORD = 0): PtrInt;
    function GetSystemFolderIcon: PtrInt;
    function GetSystemArchiveIcon: PtrInt;
    function GetSystemShortcutIcon: PtrInt; inline;
    function GetSystemExecutableIcon: PtrInt; inline;
  {$ENDIF}
  {$IF DEFINED(UNIX) AND NOT (DEFINED(DARWIN) OR DEFINED(HAIKU))}
    function GetSystemFolderIcon: PtrInt;
    function GetSystemArchiveIcon: PtrInt;
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
    function GetSystemExecutableIcon: PtrInt;
    function GetMimeIcon(AFileExt: String; AIconSize: Integer): PtrInt;
    function LoadImageFileBitmap( const filename:String; const size:Integer ): TBitmap;
  {$ENDIF}
    function GetBuiltInDriveIcon(Drive : PDrive; IconSize : Integer; clBackColor : TColor) : Graphics.TBitmap;

{$IF NOT (DEFINED(DARWIN) OR DEFINED(HAIKU))}
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
    function DrawBitmap(AFile: TDisplayFile; Canvas : TCanvas; X, Y: Integer) : Boolean;
    function DrawBitmapAlpha(AFile: TDisplayFile; Canvas : TCanvas; X, Y: Integer) : Boolean;
    {en
       Draws bitmap stretching it if needed to Width x Height.
       If Width is 0 then full bitmap width is used.
       If Height is 0 then full bitmap height is used.
       @param(iIndex
              Index of pixmap manager's bitmap.)
    }
    function DrawBitmap(iIndex: PtrInt; Canvas : TCanvas; X, Y, Width, Height: Integer) : Boolean;
    function DrawBitmap(AFile: TDisplayFile; Canvas : TCanvas; X, Y, Width, Height: Integer) : Boolean;
    {en
       Draws overlay bitmap for a file.
       @param(AFile
              File for which is needed to draw the overlay icon.)
       @param(DirectAccess
              Whether the file is on a directly accessible file source.)
    }
    function DrawBitmapOverlay(AFile: TDisplayFile; DirectAccess: Boolean; Canvas : TCanvas; X, Y: Integer) : Boolean;
    function CheckAddPixmap(AUniqueName: String; AIconSize: Integer; ADestroy: Boolean; TheIcon: PWfxIcon; out AIcon: TBitmap): PtrInt;
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
    function GetIconByFile(constref AFileSource: IFileSource; AFile: TDisplayFile; DirectAccess: Boolean; LoadIcon: Boolean;
                           IconsMode: TShowIconsMode; GetIconWithLink: Boolean): PtrInt; overload;
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
    function CheckAddFileUniqueIcon(AFullPath: String; AIconSize : Integer = 0): PtrInt;
    {$ENDIF}
    function GetIconByName(const AIconName: String): PtrInt;
    function GetThemeIcon(const AIconName: String; AIconSize: Integer) : Graphics.TBitmap;
    function GetDriveIcon(Drive : PDrive; IconSize : Integer; clBackColor : TColor; LoadIcon: Boolean = True) : Graphics.TBitmap;
    function GetDefaultDriveIcon(IconSize : Integer; clBackColor : TColor) : Graphics.TBitmap;
    function GetArchiveIcon(IconSize: Integer; clBackColor : TColor) : Graphics.TBitmap;
    function GetFolderIcon(IconSize: Integer; clBackColor : TColor) : Graphics.TBitmap;
    {en
       Returns default icon for a file.
       For example default folder icon for folder, default executable icon for *.exe, etc.
    }
    function GetDefaultIcon(AFile: TFile): PtrInt;
{$IF DEFINED(MSWINDOWS)}
    procedure ClearSystemCache;
{$ENDIF}
  end;

var
  PixMapManager: TPixMapManager = nil;

var
  ICON_SIZES: array [0..3] of Integer = (16, 24, 32, 48);

procedure LoadPixMapManager;

function AdjustIconSize(ASize: Integer; APixelsPerInch: Integer): Integer;

function StretchBitmap(var bmBitmap : Graphics.TBitmap; iIconSize : Integer;
                       clBackColor : TColor; bFreeAtEnd : Boolean = False) : Graphics.TBitmap;

procedure AssignRetinaBitmapForControl(
  const button: TCustomSpeedButton;
  const imageSize: Integer;
  bitmap: Graphics.TBitmap);

procedure AssignRetinaBitmapForControl(
  const imageControl: TCustomImage;
  const imageSize: Integer;
  bitmap: Graphics.TBitmap);

{$IF DEFINED(DARWIN)}
function NSImageToTBitmap( const image:NSImage ): TBitmap;
function getBestNSImageWithSize( const srcImage:NSImage; const size:Integer ): NSImage;
{$ENDIF}

implementation

uses
  GraphType, LCLIntf, LCLType, LCLProc, Forms, uGlobsPaths, WcxPlugin, uClassesEx,
  DCStrUtils, uDCUtils, uFileSystemFileSource, uReSample, uDebug, uFileSourceProperty,
  IntfGraphics, DCOSUtils, DCClassesUtf8, LazUTF8, uGraphics, uHash, uSysFolders
  {$IFDEF GTK2_FIX}
    , uPixMapGtk, gdk2pixbuf, gdk2, glib2
  {$ENDIF}
  {$IFDEF MSWINDOWS}
    , ActiveX, CommCtrl, ShellAPI, Windows, DCFileAttributes, uBitmap, uGdiPlus,
      DCConvertEncoding, uShlObjAdditional, uShellFolder, uMyWindows,
      uShellFileSourceUtil
  {$ELSE}
    , StrUtils, Types, DCBasicTypes
  {$ENDIF}
  {$IFDEF RabbitVCS}
  , uRabbitVCS
  {$ENDIF}
  ;

{$IF DEFINED(MSWINDOWS)}
type
  TBitmap = Graphics.TBitmap;
{$ENDIF}

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
begin
  if (bmBitmap.Height > 0) and (bmBitmap.Width > 0) and
     ((iIconSize <> bmBitmap.Height) or (iIconSize <> bmBitmap.Width)) then
  begin
    Result := Graphics.TBitMap.Create;
    try
      Result.SetSize(iIconSize, iIconSize);
      Stretch(bmBitmap, Result, ResampleFilters[2].Filter, ResampleFilters[2].Width);
      if bFreeAtEnd then FreeAndNil(bmBitmap);
    except
      FreeAndNil(Result);
      raise;
    end;
  end
  // Don't need to stretch.
  else if bFreeAtEnd then
  begin
    Result := bmBitmap;
    bmBitmap := nil;
  end
  else begin
    Result := Graphics.TBitMap.Create;
    try
      Result.Assign(bmBitmap);
    except
      FreeAndNil(Result);
      raise;
    end;
  end;
end;

procedure AssignRetinaBitmapForControl(
  const button: TCustomSpeedButton;
  const imageSize: Integer;
  bitmap: Graphics.TBitmap);
var
  ScaleFactor: Double;
  oldImages: TCustomImageList;
  images: TImageList;
  imageListSize: Integer;
begin
  oldImages:= button.Images;
  ScaleFactor := findScaleFactorByControl(button);
  imageListSize := Round(imageSize * ScaleFactor);
  images := TImageList.Create(button);
  images.Width := imageListSize;
  images.Height := imageListSize;
  images.Scaled := (ScaleFactor > 1.0);
  images.Add(bitmap, nil);
  button.ImageWidth := imageSize;
  button.Images := images;
  button.ImageIndex := 0;
  FreeAndNil(bitmap);
  FreeAndNil(oldImages);
end;

procedure AssignRetinaBitmapForControl(
  const imageControl: TCustomImage;
  const imageSize: Integer;
  bitmap: Graphics.TBitmap);
var
  oldImages: TCustomImageList;
  images: TImageList;
  imageListSize: Integer;
begin
  oldImages:= imageControl.Images;
  imageListSize := Round(imageSize * findScaleFactorByControl(imageControl));
  images := TImageList.Create(imageControl);
  images.Width := imageListSize;
  images.Height := imageListSize;
  images.Add(bitmap, nil);
  imageControl.ImageWidth := imageSize;
  imageControl.Images := images;
  imageControl.ImageIndex := 0;
  FreeAndNil(bitmap);
  FreeAndNil(oldImages);
end;

{ TPixMapManager }

{ TPixMapManager.LoadBitmapFromFile }
function TPixMapManager.LoadBitmapFromFile(AIconFileName: String; out ABitmap: Graphics.TBitmap): Boolean;
var
  {$IFDEF GTK2_FIX}
  pbPicture : PGdkPixbuf;
  {$ELSE}
  Picture: TPicture;
  {$ENDIF}
begin
  Result:= False;

  {$IFDEF GTK2_FIX}
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
      ABitmap.Assign(Picture.Graphic);

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
  AIcon: TIcon;
  iIndex : PtrInt;
  FileExt: String;
  GraphicClass: TGraphicClass;
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
      if ExtractIconExW(PWChar(CeUtf8ToUtf16(IconFileName)), iIconIndex, phIconLarge, phIconSmall, 1) = 2 then // if extracted both icons
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
            if fromWhatItWasLoaded<> nil then fromWhatItWasLoaded^ := fwbwlResourceFileExtracted;
          end;
          DestroyIcon(phIconLarge);
          DestroyIcon(phIconSmall);
        end;
    end  // GetIconResourceIndex
  else
{$ENDIF}
    begin
      FileExt := ExtractOnlyFileExt(sFileName);
      // if file is graphic
      GraphicClass:= GetGraphicClassForFileExtension(FileExt);
      if (GraphicClass <> nil) and mbFileExists(sFileName) then
      begin
        if (GraphicClass = TIcon) then
        begin
          AIcon:= TIcon.Create;
          try
            AIcon.LoadFromFile(sFileName);
            AIcon.Current:= AIcon.GetBestIndexForSize(TSize.Create(iIconSize, iIconSize));
            bmStandartBitmap:= Graphics.TBitmap.Create;
            try
              if AIcon.RawImage.Description.AlphaPrec <> 0 then
                BitmapAssign(bmStandartBitmap, AIcon)
              else
                BitmapConvert(AIcon, bmStandartBitmap);
            except
              FreeAndNil(bmStandartBitmap);
            end;
          except
            on E: Exception do
              DCDebug(Format('Error: Cannot load icon [%s] : %s',[sFileName, E.Message]));
          end;
          AIcon.Free;
        end
        else if (GraphicClass = TScalableVectorGraphics) then
        begin
          Stretch := False;
          bmStandartBitmap := TScalableVectorGraphics.CreateBitmap(sFileName, iIconSize, iIconSize)
        end
        else begin
          LoadBitmapFromFile(sFileName, bmStandartBitmap);
        end;
        if fromWhatItWasLoaded <> nil then fromWhatItWasLoaded^ := fwbwlGraphicFile;
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

function TPixMapManager.CheckAddPixmapLocked(AIconName: String; AIconSize: Integer): PtrInt;
var
  fileIndex: PtrInt;
  {$IFDEF GTK2_FIX}
  pbPicture : PGdkPixbuf;
  {$ELSE}
  bmpBitmap: Graphics.TBitmap;
  {$ENDIF}
begin
  Result:= -1;
  if Length(AIconName) = 0 then Exit;

  if GetPathType(AIconName) = ptAbsolute then
    begin
        // Determine if this file is already loaded.
        fileIndex := FPixmapsFileNames.Find(AIconName);
        if fileIndex < 0 then
        begin
        {$IFDEF GTK2_FIX}
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
            {$IFDEF DARWIN}
            bmpBitmap := LoadImageFileBitmap(AIconName, AIconSize);
            {$ELSE}
            bmpBitmap := LoadBitmapEnhanced(AIconName, AIconSize, False, clNone, nil);
            {$ENDIF}
            if Assigned(bmpBitmap) then
            begin
              // MacOS' high resolution screen parameters are different from other operating systems
              {$IF NOT DEFINED(DARWIN)}
              // Shrink big bitmaps before putting them into PixmapManager,
              // to speed up later drawing.
              if (bmpBitmap.Width > 48) or (bmpBitmap.Height > 48) then
              begin
                bmpBitmap := StretchBitmap(bmpBitmap, AIconSize, clBlack, True);
              end;
              {$ENDIF}
              Result := FPixmapList.Add(bmpBitmap);
              FPixmapsFileNames.Add(AIconName, Pointer(Result));
            end;
        {$ENDIF}
          end
        else begin
            Result:= PtrInt(FPixmapsFileNames.List[fileIndex]^.Data);
        end;
    end
  else begin
      Result := CheckAddThemePixmapLocked(AIconName, AIconSize);
  end;
end;

function TPixMapManager.CheckAddPixmap(AIconName: String; AIconSize : Integer): PtrInt;
begin
  AIconName := ReplaceEnvVars(AIconName);
  if AIconSize = 0 then AIconSize := gIconsSize;

  FPixmapsLock.Acquire;
  try
    Result := CheckAddPixmapLocked(AIconName, AIconSize);
  finally
    FPixmapsLock.Release;
  end;
end;

procedure TPixMapManager.CreateIconTheme;
var
  DirList: array of string;
begin
{$IF DEFINED(UNIX) AND NOT (DEFINED(DARWIN) OR DEFINED(HAIKU))}
  {$IFDEF GTK2_FIX}
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
{$IF DEFINED(UNIX) AND NOT (DEFINED(DARWIN) OR DEFINED(HAIKU))}
  {$IFDEF GTK2_FIX}
  FIconTheme:= nil;
  {$ELSE}
  if Assigned(FIconTheme) then
    FreeAndNil(FIconTheme);
  {$ENDIF}
{$ENDIF}
  FreeAndNil(FDCIconTheme);
end;

function TPixMapManager.AddSpecial(ALow, AHigh: PtrInt): PtrInt;
var
  X, Y: Integer;
  AIcon: TBitmap;
  ABitmap: TBitmap;
  Source, Target: TLazIntfImage;
begin
  AIcon:= GetBitmap(ALow);

  Target:= TLazIntfImage.Create(AIcon.Width, AIcon.Height, [riqfRGB, riqfAlpha]);
  try
{$if lcl_fullversion < 2020000}
    Target.CreateData;
{$endif}
    Target.FillPixels(colTransparent);

    Source:= TLazIntfImage.Create(AIcon.RawImage, False);
    try
      Target.CopyPixels(Source);
    finally
      Source.Free;
    end;

    ABitmap:= GetBitmap(AHigh);
    try
      Source:= TLazIntfImage.Create(ABitmap.RawImage, False);
      try
        X:= (AIcon.Width - ABitmap.Width);
        Y:= (AIcon.Height - ABitmap.Height);
        BitmapMerge(Target, Source, X, Y);
      finally
        Source.Free;
      end;
    finally
      ABitmap.Free;
    end;
{$IF DEFINED(GTK2_FIX)}
    Result := FPixmapList.Add(ImageToPixBuf(Target));
    AIcon.Free;
{$ELSE}
    BitmapAssign(AIcon, Target);
    Result := FPixmapList.Add(AIcon);
{$ENDIF}
  finally
    Target.Free;
  end;
end;

{$IF DEFINED(UNIX) AND NOT (DEFINED(DARWIN) OR DEFINED(HAIKU))}

procedure TPixMapManager.LoadMimeIconNames;
const
  mime_globs = 'globs';
  mime_icons = 'icons';
  mime_generic_icons = 'generic-icons';
  pixmaps_cache = 'pixmaps.cache';
  cache_signature: DWord = $44435043; // 'DCPC'
  cache_version: DWord = 1;
var
  I, J, K: Integer;
  mTime: TFileTime;
  LocalMime: String;
  iconsList: TStringList;
  nodeList: TFPObjectList;
  node: THTDataNode = nil;
  cache: TFileStreamEx = nil;
  EntriesCount, IconsCount: Cardinal;
  GlobalMime: String = '/usr/share/mime/';
  sMimeType, sMimeIconName, sExtension: String;

  procedure LoadGlobs(const APath: String);
  var
    I: Integer;
    globs: TStringListEx = nil;
    icons: TStringListEx = nil;
    generic_icons: TStringListEx = nil;
  begin
    if mbFileAccess(APath + mime_globs, fmOpenRead) then
    try
      // Load mapping: MIME type -> file extension.
      globs:= TStringListEx.Create;
      globs.NameValueSeparator:= ':';
      globs.LoadFromFile(APath + mime_globs);

      // Try to load mapping: MIME type -> MIME icon name.
      if mbFileExists(APath + mime_icons) then
      begin
        icons:= TStringListEx.Create;
        icons.NameValueSeparator:= ':';
        icons.LoadFromFile(APath + mime_icons);
        if (icons.Count = 0) then FreeAndNil(icons);
      end;

      // Try to load mapping: MIME type -> generic MIME icon name.
      if mbFileExists(APath + mime_generic_icons) then
      begin
        generic_icons:= TStringListEx.Create;
        generic_icons.NameValueSeparator:= ':';
        generic_icons.LoadFromFile(APath + mime_generic_icons);
        if (generic_icons.Count = 0) then FreeAndNil(generic_icons);
      end;

      // Create mapping: file extension -> list of MIME icon names.
      for I:= 0 to globs.Count - 1 do
        if (globs.Strings[I]    <> '') and   // bypass empty lines
           (globs.Strings[I][1] <> '#') then // and comments
        begin
          sMimeType := globs.Names[I];
          sExtension:= ExtractFileExt(globs.ValueFromIndex[I]);

          // Support only extensions, not full file name masks.
          if (sExtension <> '') and (sExtension <> '.*') then
          begin
            Delete(sExtension, 1, 1);

            node := THTDataNode(FExtToMimeIconName.Find(sExtension));
            if Assigned(node) then
              iconsList := TStringList(node.Data)
            else begin
              iconsList := TStringList.Create;
              FExtToMimeIconName.Add(sExtension, iconsList);
              Inc(EntriesCount);
            end;

            if Assigned(icons) then
            begin
              J := icons.IndexOfName(sMimeType);
              if J <> -1 then
              begin
                sMimeIconName := icons.ValueFromIndex[J]; // found icon
                if iconsList.IndexOf(sMimeIconName) < 0 then
                  iconsList.Add(sMimeIconName);
              end;
            end;

            sMimeIconName:= StringReplace(sMimeType, '/', '-', []);
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
    finally
      globs.Free;
      icons.Free;
      generic_icons.Free;
    end;
  end;

begin
  LocalMime:= IncludeTrailingBackslash(GetUserDataDir) + 'mime/';

  mTime:= Max(mbFileAge(LocalMime + mime_globs),
              mbFileAge(GlobalMime + mime_globs));

  // Try to load from cache.
  if (mbFileAge(gpCfgDir + pixmaps_cache) = mTime) and
     (mbFileAccess(gpCfgDir + pixmaps_cache, fmOpenRead)) and
     (mbFileSize(gpCfgDir + pixmaps_cache) > SizeOf(DWord) * 2) then
  try
    cache := TFileStreamEx.Create(gpCfgDir + pixmaps_cache, fmOpenRead or fmShareDenyWrite);
    try
      if (cache.ReadDWord = NtoBE(cache_signature)) and
         (cache.ReadDWord = cache_version) then
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
          begin
            iconsList.Add(cache.ReadAnsiString);
          end;
        end;

        Exit;
      end;
    finally
      FreeAndNil(cache);
    end;
  except
    on E: Exception do
      DCDebug(Format('Error: Cannot load from pixmaps cache [%s] : %s',[gpCfgDir + pixmaps_cache, E.Message]));
  end;

  EntriesCount := 0;
  LoadGlobs(LocalMime);
  LoadGlobs(GlobalMime);

  // save to cache
  if EntriesCount > 0 then
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
          Result := CheckAddPixmapLocked(iconList.Strings[I], AIconSize);
          if Result <> -1 then break;
        end;
    end;
end;

function TPixMapManager.GetSystemFolderIcon: PtrInt;
var
  AIconName: String;
begin
  AIconName:= GioMimeGetIcon('inode/directory');
  if Length(AIconName) = 0 then
    Result:= -1
  else begin
    Result:= CheckAddPixmap(AIconName);
  end;
  if (Result < 0) and (AIconName <> 'folder') then
  begin
    Result:= CheckAddThemePixmap('folder');
  end;
end;

function TPixMapManager.GetSystemArchiveIcon: PtrInt;
begin
  Result:= CheckAddThemePixmap('package-x-generic');
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

function getBestNSImageWithSize( const srcImage:NSImage; const size:Integer ): NSImage;
var
  bestRect: NSRect;
  bestImageRep: NSImageRep;
  bestImage: NSImage;
begin
  Result := nil;
  if srcImage=nil then exit;

  bestRect.origin.x := 0;
  bestRect.origin.y := 0;
  bestRect.size.width := size;
  bestRect.size.height := size;
  bestImageRep:= srcImage.bestRepresentationForRect_context_hints( bestRect, nil, nil );

  bestImage:= NSImage.Alloc.InitWithSize( bestImageRep.size );
  bestImage.AddRepresentation( bestImageRep );

  Result := bestImage;
end;

function getImageFileBestNSImage( const filename:NSString; const size:Integer ): NSImage;
var
  srcImage: NSImage;
begin
  Result:= nil;
  try
    srcImage:= NSImage.Alloc.initByReferencingFile( filename );
    Result:= getBestNSImageWithSize( srcImage, size );
  finally
    if Assigned(srcImage) then srcImage.release;
  end;
end;

function NSImageToTBitmap( const image:NSImage ): TBitmap;
var
  nsbitmap: NSBitmapImageRep;
  tempData: NSData;
  tempStream: TBlobStream = nil;
  tempBitmap: TPortableNetworkGraphic = nil;
  bitmap: TBitmap;
begin
  Result:= nil;
  if image=nil then exit;

  try
    nsbitmap:= NSBitmapImageRep.imageRepWithData( image.TIFFRepresentation );
    tempData:= nsbitmap.representationUsingType_properties( NSPNGFileType, nil );
    tempStream:= TBlobStream.Create( tempData.Bytes, tempData.Length );
    tempBitmap:= TPortableNetworkGraphic.Create;
    tempBitmap.LoadFromStream( tempStream );
    bitmap:= TBitmap.Create;
    bitmap.Assign( tempBitmap );
    Result:= bitmap;
  finally
    FreeAndNil(tempBitmap);
    FreeAndNil(tempStream);
  end;
end;

function TPixMapManager.LoadImageFileBitmap( const filename:String; const size:Integer ): TBitmap;
var
  image: NSImage;
begin
  Result:= nil;
  image:= nil;
  try
    image:= getImageFileBestNSImage( StringToNSString(filename), size );
    if Assigned(image) then Result:= NSImageToTBitmap( image );
  finally
    if Assigned(image) then image.release;
  end;
end;

function TPixMapManager.CheckAddFileUniqueIcon(AFullPath: String;
  AIconSize: Integer): PtrInt;
var
  fileIndex: PtrInt;
  image: NSImage;
  bmpBitmap: Graphics.TBitmap;
  oldBmpBitmap: Graphics.TBitmap;
  key: String;
begin
  Result:= -1;
  if AIconSize = 0 then AIconSize := gIconsSize;

  key:= AFullPath;
  if AIconSize <> gIconsSize then
    key:= key + '@' + IntToStr(AIconSize);

  FPixmapsLock.Acquire;
  try
    image:= getMacOSFileUniqueIcon(AFullPath);
    if image = nil then
      Exit;

    image:= getBestNSImageWithSize(image, AIconSize);
    bmpBitmap:= NSImageToTBitmap(image);

    fileIndex := FPixmapsFileNames.Find(key);
    if fileIndex >= 0 then begin
      Result:= PtrInt(FPixmapsFileNames.List[fileIndex]^.Data);
      oldBmpBitmap:= Graphics.TBitmap(FPixmapList[Result]);
      FPixmapList[Result]:= bmpBitmap;
      oldBmpBitmap.Free;
    end else begin
      Result := FPixmapList.Add(bmpBitmap);
      FPixmapsFileNames.Add(key, Pointer(Result));
    end;
  finally
    FPixmapsLock.Release;
  end;
end;

{$ENDIF} // Unix

function TPixMapManager.CheckAddThemePixmapLocked(AIconName: String; AIconSize: Integer): PtrInt;
var
  fileIndex: PtrInt;
{$IFDEF GTK2_FIX}
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
    {$IF DEFINED(GTK2_FIX) AND DEFINED(UNIX) AND NOT DEFINED(DARWIN)}
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

function TPixMapManager.AddDefaultThemePixmap(const AIconName: String;
  AIconSize: Integer): PtrInt;
var
  bmpBitmap: Pointer;
{$IF DEFINED(GTK2_FIX)}
  sIconFileName: String;
{$ENDIF}
begin
  if AIconSize = 0 then AIconSize := gIconsSize;
{$IF DEFINED(GTK2_FIX)}
  sIconFileName := FDCIconTheme.FindIcon(AIconName, AIconSize);
  if Length(sIconFileName) = 0 then Exit(-1);
  bmpBitmap := gdk_pixbuf_new_from_file_at_size(PChar(sIconFileName), AIconSize, AIconSize, nil);
{$ELSE}
  bmpBitmap := LoadThemeIcon(FDCIconTheme, AIconName, AIconSize);
{$ENDIF}
  if (bmpBitmap = nil) then
    Result := -1
  else begin
    Result := FPixmapList.Add(bmpBitmap); // add to list
    FThemePixmapsFileNames.Add(AIconName, Pointer(Result));
  end;
end;

function TPixMapManager.LoadThemeIcon(AIconTheme: TIconTheme; const AIconName: String; AIconSize: Integer): Graphics.TBitmap;
var
  FileName: String;
  bitmapSize: Integer;
begin
  bitmapSize := Round(AIconSize * findScaleFactorByFirstForm());
  FileName:= AIconTheme.FindIcon(AIconName, bitmapSize, 1);
  if FileName = EmptyStr then Exit(nil);
  if TScalableVectorGraphics.IsFileExtensionSupported(ExtractFileExt(FileName)) then
    Result := TScalableVectorGraphics.CreateBitmap(FileName, bitmapSize, bitmapSize)
  else
  begin
    Result := CheckLoadPixmapFromFile(FileName);
    if Assigned(Result) then begin
      Result:= StretchBitmap(Result, bitmapSize, clNone, True);
    end;
  end;
end;

function TPixMapManager.LoadIconThemeBitmapLocked(AIconName: String; AIconSize: Integer): Graphics.TBitmap;
{$IFDEF GTK2_FIX}
var
  pbPicture: PGdkPixbuf = nil;
{$ENDIF}
begin
  // This function must be called under FPixmapsLock.

{$IF DEFINED(UNIX) AND NOT (DEFINED(DARWIN) OR DEFINED(HAIKU))}
  Result := nil;
  // Try to load icon from system theme
  if gShowIcons > sim_standart then
  begin
  {$IFDEF GTK2_FIX}
    pbPicture:= gtk_icon_theme_load_icon(FIconTheme, Pgchar(PChar(AIconName)),
                                         AIconSize, GTK_ICON_LOOKUP_USE_BUILTIN, nil);
    if pbPicture <> nil then
      Result := PixBufToBitmap(pbPicture);
  {$ELSE}
  Result:= LoadThemeIcon(FIconTheme, AIconName, AIconSize);
  {$ENDIF}
  end;
  if not Assigned(Result) then
{$ENDIF}
    Result:= LoadThemeIcon(FDCIconTheme, AIconName, AIconSize);
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
      if ExtractIconExW(PWChar(CeUtf8ToUtf16(AIconName)), 0, phIconLarge, phIconSmall, 1) = 0 then
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
{$IF DEFINED(GTK2_FIX)}
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

function TPixMapManager.GetSystemExecutableIcon: PtrInt;
begin
  Result:= GetMimeIcon('public.unix-executable', gIconsSize);
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
function TPixMapManager.GetShellFolderIcon(AFile: TFile): PtrInt;
const
  uFlags: UINT = SHGFI_SYSICONINDEX or SHGFI_PIDL;
var
  FileInfo: TSHFileInfoW;
begin
  if (SHGetFileInfoW(PWideChar(TFileShellProperty(AFile.LinkProperty).Item),
                     0, {%H-}FileInfo, SizeOf(FileInfo), uFlags) <> 0) then
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
    Exit;
  end;
  // Could not retrieve the icon
  if AFile.IsDirectory then
    Result := FiDirIconID
  else begin
    Result := FiDefaultIconID;
  end;
end;

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

function TPixMapManager.GetSystemFileIcon(const FileName: String; dwFileAttributes: DWORD): PtrInt;
var
  FileInfo: TSHFileInfo;
begin
  if (SHGetFileInfo(PAnsiChar(FileName),    // Ansi version is enough.
                    FILE_ATTRIBUTE_NORMAL or dwFileAttributes,
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
var
  psii: TSHStockIconInfo;
begin
  if not SHGetStockIconInfo(SIID_ZIPFILE, SHGFI_SYSICONINDEX, psii) then
    Result:= -1
  else begin
    Result:= psii.iSysImageIndex + SystemIconIndexStart;
{$IF DEFINED(LCLQT5)}
    Result := CheckAddSystemIcon(Result);
{$ENDIF}
  end;
end;

function TPixMapManager.GetSystemShortcutIcon: PtrInt;
begin
  Result:= GetSystemFileIcon('a.url');
end;

function TPixMapManager.GetSystemExecutableIcon: PtrInt;
begin
  Result:= GetSystemFileIcon('a.exe');
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
  {$ELSEIF DEFINED(UNIX) AND NOT DEFINED(HAIKU)}
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

  FOneDrivePath := TStringList.Create;
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
{$IF DEFINED(UNIX) AND NOT (DEFINED(DARWIN) OR DEFINED(HAIKU))}
  J : Integer;
  nodeList: TFPObjectList;
{$ENDIF}
begin
  if Assigned(FPixmapList) then
  begin
    for I := 0 to FPixmapList.Count - 1 do
      if Assigned(FPixmapList.Items[I]) then
  {$IFDEF GTK2_FIX}
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
  FOneDrivePath.Free;
  ImageList_Destroy(FSysImgList);
  {$ELSEIF DEFINED(UNIX) AND NOT (DEFINED(DARWIN) OR DEFINED(HAIKU))}
  for I := 0 to FExtToMimeIconName.HashTable.Count - 1 do
    begin
      nodeList := TFPObjectList(FExtToMimeIconName.HashTable.Items[I]);
      if Assigned(nodeList) then
        for J := 0 to nodeList.Count - 1 do
          TStringList(THtDataNode(nodeList.Items[J]).Data).Free;
    end;

  FreeAndNil(FExtToMimeIconName);
  {$ENDIF}

  {$IF DEFINED(MSWINDOWS) and DEFINED(LCLQT5)}
  FSystemIndexList.Free;
  {$ENDIF}

  DestroyIconTheme;
  FreeAndNil(FThemePixmapsFileNames);
  FreeAndNil(FPixmapsLock);

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
  {$IF DEFINED(XDG)}
  if gShowIcons > sim_standart then
    begin
      LoadMimeIconNames; // For use with GetMimeIcon
  {$IFNDEF GTK2_FIX}
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
  if (Win32MajorVersion >= 10) then
  begin
    FiEmblemPinned:= CheckAddThemePixmap('emblem-cloud-pinned', I);
    FiEmblemOnline:= CheckAddThemePixmap('emblem-cloud-online', I);
    FiEmblemOffline:= CheckAddThemePixmap('emblem-cloud-offline', I);
    // Microsoft OneDrive folders
    GetOneDriveFolders(FOneDrivePath);
  end;
  FiShortcutIconID := -1;
  if gShowIcons > sim_standart then
    FiShortcutIconID := GetSystemShortcutIcon;
  if FiShortcutIconID = -1 then
    FiShortcutIconID := CheckAddThemePixmap('text-html');
  {$ENDIF}
  {$IF NOT DEFINED(HAIKU)}
  FiDirIconID := -1;
  if (gShowIcons > sim_standart) and (not (cimFolder in gCustomIcons)) then
    FiDirIconID := GetSystemFolderIcon;
  if FiDirIconID = -1 then
  {$ENDIF}
  FiDirIconID:= AddDefaultThemePixmap('folder');
  FiDirLinkBrokenIconID:= AddSpecial(FiDirIconID, FiEmblemUnreadableID);
  FiLinkBrokenIconID:= AddSpecial(FiDefaultIconID, FiEmblemUnreadableID);
  FiUpDirIconID:= CheckAddThemePixmap('go-up');
  {$IF DEFINED(MSWINDOWS) OR DEFINED(XDG)}
  FiArcIconID := -1;
  if (gShowIcons > sim_standart) and (not (cimArchive in gCustomIcons)) then
    FiArcIconID := GetSystemArchiveIcon;
  if FiArcIconID = -1 then
  {$ENDIF}
  FiArcIconID := AddDefaultThemePixmap('package-x-generic');
  {$IF DEFINED(MSWINDOWS) OR DEFINED(DARWIN)}
  FiExeIconID := -1;
  if gShowIcons > sim_standart then
    FiExeIconID := GetSystemExecutableIcon;
  if FiExeIconID = -1 then
  {$ENDIF}
  FiExeIconID:= CheckAddThemePixmap('application-x-executable');
  FiSortAscID := CheckAddThemePixmap('view-sort-ascending');
  FiSortDescID := CheckAddThemePixmap('view-sort-descending');
  FiHashIconID := CheckAddThemePixmap('text-x-hash');

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

  {$IF DEFINED(DARWIN)}
  if gShowIcons <> sim_all_and_exe then begin
  {$ENDIF}
  
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
        if gMultiArcList.Items[I].FEnabled and not (mafHide in gMultiArcList.Items[I].FFlags) then
          begin
            sExt := gMultiArcList.Items[I].FExtension;
            if (Length(sExt) > 0) and (FExtList.Find(sExt) < 0) then
              FExtList.Add(sExt, TObject(FiArcIconID));
          end;
      end;

  {$IF DEFINED(DARWIN)}
  end;
  {$ENDIF}

  (* /Set archive icons *)

{$IF NOT (DEFINED(DARWIN) OR DEFINED(HAIKU))}
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
{$IFDEF GTK2_FIX}
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

function TPixMapManager.DrawBitmap(AFile: TDisplayFile; Canvas: TCanvas; X, Y: Integer): Boolean;
begin
  Result := DrawBitmap(AFile, Canvas, X, Y, gIconsSize, gIconsSize); // No bitmap stretching.
end;

function TPixMapManager.DrawBitmapAlpha(AFile: TDisplayFile; Canvas: TCanvas; X, Y: Integer): Boolean;
var
  ARect: TRect;
  IconID: PtrInt;
  ABitmap: Graphics.TBitmap;
begin
  if Assigned(AFile.Icon) then
  begin
    ABitmap:= TBitmap.Create;
    ABitmap.Assign(AFile.Icon);
  end
  else begin
    if AFile.IconID < 0 then
      IconID:= GetDefaultIcon(AFile.FSFile)
    else begin
      IconID:= AFile.IconID;
    end;
    ABitmap:= GetBitmap(IconID);
  end;
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
{$IFDEF GTK2_FIX}
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
  {$IFDEF GTK2_FIX}
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
  {$IFDEF MSWINDOWS}
  if iIndex >= SystemIconIndexStart then
    try
      if ImageList_GetIconSize(FSysImgList, @cx, @cy) then
        TrySetSize(cx, cy)
      else
        TrySetSize(gIconsSize, gIconsSize);

      {$IF DEFINED(LCLWIN32)}
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
      {$ELSEIF DEFINED(LCLQT5)}
      hicn:= ImageList_GetIcon(FSysImgList, iIndex - SystemIconIndexStart, ILD_NORMAL);
      try
        Bitmap:= BitmapCreateFromHICON(hicn);
        aRect := Classes.Bounds(X, Y, Width, Height);
        Canvas.StretchDraw(aRect, Bitmap);
      finally
        FreeAndNil(Bitmap);
        DestroyIcon(hicn);
      end
      {$ENDIF}
    except
      Result:= False;
    end;

  {$ELSE}
    Result:= False;
  {$ENDIF}
end;

function TPixMapManager.DrawBitmap(AFile: TDisplayFile; Canvas: TCanvas; X, Y, Width, Height: Integer): Boolean;
var
  aRect: TRect;
  IconID: PtrInt;
begin
  if (AFile.Icon = nil) then
  begin
    // Draw default icon if there is no icon for the file
    if AFile.IconID < 0 then
      IconID:= GetDefaultIcon(AFile.FSFile)
    else begin
      IconID:= AFile.IconID;
    end;
    Result:= DrawBitmap(IconID, Canvas, X, Y, Width, Height);
  end
  else begin
    if Width = 0 then Width:= AFile.Icon.Width;
    if Height = 0 then Height:= AFile.Icon.Height;
    aRect:= Classes.Bounds(X, Y, Width, Height);
    Canvas.StretchDraw(aRect, AFile.Icon);
  end;
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
    if DirectAccess then
    begin
      if AFile.IconOverlayID >= SystemIconIndexStart then
        Result:= DrawBitmap(AFile.IconOverlayID
                            {$IFDEF RabbitVCS} - SystemIconIndexStart {$ENDIF},
                            Canvas, X, Y)
      {$IF DEFINED(MSWINDOWS)}
      // Special case for OneDrive
      else if AFile.IconOverlayID > 0 then
      begin
        I:= gIconsSize div 2;
        Result:= DrawBitmap(AFile.IconOverlayID, Canvas, X, Y + I, I, I);
      end;
      {$ENDIF}
    end;
  {$ENDIF}
    ;
end;

function TPixMapManager.CheckAddPixmap(AUniqueName: String; AIconSize: Integer;
  ADestroy: Boolean; TheIcon: PWfxIcon; out AIcon: TBitmap): PtrInt;
var
  Index: PtrInt;
  Picture: TPicture;
  Stream: TBlobStream;
begin
  AIcon:= nil;
  // Icon has a unique name
  if Length(AUniqueName) > 0 then
  begin
    FPixmapsLock.Acquire;
    try
      // Try to find in the cache
      Index:= FPixmapsFileNames.Find(AUniqueName);
      if (Index >= 0) then
      begin
        if ADestroy then
        begin
          case TheIcon^.Format of
{$IF DEFINED(MSWINDOWS)}
            FS_ICON_FORMAT_HICON: DestroyIcon(HICON(TheIcon^.Data));
{$ENDIF}
            FS_ICON_FORMAT_BINARY: TheIcon^.Free(TheIcon^.Data);
          end;
        end;
        Exit(PtrInt(FPixmapsFileNames.List[Index]^.Data));
      end;
    finally
      FPixmapsLock.Release;
    end;
  end;
  Result:= -1;

  case TheIcon^.Format of
{$IF DEFINED(MSWINDOWS)}
    FS_ICON_FORMAT_HICON:
    begin
      AIcon:= BitmapCreateFromHICON(HICON(TheIcon^.Data));
      if ADestroy then DestroyIcon(HICON(TheIcon^.Data));
    end;
{$ENDIF}
    FS_ICON_FORMAT_FILE:
    begin
      Result:= CheckAddPixmap(AUniqueName, AIconSize);
    end;
    FS_ICON_FORMAT_BINARY:
    begin
      Picture:= TPicture.Create;
      try
        Stream:= TBlobStream.Create(TheIcon^.Data, TheIcon^.Size);
        try
          Picture.LoadFromStream(Stream);
          AIcon:= Graphics.TBitmap.Create;
          BitmapAssign(AIcon, TRasterImage(Picture.Graphic));
        finally
          Stream.Free;
        end;
      except
        // Ignore;
      end;
      Picture.Free;
      if ADestroy then TheIcon^.Free(TheIcon^.Data);
    end;
  end;
  // Icon has a unique name, save to the cache
  if Assigned(AIcon) and (Length(AUniqueName) > 0) then
  begin
    FPixmapsLock.Acquire;
    try
      Result := FPixmapList.Add(AIcon);
      FPixmapsFileNames.Add(AUniqueName, Pointer(Result));
    finally
      FPixmapsLock.Release;
    end;
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

function TPixMapManager.GetIconByFile(AFile: TFile; DirectAccess: Boolean; LoadIcon: Boolean;
                                      IconsMode: TShowIconsMode; GetIconWithLink: Boolean): PtrInt;
var
  Ext: String;
{$IFDEF MSWINDOWS}
  sFileName: String;
  FileInfo: TSHFileInfoW;
  dwFileAttributes: DWORD;
  uFlags: UINT;
const
  FILE_ATTRIBUTE_ICON = FILE_ATTRIBUTE_READONLY or FILE_ATTRIBUTE_SYSTEM;
  FILE_ATTRIBUTE_SHELL = FILE_ATTRIBUTE_DEVICE or FILE_ATTRIBUTE_VIRTUAL;
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

    if IsLinkToDirectory and GetIconWithLink then
    begin
      if Assigned(LinkProperty) and not LinkProperty.IsValid then
        Exit(FiDirLinkBrokenIconID);
    end;

    if (DirectAccess = False) then
    begin
      if (AFile.Attributes = (FILE_ATTRIBUTE_NORMAL or FILE_ATTRIBUTE_VIRTUAL)) and Assigned(AFile.LinkProperty) then
      begin
        if not LoadIcon then
          Result := -1
        else begin
          Result := GetPluginIcon(AFile.LinkProperty.LinkTo, FiDirIconID);
        end;
        Exit;
      end
      else if (AFile.Attributes = (FILE_ATTRIBUTE_OFFLINE or FILE_ATTRIBUTE_VIRTUAL)) and Assigned(AFile.LinkProperty) then
      begin
        if not LoadIcon then
          Result := -1
        else begin
          Result := CheckAddPixmap(AFile.LinkProperty.LinkTo);
          if Result < 0 then Result := FiDirIconID;
        end;
        Exit;
      end
      {$IF DEFINED(MSWINDOWS)}
      else if (AFile.Attributes and FILE_ATTRIBUTE_SHELL = FILE_ATTRIBUTE_SHELL) and Assigned(AFile.LinkProperty) then
      begin
        if not LoadIcon then
          Result := -1
        else begin
          Result:= GetShellFolderIcon(AFile);
        end;
        Exit;
      end;
      {$ENDIF}
    end;

    {$IF DEFINED(DARWIN)}
    if DirectAccess and (IconsMode = sim_all_and_exe) then
    begin
      Result:= checkAddFileUniqueIcon(FullPath);
      if Result >= 0 then
        Exit;
    end;
    {$ENDIF}

    if IsDirectory or IsLinkToDirectory then
    begin
      {$IF DEFINED(MSWINDOWS)}
      if (IconsMode < sim_all_and_exe) or
         // Directory can has a special icon only when it has a "read only" or "system" attribute
         (not (DirectAccess and ((Attributes and FILE_ATTRIBUTE_ICON) <> 0))) or
         (ScreenInfo.ColorDepth < 16) then
      {$ELSEIF DEFINED(UNIX) AND NOT (DEFINED(DARWIN) OR DEFINED(HAIKU))}
      if (IconsMode = sim_all_and_exe) and (DirectAccess) then
      begin
        if not LoadIcon then Exit(-1);

        if mbFileAccess(Path + Name + '/.directory', fmOpenRead) then
        begin
          Result := GetIconByDesktopFile(Path + Name + '/.directory', FiDirIconID);
          Exit;
        end
        else if (FHomeFolder = Path) then
        begin
          Result := CheckAddThemePixmap(GioFileGetIcon(FullPath));
          Exit;
        end
        else Exit(FiDirIconID);
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
        if Assigned(LinkProperty) and not LinkProperty.IsValid then
          Exit(FiLinkBrokenIconID);
      end;

      if (Extension = '') then
      begin
        {$IF DEFINED(UNIX) AND NOT DEFINED(HAIKU)}
        if IconsMode = sim_all_and_exe then
        begin
          if DirectAccess and (Attributes and S_IXUGO <> 0) then
          begin
            if not LoadIcon then
              Result := -1
            else begin
              {$IF DEFINED(DARWIN)}
                Result := FiExeIconID;
              {$ELSE}
                Ext := GioFileGetIcon(FullPath);
                if Ext = 'application-x-sharedlib' then
                  Result := FiExeIconID
                else
                  Result := CheckAddThemePixmap(Ext);
              {$ENDIF}
            end;
            Exit;
          end;
        end;
        {$ENDIF}
        Exit(FiDefaultIconID);
      end;

      Ext := UTF8LowerCase(Extension);

      {$IF DEFINED(MSWINDOWS)}
      if (IconsMode > sim_standart) and (Win32MajorVersion >= 10) then
      begin
        if (AFile.Attributes and FILE_ATTRIBUTE_ENCRYPTED <> 0) then
        begin
          if (IconsMode = sim_all) or
             ((Ext <> 'exe') and (Ext <> 'ico') and
              (Ext <> 'ani') and (Ext <> 'cur')) then
          begin
            if (IconsMode = sim_all) and
               ((Ext = 'ico') or (Ext = 'ani') or (Ext = 'cur')) then
              Result:= GetSystemFileIcon('aaa', AFile.Attributes)
            else begin
              Result:= GetSystemFileIcon(AFile.Name, AFile.Attributes);
            end;
            if Result > -1 then Exit;
          end;
        end;
      end;
      if IconsMode <> sim_all_and_exe then
        begin
          if Ext = 'exe' then
            Exit(FiExeIconID)
          else if Ext = 'lnk' then
            Exit(FiDefaultIconID)
          else if Ext = 'url' then
            Exit(FiShortcutIconID)
        end;
      {$ELSEIF DEFINED(UNIX) AND NOT (DEFINED(DARWIN) OR DEFINED(HAIKU))}
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

        {$IF DEFINED(MSWINDOWS)}
        if IconsMode = sim_all then
        begin
          if (Ext = 'ico') or (Ext = 'ani') or (Ext = 'cur') then
            Exit(FiDefaultIconID)
        end
        else
        {$ENDIF}

        if IconsMode <= sim_standart then
          Exit(FiDefaultIconID);

        {$IF DEFINED(UNIX) AND NOT DEFINED(HAIKU)}

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

    if (SHGetFileInfoW(PWideChar(CeUtf8ToUtf16(sFileName)),
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

function TPixMapManager.GetIconByFile(constref AFileSource: IFileSource; AFile: TDisplayFile;
  DirectAccess: Boolean; LoadIcon: Boolean; IconsMode: TShowIconsMode; GetIconWithLink: Boolean): PtrInt;
var
  ABitmap: TBitmap;
begin
  if Assigned(AFile.Icon) then Exit(-1);

  if (fspCustomIcon in AFileSource.Properties) and AFileSource.IsPathAtRoot(AFile.FSFile.Path) then
  begin
    if AFile.FSFile.Name = '..' then
    begin
      Result := FiUpDirIconID;
      Exit;
    end;

    Result:= AFileSource.GetCustomIcon(AFile.FSFile, gIconsSize, ABitmap);
    if (Result >= 0) then Exit;
    if Assigned(ABitmap) then
    begin
      AFile.Icon:= ABitmap;
      Exit(-1);
    end;
  end;

  Result:= GetIconByFile(AFile.FSFile, DirectAccess, LoadIcon, IconsMode, GetIconWithLink);
end;

{$IF DEFINED(MSWINDOWS)}
procedure TPixMapManager.ClearSystemCache;
var
  I: Integer;
  IData: IntPtr;
  AData: Pointer absolute IData;
begin
  FPixmapsLock.Acquire;
  try
    for I:= FExtList.Count - 1 downto 0 do
    begin
      AData:= FExtList.List[I]^.Data;
      if (IData >= SystemIconIndexStart) and (IData <> FiArcIconID) then
      begin
        FExtList.Remove(I);
      end;
    end;
  finally
    FPixmapsLock.Release;
  end;
end;

function TPixMapManager.GetIconOverlayByFile(AFile: TFile; DirectAccess: Boolean): PtrInt;
var
  Index: Integer;
begin
  if not DirectAccess then Exit(-1);
  Result:= SHGetOverlayIconIndex(AFile.Path, AFile.Name);
  if Result >= 0 then begin
    Result += SystemIconIndexStart;
  end
  // Special case for OneDrive
  else if (Win32MajorVersion >= 10) then
  begin
    for Index:= 0 to FOneDrivePath.Count - 1 do
    begin
      if IsInPath(FOneDrivePath[Index], AFile.Path, True, True) then
      begin
        if AFile.Attributes and FILE_ATTRIBUTE_PINNED <> 0 then
          Result:= FiEmblemPinned
        else if AFile.Attributes and FILE_ATTRIBUTE_RECALL_ON_DATA_ACCESS <> 0 then
          Result:= FiEmblemOnline
        else begin
          Result:= SHGetStorePropertyValue(AFile.FullPath, PKEY_StorageProviderState);
          case Result of
            1:
              Result:= FiEmblemOnline;
            2:
              Result:= FiEmblemOffline;
            3:
              Result:= FiEmblemPinned;
            else
              Result:= 0;
          end;
        end;
        Exit;
      end;
    end;
    Result:= 0;
  end
  else
    Result:= 0;
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

function TPixMapManager.GetDriveIcon(Drive : PDrive; IconSize : Integer; clBackColor : TColor; LoadIcon: Boolean) : Graphics.TBitmap;
{$IFDEF MSWINDOWS}
var
  PIDL: PItemIDList;
  SFI: TSHFileInfoW;
  uFlags: UINT;
  iIconSmall,
  iIconLarge: Integer;
  psii: TSHStockIconInfo;
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
      if (Win32MajorVersion < 6) and (not LoadIcon) and (Drive^.DriveType = dtNetwork) then
      begin
        Result := GetBuiltInDriveIcon(Drive, IconSize, clBackColor);
        Exit;
      end;

      SFI.hIcon := 0;
      iIconLarge:= GetSystemMetrics(SM_CXICON);
      iIconSmall:= GetSystemMetrics(SM_CXSMICON);

      if (IconSize <= iIconSmall) then
        uFlags := SHGFI_SMALLICON  // Use small icon
      else begin
        uFlags := SHGFI_LARGEICON; // Use large icon
      end;
      uFlags := uFlags or SHGFI_ICON;

      if (Drive^.DriveType = dtSpecial) then
      begin
        if Succeeded(SHParseDisplayName(PWideChar(CeUtf8ToUtf16(Drive^.DeviceId)), nil, PIDL, 0, nil)) then
        begin
          SHGetFileInfoW(PWideChar(PIDL), 0, SFI, SizeOf(SFI), uFlags or SHGFI_PIDL);
          CoTaskMemFree(PIDL);
        end;
      end
      else if (not LoadIcon) and (Drive^.DriveType = dtNetwork) and SHGetStockIconInfo(SIID_DRIVENET, uFlags, psii) then
        SFI.hIcon:= psii.hIcon
      else if (SHGetFileInfoW(PWideChar(CeUtf8ToUtf16(Drive^.Path)), 0, SFI, SizeOf(SFI), uFlags) = 0) then begin
        SFI.hIcon := 0;
      end;

      if (SFI.hIcon <> 0) then
      try
        Result:= BitmapCreateFromHICON(SFI.hIcon);
        if (IconSize <> iIconSmall) and (IconSize <> iIconLarge) then // non standart icon size
          Result := StretchBitmap(Result, IconSize, clBackColor, True);
      finally
        DestroyIcon(SFI.hIcon);
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

{$IF NOT (DEFINED(DARWIN) OR DEFINED(HAIKU))}

procedure TPixMapManager.LoadApplicationThemeIcon;
var
  AIcon: TIcon;
  LargeIcon: Graphics.TBitmap;
  SmallSize, LargeSize: Integer;
  SmallIcon: Graphics.TBitmap = nil;
begin
  LargeSize:= GetSystemMetrics(SM_CXICON);
  SmallSize:= GetSystemMetrics(SM_CXSMICON);
  LargeIcon:= LoadIconThemeBitmapLocked('doublecmd', LargeSize);
  if (LargeSize <> SmallSize) then begin
    SmallIcon:= LoadIconThemeBitmapLocked('doublecmd', SmallSize);
  end;
  if Assigned(LargeIcon) or Assigned(SmallIcon) then
  try
    AIcon:= TIcon.Create;
    try
      if Assigned(SmallIcon) then
      begin
        AIcon.Add(pf32bit, SmallIcon.Height, SmallIcon.Width);
        AIcon.AssignImage(SmallIcon);
        SmallIcon.Free;
      end;
      if Assigned(LargeIcon) then
      begin
        AIcon.Add(pf32bit, LargeIcon.Height, LargeIcon.Width);
        if AIcon.Count > 1 then AIcon.Current:= AIcon.Current + 1;
        AIcon.AssignImage(LargeIcon);
        LargeIcon.Free;
      end;
      Application.Icon.Assign(AIcon);
    finally
      AIcon.Free;
    end;
  except
    // Skip
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
var
  Q: QWord;
begin
  Q:= SysUtils.GetTickCount64;
  DCDebug('Creating PixmapManager');
  PixMapManager:=TPixMapManager.Create;
  PixMapManager.Load(gpCfgDir + 'pixmaps.txt');
  DCDebug('Creating PixmapManager done '+ IntToStr(SysUtils.GetTickCount64 - Q));
end;

initialization

finalization

  if Assigned(PixMapManager) then
  begin
    DCDebug('Shutting down PixmapManager');
    FreeAndNil(PixMapManager);
  end;

end.
