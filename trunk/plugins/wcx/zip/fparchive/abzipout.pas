(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower Abbrevia
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* ABBREVIA: AbZipOut.pas 3.05                           *}
{*********************************************************}
{* ABBREVIA: Visual Component with Zip and unzip support *}
{* (VCL)                                                 *}
{*   See AbQZpOut.pas for the CLX header                 *}
{*********************************************************}

unit AbZipOut;

{$I AbDefine.inc}

interface

uses
  SysUtils,
  Classes,
{$IFDEF MSWINDOWS}
  Windows,
  Messages,
{$ENDIF}
{$IFDEF LINUX}
  Types,
{$ENDIF}
{$IFDEF UsingCLX}
  QGraphics,
  QComCtrls,
  QImglist,
  QControls,
  QForms,
  AbQMeter,

{$ELSE}
  Graphics,
  Controls,
  Forms,
  StdCtrls,
  Grids,
  ComCtrls,
  {$IFDEF VERSION4}
  Imglist,
  {$ENDIF VERSION4}
  AbMeter,
{$ENDIF}
  AbArcTyp,
  AbBase,
  AbExcept,
  AbUnzPrc,
  AbUtils,
  AbZipPrc,
  AbZipTyp;


{$IFDEF NeedMouseWheel}
const
  Wm_MouseWheel = $020A;
{$ENDIF}

const
  cBitmapHeight = 16;
  cBitmapWidth  = 16;

type
  TAbZipAttribute =
    (zaCompressedSize, zaCompressionMethod, zaCompressionRatio, zaCRC,
     zaExternalFileAttributes, zaInternalFileAttributes, zaEncryption,
     zaTimeStamp, zaUncompressedSize, zaVersionMade, zaVersionNeeded,
     zaComment);

  TAbZipAttributes = set of TAbZipAttribute;

  TMethodStrings = array [ TAbZipCompressionMethod ] of string;

const
  AbDefZipAttributes =
    [zaCompressedSize, zaCompressionMethod, zaCompressionRatio, zaCRC,
     zaExternalFileAttributes, zaEncryption, zaTimeStamp, zaUncompressedSize];

  AbDefColor = clWindow;
  AbDefHierarchy = True;
  AbDefParentColor = False;

{.Z+}
type
  TTreeNodeFriend = class(TTreeNode)
  end;
{.Z-}

type
  TWindowsDropEvent =
    procedure(Sender : TObject; FileName : string) of object;
{$IFDEF NeedMouseWheel}
  TMouseWheelEvent =
    procedure(Sender : TObject; Shift : TShiftState;
              Delta, XPos, YPos : Word) of object;
{$ENDIF}
{TAbZipDisplayOutline does not support Owner-Draw}
type
  TAbZipDisplayOutline = class(TTreeView)
    private
      FDirBitMap          : TBitMap;
      FFileBitMap         : TBitMap;
      FAttrBitMap         : TBitMap;
      FDirBitMapSelected  : TBitMap;
      FFileBitMapSelected : TBitMap;
      FAttrBitMapSelected : TBitMap;
      FImageList          : TImageList;
      FFileIndex          : integer;
      FFileSelectedIndex  : integer;
      FDirectoryIndex     : integer;
      FDirSelectedIndex   : integer;
      FAttrIndex          : integer;
      FBitMapHeight       : integer;
      FBitMapWidth        : integer;
      FAttrSelectedIndex  : integer;

{$IFDEF NeedMouseWheel}
      FOnMouseWheel    : TMouseWheelEvent;
{$ENDIF}
      FOnWindowsDrop   : TWindowsDropEvent;

{$IFNDEF UsingCLX}
      procedure WMDropFiles(var Msg : TWMDropFiles);
        message WM_DROPFILES;
{$ENDIF}
{$IFDEF NeedMouseWheel}
      procedure WMMouseWheel(var Msg : TMessage);
        message WM_MOUSEWHEEL;
{$ENDIF}

      procedure IndexBitmaps;

      procedure SetDirectoryBitMap(Value : TBitmap);
      procedure SetFileBitMap(Value : TBitmap);
      procedure SetAttributeBitMap(Value : TBitmap);
      procedure SetDirectoryBitMapSelected(Value : TBitmap);
      procedure SetFileBitMapSelected(Value : TBitmap);
      procedure SetAttributeBitMapSelected(Value : TBitmap);
      procedure SetBitMapHeight(Value : Integer);
      procedure SetBitMapWidth(Value : Integer);

    protected
      procedure DoOnWindowsDrop(FileName : string); virtual;

{$IFDEF NeedMouseWheel}
      procedure DoOnMouseWheel(Shift : TShiftState; Delta, XPos, YPos : SmallInt);
        virtual;
{$ELSE}
  {$IFDEF UsingCLX}
      function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
        const MousePos: TPoint): Boolean;
        override;
  {$ELSE}
          function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
            MousePos: TPoint): Boolean;
            override;
  {$ENDIF}
{$ENDIF}
      procedure Loaded; override;
      procedure SetOnWindowsDrop(Value : TWindowsDropEvent);
    public
      constructor Create(AOwner : TComponent); override;
      destructor Destroy; override;
    public
      property zdPictureDirectory : TBitmap
        read FDirBitMap
        write SetDirectoryBitMap;
      property zdPictureFile : TBitmap
        read FFileBitMap
        write SetFileBitMap;
      property zdPictureZipAttribute : TBitmap
        read FAttrBitMap
        write SetAttributeBitMap;
      property zdPictureDirectorySelected : TBitmap
        read FDirBitMapSelected
        write SetDirectoryBitMapSelected;
      property zdPictureFileSelected : TBitmap
        read FFileBitMapSelected
        write SetFileBitMapSelected;
      property zdPictureZipAttributeSelected : TBitmap
        read FAttrBitMapSelected
        write SetAttributeBitMapSelected;
      property BitMapHeight : Integer
        read FBitMapHeight
        write SetBitMapHeight;
      property BitMapWidth : Integer
        read FBitMapWidth
        write SetBitMapWidth;
{$IFDEF NeedMouseWheel}
      property OnMouseWheel : TMouseWheelEvent
        read FOnMouseWheel
        write FOnMouseWheel;
{$ENDIF}
      property OnWindowsDrop : TWindowsDropEvent
        read FOnWindowsDrop
        write SetOnWindowsDrop;
  end;

type
{$IFDEF UsingClx}
  TAbCustomZipOutline = class(TWidgetControl)
{$ELSE}
  TAbCustomZipOutline = class(TWinControl)
{$ENDIF}
  protected {private}
    FArchive                : TAbZipArchive;                          
    FItemProgressMeter      : TAbMeter;
    FArchiveProgressMeter   : TAbMeter;
    FAttributes             : TAbZipAttributes;
    FAutoSave               : Boolean;
    FBaseDirectory          : string;
    FCompressionMethodToUse : TAbZipSupportedMethod;
    FDeflationOption        : TAbZipDeflationOption;
{$IFDEF WIN32}
    FDOSMode                : Boolean;
{$ENDIF}
    FFileName               : string;
    FExtractOptions         : TAbExtractOptions;
    FHierarchy              : Boolean;
    FLogFile                : string;
    FLogging                : Boolean;
    FSpanningThreshold      : Longint;
    FOutline                : TAbZipDisplayOutline;
    FPassword               : AnsiString;
    FPasswordRetries        : Byte;
    FStoreOptions           : TAbStoreOptions;
    FTempDirectory          : string;

    FOnProcessItemFailure   : TAbArchiveItemFailureEvent;
    FOnArchiveItemProgress  : TAbArchiveItemProgressEvent;
    FOnArchiveProgress      : TAbArchiveProgressEvent;
    FOnChange               : TNotifyEvent;
    FOnClick                : TNotifyEvent;
    FOnCollapse             : TTVExpandedEvent;
    FOnConfirmOverwrite     : TAbConfirmOverwriteEvent;
    FOnConfirmProcessItem   : TAbArchiveItemConfirmEvent;
    FOnConfirmSave          : TAbArchiveConfirmEvent;
    FOnDblClick             : TNotifyEvent;
    FOnDragDrop             : TDragDropEvent;
    FOnDragOver             : TDragOverEvent;
    FOnEndDrag              : TEndDragEvent;
    FOnEnter                : TNotifyEvent;
    FOnExit                 : TNotifyEvent;
    FOnExpand               : TTVExpandedEvent;
    FOnKeyDown              : TKeyEvent;
    FOnKeyPress             : TKeyPressEvent;
    FOnKeyUp                : TKeyEvent;
    FOnLoad                 : TAbArchiveEvent;
    FOnMouseDown            : TMouseEvent;
    FOnMouseMove            : TMouseMoveEvent;
    FOnMouseUp              : TMouseEvent;
{$IFDEF NeedMouseWheel}
    FOnMouseWheel           : TMouseWheelEvent;
{$ENDIF}
    FOnNeedPassword         : TAbNeedPasswordEvent;
    FOnRequestImage         : TAbRequestImageEvent;
    FOnRequestLastDisk      : TAbRequestDiskEvent;
    FOnRequestNthDisk       : TAbRequestNthDiskEvent;
    FOnRequestBlankDisk     : TAbRequestDiskEvent;
    FOnSave                 : TAbArchiveEvent;
{$IFDEF Win32}
    FOnStartDrag            : TStartDragEvent;
{$ENDIF Win32}
    FOnWindowsDrop          : TWindowsDropEvent;

  protected {methods}
    procedure AddAttributeNodes(Item : TAbZipItem; oNode : TTreeNode); 
    procedure DoProcessItemFailure(Sender : TObject; Item : TAbArchiveItem;
                                   ProcessType : TAbProcessType;
                                   ErrorClass : TAbErrorClass;
                                   ErrorCode : Integer); virtual;
    procedure DoArchiveItemProgress(Sender : TObject; Item : TAbArchiveItem;
                                    Progress : Byte; var Abort : Boolean); virtual;
    procedure DoArchiveProgress(Sender : TObject; Progress : Byte;
                                var Abort : Boolean); virtual;
    procedure DoChange; virtual;
    procedure DoClick(Sender : TObject); virtual;
    procedure DoCollapse(Sender : TObject; Node: TTreeNode); virtual;
    procedure DoConfirmProcessItem(Sender : TObject; Item : TAbArchiveItem;
                                   ProcessType : TAbProcessType;
                                   var Confirm : Boolean); virtual;
    procedure DoConfirmOverwrite(var Name : string; var Confirm : Boolean); virtual;
    procedure DoConfirmSave(Sender : TObject; var Confirm : Boolean); virtual;
    procedure DoDblClick(Sender : TObject); virtual;
    procedure DoDragDrop(Sender, Source: TObject; X, Y: Integer); virtual;
    procedure DoDragOver(Sender, Source: TObject; X, Y: Integer;
                         State: TDragState; var Accept: Boolean); virtual;
    procedure DoOnEndDrag(Sender, Target: TObject; X, Y: Integer); virtual;
    procedure DoOnEnter(Sender : TObject); virtual;
    procedure DoOnExit(Sender : TObject); virtual;
    procedure DoExpand(Sender: TObject; Node : TTreeNode); virtual;
    procedure DoKeyDown(Sender : TObject; var Key: Word; Shift: TShiftState);
                        virtual;
    procedure DoKeyPress(Sender : TObject; var Key: Char); virtual;
    procedure DoKeyUp(Sender : TObject; var Key: Word; Shift: TShiftState);
                      virtual;
    procedure DoLoad(Sender : TObject); virtual;
    procedure DoMouseDown(Sender : TObject; Button: TMouseButton;
                          Shift: TShiftState; X, Y : Integer); virtual;
    procedure DoMouseMove(Sender : TObject; Shift: TShiftState; X, Y: Integer);
                          virtual;
    procedure DoMouseUp(Sender : TObject; Button: TMouseButton;
                        Shift: TShiftState; X, Y: Integer); virtual;
{$IFDEF NeedMouseWheel}
    procedure DoMouseWheel(Sender : TObject; Shift : TShiftState;
                           Delta, XPos, YPos : Word); virtual;
{$ENDIF}
    procedure DoNeedPassword(Sender : TObject; var NewPassword : AnsiString);
                             virtual;
    procedure DoSave(Sender : TObject); virtual;
{$IFDEF WIN32}
    procedure DoOnStartDrag(Sender: TObject; var DragObject: TDragObject);
                            virtual;
{$ENDIF}
    procedure DoWindowsDrop(Sender : TObject; FileName : string); virtual;
    function GetBorderStyle : TBorderStyle;
    function GetCount : Integer;
    function GetCursor : TCursor;
{$IFNDEF UsingCLX}
    function GetDragCursor : TCursor;
{$ENDIF}
    function GetDragMode : TDragMode;
    function GetItem(Index : Integer) : TAbZipItem;
    function GetPictureDirectory : TBitmap;                           
    function GetPictureFile : TBitmap;
    function GetPictureZipAttribute: TBitmap;
    function GetPictureDirectorySelected : TBitmap;                           
    function GetPictureFileSelected : TBitmap;
    function GetPictureZipAttributeSelected : TBitmap;
    function GetPictureHeight : Integer;
    function GetPictureWidth : Integer;
    function GetSelectedItem : LongInt;
    function GetSelectedZipItem : TAbZipItem;
    function GetStatus : TAbArchiveStatus;
    function GetVersion : string;
    function GetZipfileComment : AnsiString;
    procedure InitArchive;
    procedure Loaded; override;
    procedure Notification(Component: TComponent; Operation: TOperation);
                           override;
    procedure PutItem(Index : Integer; Value : TAbZipItem);
    procedure SetAttributes(Value : TAbZipAttributes);
    procedure SetAutoSave(Value : Boolean);
    procedure SetBaseDirectory(Value : string);
    procedure SetBorderStyle(Value : TBorderStyle);
    procedure SetCompressionMethodToUse(Value : TAbZipSupportedMethod);
    procedure SetDeflationOption(Value : TAbZipDeflationOption);
{$IFDEF WIN32}
    procedure SetDOSMode(Value : Boolean);
{$ENDIF}
    procedure SetCursor(Value : TCursor);
{$IFNDEF UsingCLX}
    procedure SetDragCursor(Value : TCursor);
{$ENDIF}
{$IFNDEF UsingCLX}
    procedure SetDragMode(Value : TDragMode); override;
{$ENDIF}
    procedure SetExtractOptions(Value : TAbExtractOptions);
    procedure SetFileName(const aFileName : string); virtual;
    procedure SetHierarchy(Value : Boolean);
    procedure SetLogFile(Value : string);
    procedure SetLogging(Value : Boolean);
    procedure SetOnRequestImage(Value : TAbRequestImageEvent);
    procedure SetOnRequestLastDisk(Value : TAbRequestDiskEvent);
    procedure SetOnRequestNthDisk(Value : TAbRequestNthDiskEvent);
    procedure SetOnRequestBlankDisk(Value : TAbRequestDiskEvent);
    procedure SetOnWindowsDrop(Value : TWindowsDropEvent);
    procedure SetPassword(Value : AnsiString);
    procedure SetPasswordRetries(Value : Byte);
    procedure SetPictureDirectory(Value : TBitmap);                   
    procedure SetPictureFile(Value : TBitmap);
    procedure SetPictureZipAttribute(Value : TBitmap);
    procedure SetPictureDirectorySelected(Value : TBitmap);
    procedure SetPictureFileSelected(Value : TBitmap);
    procedure SetPictureZipAttributeSelected(Value : TBitmap);
    procedure SetPictureHeight(Value : Integer);
    procedure SetPictureWidth(Value : Integer);
    procedure SetSelectedItem(Value : LongInt);
    procedure SetStoreOptions(Value : TAbStoreOptions);
    procedure SetTempDirectory(Value : string);
    procedure SetSpanningThreshold(Value : Longint);
    procedure SetVersion(Value : string);
    procedure SetZipfileComment(Value : AnsiString);
    procedure TestItemProc(Sender : TObject; Item : TAbArchiveItem);
    procedure UnzipProc(Sender : TObject; Item : TAbArchiveItem;
                        const NewName : string);
    procedure UnzipToStreamProc(Sender : TObject; Item : TAbArchiveItem;
                                OutStream : TStream);
    procedure UpdateOutline;
    procedure ZipProc(Sender : TObject; Item : TAbArchiveItem;
                      OutStream : TStream);
    procedure ZipFromStreamProc(Sender : TObject; Item : TAbArchiveItem;
                                OutStream, InStream : TStream);

  protected {properties}
    property ArchiveProgressMeter : TAbMeter
             read  FArchiveProgressMeter
             write FArchiveProgressMeter;
    property Attributes : TAbZipAttributes
             read  FAttributes
             write SetAttributes
             default AbDefZipAttributes;
    property AutoSave : Boolean
             read  FAutoSave
             write SetAutoSave
             default AbDefAutoSave;
    property BaseDirectory : string
             read  FBaseDirectory
             write SetBaseDirectory;
    property BorderStyle : TBorderStyle
             read  GetBorderStyle
             write SetBorderStyle;
    property CompressionMethodToUse : TAbZipSupportedMethod
             read  FCompressionMethodToUse
             write SetCompressionMethodToUse
             default AbDefCompressionMethodToUse;
    property Cursor : TCursor
             read  GetCursor
             write SetCursor;
    property DeflationOption : TAbZipDeflationOption
             read  FDeflationOption
             write SetDeflationOption
             default AbDefDeflationOption;
{$IFDEF WIN32}
    property DOSMode : Boolean
             read FDOSMode
             write SetDOSMode;
{$ENDIF}
{$IFNDEF UsingCLX}
    property DragCursor : TCursor
             read  GetDragCursor
             write SetDragCursor;
    property DragMode : TDragMode
             read  GetDragMode
             write SetDragMode;
{$ENDIF}
    property ExtractOptions : TAbExtractOptions
             read  FExtractOptions
             write SetExtractOptions
             default AbDefExtractOptions;
    property FileName : string
             read  FFileName
             write SetFileName;
    property Hierarchy : Boolean
             read  FHierarchy
             write SetHierarchy
             default AbDefHierarchy;
    property SpanningThreshold : Longint
             read  FSpanningThreshold
             write SetSpanningThreshold
             default 0;
    property ItemProgressMeter : TAbMeter
             read  FItemProgressMeter
             write FItemProgressMeter;
    property LogFile : string
             read  FLogFile
             write SetLogFile;
    property Logging : Boolean
             read  FLogging
             write SetLogging;
    property OnWindowsDrop : TWindowsDropEvent
             read  FOnWindowsDrop
             write SetOnWindowsDrop;
    property Password : AnsiString
             read  FPassword
             write SetPassword;
    property PasswordRetries : Byte
             read  FPasswordRetries
             write SetPasswordRetries
             default AbDefPasswordRetries;
    property PictureDirectory : TBitmap
             read  GetPictureDirectory
             write SetPictureDirectory;
    property PictureFile : TBitmap
             read  GetPictureFile
             write SetPictureFile;
    property PictureZipAttribute : TBitmap
             read  GetPictureZipAttribute
             write SetPictureZipAttribute;
    property PictureDirectorySelected : TBitmap
             read  GetPictureDirectorySelected
             write SetPictureDirectorySelected;
    property PictureFileSelected : TBitmap
             read  GetPictureFileSelected
             write SetPictureFileSelected;
    property PictureZipAttributeSelected : TBitmap
             read  GetPictureZipAttributeSelected
             write SetPictureZipAttributeSelected;
    property PictureHeight : Integer
             read  GetPictureHeight
             write SetPictureHeight;
    property PictureWidth : Integer
             read  GetPictureWidth
             write SetPictureWidth;
    property StoreOptions : TAbStoreOptions
             read  FStoreOptions
             write SetStoreOptions
             default AbDefStoreOptions;
    property Version : string
             read GetVersion
             write SetVersion
             stored False;

  protected {events}
    property OnProcessItemFailure : TAbArchiveItemFailureEvent
             read  FOnProcessItemFailure
             write FOnProcessItemFailure;
    property OnArchiveItemProgress : TAbArchiveItemProgressEvent
             read  FOnArchiveItemProgress
             write FOnArchiveItemProgress;
    property OnArchiveProgress : TAbArchiveProgressEvent
             read  FOnArchiveProgress
             write FOnArchiveProgress;
    property OnChange : TNotifyEvent
             read  FOnChange
             write FOnChange;
    property OnClick : TNotifyEvent
             read  FOnClick
             write FOnClick;
    property OnConfirmProcessItem : TAbArchiveItemConfirmEvent
             read  FOnConfirmProcessItem
             write FOnConfirmProcessItem;
    property OnConfirmOverwrite : TAbConfirmOverwriteEvent
             read  FOnConfirmOverwrite
             write FOnConfirmOverwrite;
    property OnConfirmSave : TAbArchiveConfirmEvent
             read  FOnConfirmSave
             write FOnConfirmSave;
    property OnCollapse : TTVExpandedEvent
             read  FOnCollapse
             write FOnCollapse;
    property OnDblClick : TNotifyEvent
             read  FOnDblClick
             write FOnDblClick;
    property OnDragDrop : TDragDropEvent
             read FOnDragDrop
             write FOnDragDrop;
    property OnDragOver : TDragOverEvent
             read  FOnDragOver
             write FOnDragOver;
    property OnEndDrag : TEndDragEvent
             read  FOnEndDrag
             write FOnEndDrag;
    property OnEnter : TNotifyEvent
             read  FOnEnter
             write FOnEnter;
    property OnExit : TNotifyEvent
             read  FOnExit
             write FOnExit;
    property OnExpand : TTVExpandedEvent
             read  FOnExpand
             write FOnExpand;
    property OnKeyDown : TKeyEvent
             read  FOnKeyDown
             write FOnKeyDown;
    property OnKeyPress : TKeyPressEvent
             read  FOnKeyPress
             write FOnKeyPress;
    property OnKeyUp : TKeyEvent
             read  FOnKeyUp
             write FOnKeyUp;
    property OnLoad : TAbArchiveEvent
             read  FOnLoad
             write FOnLoad;
    property OnMouseDown : TMouseEvent
             read  FOnMouseDown
             write FOnMouseDown;
    property OnMouseMove : TMouseMoveEvent
             read  FOnMouseMove
             write FOnMouseMove;
    property OnMouseUp : TMouseEvent
             read  FOnMouseUp
             write FOnMouseUp;
{$IFDEF NeedMouseWheel}
    property OnMouseWheel : TMouseWheelEvent
             read  FOnMouseWheel
             write FOnMouseWheel;
{$ENDIF}
    property OnNeedPassword : TAbNeedPasswordEvent
             read  FOnNeedPassword
             write FOnNeedPassword;
    property OnRequestImage : TAbRequestImageEvent
             read  FOnRequestImage
             write SetOnRequestImage;
    property OnRequestLastDisk : TAbRequestDiskEvent
             read  FOnRequestLastDisk
             write SetOnRequestLastDisk;
    property OnRequestNthDisk : TAbRequestNthDiskEvent
             read  FOnRequestNthDisk
             write SetOnRequestNthDisk;
    property OnRequestBlankDisk : TAbRequestDiskEvent
             read  FOnRequestBlankDisk
             write SetOnRequestBlankDisk;
    property OnSave : TAbArchiveEvent
             read  FOnSave
             write FOnSave;
{$IFDEF Win32}
    property OnStartDrag : TStartDragEvent
             read  FOnStartDrag
             write FOnStartDrag;
{$ENDIF Win32}

  public {methods}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure AddFiles(const FileMask : string; SearchAttr : Integer);
      {Add files to the archive where the disk filespec matches}
    procedure AddFilesEx(const FileMask, ExclusionMask : string;
                         SearchAttr : Integer);
      {Add files that match Filemask except those matching ExclusionMask}
    procedure AddFromStream(const NewName : string; FromStream : TStream);
      {Create and add a zip item directly from a stream}
    procedure ClearTags;
      {Clear all tags from the archive}
    procedure CloseArchive;
      {closes the archive by setting FileName to ''}
    procedure DeleteAt(Index : Integer);
      {delete item specified by index}
    procedure DeleteFiles(const FileMask : string);
      {Delete all files from the archive that match the file mask}
    procedure DeleteFilesEx(const FileMask, ExclusionMask : string);
      {Delete files that match Filemask except those matching ExclusionMask}
    procedure DeleteTaggedItems;
      {delete all tagged items from the archive}
    procedure ExtractAt(Index : Integer; const NewName : string);
      {extract item specified by index}
    procedure ExtractFiles(const FileMask : string);
      {extract all files from the archive that match the mask}
    procedure ExtractFilesEx(const FileMask, ExclusionMask : string);
      {Extract files that match Filemask except those matching ExclusionMask}
    procedure ExtractTaggedItems;
      {extract all tagged items from the archive}
    procedure ExtractToStream(const aFileName : string; ToStream : TStream);
      {extract an item directly to a stream}
    function FindItem(aItem : TAbArchiveItem) : Integer;
      {extract specified item}
    function FindFile(const aFileName : string) : Integer;
      {find the item with the given file name}
    procedure FreshenFiles(const FileMask : string);
      {freshen all items that match the file mask}
    procedure FreshenFilesEx(const FileMask, ExclusionMask : string);
      {freshen items matching FileMask but not ExclusionMask}
    procedure FreshenTaggedItems;
      {freshen all tagged items}
    procedure FullCollapse;
    procedure FullExpand;
    function GetTextItem(const Value: string): LongInt;
    function GetOutLineItem(X, Y : Integer): LongInt;
    procedure Move(aItem : TAbArchiveItem; NewStoredPath : string);
    procedure OpenArchive(const aFileName : String);
      {opens the archive}
    procedure Replace(aItem : TAbArchiveItem);
    procedure Save;
      {saves the archive}
    procedure TagItems(const FileMask : string);
    procedure TestTaggedItems;
    procedure UnTagItems(const FileMask : string);                   

  public {properties}
    property Count : Integer
             read GetCount;
    property Items[Index : Integer] : TAbZipItem
             read GetItem
             write PutItem; default;
    property SelectedItem: LongInt
             read GetSelectedItem
             write SetSelectedItem;
    property SelectedZipItem : TAbZipItem
             read GetSelectedZipItem;
    property Status : TAbArchiveStatus
             read GetStatus;
    property TempDirectory : string
             read FTempDirectory
             write SetTempDirectory;
    property ZipfileComment : AnsiString
             read GetZipfileComment
             write SetZipfileComment;
  end;


type
  TAbZipOutline = class(TAbCustomZipOutline)
  published
    property Align;
    property ArchiveProgressMeter;
    property ItemProgressMeter;
    property Attributes;
    property AutoSave;
    property BaseDirectory;
    property BorderStyle;
    property Color
             default AbDefColor;
    property CompressionMethodToUse;
    property Count;
{$IFNDEF UsingCLX}
    property Ctl3D;
{$ENDIF}
    property Cursor;
    property DeflationOption;
{$IFDEF WIN32}
    property DOSMode;
{$ENDIF}
{$IFNDEF UsingCLX}
    property DragCursor;
{$ENDIF}
    property DragMode;
    property Enabled;
    property ExtractOptions;
    property Font;
    property Hierarchy;
    property LogFile;
    property Logging;
    property OnProcessItemFailure;
    property OnArchiveItemProgress;
    property OnArchiveProgress;
    property OnChange;
    property OnClick;
    property OnConfirmProcessItem;
    property OnConfirmOverwrite;
    property OnConfirmSave;
    property OnCollapse;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnExpand;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnLoad;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
{$IFNDEF NeedMouseWheel}
  {$IFNDEF UsingCLX}
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
  {$ENDIF}
{$ENDIF}
    property OnNeedPassword;
    property OnRequestImage;
    property OnRequestLastDisk;
    property OnRequestNthDisk;
    property OnRequestBlankDisk;
    property OnSave;
{$IFDEF Win32}
    property OnStartDrag;
{$ENDIF Win32}
    property OnWindowsDrop;
    property ParentColor
             default AbDefParentColor;
{$IFNDEF UsingCLX}
    property ParentCtl3D;
{$ENDIF}
    property ParentFont;
    property ParentShowHint;
    property Password;
    property PasswordRetries;
    property PictureDirectory;
    property PictureDirectorySelected;
    property PictureFile;
    property PictureFileSelected;
    property PictureZipAttribute;
    property PictureZipAttributeSelected;
    property PopupMenu;
    property ShowHint;
    property StoreOptions;
    property TabOrder;
    property TabStop;
    property SpanningThreshold;
    property Version;
    property TempDirectory;
    property Visible;
    property FileName; {must be after OnLoad}                          
  end;

implementation

uses
{$IFDEF WIN32}
  ShellApi,
{$ENDIF}
  AbConst;

{$R AbZipOut.res}

var
  MethodStrings : TMethodStrings;

type
  TAbZipArchiveFriend = class(TAbZipArchive)
  end;
  
{ -------------------------------------------------------------------------- }
{ ========================================================================== }
{ -------------------------------------------------------------------------- }
procedure TAbZipDisplayOutline.IndexBitmaps;
begin
  FImageList.Clear;

  FImageList.Height := FBitMapHeight;
  FImageList.Width  := FBitMapWidth;

  if not FAttrBitMap.Empty then
   FAttrIndex := FImageList.Add( FAttrBitMap, nil );
  if not FAttrBitMap.Empty then
    FAttrSelectedIndex := FImageList.Add( FAttrBitMapSelected, nil );
  if not FAttrBitMap.Empty then
    FDirectoryIndex := FImageList.Add( FDirBitMap, nil );
  if not FAttrBitMap.Empty then
    FDirSelectedIndex := FImageList.Add( FDirBitMapSelected , nil );
  if not FAttrBitMap.Empty then
    FFileIndex := FImageList.Add( FFileBitMap, nil );
  if not FAttrBitMap.Empty then
    FFileSelectedIndex := FImageList.Add( FFileBitMapSelected, nil );
end;
{ -------------------------------------------------------------------------- }
constructor TAbZipDisplayOutline.Create(AOwner : TComponent);
begin
  FBitMapHeight := cBitmapHeight;
  FBitMapWidth  := cBitmapWidth;

  FDirBitMap := TBitMap.Create;
  FFileBitMap := TBitMap.Create;
  FAttrBitMap := TBitMap.Create;
  FDirBitMapSelected  := TBitMap.Create;
  FFileBitMapSelected := TBitMap.Create;
  FAttrBitMapSelected := TBitMap.Create;

  FDirBitMap.LoadFromResourceName( HInstance, 'DIR' );
  FFileBitMap.LoadFromResourceName( HInstance, 'FILEFIX' );
  FAttrBitMap.LoadFromResourceName( HInstance, 'ATTR' );
  FDirBitMapSelected.LoadFromResourceName ( HInstance, 'DIRS'  );
  FFileBitMapSelected.LoadFromResourceName( HInstance, 'FILES' );
  FAttrBitMapSelected.LoadFromResourceName( HInstance, 'ATTRS' );

  inherited Create(AOwner);

  FImageList := TImageList.Create(Self);
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipDisplayOutline.Loaded;
begin
  inherited Loaded;
{$IFNDEF UsingCLX}
  if Assigned(FOnWindowsDrop) then
    DragAcceptFiles(Handle, True);
{$ENDIF}
end;
{ -------------------------------------------------------------------------- }
destructor TAbZipDisplayOutline.Destroy;
begin
  FImageList.Free;
  FDirBitMap.Free;
  FFileBitMap.Free;
  FAttrBitMap.Free;
  FDirBitMapSelected.Free;
  FFileBitMapSelected.Free;
  FAttrBitMapSelected.Free;
  inherited Destroy;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipDisplayOutline.SetAttributeBitMap(Value : TBitmap);
begin
  if Value <> nil then begin
    FAttrBitMap.assign( Value )
  end else begin
    FAttrBitMap.LoadFromResourceName( HInstance, 'ATTR' );
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipDisplayOutline.SetDirectoryBitMap(Value : TBitmap);
begin
  if Value <> nil then begin
    FDirBitMap.assign( Value )
  end else begin
    FDirBitMap.LoadFromResourceName( HInstance, 'DIR' );
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipDisplayOutline.SetFileBitMap(Value : TBitmap);
begin
  if Value <> nil then begin
    FFileBitMap.assign( Value )
  end else begin
    FFileBitMap.LoadFromResourceName( HInstance, 'FILEFIX' );
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipDisplayOutline.SetAttributeBitMapSelected(Value : TBitmap);
begin
  if Value <> nil then
    FAttrBitMapSelected.assign( Value )
  else begin
    FAttrBitMapSelected.LoadFromResourceName( HInstance, 'ATTRS' );
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipDisplayOutline.SetDirectoryBitMapSelected(Value : TBitmap);
begin
  if Value <> nil then
    FDirBitMapSelected.assign( Value )
  else begin
    FDirBitMapSelected.LoadFromResourceName ( HInstance, 'DIRS'  );
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipDisplayOutline.SetFileBitMapSelected(Value : TBitmap);
begin
  if Value <> nil then
    FFileBitMapSelected.assign( Value )
  else begin
    FFileBitMapSelected.LoadFromResourceName( HInstance, 'FILES' );
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipDisplayOutline.SetBitMapHeight(Value : Integer);
begin
  if FBitMapHeight <> Value then
    FBitMapHeight := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipDisplayOutline.SetBitMapWidth(Value : Integer);
begin
  if FBitMapWidth <> Value then
    FBitMapWidth := Value;
end;
{ -------------------------------------------------------------------------- }
{$IFDEF NeedMouseWheel}
procedure TAbZipDisplayOutline.WMMouseWheel(var Msg : TMessage);
begin
  inherited;

  with Msg do
    DoOnMouseWheel(KeysToShiftState(LOWORD(wParam)) {fwKeys},
                   HIWORD(wParam) {zDelta},
                   LOWORD(lParam) {xPos},   HIWORD(lParam) {yPos});
end;
{$ENDIF}
{$IFNDEF UsingCLX}
{ -------------------------------------------------------------------------- }
procedure TAbZipDisplayOutline.WMDropFiles(var Msg : TWMDropFiles);
var
  FileName : string;
  I : Integer;
  NumFiles : Integer;
begin
  Msg.Result := 1;
  NumFiles := DragQueryFile(Msg.Drop, Cardinal(-1), nil, 0);
  try
    for I := 0 to pred(NumFiles) do begin
      SetLength(FileName, DragQueryFile(Msg.Drop, I, nil, 0));
      DragQueryFile(Msg.Drop, I, PChar(FileName), Length(FileName) + 1);
      DoOnWindowsDrop(FileName);
    end;
  finally
    DragFinish(Msg.Drop);
  end;
  if IsIconic(Application.Handle) then
    ShowWindow(Application.Handle, SW_SHOWNORMAL)
  else
    BringWindowToTop(Handle);
end;
{$ENDIF}
{ -------------------------------------------------------------------------- }
procedure TAbZipDisplayOutline.DoOnWindowsDrop(FileName : string);
begin
  if csDesigning in ComponentState then
    Exit;
  if csLoading in ComponentState then
    Exit;
  if Assigned(FOnWindowsDrop) then
    FOnWindowsDrop(Self, FileName);
end;

{$IFDEF NeedMouseWheel}
procedure TAbZipDisplayOutline.DoOnMouseWheel(Shift : TShiftState;
                                               Delta, XPos, YPos : SmallInt);
const
  WHEEL_DELTA = 120;
var
//  Next : LongInt;
  oHold : TTreeNode;
  oNode : TTreeNode;
begin
  if Assigned(FOnMouseWheel) then
    FOnMouseWheel(Self, Shift, Delta, XPos, YPos);

  if Items.Count = 0 then
    Exit;

  if Selected = nil then
    exit;

  if Selected.HasChildren then
    Selected.Expand( false );

  oNode := nil;
  oHold := Selected;  
  if Delta < 0 then begin
    if oHold.HasChildren then
      oNode := oHold.getFirstChild;
    if oNode = nil then
      oNode := oHold.GetNextChild( oHold );
    if oNode = nil then
      oNode := oHold.GetNext;
  end else begin
    oNode := oHold.GetPrevChild( oHold );
    if oNode <> nil then begin
      if oNode.HasChildren then
        oNode := oNode.GetLastChild;
    end else
      oNode := oHold.GetPrev;
  end;
  if oNode <> nil then
    Selected := oNode;
end;
{$ELSE}
  {$IFDEF LINUX}
    function TAbZipDisplayOutline.DoMouseWheel(Shift: TShiftState; 
       WheelDelta: Integer; const MousePos: TPoint): Boolean;
     const
       WHEEL_DELTA = 120;
     var
      oHold : TTreeNode;
      oNode : TTreeNode;
    begin
      { We always return true - if there's an event handler that returns }
      { false, we'll do the work; if it returns true, the work has been  }
      { done, ergo this routine should return true.                      }
      Result := True;
      if not inherited DoMouseWheel(Shift, WheelDelta, MousePos) then begin
        if Items.Count = 0 then
          Exit;
    
          if Selected = nil then
            exit;
          if Selected.HasChildren then
            Selected.Expand( false );
            
          oNode := nil;
          oHold := Selected;  
          if WheelDelta < 0 then begin
            if oHold.HasChildren then
              oNode := oHold.getFirstChild;
            if oNode = nil then
              oNode := oHold.GetNextChild( oHold );
            if oNode = nil then
              oNode := oHold.GetNext;
          end else begin
            oNode := oHold.GetPrevChild( oHold );
            if oNode <> nil then begin
              if oNode.HasChildren then
                oNode := oNode.GetLastChild;
            end else
              oNode := oHold.GetPrev;
          end;
          if oNode <> nil then
            Selected := oNode;
      end;
    end; 
  {$ELSE}
    {$IFDEF UsingCLX}
    function TAbZipDisplayOutline.DoMouseWheel(Shift: TShiftState;
      WheelDelta: Integer; const MousePos: TPoint): Boolean;
    {$ELSE}
    function TAbZipDisplayOutline.DoMouseWheel(Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint): Boolean;
    {$ENDIF}
     const
       WHEEL_DELTA = 120;
     var
      oHold : TTreeNode;
      oNode : TTreeNode;
    begin
      { We always return true - if there's an event handler that returns }
      { false, we'll do the work; if it returns true, the work has been  }
      { done, ergo this routine should return true.                      }
      Result := True;
      if not inherited DoMouseWheel(Shift, WheelDelta, MousePos) then begin
        if Items.Count = 0 then
          Exit;
    
          if Selected = nil then
            exit;
          if Selected.HasChildren then
            Selected.Expand( false );
            
          oNode := nil;
          oHold := Selected;  
          if WheelDelta < 0 then begin
            if oHold.HasChildren then
              oNode := oHold.getFirstChild;
            if oNode = nil then
              oNode := oHold.GetNextChild( oHold );
            if oNode = nil then
              oNode := oHold.GetNext;
          end else begin
            oNode := oHold.GetPrevChild( oHold );
            if oNode <> nil then begin
              if oNode.HasChildren then
                oNode := oNode.GetLastChild;
            end else
              oNode := oHold.GetPrev;
          end;
          if oNode <> nil then
            Selected := oNode;
      end;
    end; 
  {$ENDIF}
{$ENDIF}
{ -------------------------------------------------------------------------- }
procedure TAbZipDisplayOutline.SetOnWindowsDrop(Value : TWindowsDropEvent);
{$IFNDEF UsingCLX}
var
  WasAccepting : Boolean;
{$ENDIF}
begin
{$IFNDEF UsingCLX}
  WasAccepting := Assigned(FOnWindowsDrop);
  FOnWindowsDrop := Value;
  if csLoading in ComponentState then
    Exit;
  if csDestroying in ComponentState then
    Exit;
  if Assigned(Value) then
    DragAcceptFiles(Handle, True)
  else if WasAccepting then
    DragAcceptFiles(Handle, False);
{$ENDIF}
end;
{ -------------------------------------------------------------------------- }
{ ========================================================================== }
{ -------------------------------------------------------------------------- }
constructor TAbCustomZipOutline.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  Width := 300;
  Height := 143;
  Color := AbDefColor;
  ParentColor := AbDefParentColor;

  FOutline := TAbZipDisplayOutline.Create(nil); {!!.05 Self -> nil to avoid streaming error, in D4}
  FOutline.Parent := Self;
  FOutline.Visible := True;
  FOutline.Align := alClient;
  FOutline.ParentColor := True;
{$IFNDEF UsingCLX}
  FOutline.ParentCtl3D := True;
{$ENDIF}
  FOutline.ParentFont := True;
  FOutline.ParentShowHint := True;

  FOutline.Images := FOutline.FImageList;

  AutoSave := AbDefAutoSave;
  Attributes := AbDefZipAttributes;
  CompressionMethodToUse := AbDefCompressionMethodToUse;
  DeflationOption := AbDefDeflationOption;
  ExtractOptions := AbDefExtractOptions;
  Hierarchy := AbDefHierarchy;
  PasswordRetries := AbDefPasswordRetries;
  StoreOptions := AbDefStoreOptions;
end;
{ -------------------------------------------------------------------------- }
destructor TAbCustomZipOutline.Destroy;
begin
  FArchive.Free;
  FOutLine.Free; {!!.05 Remove Self}
  inherited Destroy;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.AddAttributeNodes( Item : TAbZipItem;
                                                 oNode : TTreeNode );
var
  ExtAttrString : string;
  dt : TDateTime;
  li : LongInt;
  s : string;
  tmpNode : TTreeNode;
begin
  with Item do begin
    if zaCompressedSize in Attributes then begin
      tmpNode := FOutline.Items.AddChild(oNode,
                         Format(AbStrRes(AbCompressedSizeFormat),
                                 [CompressedSize]));
      tmpNode.ImageIndex    := FOutline.FAttrIndex;
      tmpNode.SelectedIndex := FOutline.FAttrSelectedIndex;
    end;
    if zaUnCompressedSize in Attributes then begin
      tmpNode := FOutline.Items.AddChild(oNode,                       
                         Format(AbStrRes(AbUncompressedSizeFormat),
                                 [UncompressedSize]));
      tmpNode.ImageIndex    := FOutline.FAttrIndex;
      tmpNode.SelectedIndex := FOutline.FAttrSelectedIndex;
    end;
    if zaCompressionMethod in Attributes then begin
      tmpNode := FOutline.Items.AddChild(oNode,                       
                         Format(AbStrRes(AbCompressionMethodFormat),
                                 [MethodStrings[CompressionMethod]]));
      tmpNode.ImageIndex    := FOutline.FAttrIndex;
      tmpNode.SelectedIndex := FOutline.FAttrSelectedIndex;
    end;
    if zaCompressionRatio in Attributes then begin
      tmpNode := FOutline.Items.AddChild(oNode,                       
                         Format(AbStrRes(AbCompressionRatioFormat),
                                 [CompressionRatio]));
      tmpNode.ImageIndex    := FOutline.FAttrIndex;
      tmpNode.SelectedIndex := FOutline.FAttrSelectedIndex;
    end;
    if zaCRC in Attributes then begin
      tmpNode := FOutline.Items.AddChild(oNode,                       
                         Format(AbStrRes(AbCRCFormat),
                                 [CRC32]));
      tmpNode.ImageIndex    := FOutline.FAttrIndex;
      tmpNode.SelectedIndex := FOutline.FAttrSelectedIndex;
    end;
    if zaExternalFileAttributes in Attributes then begin
      ExtAttrString := '';
{$IFDEF MSWINDOWS}
{$IFDEF Version6} {$WARN SYMBOL_PLATFORM OFF} {$ENDIF}
      if (faReadOnly and ExternalFileAttributes) = faReadOnly then
        ExtAttrString := ExtAttrString + AbStrRes(AbReadOnly);
      if (faHidden and ExternalFileAttributes) = faHidden then
        ExtAttrString := ExtAttrString + AbStrRes(AbHidden);
      if (faSysFile and ExternalFileAttributes) = faSysFile then
        ExtAttrString := ExtAttrString + AbStrRes(AbSystem);
      if (faArchive and ExternalFileAttributes) = faArchive then
        ExtAttrString := ExtAttrString + AbStrRes(AbArchived);
{$IFDEF Version6} {$WARN SYMBOL_PLATFORM ON} {$ENDIF}
{$ENDIF}
      tmpNode := FOutline.Items.AddChild(oNode,
                         Format(AbStrRes(AbEFAFormat),
                                 [ExtAttrString]));
      tmpNode.ImageIndex    := FOutline.FAttrIndex;
      tmpNode.SelectedIndex := FOutline.FAttrSelectedIndex;
    end;
    if zaInternalFileAttributes in Attributes then
      if InternalFileAttributes = 1 then begin
        tmpNode := FOutline.Items.AddChild(oNode,                     
                           Format(AbStrRes(AbIFAFormat),
                                   [AbStrRes(AbText)]));
        tmpNode.ImageIndex    := FOutline.FAttrIndex;
        tmpNode.SelectedIndex := FOutline.FAttrSelectedIndex;
      end else begin
        tmpNode := FOutline.Items.AddChild(oNode,
                           Format(AbStrRes(AbIFAFormat),
                                   [AbStrRes(AbBinary)]));
        tmpNode.ImageIndex    := FOutline.FAttrIndex;
        tmpNode.SelectedIndex := FOutline.FAttrSelectedIndex;
      end;
    if zaEncryption in Attributes then
      if IsEncrypted then begin
        tmpNode := FOutline.Items.AddChild(oNode,                     
                           Format(AbStrRes(AbEncryptionFormat),
                                   [AbStrRes(AbEncrypted)]));
        tmpNode.ImageIndex    := FOutline.FAttrIndex;
        tmpNode.SelectedIndex := FOutline.FAttrSelectedIndex;
      end else begin
        tmpNode := FOutline.Items.AddChild(oNode,                     
                           Format(AbStrRes(AbEncryptionFormat),
                                   [AbStrRes(AbNotEncrypted)]));
        tmpNode.ImageIndex    := FOutline.FAttrIndex;
        tmpNode.SelectedIndex := FOutline.FAttrSelectedIndex;
      end;
    if zaTimeStamp in Attributes then begin
      if (LastModFileDate + LastModFileTime = 0) then
        s := AbStrRes(AbUnknown)
      else begin
        li := LongInt(LastModFileDate) shl 16 + LastModFileTime;
        dt := FileDateToDateTime(li);
        s := DateTimeToStr(dt);
      end;
      tmpNode := FOutline.Items.AddChild(oNode,                     
                         Format(AbStrRes(AbTimeStampFormat), [s]));
      tmpNode.ImageIndex    := FOutline.FAttrIndex;
      tmpNode.SelectedIndex := FOutline.FAttrSelectedIndex;
    end;
    if zaVersionMade in Attributes then begin
      tmpNode := FOutline.Items.AddChild(oNode,                     
                         Format(AbStrRes(AbMadeByFormat),
                                 [Lo(VersionMadeBy)/ 10.0]));
      tmpNode.ImageIndex    := FOutline.FAttrIndex;
      tmpNode.SelectedIndex := FOutline.FAttrSelectedIndex;
    end;
    if zaVersionNeeded in Attributes then begin
      tmpNode := FOutline.Items.AddChild(oNode,                     
                         Format(AbStrRes(AbNeededFormat),
                                 [Lo(VersionNeededToExtract)/ 10.0]));
      tmpNode.ImageIndex    := FOutline.FAttrIndex;
      tmpNode.SelectedIndex := FOutline.FAttrSelectedIndex;
    end;
    if zaComment in Attributes then begin
      tmpNode := FOutline.Items.AddChild(oNode,
                         Format(AbStrRes(AbCommentFormat),
                                 [FileComment]));
      tmpNode.ImageIndex    := FOutline.FAttrIndex;
      tmpNode.SelectedIndex := FOutline.FAttrSelectedIndex;
    end;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.AddFiles(const FileMask : string;
                                       SearchAttr : Integer);
  {Add files to the archive where the disk filespec matches}
begin
  if Assigned(FArchive) then
    FArchive.AddFiles(FileMask, SearchAttr)
  else
    raise EAbNoArchive.Create;
  DoChange;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.AddFilesEx(const FileMask, ExclusionMask : string;
                                         SearchAttr : Integer);
  {Add files that match Filemask except those matching ExclusionMask}
begin
  if Assigned(FArchive) then
    FArchive.AddFilesEx(FileMask, ExclusionMask, SearchAttr)
  else
    raise EAbNoArchive.Create;
  DoChange;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.AddFromStream(const NewName : string;
                                            FromStream : TStream);
  {Add zip item directly from TStream descendant}
begin
  if Assigned(FArchive) then begin
    FromStream.Position := 0;
    FArchive.AddFromStream(NewName, FromStream);
  end else
    raise EAbNoArchive.Create;
  DoChange;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.ClearTags;
  {Clear all tags from the archive}
begin
  if Assigned(FArchive) then
    FArchive.ClearTags
  else
    raise EAbNoArchive.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.DeleteAt(Index : Integer);
  {delete item at Index}
begin
  if Assigned( FArchive ) then
    FArchive.DeleteAt( Index )
  else
    raise EAbNoArchive.Create;
  DoChange;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.DeleteFiles(const FileMask : string);
  {delete all files from the archive that match the file mask}
begin
  if Assigned(FArchive) then
    FArchive.DeleteFiles(FileMask)
  else
    raise EAbNoArchive.Create;
  DoChange;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.DeleteFilesEx(const FileMask, ExclusionMask : string);
  {Delete files that match Filemask except those matching ExclusionMask}
begin
  if Assigned(FArchive) then
    FArchive.DeleteFilesEx(FileMask, ExclusionMask)
  else
    raise EAbNoArchive.Create;
  DoChange;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.DeleteTaggedItems;
  {delete all tagged items from the archive}
begin
  if Assigned(FArchive) then
    FArchive.DeleteTaggedItems
  else
    raise EAbNoArchive.Create;
  DoChange;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.DoProcessItemFailure(Sender : TObject;
                                              Item : TAbArchiveItem;
                                              ProcessType : TAbProcessType;
                                              ErrorClass : TAbErrorClass;
                                              ErrorCode : Integer);
begin
  if Assigned(FOnProcessItemFailure) then
    FOnProcessItemFailure(Self, Item, ProcessType, ErrorClass, ErrorCode);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.DoArchiveItemProgress(Sender : TObject;
                                               Item : TAbArchiveItem;
                                               Progress : Byte;
                                               var Abort : Boolean);
begin
  Abort := False;
  if Assigned(FItemProgressMeter) then
    FItemProgressMeter.DoProgress(Progress);
  if Assigned(FOnArchiveItemProgress) then
    FOnArchiveItemProgress(Self, Item, Progress, Abort);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.DoArchiveProgress(Sender : TObject;
                                           Progress : Byte;
                                           var Abort : Boolean);
begin
  Abort := False;
  if Assigned(FArchiveProgressMeter) then
    FArchiveProgressMeter.DoProgress(Progress);
  if Assigned(FOnArchiveProgress) then
    FOnArchiveProgress(Self, Progress, Abort);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.DoChange;
begin
  {Archive now points to the new zip file}
  UpdateOutline;
  {then, call the FOnChange event...}
  if Assigned(FOnChange) then
    FOnChange(Self);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.DoClick(Sender : TObject);
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.DoCollapse(Sender: TObject; Node: TTreeNode);
begin
  if Assigned(FOnCollapse) then
    FOnCollapse(Self, Node);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.DoConfirmProcessItem(Sender : TObject;
                                              Item : TAbArchiveItem;
                                              ProcessType : TAbProcessType;
                                              var Confirm : Boolean);
begin
  Confirm := True;
  if Assigned(FItemProgressMeter) then
    FItemProgressMeter.Reset;
  if Assigned(FOnConfirmProcessItem) then
    FOnConfirmProcessItem(Self, Item, ProcessType, Confirm);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.DoConfirmOverwrite(var Name : string;
                                            var Confirm : Boolean);
begin
  Confirm := True;
  if Assigned(FOnConfirmOverwrite) then
    FOnConfirmOverwrite(Name, Confirm);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.DoConfirmSave(Sender : TObject;
                                       var Confirm : Boolean);
begin
  Confirm := True;
  if Assigned(FOnConfirmSave) then
    FOnConfirmSave(Self, Confirm);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.DoDblClick(Sender : TObject);
begin
  if Assigned(FOnDblClick) then
    FOnDblClick(Self);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.DoDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  if Assigned(FOnDragDrop) then
    FOnDragDrop(Self, Source, X, Y);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.DoDragOver(Sender, Source: TObject; X, Y: Integer;
                                    State: TDragState; var Accept: Boolean);
begin
  Accept := False;
  if Assigned(FOnDragOver) then
    FOnDragOver(Self, Source, X, Y, State, Accept);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.DoOnEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  if Assigned(FOnEndDrag) then
    FOnEndDrag(Self, Target, X, Y);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.DoOnEnter(Sender : TObject);
begin
  if Assigned(FOnEnter) then
    FOnEnter(Self);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.DoOnExit(Sender : TObject);
begin
  if Assigned(FOnExit) then
    FOnExit(Self);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.DoExpand(Sender: TObject; Node : TTreeNode);
begin
  if Assigned(FOnExpand) then
    FOnExpand(Self, Node);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.DoKeyDown(Sender : TObject; var Key: Word;
                                   Shift: TShiftState);
begin
  if Assigned(FOnKeyDown) then
    FOnKeyDown(Self, Key, Shift);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.DoKeyPress(Sender : TObject; var Key: Char);
begin
  if Assigned(FOnKeyPress) then
    FOnKeyPress(Self, Key);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.DoKeyUp(Sender : TObject; var Key: Word;
                                 Shift: TShiftState);
begin
  if Assigned(FOnKeyUp) then
    FOnKeyUp(Self, Key, Shift);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.DoLoad(Sender : TObject);
begin
  if Assigned(FOnLoad) then
    FOnLoad(Self);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.DoMouseDown(Sender : TObject; Button: TMouseButton;
                                     Shift: TShiftState;
                                     X, Y : Integer);
begin
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self, Button, Shift, X, Y);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.DoMouseMove(Sender : TObject;
                                     Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self, Shift, X, Y);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.DoMouseUp(Sender : TObject;
                                   Button: TMouseButton; Shift: TShiftState;
                                   X, Y: Integer);
begin
  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self, Button, Shift, X, Y);
end;
{ -------------------------------------------------------------------------- }
{$IFDEF NeedMouseWheel}                                                
procedure TAbCustomZipOutline.DoMouseWheel(Sender : TObject; Shift : TShiftState;
                        Delta, XPos, YPos : Word);
begin
  if Assigned(FOnMouseWheel) then
    FOnMouseWheel(Self, Shift, Delta, XPos, YPos);
end;
{$ENDIF}
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.DoNeedPassword(Sender : TObject;
                                        var NewPassword : AnsiString);
begin
  if Assigned(FOnNeedPassword) then begin
    FOnNeedPassword(Sender, NewPassword);
    Password := NewPassword;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.DoSave(Sender : TObject);
begin
  if Assigned(FOnSave) then
    FOnSave(Self);
end;
{ -------------------------------------------------------------------------- }
{$IFDEF WIN32}
procedure TAbCustomZipOutline.DoOnStartDrag(Sender: TObject;
                                       var DragObject: TDragObject);
begin
  if Assigned(FOnStartDrag) then
    FOnStartDrag(Self, DragObject);
end;
{$ENDIF}
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.DoWindowsDrop(Sender : TObject;
                                       FileName : string);
begin
  if csDesigning in ComponentState then
    Exit;
  if csLoading in ComponentState then
    Exit;
  if Assigned(FOnWindowsDrop) then
    FOnWindowsDrop(Self, FileName);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.ExtractAt(Index : Integer; const NewName : string);
  {extract a file from the archive that match the index}
begin
  if Assigned(FArchive) then
    FArchive.ExtractAt(Index, NewName)
  else
    raise EAbNoArchive.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.ExtractFiles(const FileMask : string);
  {extract all files from the archive that match the mask}
begin
  if Assigned(FArchive) then
    FArchive.ExtractFiles(FileMask)
  else
    raise EAbNoArchive.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.ExtractFilesEx(const FileMask, ExclusionMask : string);
  {extract files that match FileMask except those matching ExclusionMask}
begin
  if Assigned(FArchive) then
    FArchive.ExtractFilesEx(FileMask, ExclusionMask)
  else
    raise EAbNoArchive.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.ExtractTaggedItems;
  {extract all tagged items from the archive}
begin
  if Assigned(FArchive) then
    FArchive.ExtractTaggedItems
  else
    raise EAbNoArchive.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.ExtractToStream(const aFileName : string;
                                              ToStream : TStream);
begin
  if Assigned(FArchive) then
    FArchive.ExtractToStream(aFileName, ToStream)
  else
    raise EAbNoArchive.Create;
end;
{ -------------------------------------------------------------------------- }
function TAbCustomZipOutline.FindFile(const aFileName : string) : Integer;
begin
  if Assigned(FArchive) then
    Result := FArchive.FindFile(aFileName)
  else
    raise EAbNoArchive.Create;
end;
{ -------------------------------------------------------------------------- }
function TAbCustomZipOutline.FindItem(aItem : TAbArchiveItem) : Integer;
begin
  if Assigned(FArchive) then
    Result := FArchive.FindItem(aItem)
  else
    Result := -1;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.FreshenFiles(const FileMask : string);
  {freshen all items that match the file mask}
begin
  if Assigned(FArchive) then
    FArchive.FreshenFiles(FileMask)
  else
    raise EAbNoArchive.Create;
  DoChange;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.FreshenFilesEx(const FileMask, ExclusionMask : string);
  {freshen all items matching FileMask except those matching ExclusionMask}
begin
  if Assigned(FArchive) then
    FArchive.FreshenFilesEx(FileMask, ExclusionMask)
  else
    raise EAbNoArchive.Create;
  DoChange;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.FreshenTaggedItems;
  {freshen all tagged items}
begin
  if Assigned(FArchive) then
    FArchive.FreshenTaggedItems
  else
    raise EAbNoArchive.Create;
  DoChange;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.FullCollapse;
begin
  FOutline.FullCollapse;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.FullExpand;
begin
  FOutline.FullExpand;
end;
{ -------------------------------------------------------------------------- }
function TAbCustomZipOutline.GetBorderStyle : TBorderStyle;
begin
  Result := FOutline.BorderStyle;
end;
{ -------------------------------------------------------------------------- }
function TAbCustomZipOutline.GetCount : Integer;
begin
  if Assigned(FArchive) then
    Result := FArchive.Count
  else
    Result := 0;
end;
{ -------------------------------------------------------------------------- }
function TAbCustomZipOutline.GetCursor : TCursor;
begin
  Result := FOutline.Cursor;
end;
{ -------------------------------------------------------------------------- }
{$IFNDEF UsingCLX}
function TAbCustomZipOutline.GetDragCursor : TCursor;
begin
  Result := FOutline.DragCursor;
end;
{$ENDIF}
{ -------------------------------------------------------------------------- }
function TAbCustomZipOutline.GetDragMode : TDragMode;
begin
  Result := FOutline.DragMode;
end;
{ -------------------------------------------------------------------------- }
function TAbCustomZipOutline.GetItem(Index : Integer) : TAbZipItem;
begin
  if Assigned(FArchive) then
    Result := TAbZipItem(FArchive.ItemList[Index])
  else
    Result := nil;
end;
{ -------------------------------------------------------------------------- }
function TAbCustomZipOutline.GetPictureDirectory : TBitmap;
begin
  Result := FOutline.zdPictureDirectory;
end;
{ -------------------------------------------------------------------------- }
function TAbCustomZipOutline.GetPictureFile : TBitmap;
begin
  Result := FOutline.zdPictureFile;
end;
{ -------------------------------------------------------------------------- }
function TAbCustomZipOutline.GetPictureZipAttribute: TBitmap;
begin
  Result := FOutline.zdPictureZipAttribute;
end;
{ -------------------------------------------------------------------------- }
function TAbCustomZipOutline.GetPictureDirectorySelected : TBitmap;
begin
  Result := FOutline.zdPictureDirectorySelected;
end;
{ -------------------------------------------------------------------------- }
function TAbCustomZipOutline.GetPictureFileSelected : TBitmap;
begin
  Result := FOutline.zdPictureFileSelected;
end;
{ -------------------------------------------------------------------------- }
function TAbCustomZipOutline.GetPictureZipAttributeSelected: TBitmap;
begin
  Result := FOutline.zdPictureZipAttributeSelected;
end;
{ -------------------------------------------------------------------------- }
function TAbCustomZipOutline.GetPictureHeight: Integer;
begin
  Result := FOutline.FBitMapHeight;
end;
{ -------------------------------------------------------------------------- }
function TAbCustomZipOutline.GetPictureWidth: Integer;
begin
  Result := FOutline.FBitMapWidth;
end;
{ -------------------------------------------------------------------------- }
function TAbCustomZipOutline.GetSelectedItem : LongInt;
begin
  Result := FOutline.Selected.AbsoluteIndex;
end;
{ -------------------------------------------------------------------------- }
function TAbCustomZipOutline.GetSelectedZipItem : TAbZipItem;
begin
  {returns nil if the currently selected item of the outline is a folder or
  a zip attribute}
  if FOutline.Items.Count > 0 then
    Result := FOutline.Selected.Data
  else
    Result := nil;
end;
{ -------------------------------------------------------------------------- }
function TAbCustomZipOutline.GetStatus : TAbArchiveStatus;
begin
  if Assigned(FArchive) then
    Result := FArchive.Status
  else
    Result := asInvalid;
end;
{ -------------------------------------------------------------------------- }
function TAbCustomZipOutline.GetTextItem(const Value: string): LongInt;
var
  oNode : TTreeNode;
  oHold : TTreeNode;
begin
  Result := -1;
  if FOutline.Items.Count <= 0 then
    exit;

  oNode := FOutline.Items[0];
  while oNode <> nil do begin
    if oNode.Text = Value then
      break;
    oHold := oNode;
    oNode := nil;
    if oHold.HasChildren then
      oNode := oHold.getFirstChild;
    if oNode = nil then
      oNode := oHold.GetNextChild( oHold );
    if oNode = nil then
      oNode := oHold.GetNext;
  end;
  if oNode <> nil then
     Result := oNode.AbsoluteIndex
end;
{ -------------------------------------------------------------------------- }
function TAbCustomZipOutline.GetOutLineItem(X, Y : Integer): LongInt;
var
  oNode : TTreeNode;
begin
  oNode := FOutLine.GetNodeAt(X, X);
  if oNode <> nil then
    Result := oNode.AbsoluteIndex
  else
    Result := -1;
end;
{ -------------------------------------------------------------------------- }
function TAbCustomZipOutline.GetVersion : string;
begin
  Result := AbVersion;
end;
{ -------------------------------------------------------------------------- }
function TAbCustomZipOutline.GetZipfileComment : AnsiString;
begin
  if Assigned(FArchive) then
    Result := TAbZipArchive(FArchive).ZipfileComment
  else
    Result := '';
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.InitArchive;
begin
  if Assigned(FArchive) then begin
    {properties}
    FArchive.AutoSave := FAutoSave;
    FArchive.CompressionMethodToUse := FCompressionMethodToUse;
    SetBaseDirectory(FBaseDirectory);
    FArchive.DeflationOption := FDeflationOption;
{$IFDEF WIN32}
    FArchive.DOSMode := FDOSMode;
{$ENDIF}
    FArchive.ExtractOptions := FExtractOptions;
    FArchive.LogFile := FLogFile;
    FArchive.Logging := FLogging;
    FArchive.Password := FPassword;
    FArchive.PasswordRetries := FPasswordRetries;
    FArchive.StoreOptions := FStoreOptions;
    FArchive.TempDirectory := FTempDirectory;
    FArchive.SpanningThreshold := FSpanningThreshold;
    {events}
    TAbZipArchiveFriend(FArchive).ExtractHelper := UnzipProc;
    TAbZipArchiveFriend(FArchive).ExtractToStreamHelper := UnzipToStreamProc;
    TAbZipArchiveFriend(FArchive).InsertHelper := ZipProc;
    TAbZipArchiveFriend(FArchive).InsertFromStreamHelper := ZipFromStreamProc;
    FArchive.OnProcessItemFailure := DoProcessItemFailure;
    FArchive.OnArchiveItemProgress := DoArchiveItemProgress;
    FArchive.OnArchiveProgress := DoArchiveProgress;
    FArchive.OnConfirmProcessItem := DoConfirmProcessItem;
    FArchive.OnConfirmOverwrite := DoConfirmOverwrite;
    FArchive.OnConfirmSave := DoConfirmSave;
    FArchive.OnLoad := DoLoad;
    FArchive.OnSave := DoSave;
    FArchive.OnRequestImage := FOnRequestImage;
    FArchive.OnNeedPassword := DoNeedPassword;
    FArchive.OnRequestBlankDisk := FOnRequestBlankDisk;
    FArchive.OnRequestLastDisk := FOnRequestLastDisk;
    FArchive.OnRequestNthDisk := FOnRequestNthDisk;
    TAbZipArchiveFriend(FArchive).TestHelper := TestItemProc;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.Loaded;
begin
  inherited Loaded;
  FOutline.OnClick := DoClick;
  FOutline.OnCollapsed := DoCollapse;
  FOutline.OnDblClick := DoDblClick;
  FOutline.OnDragDrop := DoDragDrop;
  FOutline.OnDragOver := DoDragOver;
  FOutline.OnEndDrag := DoOnEndDrag;
  FOutline.OnEnter := DoOnEnter;
  FOutline.OnExit := DoOnExit;
  FOutline.OnExpanded := DoExpand;
  FOutline.OnKeyDown := DoKeyDown;
  FOutline.OnKeyPress := DoKeyPress;
  FOutline.OnKeyUp := DoKeyUp;
  FOutline.OnMouseDown := DoMouseDown;
  FOutline.OnMouseMove := DoMouseMove;
  FOutline.OnMouseUp := DoMouseUp;
{$IFDEF WIN32}
  {$IFDEF NeedMouseWheel}                                              
  FOutline.OnMouseWheel := DoMouseWheel;
  {$ENDIF}
  FOutline.OnStartDrag := DoOnStartDrag;
{$ENDIF WIN32}
  if Assigned(FOnWindowsDrop) then
    FOutline.OnWindowsDrop := DoWindowsDrop
  else
    FOutline.OnWindowsDrop := nil;
  DoChange;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.Move(aItem : TAbArchiveItem; NewStoredPath : string);
begin
  if Assigned(FArchive) then
    FArchive.Move(aItem, NewStoredPath)
  else
    raise EAbNoArchive.Create;
  DoChange;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.Notification(Component: TComponent;
  Operation: TOperation);
begin
  inherited Notification(Component, Operation);
  if (Component is TControl) and (Operation = opRemove) then
{$IFDEF UsingCLX}
    if Component = FItemProgressMeter then
      FItemProgressMeter := nil
    else if Component = FArchiveProgressMeter then
      FArchiveProgressMeter := nil
{$ELSE}
    if (Component as TControl) = FItemProgressMeter then
      FItemProgressMeter := nil
    else if (Component as TControl) = FArchiveProgressMeter then
      FArchiveProgressMeter := nil
{$ENDIF UsingCLX}
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.PutItem(Index : Integer; Value : TAbZipItem);
begin
  if Assigned(FArchive) then
    FArchive.ItemList[Index] := Value
  else
    raise EAbNoArchive.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.Replace(aItem : TAbArchiveItem);
  {replace the item}
begin
  if Assigned( FArchive ) then
    FArchive.Replace( aItem )
  else
    raise EAbNoArchive.Create;
  DoChange;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.Save;
begin
  if Assigned(FArchive) then begin
    FArchive.Save;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.SetAttributes(Value : TAbZipAttributes);
begin
  FAttributes := Value;
  UpdateOutline;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.SetAutoSave(Value : Boolean);
begin
  FAutoSave := Value;
  if Assigned(FArchive) then
    FArchive.AutoSave := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.SetBaseDirectory(Value : string);
begin
  if Assigned(FArchive) then begin
    FArchive.BaseDirectory := Value;
    FBaseDirectory := FArchive.BaseDirectory;
  end
  else
    FBaseDirectory := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.SetBorderStyle(Value : TBorderStyle);
begin
  FOutline.BorderStyle := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.SetCompressionMethodToUse(
                                            Value : TAbZipSupportedMethod);
begin
  FCompressionMethodToUse := Value;
  if Assigned(FArchive) then
    FArchive.CompressionMethodToUse := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.SetCursor(Value : TCursor);
begin
  FOutline.Cursor := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.SetDeflationOption(Value : TAbZipDeflationOption);
begin
  FDeflationOption := Value;
  if Assigned(FArchive) then
    FArchive.DeflationOption := Value;
end;
{ -------------------------------------------------------------------------- }
{$IFDEF WIN32}
procedure TAbCustomZipOutline.SetDOSMode(Value : Boolean);
begin
  FDOSMode := Value;
  if Assigned(FArchive) then
    FArchive.DOSMode := Value;
end;
{$ENDIF}
{ -------------------------------------------------------------------------- }
{$IFNDEF UsingCLX}
procedure TAbCustomZipOutline.SetDragCursor(Value : TCursor);
begin
  FOutline.DragCursor := Value;
end;
{$ENDIF}
{ -------------------------------------------------------------------------- }
{$IFNDEF UsingCLX}
procedure TAbCustomZipOutline.SetDragMode(Value : TDragMode);
begin
  {$IFDEF Win32}
  inherited SetDragMode(Value);
  {$ENDIF}
  FOutline.DragMode := Value;
end;
{$ENDIF}
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.SetExtractOptions(Value : TAbExtractOptions);
begin
  FExtractOptions := Value;
  if Assigned(FArchive) then
    FArchive.ExtractOptions := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.SetFileName(const aFileName : string);
begin
  if Assigned(FArchive) and (Status = asBusy) then
    raise EAbArchiveBusy.Create;
  FFileName := aFileName;
  try
    if Assigned(FArchive) then
      FArchive.Save;
  except
  end;
  FArchive.Free;
  FArchive := nil;
  if FileName <> '' then
    if FileExists(FileName) then begin
      if csDesigning in ComponentState then
        FArchive := TAbZipArchive.Create(FileName,
                                          fmOpenRead or
                                          fmShareDenyNone)
      else begin
        try
          FArchive := TAbZipArchive.Create(FileName,
                                            fmOpenReadWrite or
                                            fmShareDenyWrite);
        except
          {deals with read-only files}
          FArchive := TAbZipArchive.Create(FileName,
                                            fmOpenRead or
                                            fmShareDenyWrite);
        end;
        InitArchive;
      end;
      FArchive.Load;
    end
    else begin
      FArchive := TAbZipArchive.Create(FileName,
                                        fmCreate or fmShareDenyNone);
      InitArchive;
      try
        FArchive.Load;
      except
      end;
    end;
  DoChange;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.SetHierarchy(Value : Boolean);
begin
  FHierarchy := Value;
  UpdateOutline;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.SetLogFile(Value : string);
begin
  FLogFile := Value;
  if (csDesigning in ComponentState) then
    Exit;
  if Assigned(FArchive) then
    FArchive.LogFile := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.SetLogging(Value : Boolean);
begin
  FLogging := Value;
  if (csDesigning in ComponentState) then
    Exit;
  if Assigned(FArchive) then
    FArchive.Logging:= Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.SetOnRequestImage(Value : TAbRequestImageEvent);
begin
  FOnRequestImage := Value;
  if Assigned(FArchive) then
    FArchive.OnRequestImage := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.SetOnRequestLastDisk(Value : TAbRequestDiskEvent);
begin
  FOnRequestLastDisk := Value;
  if Assigned(FArchive) then
    FArchive.OnRequestLastDisk := FOnRequestLastDisk;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.SetOnRequestNthDisk(Value : TAbRequestNthDiskEvent);
begin
  FOnRequestNthDisk := Value;
  if Assigned(FArchive) then
    FArchive.OnRequestNthDisk := FOnRequestNthDisk;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.SetOnRequestBlankDisk(Value : TAbRequestDiskEvent);
begin
  FOnRequestBlankDisk := Value;
  if Assigned(FArchive) then
    FArchive.OnRequestBlankDisk := FOnRequestBlankDisk;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.SetOnWindowsDrop(Value : TWindowsDropEvent);
begin
  FOnWindowsDrop := Value;
  if csLoading in ComponentState then
    Exit;
  if csDestroying in ComponentState then
    Exit;
  if Assigned(Value) then
    FOutline.OnWindowsDrop := DoWindowsDrop
  else
    FOutline.OnWindowsDrop := nil;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.SetPassword(Value : AnsiString);
begin
  FPassword := Value;
  if Assigned(FArchive) then
    FArchive.Password := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.SetPasswordRetries(Value : Byte);
begin
  FPasswordRetries := Value;
  if Assigned(FArchive) then
    FArchive.PasswordRetries := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.SetPictureDirectory(Value : TBitmap);
begin
  if Value <> nil then begin
    if (Value.Height = FOutline.FBitMapHeight) and
       (Value.Width = FOutline.FBitMapWidth) then
      FOutline.zdPictureDirectory := Value;
  end else
    FOutline.zdPictureDirectory := nil;

end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.SetPictureFile(Value : TBitmap);
begin
  if Value <> nil then begin
    if (Value.Height = FOutline.FBitMapHeight) and
       (Value.Width = FOutline.FBitMapWidth) then
      FOutline.zdPictureFile := Value;
  end else
    FOutline.zdPictureFile := nil;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.SetPictureZipAttribute(Value : TBitmap);
begin
  if Value <> nil then begin
    if (Value.Height = FOutline.FBitMapHeight) and
       (Value.Width = FOutline.FBitMapWidth) then
      FOutline.zdPictureZipAttribute := Value;
  end else
    FOutline.zdPictureZipAttribute := nil;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.SetPictureDirectorySelected(Value : TBitmap);
begin
  if Value <> nil then begin
    if (Value.Height = FOutline.FBitMapHeight) and
       (Value.Width = FOutline.FBitMapWidth) then
      FOutline.zdPictureDirectorySelected := Value;
  end else
    FOutline.zdPictureDirectorySelected := nil;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.SetPictureFileSelected(Value : TBitmap);
begin
  if Value <> nil then begin
    if (Value.Height = FOutline.FBitMapHeight) and
       (Value.Width = FOutline.FBitMapWidth) then
      FOutline.zdPictureFileSelected := Value;
  end else
    FOutline.zdPictureFileSelected := nil;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.SetPictureZipAttributeSelected(Value : TBitmap);
begin
  if Value <> nil then begin
    if (Value.Height = FOutline.FBitMapHeight) and
       (Value.Width = FOutline.FBitMapWidth) then
      FOutline.zdPictureZipAttributeSelected := Value;
  end else
    FOutline.zdPictureZipAttributeSelected := nil;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.SetPictureHeight(Value : Integer);
begin
  FOutline.FBitMapHeight := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.SetPictureWidth(Value : Integer);
begin
  FOutline.FBitMapWidth := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.SetSelectedItem(Value : LongInt);
begin
  if ( Value >= 0 ) and ( Value <= pred( FOutline.Items.Count )) then
  FOutline.Selected := FOutline.Items[ Value ];
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.SetStoreOptions(Value : TAbStoreOptions);
begin
  FStoreOptions := Value;
  if Assigned(FArchive) then
    FArchive.StoreOptions := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.SetTempDirectory(Value : string);
begin
  FTempDirectory := Value;
  if Assigned(FArchive) then
    FArchive.TempDirectory := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.SetSpanningThreshold(Value : Longint);
begin
  FSpanningThreshold := Value;
  if Assigned(FArchive) then
    FArchive.SpanningThreshold := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.SetVersion(Value : string);
begin
  {NOP}
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.SetZipfileComment(Value : AnsiString);
begin
  if Assigned(FArchive) then
    TAbZipArchive(FArchive).ZipfileComment := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.TagItems(const FileMask : string);
  {tag all items that match the mask}
begin
  if Assigned(FArchive) then
    FArchive.TagItems(FileMask)
  else
    raise EAbNoArchive.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.UnTagItems(const FileMask : string);
  {clear tags for all items that match the mask}
begin
  if Assigned(FArchive) then
    FArchive.UnTagItems(FileMask)
  else
    raise EAbNoArchive.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.UnzipProc(Sender : TObject;
                                        Item : TAbArchiveItem;
                                        const NewName : string);
begin
  AbUnzip( TAbZipArchive(Sender), TAbZipItem(Item), NewName);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.UnzipToStreamProc(Sender : TObject;
                                                Item : TAbArchiveItem;
                                                OutStream : TStream);
begin
  if Assigned(OutStream) then
    AbUnzipToStream(TAbZipArchive(Sender), TAbZipItem(Item), OutStream);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.UpdateOutline;
var
  Found : Boolean;
  i : Integer;
  CurRoot : TTreeNode;
  CurParent : TTreeNode;
  CurChild : TTreeNode;
  RootNode : TTreeNode;
  oNode : TTreeNode;
  SubDir : string;
  ItemString : string;

  function GetSubDir(var ItemString : string) : string;
  var
    i : Integer;
  begin
{$IFDEF WIN32}
    i := Pos('\', ItemString);
{$ENDIF}
{$IFDEF LINUX}
    i := Pos('/', ItemString);
{$ENDIF}
    Result := '';
    if i > 0 then begin
      Result := Copy(ItemString, 1, pred(i));
      System.Delete(ItemString, 1, i);
    end;
  end;
begin
  RootNode := nil;
  CurRoot := nil;
  FOutline.Items.Clear;
  if not Assigned(FArchive) then
    Exit;
  if FArchive.Count = 0 then
    Exit;

  FOutline.IndexBitmaps;

  if Hierarchy then begin
    for i := 0 to pred(FArchive.Count) do begin
      {do not display deleted items...}
      if FArchive.ItemList[i].Action = aaDelete then
        continue;

      ItemString := FArchive.ItemList[i].FileName;
      AbUnfixName(ItemString);

{$IFDEF WIN32}
      if ItemString[ Length( ItemString )] = AbPathDelim then
{$ENDIF}
{$IFDEF LINUX}
      if ItemString[ Length( ItemString )] = AbPathDelim then
{$ENDIF}
        continue;

      if ( FOutline.Items.Count <> 0 ) and ( CurRoot <> nil ) then begin
        SubDir := GetSubDir(ItemString);
        
        if RootNode = nil then
          RootNode := FOutline.TopItem;

        CurParent := RootNode;
        while CurParent <> nil do begin
          if CurParent.Text = SubDir then begin
            CurRoot := CurParent;
            break;
          end else begin
            CurParent := CurParent.getNextSibling;
          end;
        end;
        if CurParent = nil then begin
          ItemString := FArchive.ItemList[i].FileName;
          AbUnfixName(ItemString);
        end;
      end else
        CurParent := nil;

      SubDir := GetSubDir(ItemString);
      while SubDir <> '' do begin
        if CurParent <> nil then begin
          Found := False;
          CurChild := CurParent.GetFirstChild;
          while CurChild <> nil do begin
            if CurChild.Text <> SubDir then
              CurChild := CurParent.GetNextChild( CurChild )
            else begin
              Found := True;
              break;
            end;
          end;
          if Found then
            CurParent := CurChild
          else begin
            if ItemString <> '' then begin
              CurParent := FOutline.Items.AddChild( CurParent, SubDir );
              CurParent.ImageIndex    := FOutline.FDirectoryIndex;
              CurParent.SelectedIndex := FOutline.FDirSelectedIndex;
            end;
          end;
        end else begin
          if ItemString <> '' then begin
            CurRoot := FOutline.Items.Add( nil, SubDir );
            if FOutline.Items.Count = 1 then
              RootNode := CurRoot;
            CurRoot.ImageIndex := FOutline.FDirectoryIndex;
            CurRoot.SelectedIndex := FOutline.FDirSelectedIndex;
            CurParent := CurRoot
          end;
        end;
        SubDir := GetSubDir(ItemString);
      end;
      if ItemString <> '' then begin
//        oNode := FOutline.Items.AddChild(CurParent, ItemString);     {!!.03}
        oNode := FOutline.Items.AddChildObject(CurParent, ItemString,  {!!.03}
          FArchive.ItemList[i]);                                       {!!.03}

        if FOutline.Items.Count = 1 then
          RootNode := oNode;
        oNode.ImageIndex := FOutline.FFileIndex;
        oNode.SelectedIndex := FOutline.FFileSelectedIndex;
        AddAttributeNodes(TAbZipItem(FArchive.ItemList[i]), oNode);
      end;
    end;
  end
  else begin
    for i := 0 to pred(FArchive.Count) do begin
      ItemString := FArchive.ItemList[i].FileName;
      AbUnfixName(ItemString);
//      oNode := FOutline.Items.Add( FOutline.Selected, ItemString );  {!!.03}
      oNode := FOutline.Items.AddObject(FOutline.Selected, ItemString, {!!.03}
        FArchive.ItemList[i]);                                         {!!.03}

      oNode.ImageIndex := FOutline.FFileIndex;
      oNode.SelectedIndex := FOutline.FFileSelectedIndex;
      AddAttributeNodes(TAbZipItem(FArchive.ItemList[i]), oNode);
    end;
  end;
  FullExpand;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.TestItemProc(Sender : TObject; Item : TAbArchiveItem);
begin
  AbTestZipItem(TAbZipArchive(Sender), TAbZipItem(Item));
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.TestTaggedItems;
  {Test specified items}
begin
  if Assigned(FArchive) then
    FArchive.TestTaggedItems
  else
    raise EAbNoArchive.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.ZipProc(Sender : TObject;
                             Item : TAbArchiveItem;
                             OutStream : TStream);
begin
  AbZip(TAbZipArchive(Sender), TAbZipItem(Item), OutStream);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.ZipFromStreamProc(Sender : TObject;
                                                Item : TAbArchiveItem;
                                                OutStream, InStream : TStream);
begin
  if Assigned(InStream) then
    AbZipFromStream(TAbZipArchive(Sender), TAbZipItem(Item),
                    OutStream, InStream)
  else
    raise EAbZipNoInsertion.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.CloseArchive;
  {closes the archive by setting FileName to ''}
begin
  if FFileName <> '' then
    FileName := '';
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipOutline.OpenArchive(const aFileName : String);
  {opens the archive}
begin
  FileName := AFileName;
end;
{ -------------------------------------------------------------------------- }
procedure InitStrings;
var
  i : TAbZipCompressionMethod;
begin
  for i := Low(TAbZipCompressionMethod) to High(TAbZipCompressionMethod) do
    MethodStrings[i] := AbStrRes(AbMethod + Ord(i));
end;
{ -------------------------------------------------------------------------- }
initialization
  InitStrings;
end.

