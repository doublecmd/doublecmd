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
{* ABBREVIA: AbBrowse.pas 3.05                           *}
{*********************************************************}
{* ABBREVIA: Base Browser Component                      *}
{*********************************************************}

unit AbBrowse;

{$I AbDefine.inc}

interface

uses
  SysUtils, Classes,
  AbBase,
  AbUtils,
  AbArcTyp;

type
  TAbBaseMeterLink = class(TAbBaseComponent)
  public
    procedure DoProgress(Progress : Byte); virtual; abstract;
    procedure Reset; virtual; abstract;
  end;

  TAbBaseBrowser = class(TAbBaseComponent)
  public
    FArchive : TAbArchive;
  protected {private}
    FSpanningThreshold : Longint;
    FItemProgressMeter : TAbBaseMeterLink;
    FArchiveProgressMeter : TAbBaseMeterLink;
    FBaseDirectory : string;
    FFileName : string;
    FLogFile : string;
    FLogging : Boolean;
    FOnArchiveProgress : TAbArchiveProgressEvent;
    FOnArchiveItemProgress : TAbArchiveItemProgressEvent;
    FOnChange : TNotifyEvent;
    FOnConfirmProcessItem : TAbArchiveItemConfirmEvent;
    FOnLoad : TAbArchiveEvent;
    FOnProcessItemFailure : TAbArchiveItemFailureEvent;
    FOnRequestImage : TAbRequestImageEvent;
    FTempDirectory : string;

    { detected compression type }
    FArchiveType       : TAbArchiveType;
    FForceType             : Boolean;

  protected {private methods}
    function  GetCount : Integer;
    function  GetSpanned : Boolean;
    function  GetStatus : TAbArchiveStatus;
    procedure ResetMeters; virtual;                                    {!!.04}
    procedure SetCompressionType(const Value: TAbArchiveType);
    procedure SetBaseDirectory(const Value : string);
    procedure SetSpanningThreshold(Value : Longint);
    procedure SetLogFile(const Value : string);
    procedure SetLogging(Value : Boolean);
    procedure SetTempDirectory(const Value : string);

    procedure Loaded; override;
    procedure Notification(Component: TComponent;
                           Operation: TOperation); override;

  protected {virtual methods}
    procedure DoArchiveItemProgress(Sender : TObject;
                                    Item : TAbArchiveItem;
                                    Progress : Byte;
                                    var Abort : Boolean); virtual;
    procedure DoArchiveProgress(Sender : TObject;
                                Progress : Byte;
                                var Abort : Boolean); virtual;
    procedure DoChange; virtual;
    procedure DoConfirmProcessItem(Sender : TObject;
                                   Item : TAbArchiveItem;
                                   ProcessType : TAbProcessType;
                                   var Confirm : Boolean); virtual;
    procedure DoLoad(Sender : TObject); virtual;
    procedure DoProcessItemFailure(Sender : TObject;
                                    Item : TAbArchiveItem;
                                    ProcessType : TAbProcessType;
                                    ErrorClass : TAbErrorClass;
                                    ErrorCode : Integer); virtual;
    procedure SetOnRequestImage(Value : TAbRequestImageEvent); virtual;
    procedure InitArchive; virtual;

    {This method must be defined in descendent classes}
    procedure SetFileName(const aFileName : string); virtual; abstract;


  protected {properties}
    property Archive : TAbArchive
      read FArchive;
    property ArchiveProgressMeter : TAbBaseMeterLink
      read  FArchiveProgressMeter
      write FArchiveProgressMeter;
    property BaseDirectory : string
      read  FBaseDirectory
      write SetBaseDirectory;
    property FileName : string
      read  FFileName
      write SetFileName;
    property SpanningThreshold : Longint
      read  FSpanningThreshold
      write SetSpanningThreshold
      default 0;
    property ItemProgressMeter : TAbBaseMeterLink
      read  FItemProgressMeter
      write FItemProgressMeter;
    property LogFile : string
      read  FLogFile
      write SetLogFile;
    property Logging : Boolean
      read  FLogging
      write SetLogging
      default False;
    property Spanned : Boolean
      read GetSpanned;
    property TempDirectory : string
      read  FTempDirectory
      write SetTempDirectory;



  protected {events}
    property OnArchiveProgress : TAbArchiveProgressEvent
      read FOnArchiveProgress
      write FOnArchiveProgress;
    property OnArchiveItemProgress : TAbArchiveItemProgressEvent
      read FOnArchiveItemProgress
      write FOnArchiveItemProgress;
    property OnConfirmProcessItem : TAbArchiveItemConfirmEvent
      read FOnConfirmProcessItem
      write FOnConfirmProcessItem;
    property OnProcessItemFailure : TAbArchiveItemFailureEvent
      read FOnProcessItemFailure
      write FOnProcessItemFailure;
    property OnRequestImage : TAbRequestImageEvent
      read FOnRequestImage
      write SetOnRequestImage;

  public {methods}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure ClearTags;
      {Clear all tags from the archive}
    function FindItem(aItem : TAbArchiveItem) : Integer;
    function FindFile(const aFileName : string) : Integer;
    procedure TagItems(const FileMask : string);
      {tag all items that match the mask}
    procedure UnTagItems(const FileMask : string);
      {clear tags for all items that match the mask}
    procedure CloseArchive;
      {closes the archive by setting FileName to ''}
    procedure OpenArchive(const aFileName : string);
      {opens the archive}

  public {properties}
    property Count : Integer
      read GetCount;
    property Status : TAbArchiveStatus
      read GetStatus;

    property ArchiveType : TAbArchiveType
      read FArchiveType
      write SetCompressionType
      default atUnknown;

    property ForceType : Boolean
      read FForceType
      write FForceType
      default False;

  public {events}
    property OnChange : TNotifyEvent
      read FOnChange
      write FOnChange;
    property OnLoad : TAbArchiveEvent
      read FOnLoad
      write FOnLoad;
  end;

function AbDetermineArcType(const FN : string; AssertType : TAbArchiveType) : TAbArchiveType;

implementation

uses
  AbExcept,
{$IFDEF MSWINDOWS}
  AbCabTyp,
{$ENDIF}
  AbZipTyp,
  AbTarTyp,
  AbGzTyp,
  AbBzip2Typ,
  uClassesEx;

{ TAbBaseBrowser implementation ======================================= }

{ -------------------------------------------------------------------------- }
constructor TAbBaseBrowser.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FLogFile := '';
  FLogging := False;
  FSpanningThreshold := 0;

  FArchiveType := atUnknown;
  FForceType       := False;
end;
{ -------------------------------------------------------------------------- }
destructor TAbBaseBrowser.Destroy;
begin
 if Assigned(FArchive) then
  FArchive.Free;
  FArchive := nil;
  inherited Destroy;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseBrowser.ClearTags;
  {Clear all tags from the archive}
begin
  if Assigned(FArchive) then
    FArchive.ClearTags
  else
    raise EAbNoArchive.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseBrowser.CloseArchive;
  {closes the archive by setting FileName to ''}
begin
  if FFileName <> '' then
    FileName := '';
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseBrowser.DoArchiveItemProgress(Sender : TObject;
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
procedure TAbBaseBrowser.DoArchiveProgress(Sender : TObject;
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
procedure TAbBaseBrowser.DoChange;                                
begin
  if Assigned(FOnChange) then begin
    FOnChange(Self);
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseBrowser.DoConfirmProcessItem(Sender : TObject;
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
procedure TAbBaseBrowser.DoLoad(Sender : TObject);
begin
  if Assigned(FOnLoad) then
    FOnLoad(Self);
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseBrowser.DoProcessItemFailure(Sender : TObject;
                                              Item : TAbArchiveItem;
                                              ProcessType : TAbProcessType;
                                              ErrorClass : TAbErrorClass;
                                              ErrorCode : Integer);
begin
  if Assigned(FOnProcessItemFailure) then
    FOnProcessItemFailure(Self, Item, ProcessType, ErrorClass, ErrorCode);
end;
{ -------------------------------------------------------------------------- }
function TAbBaseBrowser.FindItem(aItem : TAbArchiveItem) : Integer;
begin
  if Assigned(FArchive) then
    Result := FArchive.FindItem(aItem)
  else
    Result := -1;
end;
{ -------------------------------------------------------------------------- }
function TAbBaseBrowser.FindFile(const aFileName : string) : Integer;
begin
  if Assigned(FArchive) then
    Result := FArchive.FindFile(aFileName)
  else
    Result := -1;
end;
{ -------------------------------------------------------------------------- }
function TAbBaseBrowser.GetSpanned : Boolean;
begin
  if Assigned(FArchive) then
    Result := FArchive.Spanned
  else
    Result := False;
end;
{ -------------------------------------------------------------------------- }
function TAbBaseBrowser.GetStatus : TAbArchiveStatus;
begin
  if Assigned(FArchive) then
    Result := FArchive.Status
  else
    Result := asInvalid;
end;
{ -------------------------------------------------------------------------- }
function TAbBaseBrowser.GetCount : Integer;
begin
  if Assigned(FArchive) then
    Result := FArchive.Count
  else
    Result := 0;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseBrowser.InitArchive;
begin
  ResetMeters;
  if Assigned(FArchive) then begin
    {properties}
    FArchive.SpanningThreshold     := FSpanningThreshold;
    FArchive.LogFile               := FLogFile;
    FArchive.Logging               := FLogging;
    FArchive.TempDirectory         := FTempDirectory;
    SetBaseDirectory(FBaseDirectory);
    {events}
    FArchive.OnArchiveProgress     := DoArchiveProgress;
    FArchive.OnArchiveItemProgress := DoArchiveItemProgress;
    FArchive.OnConfirmProcessItem  := DoConfirmProcessItem;
    FArchive.OnLoad                := DoLoad;
    FArchive.OnProcessItemFailure  := DoProcessItemFailure;
    FArchive.OnRequestImage        := FOnRequestImage;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseBrowser.Loaded;
begin
  inherited Loaded;
  DoChange;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseBrowser.Notification(Component: TComponent;
                                      Operation: TOperation);
begin
  inherited Notification(Component, Operation);
  if (Operation = opRemove) then
    if Component = FItemProgressMeter then
      FItemProgressMeter := nil
    else if Component = FArchiveProgressMeter then
      FArchiveProgressMeter := nil
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseBrowser.OpenArchive(const aFileName : string);
  {opens the archive}
begin
  FileName := AFileName;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseBrowser.ResetMeters;
begin
  if Assigned(FArchiveProgressMeter) then
    FArchiveProgressMeter.Reset;
  if Assigned(FItemProgressMeter) then
    FItemProgressMeter.Reset;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseBrowser.SetBaseDirectory(const Value : string);
begin
  if Assigned(FArchive) then begin
    FArchive.BaseDirectory := Value;
    FBaseDirectory := FArchive.BaseDirectory;
  end else
    FBaseDirectory := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseBrowser.SetSpanningThreshold(Value : Longint);
begin
  FSpanningThreshold := Value;
  if Assigned(FArchive) then
    FArchive.SpanningThreshold := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseBrowser.SetLogFile(const Value : string);
begin
  FLogFile := Value;
  if (csDesigning in ComponentState) then
    Exit;
  if Assigned(FArchive) then
    FArchive.LogFile := Value;
  SetLogging(Value <> '');
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseBrowser.SetLogging(Value : Boolean);
begin
  FLogging := Value;
  if (csDesigning in ComponentState) then
    Exit;
  if Assigned(FArchive) then
    FArchive.Logging:= Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseBrowser.SetOnRequestImage(Value : TAbRequestImageEvent);
begin
  FOnRequestImage := Value;
  if Assigned(FArchive) then
    FArchive.OnRequestImage := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseBrowser.SetTempDirectory(const Value : string);
begin
  FTempDirectory := Value;
  if Assigned(FArchive) then
    FArchive.TempDirectory := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseBrowser.TagItems(const FileMask : string);
  {tag all items that match the mask}
begin
  if Assigned(FArchive) then
    FArchive.TagItems(FileMask)
  else
    raise EAbNoArchive.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseBrowser.UnTagItems(const FileMask : string);
  {clear tags for all items that match the mask}
begin
  if Assigned(FArchive) then
    FArchive.UnTagItems(FileMask)
  else
    raise EAbNoArchive.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBaseBrowser.SetCompressionType(const Value: TAbArchiveType);
begin
  if not Assigned(FArchive) or (Status <> asInvalid) then
    FArchiveType := Value
  else
    raise EAbArchiveBusy.Create;
end;
{ -------------------------------------------------------------------------- }
function AbDetermineArcType(const FN : string; AssertType : TAbArchiveType) : TAbArchiveType;
var
  Ext : string;
  FS : TStream;
begin
  Result := AssertType;
  if Result = atUnknown then begin
    { Guess archive type based on it's extension }
    Ext := UpperCase(ExtractFileExt(FN));
    if (Ext = '.ZIP') or (Ext = '.JAR') then
      Result := atZip;
    if (Ext = '.TAR') then
      Result := atTar;
    if (Ext = '.GZ') then
      Result := atGzip;
    if (Ext = '.TGZ') then
      Result := atGzippedTar;
    if (Ext = '.CAB') then
      Result := atCab;
    if (Ext = '.BZ2') then
      Result := atBzip2;
    if (Ext = '.TBZ') then
      Result := atBzippedTar;
  end;
{$IFNDEF MSWINDOWS}
  if Result = atCab then
    Result := atUnknown;
{$ENDIF}
  if (Result <> atUnknown) and FileExists(FN) and (AbFileGetSize(FN) > 0) then begin
    { If the file doesn't exist (or is empty) presume to make one, otherwise
      verify the contents }
    FS := TFileStreamEx.Create(FN, fmOpenRead or fmShareDenyNone);
    try
      case Result of
        atZip : begin
          Result := VerifyZip(FS);
        end;
        atSelfExtZip : begin
          Result := VerifySelfExtracting(FS);
        end;
        atTar : begin
          Result := VerifyTar(FS);
        end;
        atGzip, atGzippedTar: begin
          Result := VerifyGzip(FS);
        end;
{$IFDEF MSWINDOWS}
        atCab : begin
          Result := VerifyCab(FS);
        end;
{$ENDIF}
        atBzip2, atBzippedTar: begin
          Result := VerifyBzip2(FS);
        end;
      end;
    finally
      FS.Free;
    end;
  end;
end;
{ -------------------------------------------------------------------------- }


end.
