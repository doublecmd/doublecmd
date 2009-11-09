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
{* ABBREVIA: AbZipper.pas 3.05                           *}
{*********************************************************}
{* ABBREVIA: Non-visual Component with Zip support       *}
{*********************************************************}

unit AbZipper;

{$I AbDefine.inc}

interface

uses
  SysUtils, Classes,
  AbBrowse, AbZBrows, AbUtils, AbArcTyp, AbZipTyp;

type
  TAbCustomZipper = class(TAbCustomZipBrowser)
  protected {private}
    FAutoSave               : Boolean;
    FCompressionMethodToUse : TAbZipSupportedMethod;
    FDeflationOption        : TAbZipDeflationOption;
    FDOSMode : Boolean;
    FOnConfirmSave          : TAbArchiveConfirmEvent;
    FOnSave                 : TAbArchiveEvent;
    FOnArchiveSaveProgress  : TAbArchiveProgressEvent;                 {!!.04}
    FArchiveSaveProgressMeter : TAbBaseMeterLink;                          {!!.04}

    FStoreOptions           : TAbStoreOptions;

  protected {methods}
    procedure DoConfirmSave(Sender : TObject; var Confirm : Boolean);
      virtual;
    procedure DoSave(Sender : TObject);
      virtual;
    procedure DoArchiveSaveProgress(Sender : TObject; Progress : Byte;{!!.04}
                                    var Abort : Boolean);             {!!.04}

    procedure InitArchive;
      override;
    procedure SetAutoSave(Value : Boolean);
    procedure SetCompressionMethodToUse(Value : TAbZipSupportedMethod);
    procedure SetDeflationOption(Value : TAbZipDeflationOption);
    procedure SetDOSMode( Value : Boolean );
    procedure SetFileName(const aFileName : string);
      override;
    procedure SetStoreOptions( Value : TAbStoreOptions );
    procedure SetZipfileComment(const Value : string);
      override;
    procedure ZipProc(Sender : TObject; Item : TAbArchiveItem;
                      OutStream : TStream);
    procedure ZipFromStreamProc(Sender : TObject; Item : TAbArchiveItem;
                                OutStream, InStream : TStream );
    procedure Notification(Component: TComponent;                      {!!.04}
      Operation: TOperation); override;                                {!!.04}
    procedure ResetMeters; override;                                   {!!.04}

  protected {properties}
    property AutoSave : Boolean
      read  FAutoSave
      write SetAutoSave;
    property CompressionMethodToUse : TAbZipSupportedMethod
      read  FCompressionMethodToUse
      write SetCompressionMethodToUse
      default AbDefCompressionMethodToUse;
    property DeflationOption : TAbZipDeflationOption
      read  FDeflationOption
      write SetDeflationOption
      default AbDefDeflationOption;
    property DOSMode : Boolean
      read  FDOSMode
      write SetDOSMode;
    property StoreOptions : TAbStoreOptions
      read  FStoreOptions
      write SetStoreOptions
      default AbDefStoreOptions;
    property ArchiveSaveProgressMeter : TAbBaseMeterLink                   {!!.04}
      read  FArchiveSaveProgressMeter                                  {!!.04}
      write FArchiveSaveProgressMeter;                                 {!!.04}


  protected {events}
    property OnConfirmSave : TAbArchiveConfirmEvent
      read  FOnConfirmSave
      write FOnConfirmSave;
    property OnSave : TAbArchiveEvent
      read  FOnSave
      write FOnSave;
    property OnArchiveSaveProgress : TAbArchiveProgressEvent           {!!.04}
      read FOnArchiveSaveProgress                                      {!!.04}
      write FOnArchiveSaveProgress;                                    {!!.04}

  public {methods}
    constructor Create(AOwner : TComponent);
      override;
    destructor Destroy;
      override;
    procedure AddEntries(const Paths : String; const ArchiveDirectory : String);
    procedure AddFiles(const FileMask : string; SearchAttr : Integer);
    procedure AddFilesEx(const FileMask, ExclusionMask : string; SearchAttr : Integer);
    procedure AddFromStream(const NewName : string; FromStream : TStream);
    procedure DeleteAt(Index : Integer);
    procedure DeleteFiles(const FileMask : string; Recursive : Boolean = False);
    procedure DeleteFilesEx(const FileMask, ExclusionMask : string; Recursive : Boolean = False);
    procedure DeleteDirectoriesRecursively(const Paths : string);
    procedure DeleteTaggedItems;
    procedure FreshenFiles(const FileMask : string);
    procedure FreshenFilesEx(const FileMask, ExclusionMask : string);
    procedure FreshenTaggedItems;
    procedure Move(aItem : TAbArchiveItem; const NewStoredPath : string);
    procedure Save;
    procedure Replace(aItem : TAbArchiveItem);
  end;

type
  TAbZipper = class(TAbCustomZipper)
  published
    property ArchiveProgressMeter;
    property ArchiveSaveProgressMeter;                                 {!!.04}
    property ItemProgressMeter;
    property AutoSave;
    property BaseDirectory;
    property CompressionMethodToUse;
    property DeflationOption;
    property DOSMode;
    property SpanningThreshold;
    property LogFile;
    property Logging;
    property OnArchiveProgress;
    property OnArchiveSaveProgress;                                    {!!.04}
    property OnArchiveItemProgress;
    property OnChange;
    property OnConfirmProcessItem;
    property OnConfirmSave;
    property OnLoad;
    property OnProcessItemFailure;
    property OnRequestBlankDisk;
    property OnRequestImage;
    property OnRequestLastDisk;
    property OnRequestNthDisk;
    property OnSave;
    property Password;
    property StoreOptions;
    property TempDirectory;
    property Version;
    property FileName; {must be after OnLoad}
  end;

implementation

uses
  AbExcept,
  AbZipPrc,
  AbTarTyp,
  AbGzTyp;

{ -------------------------------------------------------------------------- }
constructor TAbCustomZipper.Create( AOwner : TComponent );
begin
  inherited Create( AOwner );
  CompressionMethodToUse := AbDefCompressionMethodToUse;
  DeflationOption := AbDefDeflationOption;
  StoreOptions := AbDefStoreOptions;
end;
{ -------------------------------------------------------------------------- }
destructor TAbCustomZipper.Destroy;
begin
  inherited Destroy;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.AddEntries(const Paths : String; const ArchiveDirectory : String);
  {Add specific entries (files or directories), separates by AbPathSep.
   Each entry can't have wildcards, must be absolute path,
   or relative to BaseDirectory.
   ArchivePath specifies, where in the archive directory structure
   these entries will be put.}
begin
  if (ZipArchive <> nil) then
    ZipArchive.AddEntries(Paths, ArchiveDirectory)
  else
    raise EAbNoArchive.Create;
  DoChange;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.AddFiles(const FileMask : string; SearchAttr : Integer);
  {Add files to the archive where the disk filespec matches}
begin
  if (ZipArchive <> nil) then
    ZipArchive.AddFiles(FileMask, SearchAttr)
  else
    raise EAbNoArchive.Create;
  DoChange;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.AddFilesEx(const FileMask, ExclusionMask : string;
  SearchAttr : Integer);
  {Add files that match Filemask except those matching ExclusionMask}
begin
  if (ZipArchive <> nil) then
    ZipArchive.AddFilesEx(FileMask, ExclusionMask, SearchAttr)
  else
    raise EAbNoArchive.Create;
  DoChange;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.AddFromStream(const NewName : string;
                                        FromStream : TStream);
  {Add stream directly to archive}
begin
  if (ZipArchive <> nil) then begin
    FromStream.Position := 0;
    ZipArchive.AddFromStream(NewName, FromStream);
  end else
    raise EAbNoArchive.Create;
  DoChange;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.DeleteFiles(const FileMask : string; Recursive : Boolean);
  {delete all files from the archive that match the file mask}
  {if Recursive=False only files from matching directory are deleted}
  {Examples:
    DeleteFiles('dir/*.*', False) - deletes all files in dir/
    DeleteFiles('dir/*.*', True)  - delete all files in dir/ and in subdirectories
  }
begin
  if (ZipArchive <> nil) then
    ZipArchive.DeleteFiles( FileMask, Recursive )
  else
    raise EAbNoArchive.Create;
  DoChange;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.DeleteAt(Index : Integer);
  {delete item at Index}
begin
  if (ZipArchive <> nil) then
    ZipArchive.DeleteAt( Index )
  else
    raise EAbNoArchive.Create;
  DoChange;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.DeleteFilesEx(const FileMask, ExclusionMask : string;
                                        Recursive : Boolean);
  {Delete files that match Filemask except those matching ExclusionMask}
  {if Recursive=False only files from matching directory are deleted}
  {Examples:
    DeleteFilesEx('dir/*;*.pas', 'src/*', True)
      deletes all files in dir/ and in subdirectories, and all *.pas in every directory
      but does not delete anything in src/ directory
  }
begin
  if (ZipArchive <> nil) then
    ZipArchive.DeleteFilesEx(FileMask, ExclusionMask, Recursive)
  else
    raise EAbNoArchive.Create;
  DoChange;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.DeleteDirectoriesRecursively(const Paths : string);
  {Delete directory entry and all file and directory entries matching the same path recursively}
  {Example: 'src/;bin/' deletes whole src/ and bin/ dirs with all files and directories in them}
begin
  if (ZipArchive <> nil) then
    ZipArchive.DeleteDirectoriesRecursively(Paths)
  else
    raise EAbNoArchive.Create;
  DoChange;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.DeleteTaggedItems;
  {delete all tagged items from the archive}
begin
  if (ZipArchive <> nil) then
    ZipArchive.DeleteTaggedItems
  else
    raise EAbNoArchive.Create;
  DoChange;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.DoConfirmSave(Sender : TObject; var Confirm : Boolean);
begin
  Confirm := True;
  if Assigned(FOnConfirmSave) then
    FOnConfirmSave(Self, Confirm);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.DoSave(Sender : TObject);
begin
  if Assigned(FOnSave) then
    FOnSave(Self);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.FreshenFiles(const FileMask : string);
  {freshen all items that match the file mask}
begin
  if (ZipArchive <> nil) then
    ZipArchive.FreshenFiles( FileMask )
  else
    raise EAbNoArchive.Create;
  DoChange;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.FreshenFilesEx(const FileMask, ExclusionMask : string);
  {freshen all items matching FileMask except those matching ExclusionMask}
begin
  if (ZipArchive <> nil) then
    ZipArchive.FreshenFilesEx( FileMask, ExclusionMask )
  else
    raise EAbNoArchive.Create;
  DoChange;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.FreshenTaggedItems;
  {freshen all tagged items}
begin
  if (ZipArchive <> nil) then
    ZipArchive.FreshenTaggedItems
  else
    raise EAbNoArchive.Create;
  DoChange;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.InitArchive;
begin
  inherited InitArchive;
  if (ZipArchive <> nil) then begin
    if (ZipArchive is TAbZipArchive) then begin
      {properties}
      ZipArchive.AutoSave                              := FAutoSave;
      TAbZipArchive(ZipArchive).CompressionMethodToUse := FCompressionMethodToUse;
      TAbZipArchive(ZipArchive).DeflationOption        := FDeflationOption;
      FArchive.DOSMode                                 := FDOSMode;
      ZipArchive.StoreOptions                          := FStoreOptions;
      {events}
      ZipArchive.OnArchiveSaveProgress                 := DoArchiveSaveProgress; {!!.04}
      ZipArchive.OnConfirmSave                         := DoConfirmSave;
      TAbZipArchive(ZipArchive).OnRequestBlankDisk     := OnRequestBlankDisk;
      ZipArchive.OnSave                                := DoSave;
      TAbZipArchive(ZipArchive).InsertHelper           := ZipProc;
      TAbZipArchive(ZipArchive).InsertFromStreamHelper := ZipFromStreamProc;
    end;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.Move(aItem : TAbArchiveItem; const NewStoredPath : string);
  {renames the item}
begin
  if (ZipArchive <> nil) then
    ZipArchive.Move(aItem, NewStoredPath)
  else
    raise EAbNoArchive.Create;
  DoChange;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.Replace(aItem : TAbArchiveItem);
  {replace the item}
begin
  if (ZipArchive <> nil) then
    ZipArchive.Replace( aItem )
  else
    raise EAbNoArchive.Create;
  DoChange;                                                            
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.Save;
begin
  if (ZipArchive <> nil) then begin
    ZipArchive.Save;
    DoChange;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.SetAutoSave(Value : Boolean);
begin
  FAutoSave := Value;
  if (ZipArchive <> nil) then
    ZipArchive.AutoSave := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.SetCompressionMethodToUse(
  Value : TAbZipSupportedMethod);
begin
  FCompressionMethodToUse := Value;
  if (ZipArchive <> nil) then
    (ZipArchive as TAbZipArchive).CompressionMethodToUse := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.SetDeflationOption(Value : TAbZipDeflationOption);
begin
  FDeflationOption := Value;
  if (ZipArchive <> nil) then
    (ZipArchive as TAbZipArchive).DeflationOption := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.SetDOSMode(Value : Boolean);
begin
  FDOSMode := Value;
  if (ZipArchive <> nil) then
    ZipArchive.DOSMode := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.SetFileName(const aFileName : string);
var
  ArcType : TAbArchiveType;
begin

  FFileName := aFileName;
  if (csDesigning in ComponentState) then
    Exit;

  if Assigned(FArchive) then
   begin
     FArchive.Save;
     FArchive.Free;
     FArchive := nil;
   end;

  ArcType := ArchiveType;

  if (FileName <> '') then
    if FileExists(FileName) then begin { open it }

    if not ForceType then
      ArcType := AbDetermineArcType(FileName, atUnknown);

      case ArcType of
        atZip, atSpannedZip, atSelfExtZip : begin                        {!!.02}
         FArchive := TAbZipArchive.Create(FileName, fmOpenRead or fmShareDenyNone);
         InitArchive;
        end;

        atTar : begin
          FArchive := TAbTarArchive.Create(FileName, fmOpenRead or fmShareDenyNone);
          inherited InitArchive;
        end;

        atGZip : begin
          FArchive := TAbGzipArchive.Create(FileName, fmOpenRead or fmShareDenyNone);
          TAbGzipArchive(FArchive).TarAutoHandle := FTarAutoHandle;
          TAbGzipArchive(FArchive).IsGzippedTar := False;
          inherited InitArchive;
        end;

        atGZippedTar : begin
          FArchive := TAbGzipArchive.Create(FileName, fmOpenRead or fmShareDenyNone);
          TAbGzipArchive(FArchive).TarAutoHandle := FTarAutoHandle;
          TAbGzipArchive(FArchive).IsGzippedTar := True;
          inherited InitArchive;
        end;

        else
          raise EAbUnhandledType.Create;
      end {case};
      FArchive.Load;
      FArchiveType := ArcType;

    end else begin  { file doesn't exist, so create a new one }
      if not ForceType then                                              {!!.01}
        ArcType := AbDetermineArcType(FileName, atUnknown);              {!!.01}

      case ArcType of                                                    {!!.01}
        atZip : begin                                                    
          FArchive := TAbZipArchive.Create(FileName, fmCreate or fmShareDenyWrite);
          InitArchive;
        end;

        atTar : begin
          FArchive := TAbTarArchive.Create(FileName, fmCreate or fmShareDenyWrite);
          inherited InitArchive;
        end;

        atGZip : begin
          FArchive := TAbGzipArchive.Create(FileName, fmCreate or fmShareDenyWrite);
          TAbGzipArchive(FArchive).TarAutoHandle := FTarAutoHandle;
          TAbGzipArchive(FArchive).IsGzippedTar := False;
          inherited InitArchive;
        end;

        atGZippedTar : begin
          FArchive := TAbGzipArchive.Create(FileName, fmCreate or fmShareDenyWrite);
          TAbGzipArchive(FArchive).TarAutoHandle := FTarAutoHandle;
          TAbGzipArchive(FArchive).IsGzippedTar := True;
          inherited InitArchive;
        end;

        else
          raise EAbUnhandledType.Create;
      end {case};

      FArchiveType := ArcType;
    end;
  DoChange;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.SetStoreOptions(Value : TAbStoreOptions);
begin
  FStoreOptions := Value;
  if (ZipArchive <> nil) then
    ZipArchive.StoreOptions := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.SetZipfileComment(const Value : string);
begin
  if (ZipArchive <> nil) then
    (ZipArchive as TAbZipArchive).ZipfileComment := Value
  else
    raise EAbNoArchive.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.ZipProc(Sender : TObject; Item : TAbArchiveItem;
  OutStream : TStream);
begin
  AbZip(TAbZipArchive(Sender), TAbZipItem(Item), OutStream);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.ZipFromStreamProc(Sender : TObject; Item : TAbArchiveItem;
  OutStream, InStream : TStream);
begin
  if Assigned(InStream) then
    AbZipFromStream(TAbZipArchive(Sender), TAbZipItem(Item),
                    OutStream, InStream)
  else
    raise EAbZipNoInsertion.Create;
end;
{ -------------------------------------------------------------------------- }
{!!.04 - Added}
procedure TAbCustomZipper.DoArchiveSaveProgress(Sender : TObject;
                                                Progress : Byte;
                                                var Abort : Boolean);
begin
  Abort := False;
  if Assigned(FArchiveSaveProgressMeter) then
    FArchiveSaveProgressMeter.DoProgress(Progress);
  if Assigned(FOnArchiveSaveProgress) then
    FOnArchiveSaveProgress(Self, Progress, Abort);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.Notification(Component: TComponent;
                                       Operation: TOperation);
begin
  inherited Notification(Component, Operation);
  if (Operation = opRemove) then
    if Component = FArchiveSaveProgressMeter then
      FArchiveSaveProgressMeter := nil
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.ResetMeters;
begin
  inherited ResetMeters;
  if Assigned(FArchiveSaveProgressMeter) then
    FArchiveSaveProgressMeter.Reset;
end;
{!!.04 - Added end}
{ -------------------------------------------------------------------------- }


end.

