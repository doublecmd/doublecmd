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
{* ABBREVIA: AbCabKit.PAS 3.04                           *}
{*********************************************************}
{* ABBREVIA: Cabinet file builder/extractor component    *}
{*********************************************************}

unit AbCabKit;

{$I AbDefine.inc}

interface

uses
  SysUtils, Classes, AbArcTyp, AbUtils, AbCabTyp,
  AbCabMak, AbCBrows;

type
  TAbCustomCabKit = class(TAbCustomMakeCab)
  protected {private}
    FExtractOptions : TAbExtractOptions;
    FOnConfirmOverwrite : TAbConfirmOverwriteEvent;

    procedure DoConfirmOverwrite(var Name : string;
                                 var Confirm : Boolean);
    procedure InitArchive; override;
    procedure SetExtractOptions( Value : TAbExtractOptions );
    procedure SetFileName(const aFileName : string); override;


  protected {properties}
    property ExtractOptions : TAbExtractOptions
      read  FExtractOptions
      write SetExtractOptions
      default AbDefExtractOptions;

  protected {events}
    property OnConfirmOverwrite : TAbConfirmOverwriteEvent
      read  FOnConfirmOverwrite
      write FOnConfirmOverwrite;

  public {methods}
    constructor Create( AOwner : TComponent ); override;
    destructor Destroy; override;

    procedure ExtractAt(Index : Integer; const NewName : string);
    procedure ExtractFiles(const FileMask : string);
    procedure ExtractFilesEx(const FileMask, ExclusionMask : string);
    procedure ExtractTaggedItems;
  end;

  TAbCabKit = class(TAbCustomCabKit)
  published
    property ArchiveProgressMeter;
    property BaseDirectory;
    property CabSize;
    property CompressionType;
    property CurrentCab;
    property ExtractOptions;
    property FolderCount;
    property FolderThreshold;
    property HasNext;
    property HasPrev;
    property ItemProgressMeter;
    property OnArchiveProgress;
    property OnArchiveItemProgress;
    property OnChange;
    property OnConfirmOverwrite;
    property OnConfirmProcessItem;
    property OnLoad;
    property OnProcessItemFailure;
    property OnRequestImage;
    property OnSave;
    property SetID;
    property SpanningThreshold;
    property TempDirectory;
    property Version;
    property FileName; {must be after OnLoad}
  end;


implementation

uses
  AbExcept;

{ TAbCustomCabKit ==================================================== }
constructor TAbCustomCabKit.Create(AOwner : TComponent);
begin
  inherited Create( AOwner );
  ExtractOptions := AbDefExtractOptions;
end;
{ -------------------------------------------------------------------------- }
destructor TAbCustomCabKit.Destroy;
begin
  inherited Destroy;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomCabKit.DoConfirmOverwrite(var Name : string;
                                             var Confirm : Boolean);
begin
  if Assigned(FOnConfirmOverwrite) then
    FOnConfirmOverwrite(Name, Confirm);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomCabKit.ExtractAt(Index : Integer; const NewName : string);
  {extract a file from the archive that match the index}
begin
  if Assigned( CabArchive ) then
    CabArchive.ExtractAt( Index, NewName )
  else
    raise EAbNoArchive.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomCabKit.ExtractFiles(const FileMask : string);
  {Extract files from the cabinet matching the filemask}
begin
  if Assigned(CabArchive) then
    CabArchive.ExtractFiles(FileMask)
  else
    raise EAbNoArchive.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomCabKit.ExtractFilesEx(const FileMask, ExclusionMask : string);
  {Extract files from the cabinet matching the FileMask, exluding those
   matching ExclusionMask}
begin
  if Assigned(CabArchive) then
    CabArchive.ExtractFilesEx(FileMask, ExclusionMask)
  else
    raise EAbNoArchive.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomCabKit.ExtractTaggedItems;
  {Extract items in the archive that have been tagged}
begin
  if Assigned(CabArchive) then
    CabArchive.ExtractTaggedItems
  else
    raise EAbNoArchive.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomCabKit.InitArchive;
begin
  inherited InitArchive;
  if Assigned( CabArchive ) then begin
    {poperties}
    CabArchive.ExtractOptions     := FExtractOptions;
    {events}
    CabArchive.OnConfirmOverwrite := DoConfirmOverwrite;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomCabKit.SetExtractOptions( Value : TAbExtractOptions );
begin
  FExtractOptions := Value;
  if Assigned( FArchive ) then
    FArchive.ExtractOptions := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomCabKit.SetFileName(const aFileName : string);
  {Create or open the specified cabinet file}
begin
  FFilename := aFileName;
  if csDesigning in ComponentState then
    Exit;
  if Assigned(FArchive) then begin
    FArchive.Free;
    FArchive := nil;
  end;
  if (aFileName <> '') then begin
    if (aFileName <> '') and FileExists(aFilename) then
      FArchive := TAbCabArchive.Create(aFileName, fmOpenRead)
    else
      FArchive := TAbCabArchive.Create(aFileName, fmOpenWrite);
    InitArchive;
    FArchive.Load;
  end;
  DoChange;
end;
{ -------------------------------------------------------------------------- }

end.
