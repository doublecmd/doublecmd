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
{* ABBREVIA: ABUnzper.pas 3.04                           *}
{*********************************************************}
{* ABBREVIA: Non-visual Component with UnZip support     *}
{*********************************************************}

unit AbUnzper;

{$I AbDefine.inc}

interface

uses
  SysUtils, Classes,
  AbBrowse, AbZBrows,
  AbArcTyp, AbBase, AbUtils, AbZipTyp;

type
  TAbCustomUnZipper = class(TAbCustomZipBrowser)
  protected {private}
    FExtractOptions     : TAbExtractOptions;
    FOnConfirmOverwrite : TAbConfirmOverwriteEvent;
    FOnNeedPassword     : TAbNeedPasswordEvent;
    FPasswordRetries    : Byte;

  protected {methods}
    procedure DoConfirmOverwrite(var Name : string;
                                 var Confirm : Boolean);
      virtual;
    procedure DoNeedPassword(Sender : TObject;
                             var NewPassword : string);
      virtual;
    procedure InitArchive; override;
    procedure SetExtractOptions(Value : TAbExtractOptions);
    procedure SetPasswordRetries(Value : Byte);
    procedure UnzipProc(Sender : TObject; Item : TAbArchiveItem;
                        const NewName : string );
    procedure UnzipToStreamProc(Sender : TObject; Item : TAbArchiveItem;
                                OutStream : TStream);
    procedure TestItemProc(Sender : TObject; Item : TAbArchiveItem);

  protected {properties}
    property ExtractOptions : TAbExtractOptions
      read  FExtractOptions
      write SetExtractOptions
      default AbDefExtractOptions;
    property OnConfirmOverwrite : TAbConfirmOverwriteEvent
      read  FOnConfirmOverwrite
      write FOnConfirmOverwrite;
    property OnNeedPassword : TAbNeedPasswordEvent
      read  FOnNeedPassword
      write FOnNeedPassword;
    property PasswordRetries : Byte
      read  FPasswordRetries
      write SetPasswordRetries
      default AbDefPasswordRetries;

  public {methods}
    constructor Create( AOwner : TComponent );
      override;
    destructor Destroy;
      override;
    procedure ExtractAt(Index : Integer; const NewName : string);
    procedure ExtractFiles(const FileMask : string);
    procedure ExtractFilesEx(const FileMask, ExclusionMask : string);
    procedure ExtractToStream(const aFileName : string; ToStream : TStream);
    procedure ExtractTaggedItems;
    procedure TestTaggedItems;
  end;

  TAbUnZipper = class(TAbCustomUnZipper)
  published
    property ArchiveProgressMeter;
    property ItemProgressMeter;
    property BaseDirectory;
    property ExtractOptions;
    property LogFile;
    property Logging;
    property OnArchiveProgress;
    property OnArchiveItemProgress;
    property OnChange;
    property OnConfirmOverwrite;
    property OnConfirmProcessItem;
    property OnLoad;
    property OnNeedPassword;
    property OnRequestImage;
    property OnProcessItemFailure;
    property OnRequestLastDisk;
    property OnRequestNthDisk;
    property Password;
    property PasswordRetries;
    property TempDirectory;
    property Version;
    property FileName; {must be after OnLoad}                          
  end;


implementation

uses
  AbExcept,
  AbUnzPrc;

{ -------------------------------------------------------------------------- }
constructor TAbCustomUnZipper.Create( AOwner : TComponent );
begin
  inherited Create(AOwner);
  ExtractOptions := AbDefExtractOptions;
  PasswordRetries := AbDefPasswordRetries;
end;
{ -------------------------------------------------------------------------- }
destructor TAbCustomUnZipper.Destroy;
begin
  inherited Destroy;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomUnZipper.DoConfirmOverwrite(var Name : string;
                                               var Confirm : Boolean);
begin
  Confirm := True;
  if Assigned(FOnConfirmOverwrite) then
    FOnConfirmOverwrite( Name, Confirm );
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomUnZipper.DoNeedPassword(Sender : TObject;
                                           var NewPassword : string);
begin
  if Assigned(FOnNeedPassword) then begin
    FOnNeedPassword(Self, NewPassword);
    Password := NewPassword;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomUnZipper.ExtractAt(Index : Integer; const NewName : string);
  {extract a file from the archive that match the index}
begin
  if (ZipArchive <> nil) then
    ZipArchive.ExtractAt(Index, NewName)
  else
    raise EAbNoArchive.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomUnZipper.ExtractFiles(const FileMask : string);
  {extract all files from the archive that match the mask}
begin
  if (ZipArchive <> nil) then
    ZipArchive.ExtractFiles( FileMask )
  else
    raise EAbNoArchive.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomUnZipper.ExtractFilesEx(const FileMask, ExclusionMask : string);
  {extract files matching FileMask except those matching ExclusionMask}
begin
  if (ZipArchive <> nil) then
    ZipArchive.ExtractFilesEx(FileMask, ExclusionMask)
  else
    raise EAbNoArchive.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomUnZipper.ExtractToStream(const aFileName : string;
                                            ToStream : TStream);
begin
  if (ZipArchive <> nil) then
    ZipArchive.ExtractToStream(aFileName, ToStream)
  else
    raise EAbNoArchive.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomUnZipper.ExtractTaggedItems;
  {extract all tagged items from the archive}
begin
  if (ZipArchive <> nil) then
    ZipArchive.ExtractTaggedItems
  else
    raise EAbNoArchive.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomUnZipper.InitArchive;
begin
  inherited InitArchive;
  if (ZipArchive <> nil) then begin
    {properties}
    ZipArchive.ExtractOptions                       := FExtractOptions;
    TAbZipArchive(ZipArchive).PasswordRetries       := FPasswordRetries;
    {events}
    ZipArchive.OnConfirmOverwrite                   := DoConfirmOverwrite;
    TAbZipArchive(ZipArchive).OnNeedPassword        := DoNeedPassword;
    TAbZipArchive(ZipArchive).TestHelper            := TestItemProc;
    TAbZipArchive(ZipArchive).ExtractHelper         := UnzipProc;
    TAbZipArchive(ZipArchive).ExtractToStreamHelper := UnzipToStreamProc;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomUnZipper.SetExtractOptions(Value : TAbExtractOptions);
begin
  FExtractOptions := Value;
  if (ZipArchive <> nil) then
    ZipArchive.ExtractOptions := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomUnZipper.SetPasswordRetries(Value : Byte);
begin
  FPasswordRetries := Value;
  if (ZipArchive <> nil) then
    TAbZipArchive(ZipArchive).PasswordRetries := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomUnZipper.TestTaggedItems;
  {Test specified items}
begin
  if (ZipArchive <> nil) then
    ZipArchive.TestTaggedItems
  else
    raise EAbNoArchive.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomUnZipper.UnzipProc(Sender : TObject;
                                      Item : TAbArchiveItem;
                                      const NewName : string);
begin
  AbUnzip( TAbZipArchive(Sender), TAbZipItem(Item), NewName);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomUnZipper.UnzipToStreamProc(Sender : TObject;
                                              Item : TAbArchiveItem;
                                              OutStream : TStream);
begin
  AbUnzipToStream(TAbZipArchive(Sender), TAbZipItem(Item), OutStream);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomUnZipper.TestItemProc(Sender : TObject;
                                         Item : TAbArchiveItem);
begin
  AbTestZipItem(TAbZipArchive(Sender), TAbZipItem(Item));
end;
{ -------------------------------------------------------------------------- }

end.

