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
 *   Craig Peterson <capeterson@users.sourceforge.net>
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* ABBREVIA: AbZBrows.pas                                *}
{*********************************************************}
{* ABBREVIA: Zip file Browser Component                  *}
{*********************************************************}

unit AbZBrows;

{$I AbDefine.inc}

interface

uses
  Classes,
  AbArcTyp, AbBrowse, AbSpanSt, AbZipTyp;

type
  TAbCustomZipBrowser = class(TAbBaseBrowser)
  private
    function GetTarAutoHandle: Boolean;
    procedure SetTarAutoHandle(const Value: Boolean);
  protected {private}
    FPassword          : AnsiString;
    FOnRequestLastDisk : TAbRequestDiskEvent;
    FOnRequestNthDisk  : TAbRequestNthDiskEvent;
    FOnRequestBlankDisk     : TAbRequestDiskEvent;
    FTarAutoHandle     : Boolean;

  protected {methods}
    function  GetItem(Index : Integer) : TAbZipItem; virtual;
    function  GetStream: TStream;
    function  GetZipfileComment : AnsiString;
    procedure InitArchive;
      override;
    procedure SetFileName(const aFileName : string);
      override;
    procedure SetStream(aValue: TStream);
    procedure SetOnRequestLastDisk(Value : TAbRequestDiskEvent);
    procedure SetOnRequestNthDisk(Value : TAbRequestNthDiskEvent);
    procedure SetOnRequestBlankDisk(Value : TAbRequestDiskEvent);

    procedure SetPassword(const Value : AnsiString);
    procedure SetZipfileComment(const Value : AnsiString);
      virtual;

  protected {properties}
    property Password : AnsiString
      read  FPassword
      write SetPassword;

  protected {events}
    property OnRequestLastDisk : TAbRequestDiskEvent
      read  FOnRequestLastDisk
      write SetOnRequestLastDisk;
    property OnRequestNthDisk : TAbRequestNthDiskEvent
      read  FOnRequestNthDisk
      write SetOnRequestNthDisk;
    property OnRequestBlankDisk : TAbRequestDiskEvent
      read  FOnRequestBlankDisk
      write SetOnRequestBlankDisk;

  public {methods}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

  public {properties}
    property Items[Index : Integer] : TAbZipItem
      read  GetItem; default;
    property Stream : TStream // This can be used instead of Filename
      read GetStream write SetStream;
    property ZipArchive : {TAbZipArchive} TAbArchive
      read FArchive;
    property ZipfileComment : AnsiString
      read GetZipfileComment
      write SetZipfileComment;

    property TarAutoHandle : Boolean
      read GetTarAutoHandle
      write SetTarAutoHandle;
  end;

  TAbZipBrowser = class(TAbCustomZipBrowser)
  published
    property ArchiveProgressMeter;
    property ItemProgressMeter;
    property BaseDirectory;
    property LogFile;
    property Logging;
    property OnArchiveProgress;
    property OnArchiveItemProgress;
    property OnChange;
    property OnConfirmProcessItem;
    property OnLoad;
    property OnProcessItemFailure;
    property OnRequestLastDisk;
    property OnRequestNthDisk;
    property Version;
    property TarAutoHandle;
    property FileName;  {must be after OnLoad}
  end;


implementation

uses
  SysUtils, AbBzip2Typ, AbExcept, AbGzTyp, AbTarTyp, AbUtils, DCOSUtils;

{ TAbCustomZipBrowser implementation ======================================= }

{ -------------------------------------------------------------------------- }
constructor TAbCustomZipBrowser.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
end;
{ -------------------------------------------------------------------------- }
destructor TAbCustomZipBrowser.Destroy;
begin
  inherited Destroy;
end;
{ -------------------------------------------------------------------------- }
function TAbCustomZipBrowser.GetItem(Index : Integer) : TAbZipItem;
begin
  Result := TAbZipItem(ZipArchive.ItemList[Index]);
end;
{ -------------------------------------------------------------------------- }
function TAbCustomZipBrowser.GetStream: TStream;
begin
  if FArchive <> nil then
    Result := FArchive.FStream
  else
    Result := nil
end;
{ -------------------------------------------------------------------------- }
function TAbCustomZipBrowser.GetTarAutoHandle: Boolean;
begin
  Result := False;
  if FArchive is TAbGzipArchive then
    Result := TAbGzipArchive(FArchive).TarAutoHandle
  else if FArchive is TAbBzip2Archive then
    Result := TAbBzip2Archive(FArchive).TarAutoHandle;
end;
{ -------------------------------------------------------------------------- }
function TAbCustomZipBrowser.GetZipfileComment : AnsiString;
begin
  if ZipArchive is TAbZipArchive then
    Result := TAbZipArchive(ZipArchive).ZipfileComment
  else
    Result := '';
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipBrowser.InitArchive;
begin
  inherited InitArchive;
  if ZipArchive is TAbZipArchive then begin
    {properties}
    TAbZipArchive(ZipArchive).Password          := FPassword;
    {events}
    TAbZipArchive(ZipArchive).OnRequestLastDisk := FOnRequestLastDisk;
    TAbZipArchive(ZipArchive).OnRequestNthDisk  := FOnRequestNthDisk;
  end;
end;
{ -------------------------------------------------------------------------- }

procedure TAbCustomZipBrowser.SetFileName(const aFileName : string);
var
  ArcType : TAbArchiveType;
begin

  FFileName := aFileName;
  if csDesigning in ComponentState then
    Exit;
  try
    if Assigned(FArchive) then begin
      FArchive.Save;
    end;
  except
  end;
  FArchive.Free;
  FArchive := nil;

  if FileName <> '' then begin
    if mbFileExists(FileName) then begin { open it }
      ArcType := ArchiveType;
      if not ForceType then
         ArcType := AbDetermineArcType(FileName, atUnknown);

      case ArcType of
        atZip, atSpannedZip, atSelfExtZip : begin
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

        atBzip2 : begin
          FArchive := TAbBzip2Archive.Create(FileName, fmOpenRead or fmShareDenyNone);
          TAbBzip2Archive(FArchive).TarAutoHandle := FTarAutoHandle;
          TAbBzip2Archive(FArchive).IsBzippedTar := False;
          inherited InitArchive;
        end;

        atBzippedTar : begin
          FArchive := TAbBzip2Archive.Create(FileName, fmOpenRead or fmShareDenyNone);
          TAbBzip2Archive(FArchive).TarAutoHandle := FTarAutoHandle;
          TAbBzip2Archive(FArchive).IsBzippedTar := True;
          inherited InitArchive;
        end;

        else
          raise EAbUnhandledType.Create;
      end {case};
      FArchive.Load;
      FArchiveType := ArcType;
    end;
  end;
  DoChange;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipBrowser.SetStream(aValue: TStream);
var
  ArcType : TAbArchiveType;
begin
  FFileName := '';
  try
    if FArchive <> nil then
      FArchive.Save;
  except
  end;
  FreeAndNil(FArchive);

  if aValue <> nil then begin
    ArcType := ArchiveType;
    if not ForceType then
      ArcType := AbDetermineArcType(aValue);

    case ArcType of
      atZip, atSpannedZip, atSelfExtZip : begin
        FArchive := TAbZipArchive.CreateFromStream(aValue, '');
      end;

      atTar : begin
        FArchive := TAbTarArchive.CreateFromStream(aValue, '');
      end;

      atGZip, atGZippedTar : begin
        FArchive := TAbGzipArchive.CreateFromStream(aValue, '');
        TAbGzipArchive(FArchive).TarAutoHandle := FTarAutoHandle;
        TAbGzipArchive(FArchive).IsGzippedTar := (ArcType = atGZippedTar);
      end;

      atBzip2, atBzippedTar : begin
        FArchive := TAbBzip2Archive.CreateFromStream(aValue, '');
        TAbBzip2Archive(FArchive).TarAutoHandle := FTarAutoHandle;
        TAbBzip2Archive(FArchive).IsBzippedTar := (ArcType = atBzippedTar);
      end;

      else
        raise EAbUnhandledType.Create;
    end {case};
    InitArchive;
    FArchive.Load;
    FArchiveType := ArcType;
  end;
  DoChange;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipBrowser.SetOnRequestBlankDisk(Value : TAbRequestDiskEvent);
begin
  FOnRequestBlankDisk := Value;
  if ZipArchive is TAbZipArchive then
    TAbZipArchive(ZipArchive).OnRequestBlankDisk := FOnRequestBlankDisk;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipBrowser.SetOnRequestLastDisk(Value : TAbRequestDiskEvent);
begin
  FOnRequestLastDisk := Value;
  if ZipArchive is TAbZipArchive then
    TAbZipArchive(ZipArchive).OnRequestLastDisk := FOnRequestLastDisk;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipBrowser.SetOnRequestNthDisk(Value : TAbRequestNthDiskEvent);
begin
  FOnRequestNthDisk := Value;
  if ZipArchive is TAbZipArchive then
    TAbZipArchive(ZipArchive).OnRequestNthDisk := FOnRequestNthDisk;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipBrowser.SetPassword(const Value : AnsiString);
begin
  FPassword := Value;
  if ZipArchive is TAbZipArchive then
    TAbZipArchive(ZipArchive).Password := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipBrowser.SetTarAutoHandle(const Value: Boolean);
begin
  FTarAutoHandle := Value;

  if FArchive is TAbGzipArchive then begin
    if TAbGzipArchive(FArchive).TarAutoHandle <> Value then begin
      TAbGzipArchive(FArchive).TarAutoHandle := Value;
      InitArchive;
      FArchive.Load;
      DoChange;
    end;
  end;
  if FArchive is TAbBzip2Archive then begin
    if TAbBzip2Archive(FArchive).TarAutoHandle <> Value then begin
      TAbBzip2Archive(FArchive).TarAutoHandle := Value;
      InitArchive;
      FArchive.Load;
      DoChange;
    end;
  end;
end;

procedure TAbCustomZipBrowser.SetZipfileComment(const Value : AnsiString);
begin
  {NOP - descendents wishing to set this property should override}
end;
{ -------------------------------------------------------------------------- }

end.
