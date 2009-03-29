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
{* ABBREVIA: AbCBrows.pas 3.05                           *}
{*********************************************************}
{* ABBREVIA: Cabinet file browser component              *}
{*********************************************************}

unit AbCBrows;

{$I AbDefine.inc}

interface

uses
  SysUtils, Classes,
  AbBrowse,
  AbCabTyp;

type
  TAbCustomCabBrowser = class(TAbBaseBrowser)
  protected {private}
    FSetID : Word;

    function  GetCabArchive : TAbCabArchive;
    function  GetCabSize : Longint;
    function  GetCurrentCab : Word;
    function  GetFolderCount : Word;
    function  GetItem(Index : Integer) : TAbCabItem; virtual;
    function  GetHasNext : Boolean;
    function  GetHasPrev : Boolean;
    function  GetSetID : Word;
    procedure InitArchive;
      override;
    procedure SetFileName(const aFileName : string); override;
    procedure SetSetID(Value : Word);

  protected {properties}
    property CabSize : Longint
      read GetCabSize;
    property CurrentCab : Word
      read GetCurrentCab;
    property FolderCount : Word
      read GetFolderCount;
    property HasNext : Boolean
      read GetHasNext;
    property HasPrev : Boolean
      read GetHasPrev;
    property SetID : Word
      read GetSetID
      write SetSetID;

  public {methods}
    constructor Create(AOwner : TComponent);
      override;
    destructor Destroy;
      override;

  public {properties}
    property CabArchive : TAbCabArchive
      read  GetCabArchive;
    property Items[Index : Integer] : TAbCabItem
      read  GetItem; default;
  end;


type
  TAbCabBrowser = class(TAbCustomCabBrowser)
  published
    property ArchiveProgressMeter;
    property BaseDirectory;
    property CabSize;
    property CurrentCab;
    property FolderCount;
    property HasNext;
    property HasPrev;
    property ItemProgressMeter;
    property LogFile;
    property Logging;
    property OnArchiveProgress;
    property OnArchiveItemProgress;
    property OnChange;
    property OnLoad;
    property SetID;
    property TempDirectory;
    property Version;
    property FileName;  {must be after OnLoad}
  end;
  {.Z+}


implementation

uses
  AbArcTyp,
  AbUtils;

{ TAbCustomCabBrowser ====================================================== }
constructor TAbCustomCabBrowser.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FArchiveType := atCab;
end;
{ -------------------------------------------------------------------------- }
destructor TAbCustomCabBrowser.Destroy;
begin
  inherited Destroy;
end;
{ -------------------------------------------------------------------------- }
function TAbCustomCabBrowser.GetCabArchive : TAbCabArchive;
begin
  if Assigned(Archive) then
    Result := TAbCabArchive(Archive)
  else
    Result := nil;
end;
{ -------------------------------------------------------------------------- }
function TAbCustomCabBrowser.GetCabSize : Longint;
begin
  if Assigned(Archive) then
    Result := TAbCabArchive(Archive).CabSize
  else
    Result := 0;
end;
{ -------------------------------------------------------------------------- }
function TAbCustomCabBrowser.GetCurrentCab : Word;
begin
  if Assigned(Archive) then
    Result := TAbCabArchive(Archive).CurrentCab
  else
    Result := 0;
end;
{ -------------------------------------------------------------------------- }
function TAbCustomCabBrowser.GetFolderCount : Word;
begin
  if Assigned(Archive) then
    Result := TAbCabArchive(Archive).FolderCount
  else
    Result := 0;
end;
{ -------------------------------------------------------------------------- }
function TAbCustomCabBrowser.GetHasNext : Boolean;
begin
  if Assigned(Archive) then
    Result := TAbCabArchive(Archive).HasNext
  else
    Result := False;
end;
{ -------------------------------------------------------------------------- }
function TAbCustomCabBrowser.GetHasPrev : Boolean;
begin
  if Assigned(Archive) then
    Result := TAbCabArchive(Archive).HasPrev
  else
    Result := False;
end;
{ -------------------------------------------------------------------------- }
function TAbCustomCabBrowser.GetItem(Index : Integer) : TAbCabItem;
  {return cabinet item}
begin
  if Assigned(CabArchive) then
    Result := CabArchive.Items[Index]
  else
    Result := nil;
end;
{ -------------------------------------------------------------------------- }
function TAbCustomCabBrowser.GetSetID : Word;
begin
  if Assigned(Archive) then
    Result := TAbCabArchive(Archive).SetID
  else
    Result := 0;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomCabBrowser.InitArchive;
begin
  inherited InitArchive;
  if Assigned(Archive) then
    TAbCabArchive(Archive).SetID := FSetID;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomCabBrowser.SetFileName(const aFileName : string);
  {open/create cabinet archive}
begin
  FFileName := aFileName;
  if (csDesigning in ComponentState) then
    Exit;
  if Assigned(FArchive) then begin
    FArchive.Free;
    FArchive := nil;
  end;
  if (aFileName <> '') and
      FileExists(aFilename) and
     (AbDetermineArcType(aFileName, atCab) = atCab) then
  begin
    FArchive := TAbCabArchive.Create(aFileName, fmOpenRead);
    InitArchive;
    FArchive.Load;
  end;
  DoChange;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomCabBrowser.SetSetID(Value : Word);
begin
  FSetID := Value;
  if Assigned(Archive) then
    TAbCabArchive(Archive).SetID := Value;
end;
{ -------------------------------------------------------------------------- }

end.
