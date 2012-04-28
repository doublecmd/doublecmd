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
{* ABBREVIA: ZipApp.pas                                 *}
{*********************************************************}
{* ABBREVIA: Additional classes and routines                *}
{*********************************************************}

unit ZipApp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AbArcTyp, AbZipKit, AbUtils;

type
  { TAbArchiveItemHelper }

  TAbArchiveItemHelper = class helper for TAbArchiveItem
    function MatchesPath(const Path : String; Recursive : Boolean = False) : Boolean;
    function MatchesPathEx(const Paths : String; Recursive : Boolean = False) : Boolean;
  end;

  { TAbArchiveAccess }

  TAbArchiveAccess = class(TAbArchive)

  end;

  { TAbZipKit }

  TAbZipKit = class(TAbCustomZipKit)
  public
    procedure AddEntry(const Path : String; const ArchiveDirectory : String);
    {en
        Delete directory entry and all file and directory entries matching
        the same path recursively
    }
    procedure DeleteDirectoriesRecursively(const Paths : String);
    {en
       Test specific item in the archive
    }
    procedure TestItemAt(Index : Integer);
  end;

{en
  See if DirPath matches PathToMatch.
  If Recursive=True it is allowed for DirPath to point to a subdirectory of PathToMatch,
  for example: PathToMatch = 'dir/', DirPath = 'dir/subdir' - Result is True.
}
function AbDirMatch(DirPath : String; PathToMatch : String; Recursive : Boolean) : Boolean;
{en
   From a list of paths separated with AbPathSep (';') extracts a path from
   the position StartPos (counted from 1) and modifies StartPos to point to next entry.
   When no more entries are found, returns empty string.
}
function AbExtractEntry(const Entries : String; var StartPos : Integer) : String;

implementation

uses
  AbExcept;

{ TAbArchiveItemHelper }

function TAbArchiveItemHelper.MatchesPath(const Path: String; Recursive: Boolean): Boolean;
var
  Value : string;
  Drive, Dir, Name : string;
begin
  Value := Path;
  if (Value <> '') and (RightStr(Value, 1) <> AbPathDelim) then
    Value := Value + AbPathDelim;
  AbUnfixName(Value);
  AbParseFileName(Path, Drive, Dir, Name);
  Value := Dir + Name;
  Name := FileName;
  AbUnfixName(Name);
  Result := AbDirMatch(Name, Value, Recursive);
end;

function TAbArchiveItemHelper.MatchesPathEx(const Paths: String; Recursive: Boolean): Boolean;
var
  Position: Integer;
  Path: String;
begin
  Result := True;

  Position := 1;
  while True do
  begin
    Path := AbExtractEntry(Paths, Position);
    if Path = '' then Break;
    if MatchesPath(Path, Recursive) then Exit;
  end;

  Result := False;
end;

{ TAbZipKit }

procedure TAbZipKit.AddEntry(const Path: String; const ArchiveDirectory: String);
var
  Item : TAbArchiveItem;
begin
  with TAbArchiveAccess(Archive) do
  begin
    Item := CreateItem(Path);
    Add(Item);
  end;
end;

procedure TAbZipKit.DeleteDirectoriesRecursively(const Paths: String);
var
  I : Integer;
begin
  TAbArchiveAccess(Archive).CheckValid;
  if Count > 0 then
  begin
    for I := Pred(Count) downto 0 do
    begin
      with Archive.ItemList[I] do
        if MatchesPathEx(Paths, True) then
          DeleteAt(I);
    end;
  end;
end;

procedure TAbZipKit.TestItemAt(Index: Integer);
begin
  if (Archive <> nil) then
    TAbArchiveAccess(Archive).TestItemAt(Index)
  else
    raise EAbNoArchive.Create;
end;

function AbDirMatch(DirPath : String; PathToMatch : String; Recursive : Boolean) : Boolean;
begin
  if Recursive then
    PathToMatch := PathToMatch + '*'; // append wildcard

  Result := AbPatternMatch(DirPath, 1, PathToMatch, 1);
end;

function AbExtractEntry(const Entries : String; var StartPos : Integer) : String;
var
  I  : Integer;
  Len: Integer;
begin
  Result := '';
  Len := Length(Entries);
  I := StartPos;
  if (I >= 1) and (I <= Len) then
  begin
    while (I <= Len) and (Entries[I] <> AbPathSep) do Inc(I);
    Result := Copy(Entries, StartPos, I - StartPos);
    if (I <= Len) and (Entries[I] = AbPathSep) then Inc(I);

    StartPos := I;
  end;
end;

end.

