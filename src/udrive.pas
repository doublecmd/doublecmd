{
   Double Commander
   -------------------------------------------------------------------------
   Structures describing drives.

   Copyright (C) 2006-2010  Koblov Alexander (Alexx2000@mail.ru)
   Copyright (C) 2010  Przemyslaw Nagay (cobines@gmail.com)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit uDrive;

{$mode objfpc}{$H+}

interface

uses
  Classes;

type
  TDriveType = (dtUnknown,
                dtFlash,      // Flash drive
                dtFloppy,     // 3.5'', ZIP drive, etc.
                dtHardDisk,   // Hard disk drive
                dtNetwork,    // Network share
                dtOptical,    // CD, DVD, Blu-Ray, etc.
                dtRamDisk,    // Ram-disk
                dtRemovable); // Drive with removable media

  { TDrive }

  // On Linux we also put here mount points other than drives.

  TDrive = record
    DisplayName,            //<en Name displayed to the user.
    Path,                   //<en Where this drive is or should be mounted (by /etc/fstab).
    DriveLabel,             //<en Drive label if filesystem on the drive supports it.
    DeviceId: String;       //<en Device ID that can be used for mounting, ejecting, etc.
    DriveType : TDriveType;
    FileSystem: String;     //<en Filesystem on the drive
    IsMediaAvailable: Boolean; //<en Is media available in a drive with removable media.
    IsMediaEjectable: Boolean; //<en Can eject media by a command.
    IsMediaRemovable: Boolean; //<en If the drive has removable media.
    IsMounted: Boolean;        //<en Is the drive mounted.
  end;
  PDrive = ^TDrive;

  { TDrivesList }

  TDrivesList = class
  private
    FList: TFPList;
  protected
    function Get(Index: Integer): PDrive;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(ADrive: PDrive): Integer;
    procedure Remove(Index: Integer);
    procedure RemoveAll;
    procedure Sort(Compare: TListSortCompare);
    property Items[Index: Integer]: PDrive read Get; default;
    property Count: Integer read GetCount;
  end;

implementation

uses
  SysUtils;

{ TDrivesList }

constructor TDrivesList.Create;
begin
  FList := TFPList.Create;
end;

destructor TDrivesList.Destroy;
begin
  inherited Destroy;
  RemoveAll;
  FList.Free;
end;

function TDrivesList.Add(ADrive: PDrive): Integer;
begin
  Result := FList.Add(ADrive);
end;

procedure TDrivesList.Remove(Index: Integer);
begin
  if (Index >= 0) and (Index < FList.Count) then
  begin
    Dispose(PDrive(FList[Index]));
    FList.Delete(Index);
  end
  else
    raise ERangeError.Create('Invalid index');
end;

procedure TDrivesList.RemoveAll;
begin
  while FList.Count > 0 do
    Remove(0);
end;

procedure TDrivesList.Sort(Compare: TListSortCompare);
begin
  FList.Sort(Compare);
end;

function TDrivesList.Get(Index: Integer): PDrive;
begin
  if (Index >= 0) and (Index < FList.Count) then
  begin
    Result := PDrive(FList.Items[Index]);
  end
  else
    raise ERangeError.Create('Invalid index');
end;

function TDrivesList.GetCount: Integer;
begin
  Result := FList.Count;
end;

end.

