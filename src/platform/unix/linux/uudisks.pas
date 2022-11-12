{
   Double Commander
   -------------------------------------------------------------------------
   UDisks types unit

   Copyright (C) 2010-2012  Przemyslaw Nagay (cobines@gmail.com)

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

unit uUDisks;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type
  TUDisksDeviceInfo = record
    DeviceObjectPath: String;
    DeviceFile: String;
    DeviceIsDrive,
    DeviceIsSystemInternal,
    DeviceIsPartition,
    DeviceIsPartitionTable, // Does the device have a partition table
    DeviceIsMounted,
    DeviceIsRemovable,   // If contains removable media.
    DeviceIsOpticalDisc, // If is an optical drive and optical disk is inserted.
    DeviceIsMediaAvailable,
    DriveIsMediaEjectable: Boolean;
    DeviceMountPaths: TStringArray;
    DevicePresentationHide: Boolean;
    DevicePresentationName: String;
    DevicePresentationIconName: String;
    DeviceAutomountHint: String;  // Whether automatically mount or not
    DriveConnectionInterface,
    DriveMedia: String; // Type of media currently in the drive.
    DriveMediaCompatibility: TStringArray; // Possible media types.
    IdUsage,
    IdType,
    IdVersion,
    IdUuid,
    IdLabel,
    DeviceSize,
    PartitionSlave: String; // Owner device if this is a partition
  end;

  TUDisksDevicesInfos = array of TUDisksDeviceInfo;

  TUDisksMethod = (UDisks_DeviceAdded,
                   UDisks_DeviceRemoved,
                   UDisks_DeviceChanged);

  TUDisksDeviceNotify = procedure(Reason: TUDisksMethod; const ObjectPath: String) of object;

  TUDisksObserverList = specialize TFPGList<TUDisksDeviceNotify>;

implementation

end.

