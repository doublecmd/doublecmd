{/***************************************************************************
 * CVSID: $Id$
 *
 * libhal.h : HAL daemon C convenience library headers
 *
 * Copyright (C) 2003 David Zeuthen, <david@fubar.dk>
 * Copyright (C) 2007 Codethink Ltd. Author Rob Taylor <rob.taylor@codethink.co.uk>
 *
 * Licensed under the Academic Free License version 2.1
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 *
 **************************************************************************/}

unit libhal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dbus;

type

{/**
 * LibHalPropertyType:
 *
 * Possible types for properties on hal device objects
 */}
  LibHalPropertyType =
  (
    LIBHAL_PROPERTY_TYPE_INVALID = DBUS_TYPE_INVALID, //** Used to report error condition */
    LIBHAL_PROPERTY_TYPE_INT32   = DBUS_TYPE_INT32,//** Type for 32-bit signed integer property */
    LIBHAL_PROPERTY_TYPE_UINT64  = DBUS_TYPE_UINT64,//** Type for 64-bit unsigned integer property */
    LIBHAL_PROPERTY_TYPE_DOUBLE  = DBUS_TYPE_DOUBLE,//** Type for double precision floating point property */
    LIBHAL_PROPERTY_TYPE_BOOLEAN = DBUS_TYPE_BOOLEAN,//** Type for boolean property */
    LIBHAL_PROPERTY_TYPE_STRING  = DBUS_TYPE_STRING//** Type for UTF-8 string property */
//С…Р·    LIBHAL_PROPERTY_TYPE_STRLIST = ((int) (DBUS_TYPE_STRING<<8)+('l'))//** Type for list of UTF-8 strings property */
  );


  LibHalContext = record end;
  PLibHalContext = ^LibHalContext;

  LibHalProperty = record end;
  PLibHalProperty = ^LibHalProperty;


  LibHalPropertySet = record end;
  PLibHalPropertySet = ^LibHalPropertySet;

{
 * LibHalDeviceAdded:
 * @ctx: context for connection to hald
 * @udi: the Unique Device Id
 *
 * Type for callback when a device is added.
 }
LibHalDeviceAdded = procedure(ctx: PLibHalContext; const udi: PChar);

{
 * LibHalDeviceRemoved:
 * @ctx: context for connection to hald
 * @udi: the Unique Device Id
 *
 * Type for callback when a device is removed.
 }
LibHalDeviceRemoved = procedure(ctx: PLibHalContext; const udi: PChar);

//* Create a new context for a connection with hald */
function libhal_ctx_new:PLibHalContext; cdecl; external 'libhal';
//* Set DBus connection to use to talk to hald. */
function libhal_ctx_set_dbus_connection(ctx: PLibHalContext; conn: PDBusConnection):dbus_bool_t; cdecl; external 'libhal';
//* Initialize the connection to hald */
function libhal_ctx_init(ctx : PLibHalContext;error: PDBusError):dbus_bool_t; cdecl; external 'libhal';
//* Get all devices in the Global Device List (GDL). */
function libhal_get_all_devices(ctx: PLibHalContext;
                        num_devices: PInteger;
                              error: PDBusError):PPChar; cdecl; external 'libhal';
//* Frees a nul-terminated string */
procedure libhal_free_string(str :PChar); cdecl; external 'libhal';
//* Frees a NULL-terminated array of strings. If passed NULL, does nothing. */
procedure libhal_free_string_array(str_array: PPChar); cdecl; external 'libhal';
//* Free a LibHalContext resource */
function libhal_ctx_free(ctx: PLibHalContext):dbus_bool_t; cdecl; external 'libhal';
//* Determine if a property on a device exists. */
function libhal_device_property_exists (ctx : PLibHalContext;
				   const udi: PChar;
				   const key: PChar;
				   error: PDBusError):dbus_bool_t; cdecl; external 'libhal';
//* Get the value of a property of type string. */
function libhal_device_get_property_string (ctx: PLibHalContext;
				       const udi: PChar;
				       const key: PChar;
                                           error: PDBusError):PChar; cdecl; external 'libhal';
//* Get the value of a property of type string list. */
function libhal_device_get_property_strlist (ctx: PLibHalContext;
				       const udi: PChar;
				       const key: PChar;
                                           error: PDBusError): PPChar; cdecl; external 'libhal';
//* Retrieve all the properties on a device. */
function libhal_device_get_all_properties (ctx : PLibHalContext;
				     const udi : Pchar;
                                         error : PDBusError):PLibHalPropertySet; cdecl; external 'libhal';
//* Get type of property. */
function libhal_ps_get_type (const set_ : PLibHalPropertySet;
                             const key_ : PChar):LibHalPropertyType; cdecl; external 'libhal';
//* Get the value of a property of type string. */
function libhal_ps_get_string  (const set_ :PLibHalPropertySet;
                                const key_ :PChar):Pchar; cdecl; external 'libhal';
//* Get the number of properties in a property set. */
function libhal_property_set_get_num_elems (set_ : PLibHalPropertySet):integer; cdecl; external 'libhal';
//* Set the callback for when a device is added */
function libhal_ctx_set_device_added(ctx: PLibHalContext;
                                callback: LibHalDeviceAdded):dbus_bool_t; cdecl; external 'libhal';
//* Set the callback for when a device is removed */
function libhal_ctx_set_device_removed(ctx: PLibHalContext;
                                  callback: LibHalDeviceRemoved):dbus_bool_t; cdecl; external 'libhal';

implementation

end.

