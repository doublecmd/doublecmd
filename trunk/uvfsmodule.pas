{
   Double Commander - Virtual File System support
   - virtual class for manage Shared Object
 
   Copyright (C) 2006-2008  Koblov Alexander (Alexx2000@mail.ru)
 
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

unit uVFSmodule;

interface
uses
  uVFSTypes, uFileList;

{$mode objfpc}{$H+}
Type
  TVFSModule = class

  public
    function LoadModule(const sName:String):Boolean;virtual;abstract; {Load plugin}
    procedure UnloadModule;virtual;abstract;
    function VFSInit:Boolean;virtual;abstract;
    procedure VFSDestroy;virtual;abstract;
    function VFSCaps : TVFSCaps;virtual;abstract;

    function VFSConfigure(Parent: THandle):Boolean;virtual;abstract;
    function VFSOpen(const sName:String; bCanYouHandleThisFile : Boolean = False):Boolean;virtual;abstract;
    function VFSClose:Boolean;virtual;abstract;
    function VFSRefresh : Boolean;virtual;abstract;
    
    function VFSMkDir(const sDirName:String ):Boolean;virtual;abstract;
    function VFSRmDir(const sDirName:String):Boolean;virtual;abstract;
    
    function VFSCopyOut(var flSrcList : TFileList; sDstPath:String; Flags: Integer):Boolean;virtual;abstract;
    function VFSCopyIn(var flSrcList : TFileList; sDstName:String; Flags : Integer):Boolean;virtual;abstract;
    function VFSCopyOutEx(var flSrcList : TFileList; sDstPath:String; Flags: Integer):Boolean;virtual;abstract;
    function VFSCopyInEx(var flSrcList : TFileList; sDstName:String; Flags : Integer):Boolean;virtual;abstract;
    function VFSRename(const sSrcName, sDstName:String):Boolean;virtual;abstract;
    function VFSRun(const sName:String):Boolean;virtual;abstract;
    function VFSDelete(var flNameList:TFileList):Boolean;virtual;abstract;

    function VFSList(const sDir:String; var fl:TFileList):Boolean;virtual;abstract;

    function VFSMisc : Cardinal;virtual;abstract;
  end;

implementation


end.
