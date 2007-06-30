{
   Double Commander
   -------------------------------------------------------------------------
   Implementation of Virtual File System

   Copyright (C) 2006-2007  Koblov Alexander (Alexx2000@mail.ru)

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

unit uVFS;


interface
uses
  Classes, uGlobs, uFileList, uVFSutil, uTypes, uVFSmodule, uWCXmodule, uWFXmodule;
type

  { TVFS }

  TVFS = class
  protected
    FPlugins : TStringList;
    FCurrentPlugin : String;
    sLastArchive:String;
    FVFSModule : TVFSmodule;
  public
    constructor Create;
    destructor Destroy; override;
    function ChangeDirLevel(frp:PFileRecItem; var flist: TFileList; cdUpLevel : Boolean) : Boolean;
    function FindModule(const sFileName:String):Boolean;
    property VFSmodule : TVFSmodule read FVFSModule;
    property ArcFullName : String read sLastArchive;
    property Plugins : TStringList read FPlugins;
  end; //class TVFS

implementation

uses
  SysUtils, uGlobsPaths, uFindEx, uOSUtils, LCLProc;

{ TVFS }

constructor TVFS.Create;
begin
  FPlugins := TStringList.Create;
  gIni.ReadSectionRaw('PackerPlugins', FPlugins);
  sLastArchive:='';  // nothing
end;

destructor TVFS.Destroy;
begin
  if Assigned(FVFSModule) then
     FVFSModule.Destroy;
  FVFSModule := nil;
  FreeAndNil(FPlugins);
  inherited
end;



function TVFS.ChangeDirLevel(frp:PFileRecItem; var flist: TFileList; cdUpLevel : Boolean) : Boolean;
var
  Folder : String;
begin
  Result := False;

  if cdUpLevel then
    begin
      if frp^.sPath = '' then  // Exit from VFS
        Exit;
      Folder := frp^.sPath;
    end
  else
    begin
      Folder := IncludeTrailingPathDelimiter(frp^.sPath + frp^.sName);
    end;

  //DebugLN('Folder = ' + Folder);

  FVFSModule.VFSList(Folder, flist);

  Result := True;
end;



function TVFS.FindModule(const sFileName:String):Boolean;
var
  Count, i:Integer;
  sExt, tmp:String;
  Index : Integer;
begin
  Result := False;
  tmp := '';
  sExt := LowerCase(ExtractFileExt(sFileName));
  sExt := copy(sExt,2,length(sExt));
  DebugLN('sExt = ', sExt);
  tmp := FPlugins.Values[sExt];
  

  //**************** Debug
     //DebugLN(FPlugins.Text);
     for i:=0 to FPlugins.Count -1 do
     DebugLN(FPlugins.ValueFromIndex[i]);
  //***************


  DebugLN('tmp = ', tmp);
  if tmp <> '' then
    begin
      Index := Pos(',', tmp) + 1;
      FCurrentPlugin := Copy(tmp, Index, Length(tmp));
      DebugLN('FCurrentPlugin = ', FCurrentPlugin);
      sLastArchive := sFileName;
      DebugLN('sLastArchive = ', sLastArchive);

      FVFSModule := TWCXModule.Create;
      FVFSModule.LoadModule(FCurrentPlugin);

      FVFSModule.VFSOpen(sLastArchive);

      DebugLN('After Module Load');

      Result := True;
    end
  else
    if sExt = 'wfx' then  // WFX Support
      begin

        FVFSModule := TWFXModule.Create;
        FVFSModule.LoadModule(sFileName);

        FVFSModule.VFSOpen('');
        //*********************
        DebugLn(PChar(Pointer(FVFSModule.VFSCaps)));
        //*********************
        Result := True;
      end;
end;


initialization

end.
