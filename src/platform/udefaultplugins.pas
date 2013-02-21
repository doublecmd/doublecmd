{
   Double Commander
   -------------------------------------------------------------------------
   Some useful functions to work with plugins

   Copyright (C) 2011-2013 Alexander Koblov (alexx2000@mail.ru)

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

unit uDefaultPlugins;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

const
  WcxMask = '*.wcx'{$IFDEF CPU64} + ';*.wcx64'{$ENDIF};
  WdxMask = '*.wdx'{$IFDEF CPU64} + ';*.wdx64'{$ENDIF};
  WfxMask = '*.wfx'{$IFDEF CPU64} + ';*.wfx64'{$ENDIF};
  WlxMask = '*.wlx'{$IFDEF CPU64} + ';*.wlx64'{$ENDIF};

type
  TBinaryType = (btUnknown, btPe32, btPe64, btElf32, btElf64, btMacho32, btMacho64);

const
  PluginBinaryType =
    {$IF DEFINED(WIN32)}
    btPe32
    {$ELSEIF DEFINED(WIN64)}
    btPe64
    {$ELSEIF DEFINED(DARWIN) AND DEFINED(CPU32)}
    btMacho32
    {$ELSEIF DEFINED(DARWIN) AND DEFINED(CPU64)}
    btMacho64
    {$ELSEIF DEFINED(UNIX) AND DEFINED(CPU32)}
    btElf32
    {$ELSEIF DEFINED(UNIX) AND DEFINED(CPU64)}
    btElf64
    {$ELSE}
    btUnknown
    {$ENDIF}
    ;

  PluginBinaryTypeString: array[TBinaryType] of UTF8String =
    (
      'Unknown',
      'Windows 32 bit',
      'Windows 64 bit',
      'Unix 32 bit',
      'Unix 64 bit',
      'Mac OS X 32 bit',
      'Mac OS X 64 bit'
    );

procedure UpdatePlugins;
function CheckPlugin(var FileName: UTF8String): Boolean;
function GetPluginBinaryType(const FileName: UTF8String): TBinaryType;

implementation

uses
  Forms, Dialogs, DCOSUtils, DCStrUtils, uGlobs, uLng;

procedure UpdatePlugins;
var
  I: Integer;
  Folder: UTF8String;
begin
  // Wcx plugins
  Folder:= '%commander_path%' + PathDelim + 'plugins' + PathDelim + 'wcx' + PathDelim;

  I:= gWCXPlugins.IndexOfName('zip');
  if I < 0 then
    gWCXPlugins.Add('zip', 607, Folder + 'zip' + PathDelim + 'zip.wcx')
  else
    gWCXPlugins.Flags[I]:= 607;

  I:= gWCXPlugins.IndexOfName('tar');
  if I < 0 then
    gWCXPlugins.Add('tar', 95, Folder + 'zip' + PathDelim + 'zip.wcx')
  else
    gWCXPlugins.Flags[I]:= 95;

  I:= gWCXPlugins.IndexOfName('bz2');
  if I < 0 then
    gWCXPlugins.Add('bz2', 91, Folder + 'zip' + PathDelim + 'zip.wcx')
  else
    begin
      gWCXPlugins.Flags[I]:= 91;
      // For bz2 used another plugin, so update path too
      gWCXPlugins.FileName[I]:= Folder + 'zip' + PathDelim + 'zip.wcx';
    end;

  I:= gWCXPlugins.IndexOfName('tbz');
  if I < 0 then
    gWCXPlugins.Add('tbz', 95, Folder + 'zip' + PathDelim + 'zip.wcx')
  else
    gWCXPlugins.Flags[I]:= 95;

  I:= gWCXPlugins.IndexOfName('gz');
  if I < 0 then
    gWCXPlugins.Add('gz', 91, Folder + 'zip' + PathDelim + 'zip.wcx')
  else
    gWCXPlugins.Flags[I]:= 91;

  I:= gWCXPlugins.IndexOfName('tgz');
  if I < 0 then
    gWCXPlugins.Add('tgz', 95, Folder + 'zip' + PathDelim + 'zip.wcx')
  else
    gWCXPlugins.Flags[I]:= 95;

  I:= gWCXPlugins.IndexOfName('lzma');
  if I < 0 then
    gWCXPlugins.Add('lzma', 1, Folder + 'lzma' + PathDelim + 'lzma.wcx')
  else
    gWCXPlugins.Flags[I]:= 1;

  I:= gWCXPlugins.IndexOfName('cpio');
  if I < 0 then
    gWCXPlugins.Add('cpio', 0, Folder + 'cpio' + PathDelim + 'cpio.wcx')
  else
    gWCXPlugins.Flags[I]:= 0;

  I:= gWCXPlugins.IndexOfName('deb');
  if I < 0 then
    gWCXPlugins.Add('deb', 4, Folder + 'deb' + PathDelim + 'deb.wcx')
  else
    gWCXPlugins.Flags[I]:= 4;

  I:= gWCXPlugins.IndexOfName('rpm');
  if I < 0 then
    gWCXPlugins.Add('rpm', 4, Folder + 'rpm' + PathDelim + 'rpm.wcx')
  else
    gWCXPlugins.Flags[I]:= 4;

  I:= gWCXPlugins.IndexOfName('rar');
  if I < 0 then
    gWCXPlugins.Add('rar', 68, Folder + 'unrar' + PathDelim + 'unrar.wcx')
  else
    gWCXPlugins.Flags[I]:= 68;

  // Wfx plugins
  Folder:= '%commander_path%' + PathDelim + 'plugins' + PathDelim + 'wfx' + PathDelim;

  if gWFXPlugins.IndexOfName('FTP') < 0 then
  begin
    gWFXPlugins.Add('FTP', Folder + 'ftp' + PathDelim + 'ftp.wfx');
  end;

  {$IF DEFINED(UNIX)}
  if gWFXPlugins.IndexOfName('Windows Network') < 0 then
  begin
    gWFXPlugins.Add('Windows Network', Folder + 'samba' + PathDelim + 'samba.wfx');
  end;
  {$ENDIF}
end;

function CheckPlugin(var FileName: UTF8String): Boolean;
var
  PluginType: TBinaryType;
begin
  {$IF DEFINED(CPU64)}
  if (StrEnds(FileName, '64') = False) and mbFileExists(FileName + '64') then
  begin
    FileName:= FileName + '64';
  end;
  {$ENDIF}
  PluginType:= GetPluginBinaryType(FileName);
  case PluginType of
    PluginBinaryType: Exit(True);
    btUnknown: MessageDlg(Application.Title, rsMsgInvalidPlugin, mtError, [mbOK], 0, mbOK);
    else MessageDlg(Application.Title,
                    Format(rsMsgInvalidPluginArchitecture,
                    [
                      PluginBinaryTypeString[PluginType],
                      LineEnding,
                      PluginBinaryTypeString[PluginBinaryType]
                    ]), mtError, [mbOK], 0, mbOK);
  end;
  Result:= False;
end;

function GetPluginBinaryType(const FileName: UTF8String): TBinaryType;
var
  fsFileStream: TFileStream;
begin
  try
    fsFileStream:= TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    try
      // Check Windows executable
      if fsFileStream.ReadWord = $5A4D then // 'MZ'
      begin
        fsFileStream.Seek(60, soBeginning);
        fsFileStream.Seek(fsFileStream.ReadDWord, soBeginning);
        if fsFileStream.ReadDWord = $4550 then // 'PE'
        begin
          fsFileStream.Seek(20, soCurrent);
          case fsFileStream.ReadWord of
            $10B: Exit(btPe32); // 32 bit
            $20B: Exit(btPe64); // 64 bit
          end;
        end;
      end;
      fsFileStream.Seek(0, soBeginning);
      // Check Unix executable
      if fsFileStream.ReadDWord = $464C457F then // 'ELF'
      begin
        case fsFileStream.ReadByte of
          1: Exit(btElf32); // 32 bit
          2: Exit(btElf64); // 64 bit
        end;
      end;
      fsFileStream.Seek(0, soBeginning);
      // Check Darwin executable
      case fsFileStream.ReadDWord of
        $feedface, $cefaedfe: Exit(btMacho32); // 32 bit
        $feedfacf, $cffaedfe: Exit(btMacho64); // 64 bit
      end;
      Result:= btUnknown;
    finally
      fsFileStream.Free;
    end;
  except
    Result:= btUnknown;
  end;
end;

end.

