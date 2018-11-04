{
   Double Commander
   -------------------------------------------------------------------------
   Some useful functions to work with plugins

   Copyright (C) 2011-2018 Alexander Koblov (alexx2000@mail.ru)

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

  PluginBinaryTypeString: array[TBinaryType] of String =
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
function CheckPlugin(var FileName: String): Boolean;
function GetPluginBinaryType(const FileName: String): TBinaryType;

implementation

uses
  //Lazarus, Free-Pascal, etc.
  Forms, Dialogs,
  
  //DC
  {$IF DEFINED(CPU64)}
  DCStrUtils,
  {$ENDIF}
  DCOSUtils, DCClassesUtf8, uGlobs, uLng, uDCUtils;

procedure UpdatePlugins;
var
  I: Integer;
  Folder: String;
begin
  // Wcx plugins
  Folder:= '%commander_path%' + PathDelim + 'plugins' + PathDelim + 'wcx' + PathDelim;

  I:= gWCXPlugins.IndexOfName('zip');
  if I < 0 then
    gWCXPlugins.Add('zip', 735, Folder + 'zip' + PathDelim + 'zip.wcx')
  else
    gWCXPlugins.Flags[I]:= 735;

  I:= gWCXPlugins.IndexOfName('jar');
  if I < 0 then
    gWCXPlugins.Add('jar', 990, Folder + 'zip' + PathDelim + 'zip.wcx');

  {$IF DEFINED(MSWINDOWS)}
  I:= gWCXPlugins.IndexOfName('7z');
  if I < 0 then
    gWCXPlugins.Add('7z', 607, Folder + 'sevenzip' + PathDelim + 'sevenzip.wcx');
  {$ENDIF}

  I:= gWCXPlugins.IndexOfName('tar');
  if I < 0 then
    gWCXPlugins.Add('tar', 223, Folder + 'zip' + PathDelim + 'zip.wcx')
  else
    gWCXPlugins.Flags[I]:= 223;

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
    gWCXPlugins.Add('lzma', 1, Folder + 'zip' + PathDelim + 'zip.wcx')
  else begin
    gWCXPlugins.Flags[I]:= 1;
    // For lzma used another plugin, so update path too
    gWCXPlugins.FileName[I]:= Folder + 'zip' + PathDelim + 'zip.wcx';
  end;

  I:= gWCXPlugins.IndexOfName('tlz');
  if I < 0 then
    gWCXPlugins.Add('tlz', 95, Folder + 'zip' + PathDelim + 'zip.wcx')
  else
    gWCXPlugins.Flags[I]:= 95;

  {$IF NOT DEFINED(DARWIN)}
  I:= gWCXPlugins.IndexOfName('xz');
  if I < 0 then
    gWCXPlugins.Add('xz', 91, Folder + 'zip' + PathDelim + 'zip.wcx');

  I:= gWCXPlugins.IndexOfName('txz');
  if I < 0 then
    gWCXPlugins.Add('txz', 95, Folder + 'zip' + PathDelim + 'zip.wcx');

  I:= gWCXPlugins.IndexOfName('zipx');
  if I < 0 then
    gWCXPlugins.Add('zipx', 223, Folder + 'zip' + PathDelim + 'zip.wcx')
  else
    gWCXPlugins.Flags[I]:= 223;
  {$ENDIF}

  {$IF DEFINED(MSWINDOWS)}
  I:= gWCXPlugins.IndexOfName('cpio');
  if I < 0 then
    gWCXPlugins.Add('cpio', 4, Folder + 'sevenzip' + PathDelim + 'sevenzip.wcx')
  else begin
    gWCXPlugins.Flags[I]:= 4;
    // For cpio used another plugin, so update path too
    gWCXPlugins.FileName[I]:= Folder + 'sevenzip' + PathDelim + 'sevenzip.wcx';
  end;

  I:= gWCXPlugins.IndexOfName('deb');
  if I < 0 then
    gWCXPlugins.Add('deb', 4, Folder + 'sevenzip' + PathDelim + 'sevenzip.wcx')
  else begin
    gWCXPlugins.Flags[I]:= 4;
    // For deb used another plugin, so update path too
    gWCXPlugins.FileName[I]:= Folder + 'sevenzip' + PathDelim + 'sevenzip.wcx';
  end;

  I:= gWCXPlugins.IndexOfName('arj');
  if I < 0 then
    gWCXPlugins.Add('arj', 4, Folder + 'sevenzip' + PathDelim + 'sevenzip.wcx');

  I:= gWCXPlugins.IndexOfName('cab');
  if I < 0 then
    gWCXPlugins.Add('cab', 4, Folder + 'sevenzip' + PathDelim + 'sevenzip.wcx');

  I:= gWCXPlugins.IndexOfName('cramfs');
  if I < 0 then
    gWCXPlugins.Add('cramfs', 4, Folder + 'sevenzip' + PathDelim + 'sevenzip.wcx');

  I:= gWCXPlugins.IndexOfName('dmg');
  if I < 0 then
    gWCXPlugins.Add('dmg', 4, Folder + 'sevenzip' + PathDelim + 'sevenzip.wcx');

  I:= gWCXPlugins.IndexOfName('fat');
  if I < 0 then
    gWCXPlugins.Add('fat', 4, Folder + 'sevenzip' + PathDelim + 'sevenzip.wcx');

  I:= gWCXPlugins.IndexOfName('hfs');
  if I < 0 then
    gWCXPlugins.Add('hfs', 4, Folder + 'sevenzip' + PathDelim + 'sevenzip.wcx');

  I:= gWCXPlugins.IndexOfName('iso');
  if I < 0 then
    gWCXPlugins.Add('iso', 4, Folder + 'sevenzip' + PathDelim + 'sevenzip.wcx');

  I:= gWCXPlugins.IndexOfName('lha');
  if I < 0 then
    gWCXPlugins.Add('lha', 4, Folder + 'sevenzip' + PathDelim + 'sevenzip.wcx');

  I:= gWCXPlugins.IndexOfName('lzh');
  if I < 0 then
    gWCXPlugins.Add('lzh', 4, Folder + 'sevenzip' + PathDelim + 'sevenzip.wcx');

  I:= gWCXPlugins.IndexOfName('ntfs');
  if I < 0 then
    gWCXPlugins.Add('ntfs', 4, Folder + 'sevenzip' + PathDelim + 'sevenzip.wcx');

  I:= gWCXPlugins.IndexOfName('squashfs');
  if I < 0 then
    gWCXPlugins.Add('squashfs', 4, Folder + 'sevenzip' + PathDelim + 'sevenzip.wcx');

  I:= gWCXPlugins.IndexOfName('taz');
  if I < 0 then
    gWCXPlugins.Add('taz', 4, Folder + 'sevenzip' + PathDelim + 'sevenzip.wcx');

  I:= gWCXPlugins.IndexOfName('vhd');
  if I < 0 then
    gWCXPlugins.Add('vhd', 4, Folder + 'sevenzip' + PathDelim + 'sevenzip.wcx');

  I:= gWCXPlugins.IndexOfName('wim');
  if I < 0 then
    gWCXPlugins.Add('wim', 85, Folder + 'sevenzip' + PathDelim + 'sevenzip.wcx');

  I:= gWCXPlugins.IndexOfName('xar');
  if I < 0 then
    gWCXPlugins.Add('xar', 4, Folder + 'sevenzip' + PathDelim + 'sevenzip.wcx');

  I:= gWCXPlugins.IndexOfName('z');
  if I < 0 then
    gWCXPlugins.Add('z', 4, Folder + 'sevenzip' + PathDelim + 'sevenzip.wcx');
  {$ELSE}
  I:= gWCXPlugins.IndexOfName('cpio');
  if I < 0 then
    gWCXPlugins.Add('cpio', 4, Folder + 'cpio' + PathDelim + 'cpio.wcx')
  else
    gWCXPlugins.Flags[I]:= 4;

  I:= gWCXPlugins.IndexOfName('deb');
  if I < 0 then
    gWCXPlugins.Add('deb', 4, Folder + 'deb' + PathDelim + 'deb.wcx')
  else
    gWCXPlugins.Flags[I]:= 4;
  {$ENDIF}

  I:= gWCXPlugins.IndexOfName('rpm');
  if I < 0 then
    gWCXPlugins.Add('rpm', 4, Folder + 'rpm' + PathDelim + 'rpm.wcx')
  else
    gWCXPlugins.Flags[I]:= 4;

  I:= gWCXPlugins.IndexOfName('rar');
  {$IF DEFINED(MSWINDOWS)}
  if I < 0 then
    gWCXPlugins.Add('rar', 607, Folder + 'unrar' + PathDelim + 'unrar.wcx')
  else
    gWCXPlugins.Flags[I]:= 607;
  {$ELSE}
  if I < 0 then
    gWCXPlugins.Add('rar', 68, Folder + 'unrar' + PathDelim + 'unrar.wcx')
  else
    gWCXPlugins.Flags[I]:= 68;
  {$ENDIF}

  // Wdx plugins
  Folder:= '%commander_path%' + PathDelim + 'plugins' + PathDelim + 'wdx' + PathDelim;

  if gWdxPlugins.IndexOfName('deb_wdx') < 0 then
  begin
    gWdxPlugins.Add('deb_wdx', Folder + 'deb_wdx' + PathDelim + 'deb_wdx.wdx', 'EXT="DEB"');
  end;

  if gWdxPlugins.IndexOfName('rpm_wdx') < 0 then
  begin
    gWdxPlugins.Add('rpm_wdx', Folder + 'rpm_wdx' + PathDelim + 'rpm_wdx.wdx', 'EXT="RPM"');
  end;

  if gWdxPlugins.IndexOfName('audioinfo') < 0 then
  begin
    gWdxPlugins.Add(GetCmdDirFromEnvVar(Folder) + 'audioinfo' + PathDelim + 'audioinfo.wdx');
  end;

  // Wfx plugins
  Folder:= '%commander_path%' + PathDelim + 'plugins' + PathDelim + 'wfx' + PathDelim;

  if gWFXPlugins.IndexOfName('FTP') < 0 then
  begin
    gWFXPlugins.Add('FTP', Folder + 'ftp' + PathDelim + 'ftp.wfx');
  end;

  {$IF DEFINED(UNIX) AND NOT DEFINED(DARWIN)}
  if gWFXPlugins.IndexOfName('Windows Network') < 0 then
  begin
    gWFXPlugins.Add('Windows Network', Folder + 'samba' + PathDelim + 'samba.wfx');
  end;
  {$ENDIF}

  {$IF DEFINED(LINUX)}
  // Wlx plugins
  Folder:= '%commander_path%' + PathDelim + 'plugins' + PathDelim + 'wlx' + PathDelim;

  I:= gWlxPlugins.IndexOfName('wlxMplayer');
  if I >= 0 then
  begin
    gWlxPlugins.GetWlxModule(I).FileName:= Folder + 'wlxmplayer' + PathDelim + 'wlxmplayer.wlx';
  end;
  {$ENDIF}
end;

function CheckPlugin(var FileName: String): Boolean;
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

function GetPluginBinaryType(const FileName: String): TBinaryType;
var
  fsFileStream: TFileStreamEx;
begin
  try
    fsFileStream:= TFileStreamEx.Create(FileName, fmOpenRead or fmShareDenyNone);
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

