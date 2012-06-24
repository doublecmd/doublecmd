unit uDefaultPlugins;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

procedure UpdatePlugins;

implementation

uses
  uGlobs;

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

end.

