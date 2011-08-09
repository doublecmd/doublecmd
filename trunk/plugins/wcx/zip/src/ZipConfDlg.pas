{
   Double commander
   -------------------------------------------------------------------------
   WCX plugin for working with *.zip, *.gz, *.tar, *.tgz archives


   Copyright (C) 2008  Koblov Alexander (Alexx2000@mail.ru)


   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   in a file called COPYING along with this program; if not, write to
   the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
   02139, USA.
}

unit ZipConfDlg;

{$mode objfpc}{$H+}

{$R ZipConfDlg.lfm}

interface

uses
  SysUtils, Extension;

procedure CreateZipConfDlg;
procedure LoadConfig;
procedure SaveConfig;
  
implementation

uses ZipFunc, AbZipTyp, IniFiles;

function DlgProc (pDlg: PtrUInt; DlgItemName: PAnsiChar; Msg, wParam, lParam: PtrInt): PtrInt; stdcall;
var
 iIndex: Integer;
begin
  Result:= 0;
  with gStartupInfo do
  begin
    case Msg of
      DN_INITDIALOG:
        begin
          case gCompressionMethodToUse of
            smStored:
              SendDlgMsg(pDlg, 'cbCompressionMethodToUse', DM_LISTSETITEMINDEX, 0, 0);
            smDeflated:
              SendDlgMsg(pDlg, 'cbCompressionMethodToUse', DM_LISTSETITEMINDEX, 1, 0);
            smBestMethod:
              SendDlgMsg(pDlg, 'cbCompressionMethodToUse', DM_LISTSETITEMINDEX, 2, 0);
          end; // case
          case gDeflationOption of
            doNormal:
              SendDlgMsg(pDlg, 'cbDeflationOption', DM_LISTSETITEMINDEX, 0, 0);
            doMaximum:
              SendDlgMsg(pDlg, 'cbDeflationOption', DM_LISTSETITEMINDEX, 1, 0);
            doFast:
              SendDlgMsg(pDlg, 'cbDeflationOption', DM_LISTSETITEMINDEX, 2, 0);
            doSuperFast:
              SendDlgMsg(pDlg, 'cbDeflationOption', DM_LISTSETITEMINDEX, 3, 0);
          end; // case
        end;
      DN_CLICK:
        if DlgItemName = 'btnOK' then
          begin
            iIndex:= SendDlgMsg(pDlg, 'cbCompressionMethodToUse', DM_LISTGETITEMINDEX, 0, 0);
            case iIndex of
              0:
                gCompressionMethodToUse:= smStored;
              1:
                gCompressionMethodToUse:= smDeflated;
              2:
                gCompressionMethodToUse:= smBestMethod;
            end; // case
            iIndex:= SendDlgMsg(pDlg, 'cbDeflationOption', DM_LISTGETITEMINDEX, 0, 0);
            case iIndex of
              0:
                gDeflationOption:= doNormal;
              1:
                gDeflationOption:= doMaximum;
              2:
                gDeflationOption:= doFast;
              3:
                gDeflationOption:= doSuperFast;
            end; // case
            SaveConfig;
            SendDlgMsg(pDlg, DlgItemName, DM_CLOSE, 1, 0);
          end
        else if DlgItemName = 'btnCancel' then
          SendDlgMsg(pDlg, DlgItemName, DM_CLOSE, 2, 0);
    end;// case
  end; // with
end;

procedure CreateZipConfDlg;
var
  ResHandle: TFPResourceHandle = 0;
  ResGlobal: TFPResourceHGLOBAL = 0;
  ResData: Pointer = nil;
  ResSize: LongWord;
begin
  try
    ResHandle := FindResource(HINSTANCE, PChar('TDIALOGBOX'), MAKEINTRESOURCE(10) {RT_RCDATA});
    if ResHandle <> 0 then
    begin
      ResGlobal := LoadResource(HINSTANCE, ResHandle);
      if ResGlobal <> 0 then
      begin
        ResData := LockResource(ResGlobal);
        ResSize := SizeofResource(HINSTANCE, ResHandle);

        with gStartupInfo do
        begin
          DialogBoxLRS(ResData, ResSize, @DlgProc);
        end;
      end;
    end;

  finally
    if ResGlobal <> 0 then
    begin
      UnlockResource(ResGlobal);
      FreeResource(ResGlobal);
    end;
  end;
end;

procedure LoadConfig;
var
  gIni: TIniFile;
begin
  gIni:= TIniFile.Create(gStartupInfo.PluginConfDir + IniFileName);
  try
    gCompressionMethodToUse:= TAbZipSupportedMethod(gIni.ReadInteger('Configuration', 'CompressionMethodToUse', 2));
    gDeflationOption:= TAbZipDeflationOption(gIni.ReadInteger('Configuration', 'DeflationOption', 0));
  finally
    gIni.Free;
  end;
end;

procedure SaveConfig;
var
  gIni: TIniFile;
begin
  gIni:= TIniFile.Create(gStartupInfo.PluginConfDir + IniFileName);
  try
    gIni.WriteInteger('Configuration', 'CompressionMethodToUse', Integer(gCompressionMethodToUse));
    gIni.WriteInteger('Configuration', 'DeflationOption', Integer(gDeflationOption));
  finally
    gIni.Free;
  end;
end;

end.

