{
   Double commander
   -------------------------------------------------------------------------
   WCX plugin for working with *.zip, *.gz, *.bz2, *.tar, *.tgz, *.tbz archives

   Copyright (C) 2008-2023 Alexander Koblov (alexx2000@mail.ru)

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
{$include calling.inc}

{$R ZipConfDlg.lfm}

interface

uses
  SysUtils, Extension;

procedure CreateZipConfDlg;

implementation

uses
  ZipFunc, ZipOpt, ZipLng, AbZipTyp;

function GetComboBox(pDlg: PtrUInt; DlgItemName: PAnsiChar): PtrInt;
var
  Index: IntPtr;
begin
  with gStartupInfo do
  begin
    Index:= SendDlgMsg(pDlg, DlgItemName, DM_LISTGETITEMINDEX, 0, 0);
    Result:= SendDlgMsg(pDlg, DlgItemName, DM_LISTGETDATA, Index, 0);
  end;
end;

procedure SetComboBox(pDlg: PtrUInt; DlgItemName: PAnsiChar; ItemData: PtrInt);
var
  Index, Count: Integer;
begin
  with gStartupInfo do
  begin
    Count:= SendDlgMsg(pDlg, DlgItemName, DM_LISTGETCOUNT, 0, 0);
    for Index:= 0 to Count - 1 do
    begin
      if SendDlgMsg(pDlg, DlgItemName, DM_LISTGETDATA, Index, 0) = ItemData then
      begin
        SendDlgMsg(pDlg, DlgItemName, DM_LISTSETITEMINDEX, Index, 0);
        Exit;
      end;
    end;
  end;
end;

function ComboBoxAdd(pDlg: PtrUInt; DlgItemName: PAnsiChar; ItemText: String; ItemData: PtrInt): IntPtr;
var
  P: PAnsiChar;
  AText: IntPtr absolute P;
begin
  P:= PAnsiChar(ItemText);
  Result:= gStartupInfo.SendDlgMsg(pDlg, DlgItemName, DM_LISTADD, AText, ItemData);
end;

function AddCompressionLevel(pDlg: PtrUInt; const AName: String; ALevel: IntPtr): IntPtr;
var
  AText: String;
begin
  AText:= AName + ' (' + IntToStr(ALevel) + ')';
  Result:= ComboBoxAdd(pDlg, 'cbCompressionLevel', AText, ALevel);
end;

procedure UpdateLevel(pDlg: PtrUInt; ALevel: IntPtr);
var
  Index: IntPtr;
  AFormat: TArchiveFormat;
  AMethod: TAbZipCompressionMethod;
begin
  with gStartupInfo do
  begin
    SendDlgMsg(pDlg, 'cbCompressionLevel', DM_LISTCLEAR, 0, 0);
    AFormat:= TArchiveFormat(GetComboBox(pDlg, 'cbArchiveFormat'));
    Index:= SendDlgMsg(pDlg, 'cbCompressionMethod', DM_LISTGETITEMINDEX, 0, 0);
    AMethod:= TAbZipCompressionMethod(SendDlgMsg(pDlg, 'cbCompressionMethod', DM_LISTGETDATA, Index, 0));
    if (AMethod = cmStored) then
    begin
      SendDlgMsg(pDlg, 'cbCompressionLevel', DM_ENABLE, 0, 0);
    end
    else begin
      SendDlgMsg(pDlg, 'cbCompressionLevel', DM_ENABLE, 1, 0);
     case AMethod of
       cmDeflated,
       cmEnhancedDeflated:
         begin
          AddCompressionLevel(pDlg, rsCompressionLevelFastest, 1);
          AddCompressionLevel(pDlg, rsCompressionLevelFast, 3);
          Index:= AddCompressionLevel(pDlg, rsCompressionLevelNormal, 6);
          AddCompressionLevel(pDlg, rsCompressionLevelMaximum, 9);
         end;
       cmXz,
       cmLZMA,
       cmBzip2:
         begin
          AddCompressionLevel(pDlg, rsCompressionLevelFastest, 1);
          AddCompressionLevel(pDlg, rsCompressionLevelFast, 3);
          Index:= AddCompressionLevel(pDlg, rsCompressionLevelNormal, 5);
          AddCompressionLevel(pDlg, rsCompressionLevelMaximum, 7);
          AddCompressionLevel(pDlg, rsCompressionLevelUltra, 9);
         end;
       cmZstd:
         begin
           AddCompressionLevel(pDlg, rsCompressionLevelFastest, 3);
           AddCompressionLevel(pDlg, rsCompressionLevelFast, 5);
           Index:= AddCompressionLevel(pDlg, rsCompressionLevelNormal, 11);
           AddCompressionLevel(pDlg, rsCompressionLevelMaximum, 17);
           AddCompressionLevel(pDlg, rsCompressionLevelUltra, 22);
         end;
     end;
     if ALevel < 0 then
       SendDlgMsg(pDlg, 'cbCompressionLevel', DM_LISTSETITEMINDEX, Index, 0)
     else begin
       SetComboBox(pDlg, 'cbCompressionLevel', PluginConfig[AFormat].Level);
     end;
    end;
  end;
end;

procedure UpdateMethod(pDlg: PtrUInt);
var
  Index: IntPtr;
  AFormat: TArchiveFormat;
begin
  with gStartupInfo do
  begin
    SendDlgMsg(pDlg, 'cbCompressionMethod', DM_LISTCLEAR, 0, 0);
    AFormat:= TArchiveFormat(GetComboBox(pDlg, 'cbArchiveFormat'));
    case AFormat of
      afGzip:
        ComboBoxAdd(pDlg, 'cbCompressionMethod', 'Deflate', PtrInt(cmDeflated));
      afXzip:
        ComboBoxAdd(pDlg, 'cbCompressionMethod', 'LZMA2', PtrInt(cmXz));
      afBzip2:
        ComboBoxAdd(pDlg, 'cbCompressionMethod', 'BZip2', PtrInt(cmBzip2));
      afZstd:
        ComboBoxAdd(pDlg, 'cbCompressionMethod', 'Zstandard', PtrInt(cmZstd));
      afZip:
        begin
          ComboBoxAdd(pDlg, 'cbCompressionMethod', rsCompressionMethodStore, PtrInt(cmStored));
          ComboBoxAdd(pDlg, 'cbCompressionMethod', 'Deflate', PtrInt(cmDeflated));
          ComboBoxAdd(pDlg, 'cbCompressionMethod', rsCompressionMethodOptimal, PtrInt(cmEnhancedDeflated));
        end;
      afZipx:
        begin
          ComboBoxAdd(pDlg, 'cbCompressionMethod', 'LZMA2', PtrInt(cmXz));
          ComboBoxAdd(pDlg, 'cbCompressionMethod', 'Zstandard', PtrInt(cmZstd));
        end;
    end; // case
    Index:= SendDlgMsg(pDlg, 'cbCompressionMethod', DM_LISTGETCOUNT, 0, 0);
    if (Index = 1) then
    begin
      SendDlgMsg(pDlg, 'cbCompressionMethod', DM_LISTSETITEMINDEX, 0, 0);
      SendDlgMsg(pDlg, 'cbCompressionMethod', DM_ENABLE, 0, 0);
    end
    else begin
      SendDlgMsg(pDlg, 'cbCompressionMethod', DM_ENABLE, 1, 0);
      SetComboBox(pDlg, 'cbCompressionMethod', PluginConfig[AFormat].Method);
    end;
  end;
  UpdateLevel(pDlg, PluginConfig[AFormat].Level);
end;

function DlgProc (pDlg: PtrUInt; DlgItemName: PAnsiChar; Msg, wParam, lParam: PtrInt): PtrInt; dcpcall;
var
  Index: IntPtr;
  AFormat: TArchiveFormat;
begin
  Result:= 0;
  with gStartupInfo do
  begin
    case Msg of
      DN_INITDIALOG:
        begin
          ComboBoxAdd(pDlg, 'cbArchiveFormat', 'gz', PtrInt(afGzip));
          ComboBoxAdd(pDlg, 'cbArchiveFormat', 'xz', PtrInt(afXzip));
          ComboBoxAdd(pDlg, 'cbArchiveFormat', 'bz2', PtrInt(afBzip2));
          ComboBoxAdd(pDlg, 'cbArchiveFormat', 'zst', PtrInt(afZstd));
          Index:= ComboBoxAdd(pDlg, 'cbArchiveFormat', 'zip', PtrInt(afZip));
          ComboBoxAdd(pDlg, 'cbArchiveFormat', 'zipx', PtrInt(afZipx));

          SendDlgMsg(pDlg, 'cbArchiveFormat', DM_LISTSETITEMINDEX, Index, 0);

          UpdateMethod(pDlg);

          SendDlgMsg(pDlg, 'chkTarAutoHandle', DM_SETCHECK, PtrInt(gTarAutoHandle), 0);
        end;
      DN_CHANGE:
        begin
          if DlgItemName = 'cbArchiveFormat' then
          begin
            UpdateMethod(pDlg);
          end
          else if DlgItemName = 'cbCompressionMethod' then
          begin
            UpdateLevel(pDlg, -1);
          end;
        end;
      DN_CLICK:
        if DlgItemName = 'btnOK' then
          begin
            AFormat:= TArchiveFormat(GetComboBox(pDlg, 'cbArchiveFormat'));
            PluginConfig[AFormat].Level:= GetComboBox(pDlg, 'cbCompressionLevel');
            PluginConfig[AFormat].Method:= GetComboBox(pDlg, 'cbCompressionMethod');
            gTarAutoHandle:= Boolean(SendDlgMsg(pDlg, 'chkTarAutoHandle', DM_GETCHECK, 0, 0));
            SaveConfiguration;
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

end.

