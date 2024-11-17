{
  Double Commander
  -------------------------------------------------------------------------
  SevenZip archiver plugin, dialogs unit

  Copyright (C) 2014-2024 Alexander Koblov (alexx2000@mail.ru)

  Based on 7-Zip 15.06 (http://7-zip.org)
  7-Zip Copyright (C) 1999-2015 Igor Pavlov

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
}

unit SevenZipDlg;

{$mode delphi}

interface

uses
  Classes, SysUtils, Windows, Math, SevenZipOpt, SevenZipLng, JclCompression, Extension;

procedure ShowConfigurationDialog(Parent: HWND);
function ShowPasswordQuery(var Encrypt: Boolean; var Password: WideString): Boolean;

procedure DialogInitialize(StartupInfo: PExtensionStartupInfo);

implementation

uses
  LazUTF8, SevenZipCodecs, SevenZipHlp;

{$R SevenZipDlg.lfm}
{$R SevenZipPwd.lfm}

const
  IDC_PASSWORD = 'edtPassword';
  IDC_SHOW_PASSWORD = 'cbShowPassword';
  IDC_ENCRYPT_HEADER = 'cbEncryptNames';

const
  IDC_APPLY_BUTTON = 'btnApply';
  IDC_COMP_FORMAT = 'cbArchiveFormat';
  IDC_COMP_METHOD = 'cbCompressionMethod';
  IDC_COMP_LEVEL = 'cbCompressionLevel';
  IDC_COMP_DICT = 'cbCompressionDictionary';
  IDC_COMP_WORD = 'cbCompressionWord';
  IDC_COMP_SOLID = 'cbCompressionSolid';
  IDC_COMP_THREAD = 'cbThreads';
  IDC_MAX_THREAD = 'lblMaxThreads';
  IDC_PARAMETERS = 'edtParameters';
  IDC_MEMORY_COMP = 'lblMemoryCompressionValue';
  IDC_MEMORY_DECOMP = 'lblMemoryDecompressionValue';

type
  PPasswordData = ^TPasswordData;
  TPasswordData = record
    Password: String;
    EncryptHeader: Boolean;
  end;

var
  gStartupInfo: TExtensionStartupInfo;

procedure SetDlgItemText(hDlg: HWND; const DlgItemName: String; lpString: PAnsiChar);
var
  Data: PtrInt absolute lpString;
begin
  gStartupInfo.SendDlgMsg(hDlg, PAnsiChar(DlgItemName), DM_SETTEXT, Data, 0);
end;

function GetComboBox(pDlg: PtrUInt; DlgItemName: PAnsiChar): PtrInt;
var
  Index: IntPtr;
begin
  with gStartupInfo do
  begin
    Index:= SendDlgMsg(pDlg, DlgItemName, DM_LISTGETITEMINDEX, 0, 0);
    if (Index < 0) then
      Result:= Index
    else begin
      Result:= SendDlgMsg(pDlg, DlgItemName, DM_LISTGETDATA, Index, 0);
    end;
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

procedure SaveArchiver(hwndDlg: HWND);
var
  Data: PtrInt;
  Format: TArchiveFormat;
  Parameters: PAnsiChar absolute Data;
begin
  Format:= TArchiveFormat(gStartupInfo.SendDlgMsg(hwndDlg, IDC_APPLY_BUTTON, DM_GETDLGDATA, 0, 0));
  PluginConfig[Format].Level:= GetComboBox(hwndDlg, IDC_COMP_LEVEL);
  PluginConfig[Format].Method:= GetComboBox(hwndDlg, IDC_COMP_METHOD);
  if PluginConfig[Format].Level <> PtrInt(clStore) then
  begin
    PluginConfig[Format].Dictionary:= GetComboBox(hwndDlg, IDC_COMP_DICT);
    PluginConfig[Format].WordSize:= GetComboBox(hwndDlg, IDC_COMP_WORD);
    PluginConfig[Format].SolidSize:= GetComboBox(hwndDlg, IDC_COMP_SOLID);
    PluginConfig[Format].ThreadCount:= GetComboBox(hwndDlg, IDC_COMP_THREAD);
  end;
  Data:= gStartupInfo.SendDlgMsg(hwndDlg, IDC_PARAMETERS, DM_GETTEXT, 0, 0);
  PluginConfig[Format].Parameters:= Parameters;

  SaveConfiguration;
end;

function GetMemoryUsage(hwndDlg: HWND; out decompressMemory: Int64): Int64;
var
  size: Int64 = 0;
  Dictionary, hs,
  numThreads, numThreads1,
  numBlockThreads: Cardinal;
  size1, chunkSize: Int64;
  Level: TCompressionLevel;
  Method: TJclCompressionMethod;
begin
  Level := TCompressionLevel(GetComboBox(hwndDlg, IDC_COMP_LEVEL));
  if (level = clStore) then
  begin
    decompressMemory := (1 shl 20);
    Exit(decompressMemory);
  end;

  decompressMemory := -1;
  Dictionary := Cardinal(GetComboBox(hwndDlg, IDC_COMP_DICT));
  Method := TJclCompressionMethod(GetComboBox(hwndDlg, IDC_COMP_METHOD));

  if (Method <> cmDeflate) and (Method <> cmDeflate64) and (level >= clUltra) then
    size += (12 shl 20) * 2 + (5 shl 20);

  numThreads := GetComboBox(hwndDlg, IDC_COMP_THREAD);

  case (method) of
    cmLZMA,
    cmLZMA2:
      begin
        hs := dictionary - 1;
        hs := hs or (hs shr 1);
        hs := hs or (hs shr 2);
        hs := hs or (hs shr 4);
        hs := hs or (hs shr 8);
        hs := hs shr 1;
        hs := hs or $FFFF;
        if (hs > (1 shl 24)) then
          hs := hs shr 1;
        Inc(hs);
        size1 := Int64(hs) * 4;
        size1 += Int64(dictionary) * 4;
        if (level >= clNormal) then
          size1 += Int64(dictionary) * 4;
        size1 += (2 shl 20);

        numThreads1 := 1;
        if (numThreads > 1) and (level >= clNormal) then
        begin
          size1 += (2 shl 20) + (4 shl 20);
          numThreads1 := 2;
        end;

        numBlockThreads := numThreads div numThreads1;

        if (method = cmLZMA) or (numBlockThreads = 1) then
          size1 += Int64(dictionary) * 3 div 2
        else
        begin
          chunkSize := Int64(dictionary) shl 2;
          chunkSize := Max(chunkSize, Int64(1 shl 20));
          chunkSize := Min(chunkSize, Int64(1 shl 28));
          chunkSize := Max(chunkSize, Int64(dictionary));
          size1 += chunkSize * 2;
        end;
        size += size1 * numBlockThreads;

        decompressMemory := Int64(dictionary) + (2 shl 20);
        Exit(size);
      end;
    cmPPMd:
    begin
      decompressMemory := Int64(dictionary) + (2 shl 20);
      Exit(size + decompressMemory);
    end;
    cmDeflate,
    cmDeflate64:
      begin
        if (level >= clMaximum) then
          size += (1 shl 20);
        size += 3 shl 20;
        decompressMemory := (2 shl 20);
        Exit(size);
      end;
    cmBZip2:
      begin
        decompressMemory := (7 shl 20);
        size1 := (10 shl 20);
        Exit(size + size1 * numThreads);
      end;
  end;
  Result := -1;
end;

procedure UpdateMemoryUsage(hwndDlg: HWND);
var
  Comp, Decomp: Int64;
begin
  if (GetComboBox(hwndDlg, IDC_COMP_METHOD) > cmMaximum) then
  begin
    SetDlgItemText(hwndDlg, IDC_MEMORY_COMP, '?');
    SetDlgItemText(hwndDlg, IDC_MEMORY_DECOMP, '?');
  end
  else begin
    Comp := GetMemoryUsage(hwndDlg, Decomp);
    SetDlgItemText(hwndDlg, IDC_MEMORY_COMP, PAnsiChar(IntToStr(Comp div cMega) + 'Mb'));
    SetDlgItemText(hwndDlg, IDC_MEMORY_DECOMP, PAnsiChar(IntToStr(Decomp div cMega) + 'Mb'));
  end;
end;

procedure SetDefaultOptions(hwndDlg: HWND);
var
  Value: PtrInt;
  Level: TCompressionLevel;
  Method: TJclCompressionMethod;
begin
  Value:= GetComboBox(hwndDlg, IDC_COMP_METHOD);

  if (Value <= cmMaximum) then
  begin
    // Get compression method
    Method:= TJclCompressionMethod(Value);
    // Get compression level
    Level:= TCompressionLevel(GetComboBox(hwndDlg, IDC_COMP_LEVEL));

    case Method of
    cmDeflate,
    cmDeflate64:
       begin
         case Level of
         clFastest,
         clFast,
         clNormal:
           SetComboBox(hwndDlg, IDC_COMP_WORD, 32);
         clMaximum:
           SetComboBox(hwndDlg, IDC_COMP_WORD, 64);
         clUltra:
           SetComboBox(hwndDlg, IDC_COMP_WORD, 128);
         end;
         gStartupInfo.SendDlgMsg(hwndDlg, IDC_COMP_DICT, DM_LISTSETITEMINDEX, 0, 0);
       end;
    cmBZip2:
       begin
         case Level of
         clFastest:
           begin
             SetComboBox(hwndDlg, IDC_COMP_DICT, 100 * cKilo);
             SetComboBox(hwndDlg, IDC_COMP_SOLID, 8 * cKilo);
           end;
         clFast:
           begin
             SetComboBox(hwndDlg, IDC_COMP_DICT, 500 * cKilo);
             SetComboBox(hwndDlg, IDC_COMP_SOLID, 32 * cKilo);
           end;
         clNormal,
         clMaximum,
         clUltra:
           begin
             SetComboBox(hwndDlg, IDC_COMP_DICT, 900 * cKilo);
             SetComboBox(hwndDlg, IDC_COMP_SOLID, 64 * cKilo);
           end;
         end;
       end;
    cmLZMA,
    cmLZMA2:
       begin
         case Level of
         clFastest:
           begin
             SetComboBox(hwndDlg, IDC_COMP_DICT, 64 * cKilo);
             SetComboBox(hwndDlg, IDC_COMP_WORD, 32);
             SetComboBox(hwndDlg, IDC_COMP_SOLID, 8 * cKilo);
           end;
         clFast:
           begin
             SetComboBox(hwndDlg, IDC_COMP_DICT, cMega);
             SetComboBox(hwndDlg, IDC_COMP_WORD, 32);
             SetComboBox(hwndDlg, IDC_COMP_SOLID, 128 * cKilo);
           end;
         clNormal:
           begin
             SetComboBox(hwndDlg, IDC_COMP_DICT, 16 * cMega);
             SetComboBox(hwndDlg, IDC_COMP_WORD, 32);
             SetComboBox(hwndDlg, IDC_COMP_SOLID, 2 * cMega);
           end;
         clMaximum:
           begin
             SetComboBox(hwndDlg, IDC_COMP_DICT, 32 * cMega);
             SetComboBox(hwndDlg, IDC_COMP_WORD, 64);
             SetComboBox(hwndDlg, IDC_COMP_SOLID, 4 * cMega);
           end;
         clUltra:
           begin
             SetComboBox(hwndDlg, IDC_COMP_DICT, 64 * cMega);
             SetComboBox(hwndDlg, IDC_COMP_WORD, 64);
             SetComboBox(hwndDlg, IDC_COMP_SOLID, 4 * cMega);
           end;
         end;
       end;
    cmPPMd:
       begin
         case Level of
          clFastest,
          clFast:
            begin
              SetComboBox(hwndDlg, IDC_COMP_DICT, 4 * cMega);
              SetComboBox(hwndDlg, IDC_COMP_WORD, 4);
              SetComboBox(hwndDlg, IDC_COMP_SOLID, 512 * cKilo);
            end;
         clNormal:
           begin
             SetComboBox(hwndDlg, IDC_COMP_DICT, 16 * cMega);
             SetComboBox(hwndDlg, IDC_COMP_WORD, 6);
             SetComboBox(hwndDlg, IDC_COMP_SOLID, 2 * cMega);
           end;
         clMaximum:
           begin
             SetComboBox(hwndDlg, IDC_COMP_DICT, 64 * cMega);
             SetComboBox(hwndDlg, IDC_COMP_WORD, 16);
             SetComboBox(hwndDlg, IDC_COMP_SOLID, 4 * cMega);
           end;
         clUltra:
           begin
             SetComboBox(hwndDlg, IDC_COMP_DICT, 192 * cMega);
             SetComboBox(hwndDlg, IDC_COMP_WORD, 16);
             SetComboBox(hwndDlg, IDC_COMP_SOLID, 4 * cMega);
           end;
         end;
       end;
    end;
  end;
  UpdateMemoryUsage(hwndDlg);
end;

procedure UpdateSolid(hwndDlg: HWND);
var
  Index: Integer;
  Format: TArchiveFormat;
  Level: TCompressionLevel;
begin
  gStartupInfo.SendDlgMsg(hwndDlg, IDC_COMP_SOLID, DM_LISTCLEAR, 0, 0);
  // Get compression level
  Level:= TCompressionLevel(GetComboBox(hwndDlg, IDC_COMP_LEVEL));
  Format:= TArchiveFormat(gStartupInfo.SendDlgMsg(hwndDlg, IDC_APPLY_BUTTON, DM_GETDLGDATA, 0, 0));
  if (Format in [afSevenZip]) and (Level <> clStore) then
  begin
    ComboBoxAdd(hwndDlg, IDC_COMP_SOLID, rsSolidBlockNonSolid, kNoSolidBlockSize);
    for Index:= Low(SolidBlock) to High(SolidBlock) do
    begin
      ComboBoxAdd(hwndDlg, IDC_COMP_SOLID, FormatFileSize(Int64(SolidBlock[Index]) * cKilo), PtrInt(SolidBlock[Index]));
    end;
    ComboBoxAdd(hwndDlg, IDC_COMP_SOLID, rsSolidBlockSolid, kSolidBlockSize);
  end;
end;

procedure UpdateThread(hwndDlg: HWND; dwAlgoThreadMax: LongWord);
var
  Index: LongWord;
  wsMaxThread: String;
  dwMaxThread: LongWord;
  dwDefaultValue: DWORD;
  dwHardwareThreads: DWORD;
begin
  gStartupInfo.SendDlgMsg(hwndDlg, IDC_COMP_THREAD, DM_LISTCLEAR, 0, 0);
  dwHardwareThreads:= GetNumberOfProcessors;
  dwDefaultValue:= dwHardwareThreads;
  dwMaxThread:= dwHardwareThreads * 2;
  if dwMaxThread > dwAlgoThreadMax then dwMaxThread:= dwAlgoThreadMax;
  if dwAlgoThreadMax < dwDefaultValue then dwDefaultValue:= dwAlgoThreadMax;
  for Index:= 1 to dwMaxThread do
  begin
    ComboBoxAdd(hwndDlg, IDC_COMP_THREAD, IntToStr(Index), Index);
  end;
  wsMaxThread:= '/ ' + IntToStr(dwHardwareThreads);
  gStartupInfo.SendDlgMsg(hwndDlg, IDC_COMP_THREAD, DM_LISTSETITEMINDEX, dwDefaultValue - 1, 0);
  SetDlgItemText(hwndDlg, IDC_MAX_THREAD, PAnsiChar(wsMaxThread));
end;

procedure UpdateMethod(hwndDlg: HWND);
var
  Index: PtrInt;
  Format: TArchiveFormat;
  dwAlgoThreadMax: LongWord = 1;
  Method: TJclCompressionMethod;
begin
  // Clear comboboxes
  gStartupInfo.SendDlgMsg(hwndDlg, IDC_COMP_DICT, DM_LISTCLEAR, 0, 0);
  gStartupInfo.SendDlgMsg(hwndDlg, IDC_COMP_WORD, DM_LISTCLEAR, 0, 0);
  Format:= TArchiveFormat(gStartupInfo.SendDlgMsg(hwndDlg, IDC_APPLY_BUTTON, DM_GETDLGDATA, 0, 0));
  gStartupInfo.SendDlgMsg(hwndDlg, IDC_COMP_DICT, DM_ENABLE, PtrInt(not (Format in [afTar, afWim])), 0);
  // Get Compression method
  Index:= GetComboBox(hwndDlg, IDC_COMP_METHOD);
  if Index > cmMaximum then
  begin
    dwAlgoThreadMax:= GetNumberOfProcessors;
    gStartupInfo.SendDlgMsg(hwndDlg, IDC_COMP_DICT, DM_ENABLE, PtrInt(False), 0);
    gStartupInfo.SendDlgMsg(hwndDlg, IDC_COMP_WORD, DM_ENABLE, PtrInt(False), 0);
  end
  else begin
    Method:= TJclCompressionMethod(Index);
    gStartupInfo.SendDlgMsg(hwndDlg, IDC_COMP_WORD, DM_ENABLE, PtrInt((Format in [afSevenZip, afGzip, afXz, afZip]) and (Method <> cmBZip2)), 0);
    case Method of
    cmDeflate:
      begin
        for Index:= Low(DeflateDict) to High(DeflateDict) do
        begin
          ComboBoxAdd(hwndDlg, IDC_COMP_DICT, FormatFileSize(DeflateDict[Index]), PtrInt(DeflateDict[Index]));
        end;
        for Index:= Low(DeflateWordSize) to High(DeflateWordSize) do
        begin
          ComboBoxAdd(hwndDlg, IDC_COMP_WORD, IntToStr(DeflateWordSize[Index]), PtrInt(DeflateWordSize[Index]));
        end;
      end;
    cmDeflate64:
      begin
        for Index:= Low(Deflate64Dict) to High(Deflate64Dict) do
        begin
          ComboBoxAdd(hwndDlg, IDC_COMP_DICT, FormatFileSize(Deflate64Dict[Index]), PtrInt(Deflate64Dict[Index]));
        end;
        for Index:= Low(Deflate64WordSize) to High(Deflate64WordSize) do
        begin
          ComboBoxAdd(hwndDlg, IDC_COMP_WORD, IntToStr(Deflate64WordSize[Index]), PtrInt(Deflate64WordSize[Index]));
        end;
      end;
    cmLZMA,
    cmLZMA2:
      begin
        for Index:= Low(LZMADict) to High(LZMADict) do
        begin
          ComboBoxAdd(hwndDlg, IDC_COMP_DICT, FormatFileSize(LZMADict[Index], False), PtrInt(LZMADict[Index]));
        end;
        for Index:= Low(LZMAWordSize) to High(LZMAWordSize) do
        begin
          ComboBoxAdd(hwndDlg, IDC_COMP_WORD, IntToStr(LZMAWordSize[Index]), PtrInt(LZMAWordSize[Index]));
        end;
        dwAlgoThreadMax:= IfThen(Method = cmLZMA, 2, 32);
      end;
    cmBZip2:
      begin
        for Index:= Low(BZip2Dict) to High(BZip2Dict) do
        begin
          ComboBoxAdd(hwndDlg, IDC_COMP_DICT, FormatFileSize(BZip2Dict[Index]), PtrInt(BZip2Dict[Index]));
        end;
        dwAlgoThreadMax:= 32;
      end;
    cmPPMd:
      begin
        for Index:= Low(PPMdDict) to High(PPMdDict) do
        begin
          ComboBoxAdd(hwndDlg, IDC_COMP_DICT, FormatFileSize(PPMdDict[Index], False), PtrInt(PPMdDict[Index]));
        end;
        for Index:= Low(PPMdWordSize) to High(PPMdWordSize) do
        begin
          ComboBoxAdd(hwndDlg, IDC_COMP_WORD, IntToStr(PPMdWordSize[Index]), PtrInt(PPMdWordSize[Index]));
        end;
      end;
    end;
    if Format = afZip then dwAlgoThreadMax:= 128;
  end;
  UpdateThread(hwndDlg, dwAlgoThreadMax);
end;

procedure FillMethod(hwndDlg: HWND);
var
  Index: Integer;
  Format: TArchiveFormat;
begin
  // Clear combobox
  gStartupInfo.SendDlgMsg(hwndDlg, IDC_COMP_METHOD, DM_LISTCLEAR, 0, 0);
  Format:= TArchiveFormat(gStartupInfo.SendDlgMsg(hwndDlg, IDC_APPLY_BUTTON, DM_GETDLGDATA, 0, 0));
  case Format of
  afSevenZip:
   begin
     // Fill compression method
     ComboBoxAdd(hwndDlg, IDC_COMP_METHOD, 'LZMA', PtrInt(cmLZMA));
     ComboBoxAdd(hwndDlg, IDC_COMP_METHOD, 'LZMA2', PtrInt(cmLZMA2));
     ComboBoxAdd(hwndDlg, IDC_COMP_METHOD, 'PPMd', PtrInt(cmPPMd));
     ComboBoxAdd(hwndDlg, IDC_COMP_METHOD, 'BZip2', PtrInt(cmBZip2));
     if Assigned(ACodecs) then begin
       for Index:= 0 to ACodecs.Count - 1 do begin
         ComboBoxAdd(hwndDlg, IDC_COMP_METHOD, UTF8Encode(ACodecs[Index].Name), PtrInt(ACodecs[Index].ID));
       end;
     end;
     SetComboBox(hwndDlg, IDC_COMP_METHOD, PluginConfig[Format].Method);
   end;
  afBzip2:
   begin
     // Fill compression method
     ComboBoxAdd(hwndDlg, IDC_COMP_METHOD, 'BZip2', PtrInt(cmBZip2));
     gStartupInfo.SendDlgMsg(hwndDlg, IDC_COMP_METHOD, DM_LISTSETITEMINDEX, 0, 0);
   end;
  afGzip:
   begin
     // Fill compression method
     ComboBoxAdd(hwndDlg, IDC_COMP_METHOD, 'Deflate', PtrInt(cmDeflate));
     gStartupInfo.SendDlgMsg(hwndDlg, IDC_COMP_METHOD, DM_LISTSETITEMINDEX, 0, 0);
   end;
  afXz:
   begin
     // Fill compression method
     ComboBoxAdd(hwndDlg, IDC_COMP_METHOD, 'LZMA2', PtrInt(cmLZMA2));
     gStartupInfo.SendDlgMsg(hwndDlg, IDC_COMP_METHOD, DM_LISTSETITEMINDEX, 0, 0);
   end;
  afZip:
   begin
     // Fill compression method
     ComboBoxAdd(hwndDlg, IDC_COMP_METHOD, 'Deflate', PtrInt(cmDeflate));
     ComboBoxAdd(hwndDlg, IDC_COMP_METHOD, 'Deflate64', PtrInt(cmDeflate64));
     ComboBoxAdd(hwndDlg, IDC_COMP_METHOD, 'BZip2', PtrInt(cmBZip2));
     ComboBoxAdd(hwndDlg, IDC_COMP_METHOD, 'LZMA', PtrInt(cmLZMA));
     ComboBoxAdd(hwndDlg, IDC_COMP_METHOD, 'PPMd', PtrInt(cmPPMd));
   end;
  end;
end;

procedure UpdateFormat(hwndDlg: HWND);
var
  Format: TArchiveFormat;
begin
  // Clear comboboxes
  gStartupInfo.SendDlgMsg(hwndDlg, IDC_COMP_LEVEL, DM_LISTCLEAR, 0, 0);

  // Get archive format
  Format:= TArchiveFormat(GetComboBox(hwndDlg, IDC_COMP_FORMAT));
  gStartupInfo.SendDlgMsg(hwndDlg, IDC_APPLY_BUTTON, DM_SETDLGDATA, PtrInt(Format), 0);
  gStartupInfo.SendDlgMsg(hwndDlg, IDC_COMP_SOLID, DM_ENABLE, PtrInt(Format = afSevenZip), 0);
  // 7Zip and Zip
  if Format in [afSevenZip, afZip] then
  begin
    // Fill compression level
    ComboBoxAdd(hwndDlg, IDC_COMP_LEVEL, rsCompressionLevelStore, PtrInt(clStore));
    ComboBoxAdd(hwndDlg, IDC_COMP_LEVEL, rsCompressionLevelFastest, PtrInt(clFastest));
    ComboBoxAdd(hwndDlg, IDC_COMP_LEVEL, rsCompressionLevelFast, PtrInt(clFast));
    ComboBoxAdd(hwndDlg, IDC_COMP_LEVEL, rsCompressionLevelNormal, PtrInt(clNormal));
    ComboBoxAdd(hwndDlg, IDC_COMP_LEVEL, rsCompressionLevelMaximum, PtrInt(clMaximum));
    ComboBoxAdd(hwndDlg, IDC_COMP_LEVEL, rsCompressionLevelUltra, PtrInt(clUltra));
  end
  else if Format in [afBzip2, afXz] then
  begin
    // Fill compression level
    ComboBoxAdd(hwndDlg, IDC_COMP_LEVEL, rsCompressionLevelFastest, PtrInt(clFastest));
    ComboBoxAdd(hwndDlg, IDC_COMP_LEVEL, rsCompressionLevelFast, PtrInt(clFast));
    ComboBoxAdd(hwndDlg, IDC_COMP_LEVEL, rsCompressionLevelNormal, PtrInt(clNormal));
    ComboBoxAdd(hwndDlg, IDC_COMP_LEVEL, rsCompressionLevelMaximum, PtrInt(clMaximum));
    ComboBoxAdd(hwndDlg, IDC_COMP_LEVEL, rsCompressionLevelUltra, PtrInt(clUltra));
  end
  else if Format in [afGzip] then
  begin
    // Fill compression level
    ComboBoxAdd(hwndDlg, IDC_COMP_LEVEL, rsCompressionLevelFast, PtrInt(clFast));
    ComboBoxAdd(hwndDlg, IDC_COMP_LEVEL, rsCompressionLevelNormal, PtrInt(clNormal));
    ComboBoxAdd(hwndDlg, IDC_COMP_LEVEL, rsCompressionLevelMaximum, PtrInt(clMaximum));
    ComboBoxAdd(hwndDlg, IDC_COMP_LEVEL, rsCompressionLevelUltra, PtrInt(clUltra));
  end
  else begin
    // Fill compression level
    ComboBoxAdd(hwndDlg, IDC_COMP_LEVEL, rsCompressionLevelStore, PtrInt(clStore));
  end;
  FillMethod(hwndDlg);
end;

procedure UpdateLevel(hwndDlg: HWND; First: Boolean);
var
  MethodStd: Boolean;
  Format: TArchiveFormat;
  Level: TCompressionLevel;
begin
  Format:= TArchiveFormat(gStartupInfo.SendDlgMsg(hwndDlg, IDC_APPLY_BUTTON, DM_GETDLGDATA, 0, 0));
  // Get compression level
  Level:= TCompressionLevel(GetComboBox(hwndDlg, IDC_COMP_LEVEL));
  // Get compression method
  MethodStd:= (GetComboBox(hwndDlg, IDC_COMP_METHOD) <= cmMaximum);

  gStartupInfo.SendDlgMsg(hwndDlg, IDC_COMP_DICT, DM_ENABLE, PtrInt((Level <> clStore) and MethodStd), 0);
  gStartupInfo.SendDlgMsg(hwndDlg, IDC_COMP_WORD, DM_ENABLE, PtrInt((Level <> clStore) and MethodStd), 0);
  gStartupInfo.SendDlgMsg(hwndDlg, IDC_COMP_SOLID, DM_ENABLE, PtrInt((Format = afSevenZip) and (Level <> clStore)), 0);

  if Level = clStore then
  begin
    gStartupInfo.SendDlgMsg(hwndDlg, IDC_COMP_METHOD, DM_LISTCLEAR, 0, 0);
    gStartupInfo.SendDlgMsg(hwndDlg, IDC_COMP_DICT, DM_LISTCLEAR, 0, 0);
    gStartupInfo.SendDlgMsg(hwndDlg, IDC_COMP_WORD, DM_LISTCLEAR, 0, 0);
    gStartupInfo.SendDlgMsg(hwndDlg, IDC_COMP_SOLID, DM_LISTCLEAR, 0, 0);
    ComboBoxAdd(hwndDlg, IDC_COMP_METHOD, UTF8Encode(MethodName[cmCopy]), PtrInt(cmCopy));
    gStartupInfo.SendDlgMsg(hwndDlg, IDC_COMP_METHOD, DM_LISTSETITEMINDEX, 0, 0);
    UpdateThread(hwndDlg, 1);
  end
  else if not First then
  begin
    FillMethod(hwndDlg);
    PluginConfig[Format].Method:= DefaultConfig[Format].Method;
    SetComboBox(hwndDlg, IDC_COMP_METHOD, PluginConfig[Format].Method);
    UpdateMethod(hwndDlg);
    UpdateSolid(hwndDlg);
    gStartupInfo.SendDlgMsg(hwndDlg, IDC_COMP_SOLID, DM_ENABLE, PtrInt(Format = afSevenZip), 0);
  end;
end;

procedure SelectFormat(hwndDlg: HWND);
var
  Format: TArchiveFormat;
begin
  UpdateFormat(hwndDlg);
  Format:= TArchiveFormat(gStartupInfo.SendDlgMsg(hwndDlg, IDC_APPLY_BUTTON, DM_GETDLGDATA, 0, 0));
  SetComboBox(hwndDlg, IDC_COMP_LEVEL, PluginConfig[Format].Level);
  SetComboBox(hwndDlg, IDC_COMP_METHOD, PluginConfig[Format].Method);
  UpdateMethod(hwndDlg);
  UpdateLevel(hwndDlg, True);
  UpdateSolid(hwndDlg);
  SetComboBox(hwndDlg, IDC_COMP_DICT, PluginConfig[Format].Dictionary);
  SetComboBox(hwndDlg, IDC_COMP_WORD, PluginConfig[Format].WordSize);
  SetComboBox(hwndDlg, IDC_COMP_SOLID, PluginConfig[Format].SolidSize);
  SetComboBox(hwndDlg, IDC_COMP_THREAD, PluginConfig[Format].ThreadCount);
  SetDlgItemText(hwndDlg, IDC_PARAMETERS, PAnsiChar(Utf16ToUtf8(PluginConfig[Format].Parameters)));
  UpdateMemoryUsage(hwndDlg);
end;

function DlgProc(hwndDlg: PtrUInt; DlgItemName: PAnsiChar; Msg, wParam, lParam: PtrInt): PtrInt; winapi;
var
  Index: TArchiveFormat;
begin
  Result:= 0;
  case Msg of
    DN_INITDIALOG:
    begin
      for Index:= Low(ArchiveExtension) to High(ArchiveExtension) do
      begin
        ComboBoxAdd(hwndDlg, IDC_COMP_FORMAT, UTF8Encode(ArchiveExtension[Index]), PtrInt(Index));
      end;
      gStartupInfo.SendDlgMsg(hwndDlg, IDC_COMP_FORMAT, DM_LISTSETITEMINDEX, 0, 0);
      gStartupInfo.SendDlgMsg(hwndDlg, IDC_APPLY_BUTTON, DM_SETDLGDATA, PtrInt(afSevenZip), 0);
      SelectFormat(hwndDlg);
      Result:= 1;
    end;
    DN_CHANGE:
    begin
      if DlgItemName = IDC_COMP_FORMAT then
        begin
          SelectFormat(hwndDlg);
        end
      else if DlgItemName = IDC_COMP_METHOD then
        begin
          UpdateMethod(hwndDlg);
          SetDefaultOptions(hwndDlg);
        end
      else if DlgItemName = IDC_COMP_LEVEL then
        begin
          UpdateLevel(hwndDlg, False);
          SetDefaultOptions(hwndDlg);
        end
      else if (DlgItemName = IDC_COMP_DICT) or
              (DlgItemName = IDC_COMP_WORD) or
              (DlgItemName = IDC_COMP_THREAD) then
        begin
          UpdateMemoryUsage(hwndDlg);
        end;
      end;
    DN_CLICK:
    begin
      if DlgItemName = 'btnOK' then
        begin
          SaveArchiver(hwndDlg);
          gStartupInfo.SendDlgMsg(hwndDlg, nil, DM_CLOSE, 1, 0);
        end;
      if DlgItemName = 'btnCancel' then
        gStartupInfo.SendDlgMsg(hwndDlg, nil, DM_CLOSE, 2, 0);
      if DlgItemName = IDC_APPLY_BUTTON then
        SaveArchiver(hwndDlg);
      end;
    else begin
      Result:= 0;
    end;
  end;
end;

function PasswordDialog(hwndDlg: PtrUInt; DlgItemName: PAnsiChar; Msg, wParam, lParam: PtrInt): PtrInt; winapi;
var
  Data: PtrInt;
  AText: PAnsiChar absolute Data;
  PasswordData: PPasswordData absolute lParam;
begin
  Result:= 0;
  case Msg of
    DN_INITDIALOG:
    begin
      lParam:= gStartupInfo.SendDlgMsg(hwndDlg, nil, DM_GETDLGDATA, 0, 0);
      AText:= PAnsiChar(PasswordData^.Password);
      gStartupInfo.SendDlgMsg(hwndDlg, IDC_PASSWORD, DM_SETTEXT, Data, 0);
      gStartupInfo.SendDlgMsg(hwndDlg, IDC_PASSWORD, DM_SETMAXTEXTLENGTH, MAX_PATH, 0);
      gStartupInfo.SendDlgMsg(hwndDlg, IDC_ENCRYPT_HEADER, DM_ENABLE, PtrInt(PasswordData^.EncryptHeader), 0)
    end;
    DN_CHANGE:
    begin
      if DlgItemName = IDC_SHOW_PASSWORD then
      begin
        Data:= gStartupInfo.SendDlgMsg(hwndDlg, IDC_SHOW_PASSWORD, DM_GETCHECK, 0, 0);
        wParam:= IfThen(Data <> 0, 0, Ord('*'));
        gStartupInfo.SendDlgMsg(hwndDlg, IDC_PASSWORD, DM_SETPASSWORDCHAR, wParam, 0);
      end;
    end;
    DN_CLICK:
    begin
      if DlgItemName = 'btnOK' then
        begin
          lParam:= gStartupInfo.SendDlgMsg(hwndDlg, nil, DM_GETDLGDATA, 0, 0);
          PasswordData^.EncryptHeader:= gStartupInfo.SendDlgMsg(hwndDlg, IDC_ENCRYPT_HEADER, DM_GETCHECK, 0, 0) <> 0;
          Data:= gStartupInfo.SendDlgMsg(hwndDlg, IDC_PASSWORD, DM_GETTEXT, 0, 0);
          PasswordData^.Password:= AText;
          gStartupInfo.SendDlgMsg(hwndDlg, nil, DM_CLOSE, 1, 0);
        end
      else if DlgItemName = 'btnCancel' then
      begin
        gStartupInfo.SendDlgMsg(hwndDlg, nil, DM_CLOSE, 2, 0);
      end;
    end
    else begin
      Result:= 0;
    end;
  end;
end;

function ShowPasswordQuery(var Encrypt: Boolean; var Password: WideString): Boolean;
var
  ResData: Pointer;
  ResSize: LongWord;
  PasswordData: TPasswordData;
  ResHandle: TFPResourceHandle;
  ResGlobal: TFPResourceHGLOBAL;
begin
  ResHandle := FindResource(HINSTANCE, PAnsiChar('TPasswordBox'), MAKEINTRESOURCE(10) {RT_RCDATA});
  if ResHandle <> 0 then
  begin
    ResGlobal := LoadResource(HINSTANCE, ResHandle);
    if ResGlobal <> 0 then
    try
      ResData := LockResource(ResGlobal);
      ResSize := SizeofResource(HINSTANCE, ResHandle);

      with gStartupInfo do
      begin
        PasswordData.EncryptHeader:= Encrypt;
        PasswordData.Password:= UTF16ToUTF8(Password);

        Result:= DialogBoxParam(ResData, ResSize, @PasswordDialog, DB_LRS, @PasswordData, nil) <> 0;

        if Result then
        begin
          Encrypt:= PasswordData.EncryptHeader;
          Password:= UTF8ToUTF16(PasswordData.Password);
        end;
      end;
    finally
      UnlockResource(ResGlobal);
      FreeResource(ResGlobal);
    end;
  end;
end;

procedure ShowConfigurationDialog(Parent: HWND);
var
  ResSize: LongWord;
  ResData: Pointer = nil;
  ResHandle: TFPResourceHandle;
  ResGlobal: TFPResourceHGLOBAL;
begin
  ResHandle := FindResource(HINSTANCE, PChar('TDIALOGBOX'), MAKEINTRESOURCE(10) {RT_RCDATA});
  if ResHandle <> 0 then
  begin
    ResGlobal := LoadResource(HINSTANCE, ResHandle);
    if ResGlobal <> 0 then
    try
      ResData := LockResource(ResGlobal);
      ResSize := SizeofResource(HINSTANCE, ResHandle);

      with gStartupInfo do
      begin
        DialogBoxLRS(ResData, ResSize, @DlgProc);
      end;
    finally
      UnlockResource(ResGlobal);
      FreeResource(ResGlobal);
    end;
  end;
end;

procedure DialogInitialize(StartupInfo: PExtensionStartupInfo);
begin
  gStartupInfo:= StartupInfo^;
  MessageBoxFunction:= gStartupInfo.MessageBox;
end;

end.

