{
  Double Commander
  -------------------------------------------------------------------------
  SevenZip archiver plugin, dialogs unit

  Copyright (C) 2014-2017 Alexander Koblov (alexx2000@mail.ru)

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
  Classes, SysUtils, Windows, Math, SevenZipOpt, SevenZipLng, JclCompression;

procedure ShowConfigurationDialog(Parent: HWND);
function ShowPasswordQuery(var Encrypt: Boolean; var Password: WideString): Boolean;

implementation

uses
  LazUTF8, SevenZipCodecs;

{$R *.res}

const
  IDC_PASSWORD = 1001;
  IDC_SHOW_PASSWORD = 1002;
  IDC_ENCRYPT_HEADER = 0;

const
  IDC_APPLY_BUTTON = 9;
  IDC_COMP_FORMAT = 1076;
  IDC_COMP_METHOD = 1078;
  IDC_COMP_LEVEL = 1074;
  IDC_VOLUME_SIZE = 1077;
  IDC_COMP_DICT = 1079;
  IDC_COMP_WORD = 1080;
  IDC_COMP_SOLID = 1081;
  IDC_COMP_THREAD = 1082;
  IDC_MAX_THREAD = 1083;
  IDC_PARAMETERS = 1091;
  IDC_MEMORY_COMP = 1027;
  IDC_MEMORY_DECOMP = 1028;

function GetComboBox(hwndDlg: HWND; ItemID: Integer): PtrInt;
var
  Index: Integer;
begin
  Index:= SendDlgItemMessage(hwndDlg, ItemID, CB_GETCURSEL, 0, 0);
  Result:= SendDlgItemMessage(hwndDlg, ItemID, CB_GETITEMDATA, Index, 0);
end;

procedure SetComboBox(hwndDlg: HWND; ItemID: Integer; ItemData: PtrInt);
var
  Index, Count: Integer;
begin
  Count:= SendDlgItemMessage(hwndDlg, ItemID, CB_GETCOUNT, 0, 0);
  for Index:= 0 to Count - 1 do
  begin
    if SendDlgItemMessage(hwndDlg, ItemID, CB_GETITEMDATA, Index, 0) = ItemData then
    begin
      SendDlgItemMessage(hwndDlg, ItemID, CB_SETCURSEL, Index, 0);
      Exit;
    end;
  end;
end;

procedure SaveArchiver(hwndDlg: HWND);
var
  Format: TArchiveFormat;
  Parameters: array[0..MAX_PATH] of WideChar;
begin
  Format:= TArchiveFormat(GetWindowLongPtr(hwndDlg, GWLP_USERDATA));
  PluginConfig[Format].Level:= GetComboBox(hwndDlg, IDC_COMP_LEVEL);
  PluginConfig[Format].Method:= GetComboBox(hwndDlg, IDC_COMP_METHOD);
  if PluginConfig[Format].Level <> PtrInt(clStore) then
  begin
    PluginConfig[Format].Dictionary:= GetComboBox(hwndDlg, IDC_COMP_DICT);
    PluginConfig[Format].WordSize:= GetComboBox(hwndDlg, IDC_COMP_WORD);
    PluginConfig[Format].SolidSize:= GetComboBox(hwndDlg, IDC_COMP_SOLID);
    PluginConfig[Format].ThreadCount:= GetComboBox(hwndDlg, IDC_COMP_THREAD);
  end;
  GetDlgItemTextW(hwndDlg, IDC_PARAMETERS, Parameters, MAX_PATH);
  PluginConfig[Format].Parameters:= Parameters;

  SaveConfiguration;
end;

function ComboBoxAdd(hwndDlg: HWND; ItemID: Integer; ItemText: String; ItemData: PtrInt): Integer;
var
  Text: UnicodeString;
begin
  Text:= UTF8ToUTF16(ItemText);
  Result:= SendDlgItemMessageW(hwndDlg, ItemID, CB_ADDSTRING, 0, LPARAM(PWideChar(Text)));
  SendDlgItemMessage(hwndDlg, ItemID, CB_SETITEMDATA, Result, ItemData);
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
         SendDlgItemMessage(hwndDlg, IDC_COMP_DICT, CB_SETCURSEL, 0, 0);
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
  SendDlgItemMessage(hwndDlg, IDC_COMP_SOLID, CB_RESETCONTENT, 0, 0);
  // Get compression level
  Level:= TCompressionLevel(GetComboBox(hwndDlg, IDC_COMP_LEVEL));
  Format:= TArchiveFormat(GetWindowLongPtr(hwndDlg, GWLP_USERDATA));
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
  dwMaxThread: LongWord;
  dwDefaultValue: DWORD;
  wsMaxThread: WideString;
  dwHardwareThreads: DWORD;
begin
  SendDlgItemMessage(hwndDlg, IDC_COMP_THREAD, CB_RESETCONTENT, 0, 0);
  dwHardwareThreads:= GetNumberOfProcessors;
  dwDefaultValue:= dwHardwareThreads;
  dwMaxThread:= dwHardwareThreads * 2;
  if dwMaxThread > dwAlgoThreadMax then dwMaxThread:= dwAlgoThreadMax;
  if dwAlgoThreadMax < dwDefaultValue then dwDefaultValue:= dwAlgoThreadMax;
  for Index:= 1 to dwMaxThread do
  begin
    ComboBoxAdd(hwndDlg, IDC_COMP_THREAD, IntToStr(Index), Index);
  end;
  wsMaxThread:= '/ ' + WideString(IntToStr(dwHardwareThreads));
  SendDlgItemMessage(hwndDlg, IDC_COMP_THREAD, CB_SETCURSEL, dwDefaultValue - 1, 0);
  SendDlgItemMessageW(hwndDlg, IDC_MAX_THREAD, WM_SETTEXT, 0, LPARAM(PWideChar(wsMaxThread)));
end;

procedure UpdateMethod(hwndDlg: HWND);
var
  Index: PtrInt;
  Format: TArchiveFormat;
  dwAlgoThreadMax: LongWord = 1;
  Method: TJclCompressionMethod;
begin
  // Clear comboboxes
  SendDlgItemMessage(hwndDlg, IDC_COMP_DICT, CB_RESETCONTENT, 0, 0);
  SendDlgItemMessage(hwndDlg, IDC_COMP_WORD, CB_RESETCONTENT, 0, 0);
  Format:= TArchiveFormat(GetWindowLongPtr(hwndDlg, GWLP_USERDATA));
  EnableWindow(GetDlgItem(hwndDlg, IDC_COMP_DICT), not (Format in [afTar, afWim]));
  // Get Compression method
  Index:= GetComboBox(hwndDlg, IDC_COMP_METHOD);
  if Index > cmMaximum then
  begin
    dwAlgoThreadMax:= GetNumberOfProcessors;
    EnableWindow(GetDlgItem(hwndDlg, IDC_COMP_DICT), False);
    EnableWindow(GetDlgItem(hwndDlg, IDC_COMP_WORD), False);
  end
  else begin
    Method:= TJclCompressionMethod(Index);
    EnableWindow(GetDlgItem(hwndDlg, IDC_COMP_WORD), (Format in [afSevenZip, afGzip, afXz, afZip]) and (Method <> cmBZip2));
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
  SendDlgItemMessage(hwndDlg, IDC_COMP_METHOD, CB_RESETCONTENT, 0, 0);
  Format:= TArchiveFormat(GetWindowLongPtr(hwndDlg, GWLP_USERDATA));
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
         ComboBoxAdd(hwndDlg, IDC_COMP_METHOD, ACodecs[Index].Name, PtrInt(ACodecs[Index].ID));
       end;
     end;
     SetComboBox(hwndDlg, IDC_COMP_METHOD, PluginConfig[Format].Method);
   end;
  afBzip2:
   begin
     // Fill compression method
     ComboBoxAdd(hwndDlg, IDC_COMP_METHOD, 'BZip2', PtrInt(cmBZip2));
     SendDlgItemMessage(hwndDlg, IDC_COMP_METHOD, CB_SETCURSEL, 0, 0);
   end;
  afGzip:
   begin
     // Fill compression method
     ComboBoxAdd(hwndDlg, IDC_COMP_METHOD, 'Deflate', PtrInt(cmDeflate));
     SendDlgItemMessage(hwndDlg, IDC_COMP_METHOD, CB_SETCURSEL, 0, 0);
   end;
  afXz:
   begin
     // Fill compression method
     ComboBoxAdd(hwndDlg, IDC_COMP_METHOD, 'LZMA2', PtrInt(cmLZMA2));
     SendDlgItemMessage(hwndDlg, IDC_COMP_METHOD, CB_SETCURSEL, 0, 0);
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
  SendDlgItemMessage(hwndDlg, IDC_COMP_LEVEL, CB_RESETCONTENT, 0, 0);
  // Get archive format
  Format:= TArchiveFormat(GetComboBox(hwndDlg, IDC_COMP_FORMAT));
  SetWindowLongPtr(hwndDlg, GWLP_USERDATA, LONG_PTR(Format));
  EnableWindow(GetDlgItem(hwndDlg, IDC_COMP_SOLID), Format = afSevenZip);
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
  Format:= TArchiveFormat(GetWindowLongPtr(hwndDlg, GWLP_USERDATA));
  // Get compression level
  Level:= TCompressionLevel(GetComboBox(hwndDlg, IDC_COMP_LEVEL));
  // Get compression method
  MethodStd:= (GetComboBox(hwndDlg, IDC_COMP_METHOD) <= cmMaximum);

  EnableWindow(GetDlgItem(hwndDlg, IDC_COMP_DICT), (Level <> clStore) and MethodStd);
  EnableWindow(GetDlgItem(hwndDlg, IDC_COMP_WORD), (Level <> clStore) and MethodStd);
  EnableWindow(GetDlgItem(hwndDlg, IDC_COMP_SOLID), (Format = afSevenZip) and (Level <> clStore));

  if Level = clStore then
  begin
    SendDlgItemMessage(hwndDlg, IDC_COMP_METHOD, CB_RESETCONTENT, 0, 0);
    SendDlgItemMessage(hwndDlg, IDC_COMP_DICT, CB_RESETCONTENT, 0, 0);
    SendDlgItemMessage(hwndDlg, IDC_COMP_WORD, CB_RESETCONTENT, 0, 0);
    SendDlgItemMessage(hwndDlg, IDC_COMP_SOLID, CB_RESETCONTENT, 0, 0);
    ComboBoxAdd(hwndDlg, IDC_COMP_METHOD, MethodName[cmCopy], PtrInt(cmCopy));
    SendDlgItemMessage(hwndDlg, IDC_COMP_METHOD, CB_SETCURSEL, 0, 0);
    UpdateThread(hwndDlg, 1);
  end
  else if not First then
  begin
    FillMethod(hwndDlg);
    PluginConfig[Format].Method:= DefaultConfig[Format].Method;
    SetComboBox(hwndDlg, IDC_COMP_METHOD, PluginConfig[Format].Method);
    UpdateMethod(hwndDlg);
    UpdateSolid(hwndDlg);
    EnableWindow(GetDlgItem(hwndDlg, IDC_COMP_SOLID), Format = afSevenZip);
  end;
end;

procedure SelectFormat(hwndDlg: HWND);
var
  Format: TArchiveFormat;
begin
  UpdateFormat(hwndDlg);
  Format:= TArchiveFormat(GetWindowLongPtr(hwndDlg, GWLP_USERDATA));
  SetComboBox(hwndDlg, IDC_COMP_LEVEL, PluginConfig[Format].Level);
  SetComboBox(hwndDlg, IDC_COMP_METHOD, PluginConfig[Format].Method);
  UpdateMethod(hwndDlg);
  UpdateLevel(hwndDlg, True);
  UpdateSolid(hwndDlg);
  SetComboBox(hwndDlg, IDC_COMP_DICT, PluginConfig[Format].Dictionary);
  SetComboBox(hwndDlg, IDC_COMP_WORD, PluginConfig[Format].WordSize);
  SetComboBox(hwndDlg, IDC_COMP_SOLID, PluginConfig[Format].SolidSize);
  SetComboBox(hwndDlg, IDC_COMP_THREAD, PluginConfig[Format].ThreadCount);
  SetDlgItemTextW(hwndDlg, IDC_PARAMETERS, PWideChar(PluginConfig[Format].Parameters));
  UpdateMemoryUsage(hwndDlg);
end;

function DialogProc(hwndDlg: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): INT_PTR; stdcall;
var
  Index: TArchiveFormat;
begin
  case uMsg of
    WM_INITDIALOG:
    begin
      EnableWindow(GetDlgItem(hwndDlg, IDC_VOLUME_SIZE), False);
      for Index:= Low(ArchiveExtension) to High(ArchiveExtension) do
      ComboBoxAdd(hwndDlg, IDC_COMP_FORMAT, ArchiveExtension[Index], PtrInt(Index));
      SendDlgItemMessage(hwndDlg, IDC_COMP_FORMAT, CB_SETCURSEL, 0, 0);
      SelectFormat(hwndDlg);
      Result:= 1;
    end;
    WM_COMMAND:
    begin
      case LOWORD(wParam) of
      IDC_COMP_FORMAT:
        begin
          if (HIWORD(wParam) = CBN_SELCHANGE) then
          begin
            SelectFormat(hwndDlg);
          end;
        end;
      IDC_COMP_METHOD:
        if (HIWORD(wParam) = CBN_SELCHANGE) then
        begin
          UpdateMethod(hwndDlg);
          SetDefaultOptions(hwndDlg);
        end;
      IDC_COMP_LEVEL:
        if (HIWORD(wParam) = CBN_SELCHANGE) then
        begin
          UpdateLevel(hwndDlg, False);
          SetDefaultOptions(hwndDlg);
        end;
      IDC_COMP_DICT,
      IDC_COMP_WORD,
      IDC_COMP_THREAD:
        if (HIWORD(wParam) = CBN_SELCHANGE) then
        begin
          UpdateMemoryUsage(hwndDlg);
        end;
      IDOK:
        begin
          SaveArchiver(hwndDlg);
          EndDialog(hwndDlg, IDOK);
        end;
      IDCANCEL:
        EndDialog(hwndDlg, IDCANCEL);
      IDC_APPLY_BUTTON:
        SaveArchiver(hwndDlg);
      end;
    end;
    WM_CLOSE:
    begin
      EndDialog(hwndDlg, 0);
    end
    else begin
      Result:= 0;
    end;
  end;
end;

function PasswordDialog(hwndDlg: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): INT_PTR; stdcall;
var
  PasswordData: PPasswordData;
begin
  PasswordData:= PPasswordData(GetWindowLongPtr(hwndDlg, GWLP_USERDATA));
  case uMsg of
    WM_INITDIALOG:
    begin
      PasswordData:= PPasswordData(lParam);
      SetWindowLongPtr(hwndDlg, GWLP_USERDATA, LONG_PTR(lParam));
      SetDlgItemTextW(hwndDlg, IDC_PASSWORD, PasswordData^.Password);
      SendDlgItemMessage(hwndDlg, IDC_PASSWORD, EM_SETLIMITTEXT, MAX_PATH, 0);
      EnableWindow(GetDlgItem(hwndDlg, IDC_ENCRYPT_HEADER), PasswordData^.EncryptHeader);
      Exit(1);
    end;
    WM_COMMAND:
    begin
      case LOWORD(wParam) of
       IDOK:
       begin
         PasswordData^.EncryptHeader:= IsDlgButtonChecked(hwndDlg, IDC_ENCRYPT_HEADER) <> 0;
         GetDlgItemTextW(hwndDlg, IDC_PASSWORD, PasswordData^.Password, MAX_PATH);
   	 EndDialog(hwndDlg, IDOK);
       end;
       IDCANCEL:
         EndDialog(hwndDlg, IDCANCEL);
       IDC_SHOW_PASSWORD:
       begin
         wParam:= (not IsDlgButtonChecked(hwndDlg, IDC_SHOW_PASSWORD) and $01) * $2A;
      	 SendDlgItemMessageW(hwndDlg, IDC_PASSWORD, EM_SETPASSWORDCHAR, wParam, 0);
       end;
      end;
    end;
  end;
  Result:= 0;
end;

function ShowPasswordQuery(var Encrypt: Boolean; var Password: WideString): Boolean;
var
  PasswordData: TPasswordData;
begin
  PasswordData.Password:= Password;
  PasswordData.EncryptHeader:= Encrypt;
  Result:= (DialogBoxParam(hInstance, 'DIALOG_PWD', 0, @PasswordDialog, LPARAM(@PasswordData)) = IDOK);
  if Result then
  begin
    Password:= PasswordData.Password;
    Encrypt:= PasswordData.EncryptHeader;
  end;
end;

procedure ShowConfigurationDialog(Parent: HWND);
begin
  DialogBox(hInstance, 'DIALOG_CFG', Parent, @DialogProc);
end;

end.

