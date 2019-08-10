unit uImport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows;

function FindImportLibrary(hModule: THandle; pLibName: PAnsiChar): PPointer;
function FindImportFunction(pLibrary: PPointer; pFunction: Pointer): PPointer;
function ReplaceImportFunction(pOldFunction: PPointer; pNewFunction: Pointer): Pointer;

implementation

type
{$IFDEF WIN64}
  PIMAGE_NT_HEADERS = PIMAGE_NT_HEADERS64;
{$ELSE}
  PIMAGE_NT_HEADERS = PIMAGE_NT_HEADERS32;
{$ENDIF}

function FindImageDirectory(hModule: THandle; Index: Integer; out DataDir: PIMAGE_DATA_DIRECTORY): Pointer;
var
  pNTHeaders: PIMAGE_NT_HEADERS;
  pModule: PByte absolute hModule;
  pDosHeader: PIMAGE_DOS_HEADER absolute hModule;
begin
  if pDosHeader^.e_magic = IMAGE_DOS_SIGNATURE then
  begin
    pNTHeaders := @pModule[pDosHeader^.e_lfanew];
    if pNTHeaders^.Signature = IMAGE_NT_SIGNATURE then
    begin
      DataDir := @pNTHeaders^.OptionalHeader.DataDirectory[Index];
      Result := @pModule[DataDir^.VirtualAddress];
      Exit;
    end;
  end;
  Result := nil;
end;

function FindImportLibrary(hModule: THandle; pLibName: PAnsiChar): PPointer;
var
  pEnd: PByte;
  pImpDir: PIMAGE_DATA_DIRECTORY;
  pImpDesc: PIMAGE_IMPORT_DESCRIPTOR;
  pModule: PAnsiChar absolute hModule;
begin
  pImpDesc := FindImageDirectory(hModule, IMAGE_DIRECTORY_ENTRY_IMPORT, pImpDir);
  if pImpDesc = nil then Exit(nil);

  pEnd := PByte(pImpDesc) + pImpDir^.Size;

  while (PByte(pImpDesc) < pEnd) and (pImpDesc^.FirstThunk <> 0) do
  begin
    if StrComp(@pModule[pImpDesc^.Name], pLibName) = 0 then
    begin
      Result := @pModule[pImpDesc^.FirstThunk];
      Exit;
    end;
    Inc(pImpDesc);
  end;
  Result := nil;
end;

function FindImportFunction(pLibrary: PPointer; pFunction: Pointer): PPointer;
begin
  while Assigned(pLibrary^) do
  begin
    if pLibrary^ = pFunction then Exit(pLibrary);
    Inc(pLibrary);
  end;
  Result := nil;
end;

function ReplaceImportFunction(pOldFunction: PPointer; pNewFunction: Pointer): Pointer;
var
  dwOldProtect: DWORD = 0;
begin
  if VirtualProtect(pOldFunction, SizeOf(Pointer), PAGE_READWRITE, dwOldProtect) then
  begin
    Result := pOldFunction^;
    pOldFunction^ := pNewFunction;
    VirtualProtect(pOldFunction, SizeOf(Pointer), dwOldProtect, dwOldProtect);
  end;
end;

end.

