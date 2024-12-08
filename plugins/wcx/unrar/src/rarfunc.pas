{
   Double commander
   -------------------------------------------------------------------------
   Wcx plugin for packing RAR archives

   Copyright (C) 2015-2023 Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along
   with this program; if not, write to the Free Software Foundation, Inc.,
   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
}

unit RarFunc;

{$mode objfpc}{$H+}
{$include calling.inc}

interface

uses
  Classes, SysUtils, WcxPlugin;

procedure PackSetDefaultParams(dps: PPackDefaultParamStruct); dcpcall; export;
procedure ConfigurePacker(Parent: HWND; DllInstance: THandle); dcpcall; export;
function DeleteFilesW(PackedFile, DeleteList: PWideChar): Integer; dcpcall; export;
function PackFilesW(PackedFile: PWideChar; SubPath: PWideChar; SrcPath: PWideChar; AddList: PWideChar; Flags: Integer): Integer; dcpcall; export;

var
  IniFileName: String;

implementation

uses
  Process, LazUTF8, DCConvertEncoding, DCProcessUtf8, DCOSUtils, UnRARFunc,
  RarConfDlg, RarLng, Extension;

const
  UTF16LEBOM: WideChar = #$FEFF;
  LineEndingW = WideChar(#13) + WideChar(#10);

function RarToWcx(Error: Integer): Integer;
begin
  case Error of
    0:   Result:= E_SUCCESS;       // Successful operation
    1:   Result:= E_SUCCESS;       // Warning. Non fatal error(s) occurred
    2:   Result:= E_BAD_ARCHIVE;   // A fatal error occurred
    3:   Result:= E_BAD_DATA;      // Invalid checksum. Data is damaged
    4:   Result:= E_EOPEN;         // Attempt to modify a locked archive
    5:   Result:= E_EWRITE;        // Write error
    6:   Result:= E_EOPEN;         // File open error
    7:   Result:= E_NOT_SUPPORTED; // Wrong command line option
    8:   Result:= E_NO_MEMORY;     // Not enough memory
    9:   Result:= E_ECREATE;       // File create error
    10:  Result:= E_BAD_DATA;      // No files matching the specified mask and options were found
    11:  Result:= E_BAD_DATA;      // Wrong password
    12:  Result:= E_EREAD;         // Read error
    255: Result:= E_EABORTED;      // User break
    else Result:= E_UNKNOWN;       // Unknown
  end;
end;

function RarExists(const FileName: String): Boolean;
var
  Message: String;
begin
  Result:= mbFileExists(FileName);
  if not Result then
  begin
    Message:= Format(rsMsgExecutableNotFound, [FileName]);
    gStartupInfo.MessageBox(PAnsiChar(Message), nil, MB_OK or MB_ICONERROR);
  end;
end;

function ExecuteRar(Process: TProcessUtf8; FileList : UnicodeString): Integer;
var
  TempFile: THandle;
  S, FileName: String;
  Percent: Integer = 0;
begin
  FileName:= GetTempName('');
  TempFile:= mbFileCreate(FileName);
  if (TempFile = feInvalidHandle) then Exit(E_ECREATE);
  try
    FileWrite(TempFile, FileList[1], Length(FileList) * SizeOf(WideChar));
    FileClose(TempFile);

    Process.Parameters.Add('@' + FileName);

    Process.Execute;

    if poUsePipes in Process.Options then
    begin
      S:= EmptyStr;
      SetLength(FileName, MAX_PATH);
      while Process.Running do
      begin
        if Process.Output.NumBytesAvailable = 0 then
          Sleep(100)
        else begin
          SetLength(FileName, Process.Output.Read(FileName[1], Length(FileName)));
          S+= FileName;
          Result:= Pos('%', S);
          if Result > 0 then
          begin
            TempFile:= Result - 1;
            while S[TempFile] in ['0'..'9'] do
              Dec(TempFile);
            if (Result - TempFile) > 1 then
            begin
              Percent:= StrToIntDef(Copy(S, TempFile + 1, Result - TempFile - 1), Percent);
            end;
            S:= EmptyStr;
          end;
        end;
        if ProcessDataProcW(nil, -(Percent + 1000)) = 0 then
        begin
          Process.Terminate(255);
          Exit(E_EABORTED);
        end;
      end;
    end;

    Process.WaitOnExit;

    Result:= RarToWcx(Process.ExitStatus);
  finally
    DeleteFile(FileName);
  end;
end;

procedure PackSetDefaultParams(dps: PPackDefaultParamStruct); dcpcall; export;
begin
  IniFileName:= CeSysToUtf8(dps^.DefaultIniName);

  LoadConfig;
end;

procedure ConfigurePacker(Parent: HWND; DllInstance: THandle); dcpcall; export;
begin
  CreateRarConfDlg;
end;

function DeleteFilesW(PackedFile, DeleteList: PWideChar): Integer; dcpcall; export;
var
 Rar: String;
 Process : TProcessUtf8;
 FileName : UnicodeString;
 FileList : UnicodeString;
 FolderName: UnicodeString;
begin
  Rar:= mbExpandEnvironmentStrings(WinRar);
  if not RarExists(Rar) then Exit(E_HANDLED);

  Process := TProcessUtf8.Create(nil);
  try
    Process.Executable:= Rar;
    Process.Parameters.Add('d');
    Process.Parameters.Add('-c-');
    Process.Parameters.Add('-r-');
    Process.Parameters.Add(CeUtf16ToUtf8(UnicodeString(PackedFile)));

    try
      // Parse file list
      FileList:= UTF16LEBOM;
      while DeleteList^ <> #0 do
      begin
        FileName := DeleteList; // Convert PWideChar to UnicodeString (up to first #0).

        FileList += FileName + LineEndingW;

        // If ends with '*' then delete directory
        if FileName[Length(FileName)] = '*' then
        begin
          FolderName:= FileName;
          Delete(FolderName, Length(FileName) - 3, 4);
          FileList += FolderName + LineEndingW;
        end;

        DeleteList := DeleteList + Length(FileName) + 1; // move after filename and ending #0
      end;

      Result:= ExecuteRar(Process, FileList);

    except
      Result:= E_EOPEN;
    end;
  finally
    Process.Free;
  end;
end;

function PackFilesW(PackedFile: PWideChar; SubPath: PWideChar;  SrcPath: PWideChar;  AddList: PWideChar;  Flags: Integer): Integer;dcpcall; export;
const
{$IF DEFINED(MSWINDOWS)}
  SFXExt = '.exe';
{$ELSE}
  SFXExt = '.run';
{$ENDIF}
var
  Rar: String;
  Process : TProcessUtf8;
  FileList: UnicodeString;
  FileName: UnicodeString;
  FolderName: UnicodeString;
  Password: array[0..MAX_PATH] of AnsiChar;
begin
  Rar:= mbExpandEnvironmentStrings(WinRar);
  if not RarExists(Rar) then Exit(E_HANDLED);

  Process := TProcessUtf8.Create(nil);
  try
    Process.Executable:= Rar;

    if FileIsConsoleExe(Process.Executable) then begin
      Process.Options:= [poUsePipes, poNoConsole, poNewProcessGroup];
    end;

    if (Flags and PK_PACK_MOVE_FILES <> 0) then
      Process.Parameters.Add('m')
    else begin
      Process.Parameters.Add('a');
    end;
    Process.Parameters.Add('-c-');
    Process.Parameters.Add('-r-');
    // Create solid archive
    if Solid then Process.Parameters.Add('-s');
    // Compression method
    Process.Parameters.Add('-m' + IntToStr(Method));

    if SameStr(ExtractFileExt(CeUtf16ToUtf8(UnicodeString(PackedFile))), SFXExt) then
      Process.Parameters.Add('-sfx');

    // Add user command line parameters
    if Length(Args) > 0 then CommandToList(Args, Process.Parameters);

    // Add data recovery record
    if Recovery and (Pos('-rr', Args) = 0) then
      Process.Parameters.Add('-rr3p');

    // Encrypt archive
    if (Flags and PK_PACK_ENCRYPT <> 0) then
    begin
      FillChar(Password, SizeOf(Password), #0);
      if gStartupInfo.InputBox('Rar', PAnsiChar(rsMsgPasswordEnter), True, PAnsiChar(Password), MAX_PATH) then
      begin
       if Encrypt then
         Process.Parameters.Add('-hp' + Password)
       else begin
         Process.Parameters.Add('-p' + Password);
       end;
      end
      else begin
        Exit(E_EABORTED);
      end;
    end;

    // Destination path
    if Assigned(SubPath) then
    begin
      Process.Parameters.Add('-ap' + CeUtf16ToUtf8(UnicodeString(SubPath)));
    end;

    Process.Parameters.Add(CeUtf16ToUtf8(UnicodeString(PackedFile)));

    // Source path
    if Assigned(SrcPath) then
    begin
      Process.CurrentDirectory:= CeUtf16ToUtf8(UnicodeString(SrcPath));
    end;

    try
      // Parse file list
      FileList:= UTF16LEBOM;
      while AddList^ <> #0 do
      begin
        FileName := UnicodeString(AddList);

        FileList += FileName + LineEndingW;

        // If ends with '/' then add directory
        if FileName[Length(FileName)] = PathDelim then
        begin
          FolderName:= FileName;
          Delete(FolderName, Length(FileName), 1);
          FileList += FolderName + LineEndingW;
        end;

        Inc(AddList, Length(FileName) + 1);
      end;

      Result:= ExecuteRar(Process, FileList);

    except
      Result:= E_EOPEN;
    end;
  finally
    Process.Free;
  end;
end;

end.

