{
   Double Commander
   -------------------------------------------------------------------------
   Extension API implementation

   Copyright (C) 2008-2023 Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit uExtension;

{$mode objfpc}{$H+}
{$include calling.inc}

interface

uses
  Classes, SysUtils, Extension, Translations;

type

  { TDcxModule }

  TDcxModule = class
  protected
    FPOFile: TPOFile;
    FModulePath: String;
    FModuleHandle: TLibHandle;
  public
    destructor Destroy; override;
    procedure InitializeExtension(StartupInfo: PExtensionStartupInfo);
  end;

implementation

uses
  Math, LazFileUtils, DCOSUtils, fDialogBox, uGlobs, uGlobsPaths;

function Translate(Translation: Pointer; Identifier, Original: PAnsiChar; Output: PAnsiChar; OutLen: Integer): Integer; dcpcall;
var
  AText: String;
  POFile: TPOFile absolute Translation;
begin
  if (POFile = nil) then
  begin
    Result:= 0;
    Output^:= #0;
  end
  else begin
    AText:= POFile.Translate(Identifier, Original);
    StrPLCopy(Output, AText, OutLen - 1);
    Result:= Min(Length(AText), OutLen - 1);
  end;
end;

{ TDcxModule }

destructor TDcxModule.Destroy;
begin
  inherited Destroy;
  FPOFile.Free;
end;

procedure TDcxModule.InitializeExtension(StartupInfo: PExtensionStartupInfo);
var
  Language: String;
  AFileName, APath: String;
begin
  FillByte(StartupInfo^, SizeOf(TExtensionStartupInfo), 0);

  AFileName:= FModulePath;
  APath:= ExtractFilePath(AFileName) + 'language' + PathDelim;
  Language:= ExtractFileExt(ExtractFileNameOnly(gPOFileName));
  AFileName:= APath + ExtractFileNameOnly(AFileName) + Language + '.po';
  if mbFileExists(AFileName) then FPOFile:= TPOFile.Create(AFileName);

  with StartupInfo^ do
  begin
    StructSize:= SizeOf(TExtensionStartupInfo);
    PluginDir:= ExtractFilePath(FModulePath);
    PluginConfDir:= gpCfgDir;
    InputBox:= @fDialogBox.InputBox;
    MessageBox:= @fDialogBox.MessageBox;
    DialogBoxLFM:= @fDialogBox.DialogBoxLFM;
    DialogBoxLRS:= @fDialogBox.DialogBoxLRS;
    DialogBoxLFMFile:= @fDialogBox.DialogBoxLFMFile;
    SendDlgMsg:= @fDialogBox.SendDlgMsg;
    Translation:= FPOFile;
    TranslateString:= @Translate;
  end;
end;

end.

