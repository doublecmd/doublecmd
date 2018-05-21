{
    Double Commander
    -------------------------------------------------------------------------
    This unit is needed for using translated form strings made by Lazarus IDE.
    It loads localized form strings from .po file.

    Copyright (C) 2007-2016 Alexander Koblov (alexx2000@mail.ru)

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

unit uTranslator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, TypInfo, Translations;
type

 { TTranslator }

 TTranslator = class(TAbstractTranslator)
 private
  FPOFile: TPOFile;
 public
  constructor Create(const FileName: String);
  destructor Destroy; override;
  procedure TranslateStringProperty(Sender: TObject; const Instance: TPersistent;
    PropInfo: PPropInfo; var Content: String); override;
 end;
  
implementation

uses
  LCLProc;

{ TTranslator }

constructor TTranslator.Create(const FileName: String);
begin
  inherited Create;
  FPOFile := TPOFile.Create(FileName);
end;

destructor TTranslator.Destroy;
begin
  FPOFile.Free;
  inherited Destroy;
end;

procedure TTranslator.TranslateStringProperty(Sender: TObject;
  const Instance: TPersistent; PropInfo: PPropInfo; var Content: String);
var
  Reader: TReader;
  Identifier: String;
begin
  if (PropInfo = nil) then Exit;
  if (CompareText(PropInfo^.PropType^.Name, 'TTRANSLATESTRING') <> 0) then Exit;
  if (Sender is TReader) then
  begin
    Reader := TReader(Sender);
    if Reader.Driver is TLRSObjectReader then
      Identifier := TLRSObjectReader(Reader.Driver).GetStackPath
    else begin
      Identifier := Instance.ClassName + '.' + PropInfo^.Name;
    end;
    // DebugLn(UpperCase(Identifier) + '=' + Content);
    Content := FPOFile.Translate(Identifier, Content);
  end;
end;

end.

