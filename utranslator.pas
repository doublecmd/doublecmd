{
    Double Commander
    -------------------------------------------------------------------------
    This unit is needed for using translated form strings made by Lazarus IDE.
    It loads localized form strings from .po file.

    Copyright (C) 2007  Koblov Alexander (Alexx2000@mail.ru)

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

unit uTranslator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, GetText, Controls, typinfo, Translations;
type

 { TTranslator }

 TTranslator = class(TAbstractTranslator)
 private
  FFormClassName : String;
  FPOFile:TPOFile;
 public
  constructor Create(POFileName:string);
  destructor Destroy;override;
  procedure TranslateStringProperty(Sender:TObject; const Instance: TPersistent; PropInfo: PPropInfo; var Content:string);override;
 end;
  
implementation
uses
  Forms, LCLProc;

{ TTranslator }

constructor TTranslator.Create(POFileName: string);
begin
  inherited Create;
  FPOFile := TPOFile.Create(POFileName);
end;

destructor TTranslator.Destroy;
begin
  FPOFile.Free;
  inherited Destroy;
end;

procedure TTranslator.TranslateStringProperty(Sender: TObject;
  const Instance: TPersistent; PropInfo: PPropInfo; var Content: string);
begin
  if Instance.InheritsFrom(TForm) then
    begin
      FFormClassName := Instance.ClassName;
      //DebugLn(UpperCase(FFormClassName + '.'+PropInfo^.Name) + '=' + Content);
      Content := FPOFile.Translate(UpperCase(FFormClassName + '.' + PropInfo^.Name), Content);
    end
  else
    begin
      //DebugLn(UpperCase(FFormClassName + '.'+Instance.GetNamePath + '.' + PropInfo^.Name) + '=' + Content);
      Content := FPOFile.Translate(UpperCase(FFormClassName + '.'+Instance.GetNamePath + '.'+ PropInfo^.Name), Content);
    end;
  // convert UTF8 to current local
  Content := UTF8ToSystemCharSet(Content);
end;

end.

