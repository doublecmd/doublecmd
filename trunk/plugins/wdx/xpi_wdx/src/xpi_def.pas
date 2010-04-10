{
   Double commander
   -------------------------------------------------------------------------
   xpi_wdx is destined to obtainin the information from xpi-files
   in which extensions and themes for Gecko-based applications are distributed.

   Copyright (C) 2010 Koblov Alexander (Alexx2000@mail.ru)

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
}

unit xpi_def;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type

  { TInstallManifest }

  TInstallManifest = class
  public
    ID,
    Name,
    Version,
    Description,
    Creator,
    Developer,
    Contributor,
    Translator,
    HomePageURL,
    UpdateURL,
    TargetPlatform,
    Compatibility: AnsiString;
    TargetApplication: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
  end;

var
  ApplicationList: TStringList;

implementation

{ TInstallManifest }

procedure TInstallManifest.Clear;
begin
  ID              := EmptyStr;
  Name            := EmptyStr;
  Version         := EmptyStr;
  Description     := EmptyStr;
  Creator         := EmptyStr;
  Developer       := EmptyStr;
  Contributor     := EmptyStr;
  Translator      := EmptyStr;
  HomePageURL     := EmptyStr;
  UpdateURL       := EmptyStr;
  TargetPlatform  := EmptyStr;
  Compatibility   := EmptyStr;
  TargetApplication.Clear;
end;

constructor TInstallManifest.Create;
begin
  TargetApplication:= TStringList.Create;
end;

destructor TInstallManifest.Destroy;
begin
  FreeAndNil(TargetApplication);
  inherited Destroy;
end;

initialization
  ApplicationList:= TStringList.Create;
  ApplicationList.Add('{ec8030f7-c20a-464f-9b0e-13a3a9e97384}=fx'); // Firefox
  ApplicationList.Add('{86c18b42-e466-45a9-ae7a-9b95ba6f5640}=mz'); // Mozilla Suite
  ApplicationList.Add('{92650c4d-4b8e-4d2a-b7eb-24ecf4f6b63a}=sm'); // SeaMonkey
  ApplicationList.Add('{3550f703-e582-4d05-9a08-453d09bdfdc6}=mt'); // Mozilla Thunderbird
  ApplicationList.Add('{a463f10c-3994-11da-9945-000d60ca027b}=fl'); // Flock
  ApplicationList.Add('{718e30fb-e89b-41dd-9da7-e25a45638b28}=sb'); // Sunbird
  ApplicationList.Add('{a23983c0-fd0e-11dc-95ff-0800200c9a66}=fc'); // Fennec
  //ApplicationList.Add('');

finalization
  FreeAndNil(ApplicationList);

end.

