(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower Abbrevia
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* ABBREVIA: AbRegLinux.pas 3.05                         *}
{*********************************************************}
{* ABBREVIA: Registrations                               *}
{*********************************************************}

{$I AbDefine.inc}
unit AbRegLinux;

{$R AbReg.res}

interface

{$IFDEF MSWINDOWS}
  !! Error, this unit is for CLX on Linux, use AbRegClx.pas for Windows
{$ENDIF}

uses
  Classes,
  AbQZpOut, AbQView, AbQZView, AbQMeter;

procedure Register;

implementation

uses
  AbUtils,
  AbQPeDir,
  AbQPeFn,
  AbQPePas,
  AbQPeVer,
  AbQPeCol,
  AbZBrows,
  AbZipper,
  AbUnzper,
  AbZipKit,
  AbSelfEx,
  DesignIntf,
  DesignEditors;

procedure Register;
begin
  RegisterPropertyEditor( TypeInfo( string ), TAbZipBrowser, 'FileName',
                          TAbFileNameProperty );
  RegisterPropertyEditor( TypeInfo( string ), TAbZipper, 'FileName',
                          TAbFileNameProperty );
  RegisterPropertyEditor( TypeInfo( string ), TAbUnZipper, 'FileName',
                          TAbFileNameProperty );
  RegisterPropertyEditor( TypeInfo( string ), TAbZipKit, 'FileName',
                          TAbFileNameProperty );
  RegisterPropertyEditor( TypeInfo( string ), TAbZipOutline, 'FileName',
                          TAbFileNameProperty );
  RegisterPropertyEditor( TypeInfo( string ), TAbZipBrowser, 'LogFile',
                          TAbLogNameProperty );
  RegisterPropertyEditor( TypeInfo( string ), TAbZipper, 'LogFile',
                          TAbLogNameProperty );
  RegisterPropertyEditor( TypeInfo( string ), TAbUnZipper, 'LogFile',
                          TAbLogNameProperty );
  RegisterPropertyEditor( TypeInfo( string ), TAbZipKit, 'LogFile',
                          TAbLogNameProperty );
  RegisterPropertyEditor( TypeInfo( string ), TAbZipOutline, 'LogFile',
                          TAbLogNameProperty );
  RegisterPropertyEditor( TypeInfo( string ), TAbMakeSelfExe, 'SelfExe',
                          TAbExeNameProperty );
  RegisterPropertyEditor( TypeInfo( string ), TAbMakeSelfExe, 'StubExe',
                          TAbExeNameProperty );
  RegisterPropertyEditor( TypeInfo( string ), TAbMakeSelfExe, 'ZipFile',
                          TAbFileNameProperty );
  RegisterPropertyEditor( TypeInfo( string ), TAbZipBrowser, 'BaseDirectory',
                          TAbDirectoryProperty );
  RegisterPropertyEditor( TypeInfo( string ), TAbZipper, 'BaseDirectory',
                          TAbDirectoryProperty );
  RegisterPropertyEditor( TypeInfo( string ), TAbUnZipper, 'BaseDirectory',
                          TAbDirectoryProperty );
  RegisterPropertyEditor( TypeInfo( string ), TAbZipKit, 'BaseDirectory',
                          TAbDirectoryProperty );
  RegisterPropertyEditor( TypeInfo( string ), TAbZipOutline, 'BaseDirectory',
                          TAbDirectoryProperty );
  RegisterPropertyEditor( TypeInfo( string ), TAbZipBrowser, 'TempDirectory',
                          TAbDirectoryProperty );
  RegisterPropertyEditor( TypeInfo( string ), TAbZipper, 'TempDirectory',
                          TAbDirectoryProperty );
  RegisterPropertyEditor( TypeInfo( string ), TAbUnZipper, 'TempDirectory',
                          TAbDirectoryProperty );
  RegisterPropertyEditor( TypeInfo( string ), TAbZipKit, 'TempDirectory',
                          TAbDirectoryProperty );
  RegisterPropertyEditor( TypeInfo( string ), TAbZipOutline, 'TempDirectory',
                          TAbDirectoryProperty );
  RegisterPropertyEditor( TypeInfo( string ), TAbZipBrowser, 'Version',
                          TAbVersionProperty );
  RegisterPropertyEditor( TypeInfo( string ), TAbZipper, 'Version',
                          TAbVersionProperty );
  RegisterPropertyEditor( TypeInfo( string ), TAbUnZipper, 'Version',
                          TAbVersionProperty );
  RegisterPropertyEditor( TypeInfo( string ), TAbZipKit, 'Version',
                          TAbVersionProperty );
  RegisterPropertyEditor( TypeInfo( string ), TAbZipOutline, 'Version',
                          TAbVersionProperty );
  RegisterPropertyEditor( TypeInfo( string ), TAbZipView, 'Version',
                          TAbVersionProperty );
  RegisterPropertyEditor( TypeInfo( string ), TAbMeter, 'Version',
                          TAbVersionProperty );
  RegisterPropertyEditor( TypeInfo( string ), TAbMakeSelfExe, 'Version',
                          TAbVersionProperty );
  RegisterPropertyEditor( TypeInfo( string ), TAbZipBrowser, 'Password',
                          TAbPasswordProperty );
  RegisterPropertyEditor( TypeInfo( string ), TAbZipper, 'Password',
                          TAbPasswordProperty );
  RegisterPropertyEditor( TypeInfo( string ), TAbUnZipper, 'Password',
                          TAbPasswordProperty );
  RegisterPropertyEditor( TypeInfo( string ), TAbZipKit, 'Password',
                          TAbPasswordProperty );
  RegisterPropertyEditor( TypeInfo( string ), TAbZipOutline, 'Password',
                          TAbPasswordProperty );
  RegisterPropertyEditor( TypeInfo( TAbColHeadings ), TAbZipView, 'Headings',
                          TAbColHeadingsProperty );
  RegisterComponents( 'Abbrevia',
                      [TAbMeterLink,
                        TAbZipBrowser,
                        TAbUnzipper,
                        TAbZipper,
                        TAbZipKit,
                        TAbZipOutline,
                        TAbZipView,
                        TAbMeter,
                        TAbMakeSelfExe]);
end;

end.
