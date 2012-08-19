// +----------------------------------------------------------------------+
// |    chsdet - Charset Detector Library                                 |
// +----------------------------------------------------------------------+
// | Copyright (C) 2006, Nick Yakowlew     http://chsdet.sourceforge.net  |
// +----------------------------------------------------------------------+
// | Based on Mozilla sources     http://www.mozilla.org/projects/intl/   |
// +----------------------------------------------------------------------+
// | This library is free software; you can redistribute it and/or modify |
// | it under the terms of the GNU General Public License as published by |
// | the Free Software Foundation; either version 2 of the License, or    |
// | (at your option) any later version.                                  |
// | This library is distributed in the hope that it will be useful       |
// | but WITHOUT ANY WARRANTY; without even the implied warranty of       |
// | MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                 |
// | See the GNU Lesser General Public License for more details.          |
// | http://www.opensource.org/licenses/lgpl-license.php                  |
// +----------------------------------------------------------------------+
//
// $Id: chsdIntf.pas,v 1.4 2008/06/22 09:04:20 ya_nick Exp $

unit chsdIntf;

interface

uses
  nsCore;

	procedure csd_Reset; stdcall;
  function  csd_HandleData(aBuf: PChar; aLen: integer): integer; stdcall;
  function  csd_Done: boolean; stdcall;
  procedure csd_DataEnd; stdcall;
  function  csd_GetDetectedCharset: rCharsetInfo; stdcall;
  function  csd_GetKnownCharsets(var KnownCharsets: pChar): integer; stdcall;
  procedure csd_GetAbout(var About: rAboutHolder); stdcall;
  function  csd_GetDetectedBOM: eBOMKind; stdcall;
  procedure csd_DisableCharsetCP(CodePage: integer); stdcall;


implementation
uses
	nsUniversalDetector;

var
  Detector: TnsUniversalDetector = nil;

procedure csd_Reset; stdcall;
begin
	Detector.Reset;
end;

function csd_HandleData(aBuf: PChar; aLen: integer): integer; stdcall;
begin
	Result := Detector.HandleData(aBuf, aLen);
end;

function csd_Done: boolean; stdcall;
begin
	Result := Detector.Done;
end;

procedure csd_DataEnd; stdcall;
begin
	Detector.DataEnd;
end;

function csd_GetDetectedCharset: rCharsetInfo; stdcall;
begin
  Result := Detector.GetDetectedCharsetInfo;
end;

function csd_GetKnownCharsets(var KnownCharsets: pChar): integer; stdcall;
begin
  Result := Detector.GetKnownCharset(KnownCharsets);
end;

procedure csd_GetAbout(var About: rAboutHolder); stdcall;
begin
  Detector.GetAbout(About);
end;

function csd_GetDetectedBOM: eBOMKind; stdcall;
begin
  Result := Detector.BOMDetected;
end;

procedure csd_DisableCharsetCP(CodePage: integer); stdcall;
begin
  Detector.DisableCharset(CodePage);
end;

initialization
  Detector := TnsUniversalDetector.Create;

finalization
  if Detector <> nil then
  	Detector.Free;

end.


