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
// $Id: nsEscCharsetProber.pas,v 1.3 2007/05/26 13:09:38 ya_nick Exp $

unit nsEscCharsetProber;

interface

uses
	nsCore,
	MultiModelProber;

type
	TnsEscCharSetProber = class (TMultiModelProber)
		public
    	constructor Create; override;
      function GetConfidence: float; override;
  end;


implementation
uses
  nsCodingStateMachine,
  CustomDetector;
  
{$I '.\mbclass\ISO2022KRLangModel.inc'}
{$I '.\mbclass\ISO2022JPLangModel.inc'}
{$I '.\mbclass\ISO2022CNLangModel.inc'}
{$I '.\mbclass\HZLangModel.inc'}

{ TnsEscCharSetProber }
const
	NUM_OF_ESC_CHARSETS = 4;

constructor TnsEscCharSetProber.Create;
begin
  inherited;
  AddCharsetModel(HZSMModel);
  AddCharsetModel(ISO2022CNSMModel);
  AddCharsetModel(ISO2022JPSMModel);
  AddCharsetModel(ISO2022KRSMModel);
  Reset;
end;

function TnsEscCharSetProber.GetConfidence: float;
begin
  case mState of
    psFoundIt:   Result := SURE_YES;
    psNotMe:     Result := SURE_NO;
    psDetecting: Result := (SURE_YES + SURE_NO) / 2;
    else
      Result := 1.1 * SURE_NO;
  end;
end;

end.


