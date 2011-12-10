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
// $Id: nsPkg.pas,v 1.2 2007/05/20 15:46:11 ya_nick Exp $

unit nsPkg;

interface
uses
	nsCore;

type
 	nsIdxSft = (
    eIdxSft4bits  = 3
// NOT used
//    eIdxSft8bits  = 2,
//    eIdxSft16bits = 1
	);

	 nsSftMsk = (
    eSftMsk4bits  = 7
// NOT used
//    eSftMsk8bits  = 3,
//    eSftMsk16bits = 1
  );

	nsBitSft = (
    eBitSft4bits  = 2
// NOT used
//    eBitSft8bits  = 3,
//    eBitSft16bits = 4
  );

	nsUnitMsk = (
    eUnitMsk4bits  = $0000000F
// NOT used
//    eUnitMsk8bits  = $000000FF,
//    eUnitMsk16bits = $0000FFFF
  );


	nsPkgInt = record
    idxsft: nsIdxSft;
    sftmsk: nsSftMsk;
    bitsft: nsBitSft;
    unitmsk: nsUnitMsk;
    data: pPRUint32;
  end;
	pnsPkgInt = ^nsPkgInt;


function PCK4BITS(a, b, c, d, e, f, g, h: integer): integer;

function GETFROMPCK(i: integer; c: pnsPkgInt): integer;

implementation


function PCK16BITS(a: integer; b: integer): integer;
begin
  Result:= ((b shl 16) or a);
end;

function PCK8BITS(a, b, c, d: integer): integer;
begin
  Result:= PCK16BITS(((b shl 8) or a), ((d shl 8) or c));
end;

function PCK4BITS(a, b, c, d, e, f, g, h: integer): integer;
begin
  Result:= PCK8BITS(((b shl 4) or a), ((d shl 4) or c), ((f shl 4) or e), ((h shl 4) or g));
end;

function GETFROMPCK(i: integer; c: pnsPkgInt): integer;
begin
  Result:= (((aPRUint32(c^.data)[i shr integer(c^.idxsft)]) shr (i and integer(c^.sftmsk) shl integer(c^.bitsft))) and integer(c^.unitmsk));
end;

end.
