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
{* ABBREVIA: AbDefine.inc 3.04                           *}
{*********************************************************}
{* ABBREVIA: Compiler options/directives include file    *}
{*********************************************************}

{NOTE: ABDEFINE.INC is included in all ABBREVIA units; hence you can
       specify global compiler options here. ABDEFINE.INC is included
       *before* each unit's own required compiler options, so options
       specified here could be overridden by hardcoded options in the
       unit source file.}

{$IFDEF FPC}
{$MODE DELPHI}
{$ASMMODE INTEL}
{$DEFINE BuildingStub}
{$DEFINE NOQT}
{$IFDEF WIN32}
{$DEFINE MSWINDOWS}
{$ENDIF}
{$ENDIF}


{====Compiler options that can be changed====}
{$A+ Force alignment on word/dword boundaries}
{$S- No stack checking}

{$IFNDEF LINUX}
{---Global compiler defines for 32-bit OS's---}
{====Global fixed compiler options (do NOT change)====}

{$B- Incomplete boolean evaluation}
{$H+ Long string support}
{$J+ Writeable typed constants}
{$P- No open string parameters}
{$Q- Arithmetic overflow checking} {!! - Needs to be turned on!}
{$R- Range checking}               {!! - Needs to be turned on!}
{$T+ No type-checked pointers}
{$V- No var string checking}
{$X+ Extended syntax}
{$Z1 Enumerations are byte sized}
{$ENDIF LINUX}

{===========!! DO NOT ALTER ANYTHING BEYOND THIS POINT !!==================}
{===========!! DO NOT ALTER ANYTHING BEYOND THIS POINT !!==================}

{$IFDEF VER110} {C++Builder 3}
  {$DEFINE CBuilder}
  {$ObjExportAll On}
{$ENDIF}
{$IFDEF VER125} {C++Builder 4}
  {$DEFINE CBuilder}
  {$ObjExportAll On}
{$ENDIF}
{$IFDEF VER130} {C++Builder 5}
  {$IFDEF BCB}
    {$DEFINE CBuilder}
    {$ObjExportAll On}
  {$ENDIF}
{$ENDIF}
{$IFDEF VER140} {C++Builder 6}
  {$IFDEF BCB}
    {$DEFINE CBuilder}
    {$ObjExportAll On}
  {$ENDIF}
{$ENDIF}

{$IFNDEF VER80}
 {$IFNDEF VER90}
  {$IFNDEF VER93}
    {$DEFINE VERSION3} { Delphi 3.0 or BCB 3.0 or higher }
  {$ENDIF}
 {$ENDIF}
{$ENDIF}

{$IFDEF VERSION3}
 {$IFNDEF VER100}     {Delphi 3}
  {$IFNDEF VER110}    {BCB 3}
   {$DEFINE VERSION4} {Delphi 4.0 or higher}
  {$ENDIF}
 {$ENDIF}
{$ENDIF}

{$IFDEF VERSION4}
 {$IFNDEF VER120}             { Delphi 4 }
  {$IFNDEF VER125}            { BCB 4 }
   {$DEFINE VERSION5}         { Delphi 5.0 or higher }
  {$ENDIF}
 {$ENDIF}
{$ENDIF}

{$IFDEF VERSION5}
 {$IFNDEF VER130}             { Delphi 5 }
  {$IFNDEF VER135}            { BCB 5 }
    {$IFNDEF LINUX }
      {$DEFINE VERSION6}      { Delphi 6.0 or higher }
    {$ENDIF}
  {$ENDIF}
 {$ENDIF}
{$ENDIF}

{$IFDEF VERSION6}
 {$IFNDEF VER140}             { Delphi 6 }
  {$IFNDEF LINUX }
    {$DEFINE VERSION7}      { Delphi 7.0 or higher }
  {$ENDIF}
 {$ENDIF}
{$ENDIF}

{$IFDEF VERSION4}
  {$DEFINE HasLongWord}       { D4 and above have a true LongWord type }
  {$DEFINE HasInt64 }         { D4 and above have a 64-bit Integer }
{$ENDIF}
{$IFDEF LINUX}
  {$DEFINE HasLongWord}       { Kylix has a true LongWord type }
  {$DEFINE HasInt64 }         { Kylix has a 64-bit Integer }
{$ENDIF}

{$IFNDEF MSWINDOWS}
{$IFDEF VERSION3}
  {$IFNDEF LINUX}
    {$DEFINE MSWINDOWS}
  {$ENDIF}
{$ENDIF}
{$ENDIF MSWINDOWS}

{$IFDEF Win32}
  {$DEFINE NeedMouseWheel}
  {$IFDEF Version4}          {version 4+ has built-in mousewheel support}
    {$UNDEF NeedMouseWheel}
  {$ENDIF}
{$ENDIF}

{ determine whether CLX is used, should always be on Linux }
{$IFDEF LINUX}
{$DEFINE UsingCLX }
{$ENDIF LINUX}

{====General defines=================================================}

{Activate the following define to include extra code to get rid of all
 hints and warnings. Parts of ABBREVIA are written in such a way
 that the hint/warning algorithms of the Delphi 2/3 compilers are
 fooled and report things like variables being used before
 initialisation and so on when in reality the problem does not exist.}
{$DEFINE DefeatWarnings} {!!.03 -- on by default }

{====Zip defines=====================================================}

{Activate the following define when you don't want Visual parts of
the VCL library included for a program using a TAbArchive or
TAbZipArchive}
{.$DEFINE BuildingStub}

{Activate the following define to include support for extracting files
using PKzip compatible unShrink.}

{.$DEFINE UnzipShrinkSupport}

{Activate the following define to include support for extracting files
using PKZip compatible unReduce.}

{.$DEFINE UnzipReduceSupport}

{Activate the following define to include support for extracting files
using PKZip compatible unImplode.}

{.$DEFINE UnzipImplodeSupport}

{Activate the following to include support for extracting files using
all older PKZip compatible methods (Shrink, Reduce, Implode}

{$DEFINE UnzipBackwardSupport}

{Activate the following to include logging support in the deflate/
 inflate code. Since this logging support is a by-product of assertion
 checking, you should only activate it if that is also on: $C+}
{$IFOPT C+} //if Assertions are on
{.$DEFINE UseLogging}
{$ENDIF}


{-------- !! DO NOT CHANGE DEFINES BELOW THIS LINE !! --------}

{$UNDEF AB_DEBUG}

{.$IFOPT D+}
{$DEFINE AB_DEBUG}
{.$ENDIF}

{$IFDEF UnzipBackwardSupport}
  {$DEFINE UnzipShrinkSupport}
  {$DEFINE UnzipReduceSupport}
  {$DEFINE UnzipImplodeSupport}
{$ENDIF}
