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
{* ABBREVIA: AbDefine.inc 3.05                           *}
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
{$IFDEF CPU32}
{$DEFINE UseGreedyAsm}
{$ELSE}
{$DEFINE UseGreedyPascal}
{$ENDIF}
{$DEFINE BuildingStub}
{$DEFINE NOQT}
{$IFDEF WIN32}
{$DEFINE MSWINDOWS}
{$ENDIF}
{$ENDIF}


{====Compiler options that can be changed====}
{$A+ Force alignment on word/dword boundaries}
{$S- No stack checking}

{---Global compiler defines for 32-bit OS's---}
{====Global fixed compiler options (do NOT change)====}

{$B- Incomplete boolean evaluation}
{$H+ Long string support}
{$P- No open string parameters}
{$Q- Arithmetic overflow checking} {!! - Needs to be turned on!}
{$R- Range checking}               {!! - Needs to be turned on!}
{$T+ No type-checked pointers}
{$V- No var string checking}
{$X+ Extended syntax}
{$Z1 Enumerations are byte sized}

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
 that the hint/warning algorithms of the Delphi compilers are
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

{
 According to
    http://www.gzip.org/zlib/rfc1952.txt

 A compliant gzip compressor should calculate and set the CRC32 and ISIZE.
 However, a compliant decompressor should not check these values.

 If you want to check the the values of the CRC32 and ISIZE in a GZIP file
 when decompressing enable the STRICTGZIP define below. }

{.$DEFINE STRICTGZIP}

{ The following define is ONLY used for Abbrevia Unit Tests.
  It has no effect on the Abbrevia Library.

  If defined it uses Winzip to create and test archives for compatability.
  The winzip tests require Systools stSpawn.pas
  It can be downloaded at http://sf.net/projects/tpsystools }

{$IFNDEF LINUX}
{$DEFINE WINZIPTESTS}
{$ENDIF}

{ This a temporary Define for Abbrevia 3.05 Hopefully it's need will be removed
  when Abbrevia 4.0 is released. With something that can be defined at runtime.
  Which would be much easier to unit test!
  
  This only effects Procedure AbUnZip in AbUnzPrc.Pas.

  When extracting files, we have three options.
  1.  Clobber - Write in Place (Side Effect if Password or ZIP File is invalid and a version of the file you where
                                extracting was on the disk, the original GONE!!!!)
  2.  Memory - File is extracted to Memory, if succesful then it is written to disk
  3.  TempFile - File is extract to the Temp Directory, if sucessful then it is moved to correct location.

  In testing, the memory option is the fastest as long as you don't end up using the Page File.
  Once you start to page, this option is usually (Not always) slower than TempFile method.
  Clobber was not tested for performance, it was the original method, I would assume it is faster
  than the TempFile method (If extracting to the same machine) but if extract to a network I suspect
  it could be slower.

  After much thought, I have decided to make TempFile the default method, I need comments if you
  think this should be different. }

{.$DEFINE AbUnZipClobber}
{.$DEFINE AbUnZipMemory}
{$DEFINE AbUnZipTempFile}

{$IFDEF AbUnZipClobber}
  {$IFDEF AbUnZipMemory}
    Only one method can be used.
  {$ENDIF}
  {$IFDEF AbUnZipTempFile}
    Only one method can be used.
  {$ENDIF}
{$ENDIF}
{$IFDEF AbUnZipMemory}
  {$IFDEF AbUnZipTempFile}
    Only one method can be used.
  {$ENDIF}
{$ENDIF}

{$IFNDEF AbUnzipClobber}
  {$IFNDEF AbUnzipMemory}
    {$IFNDEF AbUnzipTempFile}
       Need to select on of the above methods.
    {$ENDIF}
  {$ENDIF}
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
