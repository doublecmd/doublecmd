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
{* ABBREVIA: AbDefine.inc                                *}
{*********************************************************}
{* ABBREVIA: Compiler options/directives include file    *}
{*********************************************************}

{NOTE: ABDEFINE.INC is included in all ABBREVIA units; hence you can
       specify global compiler options here. ABDEFINE.INC is included
       *before* each unit's own required compiler options, so options
       specified here could be overridden by hardcoded options in the
       unit source file.}


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

{====Platform defines================================================}

{ map Delphi platform defines to FreePascal's (MSWINDOWS/UNIX/LINUX/DARWIN) }
{$IFNDEF FPC}
  {$IF DEFINED(LINUX) AND (CompilerVersion < 15)}
    {$DEFINE KYLIX}
    {$DEFINE UNIX}
  {$IFEND}
  {$IFDEF MACOS}
    {$DEFINE DARWIN}
  {$ENDIF}
  {$IFDEF POSIX}
    {$DEFINE UNIX}
  {$ENDIF}
{$ENDIF}

{ Unix API (Kylix/Delphi/FreePascal) }
{$IFDEF UNIX}
  {$IF DEFINED(FPC)}
    {$DEFINE FPCUnixAPI}
  {$ELSEIF DEFINED(KYLIX)}
    {$DEFINE LibcAPI}
  {$ELSE}
    {$DEFINE PosixAPI}
  {$IFEND}
{$ENDIF}

{$IFDEF FPC}
  {$MODE DELPHI}
  {$PACKRECORDS C}
{$ENDIF}

{Activate this define to show CLX/LCL dialogs for spanning media requests.  The
 default behavior will abort the operation instead.  This define is only safe
 when using Abbrevia from the foreground thread.  If using it from a background
 thread override OnRequestLastDisk, OnRequestNthDisk, and OnRequestBlankDisk and
 synchronize to the foreground yourself.  The Windows version always MessageBox
 so it's thread-safe.}
{.$DEFINE UnixDialogs}

{====RTL defines=====================================================}

{$IFNDEF FPC}
  {$IF RTLVersion >= 18} // Delphi 2006
    {$DEFINE HasAdvancedRecords}
  {$IFEND}
  {$IF RTLVersion >= 20} // Delphi 2009
    {$DEFINE HasThreadFinished}
  {$IFEND}
  {$IF RTLVersion >= 21} // Delphi 2010
    {$DEFINE HasThreadStart}
  {$IFEND}
  {$IF RTLVersion >= 23} // Delphi XE2
    {$DEFINE HasPlatformsAttribute}
  {$IFEND}
{$ENDIF}

{====Widgetset defines===============================================}

{ VCL version specific defines }
{$IFNDEF FPC}
  {$IF RTLVersion >= 17} // Delphi 2005
    {$DEFINE HasOnMouseActivate}
  {$IFEND}
  {$IF RTLVersion >= 18} // Delphi 2006
    {$DEFINE HasOnMouseEnter}
  {$IFEND}
  {$IF RTLVersion >= 20} // Delphi 2009
    {$DEFINE HasListViewGroups}
    {$DEFINE HasListViewOnItemChecked}
    {$DEFINE HasParentDoubleBuffered}
    {$DEFINE HasTreeViewExpandedImageIndex}
  {$IFEND}
  {$IF RTLVersion >= 21} // Delphi 2010
    {$DEFINE HasGridDrawingStyle}
    {$DEFINE HasTouch}
  {$IFEND}
{$ENDIF}

{====General defines=================================================}

{Activate the following define to include extra code to get rid of all
 hints and warnings. Parts of ABBREVIA are written in such a way
 that the hint/warning algorithms of the Delphi compilers are
 fooled and report things like variables being used before
 initialisation and so on when in reality the problem does not exist.}
{$DEFINE DefeatWarnings}

{ Disable warnings for explicit string casts }
{$IFDEF UNICODE}
  {$WARN EXPLICIT_STRING_CAST OFF}
  {$WARN EXPLICIT_STRING_CAST_LOSS OFF}
{$ENDIF}

{ Disable hints on Delphi XE2/Mac to prevent unexpanded inline messages }
{$IFDEF POSIX}
  {$HINTS OFF}
{$ENDIF}

{====Bzip2 defines===================================================}

{Activate this define to statically link bzip2 .obj files into the application.
 Curerntly only supported by Delphi/Win32.}
{.$DEFINE Bzip2Static}

{Activate this define to dynamically link to a libbz2.dll/libbbz2.so.1}
{.$DEFINE Bzip2Dynamic}

{Activate this define to load libbz2.dll/libbz2.so.1 at runtime using LoadLibrary}
{.$DEFINE Bzip2Runtime}

{Pick an appropriate linking method if none of the above are activate}
{$IF NOT DEFINED(Bzip2Static) AND NOT DEFINED(Bzip2Dynamic) AND NOT DEFINED(Bzip2Runtime)}
  {$IFDEF FPC}
    {$DEFINE Bzip2Runtime}
  {$ELSE}
    {$IFDEF MSWINDOWS}
      {$DEFINE Bzip2Static}
    {$ELSE}
      {$DEFINE Bzip2Dynamic}
    {$ENDIF}
  {$ENDIF}
{$IFEND}

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

{Activate the following to include support for extracting files using
BZIP2 compression.  Added in AppNote.txt v4.6. }

{.$DEFINE UnzipBzip2Support}

{Activate the following to include support for extracting files using
7-zip compatible Lzma compression.  Added in AppNote.txt v6.3.}

{.$DEFINE UnzipLzmaSupport}

{Activate the following to include support for extracting files using
zipx PPMd I compression.  Added in AppNote.txt v6.3.}

{.$DEFINE UnzipPPMdSupport}

{Activate the following to include support for extracting .wav files
using zipx WavPack compression.  Requires copyright notice in your
documentation.  Check "WavPack License.txt" for details.
Added in AppNote.txt v6.3.  }

{.$DEFINE UnzipWavPackSupport}

{Activate the following to include support for extracting files using
all newer (zipx) compatible methods (Bzip2, Lzma, PPMd, WavPack)}

{$DEFINE UnzipZipxSupport}

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

{$IFDEF MSWINDOWS}
  {.$DEFINE WINZIPTESTS}
{$ENDIF}


{-------- !! DO NOT CHANGE DEFINES BELOW THIS LINE !! --------}

{$IFDEF UnzipBackwardSupport}
  {$DEFINE UnzipShrinkSupport}
  {$DEFINE UnzipReduceSupport}
  {$DEFINE UnzipImplodeSupport}
{$ENDIF}

{$IFDEF UnzipZipxSupport}
  {$DEFINE UnzipBzip2Support}
  {$DEFINE UnzipLzmaSupport}
  {$DEFINE UnzipPPMdSupport}
  {$DEFINE UnzipWavPackSupport}
{$ENDIF}

{ Linking .obj files isn't currently supported in Kylix or FPC }
{$IF DEFINED(FPC) OR NOT DEFINED(MSWINDOWS)}
  {$UNDEF UnzipLzmaSupport}
  {$UNDEF UnzipPPMdSupport}
  {$UNDEF UnzipWavPackSupport}
{$IFEND}

