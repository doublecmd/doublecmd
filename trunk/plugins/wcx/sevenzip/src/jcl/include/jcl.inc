{**************************************************************************************************}
{                                                                                                  }
{  The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");}
{  you may not use this file except in compliance with the License. You may obtain a copy of the   }
{  License at http://www.mozilla.org/MPL/                                                          }
{                                                                                                  }
{  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF  }
{  ANY KIND, either express or implied. See the License for the specific language governing rights }
{  and limitations under the License.                                                              }
{                                                                                                  }
{ The Original Code is jcl.inc                                                                     }
{                                                                                                  }
{ The Initial Developer of the Original Code is Marcel van Brakel.                                 }
{ Portions created by Marcel van Brakel are Copyright (C) Marcel van Brakel.                       }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Marcel van Brakel                                                                              }
{   Matthias Thoma (mthoma)                                                                        }
{   Petr Vones                                                                                     }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This include file defines various JCL specific defines. The more generic defines are defined in  }
{ the jedi.inc file which is shared with the JEDI VCL.                                             }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

{$BOOLEVAL OFF}       // Boolean shortcut evaluation
{$LONGSTRINGS ON}     // Long strings
{$WRITEABLECONST OFF} // Read-only typed constants
{$TYPEDADDRESS OFF}   // Type checked pointers off
{$LOCALSYMBOLS ON}    // Local symbol information
{$DEFINITIONINFO ON}  // Symbol definition information
{$REFERENCEINFO ON}   // Symbol reference information

{$I jedi\jedi.inc} // Pull in the JCL/J-VCL shared directives

{$IFNDEF JEDI_INC}
ALERT_jedi_inc_incompatible
// secure against old versions of jedi.inc.
{$ENDIF ~JEDI_INC}

{$IFNDEF JCLINSTALL}
  {$IFDEF CLR}
    {----------------------------}
    { BDS                        }
    {----------------------------}
    {$IFDEF BDS3}
      // This file should be located in directory jcl/source/include
      // It is automatically created by the JCL installer
      // For manual installations, copy and adjust jcl/source/include/jcl.template.inc
      {$I jcld9.net.inc}
      {$DEFINE JCL_CONFIGURED}
    {$ENDIF BDS3}
    {----------------------------}
    {$IFDEF BDS4}
      // This file should be located in directory jcl/source/include
      // It is automatically created by the JCL installer
      // For manual installations, copy and adjust jcl/source/include/jcl.template.inc
      {$I jcld10.net.inc}
      {$DEFINE JCL_CONFIGURED}
    {$ENDIF BDS4}
    {----------------------------}
    {$IFDEF BDS5}
      // This file should be located in directory jcl/source/include
      // It is automatically created by the JCL installer
      // For manual installations, copy and adjust jcl/source/include/jcl.template.inc
      {$I jcld11.net.inc}
      {$DEFINE JCL_CONFIGURED}
    {$ENDIF BDS5}
    {----------------------------}
  {$ELSE ~CLR}
    {----------------------------}
    { C++Builder                 }
    {----------------------------}
    {$IFDEF BCB5}
      // This file should be located in directory jcl/source/include
      // It is automatically created by the JCL installer
      // For manual installations, copy and adjust jcl/source/include/jcl.template.inc
      {$I jclc5.inc}
      {$DEFINE JCL_CONFIGURED}
    {$ENDIF BCB5}
    {----------------------------}
    {$IFDEF BCB6}
      // This file should be located in directory jcl/source/include
      // It is automatically created by the JCL installer
      // For manual installations, copy and adjust jcl/source/include/jcl.template.inc
      {$I jclc6.inc}
      {$DEFINE JCL_CONFIGURED}
    {$ENDIF BCB6}
    {----------------------------}
    { Delphi                     }
    {----------------------------}
    {$IFDEF DELPHI5}
      // This file should be located in directory jcl/source/include
      // It is automatically created by the JCL installer
      // For manual installations, copy and adjust jcl/source/include/jcl.template.inc
      {$I jcld5.inc}
      {$DEFINE JCL_CONFIGURED}
    {$ENDIF DELPIH5}
    {----------------------------}
    {$IFDEF DELPHI6}
      // This file should be located in directory jcl/source/include
      // It is automatically created by the JCL installer
      // For manual installations, copy and adjust jcl/source/include/jcl.template.inc
      {$I jcld6.inc}
      {$DEFINE JCL_CONFIGURED}
    {$ENDIF DELPIH6}
    {----------------------------}
    {$IFDEF DELPHI7}
      // This file should be located in directory jcl/source/include
      // It is automatically created by the JCL installer
      // For manual installations, copy and adjust jcl/source/include/jcl.template.inc
      {$I jcld7.inc}
      {$DEFINE JCL_CONFIGURED}
    {$ENDIF DELPIH7}
    {----------------------------}
    { BDS                        }
    {----------------------------}
    // BDS 1 and BDS 2 have the same version numbers for their native compilers
    // no compiler defines are used for BDS 1 and BDS 2
    {$IFDEF BDS1}
      // This file should be located in directory jcl/source/include
      // It is automatically created by the JCL installer
      // For manual installations, copy and adjust jcl/source/include/jcl.template.inc
      //{$I jclcs1.inc}
      {$DEFINE JCL_CONFIGURED}
    {$ENDIF BDS1}
    {----------------------------}
    {$IFDEF BDS2}
      // This file should be located in directory jcl/source/include
      // It is automatically created by the JCL installer
      // For manual installations, copy and adjust jcl/source/include/jcl.template.inc
      //{$I jcld8.inc}
      {$DEFINE JCL_CONFIGURED}
    {$ENDIF BDS2}
    {----------------------------}
    {$IFDEF BDS3}
      // This file should be located in directory jcl/source/include
      // It is automatically created by the JCL installer
      // For manual installations, copy and adjust jcl/source/include/jcl.template.inc
      {$I jcld9.inc}
      {$DEFINE JCL_CONFIGURED}
    {$ENDIF BDS3}
    {----------------------------}
    {$IFDEF BDS4}
      // This file should be located in directory jcl/source/include
      // It is automatically created by the JCL installer
      // For manual installations, copy and adjust jcl/source/include/jcl.template.inc
      {$I jcld10.inc}
      {$DEFINE JCL_CONFIGURED}
    {$ENDIF BDS4}
    {----------------------------}
    {$IFDEF BDS5}
      // This file should be located in directory jcl/source/include
      // It is automatically created by the JCL installer
      // For manual installations, copy and adjust jcl/source/include/jcl.template.inc
      {$I jcld11.inc}
      {$DEFINE JCL_CONFIGURED}
    {$ENDIF BDS5}
    {----------------------------}
    {$IFDEF BDS6}
      // This file should be located in directory jcl/source/include
      // It is automatically created by the JCL installer
      // For manual installations, copy and adjust jcl/source/include/jcl.template.inc
      {$I jcld12.inc}
      {$DEFINE JCL_CONFIGURED}
    {$ENDIF BDS6}
    {----------------------------}
    {$IFDEF BDS7}
      // This file should be located in directory jcl/source/include
      // It is automatically created by the JCL installer
      // For manual installations, copy and adjust jcl/source/include/jcl.template.inc
      {$I jcld14.inc}
      {$DEFINE JCL_CONFIGURED}
    {$ENDIF BDS7}
    {----------------------------}
    {$IFDEF BDS8}
      // This file should be located in directory jcl/source/include
      // It is automatically created by the JCL installer
      // For manual installations, copy and adjust jcl/source/include/jcl.template.inc
      {$I jcld15.inc}
      {$DEFINE JCL_CONFIGURED}
    {$ENDIF BDS8}
    {----------------------------}
    {$IFDEF BDS9}
      {$IFDEF MSWINDOWS}
      {$IFDEF CPUX86}
      // This file should be located in directory jcl/source/include
      // It is automatically created by the JCL installer
      // For manual installations, copy and adjust jcl/source/include/jcl.template.inc
      {$I jcld16win32.inc}
      {$ENDIF CPUX86}
      {$IFDEF CPUX64}
      // This file should be located in directory jcl/source/include
      // It is automatically created by the JCL installer
      // For manual installations, copy and adjust jcl/source/include/jcl.template.inc
      {$I jcld16win64.inc}
      {$ENDIF CPUX64}
      {$DEFINE JCL_CONFIGURED}
      {$ENDIF MSWINDOWS}
    {$ENDIF BDS9}
    {----------------------------}
    {$IFDEF BDS10}
      {$IFDEF MSWINDOWS}
      {$IFDEF CPUX86}
      // This file should be located in directory jcl/source/include
      // It is automatically created by the JCL installer
      // For manual installations, copy and adjust jcl/source/include/jcl.template.inc
      {$I jcld17win32.inc}
      {$ENDIF CPUX86}
      {$IFDEF CPUX64}
      // This file should be located in directory jcl/source/include
      // It is automatically created by the JCL installer
      // For manual installations, copy and adjust jcl/source/include/jcl.template.inc
      {$I jcld17win64.inc}
      {$ENDIF CPUX64}
      {$DEFINE JCL_CONFIGURED}
      {$ENDIF MSWINDOWS}
    {$ENDIF BDS10}
    {----------------------------}
    {$IFDEF BDS11}
      {$IFDEF MSWINDOWS}
      {$IFDEF CPUX86}
      // This file should be located in directory jcl/source/include
      // It is automatically created by the JCL installer
      // For manual installations, copy and adjust jcl/source/include/jcl.template.inc
      {$I jcld18win32.inc}
      {$ENDIF CPUX86}
      {$IFDEF CPUX64}
      // This file should be located in directory jcl/source/include
      // It is automatically created by the JCL installer
      // For manual installations, copy and adjust jcl/source/include/jcl.template.inc
      {$I jcld18win64.inc}
      {$ENDIF CPUX64}
      {$DEFINE JCL_CONFIGURED}
      {$ENDIF MSWINDOWS}
    {$ENDIF BDS11}
    {----------------------------}
    {$IFDEF BDS12}
      {$IFDEF MSWINDOWS}
      {$IFDEF CPUX86}
      // This file should be located in directory jcl/source/include
      // It is automatically created by the JCL installer
      // For manual installations, copy and adjust jcl/source/include/jcl.template.inc
      {$I jcld19win32.inc}
      {$ENDIF CPUX86}
      {$IFDEF CPUX64}
      // This file should be located in directory jcl/source/include
      // It is automatically created by the JCL installer
      // For manual installations, copy and adjust jcl/source/include/jcl.template.inc
      {$I jcld19win64.inc}
      {$ENDIF CPUX64}
      {$DEFINE JCL_CONFIGURED}
      {$ENDIF MSWINDOWS}
    {$ENDIF BDS12}
    {----------------------------}
    {$IFDEF BDS14}
      {$IFDEF MSWINDOWS}
      {$IFDEF CPUX86}
      // This file should be located in directory jcl/source/include
      // It is automatically created by the JCL installer
      // For manual installations, copy and adjust jcl/source/include/jcl.template.inc
      {$I jcld20win32.inc}
      {$ENDIF CPUX86}
      {$IFDEF CPUX64}
      // This file should be located in directory jcl/source/include
      // It is automatically created by the JCL installer
      // For manual installations, copy and adjust jcl/source/include/jcl.template.inc
      {$I jcld20win64.inc}
      {$ENDIF CPUX64}
      {$DEFINE JCL_CONFIGURED}
      {$ENDIF MSWINDOWS}
    {$ENDIF BDS14}
    {----------------------------}
    {$IFDEF FPC}
      // This file should be located in directory jcl/source/include
      // It is automatically created by the JCL installer
      // For manual installations, copy and adjust jcl/source/include/jcl.template.inc
      {$I jclfpc.inc}
      {$DEFINE JCL_CONFIGURED}
    {$ENDIF FPC}
    {----------------------------}
  {$ENDIF ~CLR}

  // check configuration
  {$IFNDEF JCL_CONFIGURED}
    {$IFDEF SUPPORTS_COMPILETIME_MESSAGES}
      {$MESSAGE FATAL 'Your Delphi/BCB version is not supported by this JCL version!'}
    {$ELSE ~SUPPORTS_COMPILETIME_MESSAGES}
      'Your Delphi/BCB version is not supported by this JCL version!'
    {$ENDIF ~SUPPORTS_COMPILETIME_MESSAGES}
  {$ENDIF !JCL_CONFIGURED}

{$ENDIF ~JCLINSTALL}

// Math precision selection, mutually exclusive
{$IFDEF MATH_EXTENDED_PRECISION}
  {$UNDEF MATH_SINGLE_PRECISION}
  {$UNDEF MATH_DOUBLE_PRECISION}
{$ENDIF}
{$IFDEF MATH_DOUBLE_PRECISION}
  {$UNDEF MATH_SINGLE_PRECISION}
  {$UNDEF MATH_EXTENDED_PRECISION}
{$ENDIF}
{$IFDEF MATH_SINGLE_PRECISION}
  {$UNDEF MATH_DOUBLE_PRECISION}
  {$UNDEF MATH_EXTENDED_PRECISION}
{$ENDIF}

{$IFNDEF MATH_EXTENDED_PRECISION}
  {$IFNDEF MATH_DOUBLE_PRECISION}
    {$IFNDEF MATH_SINGLE_PRECISION}
      {$IFDEF FPC}
        {$IFDEF CPU64}
          {$DEFINE MATH_DOUBLE_PRECISION}
        {$ELSE ~CPU64}
          {$DEFINE MATH_EXTENDED_PRECISION}
        {$ENDIF ~CPU64}
      {$ELSE ~FPC}
        {$DEFINE MATH_EXTENDED_PRECISION}
      {$ENDIF ~FPC}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF MATH_EXTENDED_PRECISION}
  {$IFDEF CPU64}
    {$IFDEF FPC}
      {$MESSAGE FATAL 'FPC does not support extended floating points on x86_64!'}
    {$ENDIF FPC}
  {$ENDIF CPU64}
{$ENDIF MATH_EXTENDED_PRECISION}

// PCRE options, mutually exclusive
{$IFDEF PCRE_STATICLINK}
  {$UNDEF PCRE_LINKDLL}
  {$UNDEF PCRE_LINKONREQUEST}
  {$UNDEF PCRE_RTL}
{$ENDIF PCRE_STATICLINK}
{$IFDEF PCRE_LINKDLL}
  {$UNDEF PCRE_LINKONREQUEST}
  {$UNDEF PCRE_RTL}
{$ENDIF PCRE_LINKDLL}
{$IFDEF PCRE_LINKONREQUEST}
  {$UNDEF PCRE_RTL}
{$ENDIF PCRE_LINKONREQUEST}

{$IFNDEF PCRE_STATICLINK}
  {$IFNDEF PCRE_LINKDLL}
    {$IFNDEF PCRE_LINKONREQUEST}
      {$IFNDEF PCRE_RTL}
        {$DEFINE PCRE_LINKONREQUEST}
      {$ENDIF ~PCRE_RTL}
    {$ENDIF ~PCRE_LINKONREQUEST}
  {$ENDIF ~PCRE_LINKDLL}
{$ENDIF ~PCRE_STATICLINK}

{$IFNDEF PCRE_STATICLINK}
  {$IFNDEF PCRE_RTL}
    {$DEFINE PCRE_EXPORT_CDECL}
  {$ENDIF ~PCRE_RTL}
{$ENDIF ~PCRE_STATICLINK}

{$IFNDEF PCRE_STATICLINK}
  {$UNDEF PCRE_16}
{$ENDIF ~PCRE_STATICLINK}

{$IFNDEF PCRE_8}
  {$IFNDEF PCRE_16}
    {$DEFINE PCRE_RTL}
    {$UNDEF PCRE_STATICLINK}
    {$UNDEF PCRE_LINKDLL}
    {$UNDEF PCRE_LINKONREQUEST}
  {$ENDIF ~PCRE_16}
{$ENDIF ~PCRE_8}

{$IFDEF PCRE_8}
  {$IFDEF PCRE_16}
    {$IFDEF PCRE_PREFER_16}
      {$DEFINE JCL_PCRE_16}
    {$ELSE ~PCRE_PREFER_16}
      {$DEFINE JCL_PCRE_8}
    {$ENDIF ~PCRE_PREFER_16}
  {$ELSE ~PCRE_16}
    {$DEFINE JCL_PCRE_8}
  {$ENDIF ~PCRE_16}
{$ELSE ~PCRE_8}
  {$IFDEF PCRE_16}
    {$DEFINE JCL_PCRE_16}
  {$ENDIF PCRE_16}
{$ENDIF ~PCRE_8}

{$IFDEF JCL_PCRE_8}
  {$DEFINE JCL_PCRE}
{$ENDIF JCL_PCRE_8}

{$IFDEF JCL_PCRE_16}
  {$DEFINE JCL_PCRE}
{$ENDIF JCL_PCRE_16}

// BZip2 options
{$IFDEF BZIP2_STATICLINK}
  {$UNDEF BZIP2_LINKDLL}
  {$UNDEF BZIP2_LINKONREQUEST}
{$ENDIF BZIP2_STATICLINK}
{$IFDEF BZIP2_LINKDLL}
  {$UNDEF BZIP2_LINKONREQUEST}
{$ENDIF BZIP2_LINKDLL}

{$IFNDEF BZIP2_STATICLINK}
  {$IFNDEF BZIP2_LINKDLL}
    {$IFNDEF BZIP2_LINKONREQUEST}
      {$DEFINE BZIP2_STATICLINK}
    {$ENDIF ~BZIP2_LINKONREQUEST}
  {$ENDIF ~BZIP2_LINKDLL}
{$ENDIF ~BZIP2_STATICLINK}

{$IFDEF BZIP2_STATICLINK}
  {$DEFINE BZIP2_EXPORT_STDCALL}
{$ENDIF BZIP2_STATICLINK}

{$IFDEF BZIP2_LINKDLL}
  {$DEFINE BZIP2_EXPORT_CDECL}
{$ENDIF BZIP2_LINKDLL}

{$IFDEF BZIP2_LINKONREQUEST}
  {$DEFINE BZIP2_EXPORT_CDECL}
{$ENDIF BZIP2_LINKONREQUEST}


// ZLib options
{$IFDEF ZLIB_STATICLINK}
  {$UNDEF ZLIB_LINKDLL}
  {$UNDEF ZLIB_LINKONREQUEST}
  {$UNDEF ZLIB_RTL}
{$ENDIF ZLIB_STATICLINK}
{$IFDEF ZLIB_LINKDLL}
  {$UNDEF ZLIB_LINKONREQUEST}
  {$UNDEF ZLIB_RTL}
{$ENDIF ZLIB_LINKDLL}
{$IFDEF ZLIB_LINKONREQUEST}
  {$UNDEF ZLIB_RTL}
{$ENDIF ZLIB_LINKONREQUEST}

{$IFNDEF ZLIB_STATICLINK}
  {$IFNDEF ZLIB_LINKDLL}
    {$IFNDEF ZLIB_LINKONREQUEST}
      {$IFNDEF ZLIB_RTL}
        {$DEFINE ZLIB_STATICLINK}
      {$ENDIF ~ZLIB_RTL}
    {$ENDIF ~ZLIB_LINKONREQUEST}
  {$ENDIF ~ZLIB_LINKDLL}
{$ENDIF ~ZLIB_STATICLINK}

{$IFDEF ZLIB_LINKDLL}
  {$DEFINE ZLIB_EXPORT_CDECL}
{$ENDIF ZLIB_LINKDLL}
{$IFDEF ZLIB_LINKONREQUEST}
  {$DEFINE ZLIB_EXPORT_CDECL}
{$ENDIF ZLIB_LINKONREQUEST}
// calling convention for static link is fastcall

{$IFNDEF HAS_UNIT_CHARACTER}
  {$UNDEF UNICODE_RTL_DATABASE}
{$ENDIF ~HAS_UNIT_CHARACTER}

{$IFDEF UNICODE_RAW_DATA}
  {$UNDEF UNICODE_ZLIB_DATA}
  {$UNDEF UNICODE_BZIP2_DATA}
{$ENDIF UNICODE_RAW_DATA}

{$IFDEF UNICODE_ZLIB_DATA}
  {$UNDEF UNICODE_RAW_DATA}
  {$UNDEF UNICODE_BZIP2_DATA}
{$ENDIF UNICODE_ZLIB_DATA}

{$IFNDEF UNICODE_ZLIB_DATA}
  {$IFNDEF UNICODE_BZIP2_DATA}
    {$DEFINE UNICODE_RAW_DATA}
  {$ENDIF ~UNICODE_BZIP2_DATA}
{$ENDIF ~UNICODE_ZLIB_DATA}

{$IFDEF CONTAINER_ANSISTR}
  {$UNDEF CONTAINER_WIDESTR}
  {$UNDEF CONTAINER_UNICODESTR}
  {$UNDEF CONTAINER_NOSTR}
{$ENDIF CONTAINER_ANSISTR}

{$IFDEF CONTAINER_WIDESTR}
  {$UNDEF CONTAINER_UNICODESTR}
  {$UNDEF CONTAINER_NOSTR}
{$ENDIF CONTAINER_WIDESTR}

{$IFDEF CONTAINER_UNICODESTR}
  {$UNDEF CONTAINER_NOSTR}
{$ENDIF CONTAINER_UNICODESTR}

{$IFNDEF CONTAINER_ANSISTR}
  {$IFNDEF CONTAINER_WIDESTR}
    {$IFNDEF CONTAINER_UNICODESTR}
      {$IFNDEF CONTAINER_NOSTR}
        {$IFDEF SUPPORTS_UNICODE_STRING}
          {$DEFINE CONTAINER_UNICODESTR}
        {$ELSE ~SUPPORTS_UNICODE_STRING}
          {$DEFINE CONTAINER_ANSISTR}
        {$ENDIF ~SUPPORTS_UNICODE_STRING}
      {$ENDIF ~CONTAINER_NOSTR}
    {$ENDIF ~CONTAINER_UNICODESTR}
  {$ENDIF ~CONTAINER_WIDESTR}
{$ENDIF ~CONTAINER_ANSISTR}

// 7zip options
{$IFDEF 7ZIP_STATICLINK}
  {$UNDEF 7ZIP_LINKDLL}
  {$UNDEF 7ZIP_LINKONREQUEST}
{$ENDIF 7ZIP_STATICLINK}

{$IFDEF 7ZIP_LINKDLL}
  {$UNDEF 7ZIP_LINKONREQUEST}
{$ENDIF 7ZIP_LINKDLL}

{$IFNDEF 7ZIP_STATICLINK}
  {$IFNDEF 7ZIP_LINKDLL}
    {$IFNDEF 7ZIP_LINKONREQUEST}
      {$DEFINE 7ZIP_LINKONREQUEST}
    {$ENDIF ~7ZIP_LINKONREQUEST}
  {$ENDIF ~7ZIP_LINKDLL}
{$ENDIF ~7ZIP_STATICLINK}

{$IFDEF SUPPORTS_UNSAFE_WARNINGS}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}

{$IFNDEF DROP_OBSOLETE_CODE}
  {$IFNDEF JCLINSTALL}
    {$DEFINE KEEP_DEPRECATED}
  {$ENDIF}
{$ENDIF}

{$IFDEF CLR}
 {$WARN UNSAFE_TYPE ON}
 {$WARN UNSAFE_CODE ON}
 {$WARN UNSAFE_CAST ON}
 {$WARN UNIT_PLATFORM OFF}

 {$DEFINE MSWINDOWS}
 {$DEFINE PIC}
 {$DEFINE PUREPASCAL}
{$ENDIF CLR}

// Delphi 2005 has a compiler internal failure when compiling the JCL with UNITVERSIONING enabled
{$IFDEF DELPHI2005}
  {$UNDEF UNITVERSIONING}
{$ENDIF DELPHI2005}

{$IFDEF FPC}
  {$DEFINE DEBUG_NO_TD32}
{$ENDIF FPC}

