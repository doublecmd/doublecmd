-----------Summary
Charset Detector - as the name says - is a stand alone executable module for automatic charset detection of a given text.
It can be useful for internationalisation support in multilingual applications such as web-script editors or Unicode editors.
Given input buffer will be analysed to guess used encoding. The result can be used as control parameter for charset conversation procedure.
Charset Detector can be compiled (and hopefully used) for MS Windows (as dll - dynamic link library) or Linux.
Based on Mozilla's i18n component - http://www.mozilla.org/projects/intl/.

-----------State
Version 0.2.6 stable.
The latest version can be found at http://chsdet.sourceforge.net.

-----------Requirements
Charset Detector doesn't need any external components.

-----------Output
As result you will get guessed charset as MS Windows Code Page id and charset name.

-----------Licence
Charset Detector is open source project and distributed under Lesser GPL.
See the GNU Lesser General Public License for more details - http://www.opensource.org/licenses/lgpl-license.php

-----------Supported charsets

 +-----------+---------------------------+------------------------+
 | Code pade |           Name            |      Note              |
 +-----------+---------------------------+------------------------+
 |      0    |  ASCII                    |   Pseudo code page.    |
 |    855    |  IBM855                   |                        |
 |    866    |  IBM866                   |                        |
 |    932    |  Shift_JIS                |                        |
 |    950    |  Big5                     |                        |
 |   1200    |  UTF-16LE                 |                        |
 |   1201    |  UTF-16BE                 |                        |
 |   1251    |  windows-1251             |                        |
 |   1252    |  windows-1252             |                        |
 |   1253    |  windows-1253             |                        |
 |   1255    |  windows-1255             |                        |
 |  10007    |  x-mac-cyrillic           |                        |
 |  12000    |  X-ISO-10646-UCS-4-2143   |                        |
 |  12000    |  UTF-32LE                 |   MS Windows hasn't CP.|
 |           |                           |   Try to use USC-4.    |
 |  12001    |  X-ISO-10646-UCS-4-3412   |                        |
 |  12001    |  UTF-32BE                 |   MS Windows hasn't CP.|
 |           |                           |   Try to use USC-4.    |
 |  20866    |  KOI8-R                   |                        |
 |  28595    |  ISO-8859-5               |                        |
 |  28595    |  ISO-8859-5               |                        |
 |  28597    |  ISO-8859-7               |                        |
 |  28598    |  ISO-8859-8               |                        |
 |  50222    |  ISO-2022-JP              |                        |
 |  50225    |  ISO-2022-KR              |                        |
 |  50227    |  ISO-2022-CN              |                        |
 |  51932    |  EUC-JP                   |                        |
 |  51936    |  x-euc-tw                 |                        |
 |  51949    |  EUC-KR                   |                        |
 |  52936    |  HZ-GB-2312               |                        |
 |  54936    |  GB18030                  |                        |
 |  65001    |  UTF-8                    |                        |
 +-----------+---------------------------+------------------------+
  
-----------Types
Return values

  NS_OK = 0;
  NS_ERROR_OUT_OF_MEMORY = $8007000e;

Returned types

  rCharsetInfo = record
  	Name: pChar;				// charset GNU canonical name
    CodePage: integer;			// MS Windows CodePage id
    Language: pChar;			// 
  end;

  rAboutHolder = record
    MajorVersionNr: Cardinal;	// Library's Major Version #
    MinorVersionNr: Cardinal;	// Library's Minor Version #
    BuildVersionNr: Cardinal;	// Library's Build/Release Version #
    About: pChar;               // Copyleft information; 
  end;

-----------Exported functions
  procedure chsd_Reset; stdcall; 
  Reset Charset Detector state. Prepare to new analyse.
   
  function chsd_HandleData(aBuf: PChar; aLen: integer): integer; stdcall; 
  Analyse given buffer.
  Parameters
  	aBuf - pointer to buffer with text.
	sLen - buffer length; 
  Return value 
  	NS_ERROR_OUT_OF_MEMORY - failure. Unable to create internal objects.
  	NS_OK - success.
  Note
  	Function can be called more that one time to continue guessing. Charset Detector 
	remember last state until chsd_Reset called.
	 	
  function chsd_Done: Boolean; stdcall; 
  Return value
    TRUE - Charset Detector is sure about text encoding.
    FALSE - Overwise.
  Note
  	If input buffer is smaller then 1K Charset Detector returns anyway FALSE.
	  	
  procedure chsd_DataEnd; stdcall; 
  Signalise data end. If Charset Detector hasn't sure result (Done = FALSE) 
  the best guessed encoding will be set as result.
  
  function chsd_GetDetectedCharset: rCharsetInfo; stdcall; 
  Returns guessed charset.
  
  procedure chsd_GetKnownCharsets(var KnownCharsets: pChar); 
  Fills the parameter with all supported charsets in form
  "CodePage - Name LineFeed".
  
  procedure chsd_GetAbout(var About: rAboutHolder); stdcall; 
  Fills the parameter with version and copyleft information.
  		
-----------Sample
  The definition file "chsd_dll_intf.pas" can be found in the same direcory.
  Bellow is small usage sample.
  
  // WS: WideString; // Wide string which can be used in Unicode controls.
  
  // Get encoding of some buffer
  chsd_Reset;	
  chsd_HandleData(aBuf, aLen);

  if not chsd_Done then
    chsd_DataEnd;

  ChSInfo := chsd_GetDetectedCharset();
  
  // convert buffer to WideString
  OutputLength := MultiByteToWideChar(ChSInfo.CodePage, 0, aBuf, aLen, nil, 0);
  SetLength(WS, OutputLength);
  MultiByteToWideChar(ChSInfo.CodePage, 0, aBuf, aLen, PWideChar(WS), OutputLength);
  
  // If you using Unicode SynEdit
  SynEdit.Lines.Text := WS;
  
Nikolaj Yakowlew © 2006-2008 
