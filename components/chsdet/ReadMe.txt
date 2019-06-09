-----------Summary
Charset Detector - as the name says - is a stand alone component for automatic charset detection of a given text.
It can be useful for internationalisation support in multilingual applications such as web-script editors or Unicode editors.
Given input buffer will be analysed to guess used encoding. The result can be used as control parameter for charset conversation procedure.
Based on Mozilla's i18n component - https://dxr.mozilla.org/mozilla/source/extensions/universalchardet/.

-----------State
Version 0.2.9 stable.
Copyright (C) 2011-2019 Alexander Koblov
The latest version can be found at https://sourceforge.net/p/doublecmd/code/HEAD/tree/trunk/components/chsdet/.

-----------Original
Based on
Charset Detector - http://chsdet.sourceforge.net
Copyright (C) 2006-2013 Nikolaj Yakowlew

-----------Requirements
Charset Detector doesn't need any external components.

-----------Output
As result you will get guessed charset as MS Windows Code Page id and charset name.

-----------Licence
Charset Detector is open source project and distributed under GNU LGPL.
See the GNU Lesser General Public License for more details - https://opensource.org/licenses/LGPL-2.1

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
 |  12000    |  UTF-32LE                 |                        |
 |  12001    |  X-ISO-10646-UCS-4-3412   |                        |
 |  12001    |  UTF-32BE                 |                        |
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
    Name: PAnsiChar;      // Charset GNU canonical name
    CodePage: Integer;    // MS Windows CodePage ID
    Language: PAnsiChar;
  end;

-----------Usage sample

Below is a small usage sample in Free Pascal.

function DetectEncoding(const S: String): rCharsetInfo;
var
  Detector: TnsUniversalDetector;
begin
  Detector:= TnsUniversalDetector.Create;
  try
    Detector.Reset;
    Detector.HandleData(PAnsiChar(S), Length(S));
    if not Detector.Done then Detector.DataEnd;
    Result:= Detector.GetDetectedCharsetInfo;
  finally
    FreeAndNil(Detector);
  end;
end;
