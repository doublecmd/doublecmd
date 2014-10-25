{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JclCharsets.pas.                                                            }
{                                                                                                  }
{ The Initial Developer of the Original Code is Florent Ouchet.                                    }
{ Portions created by Florent Ouchet are Copyright Florent Ouchet. All rights reserved.            }
{                                                                                                  }
{ Contributors:                                                                                    }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Windows codepage bindings are taken from IE5 ones:                                               }
{      http://msdn.microsoft.com/en-us/library/aa752010(VS.85).aspx                                }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclCharsets;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclBase;

type
  EJclCharsetError = class(EJclError);

const
  CP_UTF16LE = 1200;

type
  TJclCharsetInfo = record
    Name: string;
    CodePage: Word;
    FamilyCodePage: Word;
  end;

const JclCharsetInfos: array [0..285] of TJclCharsetInfo =
(* Arabic (ASMO 708) ASMO-708 708 1256 *)
    ((Name: 'ASMO-708'; CodePage: 708; FamilyCodePage: 1256),
(* Arabic (DOS) DOS-720 720 1256 *)
     (Name: 'DOS-720'; CodePage: 720; FamilyCodePage: 1256),
(* Arabic (ISO) iso-8859-6 arabic, csISOLatinArabic, ECMA-114, ISO_8859-6, ISO_8859-6:1987, iso-ir-127 28596 1256 *)
     (Name: 'iso-8859-6'; CodePage: 28596; FamilyCodePage: 1256),
     (Name: 'arabic'; CodePage: 28596; FamilyCodePage: 1256),
     (Name: 'csISOLatinArabic'; CodePage: 28596; FamilyCodePage: 1256),
     (Name: 'ECMA-114'; CodePage: 28596; FamilyCodePage: 1256),
     (Name: 'ISO_8859-6'; CodePage: 28596; FamilyCodePage: 1256),
     (Name: 'ISO_8859-6:1987'; CodePage: 28596; FamilyCodePage: 1256),
     (Name: 'iso-ir-127'; CodePage: 28596; FamilyCodePage: 1256),
(* Arabic (Mac) x-mac-arabic 10004 1256 *)
     (Name: 'x-mac-arabic'; CodePage: 10004; FamilyCodePage: 1256),
(* Arabic (Windows) windows-1256 cp1256 1256 1256 *)
     (Name: 'windows-1256'; CodePage: 1256; FamilyCodePage: 1256),
     (Name: 'cp1256'; CodePage: 1256; FamilyCodePage: 1256),
(* Baltic (DOS) ibm775 CP500 775 1257 *)
     (Name: 'ibm775'; CodePage: 775; FamilyCodePage: 1257),
     (Name: 'CP500'; CodePage: 775; FamilyCodePage: 1257),
(* Baltic (ISO) iso-8859-4 csISOLatin4, ISO_8859-4, ISO_8859-4:1988, iso-ir-110, l4, latin4 28594 1257 *)
     (Name: 'iso-8859-4'; CodePage: 28594; FamilyCodePage: 1257),
     (Name: 'csISOLatin4'; CodePage: 28594; FamilyCodePage: 1257),
     (Name: 'ISO_8859-4'; CodePage: 28594; FamilyCodePage: 1257),
     (Name: 'ISO_8859-4:1988'; CodePage: 28594; FamilyCodePage: 1257),
     (Name: 'iso-ir-110'; CodePage: 28594; FamilyCodePage: 1257),
     (Name: 'l4'; CodePage: 28594; FamilyCodePage: 1257),
     (Name: 'latin4'; CodePage: 28594; FamilyCodePage: 1257),
(* Baltic (Windows) windows-1257 1257 1257 *)
     (Name: 'windows-1257'; CodePage: 1257; FamilyCodePage: 1257),
(* Central European (DOS) ibm852 cp852 852 1250 *)
     (Name: 'ibm852'; CodePage: 852; FamilyCodePage: 1250),
     (Name: 'cp852'; CodePage: 852; FamilyCodePage: 1250),
(* Central European (ISO) iso-8859-2 csISOLatin2, iso_8859-2, iso_8859-2:1987, iso8859-2, iso-ir-101, l2, latin2 28592 1250 *)
     (Name: 'iso-8859-2'; CodePage: 28592; FamilyCodePage: 1250),
     (Name: 'csISOLatin2'; CodePage: 28592; FamilyCodePage: 1250),
     (Name: 'iso_8859-2'; CodePage: 28592; FamilyCodePage: 1250),
     (Name: 'iso_8859-2:1987'; CodePage: 28592; FamilyCodePage: 1250),
     (Name: 'iso8859-2'; CodePage: 28592; FamilyCodePage: 1250),
     (Name: 'iso-ir-101'; CodePage: 28592; FamilyCodePage: 1250),
     (Name: 'l2'; CodePage: 28592; FamilyCodePage: 1250),
     (Name: 'latin2'; CodePage: 28592; FamilyCodePage: 1250),
(* Central European (Mac) x-mac-ce 10029 1250 *)
     (Name: 'x-mac-ce'; CodePage: 10029; FamilyCodePage: 1250),
(* Central European (Windows) windows-1250 x-cp1250 1250 1250 *)
     (Name: 'windows-1250'; CodePage: 1250; FamilyCodePage: 1250),
     (Name: 'x-cp1250'; CodePage: 1250; FamilyCodePage: 1250),
(* Chinese Simplified (EUC) EUC-CN x-euc-cn 51936 936 *)
     (Name: 'EUC-CN'; CodePage: 51936; FamilyCodePage: 936),
     (Name: 'x-euc-cn'; CodePage: 51936; FamilyCodePage: 936),
(* Chinese Simplified (GB2312) gb2312 chinese, CN-GB, csGB2312, csGB231280, csISO58GB231280, GB_2312-80, GB231280, GB2312-80, GBK, iso-ir-58 936 936 *)
     (Name: 'gb2312'; CodePage: 936; FamilyCodePage: 936),
     (Name: 'chinese'; CodePage: 936; FamilyCodePage: 936),
     (Name: 'CN-GB'; CodePage: 936; FamilyCodePage: 936),
     (Name: 'csGB2312'; CodePage: 936; FamilyCodePage: 936),
     (Name: 'csGB231280'; CodePage: 936; FamilyCodePage: 936),
     (Name: 'csISO58GB231280'; CodePage: 936; FamilyCodePage: 936),
     (Name: 'GB_2312-80'; CodePage: 936; FamilyCodePage: 936),
     (Name: 'GB231280'; CodePage: 936; FamilyCodePage: 936),
     (Name: 'GB2312-80'; CodePage: 936; FamilyCodePage: 936),
     (Name: 'GBK'; CodePage: 936; FamilyCodePage: 936),
     (Name: 'iso-ir-58'; CodePage: 936; FamilyCodePage: 936),
(* Chinese Simplified (HZ) hz-gb-2312 52936 936 *)
     (Name: 'hz-gb-2312'; CodePage: 52936; FamilyCodePage: 936),
(* Chinese Simplified (Mac) x-mac-chinesesimp 10008 936 *)
     (Name: 'x-mac-chinesesimp'; CodePage: 10008; FamilyCodePage: 936),
(* Chinese Traditional (Big5) big5 cn-big5, csbig5, x-x-big5 950 950 *)
     (Name: 'big5'; CodePage: 950; FamilyCodePage: 950),
     (Name: 'cn-big5'; CodePage: 950; FamilyCodePage: 950),
     (Name: 'csbig5'; CodePage: 950; FamilyCodePage: 950),
     (Name: 'x-x-big5'; CodePage: 950; FamilyCodePage: 950),
     (Name: 'MS950'; CodePage: 950; FamilyCodePage: 950),
(* Chinese Traditional (CNS) x-Chinese-CNS 20000 950 *)
     (Name: 'x-Chinese-CNS'; CodePage: 20000; FamilyCodePage: 950),
(* Chinese Traditional (Eten) x-Chinese-Eten 20002 950 *)
     (Name: 'x-Chinese-Eten'; CodePage: 20002; FamilyCodePage: 950),
(* Chinese Traditional (Mac) x-mac-chinesetrad 10002 950 *)
     (Name: 'x-mac-chinesetrad'; CodePage: 10002; FamilyCodePage: 950),
(* Cyrillic (DOS) cp866 ibm866 866 1251 *)
     (Name: 'cp866'; CodePage: 866; FamilyCodePage: 1251),
     (Name: 'ibm866'; CodePage: 866; FamilyCodePage: 1251),
(* Cyrillic (ISO) iso-8859-5 csISOLatin5, csISOLatinCyrillic, cyrillic, ISO_8859-5, ISO_8859-5:1988, iso-ir-144, l5 28595 1251 *)
     (Name: 'iso-8859-5'; CodePage: 28595; FamilyCodePage: 1251),
     (Name: 'csISOLatinCyrillic'; CodePage: 28595; FamilyCodePage: 1251),
     (Name: 'cyrillic'; CodePage: 28595; FamilyCodePage: 1251),
     (Name: 'ISO_8859-5'; CodePage: 28595; FamilyCodePage: 1251),
     (Name: 'ISO_8859-5:1988'; CodePage: 28595; FamilyCodePage: 1251),
     (Name: 'iso-ir-144'; CodePage: 28595; FamilyCodePage: 1251),
(* Cyrillic (KOI8-R) koi8-r csKOI8R, koi, koi8, koi8r 20866 1251 *)
     (Name: 'koi8-r'; CodePage: 20866; FamilyCodePage: 1251),
     (Name: 'csKOI8R'; CodePage: 20866; FamilyCodePage: 1251),
     (Name: 'koi'; CodePage: 20866; FamilyCodePage: 1251),
     (Name: 'koi8'; CodePage: 20866; FamilyCodePage: 1251),
     (Name: 'koi8r'; CodePage: 20866; FamilyCodePage: 1251),
(* Cyrillic (KOI8-U) koi8-u koi8-ru 21866 1251 *)
     (Name: 'koi8-u'; CodePage: 21866; FamilyCodePage: 1251),
     (Name: 'koi8-ru'; CodePage: 21866; FamilyCodePage: 1251),
(* Cyrillic (Mac) x-mac-cyrillic 10007 1251 *)
     (Name: 'x-mac-cyrillic'; CodePage: 10007; FamilyCodePage: 1251),
(* Cyrillic (Windows) windows-1251 x-cp1251 1251 1251 *)
     (Name: 'windows-1251'; CodePage: 1251; FamilyCodePage: 1251),
     (Name: 'x-cp1251'; CodePage: 1251; FamilyCodePage: 1251),
(* Europa x-Europa 29001 1252 *)
     (Name: 'x-Europa'; CodePage: 29001; FamilyCodePage: 1252),
(* German (IA5) x-IA5-German 20106 1252 *)
     (Name: 'x-IA5-German'; CodePage: 20106; FamilyCodePage: 1252),
(* Greek (DOS) ibm737 737 1253 *)
     (Name: 'ibm737'; CodePage: 737; FamilyCodePage: 1253),
(* Greek (ISO) iso-8859-7 csISOLatinGreek, ECMA-118, ELOT_928, greek, greek8, ISO_8859-7, ISO_8859-7:1987, iso-ir-126 28597 1253 *)
     (Name: 'iso-8859-7'; CodePage: 28597; FamilyCodePage: 1253),
     (Name: 'csISOLatinGreek'; CodePage: 28597; FamilyCodePage: 1253),
     (Name: 'ECMA-118'; CodePage: 28597; FamilyCodePage: 1253),
     (Name: 'ELOT_928'; CodePage: 28597; FamilyCodePage: 1253),
     (Name: 'greek'; CodePage: 28597; FamilyCodePage: 1253),
     (Name: 'greek8'; CodePage: 28597; FamilyCodePage: 1253),
     (Name: 'ISO_8859-7'; CodePage: 28597; FamilyCodePage: 1253),
     (Name: 'ISO_8859-7:1987'; CodePage: 28597; FamilyCodePage: 1253),
     (Name: 'iso-ir-126'; CodePage: 28597; FamilyCodePage: 1253),
(* Greek (Mac) x-mac-greek 10006 1253 *)
     (Name: 'x-mac-greek'; CodePage: 10006; FamilyCodePage: 1253),
(* Greek (Windows) windows-1253 1253 1253 *)
     (Name: 'windows-1253'; CodePage: 1253; FamilyCodePage: 1253),
(* Greek, Modern (DOS) ibm869 869 1253 *)
     (Name: 'ibm869'; CodePage: 869; FamilyCodePage: 1253),
(* Hebrew (DOS) DOS-862 862 1255 *)
     (Name: 'DOS-862'; CodePage: 862; FamilyCodePage: 1255),
(* Hebrew (ISO-Logical) iso-8859-8-i logical 38598 1255 *)
     (Name: 'iso-8859-8-i'; CodePage: 38598; FamilyCodePage: 1255),
     (Name: 'logical'; CodePage: 38598; FamilyCodePage: 1255),
(* Hebrew (ISO-Visual) iso-8859-8 csISOLatinHebrew, hebrew, ISO_8859-8, ISO_8859-8:1988, ISO-8859-8, iso-ir-138, visual 28598 1255 *)
     (Name: 'iso-8859-8'; CodePage: 28598; FamilyCodePage: 1255),
     (Name: 'csISOLatinHebrew'; CodePage: 28598; FamilyCodePage: 1255),
     (Name: 'hebrew'; CodePage: 28598; FamilyCodePage: 1255),
     (Name: 'ISO_8859-8'; CodePage: 28598; FamilyCodePage: 1255),
     (Name: 'ISO_8859-8:1988'; CodePage: 28598; FamilyCodePage: 1255),
     (Name: 'ISO-8859-8'; CodePage: 28598; FamilyCodePage: 1255),
     (Name: 'iso-ir-138'; CodePage: 28598; FamilyCodePage: 1255),
     (Name: 'visual'; CodePage: 28598; FamilyCodePage: 1255),
(* Hebrew (Mac) x-mac-hebrew 10005 1255 *)
     (Name: 'x-mac-hebrew'; CodePage: 10005; FamilyCodePage: 1255),
(* Hebrew (Windows) windows-1255 ISO_8859-8-I, ISO-8859-8, visual 1255 1255 *)
     (Name: 'windows-1255'; CodePage: 1255; FamilyCodePage: 1255),
     (Name: 'CP1255'; CodePage: 1255; FamilyCodePage: 1255),
     (Name: 'ISO_8859-8-I'; CodePage: 1255; FamilyCodePage: 1255),
     (Name: 'ISO-8859-8'; CodePage: 1255; FamilyCodePage: 1255),
     (Name: 'visual'; CodePage: 1255; FamilyCodePage: 1255),
(* IBM EBCDIC (Arabic) x-EBCDIC-Arabic 20420 1256 *)
     (Name: 'x-EBCDIC-Arabic'; CodePage: 20420; FamilyCodePage: 1256),
(* IBM EBCDIC (Cyrillic Russian) x-EBCDIC-CyrillicRussian 20880 1251 *)
     (Name: 'x-EBCDIC-CyrillicRussian'; CodePage: 20880; FamilyCodePage: 1251),
(* IBM EBCDIC (Cyrillic Serbian-Bulgarian) x-EBCDIC-CyrillicSerbianBulgarian 21025 1251 *)
     (Name: 'x-EBCDIC-CyrillicSerbianBulgarian'; CodePage: 21025; FamilyCodePage: 1251),
(* IBM EBCDIC (Denmark-Norway) x-EBCDIC-DenmarkNorway 20277 1252 *)
     (Name: 'x-EBCDIC-DenmarkNorway'; CodePage: 20277; FamilyCodePage: 1252),
(* IBM EBCDIC (Denmark-Norway-Euro) x-ebcdic-denmarknorway-euro 1142 1252 *)
     (Name: 'x-ebcdic-denmarknorway-euro'; CodePage: 1142; FamilyCodePage: 1252),
(* IBM EBCDIC (Finland-Sweden) x-EBCDIC-FinlandSweden 20278 1252 *)
     (Name: 'x-EBCDIC-FinlandSweden'; CodePage: 20278; FamilyCodePage: 1252),
(* IBM EBCDIC (Finland-Sweden-Euro) x-ebcdic-finlandsweden-euro X-EBCDIC-France 1143 1252 *)
     (Name: 'x-ebcdic-finlandsweden-euro'; CodePage: 1143; FamilyCodePage: 1252),
     (Name: 'X-EBCDIC-France'; CodePage: 1143; FamilyCodePage: 1252),
(* IBM EBCDIC (France-Euro) x-ebcdic-france-euro 1147 1252 *)
     (Name: 'x-ebcdic-france-euro'; CodePage: 1147; FamilyCodePage: 1252),
(* IBM EBCDIC (Germany) x-EBCDIC-Germany 20273 1252 *)
     (Name: 'x-EBCDIC-Germany'; CodePage: 20273; FamilyCodePage: 1252),
(* IBM EBCDIC (Germany-Euro) x-ebcdic-germany-euro 1141 1252 *)
     (Name: 'x-ebcdic-germany-euro'; CodePage: 1141; FamilyCodePage: 1252),
(* IBM EBCDIC (Greek Modern) x-EBCDIC-GreekModern 875 1253 *)
     (Name: 'x-EBCDIC-GreekModern'; CodePage: 875; FamilyCodePage: 1253),
(* IBM EBCDIC (Greek) x-EBCDIC-Greek 20423 1253 *)
     (Name: 'x-EBCDIC-Greek'; CodePage: 20423; FamilyCodePage: 1253),
(* IBM EBCDIC (Hebrew) x-EBCDIC-Hebrew 20424 1255 *)
     (Name: 'x-EBCDIC-Hebrew'; CodePage: 20424; FamilyCodePage: 1255),
(* IBM EBCDIC (Icelandic) x-EBCDIC-Icelandic 20871 1252 *)
     (Name: 'x-EBCDIC-Icelandic'; CodePage: 20871; FamilyCodePage: 1252),
(* IBM EBCDIC (Icelandic-Euro) x-ebcdic-icelandic-euro 1149 1252 *)
     (Name: 'x-ebcdic-icelandic-euro'; CodePage: 1149; FamilyCodePage: 1252),
(* IBM EBCDIC (International-Euro) x-ebcdic-international-euro 1148 1252 *)
     (Name: 'x-ebcdic-international-euro'; CodePage: 1148; FamilyCodePage: 1252),
(* IBM EBCDIC (Italy) x-EBCDIC-Italy 20280 1252 *)
     (Name: 'x-EBCDIC-Italy'; CodePage: 20280; FamilyCodePage: 1252),
(* IBM EBCDIC (Italy-Euro) x-ebcdic-italy-euro 1144 1252 *)
     (Name: 'x-ebcdic-italy-euro'; CodePage: 1144; FamilyCodePage: 1252),
(* IBM EBCDIC (Japanese and Japanese Katakana) x-EBCDIC-JapaneseAndKana 50930 932 *)
     (Name: 'x-EBCDIC-JapaneseAndKana'; CodePage: 50930; FamilyCodePage: 932),
(* IBM EBCDIC (Japanese and Japanese-Latin) x-EBCDIC-JapaneseAndJapaneseLatin 50939 932 *)
     (Name: 'x-EBCDIC-JapaneseAndJapaneseLatin'; CodePage: 50939; FamilyCodePage: 932),
(* IBM EBCDIC (Japanese and US-Canada) x-EBCDIC-JapaneseAndUSCanada 50931 932 *)
     (Name: 'x-EBCDIC-JapaneseAndUSCanada'; CodePage: 50931; FamilyCodePage: 932),
(* IBM EBCDIC (Japanese katakana) x-EBCDIC-JapaneseKatakana 20290 932 *)
     (Name: 'x-EBCDIC-JapaneseKatakana'; CodePage: 20290; FamilyCodePage: 932),
(* IBM EBCDIC (Korean and Korean Extended) x-EBCDIC-KoreanAndKoreanExtended 50933 949 *)
     (Name: 'x-EBCDIC-KoreanAndKoreanExtended'; CodePage: 50933; FamilyCodePage: 949),
(* IBM EBCDIC (Korean Extended) x-EBCDIC-KoreanExtended 20833 949 *)
     (Name: 'x-EBCDIC-KoreanExtended'; CodePage: 20833; FamilyCodePage: 949),
(* IBM EBCDIC (Multilingual Latin-2) CP870 870 1250 *)
     (Name: 'CP870'; CodePage: 870; FamilyCodePage: 1250),
(* IBM EBCDIC (Simplified Chinese) x-EBCDIC-SimplifiedChinese 50935 936 *)
     (Name: 'x-EBCDIC-SimplifiedChinese'; CodePage: 50935; FamilyCodePage: 936),
(* IBM EBCDIC (Spain) X-EBCDIC-Spain 20284 1252 *)
     (Name: 'X-EBCDIC-Spain'; CodePage: 20284; FamilyCodePage: 1252),
(* IBM EBCDIC (Spain-Euro) x-ebcdic-spain-euro 1145 1252 *)
     (Name: 'x-ebcdic-spain-euro'; CodePage: 1145; FamilyCodePage: 1252),
(* IBM EBCDIC (Thai) x-EBCDIC-Thai 20838 874 *)
     (Name: 'x-EBCDIC-Thai'; CodePage: 20838; FamilyCodePage: 874),
(* IBM EBCDIC (Traditional Chinese) x-EBCDIC-TraditionalChinese 50937 950 *)
     (Name: 'x-EBCDIC-TraditionalChinese'; CodePage: 50937; FamilyCodePage: 950),
(* IBM EBCDIC (Turkish Latin-5) CP1026 1026 1254 *)
     (Name: 'CP1026'; CodePage: 1026; FamilyCodePage: 1254),
(* IBM EBCDIC (Turkish) x-EBCDIC-Turkish 20905 1254 *)
     (Name: 'x-EBCDIC-Turkish'; CodePage: 20905; FamilyCodePage: 1254),
(* IBM EBCDIC (UK) x-EBCDIC-UK 20285 1252 *)
     (Name: 'x-EBCDIC-UK'; CodePage: 20285; FamilyCodePage: 1252),
(* IBM EBCDIC (UK-Euro) x-ebcdic-uk-euro 1146 1252 *)
     (Name: 'x-ebcdic-uk-euro'; CodePage: 1146; FamilyCodePage: 1252),
(* IBM EBCDIC (US-Canada) ebcdic-cp-us 37 1252 *)
     (Name: 'ebcdic-cp-us'; CodePage: 37; FamilyCodePage: 1252),
(* IBM EBCDIC (US-Canada-Euro) x-ebcdic-cp-us-euro 1140 1252 *)
     (Name: 'x-ebcdic-cp-us-euro'; CodePage: 1140; FamilyCodePage: 1252),
(* Icelandic (DOS) ibm861 861 1252 *)
     (Name: 'ibm861'; CodePage: 861; FamilyCodePage: 1252),
(* Icelandic (Mac) x-mac-icelandic 10079 1252 *)
     (Name: 'x-mac-icelandic'; CodePage: 10079; FamilyCodePage: 1252),
(* ISCII Assamese x-iscii-as 57006 57006 *)
     (Name: 'x-iscii-as'; CodePage: 57006; FamilyCodePage: 57006),
(* ISCII Bengali x-iscii-be 57003 57003 *)
     (Name: 'x-iscii-be'; CodePage: 57003; FamilyCodePage: 57003),
(* ISCII Devanagari x-iscii-de 57002 57002 *)
     (Name: 'x-iscii-de'; CodePage: 57002; FamilyCodePage: 57002),
(* ISCII Gujarathi x-iscii-gu 57010 57010 *)
     (Name: 'x-iscii-gu'; CodePage: 57010; FamilyCodePage: 57010),
(* ISCII Kannada x-iscii-ka 57008 57008 *)
     (Name: 'x-iscii-ka'; CodePage: 57008; FamilyCodePage: 57008),
(* ISCII Malayalam x-iscii-ma 57009 57009 *)
     (Name: 'x-iscii-ma'; CodePage: 57009; FamilyCodePage: 57009),
(* ISCII Oriya x-iscii-or 57007 57007 *)
     (Name: 'x-iscii-or'; CodePage: 57007; FamilyCodePage: 57007),
(* ISCII Panjabi x-iscii-pa 57011 57011 *)
     (Name: 'x-iscii-pa'; CodePage: 57011; FamilyCodePage: 57011),
(* ISCII Tamil x-iscii-ta 57004 57004 *)
     (Name: 'x-iscii-ta'; CodePage: 57004; FamilyCodePage: 57004),
(* ISCII Telugu x-iscii-te 57005 57005 *)
     (Name: 'x-iscii-te'; CodePage: 57005; FamilyCodePage: 57005),
(* Japanese (EUC) euc-jp csEUCPkdFmtJapanese, Extended_UNIX_Code_Packed_Format_for_Japanese, x-euc, x-euc-jp 51932 932 *)
     (Name: 'euc-jp'; CodePage: 51932; FamilyCodePage: 932),
     (Name: 'csEUCPkdFmtJapanese'; CodePage: 51932; FamilyCodePage: 932),
     (Name: 'Extended_UNIX_Code_Packed_Format_for_Japanese'; CodePage: 51932; FamilyCodePage: 932),
     (Name: 'x-euc'; CodePage: 51932; FamilyCodePage: 932),
     (Name: 'x-euc-jp'; CodePage: 51932; FamilyCodePage: 932),
(* Japanese (JIS) iso-2022-jp 50220 932 *)
     (Name: 'iso-2022-jp'; CodePage: 50220; FamilyCodePage: 932),
(* Japanese (JIS-Allow 1 byte Kana - SO/SI) iso-2022-jp _iso-2022-jp$SIO 50222 932 *)
     (Name: 'iso-2022-jp'; CodePage: 50222; FamilyCodePage: 932),
     (Name: '_iso-2022-jp$SIO'; CodePage: 50222; FamilyCodePage: 932),
(* Japanese (JIS-Allow 1 byte Kana) csISO2022JP _iso-2022-jp 50221 932 *)
     (Name: 'csISO2022JP'; CodePage: 50221; FamilyCodePage: 932),
     (Name: '_iso-2022-jp'; CodePage: 50221; FamilyCodePage: 932),
(* Japanese (Mac) x-mac-japanese 10001 932 *)
     (Name: 'x-mac-japanese'; CodePage: 10001; FamilyCodePage: 932),
(* Japanese (Shift-JIS) shift_jis csShiftJIS, csWindows31J, ms_Kanji, shift-jis, x-ms-cp932, x-sjis 932 932 *)
     (Name: 'shift_jis'; CodePage: 932; FamilyCodePage: 932),
     (Name: 'csShiftJIS'; CodePage: 932; FamilyCodePage: 932),
     (Name: 'csWindows31J'; CodePage: 932; FamilyCodePage: 932),
     (Name: 'ms_Kanji'; CodePage: 932; FamilyCodePage: 932),
     (Name: 'shift-jis'; CodePage: 932; FamilyCodePage: 932),
     (Name: 'x-ms-cp932'; CodePage: 932; FamilyCodePage: 932),
     (Name: 'x-sjis'; CodePage: 932; FamilyCodePage: 932),
     (Name: 'MS932'; CodePage: 932; FamilyCodePage: 932),
(* Korean ks_c_5601-1987 csKSC56011987, euc-kr, iso-ir-149, korean, ks_c_5601, ks_c_5601_1987, ks_c_5601-1989, KSC_5601, KSC5601 949 949 *)
     (Name: 'ks_c_5601-1987'; CodePage: 949; FamilyCodePage: 949),
     (Name: 'csKSC56011987'; CodePage: 949; FamilyCodePage: 949),
     (Name: 'euc-kr'; CodePage: 949; FamilyCodePage: 949),
     (Name: 'iso-ir-149'; CodePage: 949; FamilyCodePage: 949),
     (Name: 'korean'; CodePage: 949; FamilyCodePage: 949),
     (Name: 'ks_c_5601'; CodePage: 949; FamilyCodePage: 949),
     (Name: 'ks_c_5601_1987'; CodePage: 949; FamilyCodePage: 949),
     (Name: 'ks_c_5601-1989'; CodePage: 949; FamilyCodePage: 949),
     (Name: 'KSC_5601'; CodePage: 949; FamilyCodePage: 949),
     (Name: 'KSC5601'; CodePage: 949; FamilyCodePage: 949),
(* Korean (EUC) euc-kr csEUCKR 51949 949 *)
     (Name: 'euc-kr'; CodePage: 51949; FamilyCodePage: 949),
     (Name: 'csEUCKR'; CodePage: 51949; FamilyCodePage: 949),
(* Korean (ISO) iso-2022-kr csISO2022KR 50225 949 *)
     (Name: 'iso-2022-kr'; CodePage: 50225; FamilyCodePage: 949),
     (Name: 'csISO2022KR'; CodePage: 50225; FamilyCodePage: 949),
(* Korean (Johab) Johab 1361 1361 *)
     (Name: 'Johab'; CodePage: 1361; FamilyCodePage: 1361),
(* Korean (Mac) x-mac-korean 10003 949 *)
     (Name: 'x-mac-korean'; CodePage: 10003; FamilyCodePage: 949),
(* Latin 3 (ISO) iso-8859-3 csISOLatin3, ISO_8859-3, ISO_8859-3:1988, iso-ir-109, l3, latin3 28593 1254 *)
     (Name: 'iso-8859-3'; CodePage: 28593; FamilyCodePage: 1254),
     (Name: 'csISOLatin3'; CodePage: 28593; FamilyCodePage: 1254),
     (Name: 'ISO_8859-3'; CodePage: 28593; FamilyCodePage: 1254),
     (Name: 'ISO_8859-3:1988'; CodePage: 28593; FamilyCodePage: 1254),
     (Name: 'iso-ir-109'; CodePage: 28593; FamilyCodePage: 1254),
     (Name: 'l3,'; CodePage: 28593; FamilyCodePage: 1254),
     (Name: 'latin3'; CodePage: 28593; FamilyCodePage: 1254),
(* Latin 9 (ISO) iso-8859-15 csISOLatin9, ISO_8859-15, l9, latin9 28605 1252 *)
     (Name: 'iso-8859-15'; CodePage: 28605; FamilyCodePage: 1252),
     (Name: 'csISOLatin9'; CodePage: 28605; FamilyCodePage: 1252),
     (Name: 'ISO_8859-15'; CodePage: 28605; FamilyCodePage: 1252),
     (Name: 'l9'; CodePage: 28605; FamilyCodePage: 1252),
     (Name: 'latin9'; CodePage: 28605; FamilyCodePage: 1252),
(* Norwegian (IA5) x-IA5-Norwegian 20108 1252 *)
     (Name: 'x-IA5-Norwegian'; CodePage: 20108; FamilyCodePage: 1252),
(* OEM United States IBM437 437, cp437, csPC8, CodePage437 437 1252 *)
     (Name: 'IBM437'; CodePage: 437; FamilyCodePage: 1252),
     (Name: '437'; CodePage: 437; FamilyCodePage: 1252),
     (Name: 'cp437'; CodePage: 437; FamilyCodePage: 1252),
     (Name: 'csPC8'; CodePage: 437; FamilyCodePage: 1252),
     (Name: 'CodePage437'; CodePage: 437; FamilyCodePage: 1252),
(* Swedish (IA5) x-IA5-Swedish 20107 1252 *)
     (Name: 'x-IA5-Swedish'; CodePage: 20107; FamilyCodePage: 1252),
(* Thai (Windows) windows-874 DOS-874, iso-8859-11, TIS-620 874 874 *)
     (Name: 'windows-874'; CodePage: 874; FamilyCodePage: 874),
     (Name: 'DOS-874'; CodePage: 874; FamilyCodePage: 874),
     (Name: 'iso-8859-11'; CodePage: 874; FamilyCodePage: 874),
     (Name: 'TIS-620'; CodePage: 874; FamilyCodePage: 874),
(* Turkish (DOS) ibm857 857 1254 *)
     (Name: 'ibm857'; CodePage: 857; FamilyCodePage: 1254),
(* Turkish (ISO) iso-8859-9 csISOLatin5, ISO_8859-9, ISO_8859-9:1989, iso-ir-148, l5, latin5 28599 1254 *)
     (Name: 'iso-8859-9'; CodePage: 28599; FamilyCodePage: 1254),
     (Name: 'csISOLatin5'; CodePage: 28599; FamilyCodePage: 1254),
     (Name: 'ISO_8859-9'; CodePage: 28599; FamilyCodePage: 1254),
     (Name: 'ISO_8859-9:1989'; CodePage: 28599; FamilyCodePage: 1254),
     (Name: 'iso-ir-148'; CodePage: 28599; FamilyCodePage: 1254),
     (Name: 'l5'; CodePage: 28599; FamilyCodePage: 1254),
     (Name: 'latin5'; CodePage: 28599; FamilyCodePage: 1254),
(* Turkish (Mac) x-mac-turkish 10081 1254 *)
     (Name: 'x-mac-turkish'; CodePage: 10081; FamilyCodePage: 1254),
(* Turkish (Windows) windows-1254 ISO_8859-9, ISO_8859-9:1989, iso-8859-9, iso-ir-148, latin5 1254 1254 *)
     (Name: 'windows-1254'; CodePage: 1254; FamilyCodePage: 1254),
     (Name: 'ISO_8859-9'; CodePage: 1254; FamilyCodePage: 1254),
     (Name: 'ISO_8859-9:1989'; CodePage: 1254; FamilyCodePage: 1254),
     (Name: 'iso-8859-9'; CodePage: 1254; FamilyCodePage: 1254),
     (Name: 'iso-ir-148'; CodePage: 1254; FamilyCodePage: 1254),
(* Unicode unicode utf-16 1200 1200 *)
     (Name: 'utf-16'; CodePage: 1200; FamilyCodePage: 1200),
     (Name: 'unicode'; CodePage: 1200; FamilyCodePage: 1200),
(* Unicode (Big-Endian) unicodeFFFE 1201 1200 *)
     (Name: 'unicodeFFFE'; CodePage: 1201; FamilyCodePage: 1200),
(* Unicode (UTF-7) utf-7 csUnicode11UTF7, unicode-1-1-utf-7, x-unicode-2-0-utf-7 65000 1200 *)
     (Name: 'utf-7'; CodePage: 65000; FamilyCodePage: 1200),
     (Name: 'csUnicode11UTF7'; CodePage: 65000; FamilyCodePage: 1200),
     (Name: 'unicode-1-1-utf-7'; CodePage: 65000; FamilyCodePage: 1200),
     (Name: 'x-unicode-2-0-utf-7'; CodePage: 65000; FamilyCodePage: 1200),
(* Unicode (UTF-8) utf-8 unicode-1-1-utf-8, unicode-2-0-utf-8, x-unicode-2-0-utf-8 65001 1200 *)
     (Name: 'utf-8'; CodePage: 65001; FamilyCodePage: 1200),
     (Name: 'unicode-1-1-utf-8'; CodePage: 65001; FamilyCodePage: 1200),
     (Name: 'unicode-2-0-utf-8'; CodePage: 65001; FamilyCodePage: 1200),
     (Name: 'x-unicode-2-0-utf-8'; CodePage: 65001; FamilyCodePage: 1200),
(* US-ASCII us-ascii ANSI_X3.4-1968, ANSI_X3.4-1986, ascii, cp367, csASCII, IBM367, ISO_646.irv:1991, ISO646-US, iso-ir-6us 20127 1252 *)
     (Name: 'us-ascii'; CodePage: 20127; FamilyCodePage: 1252),
     (Name: 'ANSI_X3.4-1968'; CodePage: 20127; FamilyCodePage: 1252),
     (Name: 'ANSI_X3.4-1986'; CodePage: 20127; FamilyCodePage: 1252),
     (Name: 'ascii'; CodePage: 20127; FamilyCodePage: 1252),
     (Name: 'cp367'; CodePage: 20127; FamilyCodePage: 1252),
     (Name: 'csASCII'; CodePage: 20127; FamilyCodePage: 1252),
     (Name: 'IBM367'; CodePage: 20127; FamilyCodePage: 1252),
     (Name: 'ISO_646.irv:1991'; CodePage: 20127; FamilyCodePage: 1252),
     (Name: 'ISO646-US'; CodePage: 20127; FamilyCodePage: 1252),
     (Name: 'iso-ir-6us'; CodePage: 20127; FamilyCodePage: 1252),
(* Vietnamese (Windows) windows-1258 1258 1258 *)
     (Name: 'windows-1258'; CodePage: 1258; FamilyCodePage: 1258),
(* Western European (DOS) ibm850 850 1252 *)
     (Name: 'ibm850'; CodePage: 850; FamilyCodePage: 1252),
(* Western European (IA5) x-IA5 20105 1252 *)
     (Name: 'x-IA5'; CodePage: 20105; FamilyCodePage: 1252),
(* Western European (ISO) iso-8859-1 cp819, csISOLatin1, ibm819, iso_8859-1, iso_8859-1:1987, iso8859-1, iso-ir-100, l1, latin1 28591 1252 *)
     (Name: 'iso-8859-1'; CodePage: 28591; FamilyCodePage: 1252),
     (Name: 'cp819'; CodePage: 28591; FamilyCodePage: 1252),
     (Name: 'csISOLatin1'; CodePage: 28591; FamilyCodePage: 1252),
     (Name: 'ibm819'; CodePage: 28591; FamilyCodePage: 1252),
     (Name: 'iso_8859-1'; CodePage: 28591; FamilyCodePage: 1252),
     (Name: 'iso_8859-1:1987'; CodePage: 28591; FamilyCodePage: 1252),
     (Name: 'iso8859-1'; CodePage: 28591; FamilyCodePage: 1252),
     (Name: 'iso-ir-100'; CodePage: 28591; FamilyCodePage: 1252),
     (Name: 'l1'; CodePage: 28591; FamilyCodePage: 1252),
     (Name: 'latin1'; CodePage: 28591; FamilyCodePage: 1252),
(* Western European (Mac) macintosh 10000 1252 *)
     (Name: 'macintosh'; CodePage: 10000; FamilyCodePage: 1252),
(* Western European (Windows) Windows-1252 ANSI_X3.4-1968, ANSI_X3.4-1986, ascii, cp367, cp819, csASCII, IBM367, ibm819, ISO_646.irv:1991, iso_8859-1, iso_8859-1:1987, ISO646-US, iso8859-1, iso-8859-1, iso-ir-100, iso-ir-6, latin1, us, us-ascii, x-ansi 1252 1252 *)
     (Name: 'Windows-1252'; CodePage: 1252; FamilyCodePage: 1252),
     (Name: 'ANSI_X3.4-1968'; CodePage: 1252; FamilyCodePage: 1252),
     (Name: 'ANSI_X3.4-1986'; CodePage: 1252; FamilyCodePage: 1252),
     (Name: 'CP1252'; CodePage: 1252; FamilyCodePage: 1252),
     (Name: 'ascii'; CodePage: 1252; FamilyCodePage: 1252),
     (Name: 'cp367'; CodePage: 1252; FamilyCodePage: 1252),
     (Name: 'cp819'; CodePage: 1252; FamilyCodePage: 1252),
     (Name: 'csASCII'; CodePage: 1252; FamilyCodePage: 1252),
     (Name: 'IBM367'; CodePage: 1252; FamilyCodePage: 1252),
     (Name: 'ibm819'; CodePage: 1252; FamilyCodePage: 1252),
     (Name: 'ISO_646.irv:1991'; CodePage: 1252; FamilyCodePage: 1252),
     (Name: 'iso_8859-1'; CodePage: 1252; FamilyCodePage: 1252),
     (Name: 'iso_8859-1:1987'; CodePage: 1252; FamilyCodePage: 1252),
     (Name: 'ISO646-US'; CodePage: 1252; FamilyCodePage: 1252),
     (Name: 'iso8859-1'; CodePage: 1252; FamilyCodePage: 1252),
     (Name: 'iso-8859-1'; CodePage: 1252; FamilyCodePage: 1252),
     (Name: 'iso-ir-100'; CodePage: 1252; FamilyCodePage: 1252),
     (Name: 'iso-ir-6'; CodePage: 1252; FamilyCodePage: 1252),
     (Name: 'latin1'; CodePage: 1252; FamilyCodePage: 1252),
     (Name: 'us'; CodePage: 1252; FamilyCodePage: 1252),
     (Name: 'us-ascii'; CodePage: 1252; FamilyCodePage: 1252),
     (Name: 'x-ansi'; CodePage: 1252; FamilyCodePage: 1252) );

function FamilyCodePageFromCharsetName(const CharsetName: string): Word;
function FamilyCodePageFromCodePage(CodePage: Word): Word;
function CodePageFromCharsetName(const CharsetName: string): Word;
function CharsetInfoFromCharsetName(const CharsetName: string): TJclCharsetInfo;
function CharsetNameFromCodePage(CodePage: Word): string;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\common';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF HAS_UNITSCOPE}
  System.SysUtils,
  {$ELSE ~HAS_UNITSCOPE}
  SysUtils,
  {$ENDIF ~HAS_UNITSCOPE}
  JclResources;

function FamilyCodePageFromCharsetName(const CharsetName: string): Word;
var
  Index: Integer;
  UpperCharsetName: string;
begin
  UpperCharsetName := UpperCase(CharsetName);
  for Index := Low(JclCharsetInfos) to High(JclCharsetInfos) do
    if CompareStr(UpperCharsetName, UpperCase(JclCharsetInfos[Index].Name)) = 0 then
  begin
    Result := JclCharsetInfos[Index].FamilyCodePage;
    Exit;
  end;
  raise EJclCharsetError.CreateRes(@RsENoCharset);
end;

function CodePageFromCharsetName(const CharsetName: string): Word;
var
  Index: Integer;
  UpperCharsetName: string;
begin
  UpperCharsetName := UpperCase(CharsetName);
  for Index := Low(JclCharsetInfos) to High(JclCharsetInfos) do
    if CompareStr(UpperCharsetName, UpperCase(JclCharsetInfos[Index].Name)) = 0 then
  begin
    Result := JclCharsetInfos[Index].CodePage;
    Exit;
  end;
  raise EJclCharsetError.CreateRes(@RsENoCharset);
end;

function CharsetInfoFromCharsetName(const CharsetName: string): TJclCharsetInfo;
var
  Index: Integer;
  UpperCharsetName: string;
begin
  UpperCharsetName := UpperCase(CharsetName);
  for Index := Low(JclCharsetInfos) to High(JclCharsetInfos) do
    if CompareStr(UpperCharsetName, UpperCase(JclCharsetInfos[Index].Name)) = 0 then
  begin
    Result := JclCharsetInfos[Index];
    Exit;
  end;
  raise EJclCharsetError.CreateRes(@RsENoCharset);
end;

function FamilyCodePageFromCodePage(CodePage: Word): Word;
var
  Index: Integer;
begin
  for Index := Low(JclCharsetInfos) to High(JclCharsetInfos) do
    if JclCharsetInfos[Index].CodePage = CodePage then
  begin
    Result := JclCharsetInfos[Index].FamilyCodePage;
    Exit;
  end;
  raise EJclCharsetError.CreateRes(@RsENoCharset);
end;

function CharsetNameFromCodePage(CodePage: Word): string;
var
  Index: Integer;
begin
  for Index := Low(JclCharsetInfos) to High(JclCharsetInfos) do
    if JclCharsetInfos[Index].CodePage = CodePage then
  begin
    Result := JclCharsetInfos[Index].Name;
    Exit;
  end;
  raise EJclCharsetError.CreateRes(@RsENoCharset);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

