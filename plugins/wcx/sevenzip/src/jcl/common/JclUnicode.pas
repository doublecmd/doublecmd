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
{ The Original Code is JclUnicode.pas.                                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is Mike Lischke (public att lischke-online dott de).  }
{ Portions created by Mike Lischke are Copyright (C) 1999-2000 Mike Lischke. All Rights Reserved.  }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Marcel van Brakel                                                                              }
{   Andreas Hausladen (ahuser)                                                                     }
{   Mike Lischke                                                                                   }
{   Flier Lu (flier)                                                                               }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{   Olivier Sannier (obones)                                                                       }
{   Matthias Thoma (mthoma)                                                                        }
{   Petr Vones (pvones)                                                                            }
{   Peter Schraut (http://www.console-dev.de)                                                      }
{   Florent Ouchet (outchy)                                                                        }
{   glchapman                                                                                      }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Various Unicode related routines                                                                 }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclUnicode;

{$I jcl.inc}

// Copyright (c) 1999-2000 Mike Lischke (public att lischke-online dott de)
//

// 10-JUL-2005: (changes by Peter Schraut)
//   - added CodeBlockName, returns the blockname as string
//   - added CodeBlockRange, returns the range of the specified codeblock
//   - updated TUnicodeBlock to reflect changes in unicode 4.1
//   - updated CodeBlockFromChar to reflect changes in unicode 4.1
//   - Notes:
//      Here are a few suggestions to reflect latest namechanges in unicode 4.1,
//      but they were not done due to compatibility with old code:
//      ubGreek should be renamed to ubGreekandCoptic
//      ubCombiningMarksforSymbols should be renamed  to ubCombiningDiacriticalMarksforSymbols
//      ubPrivateUse should be renamed to ubPrivateUseArea
//
//
// 19-SEP-2003: (changes by Andreas Hausladen)
//   - added OWN_WIDESTRING_MEMMGR for faster memory managment in TWideStringList
//     under Windows
//   - fixed: TWideStringList.Destroy does not set OnChange and OnChanging to nil before calling Clear
//
//
// 29-MAR-2002: MT
//   - WideNormalize now returns strings with normalization mode nfNone unchanged.
//   - Bug fix in WideCompose: Raised exception when Result of WideComposeHangul was an
//     empty string. (#0000044)
//   - Bug fix in WideAdjustLineBreaks
//   - Added Asserts were needed.
//   - TWideStrings.IndexOfName now takes care of NormalizeForm as well.
//   - TWideStrings.IndexOf now takes care of NormalizeForm as well.
//   - TWideString.List Find now uses the same NormalizationForm for the search string as it uses
//     within the list itself.
//
// 29-NOV-2001:
//   - bug fix
// 06-JUN-2001:
//   - small changes
// 28-APR-2001:
//   - bug fixes
// 05-APR-2001:
//   - bug fixes
// 23-MAR-2001:
//   - WideSameText
//   - small changes
// 10-FEB-2001:
//   - bug fix in StringToWideStringEx and WideStringToStringEx
// 05-FEB-2001:
//   - TWideStrings.GetSeparatedText changed (no separator anymore after the last line)
// 29-JAN-2001:
//   - PrepareUnicodeData
//   - LoadInProgress critical section is now created at init time to avoid critical thread races
//   - bug fixes
// 26-JAN-2001:
//   - ExpandANSIString
//   - TWideStrings.SaveUnicode is by default True now
// 20..21-JAN-2001:
//   - StrUpperW, StrLowerW and StrTitleW removed because they potentially would need
//     a reallocation to work correctly (use the WideString versions instead)
//   - further improvements related to internal data
//   - introduced TUnicodeBlock
//   - CodeBlockFromChar improved
// 07-JAN-2001:
//   optimized access to character properties, combining class etc.
// 06-JAN-2001:
//   TWideStrings and TWideStringList improved
// APR-DEC 2000: versions 2.1 - 2.6
//   - preparation for public rlease
//   - additional conversion routines
//   - JCL compliance
//   - character properties unified
//   - character properties data and lookup improvements
//   - reworked Unicode data resource file
//   - improved simple string comparation routines (StrCompW, StrLCompW etc., include surrogate fix)
//   - special case folding data for language neutral case insensitive comparations included
//   - optimized decomposition
//   - composition and normalization support
//   - normalization conformance tests applied
//   - bug fixes
// FEB-MAR 2000: version 2.0
//   - Unicode regular expressions (URE) search class (TURESearch)
//   - generic search engine base class for both the Boyer-Moore and the RE search class
//   - whole word only search in UTBM, bug fixes in UTBM
//   - string decompositon (including hangul)
// OCT/99 - JAN/2000: version 1.0
//   - basic Unicode implementation, more than 100 WideString/UCS2 and UCS4 core functions
//   - TWideStrings and TWideStringList classes
//   - Unicode Tuned Boyer-Moore search class (TUTBMSearch)
//   - low and high level Unicode/Wide* functions
//   - low level Unicode UCS4 data import and functions
//   - helper functions
//
//  Version 2.9
// This unit contains routines and classes to manage and work with Unicode/WideString strings.
// You need Delphi 4 or higher to compile this code.
//
// Publicly available low level functions are all preceded by "Unicode..." (e.g.
// in UnicodeToUpper) while the high level functions use the Str... or Wide...
// naming scheme (e.g. StrLICompW and WideUpperCase).
//
// The normalization implementation in this unit has successfully and completely passed the
// official normative conformance testing as of Annex 9 in Technical Report #15
// (Unicode Standard Annex #15, http://www.unicode.org/unicode/reports/tr15, from 2000-08-31).
//
// Open issues:
//   - Yet to do things in the URE class are:
//     - check all character classes if they match correctly
//     - optimize rebuild of DFA (build only when pattern changes)
//     - set flag parameter of ExecuteURE
//     - add \d     any decimal digit
//           \D     any character that is not a decimal digit
//           \s     any whitespace character
//           \S     any character that is not a whitespace character
//           \w     any "word" character
//           \W     any "non-word" character
//   - The wide string classes still compare text with functions provided by the
//     particular system. This works usually fine under WinNT/W2K (although also
//     there are limitations like maximum text lengths). Under Win9x conversions
//     from and to MBCS are necessary which are bound to a particular locale and
//     so very limited in general use. These comparisons should be changed so that
//     the code in this unit is used.

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNITSCOPE}
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF MSWINDOWS}
  System.SysUtils, System.Classes,
  {$IFDEF HAS_UNIT_CHARACTER}
  System.Character,
  {$ENDIF HAS_UNIT_CHARACTER}
  {$ELSE ~HAS_UNITSCOPE}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes,
  {$IFDEF HAS_UNIT_CHARACTER}
  Character,
  {$ENDIF HAS_UNIT_CHARACTER}
  {$ENDIF ~HAS_UNITSCOPE}
  JclBase;

{$IFNDEF FPC}
 {$IFDEF MSWINDOWS}
  {$DEFINE OWN_WIDESTRING_MEMMGR}
 {$ENDIF MSWINDOWS}
{$ENDIF ~FPC}

const
  // definitions of often used characters:
  // Note: Use them only for tests of a certain character not to determine character
  //       classes (like white spaces) as in Unicode are often many code points defined
  //       being in a certain class. Hence your best option is to use the various
  //       UnicodeIs* functions.
  WideNull = WideChar(#0);
  WideTabulator = WideChar(#9);
  WideSpace = WideChar(#32);

  // logical line breaks
  WideLF = WideChar(#10);
  WideLineFeed = WideChar(#10);
  WideVerticalTab = WideChar(#11);
  WideFormFeed = WideChar(#12);
  WideCR = WideChar(#13);
  WideCarriageReturn = WideChar(#13);
  WideCRLF = WideString(#13#10);
  WideLineSeparator = WideChar($2028);
  WideParagraphSeparator = WideChar($2029);

  // byte order marks for Unicode files
  // Unicode text files (in UTF-16 format) should contain $FFFE as first character to
  // identify such a file clearly. Depending on the system where the file was created
  // on this appears either in big endian or little endian style.
  BOM_LSB_FIRST = WideChar($FEFF);
  BOM_MSB_FIRST = WideChar($FFFE);

type
  TSaveFormat = ( sfUTF16LSB, sfUTF16MSB, sfUTF8, sfAnsi );

const
  sfUnicodeLSB = sfUTF16LSB;
  sfUnicodeMSB = sfUTF16MSB;

type
  // various predefined or otherwise useful character property categories
  TCharacterCategory = (
    // normative categories
    ccLetterUppercase,
    ccLetterLowercase,
    ccLetterTitlecase,
    ccMarkNonSpacing,
    ccMarkSpacingCombining,
    ccMarkEnclosing,
    ccNumberDecimalDigit,
    ccNumberLetter,
    ccNumberOther,
    ccSeparatorSpace,
    ccSeparatorLine,
    ccSeparatorParagraph,
    ccOtherControl,
    ccOtherFormat,
    ccOtherSurrogate,
    ccOtherPrivate,
    ccOtherUnassigned,
    // informative categories
    ccLetterModifier,
    ccLetterOther,
    ccPunctuationConnector,
    ccPunctuationDash,
    ccPunctuationOpen,
    ccPunctuationClose,
    ccPunctuationInitialQuote,
    ccPunctuationFinalQuote,
    ccPunctuationOther,
    ccSymbolMath,
    ccSymbolCurrency,
    ccSymbolModifier,
    ccSymbolOther,
    // bidirectional categories
    ccLeftToRight,
    ccLeftToRightEmbedding,
    ccLeftToRightOverride,
    ccRightToLeft,
    ccRightToLeftArabic,
    ccRightToLeftEmbedding,
    ccRightToLeftOverride,
    ccPopDirectionalFormat,
    ccEuropeanNumber,
    ccEuropeanNumberSeparator,
    ccEuropeanNumberTerminator,
    ccArabicNumber,
    ccCommonNumberSeparator,
    ccBoundaryNeutral,
    ccSegmentSeparator,      // this includes tab and vertical tab
    ccWhiteSpace,            // Separator characters and control characters which should be treated by programming languages as "white space" for the purpose of parsing elements.
    ccOtherNeutrals,
    // self defined categories, they do not appear in the Unicode data file
    ccComposed,              // can be decomposed
    ccNonBreaking,
    ccSymmetric,             // has left and right forms
    ccHexDigit,              // Characters commonly used for the representation of hexadecimal numbers, plus their compatibility equivalents.
    ccQuotationMark,         // Punctuation characters that function as quotation marks.
    ccMirroring,
    ccAssigned,              // means there is a definition in the Unicode standard
    ccASCIIHexDigit,         // ASCII characters commonly used for the representation of hexadecimal numbers
    ccBidiControl,           // Format control characters which have specific functions in the Unicode Bidirectional Algorithm [UAX9].
    ccDash,                  // Punctuation characters explicitly called out as dashes in the Unicode Standard, plus their compatibility equivalents. Most of these have the General_Category value Pd, but some have the General_Category value Sm because of their use in mathematics.
    ccDeprecated,            // For a machine-readable list of deprecated characters. No characters will ever be removed from the standard, but the usage of deprecated characters is strongly discouraged.
    ccDiacritic,             // Characters that linguistically modify the meaning of another character to which they apply. Some diacritics are not combining characters, and some combining characters are not diacritics.
    ccExtender,              // Characters whose principal function is to extend the value or shape of a preceding alphabetic character. Typical of these are length and iteration marks.
    ccHyphen,                // Dashes which are used to mark connections between pieces of words, plus the Katakana middle dot. The Katakana middle dot functions like a hyphen, but is shaped like a dot rather than a dash.
    ccIdeographic,           // Characters considered to be CJKV (Chinese, Japanese, Korean, and Vietnamese) ideographs.
    ccIDSBinaryOperator,     // Used in Ideographic Description Sequences.
    ccIDSTrinaryOperator,    // Used in Ideographic Description Sequences.
    ccJoinControl,           // Format control characters which have specific functions for control of cursive joining and ligation.
    ccLogicalOrderException, // There are a small number of characters that do not use logical order. These characters require special handling in most processing.
    ccNonCharacterCodePoint, // Code points permanently reserved for internal use.
    ccOtherAlphabetic,       // Used in deriving the Alphabetic property.
    ccOtherDefaultIgnorableCodePoint, // Used in deriving the Default_Ignorable_Code_Point property.
    ccOtherGraphemeExtend,   // Used in deriving  the Grapheme_Extend property.
    ccOtherIDContinue,       // Used for backward compatibility of ID_Continue.
    ccOtherIDStart,          // Used for backward compatibility of ID_Start.
    ccOtherLowercase,        // Used in deriving the Lowercase property.
    ccOtherMath,             // Used in deriving the Math property.
    ccOtherUppercase,        // Used in deriving the Uppercase property.
    ccPatternSyntax,         // Used for pattern syntax as described in UAX #31: Unicode Identifier and Pattern Syntax [UAX31].
    ccPatternWhiteSpace,
    ccRadical,               // Used in Ideographic Description Sequences.
    ccSoftDotted,            // Characters with a "soft dot", like i or j. An accent placed on these characters causes the dot to disappear. An explicit dot above can be added where required, such as in Lithuanian.
    ccSTerm,                 // Sentence Terminal. Used in UAX #29: Unicode Text Segmentation [UAX29].
    ccTerminalPunctuation,   // Punctuation characters that generally mark the end of textual units.
    ccUnifiedIdeograph,      // Used in Ideographic Description Sequences.
    ccVariationSelector     // Indicates characters that are Variation Selectors. For details on the behavior of these characters, see StandardizedVariants.html, Section 16.4, "Variation Selectors" in [Unicode], and the Unicode Ideographic Variation Database [UTS37].
  );
  TCharacterCategories = set of TCharacterCategory;

{$IFDEF HAS_UNIT_CHARACTER}
type
  TCharacterUnicodeCategory = ccLetterUppercase..ccSymbolOther;

const
  CharacterCategoryToUnicodeCategory: array [TCharacterUnicodeCategory] of TUnicodeCategory =
    ( TUnicodeCategory.ucUppercaseLetter,    // ccLetterUppercase
      TUnicodeCategory.ucLowercaseLetter,    // ccLetterLowercase
      TUnicodeCategory.ucTitlecaseLetter,    // ccLetterTitlecase
      TUnicodeCategory.ucNonSpacingMark,     // ccMarkNonSpacing
      TUnicodeCategory.ucCombiningMark,      // ccMarkSpacingCombining
      TUnicodeCategory.ucEnclosingMark,      // ccMarkEnclosing
      TUnicodeCategory.ucDecimalNumber,      // ccNumberDecimalDigit
      TUnicodeCategory.ucLetterNumber,       // ccNumberLetter
      TUnicodeCategory.ucOtherNumber,        // ccNumberOther
      TUnicodeCategory.ucSpaceSeparator,     // ccSeparatorSpace
      TUnicodeCategory.ucLineSeparator,      // ccSeparatorLine
      TUnicodeCategory.ucParagraphSeparator, // ccSeparatorParagraph
      TUnicodeCategory.ucControl,            // ccOtherControl
      TUnicodeCategory.ucFormat,             // ccOtherFormat
      TUnicodeCategory.ucSurrogate,          // ccOtherSurrogate
      TUnicodeCategory.ucPrivateUse,         // ccOtherPrivate
      TUnicodeCategory.ucUnassigned,         // ccOtherUnassigned
      TUnicodeCategory.ucModifierLetter,     // ccLetterModifier
      TUnicodeCategory.ucOtherLetter,        // ccLetterOther
      TUnicodeCategory.ucConnectPunctuation, // ccPunctuationConnector
      TUnicodeCategory.ucDashPunctuation,    // ccPunctuationDash
      TUnicodeCategory.ucOpenPunctuation,    // ccPunctuationOpen
      TUnicodeCategory.ucClosePunctuation,   // ccPunctuationClose
      TUnicodeCategory.ucInitialPunctuation, // ccPunctuationInitialQuote
      TUnicodeCategory.ucFinalPunctuation,   // ccPunctuationFinalQuote
      TUnicodeCategory.ucOtherPunctuation,   // ccPunctuationOther
      TUnicodeCategory.ucMathSymbol,         // ccSymbolMath
      TUnicodeCategory.ucCurrencySymbol,     // ccSymbolCurrency
      TUnicodeCategory.ucModifierSymbol,     // ccSymbolModifier
      TUnicodeCategory.ucOtherSymbol );      // ccSymbolOther

  UnicodeCategoryToCharacterCategory: array [TUnicodeCategory] of TCharacterCategory =
    ( ccOtherControl,            // ucControl
      ccOtherFormat,             // ucFormat
      ccOtherUnassigned,         // ucUnassigned
      ccOtherPrivate,            // ucPrivateUse
      ccOtherSurrogate,          // ucSurrogate
      ccLetterLowercase,         // ucLowercaseLetter
      ccLetterModifier,          // ucModifierLetter
      ccLetterOther,             // ucOtherLetter
      ccLetterTitlecase,         // ucTitlecaseLetter
      ccLetterUppercase,         // ucUppercaseLetter
      ccMarkSpacingCombining,    // ucCombiningMark
      ccMarkEnclosing,           // ucEnclosingMark
      ccMarkNonSpacing,          // ucNonSpacingMark
      ccNumberDecimalDigit,      // ucDecimalNumber
      ccNumberLetter,            // ucLetterNumber
      ccNumberOther,             // ucOtherNumber
      ccPunctuationConnector,    // ucConnectPunctuation
      ccPunctuationDash,         // ucDashPunctuation
      ccPunctuationClose,        // ucClosePunctuation
      ccPunctuationFinalQuote,   // ucFinalPunctuation
      ccPunctuationInitialQuote, // ucInitialPunctuation
      ccPunctuationOther,        // ucOtherPunctuation
      ccPunctuationOpen,         // ucOpenPunctuation
      ccSymbolCurrency,          // ucCurrencySymbol
      ccSymbolModifier,          // ucModifierSymbol
      ccSymbolMath,              // ucMathSymbol
      ccSymbolOther,             // ucOtherSymbol
      ccSeparatorLine,           // ucLineSeparator
      ccSeparatorParagraph,      // ucParagraphSeparator
      ccSeparatorSpace );        // ucSpaceSeparator

function CharacterCategoriesToUnicodeCategory(const Categories: TCharacterCategories): TUnicodeCategory;
function UnicodeCategoryToCharacterCategories(Category: TUnicodeCategory): TCharacterCategories;
{$ENDIF HAS_UNIT_CHARACTER}

type
  // four forms of normalization are defined:
  TNormalizationForm = (
    nfNone, // do not normalize
    nfC,    // canonical decomposition followed by canonical composition (this is most often used)
    nfD,    // canonical decomposition
    nfKC,   // compatibility decomposition followed by a canonical composition
    nfKD    // compatibility decomposition
  );

  // 16 compatibility formatting tags are defined:
  TCompatibilityFormattingTag = (
    cftCanonical, // default when no CFT is explicited
    cftFont,      // Font variant (for example, a blackletter form)
    cftNoBreak,   // No-break version of a space or hyphen
    cftInitial,   // Initial presentation form (Arabic)
    cftMedial,    // Medial presentation form (Arabic)
    cftFinal,     // Final presentation form (Arabic)
    cftIsolated,  // Isolated presentation form (Arabic)
    cftCircle,    // Encircled form
    cftSuper,     // Superscript form
    cftSub,       // Subscript form
    cftVertical,  // Vertical layout presentation form
    cftWide,      // Wide (or zenkaku) compatibility character
    cftNarrow,    // Narrow (or hankaku) compatibility character
    cftSmall,     // Small variant form (CNS compatibility)
    cftSquare,    // CJK squared font variant
    cftFraction,  // Vulgar fraction form
    cftCompat     // Otherwise unspecified compatibility character
  );
  TCompatibilityFormattingTags = set of TCompatibilityFormattingTag;

  // used to hold information about the start and end
  // position of a unicodeblock.
  TUnicodeBlockRange = record
    RangeStart,
    RangeEnd: Cardinal;
  end;

  // An Unicode block usually corresponds to a particular language script but
  // can also represent special characters, musical symbols and the like.
  // http://www.unicode.org/Public/5.0.0/ucd/Blocks.txt
  TUnicodeBlock = (
    ubUndefined,
    ubBasicLatin,
    ubLatin1Supplement,
    ubLatinExtendedA,
    ubLatinExtendedB,
    ubIPAExtensions,
    ubSpacingModifierLetters,
    ubCombiningDiacriticalMarks,
    ubGreekandCoptic,
    ubCyrillic,
    ubCyrillicSupplement,
    ubArmenian,
    ubHebrew,
    ubArabic,
    ubSyriac,
    ubArabicSupplement,
    ubThaana,
    ubNKo,
    ubSamaritan,
    ubMandaic,
    ubDevanagari,
    ubBengali,
    ubGurmukhi,
    ubGujarati,
    ubOriya,
    ubTamil,
    ubTelugu,
    ubKannada,
    ubMalayalam,
    ubSinhala,
    ubThai,
    ubLao,
    ubTibetan,
    ubMyanmar,
    ubGeorgian,
    ubHangulJamo,
    ubEthiopic,
    ubEthiopicSupplement,
    ubCherokee,
    ubUnifiedCanadianAboriginalSyllabics,
    ubOgham,
    ubRunic,
    ubTagalog,
    ubHanunoo,
    ubBuhid,
    ubTagbanwa,
    ubKhmer,
    ubMongolian,
    ubUnifiedCanadianAboriginalSyllabicsExtended,
    ubLimbu,
    ubTaiLe,
    ubNewTaiLue,
    ubKhmerSymbols,
    ubBuginese,
    ubTaiTham,
    ubBalinese,
    ubSundanese,
    ubBatak,
    ubLepcha,
    ubOlChiki,
    ubVedicExtensions,
    ubPhoneticExtensions,
    ubPhoneticExtensionsSupplement,
    ubCombiningDiacriticalMarksSupplement,
    ubLatinExtendedAdditional,
    ubGreekExtended,
    ubGeneralPunctuation,
    ubSuperscriptsandSubscripts,
    ubCurrencySymbols,
    ubCombiningDiacriticalMarksforSymbols,
    ubLetterlikeSymbols,
    ubNumberForms,
    ubArrows,
    ubMathematicalOperators,
    ubMiscellaneousTechnical,
    ubControlPictures,
    ubOpticalCharacterRecognition,
    ubEnclosedAlphanumerics,
    ubBoxDrawing,
    ubBlockElements,
    ubGeometricShapes,
    ubMiscellaneousSymbols,
    ubDingbats,
    ubMiscellaneousMathematicalSymbolsA,
    ubSupplementalArrowsA,
    ubBraillePatterns,
    ubSupplementalArrowsB,
    ubMiscellaneousMathematicalSymbolsB,
    ubSupplementalMathematicalOperators,
    ubMiscellaneousSymbolsandArrows,
    ubGlagolitic,
    ubLatinExtendedC,
    ubCoptic,
    ubGeorgianSupplement,
    ubTifinagh,
    ubEthiopicExtended,
    ubCyrillicExtendedA,
    ubSupplementalPunctuation,
    ubCJKRadicalsSupplement,
    ubKangxiRadicals,
    ubIdeographicDescriptionCharacters,
    ubCJKSymbolsandPunctuation,
    ubHiragana,
    ubKatakana,
    ubBopomofo,
    ubHangulCompatibilityJamo,
    ubKanbun,
    ubBopomofoExtended,
    ubCJKStrokes,
    ubKatakanaPhoneticExtensions,
    ubEnclosedCJKLettersandMonths,
    ubCJKCompatibility,
    ubCJKUnifiedIdeographsExtensionA,
    ubYijingHexagramSymbols,
    ubCJKUnifiedIdeographs,
    ubYiSyllables,
    ubYiRadicals,
    ubLisu,
    ubVai,
    ubCyrillicExtendedB,
    ubBamum,
    ubModifierToneLetters,
    ubLatinExtendedD,
    ubSylotiNagri,
    ubCommonIndicNumberForms,
    ubPhagsPa,
    ubSaurashtra,
    ubDevanagariExtended,
    ubKayahLi,
    ubRejang,
    ubHangulJamoExtendedA,
    ubJavanese,
    ubCham,
    ubMyanmarExtendedA,
    ubTaiViet,
    ubEthiopicExtendedA,
    ubMeeteiMayek,
    ubHangulSyllables,
    ubHangulJamoExtendedB,
    ubHighSurrogates,
    ubHighPrivateUseSurrogates,
    ubLowSurrogates,
    ubPrivateUseArea,
    ubCJKCompatibilityIdeographs,
    ubAlphabeticPresentationForms,
    ubArabicPresentationFormsA,
    ubVariationSelectors,
    ubVerticalForms,
    ubCombiningHalfMarks,
    ubCJKCompatibilityForms,
    ubSmallFormVariants,
    ubArabicPresentationFormsB,
    ubHalfwidthandFullwidthForms,
    ubSpecials,
    ubLinearBSyllabary,
    ubLinearBIdeograms,
    ubAegeanNumbers,
    ubAncientGreekNumbers,
    ubAncientSymbols,
    ubPhaistosDisc,
    ubLycian,
    ubCarian,
    ubOldItalic,
    ubGothic,
    ubUgaritic,
    ubOldPersian,
    ubDeseret,
    ubShavian,
    ubOsmanya,
    ubCypriotSyllabary,
    ubImperialAramaic,
    ubPhoenician,
    ubLydian,
    ubKharoshthi,
    ubOldSouthArabian,
    ubAvestan,
    ubInscriptionalParthian,
    ubInscriptionalPahlavi,
    ubOldTurkic,
    ubRumiNumeralSymbols,
    ubBrahmi,
    ubKaithi,
    ubCuneiform,
    ubCuneiformNumbersAndPunctuation,
    ubEgyptianHieroglyphs,
    ubBamumSupplement,
    ubKanaSupplement,
    ubByzantineMusicalSymbols,
    ubMusicalSymbols,
    ubAncientGreekMusicalNotation,
    ubTaiXuanJingSymbols,
    ubCountingRodNumerals,
    ubMathematicalAlphanumericSymbols,
    ubMahjongTiles,
    ubDominoTiles,
    ubPlayingCards,
    ubEnclosedAlphanumericSupplement,
    ubEnclosedIdeographicSupplement,
    ubMiscellaneousSymbolsAndPictographs,
    ubEmoticons,
    ubTransportAndMapSymbols,
    ubAlchemicalSymbols,
    ubCJKUnifiedIdeographsExtensionB,
    ubCJKUnifiedIdeographsExtensionC,
    ubCJKUnifiedIdeographsExtensionD,
    ubCJKCompatibilityIdeographsSupplement,
    ubTags,
    ubVariationSelectorsSupplement,
    ubSupplementaryPrivateUseAreaA,
    ubSupplementaryPrivateUseAreaB
  );

  TUnicodeBlockData = record
    Range: TUnicodeBlockRange;
    Name: string;
  end;
  PUnicodeBlockData = ^TUnicodeBlockData;

const
  UnicodeBlockData: array [TUnicodeBlock] of TUnicodeBlockData =
    ((Range:(RangeStart: $FFFFFFFF; RangeEnd: $0000); Name: 'No-block'),
    (Range:(RangeStart: $0000; RangeEnd: $007F); Name: 'Basic Latin'),
    (Range:(RangeStart: $0080; RangeEnd: $00FF); Name: 'Latin-1 Supplement'),
    (Range:(RangeStart: $0100; RangeEnd: $017F); Name: 'Latin Extended-A'),
    (Range:(RangeStart: $0180; RangeEnd: $024F); Name: 'Latin Extended-B'),
    (Range:(RangeStart: $0250; RangeEnd: $02AF); Name: 'IPA Extensions'),
    (Range:(RangeStart: $02B0; RangeEnd: $02FF); Name: 'Spacing Modifier Letters'),
    (Range:(RangeStart: $0300; RangeEnd: $036F); Name: 'Combining Diacritical Marks'),
    (Range:(RangeStart: $0370; RangeEnd: $03FF); Name: 'Greek and Coptic'),
    (Range:(RangeStart: $0400; RangeEnd: $04FF); Name: 'Cyrillic'),
    (Range:(RangeStart: $0500; RangeEnd: $052F); Name: 'Cyrillic Supplement'),
    (Range:(RangeStart: $0530; RangeEnd: $058F); Name: 'Armenian'),
    (Range:(RangeStart: $0590; RangeEnd: $05FF); Name: 'Hebrew'),
    (Range:(RangeStart: $0600; RangeEnd: $06FF); Name: 'Arabic'),
    (Range:(RangeStart: $0700; RangeEnd: $074F); Name: 'Syriac'),
    (Range:(RangeStart: $0750; RangeEnd: $077F); Name: 'Arabic Supplement'),
    (Range:(RangeStart: $0780; RangeEnd: $07BF); Name: 'Thaana'),
    (Range:(RangeStart: $07C0; RangeEnd: $07FF); Name: 'NKo'),
    (Range:(RangeStart: $0800; RangeEnd: $083F); Name: 'Samaritan'),
    (Range:(RangeStart: $0840; RangeEnd: $085F); Name: 'Mandaic'),
    (Range:(RangeStart: $0900; RangeEnd: $097F); Name: 'Devanagari'),
    (Range:(RangeStart: $0980; RangeEnd: $09FF); Name: 'Bengali'),
    (Range:(RangeStart: $0A00; RangeEnd: $0A7F); Name: 'Gurmukhi'),
    (Range:(RangeStart: $0A80; RangeEnd: $0AFF); Name: 'Gujarati'),
    (Range:(RangeStart: $0B00; RangeEnd: $0B7F); Name: 'Oriya'),
    (Range:(RangeStart: $0B80; RangeEnd: $0BFF); Name: 'Tamil'),
    (Range:(RangeStart: $0C00; RangeEnd: $0C7F); Name: 'Telugu'),
    (Range:(RangeStart: $0C80; RangeEnd: $0CFF); Name: 'Kannada'),
    (Range:(RangeStart: $0D00; RangeEnd: $0D7F); Name: 'Malayalam'),
    (Range:(RangeStart: $0D80; RangeEnd: $0DFF); Name: 'Sinhala'),
    (Range:(RangeStart: $0E00; RangeEnd: $0E7F); Name: 'Thai'),
    (Range:(RangeStart: $0E80; RangeEnd: $0EFF); Name: 'Lao'),
    (Range:(RangeStart: $0F00; RangeEnd: $0FFF); Name: 'Tibetan'),
    (Range:(RangeStart: $1000; RangeEnd: $109F); Name: 'Myanmar'),
    (Range:(RangeStart: $10A0; RangeEnd: $10FF); Name: 'Georgian'),
    (Range:(RangeStart: $1100; RangeEnd: $11FF); Name: 'Hangul Jamo'),
    (Range:(RangeStart: $1200; RangeEnd: $137F); Name: 'Ethiopic'),
    (Range:(RangeStart: $1380; RangeEnd: $139F); Name: 'Ethiopic Supplement'),
    (Range:(RangeStart: $13A0; RangeEnd: $13FF); Name: 'Cherokee'),
    (Range:(RangeStart: $1400; RangeEnd: $167F); Name: 'Unified Canadian Aboriginal Syllabics'),
    (Range:(RangeStart: $1680; RangeEnd: $169F); Name: 'Ogham'),
    (Range:(RangeStart: $16A0; RangeEnd: $16FF); Name: 'Runic'),
    (Range:(RangeStart: $1700; RangeEnd: $171F); Name: 'Tagalog'),
    (Range:(RangeStart: $1720; RangeEnd: $173F); Name: 'Hanunoo'),
    (Range:(RangeStart: $1740; RangeEnd: $175F); Name: 'Buhid'),
    (Range:(RangeStart: $1760; RangeEnd: $177F); Name: 'Tagbanwa'),
    (Range:(RangeStart: $1780; RangeEnd: $17FF); Name: 'Khmer'),
    (Range:(RangeStart: $1800; RangeEnd: $18AF); Name: 'Mongolian'),
    (Range:(RangeStart: $18B0; RangeEnd: $18FF); Name: 'Unified Canadian Aboriginal Syllabics Extended'),
    (Range:(RangeStart: $1900; RangeEnd: $194F); Name: 'Limbu'),
    (Range:(RangeStart: $1950; RangeEnd: $197F); Name: 'Tai Le'),
    (Range:(RangeStart: $1980; RangeEnd: $19DF); Name: 'New Tai Lue'),
    (Range:(RangeStart: $19E0; RangeEnd: $19FF); Name: 'Khmer Symbols'),
    (Range:(RangeStart: $1A00; RangeEnd: $1A1F); Name: 'Buginese'),
    (Range:(RangeStart: $1A20; RangeEnd: $1AAF); Name: 'Tai Tham'),
    (Range:(RangeStart: $1B00; RangeEnd: $1B7F); Name: 'Balinese'),
    (Range:(RangeStart: $1B80; RangeEnd: $1BBF); Name: 'Sundanese'),
    (Range:(RangeStart: $1BC0; RangeEnd: $1BFF); Name: 'Batak'),
    (Range:(RangeStart: $1C00; RangeEnd: $1C4F); Name: 'Lepcha'),
    (Range:(RangeStart: $1C50; RangeEnd: $1C7F); Name: 'Ol Chiki'),
    (Range:(RangeStart: $1CD0; RangeEnd: $1CFF); Name: 'Vedic Extensions'),
    (Range:(RangeStart: $1D00; RangeEnd: $1D7F); Name: 'Phonetic Extensions'),
    (Range:(RangeStart: $1D80; RangeEnd: $1DBF); Name: 'Phonetic Extensions Supplement'),
    (Range:(RangeStart: $1DC0; RangeEnd: $1DFF); Name: 'Combining Diacritical Marks Supplement'),
    (Range:(RangeStart: $1E00; RangeEnd: $1EFF); Name: 'Latin Extended Additional'),
    (Range:(RangeStart: $1F00; RangeEnd: $1FFF); Name: 'Greek Extended'),
    (Range:(RangeStart: $2000; RangeEnd: $206F); Name: 'General Punctuation'),
    (Range:(RangeStart: $2070; RangeEnd: $209F); Name: 'Superscripts and Subscripts'),
    (Range:(RangeStart: $20A0; RangeEnd: $20CF); Name: 'Currency Symbols'),
    (Range:(RangeStart: $20D0; RangeEnd: $20FF); Name: 'Combining Diacritical Marks for Symbols'),
    (Range:(RangeStart: $2100; RangeEnd: $214F); Name: 'Letterlike Symbols'),
    (Range:(RangeStart: $2150; RangeEnd: $218F); Name: 'Number Forms'),
    (Range:(RangeStart: $2190; RangeEnd: $21FF); Name: 'Arrows'),
    (Range:(RangeStart: $2200; RangeEnd: $22FF); Name: 'Mathematical Operators'),
    (Range:(RangeStart: $2300; RangeEnd: $23FF); Name: 'Miscellaneous Technical'),
    (Range:(RangeStart: $2400; RangeEnd: $243F); Name: 'Control Pictures'),
    (Range:(RangeStart: $2440; RangeEnd: $245F); Name: 'Optical Character Recognition'),
    (Range:(RangeStart: $2460; RangeEnd: $24FF); Name: 'Enclosed Alphanumerics'),
    (Range:(RangeStart: $2500; RangeEnd: $257F); Name: 'Box Drawing'),
    (Range:(RangeStart: $2580; RangeEnd: $259F); Name: 'Block Elements'),
    (Range:(RangeStart: $25A0; RangeEnd: $25FF); Name: 'Geometric Shapes'),
    (Range:(RangeStart: $2600; RangeEnd: $26FF); Name: 'Miscellaneous Symbols'),
    (Range:(RangeStart: $2700; RangeEnd: $27BF); Name: 'Dingbats'),
    (Range:(RangeStart: $27C0; RangeEnd: $27EF); Name: 'Miscellaneous Mathematical Symbols-A'),
    (Range:(RangeStart: $27F0; RangeEnd: $27FF); Name: 'Supplemental Arrows-A'),
    (Range:(RangeStart: $2800; RangeEnd: $28FF); Name: 'Braille Patterns'),
    (Range:(RangeStart: $2900; RangeEnd: $297F); Name: 'Supplemental Arrows-B'),
    (Range:(RangeStart: $2980; RangeEnd: $29FF); Name: 'Miscellaneous Mathematical Symbols-B'),
    (Range:(RangeStart: $2A00; RangeEnd: $2AFF); Name: 'Supplemental Mathematical Operators'),
    (Range:(RangeStart: $2B00; RangeEnd: $2BFF); Name: 'Miscellaneous Symbols and Arrows'),
    (Range:(RangeStart: $2C00; RangeEnd: $2C5F); Name: 'Glagolitic'),
    (Range:(RangeStart: $2C60; RangeEnd: $2C7F); Name: 'Latin Extended-C'),
    (Range:(RangeStart: $2C80; RangeEnd: $2CFF); Name: 'Coptic'),
    (Range:(RangeStart: $2D00; RangeEnd: $2D2F); Name: 'Georgian Supplement'),
    (Range:(RangeStart: $2D30; RangeEnd: $2D7F); Name: 'Tifinagh'),
    (Range:(RangeStart: $2D80; RangeEnd: $2DDF); Name: 'Ethiopic Extended'),
    (Range:(RangeStart: $2DE0; RangeEnd: $2DFF); Name: 'Cyrillic Extended-A'),
    (Range:(RangeStart: $2E00; RangeEnd: $2E7F); Name: 'Supplemental Punctuation'),
    (Range:(RangeStart: $2E80; RangeEnd: $2EFF); Name: 'CJK Radicals Supplement'),
    (Range:(RangeStart: $2F00; RangeEnd: $2FDF); Name: 'Kangxi Radicals'),
    (Range:(RangeStart: $2FF0; RangeEnd: $2FFF); Name: 'Ideographic Description Characters'),
    (Range:(RangeStart: $3000; RangeEnd: $303F); Name: 'CJK Symbols and Punctuation'),
    (Range:(RangeStart: $3040; RangeEnd: $309F); Name: 'Hiragana'),
    (Range:(RangeStart: $30A0; RangeEnd: $30FF); Name: 'Katakana'),
    (Range:(RangeStart: $3100; RangeEnd: $312F); Name: 'Bopomofo'),
    (Range:(RangeStart: $3130; RangeEnd: $318F); Name: 'Hangul Compatibility Jamo'),
    (Range:(RangeStart: $3190; RangeEnd: $319F); Name: 'Kanbun'),
    (Range:(RangeStart: $31A0; RangeEnd: $31BF); Name: 'Bopomofo Extended'),
    (Range:(RangeStart: $31C0; RangeEnd: $31EF); Name: 'CJK Strokes'),
    (Range:(RangeStart: $31F0; RangeEnd: $31FF); Name: 'Katakana Phonetic Extensions'),
    (Range:(RangeStart: $3200; RangeEnd: $32FF); Name: 'Enclosed CJK Letters and Months'),
    (Range:(RangeStart: $3300; RangeEnd: $33FF); Name: 'CJK Compatibility'),
    (Range:(RangeStart: $3400; RangeEnd: $4DBF); Name: 'CJK Unified Ideographs Extension A'),
    (Range:(RangeStart: $4DC0; RangeEnd: $4DFF); Name: 'Yijing Hexagram Symbols'),
    (Range:(RangeStart: $4E00; RangeEnd: $9FFF); Name: 'CJK Unified Ideographs'),
    (Range:(RangeStart: $A000; RangeEnd: $A48F); Name: 'Yi Syllables'),
    (Range:(RangeStart: $A490; RangeEnd: $A4CF); Name: 'Yi Radicals'),
    (Range:(RangeStart: $A4D0; RangeEnd: $A4FF); Name: 'Lisu'),
    (Range:(RangeStart: $A500; RangeEnd: $A63F); Name: 'Vai'),
    (Range:(RangeStart: $A640; RangeEnd: $A69F); Name: 'Cyrillic Extended-B'),
    (Range:(RangeStart: $A6A0; RangeEnd: $A6FF); Name: 'Bamum'),
    (Range:(RangeStart: $A700; RangeEnd: $A71F); Name: 'Modifier Tone Letters'),
    (Range:(RangeStart: $A720; RangeEnd: $A7FF); Name: 'Latin Extended-D'),
    (Range:(RangeStart: $A800; RangeEnd: $A82F); Name: 'Syloti Nagri'),
    (Range:(RangeStart: $A830; RangeEnd: $A83F); Name: 'Common Indic Number Forms'),
    (Range:(RangeStart: $A840; RangeEnd: $A87F); Name: 'Phags-pa'),
    (Range:(RangeStart: $A880; RangeEnd: $A8DF); Name: 'Saurashtra'),
    (Range:(RangeStart: $A8E0; RangeEnd: $A8FF); Name: 'Devanagari Extended'),
    (Range:(RangeStart: $A900; RangeEnd: $A92F); Name: 'Kayah Li'),
    (Range:(RangeStart: $A930; RangeEnd: $A95F); Name: 'Rejang'),
    (Range:(RangeStart: $A960; RangeEnd: $A97F); Name: 'Hangul Jamo Extended-A'),
    (Range:(RangeStart: $A980; RangeEnd: $A9DF); Name: 'Javanese'),
    (Range:(RangeStart: $AA00; RangeEnd: $AA5F); Name: 'Cham'),
    (Range:(RangeStart: $AA60; RangeEnd: $AA7F); Name: 'Myanmar Extended-A'),
    (Range:(RangeStart: $AA80; RangeEnd: $AADF); Name: 'Tai Viet'),
    (Range:(RangeStart: $AB00; RangeEnd: $AB2F); Name: 'Ethiopic Extended-A'),
    (Range:(RangeStart: $ABC0; RangeEnd: $ABFF); Name: 'Meetei Mayek'),
    (Range:(RangeStart: $AC00; RangeEnd: $D7AF); Name: 'Hangul Syllables'),
    (Range:(RangeStart: $D7B0; RangeEnd: $D7FF); Name: 'Hangul Jamo Extended-B'),
    (Range:(RangeStart: $D800; RangeEnd: $DB7F); Name: 'High Surrogates'),
    (Range:(RangeStart: $DB80; RangeEnd: $DBFF); Name: 'High Private Use Surrogates'),
    (Range:(RangeStart: $DC00; RangeEnd: $DFFF); Name: 'Low Surrogates'),
    (Range:(RangeStart: $E000; RangeEnd: $F8FF); Name: 'Private Use Area'),
    (Range:(RangeStart: $F900; RangeEnd: $FAFF); Name: 'CJK Compatibility Ideographs'),
    (Range:(RangeStart: $FB00; RangeEnd: $FB4F); Name: 'Alphabetic Presentation Forms'),
    (Range:(RangeStart: $FB50; RangeEnd: $FDFF); Name: 'Arabic Presentation Forms-A'),
    (Range:(RangeStart: $FE00; RangeEnd: $FE0F); Name: 'Variation Selectors'),
    (Range:(RangeStart: $FE10; RangeEnd: $FE1F); Name: 'Vertical Forms'),
    (Range:(RangeStart: $FE20; RangeEnd: $FE2F); Name: 'Combining Half Marks'),
    (Range:(RangeStart: $FE30; RangeEnd: $FE4F); Name: 'CJK Compatibility Forms'),
    (Range:(RangeStart: $FE50; RangeEnd: $FE6F); Name: 'Small Form Variants'),
    (Range:(RangeStart: $FE70; RangeEnd: $FEFF); Name: 'Arabic Presentation Forms-B'),
    (Range:(RangeStart: $FF00; RangeEnd: $FFEF); Name: 'Halfwidth and Fullwidth Forms'),
    (Range:(RangeStart: $FFF0; RangeEnd: $FFFF); Name: 'Specials'),
    (Range:(RangeStart: $10000; RangeEnd: $1007F); Name: 'Linear B Syllabary'),
    (Range:(RangeStart: $10080; RangeEnd: $100FF); Name: 'Linear B Ideograms'),
    (Range:(RangeStart: $10100; RangeEnd: $1013F); Name: 'Aegean Numbers'),
    (Range:(RangeStart: $10140; RangeEnd: $1018F); Name: 'Ancient Greek Numbers'),
    (Range:(RangeStart: $10190; RangeEnd: $101CF); Name: 'Ancient Symbols'),
    (Range:(RangeStart: $101D0; RangeEnd: $101FF); Name: 'Phaistos Disc'),
    (Range:(RangeStart: $10280; RangeEnd: $1029F); Name: 'Lycian'),
    (Range:(RangeStart: $102A0; RangeEnd: $102DF); Name: 'Carian'),
    (Range:(RangeStart: $10300; RangeEnd: $1032F); Name: 'Old Italic'),
    (Range:(RangeStart: $10330; RangeEnd: $1034F); Name: 'Gothic'),
    (Range:(RangeStart: $10380; RangeEnd: $1039F); Name: 'Ugaritic'),
    (Range:(RangeStart: $103A0; RangeEnd: $103DF); Name: 'Old Persian'),
    (Range:(RangeStart: $10400; RangeEnd: $1044F); Name: 'Deseret'),
    (Range:(RangeStart: $10450; RangeEnd: $1047F); Name: 'Shavian'),
    (Range:(RangeStart: $10480; RangeEnd: $104AF); Name: 'Osmanya'),
    (Range:(RangeStart: $10800; RangeEnd: $1083F); Name: 'Cypriot Syllabary'),
    (Range:(RangeStart: $10840; RangeEnd: $1085F); Name: 'Imperial Aramaic'),
    (Range:(RangeStart: $10900; RangeEnd: $1091F); Name: 'Phoenician'),
    (Range:(RangeStart: $10920; RangeEnd: $1093F); Name: 'Lydian'),
    (Range:(RangeStart: $10A00; RangeEnd: $10A5F); Name: 'Kharoshthi'),
    (Range:(RangeStart: $10A60; RangeEnd: $10A7F); Name: 'Old South Arabian'),
    (Range:(RangeStart: $10B00; RangeEnd: $10B3F); Name: 'Avestan'),
    (Range:(RangeStart: $10B40; RangeEnd: $10B5F); Name: 'Inscriptional Parthian'),
    (Range:(RangeStart: $10B60; RangeEnd: $10B7F); Name: 'Inscriptional Pahlavi'),
    (Range:(RangeStart: $10C00; RangeEnd: $10C4F); Name: 'Old Turkic'),
    (Range:(RangeStart: $10E60; RangeEnd: $10E7F); Name: 'Rumi Numeral Symbols'),
    (Range:(RangeStart: $11000; RangeEnd: $1107F); Name: 'Brahmi'),
    (Range:(RangeStart: $11080; RangeEnd: $110CF); Name: 'Kaithi'),
    (Range:(RangeStart: $12000; RangeEnd: $123FF); Name: 'Cuneiform'),
    (Range:(RangeStart: $12400; RangeEnd: $1247F); Name: 'Cuneiform Numbers and Punctuation'),
    (Range:(RangeStart: $13000; RangeEnd: $1342F); Name: 'Egyptian Hieroglyphs'),
    (Range:(RangeStart: $16800; RangeEnd: $16A3F); Name: 'Bamum Supplement'),
    (Range:(RangeStart: $1B000; RangeEnd: $1B0FF); Name: 'Kana Supplement'),
    (Range:(RangeStart: $1D000; RangeEnd: $1D0FF); Name: 'Byzantine Musical Symbols'),
    (Range:(RangeStart: $1D100; RangeEnd: $1D1FF); Name: 'Musical Symbols'),
    (Range:(RangeStart: $1D200; RangeEnd: $1D24F); Name: 'Ancient Greek Musical Notation'),
    (Range:(RangeStart: $1D300; RangeEnd: $1D35F); Name: 'Tai Xuan Jing Symbols'),
    (Range:(RangeStart: $1D360; RangeEnd: $1D37F); Name: 'Counting Rod Numerals'),
    (Range:(RangeStart: $1D400; RangeEnd: $1D7FF); Name: 'Mathematical Alphanumeric Symbols'),
    (Range:(RangeStart: $1F000; RangeEnd: $1F02F); Name: 'Mahjong Tiles'),
    (Range:(RangeStart: $1F030; RangeEnd: $1F09F); Name: 'Domino Tiles'),
    (Range:(RangeStart: $1F0A0; RangeEnd: $1F0FF); Name: 'Playing Cards'),
    (Range:(RangeStart: $1F100; RangeEnd: $1F1FF); Name: 'Enclosed Alphanumeric Supplement'),
    (Range:(RangeStart: $1F200; RangeEnd: $1F2FF); Name: 'Enclosed Ideographic Supplement'),
    (Range:(RangeStart: $1F300; RangeEnd: $1F5FF); Name: 'Miscellaneous Symbols And Pictographs'),
    (Range:(RangeStart: $1F600; RangeEnd: $1F64F); Name: 'Emoticons'),
    (Range:(RangeStart: $1F680; RangeEnd: $1F6FF); Name: 'Transport And Map Symbols'),
    (Range:(RangeStart: $1F700; RangeEnd: $1F77F); Name: 'Alchemical Symbols'),
    (Range:(RangeStart: $20000; RangeEnd: $2A6DF); Name: 'CJK Unified Ideographs Extension B'),
    (Range:(RangeStart: $2A700; RangeEnd: $2B73F); Name: 'CJK Unified Ideographs Extension C'),
    (Range:(RangeStart: $2B740; RangeEnd: $2B81F); Name: 'CJK Unified Ideographs Extension D'),
    (Range:(RangeStart: $2F800; RangeEnd: $2FA1F); Name: 'CJK Compatibility Ideographs Supplement'),
    (Range:(RangeStart: $E0000; RangeEnd: $E007F); Name: 'Tags'),
    (Range:(RangeStart: $E0100; RangeEnd: $E01EF); Name: 'Variation Selectors Supplement'),
    (Range:(RangeStart: $F0000; RangeEnd: $FFFFF); Name: 'Supplementary Private Use Area-A'),
    (Range:(RangeStart: $100000; RangeEnd: $10FFFF); Name: 'Supplementary Private Use Area-B'));

{$IFNDEF UNICODE_RTL_DATABASE}

type
  TWideStrings = class;

  TSearchFlag = (
    sfCaseSensitive,    // match letter case
    sfIgnoreNonSpacing, // ignore non-spacing characters in search
    sfSpaceCompress,    // handle several consecutive white spaces as one white space
                        // (this applies to the pattern as well as the search text)
    sfWholeWordOnly     // match only text at end/start and/or surrounded by white spaces
  );

  TSearchFlags = set of TSearchFlag;

  // a generic search class defininition used for tuned Boyer-Moore and Unicode
  // regular expression searches
  TSearchEngine = class(TObject)
  private
    FResults: TList;      // 2 entries for each result (start and stop position)
    FOwner: TWideStrings; // at the moment unused, perhaps later to access strings faster
  protected
    function GetCount: SizeInt; virtual;
  public
    constructor Create(AOwner: TWideStrings); virtual;
    destructor Destroy; override;

    procedure AddResult(Start, Stop: SizeInt); virtual;
    procedure Clear; virtual;
    procedure ClearResults; virtual;
    procedure DeleteResult(Index: SizeInt); virtual;
    procedure FindPrepare(const Pattern: WideString; Options: TSearchFlags); overload; virtual; abstract;
    procedure FindPrepare(Pattern: PWideChar; PatternLength: SizeInt; Options: TSearchFlags); overload; virtual; abstract;
    function FindFirst(const Text: WideString; var Start, Stop: SizeInt): Boolean; overload; virtual; abstract;
    function FindFirst(Text: PWideChar; TextLen: SizeInt; var Start, Stop: SizeInt): Boolean; overload; virtual; abstract;
    function FindAll(const Text: WideString): Boolean; overload; virtual; abstract;
    function FindAll(Text: PWideChar; TextLen: SizeInt): Boolean; overload; virtual; abstract;
    procedure GetResult(Index: SizeInt; var Start, Stop: SizeInt); virtual;

    property Count: SizeInt read GetCount;
  end;

  // The Unicode Tuned Boyer-Moore (UTBM) search implementation is an extended
  // translation created from a free package written by Mark Leisher (mleisher att crl dott nmsu dott edu).
  //
  // The code handles high and low surrogates as well as case (in)dependency,
  // can ignore non-spacing characters and allows optionally to return whole
  // words only.

  // single pattern character
  PUTBMChar = ^TUTBMChar;
  TUTBMChar = record
    LoCase,
    UpCase,
    TitleCase: UCS4;
  end;

  PUTBMSkip = ^TUTBMSkip;
  TUTBMSkip = record
    BMChar: PUTBMChar;
    SkipValues: Integer;
  end;

  TUTBMSearch = class(TSearchEngine)
  private
    FFlags: TSearchFlags;
    FPattern: PUTBMChar;
    FPatternUsed: SizeInt;
    FPatternSize: SizeInt;
    FPatternLength: SizeInt;
    FSkipValues: PUTBMSkip;
    FSkipsUsed: SizeInt;
    FMD4: SizeInt;
  protected
    procedure ClearPattern;
    procedure Compile(Pattern: PUCS2; PatternLength: SizeInt; Flags: TSearchFlags);
    function Find(Text: PUCS2; TextLen: SizeInt; var MatchStart, MatchEnd: SizeInt): Boolean;
    function GetSkipValue(TextStart, TextEnd: PUCS2): SizeInt;
    function Match(Text, Start, Stop: PUCS2; var MatchStart, MatchEnd: SizeInt): Boolean;
  public
    procedure Clear; override;
    procedure FindPrepare(const Pattern: WideString; Options: TSearchFlags); overload; override;
    procedure FindPrepare(Pattern: PWideChar; PatternLength: SizeInt; Options: TSearchFlags); overload; override;
    function FindFirst(const Text: WideString; var Start, Stop: SizeInt): Boolean; overload; override;
    function FindFirst(Text: PWideChar; TextLen: SizeInt; var Start, Stop: SizeInt): Boolean; overload; override;
    function FindAll(const Text: WideString): Boolean; overload; override;
    function FindAll(Text: PWideChar; TextLen: SizeInt): Boolean; overload; override;
  end;

  // Regular expression search engine for text in UCS2 form taking surrogates
  // into account. This implementation is an improved translation from the URE
  // package written by Mark Leisher (mleisher att crl dott nmsu dott edu) who used a variation
  // of the RE->DFA algorithm done by Mark Hopkins (markh att csd4 dott csd dott uwm dott edu).
  // Assumptions:
  //   o  Regular expression and text already normalized.
  //   o  Conversion to lower case assumes a 1-1 mapping.
  //
  // Definitions:
  //   Separator - any one of U+2028, U+2029, NL, CR.
  //
  // Operators:
  //   .      - match any character
  //   *      - match zero or more of the last subexpression
  //   +      - match one or more of the last subexpression
  //   ?      - match zero or one of the last subexpression
  //   ()     - subexpression grouping
  //   {m, n} - match at least m occurences and up to n occurences
  //            Note: both values can be 0 or ommitted which denotes then a unlimiting bound
  //            {,} and {0,} and {0, 0} correspond to *
  //            {, 1} and {0, 1} correspond to ?
  //            {1,} and {1, 0} correspond to +
  //   {m}    - match exactly m occurences
  //
  //   Notes:
  //     o  The "." operator normally does not match separators, but a flag is
  //        available that will allow this operator to match a separator.
  //
  // Literals and Constants:
  //   c       - literal UCS2 character
  //   \x....  - hexadecimal number of up to 4 digits
  //   \X....  - hexadecimal number of up to 4 digits
  //   \u....  - hexadecimal number of up to 4 digits
  //   \U....  - hexadecimal number of up to 4 digits
  //
  // Character classes:
  //   [...]           - Character class
  //   [^...]          - Negated character class
  //   \pN1,N2,...,Nn  - Character properties class
  //   \PN1,N2,...,Nn  - Negated character properties class
  //
  //   POSIX character classes recognized:
  //     :alnum:
  //     :alpha:
  //     :cntrl:
  //     :digit:
  //     :graph:
  //     :lower:
  //     :print:
  //     :punct:
  //     :space:
  //     :upper:
  //     :xdigit:
  //
  //   Notes:
  //     o  Character property classes are \p or \P followed by a comma separated
  //        list of integers between 0 and the maximum entry index in TCharacterCategory.
  //        These integers directly correspond to the TCharacterCategory enumeration entries.
  //        Note: upper, lower and title case classes need to have case sensitive search
  //              be enabled to match correctly!
  //
  //     o  Character classes can contain literals, constants and character
  //        property classes. Example:
  //
  //        [abc\U10A\p0,13,4]

  // structure used to handle a compacted range of characters
  PUcRange = ^TUcRange;
  TUcRange = record
    MinCode,
    MaxCode: UCS4;
  end;

  TUcCClass = record
    Ranges: array of TUcRange;
    RangesUsed: SizeInt;
  end;

  // either a single character or a list of character classes
  TUcSymbol = record
    Chr: UCS4;
    CCL: TUcCClass;
  end;

  // this is a general element structure used for expressions and stack elements
  TUcElement = record
    OnStack: Boolean;
    AType,
    LHS,
    RHS: SizeInt;
  end;

  // this is a structure used to track a list or a stack of states
  PUcStateList = ^TUcStateList;
  TUcStateList = record
    List: array of SizeInt;
    ListUsed: SizeInt;
  end;

  // structure to track the list of unique states for a symbol during reduction
  PUcSymbolTableEntry = ^TUcSymbolTableEntry;
  TUcSymbolTableEntry = record
    ID,
    AType: SizeInt;
    Mods,
    Categories: TCharacterCategories;
    Symbol: TUcSymbol;
    States: TUcStateList;
  end;

  // structure to hold a single State
  PUcState = ^TUcState;
  TUcState = record
    ID: SizeInt;
    Accepting: Boolean;
    StateList: TUcStateList;
    Transitions: array of TUcElement;
    TransitionsUsed: SizeInt;
  end;

  // structure used for keeping lists of states
  TUcStateTable = record
    States: array of TUcState;
    StatesUsed: SizeInt;
  end;

  // structure to track pairs of DFA states when equivalent states are merged
  TUcEquivalent = record
    Left,
    Right: SizeInt;
  end;

  TUcExpressionList = record
    Expressions: array of TUcElement;
    ExpressionsUsed: SizeInt;
  end;

  TUcSymbolTable = record
    Symbols: array of TUcSymbolTableEntry;
    SymbolsUsed: SizeInt;
  end;

  TUcEquivalentList = record
    Equivalents: array of TUcEquivalent;
    EquivalentsUsed: SizeInt;
  end;

  // structure used for constructing the NFA and reducing to a minimal DFA
  PUREBuffer = ^TUREBuffer;
  TUREBuffer = record
    Reducing: Boolean;
    Error: Integer;
    Flags: Cardinal;
    Stack: TUcStateList;
    SymbolTable: TUcSymbolTable;       // table of unique symbols encountered
    ExpressionList: TUcExpressionList; // tracks the unique expressions generated
                                       // for the NFA and when the NFA is reduced
    States: TUcStateTable;             // the reduced table of unique groups of NFA states
    EquivalentList: TUcEquivalentList; // tracks states when equivalent states are merged
  end;

  TUcTransition = record
    Symbol,
    NextState: SizeInt;
  end;

  PDFAState = ^TDFAState;
  TDFAState = record
    Accepting: Boolean;
    NumberTransitions: SizeInt;
    StartTransition: SizeInt;
  end;

  TDFAStates = record
    States: array of TDFAState;
    StatesUsed: SizeInt;
  end;

  TUcTransitions = record
    Transitions: array of TUcTransition;
    TransitionsUsed: SizeInt;
  end;

  TDFA = record
    Flags: Cardinal;
    SymbolTable: TUcSymbolTable;
    StateList: TDFAStates;
    TransitionList: TUcTransitions;
  end;

  TURESearch = class(TSearchEngine)
  private
    FUREBuffer: TUREBuffer;
    FDFA: TDFA;
  protected
    procedure AddEquivalentPair(L, R: SizeInt);
    procedure AddRange(var CCL: TUcCClass; Range: TUcRange);
    function AddState(NewStates: array of SizeInt): SizeInt;
    procedure AddSymbolState(Symbol, State: SizeInt);
    function BuildCharacterClass(CP: PUCS2; Limit: SizeInt; Symbol: PUcSymbolTableEntry): SizeInt;
    procedure ClearUREBuffer;
    function CompileSymbol(S: PUCS2; Limit: SizeInt; Symbol: PUcSymbolTableEntry): SizeInt;
    procedure CompileURE(RE: PWideChar; RELength: SizeInt; Casefold: Boolean);
    procedure CollectPendingOperations(var State: SizeInt);
    function ConvertRegExpToNFA(RE: PWideChar; RELength: SizeInt): SizeInt;
    function ExecuteURE(Flags: Cardinal; Text: PUCS2; TextLen: SizeInt; var MatchStart, MatchEnd: SizeInt): Boolean;
    procedure ClearDFA;
    procedure HexDigitSetup(Symbol: PUcSymbolTableEntry);
    function MakeExpression(AType, LHS, RHS: SizeInt): SizeInt;
    function MakeHexNumber(NP: PUCS2; Limit: SizeInt; var Number: UCS4): SizeInt;
    function MakeSymbol(S: PUCS2; Limit: SizeInt; out Consumed: SizeInt): SizeInt;
    procedure MergeEquivalents;
    function ParsePropertyList(Properties: PUCS2; Limit: SizeInt; var Categories: TCharacterCategories): SizeInt;
    function Peek: SizeInt;
    function Pop: SizeInt;
    function PosixCCL(CP: PUCS2; Limit: SizeInt; Symbol: PUcSymbolTableEntry): SizeInt;
    function ProbeLowSurrogate(LeftState: PUCS2; Limit: SizeInt; var Code: UCS4): SizeInt;
    procedure Push(V: SizeInt);
    procedure Reduce(Start: SizeInt);
    procedure SpaceSetup(Symbol: PUcSymbolTableEntry; Categories: TCharacterCategories);
    function SymbolsAreDifferent(A, B: PUcSymbolTableEntry): Boolean;
  public
    procedure Clear; override;
    procedure FindPrepare(const Pattern: WideString; Options: TSearchFlags); overload; override;
    procedure FindPrepare(Pattern: PWideChar; PatternLength: SizeInt; Options: TSearchFlags); overload; override;
    function FindFirst(const Text: WideString; var Start, Stop: SizeInt): Boolean; overload; override;
    function FindFirst(Text: PWideChar; TextLen: SizeInt; var Start, Stop: SizeInt): Boolean; overload; override;
    function FindAll(const Text: WideString): Boolean; overload; override;
    function FindAll(Text: PWideChar; TextLen: SizeInt): Boolean; overload; override;
  end;

  // Event used to give the application a chance to switch the way of how to save
  // the text in TWideStrings if the text contains characters not only from the
  // ANSI block but the save type is ANSI. On triggering the event the application
  // can change the property SaveUnicode as needed. This property is again checked
  // after the callback returns.
  TConfirmConversionEvent = procedure (Sender: TWideStrings; var Allowed: Boolean) of object;

  TWideStrings = class(TPersistent)
  private
    FUpdateCount: Integer;
    FLanguage: LCID;        // language can usually left alone, the system's default is used
    FSaved: Boolean;        // set in SaveToStream, True in case saving was successfull otherwise False
    FNormalizationForm: TNormalizationForm; // determines in which form Unicode strings should be stored
    FOnConfirmConversion: TConfirmConversionEvent;
    FSaveFormat: TSaveFormat;  // overrides the FSaveUnicode flag, initialized when a file is loaded,
                               // expect losses if it is set to sfAnsi before saving
    function GetCommaText: WideString;
    function GetName(Index: Integer): WideString;
    function GetValue(const Name: WideString): WideString;
    procedure ReadData(Reader: TReader);
    procedure SetCommaText(const Value: WideString);
    procedure SetNormalizationForm(const Value: TNormalizationForm);
    procedure SetValue(const Name, Value: WideString);
    procedure WriteData(Writer: TWriter);
    function GetSaveUnicode: Boolean;
    procedure SetSaveUnicode(const Value: Boolean);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoConfirmConversion(var Allowed: Boolean); virtual;
    procedure Error(const Msg: string; Data: Integer);
    function Get(Index: Integer): WideString; virtual; abstract;
    function GetCapacity: Integer; virtual;
    function GetCount: Integer; virtual; abstract;
    function GetObject(Index: Integer): TObject; virtual;
    function GetTextStr: WideString; virtual;
    procedure Put(Index: Integer; const S: WideString); virtual; abstract;
    procedure PutObject(Index: Integer; AObject: TObject); virtual; abstract;
    procedure SetCapacity(NewCapacity: Integer); virtual;
    procedure SetUpdateState(Updating: Boolean); virtual;
    procedure SetLanguage(Value: LCID); virtual;
  public
    constructor Create;

    function Add(const S: WideString): Integer; virtual;
    function AddObject(const S: WideString; AObject: TObject): Integer; virtual;
    procedure Append(const S: WideString);
    procedure AddStrings(Strings: TStrings); overload; virtual;
    procedure AddStrings(Strings: TWideStrings); overload; virtual;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure BeginUpdate;
    procedure Clear; virtual; abstract;
    procedure Delete(Index: Integer); virtual; abstract;
    procedure EndUpdate;
    function Equals(Strings: TWideStrings): Boolean; {$IFDEF RTL200_UP} reintroduce; {$ENDIF RTL200_UP}
    procedure Exchange(Index1, Index2: Integer); virtual;
    function GetSeparatedText(Separators: WideString): WideString; virtual;
    function GetText: PWideChar; virtual;
    function IndexOf(const S: WideString): Integer; virtual;
    function IndexOfName(const Name: WideString): Integer;
    function IndexOfObject(AObject: TObject): Integer;
    procedure Insert(Index: Integer; const S: WideString); virtual; abstract;
    procedure InsertObject(Index: Integer; const S: WideString; AObject: TObject);
    procedure LoadFromFile(const FileName: TFileName); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure Move(CurIndex, NewIndex: Integer); virtual;
    procedure SaveToFile(const FileName: TFileName); virtual;
    procedure SaveToStream(Stream: TStream; WithBOM: Boolean = True); virtual;
    procedure SetText(const Value: WideString); virtual;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property CommaText: WideString read GetCommaText write SetCommaText;
    property Count: Integer read GetCount;
    property Language: LCID read FLanguage write SetLanguage;
    property Names[Index: Integer]: WideString read GetName;
    property NormalizationForm: TNormalizationForm read FNormalizationForm write SetNormalizationForm default nfC;
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property Values[const Name: WideString]: WideString read GetValue write SetValue;
    property Saved: Boolean read FSaved;
    property SaveUnicode: Boolean read GetSaveUnicode write SetSaveUnicode default True;
    property SaveFormat: TSaveFormat read FSaveFormat write FSaveFormat default sfUnicodeLSB;
    property Strings[Index: Integer]: WideString read Get write Put; default;
    property Text: WideString read GetTextStr write SetText;

    property OnConfirmConversion: TConfirmConversionEvent read FOnConfirmConversion write FOnConfirmConversion;
  end;

  //----- TWideStringList class
  TWideStringItem = record
    {$IFDEF OWN_WIDESTRING_MEMMGR}
    FString: PWideChar; // "array of WideChar";
    {$ELSE ~OWN_WIDESTRING_MEMMGR}
    FString: WideString;
    {$ENDIF ~OWN_WIDESTRING_MEMMGR}
    FObject: TObject;
  end;

  TWideStringItemList = array of TWideStringItem;

  TWideStringList = class(TWideStrings)
  private
    FList: TWideStringItemList;
    FCount: Integer;
    FSorted: Boolean;
    FDuplicates: TDuplicates;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure Grow;
    procedure QuickSort(L, R: Integer);
    procedure InsertItem(Index: Integer; const S: WideString);
    procedure SetSorted(Value: Boolean);
    {$IFDEF OWN_WIDESTRING_MEMMGR}
    procedure SetListString(Index: Integer; const S: WideString);
    {$ENDIF OWN_WIDESTRING_MEMMGR}
  protected
    procedure Changed; virtual;
    procedure Changing; virtual;
    function Get(Index: Integer): WideString; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: WideString); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetUpdateState(Updating: Boolean); override;
    procedure SetLanguage(Value: LCID); override;
  public
    destructor Destroy; override;

    function Add(const S: WideString): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    function Find(const S: WideString; var Index: Integer): Boolean; virtual;
    function IndexOf(const S: WideString): Integer; override;
    procedure Insert(Index: Integer; const S: WideString); override;
    procedure Sort; virtual;

    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: Boolean read FSorted write SetSorted;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
  end;

{$ENDIF ~UNICODE_RTL_DATABASE}

{
// all these functions are now in JclWideStrings.pas
function StrLenW(Str: PWideChar): SizeInt;
function StrEndW(Str: PWideChar): PWideChar;
function StrMoveW(Dest, Source: PWideChar; Count: SizeInt): PWideChar;
function StrCopyW(Dest, Source: PWideChar): PWideChar;
function StrECopyW(Dest, Source: PWideChar): PWideChar;
function StrLCopyW(Dest, Source: PWideChar; MaxLen: SizeInt): PWideChar;
function StrPCopyWW(Dest: PWideChar; const Source: WideString): PWideChar; overload;
function StrPCopyW(Dest: PWideChar; const Source: AnsiString): PWideChar;
function StrPLCopyWW(Dest: PWideChar; const Source: WideString; MaxLen: SizeInt): PWideChar;
function StrPLCopyW(Dest: PWideChar; const Source: AnsiString; MaxLen: SizeInt): PWideChar;
function StrCatW(Dest: PWideChar; const Source: PWideChar): PWideChar;
function StrLCatW(Dest, Source: PWideChar; MaxLen: SizeInt): PWideChar;
function StrCompW(const Str1, Str2: PWideChar): Integer;
function StrICompW(const Str1, Str2: PWideChar): Integer;
function StrLCompW(const Str1, Str2: PWideChar; MaxLen: SizeInt): Integer;
function StrLICompW(const Str1, Str2: PWideChar; MaxLen: SizeInt): Integer;
function StrNScanW(const Str1, Str2: PWideChar): SizeInt;
function StrRNScanW(const Str1, Str2: PWideChar): SizeInt;
function StrScanW(Str: PWideChar; Chr: WideChar): PWideChar; overload;
function StrScanW(Str: PWideChar; Chr: WideChar; StrLen: SizeInt): PWideChar; overload;
function StrRScanW(Str: PWideChar; Chr: WideChar): PWideChar;
function StrPosW(Str, SubStr: PWideChar): PWideChar;
function StrAllocW(WideSize: SizeInt): PWideChar;
function StrBufSizeW(const Str: PWideChar): SizeInt;
function StrNewW(const Str: PWideChar): PWideChar; overload;
function StrNewW(const Str: WideString): PWideChar; overload;
procedure StrDisposeW(Str: PWideChar);
procedure StrDisposeAndNilW(var Str: PWideChar);
procedure StrSwapByteOrder(Str: PWideChar);
}

// functions involving Delphi wide strings
function WideAdjustLineBreaks(const S: WideString): WideString;
function WideCharPos(const S: WideString; const Ch: WideChar; const Index: SizeInt): SizeInt;  //az
{$IFNDEF UNICODE_RTL_DATABASE}
function WideCompose(const S: WideString; Compatible: Boolean = True): WideString; overload;
function WideCompose(const S: WideString; Tags: TCompatibilityFormattingTags): WideString; overload;
function WideDecompose(const S: WideString; Compatible: Boolean = True): WideString; overload;
function WideDecompose(const S: WideString; Tags: TCompatibilityFormattingTags): WideString; overload;
{$ENDIF ~UNICODE_RTL_DATABASE}
function WideExtractQuotedStr(var Src: PWideChar; Quote: WideChar): WideString;
function WideQuotedStr(const S: WideString; Quote: WideChar): WideString;
function WideStringOfChar(C: WideChar; Count: SizeInt): WideString;

// case conversion function
type
  TCaseType = (ctFold, ctLower, ctTitle, ctUpper);

{$IFNDEF UNICODE_RTL_DATABASE}
function WideNormalize(const S: WideString; Form: TNormalizationForm): WideString;

function WideCaseConvert(C: WideChar; CaseType: TCaseType): WideString; overload;
function WideCaseConvert(const S: WideString; CaseType: TCaseType): WideString; overload;
function WideCaseFolding(C: WideChar): WideString; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function WideCaseFolding(const S: WideString): WideString; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function WideTitleCase(C: WideChar): WideString; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function WideTitleCase(const S: WideString): WideString; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
{$ENDIF ~UNICODE_RTL_DATABASE}
function WideLowerCase(C: WideChar): WideString; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function WideLowerCase(const S: WideString): WideString; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function WideUpperCase(C: WideChar): WideString; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function WideUpperCase(const S: WideString): WideString; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}

function WideSameText(const Str1, Str2: WideString): Boolean;
function WideTrim(const S: WideString): WideString;
function WideTrimLeft(const S: WideString): WideString;
function WideTrimRight(const S: WideString): WideString;

type
  // result type for number retrieval functions
  TUcNumber = record
    Numerator,
    Denominator: Integer;
  end;

// Low level character routines
{$IFNDEF UNICODE_RTL_DATABASE}
function UnicodeNumberLookup(Code: UCS4; var Number: TUcNumber): Boolean;
function UnicodeCompose(const Codes: array of UCS4; out Composite: UCS4; Compatible: Boolean = True): Integer; overload;
function UnicodeCompose(const Codes: array of UCS4; out Composite: UCS4; Tags: TCompatibilityFormattingTags): Integer; overload;
function UnicodeCaseFold(Code: UCS4): TUCS4Array;
function UnicodeDecompose(Code: UCS4; Compatible: Boolean = True): TUCS4Array; overload;
function UnicodeDecompose(Code: UCS4; Tags: TCompatibilityFormattingTags): TUCS4Array; overload;
function UnicodeToTitle(Code: UCS4): TUCS4Array;
{$ENDIF ~UNICODE_RTL_DATABASE}
function UnicodeToUpper(Code: UCS4): TUCS4Array;
function UnicodeToLower(Code: UCS4): TUCS4Array;

// Character test routines
function UnicodeIsAlpha(C: UCS4): Boolean;
function UnicodeIsDigit(C: UCS4): Boolean;
function UnicodeIsAlphaNum(C: UCS4): Boolean;
function UnicodeIsNumberOther(C: UCS4): Boolean;
function UnicodeIsCased(C: UCS4): Boolean;
function UnicodeIsControl(C: UCS4): Boolean;
function UnicodeIsSpace(C: UCS4): Boolean;
function UnicodeIsWhiteSpace(C: UCS4): Boolean;
function UnicodeIsBlank(C: UCS4): Boolean;
function UnicodeIsPunctuation(C: UCS4): Boolean;
function UnicodeIsGraph(C: UCS4): Boolean;
function UnicodeIsPrintable(C: UCS4): Boolean;
function UnicodeIsUpper(C: UCS4): Boolean;
function UnicodeIsLower(C: UCS4): Boolean;
function UnicodeIsTitle(C: UCS4): Boolean;
{$IFNDEF UNICODE_RTL_DATABASE}
function UnicodeIsHexDigit(C: UCS4): Boolean;
{$ENDIF ~UNICODE_RTL_DATABASE}
function UnicodeIsIsoControl(C: UCS4): Boolean;
function UnicodeIsFormatControl(C: UCS4): Boolean;
function UnicodeIsSymbol(C: UCS4): Boolean;
function UnicodeIsNumber(C: UCS4): Boolean;
function UnicodeIsNonSpacing(C: UCS4): Boolean;
function UnicodeIsOpenPunctuation(C: UCS4): Boolean;
function UnicodeIsClosePunctuation(C: UCS4): Boolean;
function UnicodeIsInitialPunctuation(C: UCS4): Boolean;
function UnicodeIsFinalPunctuation(C: UCS4): Boolean;
{$IFNDEF UNICODE_RTL_DATABASE}
function UnicodeIsComposed(C: UCS4): Boolean;
function UnicodeIsQuotationMark(C: UCS4): Boolean;
function UnicodeIsSymmetric(C: UCS4): Boolean;
function UnicodeIsMirroring(C: UCS4): Boolean;
function UnicodeIsNonBreaking(C: UCS4): Boolean;

// Directionality functions
function UnicodeIsRightToLeft(C: UCS4): Boolean;
function UnicodeIsLeftToRight(C: UCS4): Boolean;
function UnicodeIsStrong(C: UCS4): Boolean;
function UnicodeIsWeak(C: UCS4): Boolean;
function UnicodeIsNeutral(C: UCS4): Boolean;
function UnicodeIsSeparator(C: UCS4): Boolean;

// Other character test functions
function UnicodeIsMark(C: UCS4): Boolean;
function UnicodeIsModifier(C: UCS4): Boolean;
{$ENDIF ~UNICODE_RTL_DATABASE}
function UnicodeIsLetterNumber(C: UCS4): Boolean;
function UnicodeIsConnectionPunctuation(C: UCS4): Boolean;
function UnicodeIsDash(C: UCS4): Boolean;
function UnicodeIsMath(C: UCS4): Boolean;
function UnicodeIsCurrency(C: UCS4): Boolean;
function UnicodeIsModifierSymbol(C: UCS4): Boolean;
function UnicodeIsSpacingMark(C: UCS4): Boolean;
function UnicodeIsEnclosing(C: UCS4): Boolean;
function UnicodeIsPrivate(C: UCS4): Boolean;
function UnicodeIsSurrogate(C: UCS4): Boolean;
function UnicodeIsLineSeparator(C: UCS4): Boolean;
function UnicodeIsParagraphSeparator(C: UCS4): Boolean;
function UnicodeIsIdentifierStart(C: UCS4): Boolean;
function UnicodeIsIdentifierPart(C: UCS4): Boolean;
function UnicodeIsDefined(C: UCS4): Boolean;
function UnicodeIsUndefined(C: UCS4): Boolean;
function UnicodeIsHan(C: UCS4): Boolean;
function UnicodeIsHangul(C: UCS4): Boolean;

function UnicodeIsUnassigned(C: UCS4): Boolean;
function UnicodeIsLetterOther(C: UCS4): Boolean;
function UnicodeIsConnector(C: UCS4): Boolean;
function UnicodeIsPunctuationOther(C: UCS4): Boolean;
function UnicodeIsSymbolOther(C: UCS4): Boolean;
{$IFNDEF UNICODE_RTL_DATABASE}
function UnicodeIsLeftToRightEmbedding(C: UCS4): Boolean;
function UnicodeIsLeftToRightOverride(C: UCS4): Boolean;
function UnicodeIsRightToLeftArabic(C: UCS4): Boolean;
function UnicodeIsRightToLeftEmbedding(C: UCS4): Boolean;
function UnicodeIsRightToLeftOverride(C: UCS4): Boolean;
function UnicodeIsPopDirectionalFormat(C: UCS4): Boolean;
function UnicodeIsEuropeanNumber(C: UCS4): Boolean;
function UnicodeIsEuropeanNumberSeparator(C: UCS4): Boolean;
function UnicodeIsEuropeanNumberTerminator(C: UCS4): Boolean;
function UnicodeIsArabicNumber(C: UCS4): Boolean;
function UnicodeIsCommonNumberSeparator(C: UCS4): Boolean;
function UnicodeIsBoundaryNeutral(C: UCS4): Boolean;
function UnicodeIsSegmentSeparator(C: UCS4): Boolean;
function UnicodeIsOtherNeutrals(C: UCS4): Boolean;
function UnicodeIsASCIIHexDigit(C: UCS4): Boolean;
function UnicodeIsBidiControl(C: UCS4): Boolean;
function UnicodeIsDeprecated(C: UCS4): Boolean;
function UnicodeIsDiacritic(C: UCS4): Boolean;
function UnicodeIsExtender(C: UCS4): Boolean;
function UnicodeIsHyphen(C: UCS4): Boolean;
function UnicodeIsIdeographic(C: UCS4): Boolean;
function UnicodeIsIDSBinaryOperator(C: UCS4): Boolean;
function UnicodeIsIDSTrinaryOperator(C: UCS4): Boolean;
function UnicodeIsJoinControl(C: UCS4): Boolean;
function UnicodeIsLogicalOrderException(C: UCS4): Boolean;
function UnicodeIsNonCharacterCodePoint(C: UCS4): Boolean;
function UnicodeIsOtherAlphabetic(C: UCS4): Boolean;
function UnicodeIsOtherDefaultIgnorableCodePoint(C: UCS4): Boolean;
function UnicodeIsOtherGraphemeExtend(C: UCS4): Boolean;
function UnicodeIsOtherIDContinue(C: UCS4): Boolean;
function UnicodeIsOtherIDStart(C: UCS4): Boolean;
function UnicodeIsOtherLowercase(C: UCS4): Boolean;
function UnicodeIsOtherMath(C: UCS4): Boolean;
function UnicodeIsOtherUppercase(C: UCS4): Boolean;
function UnicodeIsPatternSyntax(C: UCS4): Boolean;
function UnicodeIsPatternWhiteSpace(C: UCS4): Boolean;
function UnicodeIsRadical(C: UCS4): Boolean;
function UnicodeIsSoftDotted(C: UCS4): Boolean;
function UnicodeIsSTerm(C: UCS4): Boolean;
function UnicodeIsTerminalPunctuation(C: UCS4): Boolean;
function UnicodeIsUnifiedIdeograph(C: UCS4): Boolean;
function UnicodeIsVariationSelector(C: UCS4): Boolean;
{$ENDIF ~UNICODE_RTL_DATABASE}

// Utility functions
function CharSetFromLocale(Language: LCID): Byte;
function GetCharSetFromLocale(Language: LCID; out FontCharSet: Byte): Boolean;
function CodePageFromLocale(Language: LCID): Word;
function CodeBlockName(const CB: TUnicodeBlock): string;
function CodeBlockRange(const CB: TUnicodeBlock): TUnicodeBlockRange;
function CodeBlockFromChar(const C: UCS4): TUnicodeBlock;
function KeyboardCodePage: Word;
function KeyUnicode(C: Char): WideChar;
function StringToWideStringEx(const S: AnsiString; CodePage: Word): WideString;
function TranslateString(const S: AnsiString; CP1, CP2: Word): AnsiString;
function WideStringToStringEx(const WS: WideString; CodePage: Word): AnsiString;

type
  TCompareFunc = function (const W1, W2: WideString; Locale: LCID): Integer;

var
  WideCompareText: TCompareFunc;

type
  EJclUnicodeError = class(EJclError);

// functions to load Unicode data from resource
procedure LoadCharacterCategories;
procedure LoadCaseMappingData;
procedure LoadDecompositionData;
procedure LoadCombiningClassData;
procedure LoadNumberData;
procedure LoadCompositionData;

// functions around TUCS4Array
function UCS4Array(Ch: UCS4): TUCS4Array;
function UCS4ArrayConcat(Left, Right: UCS4): TUCS4Array; overload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
procedure UCS4ArrayConcat(var Left: TUCS4Array; Right: UCS4); overload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
procedure UCS4ArrayConcat(var Left: TUCS4Array; const Right: TUCS4Array); overload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
function UCS4ArrayEquals(const Left: TUCS4Array; const Right: TUCS4Array): Boolean; overload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
function UCS4ArrayEquals(const Left: TUCS4Array; Right: UCS4): Boolean; overload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
function UCS4ArrayEquals(const Left: TUCS4Array; const Right: AnsiString): Boolean; overload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
function UCS4ArrayEquals(const Left: TUCS4Array; Right: AnsiChar): Boolean; overload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

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

// Unicode data for case mapping, decomposition, numbers etc. This data is
// loaded on demand which means only those parts will be put in memory which are
// needed by one of the lookup functions.
// Note: There is a little tool called UDExtract which creates a resouce script from
//       the Unicode database file which can be compiled to the needed res file.
//       This tool, including its source code, can be downloaded from www.lischke-online.de/Unicode.html.

{$IFNDEF UNICODE_RTL_DATABASE}
{$IFDEF UNICODE_RAW_DATA}
{$R JclUnicode.res}
{$ENDIF UNICODE_RAW_DATA}
{$IFDEF UNICODE_BZIP2_DATA}
{$R JclUnicodeBZip2.res}
{$ENDIF UNICODE_BZIP2_DATA}
{$IFDEF UNICODE_ZLIB_DATA}
{$R JclUnicodeZLib.res}
{$ENDIF UNICODE_ZLIB_DATA}
{$ENDIF ~UNICODE_RTL_DATABASE}

uses
  {$IFDEF HAS_UNIT_RTLCONSTS}
  {$IFDEF HAS_UNITSCOPE}
  System.RtlConsts,
  {$ELSE ~HAS_UNITSCOPE}
  RtlConsts,
  {$ENDIF ~HAS_UNITSCOPE}
  {$ENDIF HAS_UNIT_RTLCONSTS}
  {$IFNDEF UNICODE_RTL_DATABASE}
  {$IFDEF UNICODE_BZIP2_DATA}
  BZip2,
  {$ENDIF UNICODE_BZIP2_DATA}
  {$IFDEF UNICODE_ZLIB_DATA}
  ZLibh,
  {$ENDIF UNICODE_ZLIB_DATA}
  JclStreams,
  {$IFNDEF UNICODE_RAW_DATA}
  JclCompression,
  {$ENDIF ~UNICODE_RAW_DATA}
  {$ENDIF ~UNICODE_RTL_DATABASE}
  JclResources, JclSynch, JclSysUtils, JclSysInfo, JclStringConversions, JclWideStrings;

const
  {$IFDEF FPC} // declarations from unit [Rtl]Consts
  SDuplicateString = 'String list does not allow duplicates';
  SListIndexError = 'List index out of bounds (%d)';
  SSortedListError = 'Operation not allowed on sorted string list';
  {$ENDIF FPC}
  // some predefined sets to shorten parameter lists below and ease repeative usage
  ClassLetter = [ccLetterUppercase, ccLetterLowercase, ccLetterTitlecase, ccLetterModifier, ccLetterOther];
  ClassSpace = [ccSeparatorSpace];
  ClassPunctuation = [ccPunctuationConnector, ccPunctuationDash, ccPunctuationOpen, ccPunctuationClose,
    ccPunctuationOther, ccPunctuationInitialQuote, ccPunctuationFinalQuote];
  ClassMark = [ccMarkNonSpacing, ccMarkSpacingCombining, ccMarkEnclosing];
  ClassNumber = [ccNumberDecimalDigit, ccNumberLetter, ccNumberOther];
  ClassSymbol = [ccSymbolMath, ccSymbolCurrency, ccSymbolModifier, ccSymbolOther];
  ClassEuropeanNumber = [ccEuropeanNumber, ccEuropeanNumberSeparator, ccEuropeanNumberTerminator];

  // used to negate a set of categories
  ClassAll = [Low(TCharacterCategory)..High(TCharacterCategory)];

{$IFDEF HAS_UNIT_CHARACTER}
function CharacterCategoriesToUnicodeCategory(const Categories: TCharacterCategories): TUnicodeCategory;
var
  Category: TCharacterUnicodeCategory;
begin
  for Category := Low(TCharacterUnicodeCategory) to High(TCharacterUnicodeCategory) do
    if Category in Categories then
  begin
    Result := CharacterCategoryToUnicodeCategory[Category];
    Exit;
  end;
  Result := TUnicodeCategory.ucUnassigned;
end;

function UnicodeCategoryToCharacterCategories(Category: TUnicodeCategory): TCharacterCategories;
begin
  Result := [];
  Include(Result, UnicodeCategoryToCharacterCategory[Category]);
end;
{$ENDIF HAS_UNIT_CHARACTER}

{$IFDEF UNICODE_RTL_DATABASE}
procedure LoadCharacterCategories;
begin
  // do nothing, the RTL database is already loaded
end;

procedure LoadCaseMappingData;
begin
  // do nothing, the RTL database is already loaded
end;

procedure LoadDecompositionData;
begin
  // do nothing, the RTL database is already loaded
end;

procedure LoadCombiningClassData;
begin
  // do nothing, the RTL database is already loaded
end;

procedure LoadNumberData;
begin
  // do nothing, the RTL database is already loaded
end;

procedure LoadCompositionData;
begin
  // do nothing, the RTL database is already loaded
end;
{$ELSE ~UNICODE_RTL_DATABASE}
var
  // As the global data can be accessed by several threads it should be guarded
  // while the data is loaded.
  LoadInProgress: TJclCriticalSection;

function OpenResourceStream(const ResName: string): TJclEasyStream;
var
  ResourceStream: TStream;
  {$IFNDEF UNICODE_RAW_DATA}
  DecompressionStream: TStream;
  RawStream: TMemoryStream;
  {$ENDIF ~UNICODE_RAW_DATA}
begin
  ResourceStream := TResourceStream.Create(HInstance, ResName, 'UNICODEDATA');
  {$IFDEF UNICODE_RAW_DATA}
  Result := TJclEasyStream.Create(ResourceStream, True);
  {$ENDIF UNICODE_RAW_DATA}
  {$IFDEF UNICODE_BZIP2_DATA}
  try
    LoadBZip2;
    DecompressionStream := TJclBZIP2DecompressionStream.Create(ResourceStream);
    try
      RawStream := TMemoryStream.Create;
      StreamCopy(DecompressionStream, RawStream);
      RawStream.Seek(0, soBeginning);
      Result := TJclEasyStream.Create(RawStream, True);
    finally
      DecompressionStream.Free;
    end;
  finally
    ResourceStream.Free;
  end;
  {$ENDIF UNICODE_BZIP2_DATA}
  {$IFDEF UNICODE_ZLIB_DATA}
  try
    LoadZLib;
    DecompressionStream := TJclZLibDecompressStream.Create(ResourceStream);
    try
      RawStream := TMemoryStream.Create;
      StreamCopy(DecompressionStream, RawStream);
      RawStream.Seek(0, soBeginning);
      Result := TJclEasyStream.Create(RawStream, True);
    finally
      DecompressionStream.Free;
    end;
  finally
    ResourceStream.Free;
  end;
  {$ENDIF UNICODE_ZLIB_DATA}
end;

function StreamReadChar(Stream: TStream): Cardinal;
begin
  Result := 0;
  Stream.ReadBuffer(Result, 3);
end;

//----------------- support for character categories -----------------------------------------------

// Character category data is quite a large block since every defined character in Unicode is assigned at least
// one category. Because of this we cannot use a sparse matrix to provide quick access as implemented for
// e.g. composition data.
// The approach used here is based on the fact that an application seldomly uses all characters defined in Unicode
// simultanously. In fact the opposite is true. Most application will use either Western Europe or Arabic or
// Far East character data, but very rarely all together. Based on this fact is the implementation of virtual
// memory using the systems paging file (aka file mapping) to load only into virtual memory what is used currently.
// The implementation is not yet finished and needs a lot of improvements yet.

type
  // start and stop of a range of code points
  TRange = record
    Start,
    Stop: Cardinal;
  end;

  TRangeArray = array of TRange;
  TCategoriesArray = array of array of TCharacterCategories;

var
  // character categories, stored in the system's swap file and mapped on demand
  CategoriesLoaded: Boolean;
  Categories: array [Byte] of TCategoriesArray;

procedure LoadCharacterCategories;
// Loads the character categories data (as saved by the Unicode database extractor, see also
// the comments about JclUnicode.res above).
var
  Size: Integer;
  Stream: TJclEasyStream;
  Category: TCharacterCategory;
  Buffer: TRangeArray;
  First, Second, Third: Byte;
  J, K: Integer;
begin
  // Data already loaded?
  if not CategoriesLoaded then
  begin
    // make sure no other code is currently modifying the global data area
    LoadInProgress.Enter;
    try
      CategoriesLoaded := True;
      Stream := OpenResourceStream('CATEGORIES');
      try
        while Stream.Position < Stream.Size do
        begin
          // a) read which category is current in the stream
          Category := TCharacterCategory(Stream.ReadByte);
          // b) read the size of the ranges and the ranges themself
          Size := Stream.ReadInteger;
          if Size > 0 then
          begin
            SetLength(Buffer, Size);
            for J := 0 to Size - 1 do
            begin
              Buffer[J].Start := StreamReadChar(Stream);
              Buffer[J].Stop := StreamReadChar(Stream);
            end;

            // c) go through every range and add the current category to each code point
            for J := 0 to Size - 1 do
              for K := Buffer[J].Start to Buffer[J].Stop do
              begin
                Assert(K < $1000000, LoadResString(@RsCategoryUnicodeChar));

                First := (K shr 16) and $FF;
                Second := (K shr 8) and $FF;
                Third := K and $FF;
                // add second step array if not yet done
                if Categories[First] = nil then
                  SetLength(Categories[First], 256);
                if Categories[First, Second] = nil then
                  SetLength(Categories[First, Second], 256);
                // The array is allocated on the exact size, but the compiler generates
                // a 32 bit "BTS" instruction that accesses memory beyond the allocated block.
                if Third < 255 then
                  Include(Categories[First, Second, Third], Category)
                else
                  Categories[First, Second, Third] := Categories[First, Second, Third] + [Category];
              end;
          end;
        end;
        // Assert(Stream.Position = Stream.Size);
      finally
        Stream.Free;
      end;
    finally
      LoadInProgress.Leave;
    end;
  end;
end;

function CategoryLookup(Code: Cardinal; Cats: TCharacterCategories): Boolean; overload;
// determines whether the Code is in the given category
var
  First, Second, Third: Byte;
begin
  Assert(Code < $1000000, LoadResString(@RsCategoryUnicodeChar));

  // load property data if not already done
  if not CategoriesLoaded then
    LoadCharacterCategories;

  First := (Code shr 16) and $FF;
  Second := (Code shr 8) and $FF;
  Third := Code and $FF;
  if (Categories[First] <> nil) and (Categories[First, Second] <> nil) then
    Result := Categories[First, Second, Third] * Cats <> []
  else
    Result := False;
end;

//----------------- support for case mapping -------------------------------------------------------

type
  TCase = array [TCaseType] of TUCS4Array; // mapping for case fold, lower, title and upper in this order
  TCaseArray = array of array of TCase;

var
  // An array for all case mappings (including 1 to many casing if saved by the extraction program).
  // The organization is a sparse, two stage matrix.
  // SingletonMapping is to quickly return a single default mapping.
  CaseDataLoaded: Boolean;
  CaseMapping: array [Byte] of TCaseArray;

procedure LoadCaseMappingData;
var
  Stream: TJclEasyStream;
  I, J, Code, Size: Integer;
  First, Second, Third: Byte;
begin
  if not CaseDataLoaded then
  begin
    // make sure no other code is currently modifying the global data area
    LoadInProgress.Enter;

    try
      CaseDataLoaded := True;
      Stream := OpenResourceStream('CASE');
      try
        // the first entry in the stream is the number of entries in the case mapping table
        Size := Stream.ReadInteger;
        for I := 0 to Size - 1 do
        begin
          // a) read actual code point
          Code := StreamReadChar(Stream);
          Assert(Code < $1000000, LoadResString(@RsCasedUnicodeChar));

          // if there is no high byte entry in the first stage table then create one
          First := (Code shr 16) and $FF;
          Second := (Code shr 8) and $FF;
          Third := Code and $FF;
          if CaseMapping[First] = nil then
            SetLength(CaseMapping[First], 256);
          if CaseMapping[First, Second] = nil then
            SetLength(CaseMapping[First, Second], 256);

          // b) read fold case array
          Size := Stream.ReadByte;
          if Size > 0 then
          begin
            SetLength(CaseMapping[First, Second, Third, ctFold], Size);
            for J := 0 to Size - 1 do
              CaseMapping[First, Second, Third, ctFold, J] := StreamReadChar(Stream);
          end;
          // c) read lower case array
          Size := Stream.ReadByte;
          if Size > 0 then
          begin
            SetLength(CaseMapping[First, Second, Third, ctLower], Size);
            for J := 0 to Size - 1 do
              CaseMapping[First, Second, Third, ctLower, J] := StreamReadChar(Stream);
          end;
          // d) read title case array
          Size := Stream.ReadByte;
          if Size > 0 then
          begin
            SetLength(CaseMapping[First, Second, Third, ctTitle], Size);
            for J := 0 to Size - 1 do
              CaseMapping[First, Second, Third, ctTitle, J] := StreamReadChar(Stream);
          end;
          // e) read upper case array
          Size := Stream.ReadByte;
          if Size > 0 then
          begin
            SetLength(CaseMapping[First, Second, Third, ctUpper], Size);
            for J := 0 to Size - 1 do
              CaseMapping[First, Second, Third, ctUpper, J] := StreamReadChar(Stream);
          end;
        end;
        Assert(Stream.Position = Stream.Size);
      finally
        Stream.Free;
      end;
    finally
      LoadInProgress.Leave;
    end;
  end;
end;

function CaseLookup(Code: Cardinal; CaseType: TCaseType; var Mapping: TUCS4Array): Boolean;
// Performs a lookup of the given code; returns True if Found, with Mapping referring to the mapping.
// ctFold is handled specially: if no mapping is found then result of looking up ctLower
//   is returned
var
  First, Second, Third: Byte;
begin
  Assert(Code < $1000000, LoadResString(@RsCasedUnicodeChar));

  // load case mapping data if not already done
  if not CaseDataLoaded then
    LoadCaseMappingData;

  First := (Code shr 16) and $FF;
  Second := (Code shr 8) and $FF;
  Third := Code and $FF;
  // Check first stage table whether there is a mapping for a particular block and
  // (if so) then whether there is a mapping or not.
  if (CaseMapping[First] <> nil) and (CaseMapping[First, Second] <> nil) and
     (CaseMapping[First, Second, Third, CaseType] <> nil) then
    Mapping := CaseMapping[First, Second, Third, CaseType]
  else
    Mapping := nil;
  Result := Assigned(Mapping);
  // defer to lower case if no fold case exists
  if not Result and (CaseType = ctFold) and (CaseMapping[First] <> nil) and
    (CaseMapping[First, Second] <> nil) and (CaseMapping[First, Second, Third, ctLower] <> nil) then
  begin
    Mapping := CaseMapping[First, Second, Third, ctLower];
    Result := Assigned(Mapping);
  end;
end;

function UnicodeCaseFold(Code: UCS4): TUCS4Array;
// This function returnes an array of special case fold mappings if there is one defined for the given
// code, otherwise the lower case will be returned. This all applies only to cased code points.
// Uncased code points are returned unchanged.
begin
  SetLength(Result, 0);
  if not CaseLookup(Code, ctFold, Result) then
  begin
    SetLength(Result, 1);
    Result[0] := Code;
  end;
end;

{$ENDIF ~UNICODE_RTL_DATABASE}

function UnicodeToUpper(Code: UCS4): TUCS4Array;
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  SetLength(Result, 1);
  Result[0] := Ord(TCharacter.ToUpper(Chr(Code)));
  {$ELSE ~UNICODE_RTL_DATABASE}
  SetLength(Result, 0);
  if not CaseLookup(Code, ctUpper, Result) then
  begin
    SetLength(Result, 1);
    Result[0] := Code;
  end;
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function UnicodeToLower(Code: UCS4): TUCS4Array;
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  SetLength(Result, 1);
  Result[0] := Ord(TCharacter.ToLower(Chr(Code)));
  {$ELSE ~UNICODE_RTL_DATABASE}
  SetLength(Result, 0);
  if not CaseLookup(Code, ctLower, Result) then
  begin
    SetLength(Result, 1);
    Result[0] := Code;
  end;
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

{$IFNDEF UNICODE_RTL_DATABASE}

function UnicodeToTitle(Code: UCS4): TUCS4Array;
begin
  SetLength(Result, 0);
  if not CaseLookup(Code, ctTitle, Result) then
  begin
    SetLength(Result, 1);
    Result[0] := Code;
  end;
end;

//----------------- support for decomposition ------------------------------------------------------

const
  // constants for hangul composition and hangul-to-jamo decomposition
  SBase = $AC00;             // hangul syllables start code point
  LBase = $1100;             // leading syllable
  VBase = $1161;
  TBase = $11A7;             // trailing syllable
  LCount = 19;
  VCount = 21;
  TCount = 28;
  NCount = VCount * TCount;   // 588
  SCount = LCount * NCount;   // 11172

type
  TDecomposition = record
    Tag: TCompatibilityFormattingTag;
    Leaves: TUCS4Array;
  end;
  TDecompositions = array of array of TDecomposition;
  TDecompositionsArray = array [Byte] of TDecompositions;

var
  // list of decompositions, organized (again) as three stage matrix
  // Note: there are two tables, one for canonical decompositions and the other one
  //       for compatibility decompositions.
  DecompositionsLoaded: Boolean;
  Decompositions: TDecompositionsArray;

procedure LoadDecompositionData;
var
  Stream: TJclEasyStream;
  I, J, Code, Size: Integer;
  First, Second, Third: Byte;
begin
  if not DecompositionsLoaded then
  begin
    // make sure no other code is currently modifying the global data area
    LoadInProgress.Enter;

    try
      DecompositionsLoaded := True;
      Stream := OpenResourceStream('DECOMPOSITION');
      try
        // determine how many decomposition entries we have
        Size := Stream.ReadInteger;
        for I := 0 to Size - 1 do
        begin
          Code := StreamReadChar(Stream);

          Assert(Code < $1000000, LoadResString(@RsDecomposedUnicodeChar));

          First := (Code shr 16) and $FF;
          Second := (Code shr 8) and $FF;
          Third := Code and $FF;

          // if there is no high byte entry in the first stage table then create one
          if Decompositions[First] = nil then
            SetLength(Decompositions[First], 256);
          if Decompositions[First, Second] = nil then
            SetLength(Decompositions[First, Second], 256);

          Size := Stream.ReadByte;
          if Size > 0 then
          begin
            Decompositions[First, Second, Third].Tag := TCompatibilityFormattingTag(Stream.ReadByte);
            SetLength(Decompositions[First, Second, Third].Leaves, Size);
            for J := 0 to Size - 1 do
              Decompositions[First, Second, Third].Leaves[J] := StreamReadChar(Stream);
          end;
        end;
        Assert(Stream.Position = Stream.Size);
      finally
        Stream.Free;
      end;
    finally
      LoadInProgress.Leave;
    end;
  end;
end;

function UnicodeDecomposeHangul(Code: UCS4): TUCS4Array;
// algorithmically decomposes hangul character
var
  Rest: Integer;
begin
  Dec(Code, SBase);
  Rest := Code mod TCount;
  if Rest = 0 then
    SetLength(Result, 2)
  else
    SetLength(Result, 3);
  Result[0] := LBase + (Code div NCount);
  Result[1] := VBase + ((Code mod NCount) div TCount);
  if Rest <> 0 then
    Result[2] := TBase + Rest;
end;

function UnicodeDecompose(Code: UCS4; Compatible: Boolean): TUCS4Array;
var
  First, Second, Third: Byte;
begin
  Assert(Code < $1000000, LoadResString(@RsDecomposedUnicodeChar));

  // load decomposition data if not already done
  if not DecompositionsLoaded then
    LoadDecompositionData;

  Result := nil;

  // if the code is hangul then decomposition is algorithmically
  if UnicodeIsHangul(Code) then
    Result := UnicodeDecomposeHangul(Code)
  else
  begin
    First := (Code shr 16) and $FF;
    Second := (Code shr 8) and $FF;
    Third := Code and $FF;

    if (Decompositions[First] <> nil) and (Decompositions[First, Second] <> nil)
      and (Decompositions[First, Second, Third].Leaves <> nil)
      and (Compatible or (Decompositions[First, Second, Third].Tag = cftCanonical)) then
      Result := Decompositions[First, Second, Third].Leaves
    else
      Result := nil;
  end;
end;

function UnicodeDecompose(Code: UCS4; Tags: TCompatibilityFormattingTags): TUCS4Array;
var
  First, Second, Third: Byte;
begin
  Assert(Code < $1000000, LoadResString(@RsDecomposedUnicodeChar));

  // load decomposition data if not already done
  if not DecompositionsLoaded then
    LoadDecompositionData;

  Result := nil;

  // if the code is hangul then decomposition is algorithmically
  if UnicodeIsHangul(Code) then
    Result := UnicodeDecomposeHangul(Code)
  else
  begin
    First := (Code shr 16) and $FF;
    Second := (Code shr 8) and $FF;
    Third := Code and $FF;

    if (Decompositions[First] <> nil) and (Decompositions[First, Second] <> nil)
      and (Decompositions[First, Second, Third].Leaves <> nil)
      and (Decompositions[First, Second, Third].Tag in Tags) then
      Result := Decompositions[First, Second, Third].Leaves
    else
      Result := nil;
  end;
end;

//----------------- support for combining classes --------------------------------------------------

type
  TClassArray = array of array of Byte;

var
  // canonical combining classes, again as two stage matrix
  CCCsLoaded: Boolean;
  CCCs: array [Byte] of TClassArray;

procedure LoadCombiningClassData;
var
  Stream: TJclEasyStream;
  I, J, K, Size: Integer;
  Buffer: TRangeArray;
  First, Second, Third: Byte;
begin
  // make sure no other code is currently modifying the global data area
  LoadInProgress.Enter;

  try
    if not CCCsLoaded then
    begin
      CCCsLoaded := True;
      Stream := OpenResourceStream('COMBINING');
      try
        while Stream.Position < Stream.Size do
        begin
          // a) determine which class is stored here
          I := Stream.ReadByte;
          // b) determine how many ranges are assigned to this class
          Size := Stream.ReadByte;
          // c) read start and stop code of each range
          if Size > 0 then
          begin
            SetLength(Buffer, Size);
            for J := 0 to Size - 1 do
            begin
              Buffer[J].Start := StreamReadChar(Stream);
              Buffer[J].Stop := StreamReadChar(Stream);
            end;

            // d) put this class in every of the code points just loaded
            for J := 0 to Size - 1 do
              for K := Buffer[J].Start to Buffer[J].Stop do
              begin
                // (outchy) TODO: handle in a cleaner way
                Assert(K < $1000000, LoadResString(@RsCombiningClassUnicodeChar));
                First := (K shr 16) and $FF;
                Second := (K shr 8) and $FF;
                Third := K and $FF;
                // add second step array if not yet done
                if CCCs[First] = nil then
                  SetLength(CCCs[First], 256);
                if CCCs[First, Second] = nil then
                  SetLength(CCCs[First, Second], 256);
                CCCs[First, Second, Third] := I;
              end;
          end;
        end;
        // Assert(Stream.Position = Stream.Size);
      finally
        Stream.Free;
      end;
    end;
  finally
    LoadInProgress.Leave;
  end;
end;

function CanonicalCombiningClass(Code: Cardinal): Cardinal;
var
  First, Second, Third: Byte;
begin
  Assert(Code < $1000000, LoadResString(@RsCombiningClassUnicodeChar));

  // load combining class data if not already done
  if not CCCsLoaded then
    LoadCombiningClassData;

  First := (Code shr 16) and $FF;
  Second := (Code shr 8) and $FF;
  Third := Code and $FF;
  if (CCCs[First] <> nil) and (CCCs[First, Second] <> nil) then
    Result := CCCs[First, Second, Third]
  else
    Result := 0;
end;

//----------------- support for numeric values -----------------------------------------------------

type
  // structures for handling numbers
  TCodeIndex = record
    Code,
    Index: Cardinal;
  end;

var
  // array to hold the number equivalents for specific codes
  NumberCodes: array of TCodeIndex;
  // array of numbers used in NumberCodes
  Numbers: array of TUcNumber;

procedure LoadNumberData;
var
  Stream: TJclEasyStream;
  Size, I: Integer;
begin
  // make sure no other code is currently modifying the global data area
  LoadInProgress.Enter;

  try
    if NumberCodes = nil then
    begin
      Stream := OpenResourceStream('NUMBERS');
      try
        // Numbers are special (compared to other Unicode data) as they utilize two
        // arrays, one containing all used numbers (in nominator-denominator format) and
        // another one which maps a code point to one of the numbers in the first array.

        // a) determine size of numbers array
        Size := Stream.ReadByte;
        SetLength(Numbers, Size);
        // b) read numbers data
        for I := 0 to Size - 1 do
        begin
          Numbers[I].Numerator := Stream.ReadInteger;
          Numbers[I].Denominator := Stream.ReadInteger;
        end;
        // c) determine size of index array
        Size := Stream.ReadInteger;
        SetLength(NumberCodes, Size);
        // d) read index data
        for I := 0 to Size - 1 do
        begin
          NumberCodes[I].Code := StreamReadChar(Stream);
          NumberCodes[I].Index := Stream.ReadByte;
        end;
        Assert(Stream.Position = Stream.Size);
      finally
        Stream.Free;
      end;
    end;
  finally
    LoadInProgress.Leave;
  end;
end;

function UnicodeNumberLookup(Code: UCS4; var Number: TUcNumber): Boolean;
// Searches for the given code and returns its number equivalent (if there is one).
// Typical cases are: '1/6' (U+2159), '3/8' (U+215C), 'XII' (U+216B) etc.
// Result is set to True if the code could be found.
var
  L, R, M: Integer;
begin
  // load number data if not already done
  if NumberCodes = nil then
    LoadNumberData;

  Result := False;
  L := 0;
  R := High(NumberCodes);
  while L <= R do
  begin
    M := (L + R) shr 1;
    if Code > NumberCodes[M].Code then
      L := M + 1
    else
    begin
      if Code < NumberCodes[M].Code then
        R := M - 1
      else
      begin
        Number := Numbers[NumberCodes[M].Index];
        Result := True;
        Break;
      end;
    end;
  end;
end;

//----------------- support for composition --------------------------------------------------------

type
  // maps between a pair of code points to a composite code point
  // Note: the source pair is packed into one 4 byte value to speed up search.
  TComposition = record
    Code: Cardinal;
    Tag: TCompatibilityFormattingTag;
    First: Cardinal;
    Next: array of Cardinal;
  end;

var
  // list of composition mappings
  Compositions: array of TComposition;
  MaxCompositionSize: Integer;

procedure LoadCompositionData;
var
  Stream: TJclEasyStream;
  I, J, Size: Integer;
begin
  // make sure no other code is currently modifying the global data area
  LoadInProgress.Enter;

  try
    if Compositions = nil then
    begin
      Stream := OpenResourceStream('COMPOSITION');
      try
        // a) determine size of compositions array
        Size := Stream.ReadInteger;
        SetLength(Compositions, Size);
        // b) read data
        for I := 0 to Size - 1 do
        begin
          Compositions[I].Code := StreamReadChar(Stream);
          Size := Stream.ReadByte;
          if Size > MaxCompositionSize then
            MaxCompositionSize := Size;
          SetLength(Compositions[I].Next, Size - 1);
          Compositions[I].Tag := TCompatibilityFormattingTag(Stream.ReadByte);
          Compositions[I].First := StreamReadChar(Stream);
          for J := 0 to Size - 2 do
            Compositions[I].Next[J] := StreamReadChar(Stream);
        end;
        Assert(Stream.Position = Stream.Size);
      finally
        Stream.Free;
      end;
    end;
  finally
    LoadInProgress.Leave;
  end;
end;

function UnicodeCompose(const Codes: array of UCS4; out Composite: UCS4; Compatible: Boolean): Integer;
// Maps the sequence of Codes (up to MaxCompositionSize codes) to a composite
// Result is the number of Codes that were composed (at least 1 if Codes is not empty)
var
  L, R, M, I, HighCodes, HighNext: Integer;
begin
  if Compositions = nil then
    LoadCompositionData;

  Result := 0;
  HighCodes := High(Codes);

  if HighCodes = -1 then
    Exit;

  if HighCodes = 0 then
  begin
    Result := 1;
    Composite := Codes[0];
    Exit;
  end;

  L := 0;
  R := High(Compositions);

  while L <= R do
  begin
    M := (L + R) shr 1;
    if Compositions[M].First > Codes[0] then
      R := M - 1
    else
    if Compositions[M].First < Codes[0] then
      L := M + 1
    else
    begin
      // back to the first element where Codes[0] = First
      while (M > 0) and (Compositions[M-1].First = Codes[0]) do
        Dec(M);

      while (M <= High(Compositions)) and (Compositions[M].First = Codes[0]) do
      begin
        HighNext := High(Compositions[M].Next);
        Result := 0;

        if (HighNext < HighCodes) // enough characters in buffer to be tested
          and (Compatible or (Compositions[M].Tag = cftCanonical)) then
        begin
          for I := 0 to HighNext do
            if Compositions[M].Next[I] = Codes[I + 1] then
              Result := I + 2 { +1 for first, +1 because of 0-based array }
            else
              Break;

          if Result = HighNext + 2 then // all codes matched
          begin
            Composite := Compositions[M].Code;
            Exit;
          end;
        end;

        Inc(M);
      end;
      Break;
    end;
  end;
  Result := 1;
  Composite := Codes[0];
end;

function UnicodeCompose(const Codes: array of UCS4; out Composite: UCS4; Tags: TCompatibilityFormattingTags): Integer;
// Maps the sequence of Codes (up to MaxCompositionSize codes) to a composite
// Result is the number of Codes that were composed (at least 1 if Codes is not empty)
var
  L, R, M, I, HighCodes, HighNext: Integer;
begin
  if Compositions = nil then
    LoadCompositionData;

  Result := 0;
  HighCodes := High(Codes);

  if HighCodes = -1 then
    Exit;

  if HighCodes = 0 then
  begin
    Result := 1;
    Composite := Codes[0];
    Exit;
  end;

  L := 0;
  R := High(Compositions);

  while L <= R do
  begin
    M := (L + R) shr 1;
    if Compositions[M].First > Codes[0] then
      R := M - 1
    else
    if Compositions[M].First < Codes[0] then
      L := M + 1
    else
    begin
      // back to the first element where Codes[0] = First
      while (M > 0) and (Compositions[M-1].First = Codes[0]) do
        Dec(M);

      while (M <= High(Compositions)) and (Compositions[M].First = Codes[0]) do
      begin
        HighNext := High(Compositions[M].Next);
        Result := 0;

        if (HighNext < HighCodes) // enough characters in buffer to be tested
          and (Compositions[M].Tag in Tags) then
        begin
          for I := 0 to HighNext do
            if Compositions[M].Next[I] = Codes[I + 1] then
              Result := I + 2 { +1 for first, +1 because of 0-based array }
            else
              Break;

          if Result = HighNext + 2 then // all codes matched
          begin
            Composite := Compositions[M].Code;
            Exit;
          end;
        end;

        Inc(M);
      end;
      Break;
    end;
  end;
  Result := 1;
  Composite := Codes[0];
end;

//=== { TSearchEngine } ======================================================

constructor TSearchEngine.Create(AOwner: TWideStrings);
begin
  inherited Create;
  FOwner := AOwner;
  FResults := TList.Create;
end;

destructor TSearchEngine.Destroy;
begin
  Clear;
  FResults.Free;
  inherited Destroy;
end;

procedure TSearchEngine.AddResult(Start, Stop: SizeInt);
begin
  FResults.Add(Pointer(Start));
  FResults.Add(Pointer(Stop));
end;

procedure TSearchEngine.Clear;
begin
  ClearResults;
end;

procedure TSearchEngine.ClearResults;
begin
  FResults.Clear;
end;

procedure TSearchEngine.DeleteResult(Index: SizeInt);
// explicitly deletes a search result
begin
  with FResults do
  begin
    // start index
    Delete(2 * Index);
    // stop index
    Delete(2 * Index);
  end;
end;

function TSearchEngine.GetCount: SizeInt;
// returns the number of matches found
begin
  Result := FResults.Count div 2;
end;

procedure TSearchEngine.GetResult(Index: SizeInt; var Start, Stop: SizeInt);
// returns the start position of a match (end position can be determined by
// adding the length of the pattern to the start position)
begin
  Start := SizeInt(FResults[2 * Index]);
  Stop := SizeInt(FResults[2 * Index + 1]);
end;

//----------------- TUTBSearch ---------------------------------------------------------------------

procedure TUTBMSearch.ClearPattern;
begin
  FreeMem(FPattern);
  FPattern := nil;
  FFlags := [];
  FPatternUsed := 0;
  FPatternSize := 0;
  FPatternLength := 0;
  FreeMem(FSkipValues);
  FSkipValues := nil;
  FSkipsUsed := 0;
  FMD4 := 0;
end;

function TUTBMSearch.GetSkipValue(TextStart, TextEnd: PUCS2): SizeInt;
// looks up the SkipValues value for a character
var
  I: SizeInt;
  C1,
  C2: UCS4;
  Sp: PUTBMSkip;
begin
  Result := 0;
  if TJclAddr(TextStart) < TJclAddr(TextEnd) then
  begin
    C1 := UCS4(TextStart^);
    if (TextStart + 1) < TextEnd then
      C2 := UCS4((TextStart + 1)^)
    else
      C2 := $FFFFFFFF;
    if (SurrogateHighStart <= C1) and (C1 <= SurrogateHighEnd) and
       (SurrogateLowStart <= C2) and (C2 <= $DDDD) then
      C1 := $10000 + (((C1 and $03FF) shl 10) or (C2 and $03FF));

    Sp := FSkipValues;
    for I := 0 to FSkipsUsed - 1 do
    begin
      if not (Boolean(C1 xor Sp.BMChar.UpCase) and
              Boolean(C1 xor Sp.BMChar.LoCase) and
              Boolean(C1 xor Sp.BMChar.TitleCase)) then
      begin
        if (TextEnd - TextStart) < Sp.SkipValues then
          Result := TextEnd - TextStart
        else
          Result := Sp.SkipValues;
        Exit;
      end;
      Inc(Sp);
    end;
    Result := FPatternLength;
  end;
end;

function TUTBMSearch.Match(Text, Start, Stop: PUCS2; var MatchStart, MatchEnd: SizeInt): Boolean;
// Checks once whether the text at position Start (which points to the end of the
// current text part to be matched) matches.
// Note: If whole words only are allowed then the left and right border tests are
//       done here too. The keypoint for the right border is that the next character
//       after the search string is either the text end or a space character.
//       For the left side this is similar, but there is nothing like a string
//       start marker (like the string end marker #0).
//
//       It seems not obvious, but we still can use the passed Text pointer to do
//       the left check. Although this pointer might not point to the real string
//       start (e.g. in TUTBMSearch.FindAll Text is incremented as needed) it is
//       still a valid check mark. The reason is that Text either points to the
//       real string start or a previous match (happend already, keep in mind the
//       search options do not change in the FindAll loop) and the character just
//       before Text is a space character.
//       This fact implies, though, that strings passed to Find (or FindFirst,
//       FindAll in TUTBMSearch) always really start at the given address. Although
//       this might not be the case in some circumstances (e.g. if you pass only
//       the selection from an editor) it is still assumed that a pattern matching
//       from the first position on (from the search string start) also matches
//       when whole words only are allowed.
var
  CheckSpace: Boolean;
  C1, C2: UCS4;
  Count: SizeInt;
  Cp: PUTBMChar;
begin
  // be pessimistic
  Result := False;

  // set the potential match endpoint first
  MatchEnd := (Start - Text) + 1;

  C1 := UCS4(Start^);
  if (Start + 1) < Stop then
    C2 := UCS4((Start + 1)^)
  else
    C2 := $FFFFFFFF;
  if (SurrogateHighStart <= C1) and (C1 <= SurrogateHighEnd) and
     (SurrogateLowStart <= C2) and (C2 <= SurrogateLowEnd) then
  begin
    C1 := $10000 + (((C1 and $03FF) shl 10) or (C2 and $03FF));
    // Adjust the match end point to occur after the UTF-16 character.
    Inc(MatchEnd);
  end;

  // check special cases
  if FPatternUsed = 1 then
  begin
    MatchStart := Start - Text;
    Result := True;
    Exit;
  end;

  // Early out if entire words need to be matched and the next character
  // in the search string is neither the string end nor a space character.
  if (sfWholeWordOnly in FFlags) and
     not ((Start + 1)^ = WideNull) and
     not UnicodeIsWhiteSpace(UCS4((Start + 1)^)) then
    Exit;

  // compare backward
  Cp := FPattern;
  Inc(Cp, FPatternUsed - 1);

  Count := FPatternLength;
  while (Start >= Text) and (Count > 0) do
  begin
    // ignore non-spacing characters if indicated
    if sfIgnoreNonSpacing in FFlags then
    begin
      while (Start > Text) and UnicodeIsNonSpacing(C1) do
      begin
        Dec(Start);
        C2 := UCS4(Start^);
        if (Start - 1) > Text then
          C1 := UCS4((Start - 1)^)
        else
          C1 := $FFFFFFFF;
        if (SurrogateLowStart <= C2) and (C2 <= SurrogateLowEnd) and
           (SurrogateHighStart <= C1) and (C1 <= SurrogateHighEnd) then
        begin
          C1 := $10000 + (((C1 and $03FF) shl 10) or (C2 and $03FF));
          Dec(Start);
        end
        else
          C1 := C2;
      end;
    end;

    // handle space compression if indicated
    if sfSpaceCompress in FFlags then
    begin
      CheckSpace := False;
      while (Start > Text) and (UnicodeIsWhiteSpace(C1) or UnicodeIsControl(C1)) do
      begin
        CheckSpace := UnicodeIsWhiteSpace(C1);
        Dec(Start);
        C2 := UCS4(Start^);
        if (Start - 1) > Text then
          C1 := UCS4((Start - 1)^)
        else
          C1 := $FFFFFFFF;
        if (SurrogateLowStart <= C2) and (C2 <= SurrogateLowEnd) and
           (SurrogateHighStart <= C1) and (C1 <= SurrogateHighEnd) then
        begin
          C1 := $10000 + (((C1 and $03FF) shl 10) or (C2 and $03FF));
          Dec(Start);
        end
        else
          C1 := C2;
      end;
      // Handle things if space compression was indicated and one or
      // more member characters were found.
      if CheckSpace then
      begin
        if Cp.UpCase <> $20 then
          Exit;
        Dec(Cp);
        Dec(Count);
        // If Count is 0 at this place then the space character(s) was the first
        // in the pattern and we need to correct the start position.
        if Count = 0 then
          Inc(Start);
      end;
    end;

    // handle the normal comparison cases
    if (Count > 0) and
       (Boolean(C1 xor Cp.UpCase) and
        Boolean(C1 xor Cp.LoCase) and
        Boolean(C1 xor Cp.TitleCase)) then
      Exit;

    if C1 >= $10000 then
      Dec(Count, 2)
    else
      Dec(Count, 1);
    if Count > 0 then
    begin
      Dec(Cp);
      // get the next preceding character
      if Start > Text then
      begin
        Dec(Start);
        C2 := UCS4(Start^);
        if (Start - 1) > Text then
          C1 := UCS4((Start - 1)^)
        else
          C1 := $FFFFFFFF;
        if (SurrogateLowStart <= C2) and (C2 <= SurrogateLowEnd) and
           (SurrogateHighStart <= C1) and (C1 <= SurrogateHighEnd) then
        begin
          C1 := $10000 + (((C1 and $03FF) shl 10) or (C2 and $03FF));
          Dec(Start);
        end
        else
          C1 := C2;
      end;
    end;
  end;

  // So far the string matched. Now check its left border for a space character
  // if whole word only are allowed.
  if not (sfWholeWordOnly in FFlags) or
     (Start <= Text) or
     UnicodeIsWhiteSpace(UCS4((Start - 1)^)) then
  begin
    // set the match start position
    MatchStart := Start - Text;
    Result := True;
  end;
end;

procedure TUTBMSearch.Compile(Pattern: PUCS2; PatternLength: SizeInt; Flags: TSearchFlags);
var
  HaveSpace: Boolean;
  I, J, K,
  SLen: SizeInt;
  Cp: PUTBMChar;
  Sp: PUTBMSkip;
  C1, C2,
  Sentinel: UCS4;
begin
  if (Pattern <> nil) and (Pattern^ <> #0) and (PatternLength > 0) then
  begin
    // do some initialization
    FFlags := Flags;
    // extra skip flag
    FMD4 := 1;

    Sentinel := 0;

    // allocate more storage if necessary
    FPattern := AllocMem(SizeOf(TUTBMChar) * PatternLength);
    FSkipValues := AllocMem(SizeOf(TUTBMSkip) * PatternLength);
    FPatternSize := PatternLength;

    // Preprocess the pattern to remove controls (if specified) and determine case.
    Cp := FPattern;
    I := 0;
    HaveSpace := False;
    while I < PatternLength do
    begin
      C1 := UCS4(Pattern[I]);
      if (I + 1) < PatternLength then
        C2 := UCS4(Pattern[I + 1])
      else
        C2 := $FFFFFFFF;
      if (SurrogateHighStart <= C1) and (C1 <= SurrogateHighEnd) and
         (SurrogateLowStart <= C2) and (C2 <= SurrogateLowEnd) then
        C1 := $10000 + (((C1 and $03FF) shl 10) or (C2 and $03FF));

      // Make sure the HaveSpace flag is turned off if the character is not an
      // appropriate one.
      if not UnicodeIsWhiteSpace(C1) then
        HaveSpace := False;

      // If non-spacing characters should be ignored, do it here.
      if (sfIgnoreNonSpacing in Flags) and UnicodeIsNonSpacing(C1) then
      begin
        Inc(I);
        Continue;
      end;

      // check if spaces and controls need to be compressed
      if sfSpaceCompress in Flags then
      begin
        if UnicodeIsWhiteSpace(C1) then
        begin
          if not HaveSpace then
          begin
            // Add a space and set the flag.
            Cp.UpCase := $20;
            Cp.LoCase := $20;
            Cp.TitleCase := $20;
            Inc(Cp);

            // increase the real pattern length
            Inc(FPatternLength);
            Sentinel := $20;
            HaveSpace := True;
          end;
          Inc(I);
          Continue;
        end;

        // ignore all control characters
        if UnicodeIsControl(C1) then
        begin
          Inc(I);
          Continue;
        end;
      end;

      // add the character
      if not (sfCaseSensitive in Flags) then
      begin
        { TODO : use the entire mapping, not only the first character }
        Cp.UpCase := UnicodeToUpper(C1)[0];
        Cp.LoCase := UnicodeToLower(C1)[0];
        Cp.TitleCase := UnicodeToTitle(C1)[0];
      end
      else
      begin
        Cp.UpCase := C1;
        Cp.LoCase := C1;
        Cp.TitleCase := C1;
      end;

      Sentinel := Cp.UpCase;

      // move to the next character
      Inc(Cp);

      // increase the real pattern length appropriately
      if C1 >= $10000 then
        Inc(FPatternLength, 2)
      else
        Inc(FPatternLength);

      // increment the loop index for UTF-16 characters
      if C1 > $10000 then
        Inc(I, 2)
      else
        Inc(I);
    end;

    // set the number of characters actually used
    FPatternUsed := (TJclAddr(Cp) - TJclAddr(FPattern)) div SizeOf(TUTBMChar);

    // Go through and construct the skip array and determine the actual length
    // of the pattern in UCS2 terms.
    SLen := FPatternLength - 1;
    Cp := FPattern;
    K := 0;
    for I := 0 to FPatternUsed - 1 do
    begin
      // locate the character in the FSkipValues array
      Sp := FSkipValues;
      J := 0;
      while (J < FSkipsUsed) and (Sp.BMChar.UpCase <> Cp.UpCase) do
      begin
        Inc(J);
        Inc(Sp);
      end;

      // If the character is not found, set the new FSkipValues element and
      // increase the number of FSkipValues elements.
      if J = FSkipsUsed then
      begin
        Sp.BMChar := Cp;
        Inc(FSkipsUsed);
      end;

      // Set the updated FSkipValues value.  If the character is UTF-16 and is
      // not the last one in the pattern, add one to its FSkipValues value.
      Sp.SkipValues := SLen - K;
      if (Cp.UpCase >= $10000) and ((K + 2) < SLen) then
        Inc(Sp.SkipValues);

      // set the new extra FSkipValues for the sentinel character
      if ((Cp.UpCase >= $10000) and
          ((K + 2) <= SLen) or ((K + 1) <= SLen) and
          (Cp.UpCase = Sentinel)) then
        FMD4 := SLen - K;

      // increase the actual index
      if Cp.UpCase >= $10000 then
        Inc(K, 2)
      else
        Inc(K);
      Inc(Cp);
    end;
  end;
end;

function TUTBMSearch.Find(Text: PUCS2; TextLen: SizeInt; var MatchStart, MatchEnd: SizeInt): Boolean;
// this is the main matching routine using a tuned Boyer-Moore algorithm
var
  K: SizeInt;
  Start,
  Stop: PUCS2;
begin
  Result := False;
  if (FPattern <> nil) and (FPatternUsed > 0) and (Text <> nil) and
     (TextLen > 0) and (TextLen >= FPatternLength) then
  begin
    Start := Text + FPatternLength - 1;
    Stop := Text + TextLen;

    // adjust the start point if it points to a low surrogate
    if (SurrogateLowStart <= UCS4(Start^)) and
       (UCS4(Start^) <= SurrogateLowEnd) and
       (SurrogateHighStart <= UCS4((Start - 1)^)) and
       (UCS4((Start - 1)^) <= SurrogateHighEnd) then
      Dec(Start);

    while Start < Stop do
    begin
      repeat
        K := GetSkipValue(Start, Stop);
        if K = 0 then
          Break;
        Inc(Start, K);
        if (Start < Stop) and
           (SurrogateLowStart <= UCS4(Start^)) and
           (UCS4(Start^) <= SurrogateLowEnd) and
           (SurrogateHighStart <= UCS4((Start - 1)^)) and
           (UCS4((Start - 1)^) <= SurrogateHighEnd) then
          Dec(Start);
      until False;

      if (Start < Stop) and Match(Text, Start, Stop, MatchStart, MatchEnd) then
      begin
        Result := True;
        Break;
      end;
      Inc(Start, FMD4);
      if (Start < Stop) and
         (SurrogateLowStart <= UCS4(Start^)) and
         (UCS4(Start^) <= SurrogateLowEnd) and
         (SurrogateHighStart <= UCS4((Start - 1)^)) and
         (UCS4((Start - 1)^) <= SurrogateHighEnd) then
        Dec(Start);
    end;
  end;
end;

procedure TUTBMSearch.Clear;
begin
  ClearPattern;
  inherited Clear;
end;

function TUTBMSearch.FindAll(const Text: WideString): Boolean;
begin
  Result := FindAll(PWideChar(Text), Length(Text));
end;

function TUTBMSearch.FindAll(Text: PWideChar; TextLen: SizeInt): Boolean;
// Looks for all occurences of the pattern passed to FindPrepare and creates an
// internal list of their positions.
var
  Start, Stop: SizeInt;
  Run: PWideChar;
  RunLen: SizeInt;
begin
  ClearResults;
  Run := Text;
  RunLen := TextLen;
  Start := 0;
  Stop := 0;
  // repeat to find all occurences of the pattern
  while Find(Run, RunLen, Start, Stop) do
  begin
    // store this result (consider text pointer movement)...
    AddResult(Start + (Run - Text), Stop + (Run - Text));
    // ... and advance text position and length
    Inc(Run, Stop);
    Dec(RunLen, Stop);
  end;
  Result := Count > 0;
end;

function TUTBMSearch.FindFirst(const Text: WideString; var Start, Stop: SizeInt): Boolean;
// Looks for the first occurence of the pattern passed to FindPrepare in Text and
// returns True if one could be found (in which case Start and Stop are set to
// the according indices) otherwise False. This function is in particular of
// interest if only one occurence needs to be found.
begin
  ClearResults;
  Result := Find(PWideChar(Text), Length(Text), Start, Stop);
  if Result then
    AddResult(Start, Stop);
end;

function TUTBMSearch.FindFirst(Text: PWideChar; TextLen: SizeInt; var Start, Stop: SizeInt): Boolean;
// Same as the WideString version of this method.
begin
  ClearResults;
  Result := Find(Text, TextLen, Start, Stop);
  if Result then
    AddResult(Start, Stop);
end;

procedure TUTBMSearch.FindPrepare(const Pattern: WideString; Options: TSearchFlags);
begin
  FindPrepare(PWideChar(Pattern), Length(Pattern), Options);
end;

procedure TUTBMSearch.FindPrepare(Pattern: PWideChar; PatternLength: SizeInt; Options: TSearchFlags);
// prepares following search by compiling the given pattern into an internal structure
begin
  Compile(Pattern, PatternLength, Options);
end;

//----------------- Unicode RE search core ---------------------------------------------------------

const
  // error codes
  _URE_OK = 0;
  _URE_UNEXPECTED_EOS = -1;
  _URE_CCLASS_OPEN = -2;
  _URE_UNBALANCED_GROUP = -3;
  _URE_INVALID_PROPERTY = -4;
  _URE_INVALID_RANGE = -5;
  _URE_RANGE_OPEN = -6;

  // options that can be combined for searching
  URE_IGNORE_NONSPACING = $01;
  URE_DONT_MATCHES_SEPARATORS = $02;

const
  // Flags used internally in the DFA
  _URE_DFA_CASEFOLD = $01;
  _URE_DFA_BLANKLINE = $02;

  // symbol types for the DFA
  _URE_ANY_CHAR = 1;
  _URE_CHAR = 2;
  _URE_CCLASS = 3;
  _URE_NCCLASS = 4;
  _URE_BOL_ANCHOR = 5;
  _URE_EOL_ANCHOR = 6;

  // op codes for converting the NFA to a DFA
  _URE_SYMBOL = 10;
  _URE_PAREN = 11;
  _URE_QUEST = 12;
  _URE_STAR = 13;
  _URE_PLUS = 14;
  _URE_ONE = 15;
  _URE_AND = 16;
  _URE_OR = 17;

  _URE_NOOP = $FFFF;

//----------------- TURESearch ---------------------------------------------------------------------

procedure TURESearch.Clear;
begin
  inherited Clear;
  ClearUREBuffer;
  ClearDFA;
end;

procedure TURESearch.Push(V: SizeInt);
begin
  with FUREBuffer do
  begin
    // If the 'Reducing' parameter is True, check to see if the value passed is
    // already on the stack.
    if Reducing and ExpressionList.Expressions[Word(V)].OnStack then
      Exit;

    if Stack.ListUsed = Length(Stack.List) then
      SetLength(Stack.List, Length(Stack.List) + 8);
    Stack.List[Stack.ListUsed] := V;
    Inc(Stack.ListUsed);

    // If the 'reducing' parameter is True, flag the element as being on the Stack.
    if Reducing then
      ExpressionList.Expressions[Word(V)].OnStack := True;
  end;
end;

function TURESearch.Peek: SizeInt;
begin
  if FUREBuffer.Stack.ListUsed = 0 then
    Result := _URE_NOOP
  else
    Result := FUREBuffer.Stack.List[FUREBuffer.Stack.ListUsed - 1];
end;

function TURESearch.Pop: SizeInt;
begin
  if FUREBuffer.Stack.ListUsed = 0 then
    Result := _URE_NOOP
  else
  begin
    Dec(FUREBuffer.Stack.ListUsed);
    Result := FUREBuffer.Stack.List[FUREBuffer.Stack.ListUsed];
    if FUREBuffer.Reducing then
      FUREBuffer.ExpressionList.Expressions[Word(Result)].OnStack := False;
  end;
end;

function TURESearch.ParsePropertyList(Properties: PUCS2; Limit: SizeInt;
  var Categories: TCharacterCategories): SizeInt;
// Parse a comma-separated list of integers that represent character properties.
// Combine them into a set of categories and return the number of characters consumed.
var
  N: SizeInt;
  Run,
  ListEnd: PUCS2;
begin
  Run := Properties;
  ListEnd := Run + Limit;

  N := 0;
  Categories := [];
  while (FUREBuffer.Error = _URE_OK) and (Run < ListEnd) do
  begin
    if Run^ = ',' then
    begin
      // Encountered a comma, so take the number parsed so far as category and
      // reset the number.
      Include(Categories, TCharacterCategory(N));
      N := 0;
    end
    else
    begin
      if (Run^ >= '0') and (Run^ <= '9') then
      begin
        // Encountered a digit, so start or continue building the SizeInt that
        // represents the character category.
        N := (N * 10) + SizeInt(Word(Run^) - Ord('0'));
      end
      else
      begin
        // Encountered something that is not part of the property list.
        // Indicate that we are done.
        Break;
      end;
    end;

    // If the number is to large then there is a problem.
    // Most likely a missing comma separator.
    if SizeInt(N) > Ord(High(TCharacterCategory)) then
      FUREBuffer.Error := _URE_INVALID_PROPERTY;
    Inc(Run);
  end;

  // Return the number of characters consumed.
  Result := Run - Properties;
end;

function TURESearch.MakeHexNumber(NP: PUCS2; Limit: SizeInt; var Number: UCS4): SizeInt;
// Collect a hex number with 1 to 4 digits and return the number of characters used.
var
  I: SizeInt;
  Run,
  ListEnd: PUCS2;
begin
  Run := np;
  ListEnd := Run + Limit;

  Number := 0;
  I := 0;
  while (I < 4) and (Run < ListEnd) do
  begin
    if (Run^ >= '0') and (Run^ <= '9') then
      Number := (Number shl 4) or (Cardinal(Word(Run^) - Ord('0')))
    else
    begin
      if (Run^ >= 'A') and (Run^ <= 'F') then
        Number := (Number shl 4) or (Cardinal(Word(Run^) - Ord('A')) + 10)
      else
      begin
        if (Run^ >= 'a') and (Run^ <= 'f') then
          Number := (Number shl 4) or (Cardinal(Word(Run^) - Ord('a')) + 10)
        else
          Break;
      end;
    end;
    Inc(I);
    Inc(Run);
  end;

  Result := Run - NP;
end;

procedure TURESearch.AddRange(var CCL: TUcCClass; Range: TUcRange);
// Insert a Range into a character class, removing duplicates and ordering them
// in increasing Range-start order.
var
  I: SizeInt;
  Temp: UCS4;
begin
  // If the `Casefold' flag is set, then make sure both endpoints of the Range
  // are converted to lower.
  if (FUREBuffer.Flags and _URE_DFA_CASEFOLD) <> 0 then
  begin
    { TODO : use the entire mapping, not only the first character }
    Range.MinCode := UnicodeToLower(Range.MinCode)[0];
    Range.MaxCode := UnicodeToLower(Range.MaxCode)[0];
  end;

  // Swap the Range endpoints if they are not in increasing order.
  if Range.MinCode > Range.MaxCode then
  begin
    Temp := Range.MinCode;
    Range.MinCode := Range.MaxCode;
    Range.MaxCode := Temp;
  end;

  I := 0;
  while (I < CCL.RangesUsed) and (Range.MinCode < CCL.Ranges[I].MinCode) do
    Inc(I);

  // check for a duplicate
  if (I < CCL.RangesUsed) and (Range.MinCode = CCL.Ranges[I].MinCode) and
    (Range.MaxCode = CCL.Ranges[I].MaxCode) then
    Exit;

  if CCL.RangesUsed = Length(CCL.Ranges) then
    SetLength(CCL.Ranges, Length(CCL.Ranges) + 8);

  if I < CCL.RangesUsed then
    Move(CCL.Ranges[I], CCL.Ranges[I + 1], SizeOf(TUcRange) * (CCL.RangesUsed - I));

  CCL.Ranges[I].MinCode := Range.MinCode;
  CCL.Ranges[I].MaxCode := Range.MaxCode;
  Inc(CCL.RangesUsed);
end;

type
  PTrie = ^TTrie;
  TTrie = record
    Key: UCS2;
    Len,
    Next: SizeInt;
    Setup: SizeInt;
    Categories: TCharacterCategories;
  end;

procedure TURESearch.SpaceSetup(Symbol: PUcSymbolTableEntry; Categories: TCharacterCategories);
var
  Range: TUcRange;
begin
  Symbol.Categories := Symbol.Categories + Categories;

  Range.MinCode := UCS4(WideTabulator);
  Range.MaxCode := UCS4(WideTabulator);
  AddRange(Symbol.Symbol.CCL, Range);
  Range.MinCode := UCS4(WideCarriageReturn);
  Range.MaxCode := UCS4(WideCarriageReturn);
  AddRange(Symbol.Symbol.CCL, Range);
  Range.MinCode := UCS4(WideLineFeed);
  Range.MaxCode := UCS4(WideLineFeed);
  AddRange(Symbol.Symbol.CCL, Range);
  Range.MinCode := UCS4(WideFormFeed);
  Range.MaxCode := UCS4(WideFormFeed);
  AddRange(Symbol.Symbol.CCL, Range);
  Range.MinCode := $FEFF;
  Range.MaxCode := $FEFF;
  AddRange(Symbol.Symbol.CCL, Range);
end;

procedure TURESearch.HexDigitSetup(Symbol: PUcSymbolTableEntry);
var
  Range: TUcRange;
begin
  Range.MinCode := UCS4('0');
  Range.MaxCode := UCS4('9');
  AddRange(Symbol.Symbol.CCL, Range);
  Range.MinCode := UCS4('A');
  Range.MaxCode := UCS4('F');
  AddRange(Symbol.Symbol.CCL, Range);
  Range.MinCode := UCS4('a');
  Range.MaxCode := UCS4('f');
  AddRange(Symbol.Symbol.CCL, Range);
end;

const
  CClassTrie: array [0..64] of TTrie = (
    (Key: #$003A; Len: 1; Next:  1; Setup: 0; Categories: []),
    (Key: #$0061; Len: 9; Next: 10; Setup: 0; Categories: []),
    (Key: #$0063; Len: 8; Next: 19; Setup: 0; Categories: []),
    (Key: #$0064; Len: 7; Next: 24; Setup: 0; Categories: []),
    (Key: #$0067; Len: 6; Next: 29; Setup: 0; Categories: []),
    (Key: #$006C; Len: 5; Next: 34; Setup: 0; Categories: []),
    (Key: #$0070; Len: 4; Next: 39; Setup: 0; Categories: []),
    (Key: #$0073; Len: 3; Next: 49; Setup: 0; Categories: []),
    (Key: #$0075; Len: 2; Next: 54; Setup: 0; Categories: []),
    (Key: #$0078; Len: 1; Next: 59; Setup: 0; Categories: []),
    (Key: #$006C; Len: 1; Next: 11; Setup: 0; Categories: []),
    (Key: #$006E; Len: 2; Next: 13; Setup: 0; Categories: []),
    (Key: #$0070; Len: 1; Next: 16; Setup: 0; Categories: []),
    (Key: #$0075; Len: 1; Next: 14; Setup: 0; Categories: []),
    (Key: #$006D; Len: 1; Next: 15; Setup: 0; Categories: []),
    (Key: #$003A; Len: 1; Next: 16; Setup: 1; Categories: ClassLetter + ClassNumber),
    (Key: #$0068; Len: 1; Next: 17; Setup: 0; Categories: []),
    (Key: #$0061; Len: 1; Next: 18; Setup: 0; Categories: []),
    (Key: #$003A; Len: 1; Next: 19; Setup: 1; Categories: ClassLetter),
    (Key: #$006E; Len: 1; Next: 20; Setup: 0; Categories: []),
    (Key: #$0074; Len: 1; Next: 21; Setup: 0; Categories: []),
    (Key: #$0072; Len: 1; Next: 22; Setup: 0; Categories: []),
    (Key: #$006C; Len: 1; Next: 23; Setup: 0; Categories: []),
    (Key: #$003A; Len: 1; Next: 24; Setup: 1; Categories: [ccOtherControl, ccOtherFormat]),
    (Key: #$0069; Len: 1; Next: 25; Setup: 0; Categories: []),
    (Key: #$0067; Len: 1; Next: 26; Setup: 0; Categories: []),
    (Key: #$0069; Len: 1; Next: 27; Setup: 0; Categories: []),
    (Key: #$0074; Len: 1; Next: 28; Setup: 0; Categories: []),
    (Key: #$003A; Len: 1; Next: 29; Setup: 1; Categories: ClassNumber),
    (Key: #$0072; Len: 1; Next: 30; Setup: 0; Categories: []),
    (Key: #$0061; Len: 1; Next: 31; Setup: 0; Categories: []),
    (Key: #$0070; Len: 1; Next: 32; Setup: 0; Categories: []),
    (Key: #$0068; Len: 1; Next: 33; Setup: 0; Categories: []),
    (Key: #$003A; Len: 1; Next: 34; Setup: 1; Categories: ClassMark + ClassNumber + ClassLetter + ClassPunctuation +
      ClassSymbol),
    (Key: #$006F; Len: 1; Next: 35; Setup: 0; Categories: []),
    (Key: #$0077; Len: 1; Next: 36; Setup: 0; Categories: []),
    (Key: #$0065; Len: 1; Next: 37; Setup: 0; Categories: []),
    (Key: #$0072; Len: 1; Next: 38; Setup: 0; Categories: []),
    (Key: #$003A; Len: 1; Next: 39; Setup: 1; Categories: [ccLetterLowercase]),
    (Key: #$0072; Len: 2; Next: 41; Setup: 0; Categories: []),
    (Key: #$0075; Len: 1; Next: 45; Setup: 0; Categories: []),
    (Key: #$0069; Len: 1; Next: 42; Setup: 0; Categories: []),
    (Key: #$006E; Len: 1; Next: 43; Setup: 0; Categories: []),
    (Key: #$0074; Len: 1; Next: 44; Setup: 0; Categories: []),
    (Key: #$003A; Len: 1; Next: 45; Setup: 1; Categories: ClassMark + ClassNumber + ClassLetter + ClassPunctuation +
      ClassSymbol + [ccSeparatorSpace]),
    (Key: #$006E; Len: 1; Next: 46; Setup: 0; Categories: []),
    (Key: #$0063; Len: 1; Next: 47; Setup: 0; Categories: []),
    (Key: #$0074; Len: 1; Next: 48; Setup: 0; Categories: []),
    (Key: #$003A; Len: 1; Next: 49; Setup: 1; Categories: ClassPunctuation),
    (Key: #$0070; Len: 1; Next: 50; Setup: 0; Categories: []),
    (Key: #$0061; Len: 1; Next: 51; Setup: 0; Categories: []),
    (Key: #$0063; Len: 1; Next: 52; Setup: 0; Categories: []),
    (Key: #$0065; Len: 1; Next: 53; Setup: 0; Categories: []),
    (Key: #$003A; Len: 1; Next: 54; Setup: 2; Categories: ClassSpace),
    (Key: #$0070; Len: 1; Next: 55; Setup: 0; Categories: []),
    (Key: #$0070; Len: 1; Next: 56; Setup: 0; Categories: []),
    (Key: #$0065; Len: 1; Next: 57; Setup: 0; Categories: []),
    (Key: #$0072; Len: 1; Next: 58; Setup: 0; Categories: []),
    (Key: #$003A; Len: 1; Next: 59; Setup: 1; Categories: [ccLetterUppercase]),
    (Key: #$0064; Len: 1; Next: 60; Setup: 0; Categories: []),
    (Key: #$0069; Len: 1; Next: 61; Setup: 0; Categories: []),
    (Key: #$0067; Len: 1; Next: 62; Setup: 0; Categories: []),
    (Key: #$0069; Len: 1; Next: 63; Setup: 0; Categories: []),
    (Key: #$0074; Len: 1; Next: 64; Setup: 0; Categories: []),
    (Key: #$003A; Len: 1; Next: 65; Setup: 3; Categories: [])
  );

function TURESearch.PosixCCL(CP: PUCS2; Limit: SizeInt; Symbol: PUcSymbolTableEntry): SizeInt;
// Probe for one of the POSIX colon delimited character classes in the static trie.
var
  I: SizeInt;
  N: SizeInt;
  TP: PTrie;
  Run,
  ListEnd: PUCS2;
begin
  Result := 0;
  // If the number of characters left is less than 7,
  // then this cannot be interpreted as one of the colon delimited classes.
  if Limit >= 7 then
  begin
    Run := cp;
    ListEnd := Run + Limit;
    TP := @CClassTrie[0];
    I := 0;
    while (Run < ListEnd) and (I < 8) do
    begin
      N := TP.Len;
      while (N > 0) and (TP.Key <> Run^) do
      begin
        Inc(TP);
        Dec(N);
      end;

      if N = 0 then
      begin
        Result := 0;
        Exit;
      end;

      if (Run^ = ':') and ((I = 6) or (I = 7)) then
      begin
        Inc(Run);
        Break;
      end;
      if (Run + 1) < ListEnd then
        TP := @CClassTrie[TP.Next];
      Inc(I);
      Inc(Run);
    end;

    Result := Run - CP;
    case TP.Setup of
      1:
        Symbol.Categories := Symbol.Categories + TP.Categories;
      2:
        SpaceSetup(Symbol, TP.Categories);
      3:
        HexDigitSetup(Symbol);
    else
      Result := 0;
    end;
  end;
end;

function TURESearch.BuildCharacterClass(CP: PUCS2; Limit: SizeInt; Symbol: PUcSymbolTableEntry): SizeInt;
// Construct a list of ranges and return the number of characters consumed.
var
  RangeEnd: SizeInt;
  N: SizeInt;
  Run,
  ListEnd: PUCS2;
  C, Last: UCS4;
  Range: TUcRange;
begin
  Run := cp;
  ListEnd := Run + Limit;

  if Run^ = '^' then
  begin
    Symbol.AType := _URE_NCCLASS;
    Inc(Run);
  end
  else
    Symbol.AType := _URE_CCLASS;

  Last := 0;
  RangeEnd := 0;
  while (FUREBuffer.Error = _URE_OK) and (Run < ListEnd) do
  begin
    // Allow for the special case []abc], where the first closing bracket would end an empty
    // character class, which makes no sense. Hence this bracket is treaded literally.
    if (Run^ = ']') and (Symbol.Symbol.CCL.RangesUsed > 0) then
      Break;

    C := UCS4(Run^);
    Inc(Run);

    // escape character
    if C = Ord('\') then
    begin
      if Run = ListEnd then
      begin
        // The EOS was encountered when expecting the reverse solidus to be followed by the character it is escaping.
        // Set an Error code and return the number of characters consumed up to this point.
        FUREBuffer.Error := _URE_UNEXPECTED_EOS;
        Result := Run - CP;
        Exit;
      end;

      C := UCS4(Run^);
      Inc(Run);
      case UCS2(C) of
        'a':
          C := $07;
        'b':
          C := $08;
        'f':
          C := $0C;
        'n':
          C := $0A;
        'R':
          C := $0D;
        't':
          C := $09;
        'v':
          C := $0B;
        'p', 'P':
          begin
            Inc(Run, ParsePropertyList(Run, ListEnd - Run, Symbol.Categories));
            // Invert the bit mask of the properties if this is a negated character class or if 'P' is used to specify
            // a list of character properties that should *not* match in a character class.
            if C = Ord('P') then
              Symbol.Categories := ClassAll - Symbol.Categories;
            Continue;
          end;
        'x', 'X', 'u', 'U':
          begin
            if (Run < ListEnd) and
               ((Run^ >= '0') and (Run^ <= '9') or
                (Run^ >= 'A') and (Run^ <= 'F') or
                (Run^ >= 'a') and (Run^ <= 'f')) then
              Inc(Run, MakeHexNumber(Run, ListEnd - Run, C));
          end;
      end;
    end
    else
    begin
      if C = Ord(':') then
      begin
        // Probe for a POSIX colon delimited character class.
        Dec(Run);
        N := PosixCCL(Run, ListEnd - Run, Symbol);
        if N = 0 then
          Inc(Run)
        else
        begin
          Inc(Run, N);
          Continue;
        end;
      end;
    end;

    // Check to see if the current character is a low surrogate that needs
    // to be combined with a preceding high surrogate.
    if Last <> 0 then
    begin
      if (C >= SurrogateLowStart) and (C <= SurrogateLowEnd) then
      begin
        // Construct the UTF16 character code.
        C := $10000 + (((Last and $03FF) shl 10) or (C and $03FF))
      end
      else
      begin
        // Add the isolated high surrogate to the range.
        if RangeEnd = 1 then
          Range.MaxCode := Last and $FFFF
        else
        begin
          Range.MinCode := Last and $FFFF;
          Range.MaxCode := Last and $FFFF;
        end;

        AddRange(Symbol.Symbol.CCL, Range);
        RangeEnd := 0;
      end;
    end;

    // Clear the Last character code.
    Last := 0;

    // This slightly awkward code handles the different cases needed to construct a range.
    if (C >= SurrogateHighStart) and (C <= SurrogateHighEnd) then
    begin
      // If the high surrogate is followed by a Range indicator, simply add it as the Range start.  Otherwise,
      // save it in  the next character is a low surrogate.
      if Run^ = '-' then
      begin
        Inc(Run);
        Range.MinCode := C;
        RangeEnd := 1;
      end
      else
        Last := C;
    end
    else
    begin
      if RangeEnd = 1 then
      begin
        Range.MaxCode := C;
        AddRange(Symbol.Symbol.CCL, Range);
        RangeEnd := 0;
      end
      else
      begin
        Range.MinCode := C;
        Range.MaxCode := C;
        if Run^ = '-' then
        begin
          Inc(Run);
          RangeEnd := 1;
        end
        else
          AddRange(Symbol.Symbol.CCL, Range);
      end;
    end;
  end;

  if (Run < ListEnd) and (Run^ = ']') then
    Inc(Run)
  else
  begin
    // The parse was not terminated by the character class close symbol (']'), so set an error code.
    FUREBuffer.Error := _URE_CCLASS_OPEN;
  end;
  Result := Run - CP;
end;

function TURESearch.ProbeLowSurrogate(LeftState: PUCS2; Limit: SizeInt; var Code: UCS4): SizeInt;
// probes for a low surrogate hex code
var
  I: SizeInt;
  Run,
  ListEnd: PUCS2;
begin
  I := 0;
  Code := 0;
  Run := LeftState;
  ListEnd := Run + Limit;

  while (I < 4) and (Run < ListEnd) do
  begin
    if (Run^ >= '0') and (Run^ <= '9') then
      Code := (Code shl 4) or (Cardinal(Word(Run^) - Ord('0')))
    else
    begin
      if (Run^ >= 'A') and (Run^ <= 'F') then
        Code := (Code shl 4) or (Cardinal(Word(Run^) - Ord('A')) + 10)
      else
      begin
        if (Run^ >= 'a') and (Run^ <= 'f') then
          Code := (Code shl 4) or (Cardinal(Word(Run^) - Ord('a')) + 10)
        else
          Break;
      end;
    end;
    Inc(Run);
  end;

  if (SurrogateLowStart <= Code) and (Code <= SurrogateLowEnd) then
    Result :=  Run - LeftState
  else
    Result := 0;
end;

function TURESearch.CompileSymbol(S: PUCS2; Limit: SizeInt; Symbol: PUcSymbolTableEntry): SizeInt;
var
  C: UCS4;
  Run,
  ListEnd: PUCS2;
begin
  Run := S;
  ListEnd := S + Limit;

  C := UCS4(Run^);
  Inc(Run);
  if C = Ord('\') then
  begin
    if Run = ListEnd then
    begin
      // The EOS was encountered when expecting the reverse solidus to be followed
      // by the character it is escaping. Set an Error code and return the number
      // of characters consumed up to this point.
      FUREBuffer.Error := _URE_UNEXPECTED_EOS;
      Result := Run - S;
      Exit;
    end;

    C := UCS4(Run^);
    Inc(Run);
    case UCS2(C) of
      'p', 'P':
        begin
          if UCS2(C) = 'p' then
            Symbol.AType :=_URE_CCLASS
          else
            Symbol.AType :=_URE_NCCLASS;
          Inc(Run, ParsePropertyList(Run, ListEnd - Run, Symbol.Categories));
        end;
      'a':
        begin
          Symbol.AType := _URE_CHAR;
          Symbol.Symbol.Chr := $07;
        end;
      'b':
        begin
          Symbol.AType := _URE_CHAR;
          Symbol.Symbol.Chr := $08;
        end;
      'f':
        begin
          Symbol.AType := _URE_CHAR;
          Symbol.Symbol.Chr := $0C;
        end;
      'n':
        begin
          Symbol.AType := _URE_CHAR;
          Symbol.Symbol.Chr := $0A;
        end;
      'r':
        begin
          Symbol.AType := _URE_CHAR;
          Symbol.Symbol.Chr := $0D;
        end;
      't':
        begin
          Symbol.AType := _URE_CHAR;
          Symbol.Symbol.Chr := $09;
        end;
      'v':
        begin
          Symbol.AType := _URE_CHAR;
          Symbol.Symbol.Chr := $0B;
        end;
    else
      case UCS2(C) of
        'x', 'X', 'u', 'U':
          begin
            // Collect between 1 and 4 digits representing an UCS2 code.
            if (Run < ListEnd) and
              ((Run^ >= '0') and (Run^ <= '9') or
               (Run^ >= 'A') and (Run^ <= 'F') or
               (Run^ >= 'a') and (Run^ <= 'f')) then
              Inc(Run, MakeHexNumber(Run, ListEnd - Run, C));
          end;
      end;

      // Simply add an escaped character here.
      Symbol.AType := _URE_CHAR;
      Symbol.Symbol.Chr := C;
    end;
  end
  else
  begin
    if (UCS2(C) = '^') or (UCS2(C) = '$') then
    begin
      // Handle the BOL and EOL anchors. This actually consists simply of setting
      // a flag that indicates that the user supplied anchor match function should
      // be called. This needs to be done instead of simply matching line/paragraph
      // separators because beginning-of-text and end-of-text tests are needed as well.
      if UCS2(C) = '^' then
        Symbol.AType := _URE_BOL_ANCHOR
      else
        Symbol.AType := _URE_EOL_ANCHOR;
    end
    else
    begin
      if UCS2(C) = '[' then
      begin
        // construct a character class
        Inc(Run, BuildCharacterClass(Run, ListEnd - Run, Symbol));
      end
      else
      begin
        if UCS2(C) = '.' then
          Symbol.AType := _URE_ANY_CHAR
        else
        begin
          Symbol.AType := _URE_CHAR;
          Symbol.Symbol.Chr := C;
        end;
      end;
    end;
  end;

  // If the symbol type happens to be a character and is a high surrogate, then
  // probe forward to see if it is followed by a low surrogate that needs to be added.
  if (Run < ListEnd) and
     (Symbol.AType = _URE_CHAR) and
     (SurrogateHighStart <= Symbol.Symbol.Chr) and
     (Symbol.Symbol.Chr <= SurrogateHighEnd) then
  begin
    if (SurrogateLowStart <= UCS4(Run^)) and
       (UCS4(Run^) <= SurrogateLowEnd) then
    begin
      Symbol.Symbol.Chr := $10000 + (((Symbol.Symbol.Chr and $03FF) shl 10) or (UCS4(Run^) and $03FF));
      Inc(Run);
    end
    else
    begin
      if (Run^ = '\') and (((Run + 1)^ = 'x') or ((Run + 1)^ = 'X') or
         ((Run + 1)^ = 'u') or ((Run + 1)^ = 'U')) then
      begin
        Inc(Run, ProbeLowSurrogate(Run + 2, ListEnd - (Run + 2), C));
        if (SurrogateLowStart <= C) and (C <= SurrogateLowEnd) then
        begin
          // Take into account the \[xu] in front of the hex code.
          Inc(Run, 2);
          Symbol.Symbol.Chr := $10000 + (((Symbol.Symbol.Chr and $03FF) shl 10) or (C and $03FF));
        end;
      end;
    end;
  end;

  // Last, make sure any _URE_CHAR type symbols are changed to lower if the
  // 'Casefold' flag is set.
  { TODO : use the entire mapping, not only the first character and use the
           case fold abilities of the unit. }
  if ((FUREBuffer.Flags and _URE_DFA_CASEFOLD) <> 0) and (Symbol.AType = _URE_CHAR) then
    Symbol.Symbol.Chr := UnicodeToLower(Symbol.Symbol.Chr)[0];

  // If the symbol constructed is anything other than one of the anchors,
  // make sure the _URE_DFA_BLANKLINE flag is removed.
  if (Symbol.AType <> _URE_BOL_ANCHOR) and (Symbol.AType <> _URE_EOL_ANCHOR) then
    FUREBuffer.Flags := FUREBuffer.Flags and not _URE_DFA_BLANKLINE;

  // Return the number of characters consumed.
  Result := Run - S;
end;

function TURESearch.SymbolsAreDifferent(A, B: PUcSymbolTableEntry): Boolean;
begin
  Result := False;
  if (A.AType <> B.AType) or (A.Mods <> B.Mods) or (A.Categories <> B.Categories) then
    Result := True
  else
  begin
    if (A.AType = _URE_CCLASS) or (A.AType = _URE_NCCLASS) then
    begin
      if A.Symbol.CCL.RangesUsed <> B.Symbol.CCL.RangesUsed then
        Result := True
      else
      begin
        if (A.Symbol.CCL.RangesUsed > 0) and
          not CompareMem(@A.Symbol.CCL.Ranges[0], @B.Symbol.CCL.Ranges[0],
            SizeOf(TUcRange) * A.Symbol.CCL.RangesUsed) then
          Result := True;;
      end;
    end
    else
    begin
      if (A.AType = _URE_CHAR) and (A.Symbol.Chr <> B.Symbol.Chr) then
        Result := True;
    end;
  end;
end;

function TURESearch.MakeSymbol(S: PUCS2; Limit: SizeInt; out Consumed: SizeInt): SizeInt;
// constructs a symbol, but only keep unique symbols
var
  I: SizeInt;
  Start: PUcSymbolTableEntry;
  Symbol: TUcSymbolTableEntry;
begin
  // Build the next symbol so we can test to see if it is already in the symbol table.
  ResetMemory(Symbol, SizeOf(TUcSymbolTableEntry));
  Consumed := CompileSymbol(S, Limit, @Symbol);

  // Check to see if the symbol exists.
  I := 0;
  Start := @FUREBuffer.SymbolTable.Symbols[0];
  while (I < FUREBuffer.SymbolTable.SymbolsUsed) and SymbolsAreDifferent(@Symbol, Start) do
  begin
    Inc(I);
    Inc(Start);
  end;

  if I < FUREBuffer.SymbolTable.SymbolsUsed then
  begin
    // Free up any ranges used for the symbol.
    if (Symbol.AType = _URE_CCLASS) or (Symbol.AType = _URE_NCCLASS) then
      Symbol.Symbol.CCL.Ranges := nil;
    Result := FUREBuffer.SymbolTable.Symbols[I].ID;
    Exit;
  end;

  // Need to add the new symbol.
  if FUREBuffer.SymbolTable.SymbolsUsed = Length(FUREBuffer.SymbolTable.Symbols) then
  begin
    SetLength(FUREBuffer.SymbolTable.Symbols, Length(FUREBuffer.SymbolTable.Symbols) + 8);
  end;

  Symbol.ID := FUREBuffer.SymbolTable.SymbolsUsed;
  Inc(FUREBuffer.SymbolTable.SymbolsUsed);
  FUREBuffer.SymbolTable.Symbols[Symbol.ID] := Symbol;
  Result := Symbol.ID;
end;

function TURESearch.MakeExpression(AType, LHS, RHS: SizeInt): SizeInt;
var
  I: SizeInt;
begin
  // Determine if the expression already exists or not.
  with FUREBuffer.ExpressionList do
  begin
    for I := 0 to ExpressionsUsed - 1 do
    begin
      if (Expressions[I].AType = AType) and
         (Expressions[I].LHS = LHS) and
         (Expressions[I].RHS = RHS) then
      begin
        Result := I;
        Exit;
      end;
    end;

    // Need to add a new expression.
    if ExpressionsUsed = Length(Expressions) then
      SetLength(Expressions, Length(Expressions) + 8);

    Expressions[ExpressionsUsed].OnStack := False;
    Expressions[ExpressionsUsed].AType := AType;
    Expressions[ExpressionsUsed].LHS := LHS;
    Expressions[ExpressionsUsed].RHS := RHS;

    Result := ExpressionsUsed;
    Inc(ExpressionsUsed);
  end;
end;

function IsSpecial(C: Word): Boolean;
begin
  case C of
    Word('+'),
    Word('*'),
    Word('?'),
    Word('{'),
    Word('|'),
    Word(')'):
      Result := True;
  else
    Result := False;
  end;
end;

procedure TURESearch.CollectPendingOperations(var State: SizeInt);
// collects all pending AND and OR operations and make corresponding expressions
var
  Operation: SizeInt;
begin
  repeat
    Operation := Peek;
    if (Operation <> _URE_AND) and (Operation <> _URE_OR) then
      Break;
    // make an expression with the AND or OR operator and its right hand side
    Operation := Pop;
    State := MakeExpression(Operation, Pop, State);
  until False;
end;

function TURESearch.ConvertRegExpToNFA(RE: PWideChar; RELength: SizeInt): SizeInt;
// Converts the regular expression into an NFA in a form that will be easy to
// reduce to a DFA. The starting state for the reduction will be returned.
var
  C: UCS2;
  Head, Tail: PUCS2;
  S: WideString;
  Symbol,
  State,
  LastState,
  Used,
  M, N: SizeInt;
  I: SizeInt;
begin
  State := _URE_NOOP;

  Head := RE;
  Tail := Head + RELength;
  while (FUREBuffer.Error = _URE_OK) and (Head < Tail) do
  begin
    C := Head^;
    Inc(Head);
    case C of
      '(':
        Push(_URE_PAREN);
      ')': // check for the case of too many close parentheses
        begin
          if Peek = _URE_NOOP then
          begin
            FUREBuffer.Error := _URE_UNBALANCED_GROUP;
            Break;
          end;
          CollectPendingOperations(State);
          // remove the _URE_PAREN off the stack
          Pop;
        end;
      '*':
        State := MakeExpression(_URE_STAR, State, _URE_NOOP);
      '+':
        State := MakeExpression(_URE_PLUS, State, _URE_NOOP);
      '?':
        State := MakeExpression(_URE_QUEST, State, _URE_NOOP);
      '|':
        begin
          CollectPendingOperations(State);
          Push(State);
          Push(_URE_OR);
        end;
      '{': // expressions of the form {m, n}
        begin
          C := #0;
          M := 0;
          N := 0;
          // get first number
          while UnicodeIsWhiteSpace(UCS4(Head^)) do
            Inc(Head);
          // very slow implementation
          S := '';
          while (Head^ >= WideChar('0')) and (Head^ <= WideChar('9')) do
          begin
            S := S + Head^;
            Inc(Head);
          end;
          if S <> '' then
            M := StrToInt(S);

          while UnicodeIsWhiteSpace(UCS4(Head^)) do
            Inc(Head);
          if (Head^ <> ',') and (Head^ <> '}') then
          begin
            FUREBuffer.Error := _URE_INVALID_RANGE;
            Break;
          end;

          // check for an upper limit
          if Head^ <> '}' then
          begin
            Inc(Head);
            // get second number
            while UnicodeIsWhiteSpace(UCS4(Head^)) do
              Inc(Head);
            // very slow implementation
            S := '';
            while (Head^ >= WideChar('0')) and (Head^ <= WideChar('9')) do
            begin
              S := S + Head^;
              Inc(Head);
            end;
            if S <> '' then
              N := StrToInt(S);
          end
          else
            N := M;

          if Head^ <> '}' then
          begin
            FUREBuffer.Error := _URE_RANGE_OPEN;
            Break;
          end
          else
            Inc(Head);

          // N = 0 means unlimited number of occurences
          if N = 0 then
          begin
            case M of
              0: // {,} {0,}  {0, 0} mean the same as the star operator
                State := MakeExpression(_URE_STAR, State, _URE_NOOP);
              1: // {1,} {1, 0} mean the same as the plus operator
                State := MakeExpression(_URE_PLUS, State, _URE_NOOP);
            else
              begin
                // encapsulate the expanded branches as would they be in parenthesis
                // in order to avoid unwanted concatenation with pending operations/symbols
                Push(_URE_PAREN);
                // {m,} {m, 0} mean M fixed occurences plus star operator
                // make E^m...
                for I := 1 to M - 1 do
                begin
                  Push(State);
                  Push(_URE_AND);
                end;
                // ...and repeat the last symbol one or more times
                State := MakeExpression(_URE_PLUS, State, _URE_NOOP);
                CollectPendingOperations(State);
                Pop;
              end;
            end;
          end
          else
          begin
            // check proper range limits
            if M > N then
            begin
              FUREBuffer.Error := _URE_INVALID_RANGE;
              Break;
            end;

            // check special case {0, 1} (which corresponds to the ? operator)
            if (M = 0) and (N = 1) then
              State := MakeExpression(_URE_QUEST, State, _URE_NOOP)
            else
            begin
              // handle the general case by expanding {m, n} into the equivalent
              // expression E^m | E^(m + 1) | ... | E^n

              // encapsulate the expanded branches as would they be in parenthesis
              // in order to avoid unwanted concatenation with pending operations/symbols
              Push(_URE_PAREN);
              // keep initial state as this is the one all alternatives start from
              LastState := State;

              // Consider the special case M = 0 first. Because there's no construct
              // to enter a pure epsilon-transition into the expression array I
              // work around with the question mark operator to describe the first
              // and second branch alternative.
              if M = 0 then
              begin
                State := MakeExpression(_URE_QUEST, State, _URE_NOOP);
                Inc(M, 2);
                // Mark the pending OR operation (there must always follow at
                // least on more alternative because the special case {0, 1} has
                // already been handled).
                Push(State);
                Push(_URE_OR);
              end;

              while M <= N do
              begin
                State := LastState;
                // create E^M
                for I := 1 to SizeInt(M) - 1 do
                begin
                  Push(State);
                  Push(_URE_AND);
                end;
                // finish the branch and mark it as pending OR operation if it
                // isn't the last one
                CollectPendingOperations(State);
                if M < N then
                begin
                  Push(State);
                  Push(_URE_OR);
                end;
                Inc(M);
              end;
              // remove the _URE_PAREN off the stack
              Pop;
            end;
          end;
        end;
    else
      Dec(Head);
      Symbol := MakeSymbol(Head, Tail - Head, Used);
      Inc(Head, Used);
      State := MakeExpression(_URE_SYMBOL, Symbol, _URE_NOOP);
    end;

    if (C <> '(') and (C <> '|') and (C <> '{') and (Head < Tail) and
       (not IsSpecial(Word(Head^)) or (Head^ = '(')) then
    begin
      Push(State);
      Push(_URE_AND);
    end;
  end;

  CollectPendingOperations(State);
  if FUREBuffer.Stack.ListUsed > 0 then
    FUREBuffer.Error := _URE_UNBALANCED_GROUP;

  if FUREBuffer.Error = _URE_OK then
    Result := State
  else
    Result := _URE_NOOP;
end;

procedure TURESearch.AddSymbolState(Symbol, State: SizeInt);
var
  I, J: SizeInt;
  Found: Boolean;
begin
  // Locate the symbol in the symbol table so the state can be added.
  // If the symbol doesn't exist, then we are in serious trouble.
  with FUREBuffer.SymbolTable do
  begin
    I := 0;
    while (I < SymbolsUsed) and (Symbol <> Symbols[I].ID) do
      Inc(I);

    Assert(I < SymbolsUsed);
  end;

  // Now find out if the state exists in the symbol's state list.
  with FUREBuffer.SymbolTable.Symbols[I].States do
  begin
    Found := False;
    for J := 0 to ListUsed - 1 do
    begin
      if State <= List[J] then
      begin
        Found := True;
        Break;
      end;
    end;

    if not Found then
      J := ListUsed;
    if not Found or (State < List[J]) then
    begin
      // Need to add the state in order.
      if ListUsed = Length(List) then
        SetLength(List, Length(List) + 8);
      if J < ListUsed then
        Move(List[J], List[J + 1], SizeOf(SizeInt) * (ListUsed - J));
      List[J] := State;
      Inc(ListUsed);
    end;
  end;
end;

function TURESearch.AddState(NewStates: array of SizeInt): SizeInt;
var
  I: SizeInt;
  Found: Boolean;
begin
  Found := False;
  for I := 0 to FUREBuffer.States.StatesUsed - 1 do
  begin
    if (FUREBuffer.States.States[I].StateList.ListUsed = Length(NewStates)) and
       CompareMem(@NewStates[0], @FUREBuffer.States.States[I].StateList.List[0],
         SizeOf(SizeInt) * Length(NewStates)) then
    begin
      Found := True;
      Break;
    end;
  end;

  if not Found then
  begin
    // Need to add a new DFA State (set of NFA states).
    if FUREBuffer.States.StatesUsed = Length(FUREBuffer.States.States) then
      SetLength(FUREBuffer.States.States, Length(FUREBuffer.States.States) + 8);

    with FUREBuffer.States.States[FUREBuffer.States.StatesUsed] do
    begin
      ID := FUREBuffer.States.StatesUsed;
      if (StateList.ListUsed + Length(NewStates)) >= Length(StateList.List) then
        SetLength(StateList.List, Length(StateList.List) + Length(NewStates) + 8);
      Move(NewStates[0], StateList.List[StateList.ListUsed], SizeOf(SizeInt) * Length(NewStates));
      Inc(StateList.ListUsed, Length(NewStates));
    end;
    Inc(FUREBuffer.States.StatesUsed);
  end;

  // Return the ID of the DFA state representing a group of NFA States.
  if Found then
    Result := I
  else
    Result := FUREBuffer.States.StatesUsed - 1;
end;

procedure TURESearch.Reduce(Start: SizeInt);
var
  I, J,
  Symbols: SizeInt;
  State,
  RHS,
  s1, s2,
  ns1, ns2: SizeInt;
  Evaluating: Boolean;
begin
  FUREBuffer.Reducing := True;

  // Add the starting state for the reduction.
  AddState([Start]);

  // Process each set of NFA states that get created.
  I := 0;
  // further states are added in the loop
  while I < FUREBuffer.States.StatesUsed do
  begin
    with FUREBuffer, States.States[I], ExpressionList do
    begin
      // Push the current states on the stack.
      for J := 0 to StateList.ListUsed - 1 do
        Push(StateList.List[J]);

      // Reduce the NFA states.
      Accepting := False;
      Symbols := 0;
      J := 0;
      // need a while loop here as the stack will be modified within the loop and
      // so also its usage count used to terminate the loop
      while J < FUREBuffer.Stack.ListUsed do
      begin
        State := FUREBuffer.Stack.List[J];
        Evaluating := True;

        // This inner loop is the iterative equivalent of recursively
        // reducing subexpressions generated as a result of a reduction.
        while Evaluating do
        begin
          case Expressions[State].AType of
            _URE_SYMBOL:
              begin
                ns1 := MakeExpression(_URE_ONE, _URE_NOOP, _URE_NOOP);
                AddSymbolState(Expressions[State].LHS, ns1);
                Inc(Symbols);
                Evaluating := False;
              end;
            _URE_ONE:
              begin
                Accepting := True;
                Evaluating := False;
              end;
            _URE_QUEST:
              begin
                s1 := Expressions[State].LHS;
                ns1 := MakeExpression(_URE_ONE, _URE_NOOP, _URE_NOOP);
                State := MakeExpression(_URE_OR, ns1, s1);
              end;
            _URE_PLUS:
              begin
                s1 := Expressions[State].LHS;
                ns1 := MakeExpression(_URE_STAR, s1, _URE_NOOP);
                State := MakeExpression(_URE_AND, s1, ns1);
              end;
            _URE_STAR:
              begin
                s1 := Expressions[State].LHS;
                ns1 := MakeExpression(_URE_ONE, _URE_NOOP, _URE_NOOP);
                ns2 := MakeExpression(_URE_PLUS, s1, _URE_NOOP);
                State := MakeExpression(_URE_OR, ns1, ns2);
              end;
            _URE_OR:
              begin
                s1 := Expressions[State].LHS;
                s2 := Expressions[State].RHS;
                Push(s1);
                Push(s2);
                Evaluating := False;
              end;
            _URE_AND:
              begin
                s1 := Expressions[State].LHS;
                s2 := Expressions[State].RHS;
                case Expressions[s1].AType of
                  _URE_SYMBOL:
                    begin
                      AddSymbolState(Expressions[s1].LHS, s2);
                      Inc(Symbols);
                      Evaluating := False;
                    end;
                  _URE_ONE:
                    State := s2;
                  _URE_QUEST:
                    begin
                      ns1 := Expressions[s1].LHS;
                      ns2 := MakeExpression(_URE_AND, ns1, s2);
                      State := MakeExpression(_URE_OR, s2, ns2);
                    end;
                  _URE_PLUS:
                    begin
                      ns1 := Expressions[s1].LHS;
                      ns2 := MakeExpression(_URE_OR, s2, State);
                      State := MakeExpression(_URE_AND, ns1, ns2);
                    end;
                  _URE_STAR:
                    begin
                      ns1 := Expressions[s1].LHS;
                      ns2 := MakeExpression(_URE_AND, ns1, State);
                      State := MakeExpression(_URE_OR, s2, ns2);
                    end;
                  _URE_OR:
                    begin
                      ns1 := Expressions[s1].LHS;
                      ns2 := Expressions[s1].RHS;
                      ns1 := MakeExpression(_URE_AND, ns1, s2);
                      ns2 := MakeExpression(_URE_AND, ns2, s2);
                      State := MakeExpression(_URE_OR, ns1, ns2);
                    end;
                  _URE_AND:
                    begin
                      ns1 := Expressions[s1].LHS;
                      ns2 := Expressions[s1].RHS;
                      ns2 := MakeExpression(_URE_AND, ns2, s2);
                      State := MakeExpression(_URE_AND, ns1, ns2);
                    end;
                end;
              end;
          end;
        end;
        Inc(J);
      end;

      // clear the state stack
      while Pop <> _URE_NOOP do
        { nothing };

      // generate the DFA states for the symbols collected during the current reduction
      if (TransitionsUsed + Symbols) > Length(Transitions) then
        SetLength(Transitions, Length(Transitions) + Symbols);

      // go through the symbol table and generate the DFA state transitions for
      // each symbol that has collected NFA states
      Symbols := 0;
      J := 0;
      while J < FUREBuffer.SymbolTable.SymbolsUsed do
      begin
        begin
          if FUREBuffer.SymbolTable.Symbols[J].States.ListUsed > 0 then
          begin
            Transitions[Symbols].LHS := FUREBuffer.SymbolTable.Symbols[J].ID;
            with FUREBuffer.SymbolTable.Symbols[J] do
            begin
              RHS := AddState(Copy(States.List, 0, States.ListUsed));
              States.ListUsed := 0;
            end;
            Transitions[Symbols].RHS := RHS;
            Inc(Symbols);
          end;
        end;
        Inc(J);
      end;

      // set the number of transitions actually used
      // Note: we need again to qualify a part of the TransistionsUsed path since the
      //       state array could be reallocated in the AddState call above and the
      //       with ... do will then be invalid.
      States.States[I].TransitionsUsed := Symbols;
    end;
    Inc(I);
  end;
  FUREBuffer.Reducing := False;
end;

procedure TURESearch.AddEquivalentPair(L, R: SizeInt);
var
  I: SizeInt;
begin
  L := FUREBuffer.States.States[L].ID;
  R := FUREBuffer.States.States[R].ID;

  if L <> R then
  begin
    if L > R then
    begin
      I := L;
      L := R;
      R := I;
    end;

    // Check to see if the equivalence pair already exists.
    I := 0;
    with FUREBuffer.EquivalentList do
    begin
      while (I < EquivalentsUsed) and
            ((Equivalents[I].Left <> L) or (Equivalents[I].Right <> R)) do
        Inc(I);

      if I >= EquivalentsUsed then
      begin
        if EquivalentsUsed = Length(Equivalents) then
          SetLength(Equivalents, Length(Equivalents) + 8);

        Equivalents[EquivalentsUsed].Left := L;
        Equivalents[EquivalentsUsed].Right := R;
        Inc(EquivalentsUsed);
      end;
    end;
  end;
end;

procedure TURESearch.MergeEquivalents;
// merges the DFA states that are equivalent
var
  I, J, K,
  Equal: SizeInt;
  Done: Boolean;
  State1, State2,
  LeftState,
  RightState: PUcState;
begin
  for I := 0 to FUREBuffer.States.StatesUsed - 1 do
  begin
    State1 := @FUREBuffer.States.States[I];
    if State1.ID = SizeInt(I) then
    begin
      J := 0;
      while J < I do
      begin
        State2 := @FUREBuffer.States.States[J];
        if State2.ID = SizeInt(J) then
        begin
          FUREBuffer.EquivalentList.EquivalentsUsed := 0;
          AddEquivalentPair(I, J);

          Done := False;
          Equal := 0;
          while Equal < FUREBuffer.EquivalentList.EquivalentsUsed do
          begin
            LeftState := @FUREBuffer.States.States[FUREBuffer.EquivalentList.Equivalents[Equal].Left];
            RightState := @FUREBuffer.States.States[FUREBuffer.EquivalentList.Equivalents[Equal].Right];

            if (LeftState.Accepting <> RightState.Accepting) or
               (LeftState.TransitionsUsed <> RightState.TransitionsUsed) then
            begin
              Done := True;
              Break;
            end;

            K := 0;
            while (K < LeftState.TransitionsUsed) and
                  (LeftState.Transitions[K].LHS = RightState.Transitions[K].LHS) do
              Inc(K);

            if K < LeftState.TransitionsUsed then
            begin
              Done := True;
              Break;
            end;

            for K := 0 to LeftState.TransitionsUsed - 1 do
              AddEquivalentPair(LeftState.Transitions[K].RHS, RightState.Transitions[K].RHS);

            Inc(Equal);
          end;

          if not Done then
            Break;
        end;
        Inc(J);
      end;

      if J < I then
      begin
        with FUREBuffer do
        begin
          for Equal := 0 to EquivalentList.EquivalentsUsed - 1 do
          begin
            States.States[EquivalentList.Equivalents[Equal].Right].ID :=
              States.States[EquivalentList.Equivalents[Equal].Left].ID;
          end;
        end;
      end;

    end;
  end;

  // Renumber the states appropriately
  State1 := @FUREBuffer.States.States[0];
  Equal := 0;
  for I := 0 to FUREBuffer.States.StatesUsed - 1 do
  begin
    if State1.ID = SizeInt(I) then
    begin
      State1.ID := Equal;
      Inc(Equal);
    end
    else
      State1.ID := FUREBuffer.States.States[State1.ID].ID;
    Inc(State1);
  end;
end;

procedure TURESearch.ClearUREBuffer;
var
  I: SizeInt;
begin
  with FUREBuffer do
  begin
    // quite a few dynamic arrays to free
    Stack.List := nil;
    ExpressionList.Expressions := nil;

    // the symbol table has been handed over to the DFA and will be freed on
    // release of the DFA
    SymbolTable.SymbolsUsed := 0;

    for I := 0 to States.StatesUsed - 1 do
    begin
      States.States[I].Transitions := nil;
      States.States[I].StateList.List := nil;
      States.States[I].StateList.ListUsed := 0;
      States.States[I].TransitionsUsed := 0;
    end;

    States.StatesUsed := 0;
    States.States := nil;
    EquivalentList.Equivalents := nil;
  end;
  ResetMemory(FUREBuffer, SizeOf(FUREBuffer));
end;

procedure TURESearch.CompileURE(RE: PWideChar; RELength: SizeInt; Casefold: Boolean);
var
  I, J: SizeInt;
  State: SizeInt;
  Run: PUcState;
  TP: SizeInt;

  procedure UREError(Text: string; RE: PWideChar);
  var
    S: string;
  begin
    S := RE;
    raise EJclUnicodeError.CreateResFmt(@RsUREErrorFmt, [LoadResString(@RsUREBaseString), Text, S]);
  end;

begin
  // be paranoid
  if (RE <> nil) and (RE^ <> WideNull) and (RELength > 0) then
  begin
    // Reset the various fields of the compilation buffer. Default the Flags
    // to indicate the presense of the "^$" pattern.  If any other pattern
    // occurs, then this flag will be removed.  This is done to catch this
    // special pattern and handle it specially when matching.
    ClearUREBuffer;
    ClearDFA;
    FUREBuffer.Flags := _URE_DFA_BLANKLINE;
    if Casefold then
      FUREBuffer.Flags := FUREBuffer.Flags or _URE_DFA_CASEFOLD;

    // Construct the NFA. If this stage returns a 0, then an error occured or an
    // empty expression was passed.
    State := ConvertRegExpToNFA(RE, RELength);
    if State <> _URE_NOOP then
    begin
      // Do the expression reduction to get the initial DFA.
      Reduce(State);

      // Merge all the equivalent DFA States.
      MergeEquivalents;

      // Construct the minimal DFA.
      FDFA.Flags := FUREBuffer.Flags and (_URE_DFA_CASEFOLD or _URE_DFA_BLANKLINE);

      // Free up the NFA state groups and transfer the symbols from the buffer
      // to the DFA.
      FDFA.SymbolTable := FUREBuffer.SymbolTable;
      FUREBuffer.SymbolTable.Symbols := nil;

      // Collect the total number of states and transitions needed for the DFA.
      State := 0;
      for I := 0 to FUREBuffer.States.StatesUsed - 1 do
      begin
        if FUREBuffer.States.States[I].ID = State then
        begin
          Inc(FDFA.StateList.StatesUsed);
          Inc(FDFA.TransitionList.TransitionsUsed, FUREBuffer.States.States[I].TransitionsUsed);
          Inc(State);
        end;
      end;

      // Allocate enough space for the states and transitions.
      SetLength(FDFA.StateList.States, FDFA.StateList.StatesUsed);
      SetLength(FDFA.TransitionList.Transitions, FDFA.TransitionList.TransitionsUsed);

      // Actually transfer the DFA States from the buffer.
      State := 0;
      TP := 0;
      Run := @FUREBuffer.States.States[0];
      for I := 0 to FUREBuffer.States.StatesUsed - 1 do
      begin
        if Run.ID = State then
        begin
          FDFA.StateList.States[I].StartTransition := TP;
          FDFA.StateList.States[I].NumberTransitions := Run.TransitionsUsed;
          FDFA.StateList.States[I].Accepting := Run.Accepting;

          // Add the transitions for the state
          for J := 0 to FDFA.StateList.States[I].NumberTransitions - 1 do
          begin
            FDFA.TransitionList.Transitions[TP].Symbol := Run.Transitions[J].LHS;
            FDFA.TransitionList.Transitions[TP].NextState :=
              FUREBuffer.States.States[Run.Transitions[J].RHS].ID;
            Inc(TP);
          end;

          Inc(State);
        end;
        Inc(Run);
      end;
    end
    else
    begin
      // there might be an error while parsing the pattern, show it if so
      case FUREBuffer.Error of
        _URE_UNEXPECTED_EOS:
          UREError(LoadResString(@RsUREUnexpectedEOS), RE);
        _URE_CCLASS_OPEN:
          UREError(LoadResString(@RsURECharacterClassOpen), RE);
        _URE_UNBALANCED_GROUP:
          UREError(LoadResString(@RsUREUnbalancedGroup), RE);
        _URE_INVALID_PROPERTY:
          UREError(LoadResString(@RsUREInvalidCharProperty), RE);
        _URE_INVALID_RANGE:
          UREError(LoadResString(@RsUREInvalidRepeatRange), RE);
        _URE_RANGE_OPEN:
          UREError(LoadResString(@RsURERepeatRangeOpen), RE);
      else
        // expression was empty
        raise EJclUnicodeError.CreateRes(@RsUREExpressionEmpty);
      end;
    end;
  end;
end;

procedure TURESearch.ClearDFA;
var
  I: SizeInt;
begin
  with FDFA do
  begin
    for I := 0 to SymbolTable.SymbolsUsed - 1 do
    begin
      if (SymbolTable.Symbols[I].AType = _URE_CCLASS) or
         (SymbolTable.Symbols[I].AType = _URE_NCCLASS) then
        SymbolTable.Symbols[I].Symbol.CCL.Ranges := nil;
    end;

    for I := 0 to SymbolTable.SymbolsUsed - 1 do
    begin
      FDFA.SymbolTable.Symbols[I].States.List := nil;
      FDFA.SymbolTable.Symbols[I].States.ListUsed := 0;
    end;
    SymbolTable.SymbolsUsed := 0;

    SymbolTable.Symbols := nil;
    StateList.States := nil;
    TransitionList.Transitions := nil;
  end;
  ResetMemory(FDFA, SizeOf(FDFA));
end;

function IsSeparator(C: UCS4): Boolean;
begin
  Result := (C = $D) or (C = $A) or (C = $2028) or (C = $2029);
end;

function TURESearch.ExecuteURE(Flags: Cardinal; Text: PUCS2; TextLen: SizeInt; var MatchStart,
  MatchEnd: SizeInt): Boolean;
var
  I, J: SizeInt;
  Matched,
  Found: Boolean;
  Start, Stop: SizeInt;
  C: UCS4;
  Run, Tail, lp: PUCS2;
  LastState: PDFAState;
  Symbol: PUcSymbolTableEntry;
  Rp: PUcRange;
  LCMapping: TUCS4Array;
begin
  Result := False;
  if Text <> nil then
  begin
    // Handle the special case of an empty string matching the "^$" pattern.
    if (Textlen = 0) and ((FDFA.Flags and _URE_DFA_BLANKLINE) <> 0) then
    begin
      MatchStart := 0;
      MatchEnd := 0;
      Result := True;
      Exit;
    end;

    Run := Text;
    Tail := Run + TextLen;
    Start := -1;
    Stop := -1;
    LastState := @FDFA.StateList.States[0];

    Found := False;
    while not Found and (Run < Tail) do
    begin
      lp := Run;
      C := UCS4(Run^);
      Inc(Run);

      // Check to see if this is a high surrogate that should be combined with a
      // following low surrogate.
      if (Run < Tail) and
         (SurrogateHighStart <= C) and (C <= SurrogateHighEnd) and
         (SurrogateLowStart <= UCS4(Run^)) and (UCS4(Run^) <= SurrogateLowEnd) then
      begin
        C := $10000 + (((C and $03FF) shl 10) or (UCS4(Run^) and $03FF));
        Inc(Run);
      end;

      // Determine if the character is non-spacing and should be skipped.
      if ((Flags and URE_IGNORE_NONSPACING) <> 0) and UnicodeIsNonSpacing(C) then
      begin
        Inc(Run);
        Continue;
      end;

      if (FDFA.Flags and _URE_DFA_CASEFOLD) <> 0 then
      begin
        SetLength(LCMapping, 0);
        { TODO : use the entire mapping, not only the first character }
        // (CaseLookup used for a little extra speed: avoids dynamic array allocation)
        if CaseLookup(C, ctLower, LCMapping) then
          C := LCMapping[0];
      end;

      // See if one of the transitions matches.
      I := LastState.NumberTransitions - 1;
      Matched := False;

      while not Matched and (I >= 0) do
      begin
        Symbol := @FDFA.SymbolTable.Symbols[FDFA.TransitionList.Transitions[LastState.StartTransition + I].Symbol];
        case Symbol.AType of
          _URE_ANY_CHAR:
            if ((Flags and URE_DONT_MATCHES_SEPARATORS) <> 0) or
               not IsSeparator(C) then
              Matched := True;
          _URE_CHAR:
            if C = Symbol.Symbol.Chr then
              Matched := True;
          _URE_BOL_ANCHOR:
            if Lp = Text then
            begin
              Run := lp;
              Matched := True;
            end
            else
            begin
              if IsSeparator(C) then
              begin
                if (C = $D) and (Run < Tail) and (Run^ = #$A) then
                  Inc(Run);
                Lp := Run;
                Matched := True;
              end;
            end;
          _URE_EOL_ANCHOR:
            if IsSeparator(C) then
            begin
              // Put the pointer back before the separator so the match end
              // position will be correct. This  will also cause the `Run'
              // pointer to be advanced over the current separator once the
              // match end point has been recorded.
              Run := Lp;
              Matched := True;
            end;
          _URE_CCLASS,
          _URE_NCCLASS:
            with Symbol^ do
            begin
              if Categories <> [] then
                Matched := CategoryLookup(C, Categories);
              if Symbol.CCL.RangesUsed > 0 then
              begin
                Rp := @Symbol.CCL.Ranges[0];
                for J := 0 to Symbol.CCL.RangesUsed - 1 do
                begin
                  if (Rp.MinCode <= C) and (C <= Rp.MaxCode) then
                  begin
                    Matched := True;
                    Break;
                  end;
                  Inc(Rp);
                end;
              end;

              if AType = _URE_NCCLASS then
                Matched := not Matched;
            end;
        end;

        if Matched then
        begin
          if Start = -1 then
            Start := Lp - Text
          else
            Stop := Run - Text;

          LastState := @FDFA.StateList.States[FDFA.TransitionList.Transitions[LastState.StartTransition + I].NextState];

          // If the match was an EOL anchor, adjust the pointer past the separator
          // that caused the match. The correct match position has been recorded
          // already.
          if Symbol.AType = _URE_EOL_ANCHOR then
          begin
            // skip the character that caused the match
            Inc(Run);
            // handle the infamous CRLF situation
            if (Run < Tail) and (C = $D) and (Run^ = #$A) then
              Inc(Run);
          end;
        end;
        Dec(I);
      end;

      if not Matched then
      begin
        Found := LastState.Accepting;
        if not Found then
        begin
          // If the last state was not accepting, then reset and start over.
          LastState := @FDFA.StateList.States[0];
          Start := -1;
          Stop := -1;
        end
        else
        begin
          // set start and stop pointer if not yet done
          if Start = -1 then
          begin
            Start := Lp - Text;
            Stop := Run - Text;
          end
          else
          begin
            if Stop = -1 then
              Stop := Lp - Text;
          end;
        end;
      end
      else
      begin
        if Run = Tail then
        begin
          if not LastState.Accepting then
          begin
            // This ugly hack is to make sure the end-of-line anchors match
            // when the source text hits the end. This is only done if the last
            // subexpression matches.
            for I := 0 to LastState.NumberTransitions - 1 do
            begin
              if Found then
                Break;
              Symbol := @FDFA.SymbolTable.Symbols[FDFA.TransitionList.Transitions[LastState.StartTransition + I].Symbol];
              if Symbol.AType =_URE_EOL_ANCHOR then
              begin
                LastState := @FDFA.StateList.States[FDFA.TransitionList.Transitions[LastState.StartTransition + I].NextState];
                if LastState.Accepting then
                begin
                  Stop := Run - Text;
                  Found := True;
                end
                else
                  Break;
              end;
            end;
          end
          else
          begin
            // Make sure any conditions that match all the way to the end of
            // the string match.
            Found := True;
            Stop := Run - Text;
          end;
        end;
      end;
    end;

    if Found then
    begin
      MatchStart := Start;
      MatchEnd := Stop;
    end;
    Result := Found;
  end;
end;

function TURESearch.FindAll(const Text: WideString): Boolean;
begin
  Result := FindAll(PWideChar(Text), Length(Text));
end;

function TURESearch.FindAll(Text: PWideChar; TextLen: SizeInt): Boolean;
// Looks for all occurences of the pattern passed to FindPrepare and creates an
// internal list of their positions.
var
  Start, Stop: SizeInt;
  Run: PWideChar;
  RunLen: SizeInt;
begin
  ClearResults;
  Run := Text;
  RunLen := TextLen;
  // repeat to find all occurences of the pattern
  Start := 0;
  Stop := 0;
  while ExecuteURE(0, Run, RunLen, Start, Stop) do
  begin
    // store this result (consider text pointer movement)...
    AddResult(Start + (Run - Text), Stop + (Run - Text));
    // ... and advance text position and length
    Inc(Run, Stop);
    Dec(RunLen, Stop);
  end;
  Result := FResults.Count > 0;
end;

function TURESearch.FindFirst(const Text: WideString; var Start, Stop: SizeInt): Boolean;
begin
  Result := FindFirst(PWideChar(Text), Length(Text), Start, Stop);
end;

function TURESearch.FindFirst(Text: PWideChar; TextLen: SizeInt; var Start, Stop: SizeInt): Boolean;
// Looks for the first occurence of the pattern passed to FindPrepare in Text and
// returns True if one could be found (in which case Start and Stop are set to
// the according indices) otherwise False. This function is in particular of
// interest if only one occurence needs to be found.
begin
  ClearResults;
  Result := ExecuteURE(0, Text, TextLen, Start, Stop);
  if Result then
    AddResult(Start, Stop);
end;

procedure TURESearch.FindPrepare(Pattern: PWideChar; PatternLength: SizeInt; Options: TSearchFlags);
begin
  CompileURE(Pattern, PatternLength, not (sfCaseSensitive in Options));
end;

procedure TURESearch.FindPrepare(const Pattern: WideString; Options: TSearchFlags);
begin
  CompileURE(PWideChar(Pattern), Length(Pattern), not (sfCaseSensitive in Options));
end;

//=== { TWideStrings } =======================================================

constructor TWideStrings.Create;
begin
  inherited Create;
  FLanguage := GetUserDefaultLCID;
  FNormalizationForm := nfC;
  FSaveFormat := sfUnicodeLSB;
end;

procedure TWideStrings.SetLanguage(Value: LCID);
begin
  FLanguage := Value;
end;

function TWideStrings.GetSaveUnicode: Boolean;
begin
  Result := SaveFormat = sfUnicodeLSB;
end;

procedure TWideStrings.SetSaveUnicode(const Value: Boolean);
begin
  if Value then
    SaveFormat := sfUnicodeLSB
  else
    SaveFormat := sfAnsi;
end;

function TWideStrings.Add(const S: WideString): Integer;
begin
  Result := GetCount;
  Insert(Result, S);
end;

function TWideStrings.AddObject(const S: WideString; AObject: TObject): Integer;
begin
  Result := Add(S);
  PutObject(Result, AObject);
end;

procedure TWideStrings.Append(const S: WideString);
begin
  Add(S);
end;

procedure TWideStrings.AddStrings(Strings: TStrings);
var
  I: Integer;
  {$IFNDEF SUPPORTS_UNICODE}
  CP: Word;
  {$ENDIF ~SUPPORTS_UNICODE}
begin
  BeginUpdate;
  try
    {$IFNDEF SUPPORTS_UNICODE}
    CP := CodePageFromLocale(FLanguage);
    {$ENDIF ~SUPPORTS_UNICODE}
    for I := 0 to Strings.Count - 1 do
    begin
      {$IFDEF SUPPORTS_UNICODE}
      AddObject(Strings[I], Strings.Objects[I])
      {$ELSE ~SUPPORTS_UNICODE}
      AddObject(StringToWideStringEx(Strings[I], CP), Strings.Objects[I])
      {$ENDIF ~SUPPORTS_UNICODE}
    end;
  finally
    EndUpdate;
  end;
end;

procedure TWideStrings.AddStrings(Strings: TWideStrings);
var
  I: Integer;
begin
  Assert(Strings <> nil);

  BeginUpdate;
  try
    for I := 0 to Strings.Count - 1 do
      AddObject(Strings[I], Strings.Objects[I]);
  finally
    EndUpdate;
  end;
end;

procedure TWideStrings.Assign(Source: TPersistent);
// usual assignment routine, but able to assign wide and small strings
begin
  if Source is TWideStrings then
  begin
    BeginUpdate;
    try
      Clear;
      AddStrings(TWideStrings(Source));
    finally
      EndUpdate;
    end;
  end
  else
  begin
    if Source is TStrings then
    begin
      BeginUpdate;
      try
        Clear;
        AddStrings(TStrings(Source));
      finally
        EndUpdate;
      end;
    end
    else
      inherited Assign(Source);
  end;
end;

procedure TWideStrings.AssignTo(Dest: TPersistent);
// need to do also assignment to old style TStrings, but this class doesn't know
// TWideStrings, so we need to do it from here
var
  I: Integer;
  {$IFNDEF SUPPORTS_UNICODE}
  CP: Word;
  {$ENDIF ~SUPPORTS_UNICODE}
begin
  if Dest is TStrings then
  begin
    with Dest as TStrings do
    begin
      BeginUpdate;
      try
        {$IFNDEF SUPPORTS_UNICODE}
        CP := CodePageFromLocale(FLanguage);
        {$ENDIF SUPPORTS_UNICODE}
        Clear;
        for I := 0 to Self.Count - 1 do
        begin
          {$IFDEF SUPPORTS_UNICODE}
          AddObject(Self[I], Self.Objects[I]);
          {$ELSE ~SUPPORTS_UNICODE}
          AddObject(WideStringToStringEx(Self[I], CP), Self.Objects[I]);
          {$ENDIF ~SUPPORTS_UNICODE}
        end;
      finally
        EndUpdate;
      end;
    end;
  end
  else
  begin
    if Dest is TWideStrings then
    begin
      with Dest as TWideStrings do
      begin
        BeginUpdate;
        try
          Clear;
          AddStrings(Self);
        finally
          EndUpdate;
        end;
      end;
    end
    else
      inherited AssignTo(Dest);
  end;
end;

procedure TWideStrings.BeginUpdate;
begin
  if FUpdateCount = 0 then
    SetUpdateState(True);
  Inc(FUpdateCount);
end;

procedure TWideStrings.DefineProperties(Filer: TFiler);

// Defines a private property for the content of the list.
// There's a bug in the handling of text DFMs in Classes.pas which prevents
// WideStrings from loading under some circumstances. Zbysek Hlinka
// (zhlinka att login dott cz) brought this to my attention and supplied also a solution.
// See ReadData and WriteData methods for implementation details.

  //--------------- local function --------------------------------------------

  function DoWrite: Boolean;
  begin
    if Filer.Ancestor <> nil then
    begin
      Result := True;
      if Filer.Ancestor is TWideStrings then
        Result := not Equals(TWideStrings(Filer.Ancestor))
    end
    else
      Result := Count > 0;
  end;

  //--------------- end local function ----------------------------------------

begin
  Filer.DefineProperty('WideStrings', ReadData, WriteData, DoWrite);
end;

procedure TWideStrings.DoConfirmConversion(var Allowed: Boolean);
begin
  if Assigned(FOnConfirmConversion) then
    FOnConfirmConversion(Self, Allowed);
end;

procedure TWideStrings.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    SetUpdateState(False);
end;

function TWideStrings.Equals(Strings: TWideStrings): Boolean;
var
  I, Count: Integer;
begin
  Assert(Strings <> nil);

  Result := False;
  Count := GetCount;
  if Count <> Strings.GetCount then
    Exit;
  { TODO : use internal comparation routine as soon as composition is implemented }
  for I := 0 to Count - 1 do
    if Get(I) <> Strings.Get(I) then
      Exit;
  Result := True;
end;

procedure TWideStrings.Error(const Msg: string; Data: Integer);

  function ReturnAddr: Pointer;
  asm
          {$IFDEF CPU32}
          MOV     EAX, EBP
          MOV     EAX, [EAX + 4]
          {$ENDIF CPU32}
          {$IFDEF CPU64}
          MOV     RAX, RBP
          MOV     RAX, [RAX + 8]
          {$ENDIF CPU64}
  end;

begin
  raise EStringListError.CreateFmt(Msg, [Data]) at ReturnAddr;
end;

procedure TWideStrings.Exchange(Index1, Index2: Integer);
var
  TempObject: TObject;
  TempString: WideString;
begin
  BeginUpdate;
  try
    TempString := Strings[Index1];
    TempObject := Objects[Index1];
    Strings[Index1] := Strings[Index2];
    Objects[Index1] := Objects[Index2];
    Strings[Index2] := TempString;
    Objects[Index2] := TempObject;
  finally
    EndUpdate;
  end;
end;

function TWideStrings.GetCapacity: Integer;
// Descendants may optionally override/replace this default implementation.
begin
  Result := Count;
end;

function TWideStrings.GetCommaText: WideString;
var
  S: WideString;
  P: PWideChar;
  I, Count: Integer;
begin
  Count := GetCount;
  if (Count = 1) and (Get(0) = '') then
    Result := '""'
  else
  begin
    Result := '';
    for I := 0 to Count - 1 do
    begin
      S := Get(I);
      P := PWideChar(S);
      while (P^ > WideSpace) and (P^ <> '"') and (P^ <> ',') do
        Inc(P);
      if P^ <> WideNull then
        S := WideQuotedStr(S, '"');
      Result := Result + S + ',';
    end;
    System.Delete(Result, Length(Result), 1);
  end;
end;

function TWideStrings.GetName(Index: Integer): WideString;
var
  P: Integer;
begin
  Result := Get(Index);
  P := Pos('=', Result);
  if P > 0 then
    SetLength(Result, P - 1)
  else
    Result := '';
end;

function TWideStrings.GetObject(Index: Integer): TObject;
begin
  Result := nil;
end;

function TWideStrings.GetSeparatedText(Separators: WideString): WideString;
// Same as GetText but with customizable separator characters.
var
  I, L,
  Size,
  Count,
  SepSize: Integer;
  P: PWideChar;
  S: WideString;
begin
  Count := GetCount;
  SepSize := Length(Separators);
  Size := 0;
  for I := 0 to Count - 1 do
    Inc(Size, Length(Get(I)) + SepSize);

  // set one separator less, the last line does not need a trailing separator
  SetLength(Result, Size - SepSize);
  if Size > 0 then
  begin
    P := Pointer(Result);
    I := 0;
    while True do
    begin
      S := Get(I);
      L := Length(S);
      if L <> 0 then
      begin
        // add current string
        System.Move(Pointer(S)^, P^, 2 * L);
        Inc(P, L);
      end;
      Inc(I);
      if I = Count then
        Break;

      // add separators
      System.Move(Pointer(Separators)^, P^, SizeOf(WideChar) * SepSize);
      Inc(P, SepSize);
    end;
  end;
end;

function TWideStrings.GetTextStr: WideString;
begin
  Result := GetSeparatedText(WideCRLF);
end;

function TWideStrings.GetText: PWideChar;
begin
  Result := StrNewW(GetTextStr);
end;

function TWideStrings.GetValue(const Name: WideString): WideString;
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I >= 0 then
    Result := Copy(Get(I), Length(Name) + 2, MaxInt)
  else
    Result := '';
end;

function TWideStrings.IndexOf(const S: WideString): Integer;
var
  NormString: WideString;
begin
  NormString := WideNormalize(S, FNormalizationForm);

  for Result := 0 to GetCount - 1 do
    if WideCompareText(Get(Result), NormString, FLanguage) = 0 then
      Exit;
  Result := -1;
end;

function TWideStrings.IndexOfName(const Name: WideString): Integer;
var
  P: Integer;
  S: WideString;
  NormName: WideString;
begin
  NormName := WideNormalize(Name, FNormalizationForm);

  for Result := 0 to GetCount - 1 do
  begin
    S := Get(Result);
    P := Pos('=', S);
    if (P > 0) and (WideCompareText(Copy(S, 1, P - 1), NormName, FLanguage) = 0) then
      Exit;
  end;
  Result := -1;
end;

function TWideStrings.IndexOfObject(AObject: TObject): Integer;
begin
  for Result := 0 to GetCount - 1 do
    if GetObject(Result) = AObject then
      Exit;
  Result := -1;
end;

procedure TWideStrings.InsertObject(Index: Integer; const S: WideString; AObject: TObject);
begin
  Insert(Index, S);
  PutObject(Index, AObject);
end;

procedure TWideStrings.LoadFromFile(const FileName: TFileName);
var
  Stream: TStream;
begin
  try
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  except
    RaiseLastOSError;
  end;
end;

procedure TWideStrings.LoadFromStream(Stream: TStream);
// usual loader routine, but enhanced to handle byte order marks in stream
var
  Size,
  BytesRead: Integer;
  ByteOrderMask: array [0..5] of Byte; // BOM size is max 5 bytes (cf: wikipedia)
                                       // but it is easier to implement with a multiple of 2
  Loaded: Boolean;
  SW: WideString;
  SA: AnsiString;
begin
  BeginUpdate;
  try
    Loaded := False;

    Size := Stream.Size - Stream.Position;
    ByteOrderMask[0] := 0;
    BytesRead := Stream.Read(ByteOrderMask[0],SizeOf(ByteOrderMask));

    // UTF16 LSB = Unicode LSB
    if (BytesRead >= 2) and (ByteOrderMask[0] = BOM_UTF16_LSB[0])
      and (ByteOrderMask[1] = BOM_UTF16_LSB[1]) then
    begin
      FSaveFormat := sfUTF16LSB;
      SetLength(SW, (Size - 2) div SizeOf(WideChar));
      Assert((Size and 1) <> 1,'Number of chars must be a multiple of 2');
      if BytesRead > 2 then
      begin
        System.Move(ByteOrderMask[2], SW[1], BytesRead-2); // max 4 bytes = 2 widechars
        if Size > BytesRead then
          // first 2 chars (maximum) were copied by System.Move
          Stream.Read(SW[3], Size-BytesRead);
      end;
      SetText(SW);
      Loaded := True;
    end;

    // UTF16 MSB = Unicode MSB
    if (BytesRead >= 2) and (ByteOrderMask[0] = BOM_UTF16_MSB[0])
      and (ByteOrderMask[1] = BOM_UTF16_MSB[1]) then
    begin
      FSaveFormat := sfUTF16MSB;
      SetLength(SW, (Size - 2) div SizeOf(WideChar));
      Assert((Size and 1) <> 1,'Number of chars must be a multiple of 2');
      if BytesRead > 2 then
      begin
        System.Move(ByteOrderMask[2],SW[1],BytesRead-2); // max 4 bytes = 2 widechars
        if Size > BytesRead then
          // first 2 chars (maximum) were copied by System.Move
          Stream.Read(SW[3], Size-BytesRead);
        StrSwapByteOrder(PWideChar(SW));
      end;
      SetText(SW);
      Loaded := True;
    end;

    // UTF8
    if (BytesRead >= 3) and (ByteOrderMask[0] = BOM_UTF8[0])
      and (ByteOrderMask[1] = BOM_UTF8[1]) and (ByteOrderMask[2] = BOM_UTF8[2]) then
    begin
      FSaveFormat := sfUTF8;
      SetLength(SA, (Size-3) div SizeOf(AnsiChar));
      if BytesRead > 3 then
      begin
        System.Move(ByteOrderMask[3],SA[1],BytesRead-3); // max 3 bytes = 3 chars
        if Size > BytesRead then
          // first 3 chars were copied by System.Move
          Stream.Read(SA[4], Size-BytesRead);
        SW := UTF8ToWideString(SA);
      end;
      SetText(SW);
      Loaded := True;
    end;

    // default case (Ansi)
    if not Loaded then
    begin
      FSaveFormat := sfAnsi;
      SetLength(SA, Size div SizeOf(AnsiChar));
      if BytesRead > 0 then
      begin
        System.Move(ByteOrderMask[0], SA[1], BytesRead); // max 6 bytes = 6 chars
        if Size > BytesRead then
          Stream.Read(SA[7], Size-BytesRead); // first 6 chars were copied by System.Move
      end;
      SetText(StringToWideStringEx(SA, CodePageFromLocale(FLanguage)));
    end;
  finally
    EndUpdate;
  end;
end;

procedure TWideStrings.Move(CurIndex, NewIndex: Integer);
var
  TempObject: TObject;
  TempString: WideString;
begin
  if CurIndex <> NewIndex then
  begin
    BeginUpdate;
    try
      TempString := Get(CurIndex);
      TempObject := GetObject(CurIndex);
      Delete(CurIndex);
      InsertObject(NewIndex, TempString, TempObject);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TWideStrings.ReadData(Reader: TReader);
begin
  case Reader.NextValue of
    vaLString, vaString:
      SetText(Reader.ReadString);
  else
    SetText(Reader.{$IFDEF RTL240_UP}ReadString{$ELSE}ReadWideString{$ENDIF});
  end;
end;

procedure TWideStrings.SaveToFile(const FileName: TFileName);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TWideStrings.SaveToStream(Stream: TStream; WithBOM: Boolean = True);
// Saves the currently loaded text into the given stream. WithBOM determines whether to write a
// byte order mark or not. Note: when saved as ANSI text there will never be a BOM.
var
  SW: WideString;
  SA: AnsiString;
  Allowed: Boolean;
  Run: PWideChar;
begin
  // The application can decide in which format to save the content.
  // If FSaveUnicode is False then all strings are saved in standard ANSI format
  // which is also loadable by TStrings but you should be aware that all Unicode
  // strings are then converted to ANSI based on the current system locale.
  // An extra event is supplied to ask the user about the potential loss of
  // information when converting Unicode to ANSI strings.
  SW := GetTextStr;
  Allowed := True;
  FSaved := False; // be pessimistic
  // A check for potential information loss makes only sense if the application has
  // set an event to be used as call back to ask about the conversion.
  if (FSaveFormat = sfAnsi) and Assigned(FOnConfirmConversion) then
  begin
    // application requests to save only ANSI characters, so check the text and
    // call back in case information could be lost
    Run := PWideChar(SW);
    // only ask if there's at least one Unicode character in the text
    while (Run^>#0) and (run^<=#255) do
      Inc(Run);
    // Note: The application can still set FSaveUnicode to True in the callback.
    if Run^ <> WideNull then
      DoConfirmConversion(Allowed);
  end;

  if Allowed then
  begin
    // only save if allowed
    case SaveFormat of
      sfUTF16LSB :
        begin
          if WithBOM then
            Stream.WriteBuffer(BOM_UTF16_LSB[0],SizeOf(BOM_UTF16_LSB));
          if Length(SW) > 0 then
            Stream.WriteBuffer(SW[1],Length(SW)*SizeOf(UTF16));
          FSaved := True;
        end;
      sfUTF16MSB :
        begin
          if WithBOM then
            Stream.WriteBuffer(BOM_UTF16_MSB[0],SizeOf(BOM_UTF16_MSB));
          if Length(SW) > 0 then
          begin
            StrSwapByteOrder(PWideChar(SW));
            Stream.WriteBuffer(SW[1],Length(SW)*SizeOf(UTF16));
          end;
          FSaved := True;
        end;
      sfUTF8 :
        begin
          if WithBOM then
            Stream.WriteBuffer(BOM_UTF8[0],SizeOf(BOM_UTF8));
          if Length(SW) > 0 then
          begin
            SA := WideStringToUTF8(SW);
            Stream.WriteBuffer(SA[1],Length(SA)*SizeOf(UTF8));
          end;
          FSaved := True;
        end;
      sfAnsi :
        begin
          if Length(SW) > 0 then
          begin
            SA := WideStringToStringEx(SW,CodePageFromLocale(FLanguage));
            Stream.WriteBuffer(SA[1],Length(SA)*SizeOf(AnsiChar));
          end;
          FSaved := True;
        end;
    end;
  end;
end;

procedure TWideStrings.SetCapacity(NewCapacity: Integer);
begin
  // do nothing - descendants may optionally implement this method
end;

procedure TWideStrings.SetCommaText(const Value: WideString);
var
  P, P1: PWideChar;
  S: WideString;
begin
  BeginUpdate;
  try
    Clear;
    P := PWideChar(Value);
    while (P^ >= #1) and (P^ <= WideSpace) do
      Inc(P);
    while P^ <> WideNull do
    begin
      if P^ = '"' then
        S := WideExtractQuotedStr(P, '"')
      else
      begin
        P1 := P;
        while (P^ > WideSpace) and (P^ <> ',') do
          Inc(P);
        SetString(S, P1, P - P1);
      end;
      Add(S);

      while (P^ >= #1) and (P^ <= WideSpace) do
        Inc(P);
      if P^ = ',' then
      begin
        repeat
          Inc(P);
        until not ((P^ >= #1) and (P^ <= WideSpace));
      end;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TWideStrings.SetText(const Value: WideString);
var
  Head,
  Tail: PWideChar;
  S: WideString;
begin
  BeginUpdate;
  try
    Clear;
    Head := PWideChar(Value);
    while Head^ <> WideNull do
    begin
      Tail := Head;
      while (Tail^ <> WideNull) and (Tail^ <> WideLineFeed) and (Tail^ <> WideCarriageReturn) and
        (Tail^ <> WideVerticalTab) and (Tail^ <> WideFormFeed) and (Tail^ <> WideLineSeparator) and
        (Tail^ <> WideParagraphSeparator) do
        Inc(Tail);
      SetString(S, Head, Tail - Head);
      Add(S);
      Head := Tail;
      if Head^ <> WideNull then
      begin
        Inc(Head);
        if (Tail^ = WideCarriageReturn) and (Head^ = WideLineFeed) then
          Inc(Head);
      end;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TWideStrings.SetUpdateState(Updating: Boolean);
begin
end;

procedure TWideStrings.SetNormalizationForm(const Value: TNormalizationForm);
var
  I: Integer;
begin
  if FNormalizationForm <> Value then
  begin
    FNormalizationForm := Value;
    if FNormalizationForm <> nfNone then
    begin
      // renormalize all strings according to the new form
      for I := 0 to GetCount - 1 do
        Put(I, WideNormalize(Get(I), FNormalizationForm));
    end;
  end;
end;

procedure TWideStrings.SetValue(const Name, Value: WideString);
var
  I : Integer;
begin
  I := IndexOfName(Name);
  if Value <> '' then
  begin
    if I < 0 then
      I := Add('');
    Put(I, Name + '=' + Value);
  end
  else
  begin
    if I >= 0 then
      Delete(I);
  end;
end;

procedure TWideStrings.WriteData(Writer: TWriter);
begin
  Writer.{$IFDEF RTL240_UP}WriteString{$ELSE}WriteWideString{$ENDIF}(GetTextStr);
end;

//=== { TWideStringList } ====================================================

destructor TWideStringList.Destroy;
begin
  FOnChange := nil;
  FOnChanging := nil;
  Clear;
  inherited Destroy;
end;

function TWideStringList.Add(const S: WideString): Integer;
begin
  if not Sorted then
    Result := FCount
  else
  begin
    if Find(S, Result) then
    begin
      case Duplicates of
        dupIgnore:
          Exit;
        dupError:
          Error(SDuplicateString, 0);
      end;
    end;
  end;
  InsertItem(Result, S);
end;

procedure TWideStringList.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TWideStringList.Changing;
begin
  if (FUpdateCount = 0) and Assigned(FOnChanging) then
    FOnChanging(Self);
end;

procedure TWideStringList.Clear;
{$IFDEF OWN_WIDESTRING_MEMMGR}
var
  I: Integer;
{$ENDIF OWN_WIDESTRING_MEMMGR}
begin
  if FCount <> 0 then
  begin
    Changing;
    {$IFDEF OWN_WIDESTRING_MEMMGR}
    for I := 0 to FCount - 1 do
      with FList[I] do
        if TDynWideCharArray(FString) <> nil then
          TDynWideCharArray(FString) := nil;
    {$ENDIF OWN_WIDESTRING_MEMMGR}
    // this will automatically finalize the array
    FList := nil;
    FCount := 0;
    SetCapacity(0);
    Changed;
  end;
end;

procedure TWideStringList.Delete(Index: Integer);
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    Error(SListIndexError, Index);
  Changing;

  {$IFDEF OWN_WIDESTRING_MEMMGR}
  SetListString(Index, '');
  {$ELSE ~OWN_WIDESTRING_MEMMGR}
  FList[Index].FString := '';
  {$ENDIF ~OWN_WIDESTRING_MEMMGR}
  Dec(FCount);
  if Index < FCount then
  begin
    System.Move(FList[Index + 1], FList[Index], (FCount - Index) * SizeOf(TWideStringItem));
    Pointer(FList[FCount].FString) := nil; // avoid freeing the string, the address is now used in another element
  end;
  Changed;
end;

procedure TWideStringList.Exchange(Index1, Index2: Integer);
begin
  if Cardinal(Index1) >= Cardinal(FCount) then
    Error(SListIndexError, Index1);
  if Cardinal(Index2) >= Cardinal(FCount) then
    Error(SListIndexError, Index2);
  Changing;
  ExchangeItems(Index1, Index2);
  Changed;
end;

procedure TWideStringList.ExchangeItems(Index1, Index2: Integer);
var
  Temp: TWideStringItem;
begin
  Temp := FList[Index1];
  FList[Index1] := FList[Index2];
  FList[Index2] := Temp;
end;

function TWideStringList.Find(const S: WideString; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
  NormString: WideString;
begin
  Result := False;
  NormString := WideNormalize(S, FNormalizationForm);
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := WideCompareText(FList[I].FString, NormString, FLanguage);
    if C < 0 then
      L := I+1
    else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        if Duplicates <> dupAccept then
          L := I;
      end;
    end;
  end;
  Index := L;
end;

function TWideStringList.Get(Index: Integer): WideString;
{$IFDEF OWN_WIDESTRING_MEMMGR}
var
  Len: Integer;
{$ENDIF OWN_WIDESTRING_MEMMGR}
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    Error(SListIndexError, Index);
  {$IFDEF OWN_WIDESTRING_MEMMGR}
  with FList[Index] do
  begin
    Len := Length(TDynWideCharArray(FString));
    if Len > 0 then
    begin
      SetLength(Result, Len - 1); // exclude #0
      if Result <> '' then
        System.Move(FString^, Result[1], Len * SizeOf(WideChar));
    end
    else
      Result := '';
  end;
  {$ELSE ~OWN_WIDESTRING_MEMMGR}
  Result := FList[Index].FString;
  {$ENDIF ~OWN_WIDESTRING_MEMMGR}
end;

function TWideStringList.GetCapacity: Integer;
begin
  Result := Length(FList);
end;

function TWideStringList.GetCount: Integer;
begin
  Result := FCount;
end;

function TWideStringList.GetObject(Index: Integer): TObject;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    Error(SListIndexError, Index);
  Result := FList[Index].FObject;
end;

procedure TWideStringList.Grow;
var
  Delta,
  Len: Integer;
begin
  Len := Length(FList);
  if Len > 64 then
    Delta := Len div 4
  else
  begin
    if Len > 8 then
      Delta := 16
    else
      Delta := 4;
  end;
  SetCapacity(Len + Delta);
end;

function TWideStringList.IndexOf(const S: WideString): Integer;
begin
  if not Sorted then
    Result := inherited IndexOf(S)
  else
    if not Find(S, Result) then
      Result := -1;
end;

procedure TWideStringList.Insert(Index: Integer; const S: WideString);
begin
  if Sorted then
    Error(SSortedListError, 0);
  if Cardinal(Index) > Cardinal(FCount) then
    Error(SListIndexError, Index);
  InsertItem(Index, S);
end;

{$IFDEF OWN_WIDESTRING_MEMMGR}
procedure TWideStringList.SetListString(Index: Integer; const S: WideString);
var
  Len: Integer;
  A: TDynWideCharArray;
begin
  with FList[Index] do
  begin
    Pointer(A) := TDynWideCharArray(FString);
    if A <> nil then
      A := nil; // free memory

    Len := Length(S);
    if Len > 0 then
    begin
      SetLength(A, Len + 1); // include #0
      System.Move(S[1], A[0], Len * SizeOf(WideChar));
      A[Len] := #0;
    end;

    FString := PWideChar(A);
    Pointer(A) := nil; // do not release the array on procedure exit
  end;
end;
{$ENDIF OWN_WIDESTRING_MEMMGR}

procedure TWideStringList.InsertItem(Index: Integer; const S: WideString);
begin
  Changing;
  if FCount = Length(FList) then
    Grow;
  if Index < FCount then
    System.Move(FList[Index], FList[Index + 1], (FCount - Index) * SizeOf(TWideStringItem));
  with FList[Index] do
  begin
    Pointer(FString) := nil; // avoid freeing the string, the address is now used in another element
    FObject := nil;
    if (FNormalizationForm <> nfNone) and (Length(S) > 0) then
    {$IFDEF OWN_WIDESTRING_MEMMGR}
      SetListString(Index, WideNormalize(S, FNormalizationForm))
    else
      SetListString(Index, S);
    {$ELSE ~OWN_WIDESTRING_MEMMGR}
      FString := WideNormalize(S, FNormalizationForm)
    else
      FString := S;
    {$ENDIF ~OWN_WIDESTRING_MEMMGR}
  end;
  Inc(FCount);
  Changed;
end;

procedure TWideStringList.Put(Index: Integer; const S: WideString);
begin
  if Sorted then
    Error(SSortedListError, 0);
  if Cardinal(Index) >= Cardinal(FCount) then
    Error(SListIndexError, Index);
  Changing;

  if (FNormalizationForm <> nfNone) and (Length(S) > 0) then
  {$IFDEF OWN_WIDESTRING_MEMMGR}
    SetListString(Index, WideNormalize(S, FNormalizationForm))
  else
    SetListString(Index, S);
  {$ELSE ~OWN_WIDESTRING_MEMMGR}
    FList[Index].FString := WideNormalize(S, FNormalizationForm)
  else
    FList[Index].FString := S;
  {$ENDIF ~OWN_WIDESTRING_MEMMGR}
  Changed;
end;

procedure TWideStringList.PutObject(Index: Integer; AObject: TObject);
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    Error(SListIndexError, Index);
  Changing;
  FList[Index].FObject := AObject;
  Changed;
end;

procedure TWideStringList.QuickSort(L, R: Integer);
var
  I, J: Integer;
  P: WideString;
begin
  repeat
    I := L;
    J := R;
    P := FList[(L + R) shr 1].FString;
    repeat
      while WideCompareText(FList[I].FString, P, FLanguage) < 0 do
        Inc(I);
      while WideCompareText(FList[J].FString, P, FLanguage) > 0 do
        Dec(J);
      if I <= J then
      begin
        ExchangeItems(I, J);
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(L, J);
    L := I;
  until I >= R;
end;

procedure TWideStringList.SetCapacity(NewCapacity: Integer);
begin
  SetLength(FList, NewCapacity);
  if NewCapacity < FCount then
    FCount := NewCapacity;
end;

procedure TWideStringList.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if Value then
      Sort;
    FSorted := Value;
  end;
end;

procedure TWideStringList.SetUpdateState(Updating: Boolean);
begin
  if Updating then
    Changing
  else
    Changed;
end;

procedure TWideStringList.Sort;
begin
  if not Sorted and (FCount > 1) then
  begin
    Changing;
    QuickSort(0, FCount - 1);
    Changed;
  end;
end;

procedure TWideStringList.SetLanguage(Value: LCID);
begin
  inherited SetLanguage(Value);
  if Sorted then
    Sort;
end;

{$ENDIF ~UNICODE_RTL_DATABASE}

// exchanges in each character of the given string the low order and high order
// byte to go from LSB to MSB and vice versa.
// EAX contains address of string

function WideAdjustLineBreaks(const S: WideString): WideString;
var
  Source,
  SourceEnd,
  Dest: PWideChar;
begin
  Source := Pointer(S);
  SourceEnd := Source + Length(S);

  Source := Pointer(S);
  SetString(Result, nil, SourceEnd - Source);
  Dest := Pointer(Result);

  while Source < SourceEnd do
  begin
    case Source^ of
      WideLineFeed:
        begin
          Dest^ := WideLineSeparator;
          Inc(Dest);
          Inc(Source);
        end;
      WideCarriageReturn:
        begin
          Dest^ := WideLineSeparator;
          Inc(Dest);
          Inc(Source);
          if Source^ = WideLineFeed then
            Inc(Source);
        end;
    else
      Dest^ := Source^;
      Inc(Dest);
      Inc(Source);
    end;
  end;

  SetLength(Result, (TJclAddr(Dest) - TJclAddr(Result)) div 2);
end;

function WideQuotedStr(const S: WideString; Quote: WideChar): WideString;
// works like QuotedStr from SysUtils.pas but can insert any quotation character
var
  P, Src,
  Dest: PWideChar;
  AddCount: SizeInt;
begin
  AddCount := 0;
  P := StrScanW(PWideChar(S), Quote);
  while (P <> nil) do
  begin
    Inc(P);
    Inc(AddCount);
    P := StrScanW(P, Quote);
  end;

  if AddCount = 0 then
    Result := Quote + S + Quote
  else
  begin
    SetLength(Result, Length(S) + AddCount + 2);
    Dest := PWideChar(Result);
    Dest^ := Quote;
    Inc(Dest);
    Src := PWideChar(S);
    P := StrScanW(Src, Quote);
    repeat
      Inc(P);
      Move(Src^, Dest^, 2 * (P - Src));
      Inc(Dest, P - Src);
      Dest^ := Quote;
      Inc(Dest);
      Src := P;
      P := StrScanW(Src, Quote);
    until P = nil;
    P := StrEndW(Src);
    Move(Src^, Dest^, 2 * (P - Src));
    Inc(Dest, P - Src);
    Dest^ := Quote;
  end;
end;

function WideExtractQuotedStr(var Src: PWideChar; Quote: WideChar): WideString;
// extracts a string enclosed in quote characters given by Quote
var
  P, Dest: PWideChar;
  DropCount: SizeInt;
begin
  Result := '';
  if (Src = nil) or (Src^ <> Quote) then
    Exit;

  Inc(Src);
  DropCount := 1;
  P := Src;
  Src := StrScanW(Src, Quote);

  while Src <> nil do   // count adjacent pairs of quote chars
  begin
    Inc(Src);
    if Src^ <> Quote then
      Break;
    Inc(Src);
    Inc(DropCount);
    Src := StrScanW(Src, Quote);
  end;

  if Src = nil then
    Src := StrEndW(P);
  if (Src - P) <= 1 then
    Exit;

  if DropCount = 1 then
    SetString(Result, P, Src - P - 1)
  else
  begin
    SetLength(Result, Src - P - DropCount);
    Dest := PWideChar(Result);
    Src := StrScanW(P, Quote);
    while Src <> nil do
    begin
      Inc(Src);
      if Src^ <> Quote then
        Break;
      Move(P^, Dest^, 2 * (Src - P));
      Inc(Dest, Src - P);
      Inc(Src);
      P := Src;
      Src := StrScanW(Src, Quote);
    end;
    if Src = nil then
      Src := StrEndW(P);
    Move(P^, Dest^, 2 * (Src - P - 1));
  end;
end;

function WideStringOfChar(C: WideChar; Count: SizeInt): WideString;
// returns a string of Count characters filled with C
var
  I: SizeInt;
begin
  SetLength(Result, Count);
  for I := 1 to Count do
    Result[I] := C;
end;

function WideTrim(const S: WideString): WideString;
var
  I, L: SizeInt;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (UnicodeIsWhiteSpace(UCS4(S[I])) or UnicodeIsControl(UCS4(S[I]))) do
    Inc(I);
  if I > L then
    Result := ''
  else
  begin
    while UnicodeIsWhiteSpace(UCS4(S[L])) or UnicodeIsControl(UCS4(S[L])) do
      Dec(L);
    Result := Copy(S, I, L - I + 1);
  end;
end;

function WideTrimLeft(const S: WideString): WideString;
var
  I, L: SizeInt;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (UnicodeIsWhiteSpace(UCS4(S[I])) or UnicodeIsControl(UCS4(S[I]))) do
    Inc(I);
  Result := Copy(S, I, Maxint);
end;

function WideTrimRight(const S: WideString): WideString;
var
  I: SizeInt;
begin
  I := Length(S);
  while (I > 0) and (UnicodeIsWhiteSpace(UCS4(S[I])) or UnicodeIsControl(UCS4(S[I]))) do
    Dec(I);
  Result := Copy(S, 1, I);
end;

// returns the index of character Ch in S, starts searching at index Index
// Note: This is a quick memory search. No attempt is made to interpret either
// the given charcter nor the string (ligatures, modifiers, surrogates etc.)
// Code from Azret Botash.

function WideCharPos(const S: WideString; const Ch: WideChar; const Index: SizeInt): SizeInt;
var
  P, R: PWideChar;
begin
  if (Index > 0) and (Index <= Length(S)) then
  begin
    P := PWideChar(@S[Index]);
    R := StrScanW(P, Ch);
    if R <> nil then
      Result := R - P + Index
    else
      Result := 0;
  end
  else
    Result := 0;
end;

{$IFNDEF UNICODE_RTL_DATABASE}

function WideComposeHangul(const Source: WideString): WideString;
var
  Len: SizeInt;
  Ch, Last: WideChar;
  I: SizeInt;
  LIndex, VIndex,
  SIndex, TIndex: SizeInt;
begin
  Result := '';
  Len := Length(Source);
  if Len > 0 then
  begin
    Last := Source[1];
    Result := Last;

    for I := 2 to Len do
    begin
      Ch := Source[I];

      // 1. check to see if two current characters are L and V
      LIndex := Word(Last) - LBase;
      if (0 <= LIndex) and (LIndex < LCount) then
      begin
        VIndex := Word(Ch) - VBase;
        if (0 <= VIndex) and (VIndex < VCount) then
        begin
          // make syllable of form LV
          Last := WideChar((SBase + (LIndex * VCount + VIndex) * TCount));
          Result[Length(Result)] := Last; // reset last
          Continue; // discard Ch
        end;
      end;

      // 2. check to see if two current characters are LV and T
      SIndex := Word(Last) - SBase;
      if (0 <= SIndex) and (SIndex < SCount) and ((SIndex mod TCount) = 0) then
      begin
        TIndex := Word(Ch) - TBase;
        if (0 <= TIndex) and (TIndex <= TCount) then
        begin
          // make syllable of form LVT
          Inc(Word(Last), TIndex);
          Result[Length(Result)] := Last; // reset last
          Continue; // discard Ch
        end;
      end;

      // if neither case was true, just add the character
      Last := Ch;
      Result := Result + Ch;
    end;
  end;
end;

// Returns canonical composition of characters in S.

function WideCompose(const S: WideString; Compatible: Boolean): WideString;
var
  Buffer: array of UCS4;
  LastInPos, InPos, OutPos, BufferSize, NbProcessed: SizeInt;
  Composite: UCS4;
begin
  // Set an arbitrary length for the result. This is automatically done when checking
  // for hangul composition.
  Result := WideComposeHangul(S);

  if Result = '' then
    Exit;

  if Compositions = nil then
    LoadCompositionData;

  LastInPos := Length(Result);
  if LastInPos > MaxCompositionSize then
    SetLength(Buffer, MaxCompositionSize)
  else
    SetLength(Buffer, LastInPos);

  BufferSize := 0;
  InPos := 0;
  OutPos := 0;

  while (InPos < LastInPos) or (BufferSize > 0) do
  begin
    // fill buffer from input

    while BufferSize < Length(Buffer) do
    begin
      if InPos < LastInPos then
      begin
        Inc(InPos);
        Buffer[BufferSize] := UCS4(Result[InPos]);
        Inc(BufferSize);
      end
      else
        SetLength(Buffer, BufferSize);
    end;

    if Length(Buffer) = 0 then
      Break;

    NbProcessed := UnicodeCompose(Buffer, Composite, Compatible);
    if NbProcessed = 0 then
      Break;

    if BufferSize > NbProcessed then
      Move(Buffer[NbProcessed], Buffer[0], (BufferSize - NbProcessed) * SizeOf(UCS4));
    Dec(BufferSize, NbProcessed);

    Inc(OutPos);
    Result[OutPos] := UCS2(Composite);
  end;
  // since we have likely shortened the source string we have to set the correct length on exit
  SetLength(Result, OutPos);
end;

function WideCompose(const S: WideString; Tags: TCompatibilityFormattingTags): WideString;
var
  Buffer: array of UCS4;
  LastInPos, InPos, OutPos, BufferSize, NbProcessed: SizeInt;
  Composite: UCS4;
begin
  // Set an arbitrary length for the result. This is automatically done when checking
  // for hangul composition.
  Result := WideComposeHangul(S);

  if Result = '' then
    Exit;

  if Compositions = nil then
    LoadCompositionData;

  LastInPos := Length(Result);
  if LastInPos > MaxCompositionSize then
    SetLength(Buffer, MaxCompositionSize)
  else
    SetLength(Buffer, LastInPos);

  BufferSize := 0;
  InPos := 0;
  OutPos := 0;

  while (InPos < LastInPos) or (BufferSize > 0) do
  begin
    // fill buffer from input

    while BufferSize < Length(Buffer) do
    begin
      if InPos < LastInPos then
      begin
        Inc(InPos);
        Buffer[BufferSize] := UCS4(Result[InPos]);
        Inc(BufferSize);
      end
      else
        SetLength(Buffer, BufferSize);
    end;

    if Length(Buffer) = 0 then
      Break;

    NbProcessed := UnicodeCompose(Buffer, Composite, Tags);
    if NbProcessed = 0 then
      Break;

    if BufferSize > NbProcessed then
      Move(Buffer[NbProcessed], Buffer[0], (BufferSize - NbProcessed) * SizeOf(UCS4));
    Dec(BufferSize, NbProcessed);

    Inc(OutPos);
    Result[OutPos] := UCS2(Composite);
  end;
  // since we have likely shortened the source string we have to set the correct length on exit
  SetLength(Result, OutPos);
end;

procedure FixCanonical(var S: WideString);
// Examines S and reorders all combining marks in the string so that they are in canonical order.
var
  I: SizeInt;
  Temp: WideChar;
  CurrentClass,
  LastClass: Cardinal;
begin
  I := Length(S);
  if I > 1 then
  begin
    CurrentClass := CanonicalCombiningClass(UCS4(S[I]));
    repeat
      Dec(I);
      LastClass := CurrentClass;
      CurrentClass := CanonicalCombiningClass(UCS4(S[I]));

      // A swap is presumed to be rare (and a double-swap very rare),
      // so don't worry about efficiency here.
      if (CurrentClass > LastClass) and (LastClass > 0) then
      begin
        // swap characters
        Temp := S[I];
        S[I] := S[I + 1];
        S[I + 1] := Temp;

        // if not at end, backup (one further, to compensate for loop)
        if I < Length(S) - 1 then
          Inc(I, 2);
        // reset type, since we swapped.
        CurrentClass := CanonicalCombiningClass(UCS4(S[I]));
      end;
    until I = 1;
  end;
end;

function WideDecompose(const S: WideString; Compatible: Boolean): WideString;
// returns a string with all characters of S but decomposed, e.g.  is returned as E^ etc.
var
  I, J: SizeInt;
  Decomp: TUCS4Array;
begin
  Result := '';
  Decomp := nil;

  // iterate through each source code point
  for I := 1 to Length(S) do
  begin
    Decomp := UnicodeDecompose(UCS4(S[I]), Compatible);
    if Decomp = nil then
      Result := Result + S[I]
    else
      for J := 0 to High(Decomp) do
        Result := Result + WideChar(Decomp[J]);
  end;

  // combining marks must be sorted according to their canonical combining class
  FixCanonical(Result);
end;

function WideDecompose(const S: WideString; Tags: TCompatibilityFormattingTags): WideString;
// returns a string with all characters of S but decomposed, e.g.  is returned as E^ etc.
var
  I, J: SizeInt;
  Decomp: TUCS4Array;
begin
  Result := '';
  Decomp := nil;

  // iterate through each source code point
  for I := 1 to Length(S) do
  begin
    Decomp := UnicodeDecompose(UCS4(S[I]), Tags);
    if Decomp = nil then
      Result := Result + S[I]
    else
      for J := 0 to High(Decomp) do
        Result := Result + WideChar(Decomp[J]);
  end;

  // combining marks must be sorted according to their canonical combining class
  FixCanonical(Result);
end;

function WideNormalize(const S: WideString; Form: TNormalizationForm): WideString;
var
  Temp: WideString;
  Compatible: Boolean;
begin
  Result := S;

  if Form = nfNone then
    Exit; // No normalization needed.

  Compatible := Form in [nfKC, nfKD];
  if Form in [nfD, nfKD] then
    Result := WideDecompose(S, Compatible)
  else
  begin
    Temp := WideDecompose(S, Compatible);
    Result := WideCompose(Temp, Compatible);
  end;
end;

{$ENDIF ~UNICODE_RTL_DATABASE}

function WideSameText(const Str1, Str2: WideString): Boolean;
// Compares both strings case-insensitively and returns True if both are equal, otherwise False is returned.
begin
  Result := Length(Str1) = Length(Str2);
  if Result then
    Result := StrICompW(PWideChar(Str1), PWideChar(Str2)) = 0;
end;

//----------------- general purpose case mapping ---------------------------------------------------

{$IFNDEF UNICODE_RTL_DATABASE}

function WideCaseConvert(C: WideChar; CaseType: TCaseType): WideString;
var
  I, RPos: SizeInt;
  Mapping: TUCS4Array;
begin
  SetLength(Mapping, 0);
  if not CaseLookup(UCS4(C), CaseType, Mapping) then
    Result := C
  else
  begin
    SetLength(Result, 2 * Length(Mapping));
    RPos := 1;
    for I := Low(Mapping) to High(Mapping) do
      UTF16SetNextChar(Result, RPos, Mapping[I]);
    if RPos > 0 then
      SetLength(Result, RPos - 1)
    else
      raise EJclUnexpectedEOSequenceError.Create;
  end;
end;

function WideCaseConvert(const S: WideString; CaseType: TCaseType): WideString;
var
  SLen, RLen, SPos, RPos, K, MapLen: SizeInt;
  Code: UCS4;
  Mapping: TUCS4Array;
begin
  SetLength(Mapping, 0);
  SLen := Length(S);
  RLen := SLen;
  SetLength(Result, RLen);
  SPos := 1;
  RPos := 1;
  while (SPos > 0) and (SPos <= SLen) do
  begin
    Code := UTF16GetNextChar(S, SPos);
    if SPos = -1 then
      raise EJclUnexpectedEOSequenceError.Create;

    if CaseLookup(Code, CaseType, Mapping) then
    begin
      MapLen:= Length(Mapping);
      if MapLen = 1 then
        Code := Mapping[0];
    end
    else
      MapLen := 1;

    if MapLen = 1 then
    begin
      if not UTF16SetNextChar(Result, RPos, Code) then
      begin
        Inc(RLen, SLen);
        SetLength(Result, RLen);
        UTF16SetNextChar(Result, RPos, Code);
      end;
    end
    else
    begin
      for K := Low(Mapping) to High(Mapping) do
        if not UTF16SetNextChar(Result, RPos, Code) then
      begin
        Inc(RLen, SLen);
        SetLength(Result, RLen);
        UTF16SetNextChar(Result, RPos, Code);
      end;
    end;
  end;
  if RPos > 0 then
    SetLength(Result, RPos - 1)
  else
    raise EJclUnexpectedEOSequenceError.Create;
end;

// Note that most of the assigned code points don't have a case mapping and are therefore
// returned as they are. Other code points, however, might be converted into several characters
// like the german  (eszett) whose upper case mapping is SS.

function WideCaseFolding(C: WideChar): WideString;
// Special case folding function to map a string to either its lower case or
// to special cases. This can be used for case-insensitive comparation.
begin
  Result:= WideCaseConvert(C, ctFold);
end;

function WideCaseFolding(const S: WideString): WideString;
begin
  Result:= WideCaseConvert(S, ctFold);
end;

{$ENDIF ~UNICODE_RTL_DATABASE}

function WideLowerCase(C: WideChar): WideString;
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.ToLower(C);
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result:= WideCaseConvert(C, ctLower);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function WideLowerCase(const S: WideString): WideString;
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.ToLower(S);
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result:= WideCaseConvert(S, ctLower);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

{$IFNDEF UNICODE_RTL_DATABASE}

function WideTitleCase(C: WideChar): WideString;
begin
  Result:= WideCaseConvert(C, ctTitle);
end;

function WideTitleCase(const S: WideString): WideString;
begin
  Result:= WideCaseConvert(S, ctTitle);
end;

{$ENDIF ~UNICODE_RTL_DATABASE}

function WideUpperCase(C: WideChar): WideString;
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.ToUpper(C);
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result:= WideCaseConvert(C, ctUpper);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function WideUpperCase(const S: WideString): WideString;
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.ToUpper(S);
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result:= WideCaseConvert(S, ctUpper);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

//----------------- character test routines --------------------------------------------------------

function UnicodeIsAlpha(C: UCS4): Boolean; // Is the character alphabetic?
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.IsLetter(Chr(C));
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := CategoryLookup(C, ClassLetter);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function UnicodeIsDigit(C: UCS4): Boolean; // Is the character a digit?
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.IsDigit(Chr(C));
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := CategoryLookup(C, [ccNumberDecimalDigit]);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function UnicodeIsAlphaNum(C: UCS4): Boolean; // Is the character alphabetic or a number?
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.IsLetterOrDigit(Chr(C));
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := CategoryLookup(C, ClassLetter + [ccNumberDecimalDigit]);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function UnicodeIsNumberOther(C: UCS4): Boolean;
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.GetUnicodeCategory(Chr(C)) = TUnicodeCategory.ucOtherNumber;
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := CategoryLookup(C, [ccNumberOther]);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function UnicodeIsCased(C: UCS4): Boolean;
// Is the character a "cased" character, i.e. either lower case, title case or upper case
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.GetUnicodeCategory(Chr(C)) in
    [TUnicodeCategory.ucLowercaseLetter, TUnicodeCategory.ucTitlecaseLetter, TUnicodeCategory.ucUppercaseLetter];
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := CategoryLookup(C, [ccLetterLowercase, ccLetterTitleCase, ccLetterUppercase]);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function UnicodeIsControl(C: UCS4): Boolean;
// Is the character a control character?
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.GetUnicodeCategory(Chr(C)) in
    [TUnicodeCategory.ucControl, TUnicodeCategory.ucFormat];
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := CategoryLookup(C, [ccOtherControl, ccOtherFormat]);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function UnicodeIsSpace(C: UCS4): Boolean;
// Is the character a spacing character?
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.GetUnicodeCategory(Chr(C)) = TUnicodeCategory.ucSpaceSeparator;
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := CategoryLookup(C, ClassSpace);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function UnicodeIsWhiteSpace(C: UCS4): Boolean;
// Is the character a white space character (same as UnicodeIsSpace plus
// tabulator, new line etc.)?
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.IsWhiteSpace(Chr(C));
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := CategoryLookup(C, ClassSpace + [ccWhiteSpace, ccSegmentSeparator]);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function UnicodeIsBlank(C: UCS4): Boolean;
// Is the character a space separator?
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.GetUnicodeCategory(Chr(C)) = TUnicodeCategory.ucSpaceSeparator;
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := CategoryLookup(C, [ccSeparatorSpace]);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function UnicodeIsPunctuation(C: UCS4): Boolean;
// Is the character a punctuation mark?
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.GetUnicodeCategory(Chr(C)) in
    [TUnicodeCategory.ucConnectPunctuation, TUnicodeCategory.ucDashPunctuation,
     TUnicodeCategory.ucClosePunctuation, TUnicodeCategory.ucFinalPunctuation,
     TUnicodeCategory.ucInitialPunctuation, TUnicodeCategory.ucOtherPunctuation,
     TUnicodeCategory.ucOpenPunctuation];
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := CategoryLookup(C, ClassPunctuation);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function UnicodeIsGraph(C: UCS4): Boolean;
// Is the character graphical?
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.GetUnicodeCategory(Chr(C)) in
    [TUnicodeCategory.ucCombiningMark, TUnicodeCategory.ucEnclosingMark,
     TUnicodeCategory.ucNonSpacingMark,
     TUnicodeCategory.ucDecimalNumber, TUnicodeCategory.ucLetterNumber,
     TUnicodeCategory.ucOtherNumber,
     TUnicodeCategory.ucLowercaseLetter, TUnicodeCategory.ucModifierLetter,
     TUnicodeCategory.ucOtherLetter, TUnicodeCategory.ucTitlecaseLetter,
     TUnicodeCategory.ucUppercaseLetter,
     TUnicodeCategory.ucConnectPunctuation, TUnicodeCategory.ucDashPunctuation,
     TUnicodeCategory.ucClosePunctuation, TUnicodeCategory.ucFinalPunctuation,
     TUnicodeCategory.ucInitialPunctuation, TUnicodeCategory.ucOtherPunctuation,
     TUnicodeCategory.ucOpenPunctuation,
     TUnicodeCategory.ucCurrencySymbol, TUnicodeCategory.ucModifierSymbol,
     TUnicodeCategory.ucMathSymbol, TUnicodeCategory.ucOtherSymbol];
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := CategoryLookup(C, ClassMark + ClassNumber + ClassLetter + ClassPunctuation + ClassSymbol);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function UnicodeIsPrintable(C: UCS4): Boolean;
// Is the character printable?
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.GetUnicodeCategory(Chr(C)) in
    [TUnicodeCategory.ucCombiningMark, TUnicodeCategory.ucEnclosingMark,
     TUnicodeCategory.ucNonSpacingMark,
     TUnicodeCategory.ucDecimalNumber, TUnicodeCategory.ucLetterNumber,
     TUnicodeCategory.ucOtherNumber,
     TUnicodeCategory.ucLowercaseLetter, TUnicodeCategory.ucModifierLetter,
     TUnicodeCategory.ucOtherLetter, TUnicodeCategory.ucTitlecaseLetter,
     TUnicodeCategory.ucUppercaseLetter,
     TUnicodeCategory.ucConnectPunctuation, TUnicodeCategory.ucDashPunctuation,
     TUnicodeCategory.ucClosePunctuation, TUnicodeCategory.ucFinalPunctuation,
     TUnicodeCategory.ucInitialPunctuation, TUnicodeCategory.ucOtherPunctuation,
     TUnicodeCategory.ucOpenPunctuation,
     TUnicodeCategory.ucCurrencySymbol, TUnicodeCategory.ucModifierSymbol,
     TUnicodeCategory.ucMathSymbol, TUnicodeCategory.ucOtherSymbol,
     TUnicodeCategory.ucSpaceSeparator];
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := CategoryLookup(C, ClassMark + ClassNumber + ClassLetter + ClassPunctuation + ClassSymbol +
    [ccSeparatorSpace]);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function UnicodeIsUpper(C: UCS4): Boolean;
// Is the character already upper case?
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.GetUnicodeCategory(Chr(C)) = TUnicodeCategory.ucUppercaseLetter;
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := CategoryLookup(C, [ccLetterUppercase]);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function UnicodeIsLower(C: UCS4): Boolean;
// Is the character already lower case?
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.GetUnicodeCategory(Chr(C)) = TUnicodeCategory.ucLowercaseLetter;
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := CategoryLookup(C, [ccLetterLowercase]);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function UnicodeIsTitle(C: UCS4): Boolean;
// Is the character already title case?
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.GetUnicodeCategory(Chr(C)) = TUnicodeCategory.ucTitlecaseLetter;
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := CategoryLookup(C, [ccLetterTitlecase]);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

{$IFNDEF UNICODE_RTL_DATABASE}
function UnicodeIsHexDigit(C: UCS4): Boolean;
// Is the character a hex digit?
begin
  Result := CategoryLookup(C, [ccHexDigit]);
end;
{$ENDIF ~UNICODE_RTL_DATABASE}

function UnicodeIsIsoControl(C: UCS4): Boolean;
// Is the character a C0 control character (< 32)?
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.GetUnicodeCategory(Chr(C)) = TUnicodeCategory.ucControl;
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := CategoryLookup(C, [ccOtherControl]);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function UnicodeIsFormatControl(C: UCS4): Boolean;
// Is the character a format control character?
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.GetUnicodeCategory(Chr(C)) = TUnicodeCategory.ucFormat;
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := CategoryLookup(C, [ccOtherFormat]);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function UnicodeIsSymbol(C: UCS4): Boolean;
// Is the character a symbol?
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.GetUnicodeCategory(Chr(C)) in
    [TUnicodeCategory.ucCurrencySymbol, TUnicodeCategory.ucModifierSymbol,
     TUnicodeCategory.ucMathSymbol, TUnicodeCategory.ucOtherSymbol];
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := CategoryLookup(C, ClassSymbol);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function UnicodeIsNumber(C: UCS4): Boolean;
// Is the character a number or digit?
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.GetUnicodeCategory(Chr(C)) in
    [TUnicodeCategory.ucDecimalNumber, TUnicodeCategory.ucLetterNumber,
     TUnicodeCategory.ucOtherNumber];
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := CategoryLookup(C, ClassNumber);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function UnicodeIsNonSpacing(C: UCS4): Boolean;
// Is the character non-spacing?
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.GetUnicodeCategory(Chr(C)) = TUnicodeCategory.ucNonSpacingMark;
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := CategoryLookup(C, [ccMarkNonSpacing]);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function UnicodeIsOpenPunctuation(C: UCS4): Boolean;
// Is the character an open/left punctuation (e.g. '[')?
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.GetUnicodeCategory(Chr(C)) = TUnicodeCategory.ucOpenPunctuation;
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := CategoryLookup(C, [ccPunctuationOpen]);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function UnicodeIsClosePunctuation(C: UCS4): Boolean;
// Is the character an close/right punctuation (e.g. ']')?
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.GetUnicodeCategory(Chr(C)) = TUnicodeCategory.ucClosePunctuation;
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := CategoryLookup(C, [ccPunctuationClose]);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function UnicodeIsInitialPunctuation(C: UCS4): Boolean;
// Is the character an initial punctuation (e.g. U+2018 LEFT SINGLE QUOTATION MARK)?
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.GetUnicodeCategory(Chr(C)) = TUnicodeCategory.ucInitialPunctuation;
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := CategoryLookup(C, [ccPunctuationInitialQuote]);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function UnicodeIsFinalPunctuation(C: UCS4): Boolean;
// Is the character a final punctuation (e.g. U+2019 RIGHT SINGLE QUOTATION MARK)?
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.GetUnicodeCategory(Chr(C)) = TUnicodeCategory.ucFinalPunctuation;
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := CategoryLookup(C, [ccPunctuationFinalQuote]);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

{$IFNDEF UNICODE_RTL_DATABASE}
function UnicodeIsComposed(C: UCS4): Boolean;
// Can the character be decomposed into a set of other characters?
begin
  Result := CategoryLookup(C, [ccComposed]);
end;

function UnicodeIsQuotationMark(C: UCS4): Boolean;
// Is the character one of the many quotation marks?
begin
  Result := CategoryLookup(C, [ccQuotationMark]);
end;

function UnicodeIsSymmetric(C: UCS4): Boolean;
// Is the character one that has an opposite form (i.e. <>)?
begin
  Result := CategoryLookup(C, [ccSymmetric]);
end;

function UnicodeIsMirroring(C: UCS4): Boolean;
// Is the character mirroring (superset of symmetric)?
begin
  Result := CategoryLookup(C, [ccMirroring]);
end;

function UnicodeIsNonBreaking(C: UCS4): Boolean;
// Is the character non-breaking (i.e. non-breaking space)?
begin
  Result := CategoryLookup(C, [ccNonBreaking]);
end;

function UnicodeIsRightToLeft(C: UCS4): Boolean;
// Does the character have strong right-to-left directionality (i.e. Arabic letters)?
begin
  Result := CategoryLookup(C, [ccRightToLeft]);
end;

function UnicodeIsLeftToRight(C: UCS4): Boolean;
// Does the character have strong left-to-right directionality (i.e. Latin letters)?
begin
  Result := CategoryLookup(C, [ccLeftToRight]);
end;

function UnicodeIsStrong(C: UCS4): Boolean;
// Does the character have strong directionality?
begin
  Result := CategoryLookup(C, [ccLeftToRight, ccRightToLeft]);
end;

function UnicodeIsWeak(C: UCS4): Boolean;
// Does the character have weak directionality (i.e. numbers)?
begin
  Result := CategoryLookup(C, ClassEuropeanNumber + [ccArabicNumber, ccCommonNumberSeparator]);
end;

function UnicodeIsNeutral(C: UCS4): Boolean;
// Does the character have neutral directionality (i.e. whitespace)?
begin
  Result := CategoryLookup(C, [ccSeparatorParagraph, ccSegmentSeparator, ccWhiteSpace, ccOtherNeutrals]);
end;

function UnicodeIsSeparator(C: UCS4): Boolean;
// Is the character a block or segment separator?
begin
  Result := CategoryLookup(C, [ccSeparatorParagraph, ccSegmentSeparator]);
end;

function UnicodeIsMark(C: UCS4): Boolean;
// Is the character a mark of some kind?
begin
  Result := CategoryLookup(C, ClassMark);
end;

function UnicodeIsModifier(C: UCS4): Boolean;
// Is the character a letter modifier?
begin
  Result := CategoryLookup(C, [ccLetterModifier]);
end;
{$ENDIF ~UNICODE_RTL_DATABASE}

function UnicodeIsLetterNumber(C: UCS4): Boolean;
// Is the character a number represented by a letter?
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.GetUnicodeCategory(Chr(C)) = TUnicodeCategory.ucLetterNumber;
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := CategoryLookup(C, [ccNumberLetter]);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function UnicodeIsConnectionPunctuation(C: UCS4): Boolean;
// Is the character connecting punctuation?
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.GetUnicodeCategory(Chr(C)) = TUnicodeCategory.ucConnectPunctuation;
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := CategoryLookup(C, [ccPunctuationConnector]);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function UnicodeIsDash(C: UCS4): Boolean;
// Is the character a dash punctuation?
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.GetUnicodeCategory(Chr(C)) = TUnicodeCategory.ucDashPunctuation;
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := CategoryLookup(C, [ccPunctuationDash]);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function UnicodeIsMath(C: UCS4): Boolean;
// Is the character a math character?
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.GetUnicodeCategory(Chr(C)) = TUnicodeCategory.ucMathSymbol;
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := CategoryLookup(C, [ccSymbolMath]);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function UnicodeIsCurrency(C: UCS4): Boolean;
// Is the character a currency character?
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.GetUnicodeCategory(Chr(C)) = TUnicodeCategory.ucCurrencySymbol;
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := CategoryLookup(C, [ccSymbolCurrency]);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function UnicodeIsModifierSymbol(C: UCS4): Boolean;
// Is the character a modifier symbol?
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.GetUnicodeCategory(Chr(C)) = TUnicodeCategory.ucModifierSymbol;
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := CategoryLookup(C, [ccSymbolModifier]);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function UnicodeIsSpacingMark(C: UCS4): Boolean;
// Is the character a spacing mark?
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.GetUnicodeCategory(Chr(C)) in
    [TUnicodeCategory.ucLineSeparator, TUnicodeCategory.ucParagraphSeparator,
     TUnicodeCategory.ucSpaceSeparator];
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := CategoryLookup(C, [ccMarkSpacingCombining]);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function UnicodeIsEnclosing(C: UCS4): Boolean;
// Is the character enclosing (i.e. enclosing box)?
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.GetUnicodeCategory(Chr(C)) = TUnicodeCategory.ucEnclosingMark;
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := CategoryLookup(C, [ccMarkEnclosing]);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function UnicodeIsPrivate(C: UCS4): Boolean;
// Is the character from the Private Use Area?
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.GetUnicodeCategory(Chr(C)) = TUnicodeCategory.ucPrivateUse;
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := CategoryLookup(C, [ccOtherPrivate]);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function UnicodeIsSurrogate(C: UCS4): Boolean;
// Is the character one of the surrogate codes?
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.GetUnicodeCategory(Chr(C)) = TUnicodeCategory.ucSurrogate;
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := CategoryLookup(C, [ccOtherSurrogate]);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function UnicodeIsLineSeparator(C: UCS4): Boolean;
// Is the character a line separator?
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.GetUnicodeCategory(Chr(C)) = TUnicodeCategory.ucLineSeparator;
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := CategoryLookup(C, [ccSeparatorLine]);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function UnicodeIsParagraphSeparator(C: UCS4): Boolean;
// Is th character a paragraph separator;
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.GetUnicodeCategory(Chr(C)) = TUnicodeCategory.ucParagraphSeparator;
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := CategoryLookup(C, [ccSeparatorParagraph]);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function UnicodeIsIdentifierStart(C: UCS4): Boolean;
// Can the character begin an identifier?
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.GetUnicodeCategory(Chr(C)) in
    [TUnicodeCategory.ucLowercaseLetter, TUnicodeCategory.ucModifierLetter,
     TUnicodeCategory.ucOtherLetter, TUnicodeCategory.ucTitlecaseLetter,
     TUnicodeCategory.ucUppercaseLetter,
     TUnicodeCategory.ucLetterNumber];
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := CategoryLookup(C, ClassLetter + [ccNumberLetter]);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function UnicodeIsIdentifierPart(C: UCS4): Boolean;
// Can the character appear in an identifier?
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.GetUnicodeCategory(Chr(C)) in
    [TUnicodeCategory.ucLowercaseLetter, TUnicodeCategory.ucModifierLetter,
     TUnicodeCategory.ucOtherLetter, TUnicodeCategory.ucTitlecaseLetter,
     TUnicodeCategory.ucUppercaseLetter,
     TUnicodeCategory.ucLetterNumber, TUnicodeCategory.ucDecimalNumber,
     TUnicodeCategory.ucNonSpacingMark, TUnicodeCategory.ucCombiningMark,
     TUnicodeCategory.ucConnectPunctuation,
     TUnicodeCategory.ucFormat];
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := CategoryLookup(C, ClassLetter + [ccNumberLetter, ccMarkNonSpacing, ccMarkSpacingCombining,
    ccNumberDecimalDigit, ccPunctuationConnector, ccOtherFormat]);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function UnicodeIsDefined(C: UCS4): Boolean;
// Is the character defined (appears in one of the data files)?
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.GetUnicodeCategory(Chr(C)) <> TUnicodeCategory.ucUnassigned;
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := CategoryLookup(C, [ccAssigned]);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function UnicodeIsUndefined(C: UCS4): Boolean;
// Is the character undefined (not assigned in the Unicode database)?
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.GetUnicodeCategory(Chr(C)) = TUnicodeCategory.ucUnassigned;
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := not CategoryLookup(C, [ccAssigned]);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function UnicodeIsHan(C: UCS4): Boolean;
// Is the character a Han ideograph?
begin
  Result := ((C >= $4E00) and (C <= $9FFF))  or ((C >= $F900) and (C <= $FAFF));
end;

function UnicodeIsHangul(C: UCS4): Boolean;
// Is the character a pre-composed Hangul syllable?
begin
  Result := (C >= $AC00) and (C <= $D7FF);
end;

function UnicodeIsUnassigned(C: UCS4): Boolean;
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.GetUnicodeCategory(Chr(C)) = TUnicodeCategory.ucUnassigned;
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := CategoryLookup(C, [ccOtherUnassigned]);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function UnicodeIsLetterOther(C: UCS4): Boolean;
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.GetUnicodeCategory(Chr(C)) = TUnicodeCategory.ucOtherLetter;
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := CategoryLookup(C, [ccLetterOther]);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function UnicodeIsConnector(C: UCS4): Boolean;
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.GetUnicodeCategory(Chr(C)) = TUnicodeCategory.ucConnectPunctuation;
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := CategoryLookup(C, [ccPunctuationConnector]);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function UnicodeIsPunctuationOther(C: UCS4): Boolean;
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.GetUnicodeCategory(Chr(C)) = TUnicodeCategory.ucOtherPunctuation;
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := CategoryLookup(C, [ccPunctuationOther]);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

function UnicodeIsSymbolOther(C: UCS4): Boolean;
begin
  {$IFDEF UNICODE_RTL_DATABASE}
  Result := TCharacter.GetUnicodeCategory(Chr(C)) = TUnicodeCategory.ucOtherSymbol;
  {$ELSE ~UNICODE_RTL_DATABASE}
  Result := CategoryLookup(C, [ccSymbolOther]);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

{$IFNDEF UNICODE_RTL_DATABASE}
function UnicodeIsLeftToRightEmbedding(C: UCS4): Boolean;
begin
  Result := CategoryLookup(C, [ccLeftToRightEmbedding]);
end;

function UnicodeIsLeftToRightOverride(C: UCS4): Boolean;
begin
  Result := CategoryLookup(C, [ccLeftToRightOverride]);
end;

function UnicodeIsRightToLeftArabic(C: UCS4): Boolean;
begin
  Result := CategoryLookup(C, [ccRightToLeftArabic]);
end;

function UnicodeIsRightToLeftEmbedding(C: UCS4): Boolean;
begin
  Result := CategoryLookup(C, [ccRightToLeftEmbedding]);
end;

function UnicodeIsRightToLeftOverride(C: UCS4): Boolean;
begin
  Result := CategoryLookup(C, [ccRightToLeftOverride]);
end;

function UnicodeIsPopDirectionalFormat(C: UCS4): Boolean;
begin
  Result := CategoryLookup(C, [ccPopDirectionalFormat]);
end;

function UnicodeIsEuropeanNumber(C: UCS4): Boolean;
begin
  Result := CategoryLookup(C, [ccEuropeanNumber]);
end;

function UnicodeIsEuropeanNumberSeparator(C: UCS4): Boolean;
begin
  Result := CategoryLookup(C, [ccEuropeanNumberSeparator]);
end;

function UnicodeIsEuropeanNumberTerminator(C: UCS4): Boolean;
begin
  Result := CategoryLookup(C, [ccEuropeanNumberTerminator]);
end;

function UnicodeIsArabicNumber(C: UCS4): Boolean;
begin
  Result := CategoryLookup(C, [ccArabicNumber]);
end;

function UnicodeIsCommonNumberSeparator(C: UCS4): Boolean;
begin
  Result := CategoryLookup(C, [ccCommonNumberSeparator]);
end;

function UnicodeIsBoundaryNeutral(C: UCS4): Boolean;
begin
  Result := CategoryLookup(C, [ccBoundaryNeutral]);
end;

function UnicodeIsSegmentSeparator(C: UCS4): Boolean;
begin
  Result := CategoryLookup(C, [ccSegmentSeparator]);
end;

function UnicodeIsOtherNeutrals(C: UCS4): Boolean;
begin
  Result := CategoryLookup(C, [ccOtherNeutrals]);
end;

function UnicodeIsASCIIHexDigit(C: UCS4): Boolean;
begin
  Result := CategoryLookup(C, [ccASCIIHexDigit]);
end;

function UnicodeIsBidiControl(C: UCS4): Boolean;
begin
  Result := CategoryLookup(C, [ccBidiControl]);
end;

function UnicodeIsDeprecated(C: UCS4): Boolean;
begin
  Result := CategoryLookup(C, [ccDeprecated]);
end;

function UnicodeIsDiacritic(C: UCS4): Boolean;
begin
  Result := CategoryLookup(C, [ccDiacritic]);
end;

function UnicodeIsExtender(C: UCS4): Boolean;
begin
  Result := CategoryLookup(C, [ccExtender]);
end;

function UnicodeIsHyphen(C: UCS4): Boolean;
begin
  Result := CategoryLookup(C, [ccHyphen]);
end;

function UnicodeIsIdeographic(C: UCS4): Boolean;
begin
  Result := CategoryLookup(C, [ccIdeographic]);
end;

function UnicodeIsIDSBinaryOperator(C: UCS4): Boolean;
begin
  Result := CategoryLookup(C, [ccIDSBinaryOperator]);
end;

function UnicodeIsIDSTrinaryOperator(C: UCS4): Boolean;
begin
  Result := CategoryLookup(C, [ccIDSTrinaryOperator]);
end;

function UnicodeIsJoinControl(C: UCS4): Boolean;
begin
  Result := CategoryLookup(C, [ccJoinControl]);
end;

function UnicodeIsLogicalOrderException(C: UCS4): Boolean;
begin
  Result := CategoryLookup(C, [ccLogicalOrderException]);
end;

function UnicodeIsNonCharacterCodePoint(C: UCS4): Boolean;
begin
  Result := CategoryLookup(C, [ccNonCharacterCodePoint]);
end;

function UnicodeIsOtherAlphabetic(C: UCS4): Boolean;
begin
  Result := CategoryLookup(C, [ccOtherAlphabetic]);
end;

function UnicodeIsOtherDefaultIgnorableCodePoint(C: UCS4): Boolean;
begin
  Result := CategoryLookup(C, [ccOtherDefaultIgnorableCodePoint]);
end;

function UnicodeIsOtherGraphemeExtend(C: UCS4): Boolean;
begin
  Result := CategoryLookup(C, [ccOtherGraphemeExtend]);
end;

function UnicodeIsOtherIDContinue(C: UCS4): Boolean;
begin
  Result := CategoryLookup(C, [ccOtherIDContinue]);
end;

function UnicodeIsOtherIDStart(C: UCS4): Boolean;
begin
  Result := CategoryLookup(C, [ccOtherIDStart]);
end;

function UnicodeIsOtherLowercase(C: UCS4): Boolean;
begin
  Result := CategoryLookup(C, [ccOtherLowercase]);
end;

function UnicodeIsOtherMath(C: UCS4): Boolean;
begin
  Result := CategoryLookup(C, [ccOtherMath]);
end;

function UnicodeIsOtherUppercase(C: UCS4): Boolean;
begin
  Result := CategoryLookup(C, [ccOtherUppercase]);
end;

function UnicodeIsPatternSyntax(C: UCS4): Boolean;
begin
  Result := CategoryLookup(C, [ccPatternSyntax]);
end;

function UnicodeIsPatternWhiteSpace(C: UCS4): Boolean;
begin
  Result := CategoryLookup(C, [ccPatternWhiteSpace]);
end;

function UnicodeIsRadical(C: UCS4): Boolean;
begin
  Result := CategoryLookup(C, [ccRadical]);
end;

function UnicodeIsSoftDotted(C: UCS4): Boolean;
begin
  Result := CategoryLookup(C, [ccSoftDotted]);
end;

function UnicodeIsSTerm(C: UCS4): Boolean;
begin
  Result := CategoryLookup(C, [ccSTerm]);
end;

function UnicodeIsTerminalPunctuation(C: UCS4): Boolean;
begin
  Result := CategoryLookup(C, [ccTerminalPunctuation]);
end;

function UnicodeIsUnifiedIdeograph(C: UCS4): Boolean;
begin
  Result := CategoryLookup(C, [ccUnifiedIdeograph]);
end;

function UnicodeIsVariationSelector(C: UCS4): Boolean;
begin
  Result := CategoryLookup(C, [ccVariationSelector]);
end;
{$ENDIF ~UNICODE_RTL_DATABASE}

// I need to fix a problem (introduced by MS) here. The first parameter can be a pointer
// (and is so defined) or can be a normal DWORD, depending on the dwFlags parameter.
// As usual, lpSrc has been translated to a var parameter. But this does not work in
// our case, hence the redeclaration of the function with a pointer as first parameter.

function TranslateCharsetInfoEx(lpSrc: SizeInt; out lpCs: TCharsetInfo; dwFlags: DWORD): BOOL; stdcall;
  external 'gdi32.dll' name 'TranslateCharsetInfo';

function GetCharSetFromLocale(Language: LCID; out FontCharSet: Byte): Boolean;
const
  TCI_SRCLOCALE = $1000;
var
  CP: Word;
  CSI: TCharsetInfo;
begin
  if not JclCheckWinVersion(5, 0) then // Win2k required
  begin
    // these versions of Windows don't support TCI_SRCLOCALE
    CP := CodePageFromLocale(Language);
    if CP = 0 then
      RaiseLastOSError;
    Result := TranslateCharsetInfoEx(CP, CSI, TCI_SRCCODEPAGE);
  end
  else
    Result := TranslateCharsetInfoEx(Language, CSI, TCI_SRCLOCALE);

  if Result then
    FontCharset := CSI.ciCharset;
end;

function CharSetFromLocale(Language: LCID): Byte;
begin
  if not GetCharSetFromLocale(Language, Result) then
    RaiseLastOSError;
end;

function CodePageFromLocale(Language: LCID): Word;
// determines the code page for a given locale
var
  Buf: array [0..6] of Char;
begin
  GetLocaleInfo(Language, LOCALE_IDefaultAnsiCodePage, Buf, 6);
  Result := StrToIntDef(Buf, GetACP);
end;

function KeyboardCodePage: Word;
begin
  Result := CodePageFromLocale(GetKeyboardLayout(0) and $FFFF);
end;

function KeyUnicode(C: Char): WideChar;
// converts the given character (as it comes with a WM_CHAR message) into its
// corresponding Unicode character depending on the active keyboard layout
begin
  MultiByteToWideChar(KeyboardCodePage, MB_USEGLYPHCHARS, @C, 1, @Result, 1);
end;

function CodeBlockRange(const CB: TUnicodeBlock): TUnicodeBlockRange;
// http://www.unicode.org/Public/5.0.0/ucd/Blocks.txt
begin
  Result := UnicodeBlockData[CB].Range;
end;


// Names taken from http://www.unicode.org/Public/5.0.0/ucd/Blocks.txt
function CodeBlockName(const CB: TUnicodeBlock): string;
begin
  Result := UnicodeBlockData[CB].Name;
end;

// Returns an ID for the Unicode code block to which C belongs.
// If C does not belong to any of the defined blocks then ubUndefined is returned.
// Note: the code blocks listed here are based on Unicode Version 5.0.0
function CodeBlockFromChar(const C: UCS4): TUnicodeBlock;
// http://www.unicode.org/Public/5.0.0/ucd/Blocks.txt
var
  L, H, I: TUnicodeBlock;
begin
  Result := ubUndefined;
  L := ubBasicLatin;
  H := High(TUnicodeBlock);
  while L <= H do
  begin
    I := TUnicodeBlock((Cardinal(L) + Cardinal(H)) shr 1);
    if (C >= UnicodeBlockData[I].Range.RangeStart) and (C <= UnicodeBlockData[I].Range.RangeEnd) then
    begin
      Result := I;
      Break;
    end
    else
    if C < UnicodeBlockData[I].Range.RangeStart then
    begin
      Dec(I);
      H := I;
    end
    else
    begin
      Inc(I);
      L := I;
    end;
  end;
end;


function CompareTextWin95(const W1, W2: WideString; Locale: LCID): SizeInt;
// special comparation function for Win9x since there's no system defined
// comparation function, returns -1 if W1 < W2, 0 if W1 = W2 or 1 if W1 > W2
var
  S1, S2: AnsiString;
  CP: Word;
  L1, L2: SizeInt;
begin
  L1 := Length(W1);
  L2 := Length(W2);
  SetLength(S1, L1);
  SetLength(S2, L2);
  CP := CodePageFromLocale(Locale);
  WideCharToMultiByte(CP, 0, PWideChar(W1), L1, PAnsiChar(S1), L1, nil, nil);
  WideCharToMultiByte(CP, 0, PWideChar(W2), L2, PAnsiChar(S2), L2, nil, nil);
  Result := CompareStringA(Locale, NORM_IGNORECASE, PAnsiChar(S1), Length(S1),
    PAnsiChar(S2), Length(S2)) - 2;
end;

function CompareTextWinNT(const W1, W2: WideString; Locale: LCID): SizeInt;
// Wrapper function for WinNT since there's no system defined comparation function
// in Win9x and we need a central comparation function for TWideStringList.
// Returns -1 if W1 < W2, 0 if W1 = W2 or 1 if W1 > W2
begin
  Result := CompareStringW(Locale, NORM_IGNORECASE, PWideChar(W1), Length(W1),
    PWideChar(W2), Length(W2)) - 2;
end;

function StringToWideStringEx(const S: AnsiString; CodePage: Word): WideString;
var
  InputLength,
  OutputLength: SizeInt;
begin
  InputLength := Length(S);
  OutputLength := MultiByteToWideChar(CodePage, 0, PAnsiChar(S), InputLength, nil, 0);
  SetLength(Result, OutputLength);
  MultiByteToWideChar(CodePage, 0, PAnsiChar(S), InputLength, PWideChar(Result), OutputLength);
end;

function WideStringToStringEx(const WS: WideString; CodePage: Word): AnsiString;
var
  InputLength,
  OutputLength: SizeInt;
begin
  InputLength := Length(WS);
  OutputLength := WideCharToMultiByte(CodePage, 0, PWideChar(WS), InputLength, nil, 0, nil, nil);
  SetLength(Result, OutputLength);
  WideCharToMultiByte(CodePage, 0, PWideChar(WS), InputLength, PAnsiChar(Result), OutputLength, nil, nil);
end;

function TranslateString(const S: AnsiString; CP1, CP2: Word): AnsiString;
begin
  Result:= WideStringToStringEx(StringToWideStringEx(S, CP1), CP2);
end;

function UCS4Array(Ch: UCS4): TUCS4Array;
begin
  SetLength(Result, 1);
  Result[0] := Ch;
end;

function UCS4ArrayConcat(Left, Right: UCS4): TUCS4Array;
begin
  SetLength(Result, 2);
  Result[0] := Left;
  Result[1] := Right;
end;

procedure UCS4ArrayConcat(var Left: TUCS4Array; Right: UCS4);
var
  I: SizeInt;
begin
  I := Length(Left);
  SetLength(Left, I + 1);
  Left[I] := Right;
end;

procedure UCS4ArrayConcat(var Left: TUCS4Array; const Right: TUCS4Array);
var
  I, J: SizeInt;
begin
  I := Length(Left);
  J := Length(Right);
  SetLength(Left, I + J);
  Move(Right[0], Left[I], J * SizeOf(Right[0]));
end;

function UCS4ArrayEquals(const Left: TUCS4Array; const Right: TUCS4Array): Boolean;
var
  I: SizeInt;
begin
  I := Length(Left);
  Result := I = Length(Right);
  while Result do
  begin
    Dec(I);
    Result := (I >= 0) and (Left[I] = Right[I]);
  end;
  Result := I < 0;
end;

function UCS4ArrayEquals(const Left: TUCS4Array; Right: UCS4): Boolean;
begin
  Result := (Length(Left) = 1) and (Left[0] = Right);
end;

function UCS4ArrayEquals(const Left: TUCS4Array; const Right: AnsiString): Boolean;
var
  I: SizeInt;
begin
  I := Length(Left);
  Result := I = Length(Right);
  while Result do
  begin
    Dec(I);
    Result := (I >= 0) and (Left[I] = Ord(Right[I + 1]));
  end;
  Result := I < 0;
end;

function UCS4ArrayEquals(const Left: TUCS4Array; Right: AnsiChar): Boolean;
begin
  Result := (Length(Left) = 1) and (Left[0] = Ord(Right));
end;

procedure PrepareUnicodeData;
// Prepares structures which are globally needed.
begin
  {$IFNDEF UNICODE_RTL_DATABASE}
  LoadInProgress := TJclCriticalSection.Create;
  {$ENDIF ~UNICODE_RTL_DATABASE}

  if (Win32Platform and VER_PLATFORM_WIN32_NT) <> 0 then
    @WideCompareText := @CompareTextWinNT
  else
    @WideCompareText := @CompareTextWin95;
end;

procedure FreeUnicodeData;
// Frees all data which has been allocated and which is not automatically freed by Delphi.
begin
  {$IFNDEF UNICODE_RTL_DATABASE}
  FreeAndNil(LoadInProgress);
  {$ENDIF ~UNICODE_RTL_DATABASE}
end;

initialization
  PrepareUnicodeData;
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}
  FreeUnicodeData;

end.
