{
  Double Commander
  -------------------------------------------------------------------------
  This is a pascal implementation of wcwidth() function

  Copyright (C) 2022 Alexander Koblov (alexx2000@mail.ru)

  Based on https://github.com/termux/wcwidth
  Copyright (c) 2016 Fredrik Fornwall <fredrik@fornwall.net>

  Based on https://github.com/jquast/wcwidth
  Copyright (c) 2014 Jeff Quast <contact@jeffquast.com>

  Permission is hereby granted, free of charge, to any person obtaining
  a copy of this software and associated documentation files (the
  "Software"), to deal in the Software without restriction, including
  without limitation the rights to use, copy, modify, merge, publish,
  distribute, sublicense, and/or sell copies of the Software, and to
  permit persons to whom the Software is furnished to do so, subject to
  the following conditions:

  The above copyright notice and this permission notice shall be included
  in all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

  Markus Kuhn -- 2007-05-26 (Unicode 5.0)

  Permission to use, copy, modify, and distribute this software
  for any purpose and without fee is hereby granted. The author
  disclaims all warranties with regard to this software.
}

unit VTWideCharWidth;

{$mode objfpc}

interface

uses
  SysUtils, LCLType;

function UTF8Width(const Ch: TUTF8Char): Integer;

implementation

uses
  LazUTF8;

type
  Pwidth_interval = ^Twidth_interval;
  Twidth_interval = array[0..1] of UCS4Char;

const
  // From https://github.com/jquast/wcwidth/blob/master/wcwidth/table_zero.py
  // at commit b29897e5a1b403a0e36f7fc991614981cbc42475 (2020-07-14):
  ZERO_WIDTH: array[0..323] of Twidth_interval = (
        ($00300, $0036f),  // Combining Grave Accent  ..Combining Latin Small Le
        ($00483, $00489),  // Combining Cyrillic Titlo..Combining Cyrillic Milli
        ($00591, $005bd),  // Hebrew Accent Etnahta   ..Hebrew Point Meteg
        ($005bf, $005bf),  // Hebrew Point Rafe       ..Hebrew Point Rafe
        ($005c1, $005c2),  // Hebrew Point Shin Dot   ..Hebrew Point Sin Dot
        ($005c4, $005c5),  // Hebrew Mark Upper Dot   ..Hebrew Mark Lower Dot
        ($005c7, $005c7),  // Hebrew Point Qamats Qata..Hebrew Point Qamats Qata
        ($00610, $0061a),  // Arabic Sign Sallallahou ..Arabic Small Kasra
        ($0064b, $0065f),  // Arabic Fathatan         ..Arabic Wavy Hamza Below
        ($00670, $00670),  // Arabic Letter Superscrip..Arabic Letter Superscrip
        ($006d6, $006dc),  // Arabic Small High Ligatu..Arabic Small High Seen
        ($006df, $006e4),  // Arabic Small High Rounde..Arabic Small High Madda
        ($006e7, $006e8),  // Arabic Small High Yeh   ..Arabic Small High Noon
        ($006ea, $006ed),  // Arabic Empty Centre Low ..Arabic Small Low Meem
        ($00711, $00711),  // Syriac Letter Superscrip..Syriac Letter Superscrip
        ($00730, $0074a),  // Syriac Pthaha Above     ..Syriac Barrekh
        ($007a6, $007b0),  // Thaana Abafili          ..Thaana Sukun
        ($007eb, $007f3),  // Nko Combining Short High..Nko Combining Double Dot
        ($007fd, $007fd),  // Nko Dantayalan          ..Nko Dantayalan
        ($00816, $00819),  // Samaritan Mark In       ..Samaritan Mark Dagesh
        ($0081b, $00823),  // Samaritan Mark Epentheti..Samaritan Vowel Sign A
        ($00825, $00827),  // Samaritan Vowel Sign Sho..Samaritan Vowel Sign U
        ($00829, $0082d),  // Samaritan Vowel Sign Lon..Samaritan Mark Nequdaa
        ($00859, $0085b),  // Mandaic Affrication Mark..Mandaic Gemination Mark
        ($008d3, $008e1),  // Arabic Small Low Waw    ..Arabic Small High Sign S
        ($008e3, $00902),  // Arabic Turned Damma Belo..Devanagari Sign Anusvara
        ($0093a, $0093a),  // Devanagari Vowel Sign Oe..Devanagari Vowel Sign Oe
        ($0093c, $0093c),  // Devanagari Sign Nukta   ..Devanagari Sign Nukta
        ($00941, $00948),  // Devanagari Vowel Sign U ..Devanagari Vowel Sign Ai
        ($0094d, $0094d),  // Devanagari Sign Virama  ..Devanagari Sign Virama
        ($00951, $00957),  // Devanagari Stress Sign U..Devanagari Vowel Sign Uu
        ($00962, $00963),  // Devanagari Vowel Sign Vo..Devanagari Vowel Sign Vo
        ($00981, $00981),  // Bengali Sign Candrabindu..Bengali Sign Candrabindu
        ($009bc, $009bc),  // Bengali Sign Nukta      ..Bengali Sign Nukta
        ($009c1, $009c4),  // Bengali Vowel Sign U    ..Bengali Vowel Sign Vocal
        ($009cd, $009cd),  // Bengali Sign Virama     ..Bengali Sign Virama
        ($009e2, $009e3),  // Bengali Vowel Sign Vocal..Bengali Vowel Sign Vocal
        ($009fe, $009fe),  // Bengali Sandhi Mark     ..Bengali Sandhi Mark
        ($00a01, $00a02),  // Gurmukhi Sign Adak Bindi..Gurmukhi Sign Bindi
        ($00a3c, $00a3c),  // Gurmukhi Sign Nukta     ..Gurmukhi Sign Nukta
        ($00a41, $00a42),  // Gurmukhi Vowel Sign U   ..Gurmukhi Vowel Sign Uu
        ($00a47, $00a48),  // Gurmukhi Vowel Sign Ee  ..Gurmukhi Vowel Sign Ai
        ($00a4b, $00a4d),  // Gurmukhi Vowel Sign Oo  ..Gurmukhi Sign Virama
        ($00a51, $00a51),  // Gurmukhi Sign Udaat     ..Gurmukhi Sign Udaat
        ($00a70, $00a71),  // Gurmukhi Tippi          ..Gurmukhi Addak
        ($00a75, $00a75),  // Gurmukhi Sign Yakash    ..Gurmukhi Sign Yakash
        ($00a81, $00a82),  // Gujarati Sign Candrabind..Gujarati Sign Anusvara
        ($00abc, $00abc),  // Gujarati Sign Nukta     ..Gujarati Sign Nukta
        ($00ac1, $00ac5),  // Gujarati Vowel Sign U   ..Gujarati Vowel Sign Cand
        ($00ac7, $00ac8),  // Gujarati Vowel Sign E   ..Gujarati Vowel Sign Ai
        ($00acd, $00acd),  // Gujarati Sign Virama    ..Gujarati Sign Virama
        ($00ae2, $00ae3),  // Gujarati Vowel Sign Voca..Gujarati Vowel Sign Voca
        ($00afa, $00aff),  // Gujarati Sign Sukun     ..Gujarati Sign Two-circle
        ($00b01, $00b01),  // Oriya Sign Candrabindu  ..Oriya Sign Candrabindu
        ($00b3c, $00b3c),  // Oriya Sign Nukta        ..Oriya Sign Nukta
        ($00b3f, $00b3f),  // Oriya Vowel Sign I      ..Oriya Vowel Sign I
        ($00b41, $00b44),  // Oriya Vowel Sign U      ..Oriya Vowel Sign Vocalic
        ($00b4d, $00b4d),  // Oriya Sign Virama       ..Oriya Sign Virama
        ($00b55, $00b56),  // (nil)                   ..Oriya Ai Length Mark
        ($00b62, $00b63),  // Oriya Vowel Sign Vocalic..Oriya Vowel Sign Vocalic
        ($00b82, $00b82),  // Tamil Sign Anusvara     ..Tamil Sign Anusvara
        ($00bc0, $00bc0),  // Tamil Vowel Sign Ii     ..Tamil Vowel Sign Ii
        ($00bcd, $00bcd),  // Tamil Sign Virama       ..Tamil Sign Virama
        ($00c00, $00c00),  // Telugu Sign Combining Ca..Telugu Sign Combining Ca
        ($00c04, $00c04),  // Telugu Sign Combining An..Telugu Sign Combining An
        ($00c3e, $00c40),  // Telugu Vowel Sign Aa    ..Telugu Vowel Sign Ii
        ($00c46, $00c48),  // Telugu Vowel Sign E     ..Telugu Vowel Sign Ai
        ($00c4a, $00c4d),  // Telugu Vowel Sign O     ..Telugu Sign Virama
        ($00c55, $00c56),  // Telugu Length Mark      ..Telugu Ai Length Mark
        ($00c62, $00c63),  // Telugu Vowel Sign Vocali..Telugu Vowel Sign Vocali
        ($00c81, $00c81),  // Kannada Sign Candrabindu..Kannada Sign Candrabindu
        ($00cbc, $00cbc),  // Kannada Sign Nukta      ..Kannada Sign Nukta
        ($00cbf, $00cbf),  // Kannada Vowel Sign I    ..Kannada Vowel Sign I
        ($00cc6, $00cc6),  // Kannada Vowel Sign E    ..Kannada Vowel Sign E
        ($00ccc, $00ccd),  // Kannada Vowel Sign Au   ..Kannada Sign Virama
        ($00ce2, $00ce3),  // Kannada Vowel Sign Vocal..Kannada Vowel Sign Vocal
        ($00d00, $00d01),  // Malayalam Sign Combining..Malayalam Sign Candrabin
        ($00d3b, $00d3c),  // Malayalam Sign Vertical ..Malayalam Sign Circular
        ($00d41, $00d44),  // Malayalam Vowel Sign U  ..Malayalam Vowel Sign Voc
        ($00d4d, $00d4d),  // Malayalam Sign Virama   ..Malayalam Sign Virama
        ($00d62, $00d63),  // Malayalam Vowel Sign Voc..Malayalam Vowel Sign Voc
        ($00d81, $00d81),  // (nil)                   ..(nil)
        ($00dca, $00dca),  // Sinhala Sign Al-lakuna  ..Sinhala Sign Al-lakuna
        ($00dd2, $00dd4),  // Sinhala Vowel Sign Ketti..Sinhala Vowel Sign Ketti
        ($00dd6, $00dd6),  // Sinhala Vowel Sign Diga ..Sinhala Vowel Sign Diga
        ($00e31, $00e31),  // Thai Character Mai Han-a..Thai Character Mai Han-a
        ($00e34, $00e3a),  // Thai Character Sara I   ..Thai Character Phinthu
        ($00e47, $00e4e),  // Thai Character Maitaikhu..Thai Character Yamakkan
        ($00eb1, $00eb1),  // Lao Vowel Sign Mai Kan  ..Lao Vowel Sign Mai Kan
        ($00eb4, $00ebc),  // Lao Vowel Sign I        ..Lao Semivowel Sign Lo
        ($00ec8, $00ecd),  // Lao Tone Mai Ek         ..Lao Niggahita
        ($00f18, $00f19),  // Tibetan Astrological Sig..Tibetan Astrological Sig
        ($00f35, $00f35),  // Tibetan Mark Ngas Bzung ..Tibetan Mark Ngas Bzung
        ($00f37, $00f37),  // Tibetan Mark Ngas Bzung ..Tibetan Mark Ngas Bzung
        ($00f39, $00f39),  // Tibetan Mark Tsa -phru  ..Tibetan Mark Tsa -phru
        ($00f71, $00f7e),  // Tibetan Vowel Sign Aa   ..Tibetan Sign Rjes Su Nga
        ($00f80, $00f84),  // Tibetan Vowel Sign Rever..Tibetan Mark Halanta
        ($00f86, $00f87),  // Tibetan Sign Lci Rtags  ..Tibetan Sign Yang Rtags
        ($00f8d, $00f97),  // Tibetan Subjoined Sign L..Tibetan Subjoined Letter
        ($00f99, $00fbc),  // Tibetan Subjoined Letter..Tibetan Subjoined Letter
        ($00fc6, $00fc6),  // Tibetan Symbol Padma Gda..Tibetan Symbol Padma Gda
        ($0102d, $01030),  // Myanmar Vowel Sign I    ..Myanmar Vowel Sign Uu
        ($01032, $01037),  // Myanmar Vowel Sign Ai   ..Myanmar Sign Dot Below
        ($01039, $0103a),  // Myanmar Sign Virama     ..Myanmar Sign Asat
        ($0103d, $0103e),  // Myanmar Consonant Sign M..Myanmar Consonant Sign M
        ($01058, $01059),  // Myanmar Vowel Sign Vocal..Myanmar Vowel Sign Vocal
        ($0105e, $01060),  // Myanmar Consonant Sign M..Myanmar Consonant Sign M
        ($01071, $01074),  // Myanmar Vowel Sign Geba ..Myanmar Vowel Sign Kayah
        ($01082, $01082),  // Myanmar Consonant Sign S..Myanmar Consonant Sign S
        ($01085, $01086),  // Myanmar Vowel Sign Shan ..Myanmar Vowel Sign Shan
        ($0108d, $0108d),  // Myanmar Sign Shan Counci..Myanmar Sign Shan Counci
        ($0109d, $0109d),  // Myanmar Vowel Sign Aiton..Myanmar Vowel Sign Aiton
        ($0135d, $0135f),  // Ethiopic Combining Gemin..Ethiopic Combining Gemin
        ($01712, $01714),  // Tagalog Vowel Sign I    ..Tagalog Sign Virama
        ($01732, $01734),  // Hanunoo Vowel Sign I    ..Hanunoo Sign Pamudpod
        ($01752, $01753),  // Buhid Vowel Sign I      ..Buhid Vowel Sign U
        ($01772, $01773),  // Tagbanwa Vowel Sign I   ..Tagbanwa Vowel Sign U
        ($017b4, $017b5),  // Khmer Vowel Inherent Aq ..Khmer Vowel Inherent Aa
        ($017b7, $017bd),  // Khmer Vowel Sign I      ..Khmer Vowel Sign Ua
        ($017c6, $017c6),  // Khmer Sign Nikahit      ..Khmer Sign Nikahit
        ($017c9, $017d3),  // Khmer Sign Muusikatoan  ..Khmer Sign Bathamasat
        ($017dd, $017dd),  // Khmer Sign Atthacan     ..Khmer Sign Atthacan
        ($0180b, $0180d),  // Mongolian Free Variation..Mongolian Free Variation
        ($01885, $01886),  // Mongolian Letter Ali Gal..Mongolian Letter Ali Gal
        ($018a9, $018a9),  // Mongolian Letter Ali Gal..Mongolian Letter Ali Gal
        ($01920, $01922),  // Limbu Vowel Sign A      ..Limbu Vowel Sign U
        ($01927, $01928),  // Limbu Vowel Sign E      ..Limbu Vowel Sign O
        ($01932, $01932),  // Limbu Small Letter Anusv..Limbu Small Letter Anusv
        ($01939, $0193b),  // Limbu Sign Mukphreng    ..Limbu Sign Sa-i
        ($01a17, $01a18),  // Buginese Vowel Sign I   ..Buginese Vowel Sign U
        ($01a1b, $01a1b),  // Buginese Vowel Sign Ae  ..Buginese Vowel Sign Ae
        ($01a56, $01a56),  // Tai Tham Consonant Sign ..Tai Tham Consonant Sign
        ($01a58, $01a5e),  // Tai Tham Sign Mai Kang L..Tai Tham Consonant Sign
        ($01a60, $01a60),  // Tai Tham Sign Sakot     ..Tai Tham Sign Sakot
        ($01a62, $01a62),  // Tai Tham Vowel Sign Mai ..Tai Tham Vowel Sign Mai
        ($01a65, $01a6c),  // Tai Tham Vowel Sign I   ..Tai Tham Vowel Sign Oa B
        ($01a73, $01a7c),  // Tai Tham Vowel Sign Oa A..Tai Tham Sign Khuen-lue
        ($01a7f, $01a7f),  // Tai Tham Combining Crypt..Tai Tham Combining Crypt
        ($01ab0, $01ac0),  // Combining Doubled Circum..(nil)
        ($01b00, $01b03),  // Balinese Sign Ulu Ricem ..Balinese Sign Surang
        ($01b34, $01b34),  // Balinese Sign Rerekan   ..Balinese Sign Rerekan
        ($01b36, $01b3a),  // Balinese Vowel Sign Ulu ..Balinese Vowel Sign Ra R
        ($01b3c, $01b3c),  // Balinese Vowel Sign La L..Balinese Vowel Sign La L
        ($01b42, $01b42),  // Balinese Vowel Sign Pepe..Balinese Vowel Sign Pepe
        ($01b6b, $01b73),  // Balinese Musical Symbol ..Balinese Musical Symbol
        ($01b80, $01b81),  // Sundanese Sign Panyecek ..Sundanese Sign Panglayar
        ($01ba2, $01ba5),  // Sundanese Consonant Sign..Sundanese Vowel Sign Pan
        ($01ba8, $01ba9),  // Sundanese Vowel Sign Pam..Sundanese Vowel Sign Pan
        ($01bab, $01bad),  // Sundanese Sign Virama   ..Sundanese Consonant Sign
        ($01be6, $01be6),  // Batak Sign Tompi        ..Batak Sign Tompi
        ($01be8, $01be9),  // Batak Vowel Sign Pakpak ..Batak Vowel Sign Ee
        ($01bed, $01bed),  // Batak Vowel Sign Karo O ..Batak Vowel Sign Karo O
        ($01bef, $01bf1),  // Batak Vowel Sign U For S..Batak Consonant Sign H
        ($01c2c, $01c33),  // Lepcha Vowel Sign E     ..Lepcha Consonant Sign T
        ($01c36, $01c37),  // Lepcha Sign Ran         ..Lepcha Sign Nukta
        ($01cd0, $01cd2),  // Vedic Tone Karshana     ..Vedic Tone Prenkha
        ($01cd4, $01ce0),  // Vedic Sign Yajurvedic Mi..Vedic Tone Rigvedic Kash
        ($01ce2, $01ce8),  // Vedic Sign Visarga Svari..Vedic Sign Visarga Anuda
        ($01ced, $01ced),  // Vedic Sign Tiryak       ..Vedic Sign Tiryak
        ($01cf4, $01cf4),  // Vedic Tone Candra Above ..Vedic Tone Candra Above
        ($01cf8, $01cf9),  // Vedic Tone Ring Above   ..Vedic Tone Double Ring A
        ($01dc0, $01df9),  // Combining Dotted Grave A..Combining Wide Inverted
        ($01dfb, $01dff),  // Combining Deletion Mark ..Combining Right Arrowhea
        ($020d0, $020f0),  // Combining Left Harpoon A..Combining Asterisk Above
        ($02cef, $02cf1),  // Coptic Combining Ni Abov..Coptic Combining Spiritu
        ($02d7f, $02d7f),  // Tifinagh Consonant Joine..Tifinagh Consonant Joine
        ($02de0, $02dff),  // Combining Cyrillic Lette..Combining Cyrillic Lette
        ($0302a, $0302d),  // Ideographic Level Tone M..Ideographic Entering Ton
        ($03099, $0309a),  // Combining Katakana-hirag..Combining Katakana-hirag
        ($0a66f, $0a672),  // Combining Cyrillic Vzmet..Combining Cyrillic Thous
        ($0a674, $0a67d),  // Combining Cyrillic Lette..Combining Cyrillic Payer
        ($0a69e, $0a69f),  // Combining Cyrillic Lette..Combining Cyrillic Lette
        ($0a6f0, $0a6f1),  // Bamum Combining Mark Koq..Bamum Combining Mark Tuk
        ($0a802, $0a802),  // Syloti Nagri Sign Dvisva..Syloti Nagri Sign Dvisva
        ($0a806, $0a806),  // Syloti Nagri Sign Hasant..Syloti Nagri Sign Hasant
        ($0a80b, $0a80b),  // Syloti Nagri Sign Anusva..Syloti Nagri Sign Anusva
        ($0a825, $0a826),  // Syloti Nagri Vowel Sign ..Syloti Nagri Vowel Sign
        ($0a82c, $0a82c),  // (nil)                   ..(nil)
        ($0a8c4, $0a8c5),  // Saurashtra Sign Virama  ..Saurashtra Sign Candrabi
        ($0a8e0, $0a8f1),  // Combining Devanagari Dig..Combining Devanagari Sig
        ($0a8ff, $0a8ff),  // Devanagari Vowel Sign Ay..Devanagari Vowel Sign Ay
        ($0a926, $0a92d),  // Kayah Li Vowel Ue       ..Kayah Li Tone Calya Plop
        ($0a947, $0a951),  // Rejang Vowel Sign I     ..Rejang Consonant Sign R
        ($0a980, $0a982),  // Javanese Sign Panyangga ..Javanese Sign Layar
        ($0a9b3, $0a9b3),  // Javanese Sign Cecak Telu..Javanese Sign Cecak Telu
        ($0a9b6, $0a9b9),  // Javanese Vowel Sign Wulu..Javanese Vowel Sign Suku
        ($0a9bc, $0a9bd),  // Javanese Vowel Sign Pepe..Javanese Consonant Sign
        ($0a9e5, $0a9e5),  // Myanmar Sign Shan Saw   ..Myanmar Sign Shan Saw
        ($0aa29, $0aa2e),  // Cham Vowel Sign Aa      ..Cham Vowel Sign Oe
        ($0aa31, $0aa32),  // Cham Vowel Sign Au      ..Cham Vowel Sign Ue
        ($0aa35, $0aa36),  // Cham Consonant Sign La  ..Cham Consonant Sign Wa
        ($0aa43, $0aa43),  // Cham Consonant Sign Fina..Cham Consonant Sign Fina
        ($0aa4c, $0aa4c),  // Cham Consonant Sign Fina..Cham Consonant Sign Fina
        ($0aa7c, $0aa7c),  // Myanmar Sign Tai Laing T..Myanmar Sign Tai Laing T
        ($0aab0, $0aab0),  // Tai Viet Mai Kang       ..Tai Viet Mai Kang
        ($0aab2, $0aab4),  // Tai Viet Vowel I        ..Tai Viet Vowel U
        ($0aab7, $0aab8),  // Tai Viet Mai Khit       ..Tai Viet Vowel Ia
        ($0aabe, $0aabf),  // Tai Viet Vowel Am       ..Tai Viet Tone Mai Ek
        ($0aac1, $0aac1),  // Tai Viet Tone Mai Tho   ..Tai Viet Tone Mai Tho
        ($0aaec, $0aaed),  // Meetei Mayek Vowel Sign ..Meetei Mayek Vowel Sign
        ($0aaf6, $0aaf6),  // Meetei Mayek Virama     ..Meetei Mayek Virama
        ($0abe5, $0abe5),  // Meetei Mayek Vowel Sign ..Meetei Mayek Vowel Sign
        ($0abe8, $0abe8),  // Meetei Mayek Vowel Sign ..Meetei Mayek Vowel Sign
        ($0abed, $0abed),  // Meetei Mayek Apun Iyek  ..Meetei Mayek Apun Iyek
        ($0fb1e, $0fb1e),  // Hebrew Point Judeo-spani..Hebrew Point Judeo-spani
        ($0fe00, $0fe0f),  // Variation Selector-1    ..Variation Selector-16
        ($0fe20, $0fe2f),  // Combining Ligature Left ..Combining Cyrillic Titlo
        ($101fd, $101fd),  // Phaistos Disc Sign Combi..Phaistos Disc Sign Combi
        ($102e0, $102e0),  // Coptic Epact Thousands M..Coptic Epact Thousands M
        ($10376, $1037a),  // Combining Old Permic Let..Combining Old Permic Let
        ($10a01, $10a03),  // Kharoshthi Vowel Sign I ..Kharoshthi Vowel Sign Vo
        ($10a05, $10a06),  // Kharoshthi Vowel Sign E ..Kharoshthi Vowel Sign O
        ($10a0c, $10a0f),  // Kharoshthi Vowel Length ..Kharoshthi Sign Visarga
        ($10a38, $10a3a),  // Kharoshthi Sign Bar Abov..Kharoshthi Sign Dot Belo
        ($10a3f, $10a3f),  // Kharoshthi Virama       ..Kharoshthi Virama
        ($10ae5, $10ae6),  // Manichaean Abbreviation ..Manichaean Abbreviation
        ($10d24, $10d27),  // Hanifi Rohingya Sign Har..Hanifi Rohingya Sign Tas
        ($10eab, $10eac),  // (nil)                   ..(nil)
        ($10f46, $10f50),  // Sogdian Combining Dot Be..Sogdian Combining Stroke
        ($11001, $11001),  // Brahmi Sign Anusvara    ..Brahmi Sign Anusvara
        ($11038, $11046),  // Brahmi Vowel Sign Aa    ..Brahmi Virama
        ($1107f, $11081),  // Brahmi Number Joiner    ..Kaithi Sign Anusvara
        ($110b3, $110b6),  // Kaithi Vowel Sign U     ..Kaithi Vowel Sign Ai
        ($110b9, $110ba),  // Kaithi Sign Virama      ..Kaithi Sign Nukta
        ($11100, $11102),  // Chakma Sign Candrabindu ..Chakma Sign Visarga
        ($11127, $1112b),  // Chakma Vowel Sign A     ..Chakma Vowel Sign Uu
        ($1112d, $11134),  // Chakma Vowel Sign Ai    ..Chakma Maayyaa
        ($11173, $11173),  // Mahajani Sign Nukta     ..Mahajani Sign Nukta
        ($11180, $11181),  // Sharada Sign Candrabindu..Sharada Sign Anusvara
        ($111b6, $111be),  // Sharada Vowel Sign U    ..Sharada Vowel Sign O
        ($111c9, $111cc),  // Sharada Sandhi Mark     ..Sharada Extra Short Vowe
        ($111cf, $111cf),  // (nil)                   ..(nil)
        ($1122f, $11231),  // Khojki Vowel Sign U     ..Khojki Vowel Sign Ai
        ($11234, $11234),  // Khojki Sign Anusvara    ..Khojki Sign Anusvara
        ($11236, $11237),  // Khojki Sign Nukta       ..Khojki Sign Shadda
        ($1123e, $1123e),  // Khojki Sign Sukun       ..Khojki Sign Sukun
        ($112df, $112df),  // Khudawadi Sign Anusvara ..Khudawadi Sign Anusvara
        ($112e3, $112ea),  // Khudawadi Vowel Sign U  ..Khudawadi Sign Virama
        ($11300, $11301),  // Grantha Sign Combining A..Grantha Sign Candrabindu
        ($1133b, $1133c),  // Combining Bindu Below   ..Grantha Sign Nukta
        ($11340, $11340),  // Grantha Vowel Sign Ii   ..Grantha Vowel Sign Ii
        ($11366, $1136c),  // Combining Grantha Digit ..Combining Grantha Digit
        ($11370, $11374),  // Combining Grantha Letter..Combining Grantha Letter
        ($11438, $1143f),  // Newa Vowel Sign U       ..Newa Vowel Sign Ai
        ($11442, $11444),  // Newa Sign Virama        ..Newa Sign Anusvara
        ($11446, $11446),  // Newa Sign Nukta         ..Newa Sign Nukta
        ($1145e, $1145e),  // Newa Sandhi Mark        ..Newa Sandhi Mark
        ($114b3, $114b8),  // Tirhuta Vowel Sign U    ..Tirhuta Vowel Sign Vocal
        ($114ba, $114ba),  // Tirhuta Vowel Sign Short..Tirhuta Vowel Sign Short
        ($114bf, $114c0),  // Tirhuta Sign Candrabindu..Tirhuta Sign Anusvara
        ($114c2, $114c3),  // Tirhuta Sign Virama     ..Tirhuta Sign Nukta
        ($115b2, $115b5),  // Siddham Vowel Sign U    ..Siddham Vowel Sign Vocal
        ($115bc, $115bd),  // Siddham Sign Candrabindu..Siddham Sign Anusvara
        ($115bf, $115c0),  // Siddham Sign Virama     ..Siddham Sign Nukta
        ($115dc, $115dd),  // Siddham Vowel Sign Alter..Siddham Vowel Sign Alter
        ($11633, $1163a),  // Modi Vowel Sign U       ..Modi Vowel Sign Ai
        ($1163d, $1163d),  // Modi Sign Anusvara      ..Modi Sign Anusvara
        ($1163f, $11640),  // Modi Sign Virama        ..Modi Sign Ardhacandra
        ($116ab, $116ab),  // Takri Sign Anusvara     ..Takri Sign Anusvara
        ($116ad, $116ad),  // Takri Vowel Sign Aa     ..Takri Vowel Sign Aa
        ($116b0, $116b5),  // Takri Vowel Sign U      ..Takri Vowel Sign Au
        ($116b7, $116b7),  // Takri Sign Nukta        ..Takri Sign Nukta
        ($1171d, $1171f),  // Ahom Consonant Sign Medi..Ahom Consonant Sign Medi
        ($11722, $11725),  // Ahom Vowel Sign I       ..Ahom Vowel Sign Uu
        ($11727, $1172b),  // Ahom Vowel Sign Aw      ..Ahom Sign Killer
        ($1182f, $11837),  // Dogra Vowel Sign U      ..Dogra Sign Anusvara
        ($11839, $1183a),  // Dogra Sign Virama       ..Dogra Sign Nukta
        ($1193b, $1193c),  // (nil)                   ..(nil)
        ($1193e, $1193e),  // (nil)                   ..(nil)
        ($11943, $11943),  // (nil)                   ..(nil)
        ($119d4, $119d7),  // Nandinagari Vowel Sign U..Nandinagari Vowel Sign V
        ($119da, $119db),  // Nandinagari Vowel Sign E..Nandinagari Vowel Sign A
        ($119e0, $119e0),  // Nandinagari Sign Virama ..Nandinagari Sign Virama
        ($11a01, $11a0a),  // Zanabazar Square Vowel S..Zanabazar Square Vowel L
        ($11a33, $11a38),  // Zanabazar Square Final C..Zanabazar Square Sign An
        ($11a3b, $11a3e),  // Zanabazar Square Cluster..Zanabazar Square Cluster
        ($11a47, $11a47),  // Zanabazar Square Subjoin..Zanabazar Square Subjoin
        ($11a51, $11a56),  // Soyombo Vowel Sign I    ..Soyombo Vowel Sign Oe
        ($11a59, $11a5b),  // Soyombo Vowel Sign Vocal..Soyombo Vowel Length Mar
        ($11a8a, $11a96),  // Soyombo Final Consonant ..Soyombo Sign Anusvara
        ($11a98, $11a99),  // Soyombo Gemination Mark ..Soyombo Subjoiner
        ($11c30, $11c36),  // Bhaiksuki Vowel Sign I  ..Bhaiksuki Vowel Sign Voc
        ($11c38, $11c3d),  // Bhaiksuki Vowel Sign E  ..Bhaiksuki Sign Anusvara
        ($11c3f, $11c3f),  // Bhaiksuki Sign Virama   ..Bhaiksuki Sign Virama
        ($11c92, $11ca7),  // Marchen Subjoined Letter..Marchen Subjoined Letter
        ($11caa, $11cb0),  // Marchen Subjoined Letter..Marchen Vowel Sign Aa
        ($11cb2, $11cb3),  // Marchen Vowel Sign U    ..Marchen Vowel Sign E
        ($11cb5, $11cb6),  // Marchen Sign Anusvara   ..Marchen Sign Candrabindu
        ($11d31, $11d36),  // Masaram Gondi Vowel Sign..Masaram Gondi Vowel Sign
        ($11d3a, $11d3a),  // Masaram Gondi Vowel Sign..Masaram Gondi Vowel Sign
        ($11d3c, $11d3d),  // Masaram Gondi Vowel Sign..Masaram Gondi Vowel Sign
        ($11d3f, $11d45),  // Masaram Gondi Vowel Sign..Masaram Gondi Virama
        ($11d47, $11d47),  // Masaram Gondi Ra-kara   ..Masaram Gondi Ra-kara
        ($11d90, $11d91),  // Gunjala Gondi Vowel Sign..Gunjala Gondi Vowel Sign
        ($11d95, $11d95),  // Gunjala Gondi Sign Anusv..Gunjala Gondi Sign Anusv
        ($11d97, $11d97),  // Gunjala Gondi Virama    ..Gunjala Gondi Virama
        ($11ef3, $11ef4),  // Makasar Vowel Sign I    ..Makasar Vowel Sign U
        ($16af0, $16af4),  // Bassa Vah Combining High..Bassa Vah Combining High
        ($16b30, $16b36),  // Pahawh Hmong Mark Cim Tu..Pahawh Hmong Mark Cim Ta
        ($16f4f, $16f4f),  // Miao Sign Consonant Modi..Miao Sign Consonant Modi
        ($16f8f, $16f92),  // Miao Tone Right         ..Miao Tone Below
        ($16fe4, $16fe4),  // (nil)                   ..(nil)
        ($1bc9d, $1bc9e),  // Duployan Thick Letter Se..Duployan Double Mark
        ($1d167, $1d169),  // Musical Symbol Combining..Musical Symbol Combining
        ($1d17b, $1d182),  // Musical Symbol Combining..Musical Symbol Combining
        ($1d185, $1d18b),  // Musical Symbol Combining..Musical Symbol Combining
        ($1d1aa, $1d1ad),  // Musical Symbol Combining..Musical Symbol Combining
        ($1d242, $1d244),  // Combining Greek Musical ..Combining Greek Musical
        ($1da00, $1da36),  // Signwriting Head Rim    ..Signwriting Air Sucking
        ($1da3b, $1da6c),  // Signwriting Mouth Closed..Signwriting Excitement
        ($1da75, $1da75),  // Signwriting Upper Body T..Signwriting Upper Body T
        ($1da84, $1da84),  // Signwriting Location Hea..Signwriting Location Hea
        ($1da9b, $1da9f),  // Signwriting Fill Modifie..Signwriting Fill Modifie
        ($1daa1, $1daaf),  // Signwriting Rotation Mod..Signwriting Rotation Mod
        ($1e000, $1e006),  // Combining Glagolitic Let..Combining Glagolitic Let
        ($1e008, $1e018),  // Combining Glagolitic Let..Combining Glagolitic Let
        ($1e01b, $1e021),  // Combining Glagolitic Let..Combining Glagolitic Let
        ($1e023, $1e024),  // Combining Glagolitic Let..Combining Glagolitic Let
        ($1e026, $1e02a),  // Combining Glagolitic Let..Combining Glagolitic Let
        ($1e130, $1e136),  // Nyiakeng Puachue Hmong T..Nyiakeng Puachue Hmong T
        ($1e2ec, $1e2ef),  // Wancho Tone Tup         ..Wancho Tone Koini
        ($1e8d0, $1e8d6),  // Mende Kikakui Combining ..Mende Kikakui Combining
        ($1e944, $1e94a),  // Adlam Alif Lengthener   ..Adlam Nukta
        ($e0100, $e01ef)   // Variation Selector-17   ..Variation Selector-256
  );

  // https://github.com/jquast/wcwidth/blob/master/wcwidth/table_wide.py
  // at commit b29897e5a1b403a0e36f7fc991614981cbc42475 (2020-07-14):
  WIDE_EASTASIAN: array[0..115] of Twidth_interval = (
        ($01100, $0115f),  // Hangul Choseong Kiyeok  ..Hangul Choseong Filler
        ($0231a, $0231b),  // Watch                   ..Hourglass
        ($02329, $0232a),  // Left-pointing Angle Brac..Right-pointing Angle Bra
        ($023e9, $023ec),  // Black Right-pointing Dou..Black Down-pointing Doub
        ($023f0, $023f0),  // Alarm Clock             ..Alarm Clock
        ($023f3, $023f3),  // Hourglass With Flowing S..Hourglass With Flowing S
        ($025fd, $025fe),  // White Medium Small Squar..Black Medium Small Squar
        ($02614, $02615),  // Umbrella With Rain Drops..Hot Beverage
        ($02648, $02653),  // Aries                   ..Pisces
        ($0267f, $0267f),  // Wheelchair Symbol       ..Wheelchair Symbol
        ($02693, $02693),  // Anchor                  ..Anchor
        ($026a1, $026a1),  // High Voltage Sign       ..High Voltage Sign
        ($026aa, $026ab),  // Medium White Circle     ..Medium Black Circle
        ($026bd, $026be),  // Soccer Ball             ..Baseball
        ($026c4, $026c5),  // Snowman Without Snow    ..Sun Behind Cloud
        ($026ce, $026ce),  // Ophiuchus               ..Ophiuchus
        ($026d4, $026d4),  // No Entry                ..No Entry
        ($026ea, $026ea),  // Church                  ..Church
        ($026f2, $026f3),  // Fountain                ..Flag In Hole
        ($026f5, $026f5),  // Sailboat                ..Sailboat
        ($026fa, $026fa),  // Tent                    ..Tent
        ($026fd, $026fd),  // Fuel Pump               ..Fuel Pump
        ($02705, $02705),  // White Heavy Check Mark  ..White Heavy Check Mark
        ($0270a, $0270b),  // Raised Fist             ..Raised Hand
        ($02728, $02728),  // Sparkles                ..Sparkles
        ($0274c, $0274c),  // Cross Mark              ..Cross Mark
        ($0274e, $0274e),  // Negative Squared Cross M..Negative Squared Cross M
        ($02753, $02755),  // Black Question Mark Orna..White Exclamation Mark O
        ($02757, $02757),  // Heavy Exclamation Mark S..Heavy Exclamation Mark S
        ($02795, $02797),  // Heavy Plus Sign         ..Heavy Division Sign
        ($027b0, $027b0),  // Curly Loop              ..Curly Loop
        ($027bf, $027bf),  // Double Curly Loop       ..Double Curly Loop
        ($02b1b, $02b1c),  // Black Large Square      ..White Large Square
        ($02b50, $02b50),  // White Medium Star       ..White Medium Star
        ($02b55, $02b55),  // Heavy Large Circle      ..Heavy Large Circle
        ($02e80, $02e99),  // Cjk Radical Repeat      ..Cjk Radical Rap
        ($02e9b, $02ef3),  // Cjk Radical Choke       ..Cjk Radical C-simplified
        ($02f00, $02fd5),  // Kangxi Radical One      ..Kangxi Radical Flute
        ($02ff0, $02ffb),  // Ideographic Description ..Ideographic Description
        ($03000, $0303e),  // Ideographic Space       ..Ideographic Variation In
        ($03041, $03096),  // Hiragana Letter Small A ..Hiragana Letter Small Ke
        ($03099, $030ff),  // Combining Katakana-hirag..Katakana Digraph Koto
        ($03105, $0312f),  // Bopomofo Letter B       ..Bopomofo Letter Nn
        ($03131, $0318e),  // Hangul Letter Kiyeok    ..Hangul Letter Araeae
        ($03190, $031e3),  // Ideographic Annotation L..Cjk Stroke Q
        ($031f0, $0321e),  // Katakana Letter Small Ku..Parenthesized Korean Cha
        ($03220, $03247),  // Parenthesized Ideograph ..Circled Ideograph Koto
        ($03250, $04dbf),  // Partnership Sign        ..(nil)
        ($04e00, $0a48c),  // Cjk Unified Ideograph-4e..Yi Syllable Yyr
        ($0a490, $0a4c6),  // Yi Radical Qot          ..Yi Radical Ke
        ($0a960, $0a97c),  // Hangul Choseong Tikeut-m..Hangul Choseong Ssangyeo
        ($0ac00, $0d7a3),  // Hangul Syllable Ga      ..Hangul Syllable Hih
        ($0f900, $0faff),  // Cjk Compatibility Ideogr..(nil)
        ($0fe10, $0fe19),  // Presentation Form For Ve..Presentation Form For Ve
        ($0fe30, $0fe52),  // Presentation Form For Ve..Small Full Stop
        ($0fe54, $0fe66),  // Small Semicolon         ..Small Equals Sign
        ($0fe68, $0fe6b),  // Small Reverse Solidus   ..Small Commercial At
        ($0ff01, $0ff60),  // Fullwidth Exclamation Ma..Fullwidth Right White Pa
        ($0ffe0, $0ffe6),  // Fullwidth Cent Sign     ..Fullwidth Won Sign
        ($16fe0, $16fe4),  // Tangut Iteration Mark   ..(nil)
        ($16ff0, $16ff1),  // (nil)                   ..(nil)
        ($17000, $187f7),  // (nil)                   ..(nil)
        ($18800, $18cd5),  // Tangut Component-001    ..(nil)
        ($18d00, $18d08),  // (nil)                   ..(nil)
        ($1b000, $1b11e),  // Katakana Letter Archaic ..Hentaigana Letter N-mu-m
        ($1b150, $1b152),  // Hiragana Letter Small Wi..Hiragana Letter Small Wo
        ($1b164, $1b167),  // Katakana Letter Small Wi..Katakana Letter Small N
        ($1b170, $1b2fb),  // Nushu Character-1b170   ..Nushu Character-1b2fb
        ($1f004, $1f004),  // Mahjong Tile Red Dragon ..Mahjong Tile Red Dragon
        ($1f0cf, $1f0cf),  // Playing Card Black Joker..Playing Card Black Joker
        ($1f18e, $1f18e),  // Negative Squared Ab     ..Negative Squared Ab
        ($1f191, $1f19a),  // Squared Cl              ..Squared Vs
        ($1f200, $1f202),  // Square Hiragana Hoka    ..Squared Katakana Sa
        ($1f210, $1f23b),  // Squared Cjk Unified Ideo..Squared Cjk Unified Ideo
        ($1f240, $1f248),  // Tortoise Shell Bracketed..Tortoise Shell Bracketed
        ($1f250, $1f251),  // Circled Ideograph Advant..Circled Ideograph Accept
        ($1f260, $1f265),  // Rounded Symbol For Fu   ..Rounded Symbol For Cai
        ($1f300, $1f320),  // Cyclone                 ..Shooting Star
        ($1f32d, $1f335),  // Hot Dog                 ..Cactus
        ($1f337, $1f37c),  // Tulip                   ..Baby Bottle
        ($1f37e, $1f393),  // Bottle With Popping Cork..Graduation Cap
        ($1f3a0, $1f3ca),  // Carousel Horse          ..Swimmer
        ($1f3cf, $1f3d3),  // Cricket Bat And Ball    ..Table Tennis Paddle And
        ($1f3e0, $1f3f0),  // House Building          ..European Castle
        ($1f3f4, $1f3f4),  // Waving Black Flag       ..Waving Black Flag
        ($1f3f8, $1f43e),  // Badminton Racquet And Sh..Paw Prints
        ($1f440, $1f440),  // Eyes                    ..Eyes
        ($1f442, $1f4fc),  // Ear                     ..Videocassette
        ($1f4ff, $1f53d),  // Prayer Beads            ..Down-pointing Small Red
        ($1f54b, $1f54e),  // Kaaba                   ..Menorah With Nine Branch
        ($1f550, $1f567),  // Clock Face One Oclock   ..Clock Face Twelve-thirty
        ($1f57a, $1f57a),  // Man Dancing             ..Man Dancing
        ($1f595, $1f596),  // Reversed Hand With Middl..Raised Hand With Part Be
        ($1f5a4, $1f5a4),  // Black Heart             ..Black Heart
        ($1f5fb, $1f64f),  // Mount Fuji              ..Person With Folded Hands
        ($1f680, $1f6c5),  // Rocket                  ..Left Luggage
        ($1f6cc, $1f6cc),  // Sleeping Accommodation  ..Sleeping Accommodation
        ($1f6d0, $1f6d2),  // Place Of Worship        ..Shopping Trolley
        ($1f6d5, $1f6d7),  // Hindu Temple            ..(nil)
        ($1f6eb, $1f6ec),  // Airplane Departure      ..Airplane Arriving
        ($1f6f4, $1f6fc),  // Scooter                 ..(nil)
        ($1f7e0, $1f7eb),  // Large Orange Circle     ..Large Brown Square
        ($1f90c, $1f93a),  // (nil)                   ..Fencer
        ($1f93c, $1f945),  // Wrestlers               ..Goal Net
        ($1f947, $1f978),  // First Place Medal       ..(nil)
        ($1f97a, $1f9cb),  // Face With Pleading Eyes ..(nil)
        ($1f9cd, $1f9ff),  // Standing Person         ..Nazar Amulet
        ($1fa70, $1fa74),  // Ballet Shoes            ..(nil)
        ($1fa78, $1fa7a),  // Drop Of Blood           ..Stethoscope
        ($1fa80, $1fa86),  // Yo-yo                   ..(nil)
        ($1fa90, $1faa8),  // Ringed Planet           ..(nil)
        ($1fab0, $1fab6),  // (nil)                   ..(nil)
        ($1fac0, $1fac2),  // (nil)                   ..(nil)
        ($1fad0, $1fad6),  // (nil)                   ..(nil)
        ($20000, $2fffd),  // Cjk Unified Ideograph-20..(nil)
        ($30000, $3fffd)   // (nil)                   ..(nil)
  );

function intable(table: Pwidth_interval; table_length: Integer; c: UCS4Char): Boolean;
var
  bot, top, mid: Integer;
begin
  // First quick check for Latin1 etc. characters.
  if (c < table[0, 0]) then Exit(false);

  // Binary search in table.
  bot := 0;
  top := table_length - 1;
  while (top >= bot) do
  begin
    mid := (bot + top) div 2;
    if (table[mid, 1] < c) then
      bot := mid + 1
    else if (table[mid, 0] > c) then
      top := mid - 1
    else begin
      Exit(true);
    end;
  end;
  Result := false;
end;

function wcwidth(ucs: UCS4Char): Integer;
begin
  // NOTE: created by hand, there isn't anything identifiable other than
  // general Cf category code to identify these, and some characters in Cf
  // category code are of non-zero width.
  if ((ucs = 0) or
      (ucs = $034F) or
      (($200B <= ucs) and (ucs <= $200F)) or
      (ucs = $2028) or
      (ucs = $2029) or
      (($202A <= ucs) and (ucs <= $202E)) or
      (($2060 <= ucs) and (ucs <= $2063))) then
  begin
    Exit(0);
  end;

  // C0/C1 control characters.
  if ((ucs < 32) or (($07F <= ucs) and (ucs < $0A0))) then Exit(-1);

  // Combining characters with zero width.
  if (intable(ZERO_WIDTH, Length(ZERO_WIDTH), ucs)) then Exit(0);

  if intable(WIDE_EASTASIAN, Length(WIDE_EASTASIAN), ucs) then
    Result := 2
  else
    Result := 1;
end;

function UTF8Width(const Ch: TUTF8Char): Integer;
var
  CharLen: Integer;
begin
  Result:= wcwidth(UTF8CodepointToUnicode(@Ch[1], CharLen));
end;

end.
