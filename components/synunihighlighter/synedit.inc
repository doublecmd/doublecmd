(******************************************************************************)
(* SynEdit Include File.  This file was adapted from Brad Stowers' DFS.INC    *)
(* file and used with permission.  This will help reduce headaches when new   *)
(* versions of Delphi and C++Builder are released, among other things.        *)
(******************************************************************************)
(* Brad Stowers: bstowers@pobox.com                                           *)
(* Delphi Free Stuff: http://delphifreestuff.com/                             *)
(* February 24, 1999                                                          *)
(******************************************************************************)
(*                                                                            *)
(* Complete Boolean Evaluation compiler directive is turned off by including  *)
(*   this file.                                                               *)
(*                                                                            *)
(* Here is a brief explanation of what each of the defines mean:              *)
(* SYN_WIN32            : Compilation target is 32-bit Windows                *)
(******************************************************************************)

{$IFDEF FPC}
  {$MODE OBJFPC}
{$ENDIF}

{$DEFINE SYNEDIT_INCLUDE}

{$IFdef MSWindows}
  {$DEFINE SYN_WIN32}
{$ENDIF}

{------------------------------------------------------------------------------}
{ Common compiler defines                                                      }
{------------------------------------------------------------------------------}

// defaults are short evaluation of boolean values and long strings

// lazarus change   no $B-
{$H+}

{------------------------------------------------------------------------------}
{ Please change this to suit your needs                                        }
{------------------------------------------------------------------------------}

// support for multibyte character sets
{.$DEFINE SYN_MBCSSUPPORT}

// additional tests for debugging

{.$DEFINE SYN_DEVELOPMENT_CHECKS}

{$IFDEF SYN_DEVELOPMENT_CHECKS}

{$R+,Q+,S+,T+}

{$ENDIF}

// $Id: synedit.inc 42184 2013-07-23 18:44:37Z martin $

