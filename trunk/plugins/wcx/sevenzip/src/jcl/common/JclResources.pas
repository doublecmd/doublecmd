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
{ The Original Code is JclResources.pas.                                                           }
{                                                                                                  }
{ The Initial Developer of the Original Code is Marcel van Brakel.                                 }
{ Portions created by Marcel van Brakel are Copyright (C) Marcel van Brakel. All rights reserved.  }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Alexei Koudinov                                                                                }
{   Barry Kelly                                                                                    }
{   Flier Lu (flier)                                                                               }
{   Florent Ouchet (outchy)                                                                        }
{   Jean-Fabien Connault (cycocrew)                                                                }
{   Marcel Bestebroer                                                                              }
{   Marcel van Brakel                                                                              }
{   Matthias Thoma (mthoma)                                                                        }
{   Peter Friese                                                                                   }
{   Petr Vones (pvones)                                                                            }
{   Raymond Alexander (rayspostbox3)                                                               }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{   Scott Price (scottprice)                                                                       }
{   Uwe Schuster (uschuster)                                                                       }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Unit which provides a central place for all resource strings used in the JCL                     }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclResources;

{$I jcl.inc}

interface

{$IFDEF UNITVERSIONING}
uses
  JclUnitVersioning;
{$ENDIF UNITVERSIONING}

//=== JclBase ================================================================
resourcestring
  RsCantConvertAddr64  = 'The address %s%.16x cannot be converted to 32 bit';
  RsEReplacementChar   = 'Failed to get ANSI replacement character';

//=== JclCharsets ============================================================
resourcestring
  RsENoCharset = 'No matching charset';

//=== JclCIL =================================================================
resourcestring
  RsInstructionStreamInvalid = 'Invalid IL instruction stream';
  RsCILNamenop         = 'nop';
  RsCILNamebreak       = 'break';
  RsCILNameldarg0      = 'ldarg.0';
  RsCILNameldarg1      = 'ldarg.1';
  RsCILNameldarg2      = 'ldarg.2';
  RsCILNameldarg3      = 'ldarg.3';
  RsCILNameldloc0      = 'ldloc.0';
  RsCILNameldloc1      = 'ldloc.1';
  RsCILNameldloc2      = 'ldloc.2';
  RsCILNameldloc3      = 'ldloc.3';
  RsCILNamestloc0      = 'stloc.0';
  RsCILNamestloc1      = 'stloc.1';
  RsCILNamestloc2      = 'stloc.2';
  RsCILNamestloc3      = 'stloc.3';
  RsCILNameldargs      = 'ldarg.s';
  RsCILNameldargas     = 'ldarga.s';
  RsCILNamestargs      = 'starg.s';
  RsCILNameldlocs      = 'ldloc.s';
  RsCILNameldlocas     = 'ldloca.s';
  RsCILNamestlocs      = 'stloc.s';
  RsCILNameldnull      = 'ldnull';
  RsCILNameldci4m1     = 'ldc.i4.m1';
  RsCILNameldci40      = 'ldc.i4.0';
  RsCILNameldci41      = 'ldc.i4.1';
  RsCILNameldci42      = 'ldc.i4.2';
  RsCILNameldci43      = 'ldc.i4.3';
  RsCILNameldci44      = 'ldc.i4.4';
  RsCILNameldci45      = 'ldc.i4.5';
  RsCILNameldci46      = 'ldc.i4.6';
  RsCILNameldci47      = 'ldc.i4.7';
  RsCILNameldci48      = 'ldc.i4.8';
  RsCILNameldci4s      = 'ldc.i4.s';
  RsCILNameldci4       = 'ldc.i4';
  RsCILNameldci8       = 'ldc.i8';
  RsCILNameldcr4       = 'ldc.r4';
  RsCILNameldcr8       = 'ldc.r8';
  RsCILNameunused1     = 'unused';
  RsCILNamedup         = 'dup';
  RsCILNamepop         = 'pop';
  RsCILNamejmp         = 'jmp';
  RsCILNamecall        = 'call';
  RsCILNamecalli       = 'calli';
  RsCILNameret         = 'ret';
  RsCILNamebrs         = 'br.s';
  RsCILNamebrfalses    = 'brfalse.s';
  RsCILNamebrtrues     = 'brtrue.s';
  RsCILNamebeqs        = 'beq.s';
  RsCILNamebges        = 'bge.s';
  RsCILNamebgts        = 'bgt.s';
  RsCILNamebles        = 'ble.s';
  RsCILNameblts        = 'blt.s';
  RsCILNamebneuns      = 'bne.un.s';
  RsCILNamebgeuns      = 'bge.un.s';
  RsCILNamebgtuns      = 'bgt.un.s';
  RsCILNamebleuns      = 'ble.un.s';
  RsCILNamebltuns      = 'blt.un.s';
  RsCILNamebr          = 'br';
  RsCILNamebrfalse     = 'brfalse';
  RsCILNamebrtrue      = 'brtrue';
  RsCILNamebeq         = 'beq';
  RsCILNamebge         = 'bge';
  RsCILNamebgt         = 'bgt';
  RsCILNameble         = 'ble';
  RsCILNameblt         = 'blt';
  RsCILNamebneun       = 'bne.un';
  RsCILNamebgeun       = 'bge.un';
  RsCILNamebgtun       = 'bgt.un';
  RsCILNamebleun       = 'ble.un';
  RsCILNamebltun       = 'blt.un';
  RsCILNameswitch      = 'switch';
  RsCILNameldindi1     = 'ldind.i1';
  RsCILNameldindu1     = 'ldind.u1';
  RsCILNameldindi2     = 'ldind.i2';
  RsCILNameldindu2     = 'ldind.u2';
  RsCILNameldindi4     = 'ldind.i4';
  RsCILNameldindu4     = 'ldind.u4';
  RsCILNameldindi8     = 'ldind.i8';
  RsCILNameldindi      = 'ldind.i';
  RsCILNameldindr4     = 'ldind.r4';
  RsCILNameldindr8     = 'ldind.r8';
  RsCILNameldindref    = 'ldind.ref';
  RsCILNamestindref    = 'stind.ref';
  RsCILNamestindi1     = 'stind.i1';
  RsCILNamestindi2     = 'stind.i2';
  RsCILNamestindi4     = 'stind.i4';
  RsCILNamestindi8     = 'stind.i8';
  RsCILNamestindr4     = 'stind.r4';
  RsCILNamestindr8     = 'stind.r8';
  RsCILNameadd         = 'add';
  RsCILNamesub         = 'sub';
  RsCILNamemul         = 'mul';
  RsCILNamediv         = 'div';
  RsCILNamedivun       = 'div.un';
  RsCILNamerem         = 'rem';
  RsCILNameremun       = 'rem.un';
  RsCILNameand         = 'and';
  RsCILNameor          = 'or';
  RsCILNamexor         = 'xor';
  RsCILNameshl         = 'shl';
  RsCILNameshr         = 'shr';
  RsCILNameshrun       = 'shr.un';
  RsCILNameneg         = 'neg';
  RsCILNamenot         = 'not';
  RsCILNameconvi1      = 'conv.i1';
  RsCILNameconvi2      = 'conv.i2';
  RsCILNameconvi4      = 'conv.i4';
  RsCILNameconvi8      = 'conv.i8';
  RsCILNameconvr4      = 'conv.r4';
  RsCILNameconvr8      = 'conv.r8';
  RsCILNameconvu4      = 'conv.u4';
  RsCILNameconvu8      = 'conv.u8';
  RsCILNamecallvirt    = 'callvirt';
  RsCILNamecpobj       = 'cpobj';
  RsCILNameldobj       = 'ldobj';
  RsCILNameldstr       = 'ldstr';
  RsCILNamenewobj      = 'newobj';
  RsCILNamecastclass   = 'castclass';
  RsCILNameisinst      = 'isinst';
  RsCILNameconvrun     = 'conv.r.un';
  RsCILNameunused2     = 'unused';
  RsCILNameunused3     = 'unused';
  RsCILNameunbox       = 'unbox';
  RsCILNamethrow       = 'throw';
  RsCILNameldfld       = 'ldfld';
  RsCILNameldflda      = 'ldflda';
  RsCILNamestfld       = 'stfld';
  RsCILNameldsfld      = 'ldsfld';
  RsCILNameldsflda     = 'ldsflda';
  RsCILNamestsfld      = 'stsfld';
  RsCILNamestobj       = 'stobj';
  RsCILNameconvovfi1un = 'conv.ovf.i1.un';
  RsCILNameconvovfi2un = 'conv.ovf.i2.un';
  RsCILNameconvovfi4un = 'conv.ovf.i4.un';
  RsCILNameconvovfi8un = 'conv.ovf.i8.un';
  RsCILNameconvovfu1un = 'conv.ovf.u1.un';
  RsCILNameconvovfu2un = 'conv.ovf.u2.un';
  RsCILNameconvovfu4un = 'conv.ovf.u4.un';
  RsCILNameconvovfu8un = 'conv.ovf.u8.un';
  RsCILNameconvovfiun  = 'conv.ovf.i.un';
  RsCILNameconvovfuun  = 'conv.ovf.u.un';
  RsCILNamebox         = 'box';
  RsCILNamenewarr      = 'newarr';
  RsCILNameldlen       = 'ldlen';
  RsCILNameldelema     = 'ldelema';
  RsCILNameldelemi1    = 'ldelem.i1';
  RsCILNameldelemu1    = 'ldelem.u1';
  RsCILNameldelemi2    = 'ldelem.i2';
  RsCILNameldelemu2    = 'ldelem.u2';
  RsCILNameldelemi4    = 'ldelem.i4';
  RsCILNameldelemu4    = 'ldelem.u4';
  RsCILNameldelemi8    = 'ldelem.i8';
  RsCILNameldelemi     = 'ldelem.i';
  RsCILNameldelemr4    = 'ldelem.r4';
  RsCILNameldelemr8    = 'ldelem.r8';
  RsCILNameldelemref   = 'ldelem.ref';
  RsCILNamestelemi     = 'stelem.i';
  RsCILNamestelemi1    = 'stelem.i1';
  RsCILNamestelemi2    = 'stelem.i2';
  RsCILNamestelemi4    = 'stelem.i4';
  RsCILNamestelemi8    = 'stelem.i8';
  RsCILNamestelemr4    = 'stelem.r4';
  RsCILNamestelemr8    = 'stelem.r8';
  RsCILNamestelemref   = 'stelem.ref';
  RsCILNameunused4     = 'unused';
  RsCILNameunused5     = 'unused';
  RsCILNameunused6     = 'unused';
  RsCILNameunused7     = 'unused';
  RsCILNameunused8     = 'unused';
  RsCILNameunused9     = 'unused';
  RsCILNameunused10    = 'unused';
  RsCILNameunused11    = 'unused';
  RsCILNameunused12    = 'unused';
  RsCILNameunused13    = 'unused';
  RsCILNameunused14    = 'unused';
  RsCILNameunused15    = 'unused';
  RsCILNameunused16    = 'unused';
  RsCILNameunused17    = 'unused';
  RsCILNameunused18    = 'unused';
  RsCILNameunused19    = 'unused';
  RsCILNameconvovfi1   = 'conv.ovf.i1';
  RsCILNameconvovfu1   = 'conv.ovf.u1';
  RsCILNameconvovfi2   = 'conv.ovf.i2';
  RsCILNameconvovfu2   = 'conv.ovf.u2';
  RsCILNameconvovfi4   = 'conv.ovf.i4';
  RsCILNameconvovfu4   = 'conv.ovf.u4';
  RsCILNameconvovfi8   = 'conv.ovf.i8';
  RsCILNameconvovfu8   = 'conv.ovf.u8';
  RsCILNameunused20    = 'unused';
  RsCILNameunused21    = 'unused';
  RsCILNameunused22    = 'unused';
  RsCILNameunused23    = 'unused';
  RsCILNameunused24    = 'unused';
  RsCILNameunused25    = 'unused';
  RsCILNameunused26    = 'unused';
  RsCILNamerefanyval   = 'refanyval';
  RsCILNameckfinite    = 'ckfinite';
  RsCILNameunused27    = 'unused';
  RsCILNameunused28    = 'unused';
  RsCILNamemkrefany    = 'mkrefany';
  RsCILNameunused29    = 'unused';
  RsCILNameunused30    = 'unused';
  RsCILNameunused31    = 'unused';
  RsCILNameunused32    = 'unused';
  RsCILNameunused33    = 'unused';
  RsCILNameunused34    = 'unused';
  RsCILNameunused35    = 'unused';
  RsCILNameunused36    = 'unused';
  RsCILNameunused37    = 'unused';
  RsCILNameldtoken     = 'ldtoken';
  RsCILNameconvu2      = 'conv.u2';
  RsCILNameconvu1      = 'conv.u1';
  RsCILNameconvi       = 'conv.i';
  RsCILNameconvovfi    = 'conv.ovf.i';
  RsCILNameconvovfu    = 'conv.ovf.u';
  RsCILNameaddovf      = 'add.ovf';
  RsCILNameaddovfun    = 'add.ovf.un';
  RsCILNamemulovf      = 'mul.ovf';
  RsCILNamemulovfun    = 'mul.ovf.un';
  RsCILNamesubovf      = 'sub.ovf';
  RsCILNamesubovfun    = 'sub.ovf.un';
  RsCILNameendfinally  = 'endfinally';
  RsCILNameleave       = 'leave';
  RsCILNameleaves      = 'leave.s';
  RsCILNamestindi      = 'stind.i';
  RsCILNameconvu       = 'conv.u';
  RsCILNameunused38    = 'unused';
  RsCILNameunused39    = 'unused';
  RsCILNameunused40    = 'unused';
  RsCILNameunused41    = 'unused';
  RsCILNameunused42    = 'unused';
  RsCILNameunused43    = 'unused';
  RsCILNameunused44    = 'unused';
  RsCILNameunused45    = 'unused';
  RsCILNameunused46    = 'unused';
  RsCILNameunused47    = 'unused';
  RsCILNameunused48    = 'unused';
  RsCILNameunused49    = 'unused';
  RsCILNameunused50    = 'unused';
  RsCILNameunused51    = 'unused';
  RsCILNameunused52    = 'unused';
  RsCILNameunused53    = 'unused';
  RsCILNameunused54    = 'unused';
  RsCILNameunused55    = 'unused';
  RsCILNameunused56    = 'unused';
  RsCILNameunused57    = 'unused';
  RsCILNameunused58    = 'unused';
  RsCILNameunused59    = 'unused';
  RsCILNameunused60    = 'unused';
  RsCILNameprefix7     = 'prefix7';
  RsCILNameprefix6     = 'prefix6';
  RsCILNameprefix5     = 'prefix5';
  RsCILNameprefix4     = 'prefix4';
  RsCILNameprefix3     = 'prefix3';
  RsCILNameprefix2     = 'prefix2';
  RsCILNameprefix1     = 'prefix1';
  RsCILNameprefixref   = 'prefixref';
  RsCILNamearglist     = 'arglist';
  RsCILNameceq         = 'ceq';
  RsCILNamecgt         = 'cgt';
  RsCILNamecgtun       = 'cgt.un';
  RsCILNameclt         = 'clt';
  RsCILNamecltun       = 'clt.un';
  RsCILNameldftn       = 'ldftn';
  RsCILNameldvirtftn   = 'ldvirtftn';
  RsCILNameunused61    = 'unused';
  RsCILNameldarg       = 'ldarg';
  RsCILNameldarga      = 'ldarga';
  RsCILNamestarg       = 'starg';
  RsCILNameldloc       = 'ldloc';
  RsCILNameldloca      = 'ldloca';
  RsCILNamestloc       = 'stloc';
  RsCILNamelocalloc    = 'localloc';
  RsCILNameunused62    = 'unused';
  RsCILNameendfilter   = 'endfilter';
  RsCILNameunaligned   = 'unaligned.';
  RsCILNamevolatile    = 'volatile.';
  RsCILNametail        = 'tail.';
  RsCILNameinitobj     = 'initobj';
  RsCILNameunused63    = 'unused';
  RsCILNamecpblk       = 'cpblk';
  RsCILNameinitblk     = 'initblk';
  RsCILNameunused64    = 'unused';
  RsCILNamerethrow     = 'rethrow';
  RsCILNameunused65    = 'unused';
  RsCILNamesizeof      = 'sizeof';
  RsCILNamerefanytype  = 'refanytype';
  RsCILNameunused66    = 'unused';
  RsCILNameunused67    = 'unused';
  RsCILNameunused68    = 'unused';
  RsCILNameunused69    = 'unused';
  RsCILNameunused70    = 'unused';

  RsCILCmdnop         = 'no operation';
  RsCILCmdbreak       = 'breakpoint instruction';
  RsCILCmdldarg0      = 'load argument onto the stack';
  RsCILCmdldarg1      = 'load argument onto the stack';
  RsCILCmdldarg2      = 'load argument onto the stack';
  RsCILCmdldarg3      = 'load argument onto the stack';
  RsCILCmdldloc0      = 'load local variable onto the stack';
  RsCILCmdldloc1      = 'load local variable onto the stack';
  RsCILCmdldloc2      = 'load local variable onto the stack';
  RsCILCmdldloc3      = 'load local variable onto the stack';
  RsCILCmdstloc0      = 'pop value from stack to local variable';
  RsCILCmdstloc1      = 'pop value from stack to local variable';
  RsCILCmdstloc2      = 'pop value from stack to local variable';
  RsCILCmdstloc3      = 'pop value from stack to local variable';
  RsCILCmdldargs      = 'load argument onto the stack';
  RsCILCmdldargas     = 'load an argument address';
  RsCILCmdstargs      = 'store a value in an argument slot';
  RsCILCmdldlocs      = 'load local variable onto the stack';
  RsCILCmdldlocas     = 'load local variable address';
  RsCILCmdstlocs      = 'pop value from stack to local variable';
  RsCILCmdldnull      = 'load a null pointer';
  RsCILCmdldci4m1     = 'load numeric constant';
  RsCILCmdldci40      = 'load numeric constant';
  RsCILCmdldci41      = 'load numeric constant';
  RsCILCmdldci42      = 'load numeric constant';
  RsCILCmdldci43      = 'load numeric constant';
  RsCILCmdldci44      = 'load numeric constant';
  RsCILCmdldci45      = 'load numeric constant';
  RsCILCmdldci46      = 'load numeric constant';
  RsCILCmdldci47      = 'load numeric constant';
  RsCILCmdldci48      = 'load numeric constant';
  RsCILCmdldci4s      = 'load numeric constant';
  RsCILCmdldci4       = 'load numeric constant';
  RsCILCmdldci8       = 'load numeric constant';
  RsCILCmdldcr4       = 'load numeric constant';
  RsCILCmdldcr8       = 'load numeric constant';
  RsCILCmdunused1     = '';
  RsCILCmddup         = 'duplicate the top value of the stack';
  RsCILCmdpop         = 'remove the top element of the stack';
  RsCILCmdjmp         = 'jump to method';
  RsCILCmdcall        = 'call a method';
  RsCILCmdcalli       = 'indirect method call';
  RsCILCmdret         = 'return from method';
  RsCILCmdbrs         = 'unconditional branch';
  RsCILCmdbrfalses    = 'branch on false, null, or zero';
  RsCILCmdbrtrues     = 'branch on non-false or non-null';
  RsCILCmdbeqs        = 'branch on equal';
  RsCILCmdbges        = 'branch on greater than or equal to';
  RsCILCmdbgts        = 'branch on greater than';
  RsCILCmdbles        = 'branch on less than or equal to';
  RsCILCmdblts        = 'branch on less than';
  RsCILCmdbneuns      = 'branch on not equal or unordered';
  RsCILCmdbgeuns      = 'branch on greater than or equal to, unsigned or unordered';
  RsCILCmdbgtuns      = 'branch on greater than, unsigned or unordered';
  RsCILCmdbleuns      = 'branch on less than or equal to, unsigned or unordered';
  RsCILCmdbltuns      = 'branch on less than, unsigned or unordered';
  RsCILCmdbr          = 'unconditional branch';
  RsCILCmdbrfalse     = 'branch on false, null, or zero';
  RsCILCmdbrtrue      = 'branch on non-false or non-null';
  RsCILCmdbeq         = 'branch on equal';
  RsCILCmdbge         = 'branch on greater than or equal to';
  RsCILCmdbgt         = 'branch on greater than';
  RsCILCmdble         = 'branch on less than or equal to';
  RsCILCmdblt         = 'branch on less than';
  RsCILCmdbneun       = 'branch on not equal or unordered';
  RsCILCmdbgeun       = 'branch on greater than or equal to, unsigned or unordered';
  RsCILCmdbgtun       = 'branch on greater than, unsigned or unordered';
  RsCILCmdbleun       = 'branch on less than or equal to, unsigned or unordered';
  RsCILCmdbltun       = 'branch on less than, unsigned or unordered';
  RsCILCmdswitch      = 'table switch on value';
  RsCILCmdldindi1     = 'load value indirect onto the stack';
  RsCILCmdldindu1     = 'load value indirect onto the stack';
  RsCILCmdldindi2     = 'load value indirect onto the stack';
  RsCILCmdldindu2     = 'load value indirect onto the stack';
  RsCILCmdldindi4     = 'load value indirect onto the stack';
  RsCILCmdldindu4     = 'load value indirect onto the stack';
  RsCILCmdldindi8     = 'load value indirect onto the stack';
  RsCILCmdldindi      = 'load value indirect onto the stack';
  RsCILCmdldindr4     = 'load value indirect onto the stack';
  RsCILCmdldindr8     = 'load value indirect onto the stack';
  RsCILCmdldindref    = 'load value indirect onto the stack';
  RsCILCmdstindref    = 'store value indirect from stack';
  RsCILCmdstindi1     = 'store value indirect from stack';
  RsCILCmdstindi2     = 'store value indirect from stack';
  RsCILCmdstindi4     = 'store value indirect from stack';
  RsCILCmdstindi8     = 'store value indirect from stack';
  RsCILCmdstindr4     = 'store value indirect from stack';
  RsCILCmdstindr8     = 'store value indirect from stack';
  RsCILCmdadd         = 'add numeric values';
  RsCILCmdsub         = 'subtract numeric values';
  RsCILCmdmul         = 'multiply values';
  RsCILCmddiv         = 'divide values';
  RsCILCmddivun       = 'divide integer values, unsigned';
  RsCILCmdrem         = 'compute remainder';
  RsCILCmdremun       = 'compute integer remainder, unsigned';
  RsCILCmdand         = 'bitwise AND';
  RsCILCmdor          = 'bitwise OR';
  RsCILCmdxor         = 'bitwise XOR';
  RsCILCmdshl         = 'shift integer left';
  RsCILCmdshr         = 'shift integer right';
  RsCILCmdshrun       = 'shift integer right, unsigned';
  RsCILCmdneg         = 'negate';
  RsCILCmdnot         = 'bitwise complement';
  RsCILCmdconvi1      = 'data conversion';
  RsCILCmdconvi2      = 'data conversion';
  RsCILCmdconvi4      = 'data conversion';
  RsCILCmdconvi8      = 'data conversion';
  RsCILCmdconvr4      = 'data conversion';
  RsCILCmdconvr8      = 'data conversion';
  RsCILCmdconvu4      = 'data conversion';
  RsCILCmdconvu8      = 'data conversion';
  RsCILCmdcallvirt    = 'call a method associated, at runtime, with an object';
  RsCILCmdcpobj       = 'copy a value type';
  RsCILCmdldobj       = 'copy value type to the stack';
  RsCILCmdldstr       = 'load a literal string';
  RsCILCmdnewobj      = 'create a new object';
  RsCILCmdcastclass   = 'cast an object to a class';
  RsCILCmdisinst      = 'test if an object is an instance of a class or interface';
  RsCILCmdconvrun     = 'data conversion';
  RsCILCmdunused2     = '';
  RsCILCmdunused3     = '';
  RsCILCmdunbox       = 'Convert boxed value type to its raw form';
  RsCILCmdthrow       = 'throw an exception';
  RsCILCmdldfld       = 'load field of an object';
  RsCILCmdldflda      = 'load field address';
  RsCILCmdstfld       = 'store into a field of an object';
  RsCILCmdldsfld      = 'load static field of a class';
  RsCILCmdldsflda     = 'load static field address';
  RsCILCmdstsfld      = 'store a static field of a class';
  RsCILCmdstobj       = 'store a value type from the stack into memory';
  RsCILCmdconvovfi1un = 'unsigned data conversion with overflow detection';
  RsCILCmdconvovfi2un = 'unsigned data conversion with overflow detection';
  RsCILCmdconvovfi4un = 'unsigned data conversion with overflow detection';
  RsCILCmdconvovfi8un = 'unsigned data conversion with overflow detection';
  RsCILCmdconvovfu1un = 'unsigned data conversion with overflow detection';
  RsCILCmdconvovfu2un = 'unsigned data conversion with overflow detection';
  RsCILCmdconvovfu4un = 'unsigned data conversion with overflow detection';
  RsCILCmdconvovfu8un = 'unsigned data conversion with overflow detection';
  RsCILCmdconvovfiun  = 'unsigned data conversion with overflow detection';
  RsCILCmdconvovfuun  = 'unsigned data conversion with overflow detection';
  RsCILCmdbox         = 'convert value type to object reference';
  RsCILCmdnewarr      = 'create a zero-based, one-dimensional array';
  RsCILCmdldlen       = 'load the length of an array';
  RsCILCmdldelema     = 'load address of an element of an array';
  RsCILCmdldelemi1    = 'load an element of an array';
  RsCILCmdldelemu1    = 'load an element of an array';
  RsCILCmdldelemi2    = 'load an element of an array';
  RsCILCmdldelemu2    = 'load an element of an array';
  RsCILCmdldelemi4    = 'load an element of an array';
  RsCILCmdldelemu4    = 'load an element of an array';
  RsCILCmdldelemi8    = 'load an element of an array';
  RsCILCmdldelemi     = 'load an element of an array';
  RsCILCmdldelemr4    = 'load an element of an array';
  RsCILCmdldelemr8    = 'load an element of an array';
  RsCILCmdldelemref   = 'load an element of an array';
  RsCILCmdstelemi     = 'store an element of an array';
  RsCILCmdstelemi1    = 'store an element of an array';
  RsCILCmdstelemi2    = 'store an element of an array';
  RsCILCmdstelemi4    = 'store an element of an array';
  RsCILCmdstelemi8    = 'store an element of an array';
  RsCILCmdstelemr4    = 'store an element of an array';
  RsCILCmdstelemr8    = 'store an element of an array';
  RsCILCmdstelemref   = 'store an element of an array';
  RsCILCmdunused4     = '';
  RsCILCmdunused5     = '';
  RsCILCmdunused6     = '';
  RsCILCmdunused7     = '';
  RsCILCmdunused8     = '';
  RsCILCmdunused9     = '';
  RsCILCmdunused10    = '';
  RsCILCmdunused11    = '';
  RsCILCmdunused12    = '';
  RsCILCmdunused13    = '';
  RsCILCmdunused14    = '';
  RsCILCmdunused15    = '';
  RsCILCmdunused16    = '';
  RsCILCmdunused17    = '';
  RsCILCmdunused18    = '';
  RsCILCmdunused19    = '';
  RsCILCmdconvovfi1   = 'data conversion with overflow detection';
  RsCILCmdconvovfu1   = 'data conversion with overflow detection';
  RsCILCmdconvovfi2   = 'data conversion with overflow detection';
  RsCILCmdconvovfu2   = 'data conversion with overflow detection';
  RsCILCmdconvovfi4   = 'data conversion with overflow detection';
  RsCILCmdconvovfu4   = 'data conversion with overflow detection';
  RsCILCmdconvovfi8   = 'data conversion with overflow detection';
  RsCILCmdconvovfu8   = 'data conversion with overflow detection';
  RsCILCmdunused20    = '';
  RsCILCmdunused21    = '';
  RsCILCmdunused22    = '';
  RsCILCmdunused23    = '';
  RsCILCmdunused24    = '';
  RsCILCmdunused25    = '';
  RsCILCmdunused26    = '';
  RsCILCmdrefanyval   = 'load the address out of a typed reference';
  RsCILCmdckfinite    = 'check for a finite real number';
  RsCILCmdunused27    = '';
  RsCILCmdunused28    = '';
  RsCILCmdmkrefany    = 'push a typed reference on the stack';
  RsCILCmdunused29    = '';
  RsCILCmdunused30    = '';
  RsCILCmdunused31    = '';
  RsCILCmdunused32    = '';
  RsCILCmdunused33    = '';
  RsCILCmdunused34    = '';
  RsCILCmdunused35    = '';
  RsCILCmdunused36    = '';
  RsCILCmdunused37    = '';
  RsCILCmdldtoken     = 'load the runtime representation of a metadata token';
  RsCILCmdconvu2      = 'data conversion';
  RsCILCmdconvu1      = 'data conversion';
  RsCILCmdconvi       = 'data conversion';
  RsCILCmdconvovfi    = 'data conversion with overflow detection';
  RsCILCmdconvovfu    = 'data conversion with overflow detection';
  RsCILCmdaddovf      = 'add integer values with overflow check';
  RsCILCmdaddovfun    = 'add integer values with overflow check';
  RsCILCmdmulovf      = 'multiply integer values with overflow check';
  RsCILCmdmulovfun    = 'multiply integer values with overflow check';
  RsCILCmdsubovf      = 'subtract integer values, checking for overflow';
  RsCILCmdsubovfun    = 'subtract integer values, checking for overflow';
  RsCILCmdendfinally  = 'end the finally or fault clause of an exception block';
  RsCILCmdleave       = 'exit a protected region of code';
  RsCILCmdleaves      = 'exit a protected region of code';
  RsCILCmdstindi      = 'store value indirect from stack';
  RsCILCmdconvu       = 'data conversion';
  RsCILCmdunused38    = '';
  RsCILCmdunused39    = '';
  RsCILCmdunused40    = '';
  RsCILCmdunused41    = '';
  RsCILCmdunused42    = '';
  RsCILCmdunused43    = '';
  RsCILCmdunused44    = '';
  RsCILCmdunused45    = '';
  RsCILCmdunused46    = '';
  RsCILCmdunused47    = '';
  RsCILCmdunused48    = '';
  RsCILCmdunused49    = '';
  RsCILCmdunused50    = '';
  RsCILCmdunused51    = '';
  RsCILCmdunused52    = '';
  RsCILCmdunused53    = '';
  RsCILCmdunused54    = '';
  RsCILCmdunused55    = '';
  RsCILCmdunused56    = '';
  RsCILCmdunused57    = '';
  RsCILCmdunused58    = '';
  RsCILCmdunused59    = '';
  RsCILCmdunused60    = '';
  RsCILCmdprefix7     = '';
  RsCILCmdprefix6     = '';
  RsCILCmdprefix5     = '';
  RsCILCmdprefix4     = '';
  RsCILCmdprefix3     = '';
  RsCILCmdprefix2     = '';
  RsCILCmdprefix1     = '';
  RsCILCmdprefixref   = '';
  RsCILCmdarglist     = 'get argument list';
  RsCILCmdceq         = 'compare equal';
  RsCILCmdcgt         = 'compare greater than';
  RsCILCmdcgtun       = 'compare greater than, unsigned or unordered';
  RsCILCmdclt         = 'compare less than';
  RsCILCmdcltun       = 'compare less than, unsigned or unordered';
  RsCILCmdldftn       = 'load method pointer';
  RsCILCmdldvirtftn   = 'load a virtual method pointer';
  RsCILCmdunused61    = '';
  RsCILCmdldarg       = 'load argument onto the stack';
  RsCILCmdldarga      = 'load an argument address';
  RsCILCmdstarg       = 'store a value in an argument slot';
  RsCILCmdldloc       = 'load local variable onto the stack';
  RsCILCmdldloca      = 'load local variable address';
  RsCILCmdstloc       = 'pop value from stack to local variable';
  RsCILCmdlocalloc    = 'allocate space in the local dynamic memory pool';
  RsCILCmdunused62    = '';
  RsCILCmdendfilter   = 'end filter clause of SEH';
  RsCILCmdunaligned   = 'pointer instruction may be unaligned';
  RsCILCmdvolatile    = 'pointer reference is volatile';
  RsCILCmdtail        = 'call terminates current method';
  RsCILCmdinitobj     = 'initialize a value type';
  RsCILCmdunused63    = '';
  RsCILCmdcpblk       = 'copy data from memory to memory';
  RsCILCmdinitblk     = 'initialize a block of memory to a value';
  RsCILCmdunused64    = '';
  RsCILCmdrethrow     = 'rethrow the current exception';
  RsCILCmdunused65    = '';
  RsCILCmdsizeof      = 'load the size in bytes of a value type';
  RsCILCmdrefanytype  = 'load the type out of a typed reference';
  RsCILCmdunused66    = '';
  RsCILCmdunused67    = '';
  RsCILCmdunused68    = '';
  RsCILCmdunused69    = '';
  RsCILCmdunused70    = '';

  RsCILDescrnop         = 'Do nothing';
  RsCILDescrbreak       = 'inform a debugger that a breakpoint has been reached.';
  RsCILDescrldarg0      = 'Load argument 0 onto stack';
  RsCILDescrldarg1      = 'Load argument 1 onto stack';
  RsCILDescrldarg2      = 'Load argument 2 onto stack';
  RsCILDescrldarg3      = 'Load argument 3 onto stack';
  RsCILDescrldloc0      = 'Load local variable 0 onto stack.';
  RsCILDescrldloc1      = 'Load local variable 1 onto stack.';
  RsCILDescrldloc2      = 'Load local variable 2 onto stack.';
  RsCILDescrldloc3      = 'Load local variable 3 onto stack.';
  RsCILDescrstloc0      = 'Pop value from stack into local variable 0.';
  RsCILDescrstloc1      = 'Pop value from stack into local variable 1.';
  RsCILDescrstloc2      = 'Pop value from stack into local variable 2.';
  RsCILDescrstloc3      = 'Pop value from stack into local variable 3.';
  RsCILDescrldargs      = 'Load argument numbered num onto stack, short form.';
  RsCILDescrldargas     = 'fetch the address of argument argNum, short form';
  RsCILDescrstargs      = 'Store a value to the argument numbered num, short form';
  RsCILDescrldlocs      = 'Load local variable of index indx onto stack, short form.';
  RsCILDescrldlocas     = 'Load address of local variable with index indx, short form';
  RsCILDescrstlocs      = 'Pop value from stack into local variable indx, short form.';
  RsCILDescrldnull      = 'Push null reference on the stack';
  RsCILDescrldci4m1     = 'Push -1 onto the stack as int32.';
  RsCILDescrldci40      = 'Push 0 onto the stack as int32.';
  RsCILDescrldci41      = 'Push 1 onto the stack as int32.';
  RsCILDescrldci42      = 'Push 2 onto the stack as int32.';
  RsCILDescrldci43      = 'Push 3 onto the stack as int32.';
  RsCILDescrldci44      = 'Push 4 onto the stack as int32.';
  RsCILDescrldci45      = 'Push 5 onto the stack as int32.';
  RsCILDescrldci46      = 'Push 6 onto the stack as int32.';
  RsCILDescrldci47      = 'Push 7 onto the stack as int32.';
  RsCILDescrldci48      = 'Push 8 onto the stack as int32.';
  RsCILDescrldci4s      = 'Push num onto the stack as int32, short form.';
  RsCILDescrldci4       = 'Push num of type int32 onto the stack as int32.';
  RsCILDescrldci8       = 'Push num of type int64 onto the stack as int64.';
  RsCILDescrldcr4       = 'Push num of type float32 onto the stack as F.';
  RsCILDescrldcr8       = 'Push num of type float64 onto the stack as F.';
  RsCILDescrunused1     = '';
  RsCILDescrdup         = 'duplicate value on the top of the stack';
  RsCILDescrpop         = 'pop a value from the stack';
  RsCILDescrjmp         = 'Exit current method and jump to specified method';
  RsCILDescrcall        = 'Call method described by method';
  RsCILDescrcalli       = 'Call method indicated on the stack with arguments described by callsitedescr.';
  RsCILDescrret         = 'Return from method, possibly returning a value';
  RsCILDescrbrs         = 'branch to target, short form';
  RsCILDescrbrfalses    = 'branch to target if value is zero (false), short form';
  RsCILDescrbrtrues     = 'branch to target if value is non-zero (true), short form';
  RsCILDescrbeqs        = 'branch to target if equal, short form';
  RsCILDescrbges        = 'branch to target if greater than or equal to, short form';
  RsCILDescrbgts        = 'branch to target if greater than, short form';
  RsCILDescrbles        = 'branch to target if less than or equal to, short form';
  RsCILDescrblts        = 'branch to target if less than';
  RsCILDescrbneuns      = 'branch to target if unequal or unordered, short form';
  RsCILDescrbgeuns      = 'branch to target if greater than or equal to (unsigned or unordered), short form';
  RsCILDescrbgtuns      = 'branch to target if greater than (unsigned or unordered), short form';
  RsCILDescrbleuns      = 'branch to target if less than or equal to (unsigned or unordered), short form';
  RsCILDescrbltuns      = 'Branch to target if less than (unsigned or unordered), short form';
  RsCILDescrbr          = 'branch to target ';
  RsCILDescrbrfalse     = 'branch to target if value is zero (false)';
  RsCILDescrbrtrue      = 'branch to target if value is non-zero (true)';
  RsCILDescrbeq         = 'branch to target if equal';
  RsCILDescrbge         = 'branch to target if greater than or equal to';
  RsCILDescrbgt         = 'branch to target if greater than';
  RsCILDescrble         = 'branch to target if less than or equal to';
  RsCILDescrblt         = 'branch to target if less than';
  RsCILDescrbneun       = 'branch to target if unequal or unordered';
  RsCILDescrbgeun       = 'branch to target if greater than or equal to (unsigned or unordered)';
  RsCILDescrbgtun       = 'branch to target if greater than (unsigned or unordered)';
  RsCILDescrbleun       = 'branch to target if less than or equal to (unsigned or unordered)';
  RsCILDescrbltun       = 'Branch to target if less than (unsigned or unordered) ';
  RsCILDescrswitch      = 'jump to one of n values';
  RsCILDescrldindi1     = 'Indirect load value of type int8 as int32 on the stack.';
  RsCILDescrldindu1     = 'Indirect load value of type unsigned int8 as int32 on the stack.';
  RsCILDescrldindi2     = 'Indirect load value of type int16 as int32 on the stack.';
  RsCILDescrldindu2     = 'Indirect load value of type unsigned int16 as int32 on the stack.';
  RsCILDescrldindi4     = 'Indirect load value of type int32 as int32 on the stack.';
  RsCILDescrldindu4     = 'Indirect load value of type unsigned int32 as int32 on the stack.';
  RsCILDescrldindi8     = 'Indirect load value of type int64 as int64 on the stack.';
  RsCILDescrldindi      = 'Indirect load value of type native int as native int on the stack';
  RsCILDescrldindr4     = 'Indirect load value of type float32 as F on the stack.';
  RsCILDescrldindr8     = 'Indirect load value of type float64 as F on the stack.';
  RsCILDescrldindref    = 'Indirect load value of type object ref as O on the stack.';
  RsCILDescrstindref    = 'Store value of type object ref (type O) into memory at address';
  RsCILDescrstindi1     = 'Store value of type int8 into memory at address';
  RsCILDescrstindi2     = 'Store value of type int16 into memory at address';
  RsCILDescrstindi4     = 'Store value of type int32 into memory at address';
  RsCILDescrstindi8     = 'Store value of type int64 into memory at address';
  RsCILDescrstindr4     = 'Store value of type float32 into memory at address';
  RsCILDescrstindr8     = 'Store value of type float64 into memory at address';
  RsCILDescradd         = 'Add two values, returning a new value';
  RsCILDescrsub         = 'Subtract value2 from value1, returning a new value';
  RsCILDescrmul         = 'Multiply values';
  RsCILDescrdiv         = 'Divide two values to return a quotient or floating-point result';
  RsCILDescrdivun       = 'Divide two values, unsigned, returning a quotient';
  RsCILDescrrem         = 'Remainder of dividing value1 by value2';
  RsCILDescrremun       = 'Remainder of unsigned dividing value1 by value2';
  RsCILDescrand         = 'Bitwise AND of two integral values, returns an integral value';
  RsCILDescror          = 'Bitwise OR of two integer values, returns an integer.';
  RsCILDescrxor         = 'Bitwise XOR of integer values, returns an integer';
  RsCILDescrshl         = 'Shift an integer to the left (shifting in zeros)';
  RsCILDescrshr         = 'Shift an integer right, (shift in sign), return an integer';
  RsCILDescrshrun       = 'Shift an integer right, (shift in zero), return an integer';
  RsCILDescrneg         = 'Negate value';
  RsCILDescrnot         = 'Bitwise complement';
  RsCILDescrconvi1      = 'Convert to int8, pushing int32 on stack';
  RsCILDescrconvi2      = 'Convert to int16, pushing int32 on stack';
  RsCILDescrconvi4      = 'Convert to int32, pushing int32 on stack';
  RsCILDescrconvi8      = 'Convert to int64, pushing int64 on stack';
  RsCILDescrconvr4      = 'Convert to float32, pushing F on stack';
  RsCILDescrconvr8      = 'Convert to float64, pushing F on stack';
  RsCILDescrconvu4      = 'Convert to unsigned int32, pushing int32 on stack';
  RsCILDescrconvu8      = 'Convert to unsigned int64, pushing int64 on stack';
  RsCILDescrcallvirt    = 'Call a method associated with obj';
  RsCILDescrcpobj       = 'Copy a value type from srcValObj to destValObj';
  RsCILDescrldobj       = 'Copy instance of value type classTok to the stack.';
  RsCILDescrldstr       = 'push a string object for the literal string ';
  RsCILDescrnewobj      = 'allocate an uninitialized object or value type and call ctor ';
  RsCILDescrcastclass   = 'Cast obj to class';
  RsCILDescrisinst      = 'test if object is an instance of class, returning NULL or an instance of that class or interface';
  RsCILDescrconvrun     = 'Convert unsigned integer to floating-point, pushing F on stack';
  RsCILDescrunused2     = '';
  RsCILDescrunused3     = '';
  RsCILDescrunbox       = 'Extract the value type data from obj, its boxed representation';
  RsCILDescrthrow       = 'Throw an exception';
  RsCILDescrldfld       = 'Push the value of field of object, or value type, obj, onto the stack';
  RsCILDescrldflda      = 'Push the address of field of object obj on the stack';
  RsCILDescrstfld       = 'Replace the value of field of the object obj with val';
  RsCILDescrldsfld      = 'Push the value of field on the stack';
  RsCILDescrldsflda     = 'Push the address of the static field, field, on the stack';
  RsCILDescrstsfld      = 'Replace the value of field with val';
  RsCILDescrstobj       = 'Store a value of type classTok from the stack into memory';
  RsCILDescrconvovfi1un = 'Convert unsigned to an int8 (on the stack as int32) and throw an exception on overflow';
  RsCILDescrconvovfi2un = 'Convert unsigned to an int16 (on the stack as int32) and throw an exception on overflow';
  RsCILDescrconvovfi4un = 'Convert unsigned to an int32 (on the stack as int32) and throw an exception on overflow';
  RsCILDescrconvovfi8un = 'Convert unsigned to an int64 (on the stack as int64) and throw an exception on overflow';
  RsCILDescrconvovfu1un = 'Convert unsigned to an unsigned int8 (on the stack as int32) and throw an exception on overflow';
  RsCILDescrconvovfu2un = 'Convert unsigned to an unsigned int16 (on the stack as int32) and throw an exception on overflow';
  RsCILDescrconvovfu4un = 'Convert unsigned to an unsigned int32 (on the stack as int32) and throw an exception on overflow';
  RsCILDescrconvovfu8un = 'Convert unsigned to an unsigned int64 (on the stack as int64) and throw an exception on overflow';
  RsCILDescrconvovfiun  = 'Convert unsigned to a native int (on the stack as native int) and throw an exception on overflow';
  RsCILDescrconvovfuun  = 'Convert unsigned to a native unsigned  int (on the stack as native int) and throw an exception on overflow';
  RsCILDescrbox         = 'Convert valueType to a true object reference';
  RsCILDescrnewarr      = 'create a new array with elements of type etype';
  RsCILDescrldlen       = 'push the length (of type native unsigned int) of array on the stack';
  RsCILDescrldelema     = 'Load the address of element at index onto the top of the stack';
  RsCILDescrldelemi1    = 'Load the element with type int8 at index onto the top of the stack as an int32';
  RsCILDescrldelemu1    = 'Load the element with type unsigned int8 at index onto the top of the stack as an int32';
  RsCILDescrldelemi2    = 'Load the element with type int16 at index onto the top of the stack as an int32';
  RsCILDescrldelemu2    = 'Load the element with type unsigned int16 at index onto the top of the stack as an int32';
  RsCILDescrldelemi4    = 'Load the element with type int32 at index onto the top of the stack as an int32';
  RsCILDescrldelemu4    = 'Load the element with type unsigned int32 at index onto the top of the stack as an int32 (alias for ldelem.i4)';
  RsCILDescrldelemi8    = 'Load the element with type int64 at index onto the top of the stack as an int64';
  RsCILDescrldelemi     = 'Load the element with type native int at index onto the top of the stack as an native int';
  RsCILDescrldelemr4    = 'Load the element with type float32 at index onto the top of the stack as an F';
  RsCILDescrldelemr8    = 'Load the element with type float64 at index onto the top of the stack as an F';
  RsCILDescrldelemref   = 'Load the element of type object, at index onto the top of the stack as an O';
  RsCILDescrstelemi     = 'Replace array element at index with the i value on the stack';
  RsCILDescrstelemi1    = 'Replace array element at index with the int8 value on the stack';
  RsCILDescrstelemi2    = 'Replace array element at index with the int16 value on the stack';
  RsCILDescrstelemi4    = 'Replace array element at index with the int32 value on the stack';
  RsCILDescrstelemi8    = 'Replace array element at index with the int64 value on the stack';
  RsCILDescrstelemr4    = 'Replace array element at index with the float32 value on the stack';
  RsCILDescrstelemr8    = 'Replace array element at index with the float64 value on the stack';
  RsCILDescrstelemref   = 'Replace array element at index with the ref value on the stack';
  RsCILDescrunused4     = '';
  RsCILDescrunused5     = '';
  RsCILDescrunused6     = '';
  RsCILDescrunused7     = '';
  RsCILDescrunused8     = '';
  RsCILDescrunused9     = '';
  RsCILDescrunused10    = '';
  RsCILDescrunused11    = '';
  RsCILDescrunused12    = '';
  RsCILDescrunused13    = '';
  RsCILDescrunused14    = '';
  RsCILDescrunused15    = '';
  RsCILDescrunused16    = '';
  RsCILDescrunused17    = '';
  RsCILDescrunused18    = '';
  RsCILDescrunused19    = '';
  RsCILDescrconvovfi1   = 'Convert to an int8 (on the stack as int32) and throw an exception on overflow ';
  RsCILDescrconvovfu1   = 'Convert to a unsigned int8 (on the stack as int32) and throw an exception on overflow ';
  RsCILDescrconvovfi2   = 'Convert to an int16 (on the stack as int32) and throw an exception on overflow ';
  RsCILDescrconvovfu2   = 'Convert to a unsigned int16 (on the stack as int32) and throw an exception on overflow ';
  RsCILDescrconvovfi4   = 'Convert to an int32 (on the stack as int32) and throw an exception on overflow ';
  RsCILDescrconvovfu4   = 'Convert to a unsigned int32 (on the stack as int32) and throw an exception on overflow ';
  RsCILDescrconvovfi8   = 'Convert to an int64 (on the stack as int64) and throw an exception on overflow ';
  RsCILDescrconvovfu8   = 'Convert to a unsigned int64 (on the stack as int64) and throw an exception on overflow ';
  RsCILDescrunused20    = '';
  RsCILDescrunused21    = '';
  RsCILDescrunused22    = '';
  RsCILDescrunused23    = '';
  RsCILDescrunused24    = '';
  RsCILDescrunused25    = '';
  RsCILDescrunused26    = '';
  RsCILDescrrefanyval   = 'Push the address stored in a typed reference';
  RsCILDescrckfinite    = 'throw ArithmeticException if value is not a finite number';
  RsCILDescrunused27    = '';
  RsCILDescrunused28    = '';
  RsCILDescrmkrefany    = 'push a typed reference to ptr of type class onto the stack';
  RsCILDescrunused29    = '';
  RsCILDescrunused30    = '';
  RsCILDescrunused31    = '';
  RsCILDescrunused32    = '';
  RsCILDescrunused33    = '';
  RsCILDescrunused34    = '';
  RsCILDescrunused35    = '';
  RsCILDescrunused36    = '';
  RsCILDescrunused37    = '';
  RsCILDescrldtoken     = 'Convert metadata token to its runtime representation';
  RsCILDescrconvu2      = 'Convert to unsigned int16, pushing int32 on stack';
  RsCILDescrconvu1      = 'Convert to unsigned int8, pushing int32 on stack';
  RsCILDescrconvi       = 'Convert to native int, pushing native int on stack';
  RsCILDescrconvovfi    = 'Convert to an native int (on the stack as native int) and throw an exception on overflow';
  RsCILDescrconvovfu    = 'Convert to a native unsigned  int (on the stack as native int) and throw an exception on overflow';
  RsCILDescraddovf      = 'Add signed integer values with overflow check. ';
  RsCILDescraddovfun    = 'Add unsigned integer values with overflow check.';
  RsCILDescrmulovf      = 'Multiply signed integer values. Signed result must fit in same size';
  RsCILDescrmulovfun    = 'Multiply unsigned integer values. Unsigned result must fit in same size';
  RsCILDescrsubovf      = 'Subtract native int from an native int. Signed result must fit in same size';
  RsCILDescrsubovfun    = 'Subtract native unsigned int from a native unsigned int. Unsigned result must fit in same size';
  RsCILDescrendfinally  = 'End finally clause of an exception block';
  RsCILDescrleave       = 'Exit a protected region of code.';
  RsCILDescrleaves      = 'Exit a protected region of code, short form';
  RsCILDescrstindi      = 'Store value of type native int into memory at address';
  RsCILDescrconvu       = 'Convert to native unsigned int, pushing native int on stack';
  RsCILDescrunused38    = '';
  RsCILDescrunused39    = '';
  RsCILDescrunused40    = '';
  RsCILDescrunused41    = '';
  RsCILDescrunused42    = '';
  RsCILDescrunused43    = '';
  RsCILDescrunused44    = '';
  RsCILDescrunused45    = '';
  RsCILDescrunused46    = '';
  RsCILDescrunused47    = '';
  RsCILDescrunused48    = '';
  RsCILDescrunused49    = '';
  RsCILDescrunused50    = '';
  RsCILDescrunused51    = '';
  RsCILDescrunused52    = '';
  RsCILDescrunused53    = '';
  RsCILDescrunused54    = '';
  RsCILDescrunused55    = '';
  RsCILDescrunused56    = '';
  RsCILDescrunused57    = '';
  RsCILDescrunused58    = '';
  RsCILDescrunused59    = '';
  RsCILDescrunused60    = '';
  RsCILDescrprefix7     = '';
  RsCILDescrprefix6     = '';
  RsCILDescrprefix5     = '';
  RsCILDescrprefix4     = '';
  RsCILDescrprefix3     = '';
  RsCILDescrprefix2     = '';
  RsCILDescrprefix1     = '';
  RsCILDescrprefixref   = '';
  RsCILDescrarglist     = 'return argument list handle for the current method ';
  RsCILDescrceq         = 'push 1 (of type int32) if value1 equals value2, else 0';
  RsCILDescrcgt         = 'push 1 (of type int32) if value1 > value2, else 0';
  RsCILDescrcgtun       = 'push 1 (of type int32) if value1 > value2, unsigned or unordered, else 0';
  RsCILDescrclt         = 'push 1 (of type int32) if value1 < value2, else 0';
  RsCILDescrcltun       = 'push 1 (of type int32) if value1 < value2, unsigned or unordered, else 0';
  RsCILDescrldftn       = 'Push a pointer to a method referenced by method on the stack';
  RsCILDescrldvirtftn   = 'Push address of virtual method mthd on the stack';
  RsCILDescrunused61    = '';
  RsCILDescrldarg       = 'Load argument numbered num onto stack.';
  RsCILDescrldarga      = 'fetch the address of argument argNum.';
  RsCILDescrstarg       = 'Store a value to the argument numbered num';
  RsCILDescrldloc       = 'Load local variable of index indx onto stack.';
  RsCILDescrldloca      = 'Load address of local variable with index indx';
  RsCILDescrstloc       = 'Pop value from stack into local variable indx.';
  RsCILDescrlocalloc    = 'Allocate space from the local memory pool.';
  RsCILDescrunused62    = '';
  RsCILDescrendfilter   = 'End filter clause of SEH exception handling';
  RsCILDescrunaligned   = 'Subsequent pointer instruction may be unaligned';
  RsCILDescrvolatile    = 'Subsequent pointer reference is volatile';
  RsCILDescrtail        = 'Subsequent call terminates current method';
  RsCILDescrinitobj     = 'Initialize a value type';
  RsCILDescrunused63    = '';
  RsCILDescrcpblk       = 'Copy data from memory to memory';
  RsCILDescrinitblk     = 'Set a block of memory to a given byte';
  RsCILDescrunused64    = '';
  RsCILDescrrethrow     = 'Rethrow the current exception';
  RsCILDescrunused65    = '';
  RsCILDescrsizeof      = 'Push the size, in bytes, of a value type as a unsigned int32';
  RsCILDescrrefanytype  = 'Push the type token stored in a typed reference';
  RsCILDescrunused66    = '';
  RsCILDescrunused67    = '';
  RsCILDescrunused68    = '';
  RsCILDescrunused69    = '';
  RsCILDescrunused70    = '';

//=== JclCLR =================================================================
resourcestring
  RsClrCopyright    = '// Delphi-JEDI .NET Framework IL Disassembler.  Version 0.1' +  sLineBreak +
    '// Project JEDI Code Library (JCL) Team. All rights reserved.' +  sLineBreak;
  RsUnknownTableFmt = '%s%s';
  RsUnknownTable    = 'Unknown table - ';

//=== JclCOM =================================================================
resourcestring
  RsComInvalidParam      = 'An invalid parameter was passed to the routine. If a parameter was ' +
    'expected, it might be an unassigned item or nil pointer';
  RsComFailedStreamRead  = 'Failed to read all of the data from the specified stream';
  RsComFailedStreamWrite = 'Failed to write all of the data into the specified stream';

//=== JclComplex =============================================================
resourcestring
  RsComplexInvalidString = 'Failed to create a complex number from the string provided';

//=== JclCompression =========================================================
resourcestring
  RsCompressionReadNotSupported      = 'read is not an supported operation.';
  RsCompressionWriteNotSupported     = 'write is not an supported operation.';
  RsCompressionResetNotSupported     = 'reset is not an supported operation.';
  RsCompressionSeekNotSupported      = 'seek is not an supported operation.';
  RsCompressionZLibZErrNo            = 'zlib returned: ERRNO';
  RsCompressionZLibZStreamError      = 'zlib returned: Stream error';
  RsCompressionZLibZDataError        = 'zlib returned: data error';
  RsCompressionZLibZMemError         = 'zlib returned: memory error';
  RsCompressionZLibZBufError         = 'zlib returned: buffer error';
  RsCompressionZLibZVersionError     = 'zlib returned: version error';
  RsCompressionZLibError             = 'zLib returned: unknown error (%d)';
  RsCompressionGZIPInvalidID         = 'gzip: Invalid ID (ID1=%.2x; ID2=%.2x)';
  RsCompressionGZIPUnsupportedCM     = 'gzip: unsupported compression method (%d)';
  RsCompressionGZIPHeaderCRC         = 'gzip: CRC failed, header is damaged';
  RsCompressionGZIPDecompressing     = 'gzip: this property is not readable when the data are being decompressed';
  RsCompressionGZIPNotDecompressed   = 'gzip: this property is not readable until the data are fully decompressed';
  RsCompressionGZIPDataTruncated     = 'gzip: data are truncated';
  RsCompressionGZIPInternalError     = 'gzip: internal error';
  RsCompressionGZIPDataCRCFailed     = 'gzip: CRC failed, data are damaged';
  RsCompressionGZIPExtraFieldTooLong = 'gzip: extra field is too long';
  RsCompressionGZIPBadString         = 'gzip: the string contains null chars';
  RsCompressionBZIP2SequenceError    = 'bzip2 returned: sequence error';
  RsCompressionBZIP2ParameterError   = 'bzip2 returned: parameter error';
  RsCompressionBZIP2MemoryError      = 'bzip2 returned: memory error';
  RsCompressionBZIP2DataError        = 'bzip2 returned: data error';
  RsCompressionBZIP2HeaderError      = 'bzip2 returned: header error';
  RsCompressionBZIP2IOError          = 'bzip2 returned: IO error';
  RsCompressionBZIP2EOFError         = 'bzip2 returned: unexpected end of file';
  RsCompressionBZIP2OutBuffError     = 'bzip2 returned: out buffer is too small';
  RsCompressionBZIP2ConfigError      = 'bzip2 returned: configuration error';
  RsCompressionBZIP2Error            = 'bzip2 returned: unknown error (%d)';
  RsCompressionUnavailableProperty   = 'Property is not available';
  RsCompressionCompressingError      = 'Operation is not supported while compressing';
  RsCompressionDecompressingError    = 'Operation is not supported while decompressing';
  RsCompressionNoFileName            = 'File name not supplied';
  RsCompressionUnsupportedMethod     = 'Unsupported method';
  RsCompressionDataError             = 'Data error';
  RsCompressionCRCError              = 'CRC error';
  RsCompressionNoNestedArchive       = 'Nested archive is not supported';
  RsCompressionUnknownError          = 'Unknown error';
  RsCompression7zLoadError           = 'Sevenzip: Failed to load 7z.dll';
  RsCompression7zReturnError         = 'Sevenzip: Error result (%.8x) "%s"';
  RsCompression7zOutArchiveError     = 'Sevenzip: Failed to get out archive interface for class %s';
  RsCompression7zInArchiveError      = 'Sevenzip: Failed to get in archive interface for class %s';
  RsCompression7zUnknownValueType    = 'Sevenzip: Unknown value type (%d) for property ID %d';
  RsCompression7zOnlyCurrentFile     = 'Sevenzip: Only properties for current file can be retreived';
  RsCompression7zWindows             = 'Windows';
  RsCompression7zUnix                = 'Unix';
  RsCompressionZipName               = 'Zip archive';
  RsCompressionZipExtensions         = '*.zip;' +   // Basic ZIP file
                                       '*.jar;*.ear;*.war;' +  // JAVA files
                                       '*.cbz;' + //Comic reader files - ZIP version
                                       '*.apk;' + // Android application package
                                       '*.wsz;*.wal;' + // Winamp Skins
                                       '*.xpi;*.crx;' + // Firefox, Chrome extensions
                                       '*.dfsz;' + // ???
                                       '*.pcv;' + // MozBackup file
                                       '*.bsz;' + // BSplayer skin
                                       '*.mskin;' + // Maxthon skin
                                       '*.wmz;' + // Windows Media Player skin
                                       '*.ipa;' + // iPhone/iPad application
                                       '*.docx;*.xlsx;*.pptx;' + // MsOffice
                                       '*.sxw;*.sxi;*.sxt;*.sxd;*.sxc;*.sxm;*.sxg;*.stw;*.sti;*.std;*.stc;' + // OpenOffice.org 1.x documents and templates
                                       '*.odh;*.odd;*.odt;*.odm;*.ods;*.ots;*.odg;*.otg;*.odp;*.otp;*.odf;*.odb'; // OpenOffice.org 2.x/3.x docs and templates
  RsCompressionBZip2Name             = 'BZip2 archive';
  RsCompressionBZip2Extensions       = '*.bz2;*.bzip2;*.tbz2;*.tbz';
  RsCompressionBZip2SubExtensions    = '.tbz2=.tar;.tbz=.tar';
  RsCompressionRarName               = 'Rar archive';
  RsCompressionRarExtensions         = '*.rar;*.r00;'+
                                       '*.cbr'; // Comic reader file - RAR version
  RsCompressionArjName               = 'Arj archive';
  RsCompressionArjExtensions         = '*.arj';
  RsCompressionZName                 = 'Z archive';
  RsCompressionZExtensions           = '*.z;*.taz';
  RsCompressionZSubExtensions        = '.taz=.tar';
  RsCompressionLzhName               = 'Lzh archive';
  RsCompressionLzhExtensions         = '*.lzh;*.lha';
  RsCompression7zName                = '7z archive';
  RsCompression7zExtensions          = '*.7z';
  RsCompressionCabName               = 'Cab archive';
  RsCompressionCabExtensions         = '*.cab;'+
                                       '*.fwp'; // FrontPage Web Package
  RsCompressionNsisName              = 'Nsis archive';
  RsCompressionNsisExtensions        = '*.nsis';
  RsCompressionLzmaName              = 'Lzma archive';
  RsCompressionLzmaExtensions        = '*.lzma';
  RsCompressionLzma86Name            = 'Lzma86 archive';
  RsCompressionLzma86Extensions      = '*.lzma86';
  RsCompressionPeName                = 'Pe archive';
  RsCompressionPeExtensions          = '*.exe;*.dll;*.sys;*.bpl';
  RsCompressionElfName               = 'Elf archive';
  // TODO: extension might be *.*, but then TJclCompressionStreamFormats.FindDecompressFormat can fail
  RsCompressionElfExtensions         = '*.';
  RsCompressionMachoName             = 'Mach-O archive';
  // TODO: extension might be *.*, but then TJclCompressionStreamFormats.FindDecompressFormat can fail
  RsCompressionMachoExtensions       = '*.';
  RsCompressionUdfName               = 'Udf archive';
  RsCompressionUdfExtensions         = '*.iso;*.img';
  RsCompressionXarName               = 'Xar archive';
  RsCompressionXarExtensions         = '*.xar;'+
                                       '*.safariextz'; // Safari extensions
  RsCompressionMubName               = 'Mub archive';
  // TODO: extension might be *.*, but then TJclCompressionStreamFormats.FindDecompressFormat can fail
  RsCompressionMubExtensions         = '*.';
  RsCompressionHfsName               = 'Hfs archive';
  RsCompressionHfsExtensions         = '*.hfs';
  RsCompressionDmgName               = 'Dmg archive';
  RsCompressionDmgExtensions         = '*.dmg';
  RsCompressionCompoundName          = 'Compound archive';
  RsCompressionCompoundExtensions    = '*.msi;*.msp;*.doc;*.xls;*.ppt';
  RsCompressionWimName               = 'Wim archive';
  RsCompressionWimExtensions         = '*.wim;*.swm';
  RsCompressionIsoName               = 'Iso archive';
  RsCompressionIsoExtensions         = '*.iso;*.img';
  RsCompressionChmName               = 'Chm archive';
  RsCompressionChmExtensions         = '*.chm;*.chi;*.chq;*.chw;*.hxs;*.hxi;*.hxr;*.hxq;*.hxw;*.lit';
  RsCompressionSplitName             = 'Split archive';
  RsCompressionSplitExtensions       = '*.001';
  RsCompressionRpmName               = 'Rpm archive';
  RsCompressionRpmExtensions         = '*.rpm';
  RsCompressionDebName               = 'Deb archive';
  RsCompressionDebExtensions         = '*.deb';
  RsCompressionCpioName              = 'Cpio archive';
  RsCompressionCpioExtensions        = '*.cpio';
  RsCompressionTarName               = 'Tar archive';
  RsCompressionTarExtensions         = '*.tar';
  RsCompressionGZipName              = 'GZip archive';
  RsCompressionGZipExtensions        = '*.gz;*.gzip;*.tgz;*.tpz';
  RsCompressionGZipSubExtensions     = '.tgz=.tar;.tpz=.tar';
  RsCompressionXzName                = 'Xz archive';
  RsCompressionXzExtensions          = '*.xz;*.txz';
  RsCompressionXzSubExtensions       = '.txz=.tar';
  RsCompressionNtfsName              = 'Ntfs archive';
  RsCompressionNtfsExtensions        = '*.ntfs;*.img';
  RsCompressionFatName               = 'Fat archive';
  RsCompressionFatExtensions         = '*.fat;*.img';
  RsCompressionMbrName               = 'Mbr archive';
  RsCompressionMbrExtensions         = '*.mbr';
  RsCompressionVhdName               = 'Vhd archive';
  RsCompressionVhdExtensions         = '*.vhd';
  RsCompressionVhdSubExtensions      = '.vhd=.mbr';
  RsCompressionFlvName               = 'Flv archive';
  RsCompressionFlvExtensions         = '*.flv';
  RsCompressionMsLZName              = 'MsLZ archive';
  // TODO: extension might be *.*, but then TJclCompressionStreamFormats.FindDecompressFormat can fail
  RsCompressionMsLZExtensions        = '*.';
  RsCompressionSwfName               = 'Swf archive';
  RsCompressionSwfExtensions         = '*.swf';
  RsCompressionSwfcName              = 'Swf archive';
  RsCompressionSwfcExtensions        = '*.swf';
  RsCompressionApmName               = 'APM archive';
  RsCompressionApmExtensions         = '*.';
  RsCompressionPpmdName              = 'PPMD archive';
  RsCompressionPpmdExtensions        = '*.pmd';
  RsCompressionTEName                = 'Terse Executable';
  RsCompressionTEExtensions          = '*.te';
  RsCompressionUEFIcName             = 'UEFIc archive';
  RsCompressionUEFIcExtensions       = '*.scap';
  RsCompressionUEFIsName             = 'UEFIs archive';
  RsCompressionUEFIsExtensions       = '*.';
  RsCompressionSquashFSName          = 'SquashFS archive';
  RsCompressionSquashFSExtensions    = '*.squashfs';
  RsCompressionCramFSName            = 'CramFS archive';
  RsCompressionCramFSExtensions      = '*.cramfs';
  RsCompressionDuplicate             = 'The file %s already exists in the archive';
  RsCompressionReplaceError          = 'At least one compression volume could not be replaced after an archive out-of-place update';

//=== JclConsole =============================================================
resourcestring
  RsCannotRaiseSignal = 'Cannot raise %s signal.';

//=== JclContainerIntf =======================================================
resourcestring
  RsEOutOfBounds           = 'Out of bounds';
  RsEOperationNotSupported = 'Operation not supported';
  RsEValueNotFound         = 'Value %s not found';
  RsEDuplicateElement      = 'Duplicate element';
  RsENoCollection          = 'Collection not assigned';
  RsEIllegalQueueCapacity  = 'Illegal queue capacity';
  RsEIllegalStateOperation = 'Illegal state operation';
  RsENoEqualityComparer    = 'Item equality comparer is not assigned';
  RsENoComparer            = 'Item comparer is not assigned';
  RsENoHashConverter       = 'Hash converter is not assigned';
  RsEAssignError           = 'Assignation error';
  RsEReadOnlyError         = 'Container is read-only';

//=== JclCounter =============================================================
resourcestring
  RsNoCounter = 'No high performance counters supported';

//=== JclDateTime ============================================================
resourcestring
  RsMakeUTCTime    = 'Error converting to UTC time. Time zone could not be determined';
  RsDateConversion = 'Error illegal date or time format';

//=== JclDebug ===============================================================
resourcestring
  RsUnknownFunctionAt     = 'Unknown function at %s';

//=== JclCppException ========================================================
resourcestring
  RsCppUnhandledExceptionMsg = 'Unhandled C++ exception of type ''%s'' occurred';

//=== JclDotNet ==============================================================
resourcestring
  RsEUnknownCLRVersion = '"%s" is not a known CLR version';

//=== JclExprEval ============================================================
resourcestring
  RsExprEvalRParenExpected = 'Parse error: '')'' expected';
  RsExprEvalFactorExpected = 'Parse error: Factor expected';
  RsExprEvalUnknownSymbol  = 'Parse error: Unknown symbol: ''%s''';

  RsExprEvalFirstArg = 'Parse error: ''('' and function''s first parameter expected';
  RsExprEvalNextArg  = 'Parse error: '','' and another parameter expected';
  RsExprEvalEndArgs  = 'Parse error: '')'' to close function''s parameters expected';

  RsExprEvalExprNotFound          = 'Expression compiler error: Expression ''%s'' not found';
  RsExprEvalExprPtrNotFound       = 'Expression compiler error: Expression pointer not found';
  RsExprEvalExprRefCountAssertion = 'Expression compiler error: expression refcount < 0';

//=== JclFileUtils ===========================================================
resourcestring
  // Path manipulation
  RsPathInvalidDrive = '%s is not a valid drive';

  // Files and directories
  RsFileUtilsAttrUnavailable = 'Unable to retrieve attributes of %s';

  RsCannotCreateDir = 'Unable to create directory';
  RsDelTreePathIsEmpty = 'DelTree: Path is empty';
  RsFileSearchAttrInconsistency = 'Some file search attributes are required AND rejected!';
  RsEWindowsVersionNotSupported = 'This windows version is not supported';
  RsEWindowNotValid = 'The window with handle %d is not valid';
  RsEProcessNotValid = 'The process with ID %d is not valid';
  RsEModuleNotValid = 'The Module with handle %d is not valid';

  // TJclFileVersionInfo
  RsFileUtilsNoVersionInfo = 'File contains no version information';
  RsFileUtilsFileDoesNotExist = 'The file %s does not exist';
  RsFileUtilsLanguageIndex = 'Illegal language index';
  RsFileUtilsEmptyValue = 'No value was supplied';
  RsFileUtilsValueNotFound = 'The value %s was not found.';
  // Strings returned from OSIdentTOString()
  RsVosUnknown      = 'Unknown';
  RsVosDos          = 'MS-DOS';
  RsVosOS216        = '16-bit OS/2';
  RsVosOS232        = '32-bit OS/2';
  RsVosNT           = 'Windows NT';
  RsVosWindows16    = '16-bit Windows';
  RsVosPM16         = '16-bit PM';
  RsVosPM32         = '32-bit PM';
  RsVosWindows32    = '32-bit Windows';
  RsVosDosWindows16 = '16-bit Windows, running on MS-DOS';
  RsVosDosWindows32 = 'Win32 API, running on MS-DOS';
  RsVosOS216PM16    = '16-bit PM, running on 16-bit OS/2';
  RsVosOS232PM32    = '32-bit PM, running on 32-bit OS/2';
  RsVosNTWindows32  = 'Win32 API, running on Windows/NT';
  RsVosDesignedFor  = 'Designed for %s';

  // Strings returned from OSFileTypeToString()
  RsVftUnknown         = 'Unknown';
  RsVftApp             = 'Application';
  RsVftDll             = 'Library';
  RsVftDrv             = 'Driver';
  RsVftFont            = 'Font';
  RsVftVxd             = 'Virtual device';
  RsVftStaticLib       = 'Static-link library';
  RsVft2DrvPRINTER     = 'Printer';
  RsVft2DrvKEYBOARD    = 'Keyboard';
  RsVft2DrvLANGUAGE    = 'Language';
  RsVft2DrvDISPLAY     = 'Display';
  RsVft2DrvMOUSE       = 'Mouse';
  RsVft2DrvNETWORK     = 'Network';
  RsVft2DrvSYSTEM      = 'System';
  RsVft2DrvINSTALLABLE = 'Installable';
  RsVft2DrvSOUND       = 'Sound';
  RsVft2DrvCOMM        = 'Communications';
  RsVft2FontRASTER     = 'Raster';
  RsVft2FontVECTOR     = 'Vector';
  RsVft2FontTRUETYPE   = 'TrueType';

  // TJclFileStream
  RsFileStreamCreate         = 'Unable to create temporary file stream';

  // TJclFileMapping
  RsCreateFileMapping        = 'Failed to create FileMapping';
  RsCreateFileMappingView    = 'Failed to create FileMappingView';
  RsLoadFromStreamSize       = 'Not enough space in View in procedure LoadFromStream';
  RsFileMappingInvalidHandle = 'Invalid file handle';
  RsViewNeedsMapping         = 'FileMap argument of TJclFileMappingView constructor cannot be nil';
  RsFailedToObtainSize       = 'Failed to obtain size of file';

  // GetDriveTypeStr()
  RsUnknownDrive   = 'Unknown drive type';
  RsRemovableDrive = 'Removable Drive';
  RsHardDisk       = 'Hard Disk';
  RsRemoteDrive    = 'Remote Drive';
  RsCDRomDrive     = 'CD-ROM';
  RsRamDisk        = 'RAM-Disk';

  // GetFileAttributeList()
  RsAttrDirectory  = 'Directory';
  RsAttrReadOnly   = 'ReadOnly';
  RsAttrSystemFile = 'SystemFile';
  RsAttrVolumeID   = 'Volume ID';
  RsAttrArchive    = 'Archive';
  RsAttrAnyFile    = 'AnyFile';
  RsAttrHidden     = 'Hidden';

  // GetFileAttributeListEx()
  RsAttrNormal       = 'Normal';
  RsAttrTemporary    = 'Temporary';
  RsAttrCompressed   = 'Compressed';
  RsAttrOffline      = 'Offline';
  RsAttrEncrypted    = 'Encrypted';
  RsAttrReparsePoint = 'Reparse Point';
  RsAttrSparseFile   = 'Sparse';

  // TJclFileMapping.Create
  RsFileMappingOpenFile = 'Unable to open the file';

  // TJclMappedTextReader
  RsFileIndexOutOfRange = 'Index of out range';

  // FileGetTypeName()
  RsDefaultFileTypeName = ' File';

//=== JclMapi ================================================================
resourcestring
  RsMapiError         = 'MAPI Error: (%d) "%s"';
  RsMapiMissingExport = 'Function "%s" is not exported by client';
  RsMapiInvalidIndex  = 'Index is out ot range';
  RsMapiMailNoClient  = 'No Simple MAPI client installed, cannot send the message';

  RsMapiErrUSER_ABORT               = 'User abort';
  RsMapiErrFAILURE                  = 'General MAPI failure';
  RsMapiErrLOGIN_FAILURE            = 'MAPI login failure';
  RsMapiErrDISK_FULL                = 'Disk full';
  RsMapiErrINSUFFICIENT_MEMORY      = 'Insufficient memory';
  RsMapiErrACCESS_DENIED            = 'Access denied';
  RsMapiErrTOO_MANY_SESSIONS        = 'Too many sessions';
  RsMapiErrTOO_MANY_FILES           = 'Too many files were specified';
  RsMapiErrTOO_MANY_RECIPIENTS      = 'Too many recipients were specified';
  RsMapiErrATTACHMENT_NOT_FOUND     = 'A specified attachment was not found';
  RsMapiErrATTACHMENT_OPEN_FAILURE  = 'Attachment open failure';
  RsMapiErrATTACHMENT_WRITE_FAILURE = 'Attachment write failure';
  RsMapiErrUNKNOWN_RECIPIENT        = 'Unknown recipient';
  RsMapiErrBAD_RECIPTYPE            = 'Bad recipient type';
  RsMapiErrNO_MESSAGES              = 'No messages';
  RsMapiErrINVALID_MESSAGE          = 'Invalid message';
  RsMapiErrTEXT_TOO_LARGE           = 'Text too large';
  RsMapiErrINVALID_SESSION          = 'Invalid session';
  RsMapiErrTYPE_NOT_SUPPORTED       = 'Type not supported';
  RsMapiErrAMBIGUOUS_RECIPIENT      = 'A recipient was specified ambiguously';
  RsMapiErrMESSAGE_IN_USE           = 'Message in use';
  RsMapiErrNETWORK_FAILURE          = 'Network failure';
  RsMapiErrINVALID_EDITFIELDS       = 'Invalid edit fields';
  RsMapiErrINVALID_RECIPS           = 'Invalid recipients';
  RsMapiErrNOT_SUPPORTED            = 'Not supported';

  RsMapiMailORIG    = 'From';
  RsMapiMailTO      = 'To';
  RsMapiMailCC      = 'Cc';
  RsMapiMailBCC     = 'Bcc';
  RsMapiMailSubject = 'Subject';

//=== JclMath ================================================================
resourcestring
  RsMathDomainError    = 'Domain check failure in JclMath';
  RsEmptyArray         = 'Empty array is not allowed as input parameter';
  RsNonPositiveArray   = 'Input array contains non-positive or zero values';
  RsUnexpectedValue    = 'Unexpected data value';
  RsInvalidRational    = 'Invalid rational number';
  RsDivByZero          = 'Division by zero';
  RsRationalDivByZero  = 'Rational division by zero';
  RsNoNaN              = 'NaN expected';
  RsNaNTagError        = 'NaN Tag value %d out of range';
  RsNaNSignal          = 'NaN signaling %d';
  RsPowerInfinite      = 'Power function: Result is infinite';
  RsPowerComplex       = 'Power function: Result is complex';

//=== JclMetadata ============================================================
resourcestring
  RsUnknownClassLayout      = 'Unknown class layout - $%.8x';
  RsUnknownStringFormatting = 'Unknown string formatting - $%.8x';
  RsInvalidSignatureData    = 'Invalid compressed signature data - %.2x %.2x %.2x %.2x';
  RsUnknownManifestResource = 'Unknown manifest resource visibility - %d';
  RsNoLocalVarSig           = 'Signature %s is not LocalVarSig';
  RsLocalVarSigOutOfRange   = 'LocalVarSig count %d is out of range [1..$$FFFE]';

//=== JclMIDI ================================================================
resourcestring
  RsOctaveC      = 'C';
  RsOctaveCSharp = 'C#';
  RsOctaveD      = 'D';
  RsOctaveDSharp = 'D#';
  RsOctaveE      = 'E';
  RsOctaveF      = 'F';
  RsOctaveFSharp = 'F#';
  RsOctaveG      = 'G';
  RsOctaveGSharp = 'G#';
  RsOctaveA      = 'A';
  RsOctaveASharp = 'A#';
  RsOctaveB      = 'B';

  RsMidiInvalidChannelNum = 'Invalid MIDI channel number (%d)';
  {$IFDEF UNIX}
  RsMidiNotImplemented    = 'JclMidi: MIDI I/O for Unix not (yet) implemented';
  {$ENDIF UNIX}

//=== JclMiscel ==============================================================
resourcestring
  // CreateProcAsUser
  RsCreateProcNTRequiredError         = 'Windows NT required';
  RsCreateProcBuild1057Error          = 'NT version 3.51 build 1057 or later required';

  RsCreateProcPrivilegeMissing        = 'This account does not have the privilege "%s" (%s)';
  RsCreateProcLogonUserError          = 'LogonUser failed';
  RsCreateProcAccessDenied            = 'Access denied';
  RsCreateProcLogonFailed             = 'Unable to logon';
  RsCreateProcSetStationSecurityError = 'Cannot set WindowStation "%s" security.';
  RsCreateProcSetDesktopSecurityError = 'Cannot set Desktop "%s" security.';
  RsCreateProcPrivilegesMissing       = 'This account does not have one (or more) of ' +
    'the following privileges: ' + '"%s"(%s)' + sLineBreak + '"%s"(%s)' + sLineBreak;
  RsCreateProcCommandNotFound         = 'Command or filename not found: "%s"';
  RsCreateProcFailed                  = 'CreateProcessAsUser failed';

//=== JclMultimedia ==========================================================
resourcestring
  // Multimedia timer
  RsMmTimerGetCaps     = 'Error retrieving multimedia timer device capabilities';
  RsMmSetEvent         = 'Error setting multimedia event timer';
  RsMmInconsistentId   = 'Multimedia timer callback was called with inconsistent Id';
  RsMmTimerActive      = 'This operation cannot be performed while the timer is active';

  // Audio Mixer
  RsMmMixerSource      = 'Source';
  RsMmMixerDestination = 'Destination';
  RsMmMixerUndefined   = 'Undefined';
  RsMmMixerDigital     = 'Digital';
  RsMmMixerLine        = 'Line';
  RsMmMixerMonitor     = 'Monitor';
  RsMmMixerSpeakers    = 'Speakers';
  RsMmMixerHeadphones  = 'Headphones';
  RsMmMixerTelephone   = 'Telephone';
  RsMmMixerWaveIn      = 'Waveform-audio input';
  RsMmMixerVoiceIn     = 'Voice input';
  RsMmMixerMicrophone  = 'Microphone';
  RsMmMixerSynthesizer = 'Synthesizer';
  RsMmMixerCompactDisc = 'Compact disc';
  RsMmMixerPcSpeaker   = 'PC speaker';
  RsMmMixerWaveOut     = 'Waveform-audio output';
  RsMmMixerAuxiliary   = 'Auxiliary audio line';
  RsMmMixerAnalog      = 'Analog';
  RsMmMixerNoDevices   = 'No mixer device found';
  RsMmMixerCtlNotFound = 'Line control (%s, %.8x) not found';

  // EJclMciError
  RsMmUnknownError     = 'Unknown MCI error No. %d';
  RsMmMciErrorPrefix   = 'MCI-Error: ';

  // CD audio routines
  RsMmNoCdAudio        = 'Cannot open CDAUDIO-Device';
  RsMmCdTrackNo        = 'Track: %.2u';
  RsMMCdTimeFormat     = '%2u:%.2u';
  RsMMTrackAudio       = 'Audio';
  RsMMTrackOther       = 'Other';

//=== JclNTFS ================================================================
resourcestring
  RsInvalidArgument = '%s: Invalid argument <%s>';
  RsNtfsUnableToDeleteSymbolicLink = 'Unable to delete temporary symbolic link';
  RsEUnableToCreatePropertyStorage = 'Unable to create property storage';
  RsEIncomatibleDataFormat = 'Incompatible data format';

//=== JclPCRE ================================================================
resourcestring
  RsErrNoMatch       = 'No match';
  RsErrNull          = 'Required value is null';
  RsErrBadOption     = 'Bad option';
  RsErrBadMagic      = 'Bad magic';
  RsErrUnknownNode   = 'Unknown node';
  RsErrNoMemory      = 'Out of memory';
  RsErrNoSubString   = 'No substring';
  RsErrMatchLimit    = 'Match limit';
  RsErrCallout       = 'Callout';
  RsErrBadUTF8       = 'Bad UTF-8';
  RsErrBadUTF16      = 'Bad UTF-16';
  RsErrBadUTF8Offset = 'Bad UTF-8 offset';
  RsErrBadUTF16Offset = 'Bad UTF-16 offset';
  RsErrPartial       = 'Partial';
  RsErrBadPartial    = 'Bad partial';
  RsErrInternal      = 'Internal';
  RsErrBadCount      = 'Bad count';
  RsErrDfaUItem      = 'DFA UItem';
  RsErrDfaUCond      = 'DFA UCond';
  RsErrDfaUMLimit    = 'DFA UMLimit';
  RsErrDfaWSSize     = 'DFA WSSize';
  RsErrDfaRecurse    = 'DFA Recurse';
  RsErrRecursionLimit = 'Recursion limit';
  RsErrNullWsLimit   = 'Null WS limit';
  RsErrBadNewLine    = 'Bad new line';
  RsErrBadOffset     = 'Bad offset';
  RsErrShortUTF8     = 'Short UTF-8';
  RsErrShortUTF16    = 'Short UTF-16';
  RsErrRecurseLoop   = 'Recurse loop';
  RsErrJITStackLimit = 'JIT stack limit';
  RsErrLibNotLoaded  = 'PCRE library not loaded';
  RsErrMemFuncNotSet = 'PCRE memory management functions not set';
  RsErrStudyFailed   = 'Study failed';
  RsErrCalloutError  = 'Unhandled exception in callout';
  RsErrUnknownError  = 'Unknown error';
  RsErrNoUTF8Support = 'No UTF-8 support in this version of PCRE';
  RsErrNoUTF16Support = 'No UTF-16 support in this version of PCRE';
  RsErrNoJITSupport  = 'No JIT support in this version of PCRE';
  RsErrBadMode       = 'Bad Mode';
  RsErrBadEndianness = 'Bad endianness';
  RsErrBadRestart    = 'Bad Restart';

//=== JclPeImage =============================================================
resourcestring
  RsPeReadOnlyStream = 'Stream is read-only';

  // TJclPeImage
  RsPeCantOpen                = 'Cannot open file "%s"';
  RsPeNotPE                   = 'This is not a PE format';
  RsPeUnknownTarget           = 'Unknown PE target'; 
  RsPeNotResDir               = 'Not a resource directory';
  RsPeNotAvailableForAttached = 'Feature is not available for attached images';
  RsPeSectionNotFound         = 'Section "%s" not found';

  // PE directory names
  RsPeImg_00 = 'Exports';
  RsPeImg_01 = 'Imports';
  RsPeImg_02 = 'Resources';
  RsPeImg_03 = 'Exceptions';
  RsPeImg_04 = 'Security';
  RsPeImg_05 = 'Base Relocations';
  RsPeImg_06 = 'Debug';
  RsPeImg_07 = 'Description';
  RsPeImg_08 = 'Machine Value';
  RsPeImg_09 = 'TLS';
  RsPeImg_10 = 'Load configuration';
  RsPeImg_11 = 'Bound Import';
  RsPeImg_12 = 'IAT';
  RsPeImg_13 = 'Delay load import';
  RsPeImg_14 = 'COM run-time';
  RsPeImg_Reserved = 'reserved [%.2d]';

  // NT Header names
  RsPeSignature               = 'Signature';
  RsPeMachine                 = 'Machine';
  RsPeNumberOfSections        = 'Number of Sections';
  RsPeTimeDateStamp           = 'Time Date Stamp';
  RsPePointerToSymbolTable    = 'Symbols Pointer';
  RsPeNumberOfSymbols         = 'Number of Symbols';
  RsPeSizeOfOptionalHeader    = 'Size of Optional Header';
  RsPeCharacteristics         = 'Characteristics';
  RsPeMagic                   = 'Magic';
  RsPeLinkerVersion           = 'Linker Version';
  RsPeSizeOfCode              = 'Size of Code';
  RsPeSizeOfInitializedData   = 'Size of Initialized Data';
  RsPeSizeOfUninitializedData = 'Size of Uninitialized Data';
  RsPeAddressOfEntryPoint     = 'Address of Entry Point';
  RsPeBaseOfCode              = 'Base of Code';
  RsPeBaseOfData              = 'Base of Data';
  RsPeImageBase               = 'Image Base';
  RsPeSectionAlignment        = 'Section Alignment';
  RsPeFileAlignment           = 'File Alignment';
  RsPeOperatingSystemVersion  = 'Operating System Version';
  RsPeImageVersion            = 'Image Version';
  RsPeSubsystemVersion        = 'Subsystem Version';
  RsPeWin32VersionValue       = 'Win32 Version';
  RsPeSizeOfImage             = 'Size of Image';
  RsPeSizeOfHeaders           = 'Size of Headers';
  RsPeCheckSum                = 'CheckSum';
  RsPeSubsystem               = 'Subsystem';
  RsPeDllCharacteristics      = 'Dll Characteristics';
  RsPeSizeOfStackReserve      = 'Size of Stack Reserve';
  RsPeSizeOfStackCommit       = 'Size of Stack Commit';
  RsPeSizeOfHeapReserve       = 'Size of Heap Reserve';
  RsPeSizeOfHeapCommit        = 'Size of Heap Commit';
  RsPeLoaderFlags             = 'Loader Flags';
  RsPeNumberOfRvaAndSizes     = 'Number of RVA';

  // Load config names
  RsPeVersion                       = 'Version';
  RsPeGlobalFlagsClear              = 'GlobalFlagsClear';
  RsPeGlobalFlagsSet                = 'GlobalFlagsSet';
  RsPeCriticalSectionDefaultTimeout = 'CriticalSectionDefaultTimeout';
  RsPeDeCommitFreeBlockThreshold    = 'DeCommitFreeBlockThreshold';
  RsPeDeCommitTotalFreeThreshold    = 'DeCommitTotalFreeThreshold';
  RsPeLockPrefixTable               = 'LockPrefixTable';
  RsPeMaximumAllocationSize         = 'MaximumAllocationSize';
  RsPeVirtualMemoryThreshold        = 'VirtualMemoryThreshold';
  RsPeProcessHeapFlags              = 'ProcessHeapFlags';
  RsPeProcessAffinityMask           = 'ProcessAffinityMask';
  RsPeCSDVersion                    = 'CSDVersion';
  RsPeReserved                      = 'Reserved';
  RsPeEditList                      = 'EditList';

  // Machine names
  RsPeMACHINE_UNKNOWN   = 'Unknown';
  RsPeMACHINE_I386      = 'Intel 386';
  RsPeMACHINE_R3000     = 'MIPS little-endian R3000';
  RsPeMACHINE_R4000     = 'MIPS little-endian R4000';
  RsPeMACHINE_R10000    = 'MIPS little-endian R10000';
  RsPeMACHINE_WCEMIPSV2 = 'MIPS little-endian WCE v2';
  RsPeMACHINE_ALPHA     = 'Alpha_AXP';
  RsPeMACHINE_SH3       = 'SH3 little-endian';
  RsPeMACHINE_SH3DSP    = 'SH3 DSP';
  RsPeMACHINE_SH3E      = 'SH3E little-endian';
  RsPeMACHINE_SH4       = 'SH4 little-endian';
  RsPeMACHINE_SH5       = 'SH5';
  RsPeMACHINE_ARM       = 'ARM Little-Endian';
  RsPeMACHINE_THUMB     = 'THUMB';
  RsPeMACHINE_AM33      = 'AM33';
  RsPeMACHINE_POWERPC   = 'IBM PowerPC Little-Endian';
  RsPeMACHINE_POWERPCFP = 'IBM PowerPC FP';
  RsPeMACHINE_IA64      = 'Intel 64';
  RsPeMACHINE_MIPS16    = 'MIPS16';
  RsPeMACHINE_AMPHA64   = 'ALPHA64';
  RsPeMACHINE_MIPSFPU   = 'MIPSFPU';
  RsPeMACHINE_MIPSFPU16 = 'MIPSFPU16';
  RsPeMACHINE_TRICORE   = 'Infineon';
  RsPeMACHINE_CEF       = 'CEF';
  RsPeMACHINE_EBC       = 'EFI Byte Code';
  RsPeMACHINE_AMD64     = 'AMD64 (K8)';
  RsPeMACHINE_M32R      = 'M32R little-endian';
  RsPeMACHINE_CEE       = 'CEE';

  // Subsystem names
  RsPeSUBSYSTEM_UNKNOWN     = 'Unknown';
  RsPeSUBSYSTEM_NATIVE      = 'Native';
  RsPeSUBSYSTEM_WINDOWS_GUI = 'GUI';
  RsPeSUBSYSTEM_WINDOWS_CUI = 'Console';
  RsPeSUBSYSTEM_OS2_CUI     = 'OS/2';
  RsPeSUBSYSTEM_POSIX_CUI   = 'Posix';
  RsPeSUBSYSTEM_RESERVED8   = 'Reserved 8';

  // Debug symbol type names
  RsPeDEBUG_UNKNOWN       = 'UNKNOWN';
  RsPeDEBUG_COFF          = 'COFF';
  RsPeDEBUG_CODEVIEW      = 'CODEVIEW';
  RsPeDEBUG_FPO           = 'FPO';
  RsPeDEBUG_MISC          = 'MISC';
  RsPeDEBUG_EXCEPTION     = 'EXCEPTION';
  RsPeDEBUG_FIXUP         = 'FIXUP';
  RsPeDEBUG_OMAP_TO_SRC   = 'OMAP_TO_SRC';
  RsPeDEBUG_OMAP_FROM_SRC = 'OMAP_FROM_SRC';
  RsPeDEBUG_BORLAND       = 'BORLAND';

  // TJclPePackageInfo.PackageModuleTypeToString
  RsPePkgExecutable = 'Executable';
  RsPePkgPackage    = 'Package';
  PsPePkgLibrary    = 'Library';

  // TJclPePackageInfo.PackageOptionsToString
  RsPePkgNeverBuild     = 'NeverBuild';
  RsPePkgDesignOnly     = 'DesignOnly';
  RsPePkgRunOnly        = 'RunOnly';
  RsPePkgIgnoreDupUnits = 'IgnoreDupUnits';

  // TJclPePackageInfo.ProducerToString
  RsPePkgV3Produced        = 'Delphi 3 or C++ Builder 3';
  RsPePkgProducerUndefined = 'Undefined';
  RsPePkgBCB4Produced      = 'C++ Builder 4 or later';
  RsPePkgDelphi4Produced   = 'Delphi 4 or later';

  // TJclPePackageInfo.UnitInfoFlagsToString
  RsPePkgMain     = 'Main';
  RsPePkgWeak     = 'Weak';
  RsPePkgOrgWeak  = 'OrgWeak';
  RsPePkgImplicit = 'Implicit';

//=== JclRegistry ============================================================
resourcestring
  RsUnableToOpenKeyRead  = 'Unable to open key "%s\%s" for read';
  RsUnableToOpenKeyWrite = 'Unable to open key "%s\%s" for write';
  RsUnableToAccessValue  = 'Unable to open key "%s\%s" and access value "%s"';
  RsWrongDataType        = '"%s\%s\%s" is of wrong kind or size';
  RsInconsistentPath     = '"%s" does not match RootKey';

//=== JclRTTI ================================================================
resourcestring
  RsRTTISetValueOutOfRange  = 'Set value over %s does not fit into a 32-bit integer.';
  RsRTTIValueOutOfRange   = 'Value %s out of range %s..%s.';
  RsRTTIUnknownIdentifier = 'Unknown identifier ''%s''.';
  RsRTTIInvalidBaseType   = 'Invalid base type (%s is of type %s).';
  RsRTTINoStringValue     = 'The property %s of type %s has no string value'; 

  RsRTTIVar           = 'var ';
  RsRTTIConst         = 'const ';
  RsRTTIArrayOf       = 'array of ';
  RsRTTIOut           = 'out ';
  RsRTTIBits          = 'bits';
  RsRTTIOrdinal       = 'ordinal=';
  RsRTTITrue          = 'True';
  RsRTTIFalse         = 'False';
  RsRTTITypeError     = '???';
  RsRTTITypeInfoAt    = 'Type info: %p';

  RsRTTIPropRead      = 'read';
  RsRTTIPropWrite     = 'write';
  RsRTTIPropStored    = 'stored';

  RsRTTIField         = 'field';
  RsRTTIStaticMethod  = 'static method';
  RsRTTIVirtualMethod = 'virtual method';

  RsRTTIIndex         = 'index';
  RsRTTIDefault       = 'default';

  RsRTTIName          = 'Name: ';
  RsRTTIType          = 'Type: ';
  RsRTTIFlags         = 'Flags: ';
  RsRTTIGUID          = 'GUID: ';
  RsRTTITypeKind      = 'Type kind: ';
  RsRTTIOrdinalType   = 'Ordinal type: ';
  RsRTTIMinValue      = 'Min value: ';
  RsRTTIMaxValue      = 'Max value: ';
  RsRTTINameList      = 'Names: ';
  RsRTTIClassName     = 'Class name: ';
  RsRTTIParent        = 'Parent: ';
  RsRTTIPropCount     = 'Property count: ';
  RsRTTIUnitName      = 'Unit name: ';
  RsRTTIBasedOn       = 'Based on: ';
  RsRTTIFloatType     = 'Float type: ';
  RsRTTIMethodKind    = 'Method kind: ';
  RsRTTIParamCount    = 'Parameter count: ';
  RsRTTIReturnType    = 'Return type: ';
  RsRTTIMaxLen        = 'Max length: ';
  RsRTTIElSize        = 'Element size: ';
  RsRTTIElType        = 'Element type: ';
  RsRTTIElNeedCleanup = 'Elements need clean up: ';
  RsRTTIVarType       = 'Variant type: ';

  RsDeclarationFormat = '// Declaration for ''%s'' not supported.';

//=== JclSchedule ============================================================
resourcestring
  RsScheduleInvalidTime     = 'Invalid time specification';
  RsScheduleEndBeforeStart  = 'End time can not be before start time';
  RsScheduleIntervalZero    = 'Interval should be larger than 0';
  RsScheduleNoDaySpecified  = 'At least one day of the week should be specified';
  RsScheduleIndexValueSup   = 'Property IndexValue not supported for current IndexKind';
  RsScheduleIndexValueZero  = 'IndexValue can not be 0';
  RsScheduleDayNotSupported = 'Property Day not supported for current IndexKind';
  RsScheduleDayInRange      = 'Day values should fall in the range 1 .. 31';
  RsScheduleMonthInRange    = 'Month values should fall in the range 1 .. 12';

//=== JclSecurity ============================================================
resourcestring
  RsInvalidSID = 'Invalid SID';
  RsSIDBufferTooSmall = 'SID buffer too small.';
  RsLsaError = 'LSA Error: NT Status = %.8x, message: %s'; 

//=== JclSimpleXml ===========================================================
resourcestring
  RsEInvalidXMLElementUnexpectedCharacte =
    'Invalid XML Element: Unexpected character in property declaration ("%s" found at position %d)';
  RsEInvalidXMLElementUnexpectedCharacte_ =
    'Invalid XML Element: Unexpected character in property declaration. Expecting " or '' but "%s"  found at position %d';
  RsEUnexpectedValueForLPos = 'Unexpected value for lPos at position %d';
  RsEInvalidXMLElementExpectedBeginningO = 'Invalid XML Element: Expected beginning of tag but "%s" found at position %d';
  RsEInvalidXMLElementExpectedEndOfTagBu = 'Invalid XML Element: Expected end of tag but "%s" found at position %d';
  RsEInvalidXMLElementMalformedTagFoundn = 'Invalid XML Element: malformed tag found (no valid name) at position %d';
  RsEInvalidXMLElementErroneousEndOfTagE =
    'Invalid XML Element: Erroneous end of tag, expecting </%0:s> but </%1:s> found at position %d';
  RsEInvalidCommentExpectedsButFounds = 'Invalid Comment: expected "%0:s" but found "%1:s" at position %d';
  RsEInvalidCommentNotAllowedInsideComme = 'Invalid Comment: "--" not allowed inside comments at position %d';
  RsEInvalidCommentUnexpectedEndOfData = 'Invalid Comment: Unexpected end of data at position %d';
  RsEInvalidCDATAExpectedsButFounds = 'Invalid CDATA: expected "%0:s" but found "%1:s" at position %d';
  RsEInvalidCDATAUnexpectedEndOfData = 'Invalid CDATA: Unexpected end of data at position %d';
  RsEInvalidHeaderExpectedsButFounds = 'Invalid Header: expected "%0:s" but found "%1:s" at position %d';
  RsEInvalidStylesheetExpectedsButFounds = 'Invalid Stylesheet: expected "%0:s" but found "%1:s" at position %d';
  RsEInvalidStylesheetUnexpectedEndOfDat = 'Invalid Stylesheet: Unexpected end of data at position %d';
  RsEInvalidMSOExpectedsButFounds = 'Invalid MSO: expected "%0:s" but found "%1:s" at position %d';
  RsEInvalidMSOUnexpectedEndOfDat = 'Invalid MSO: Unexpected end of data at position %d';
  RsEInvalidDocumentUnexpectedTextInFile = 'Invalid Document: Unexpected text in file prolog at position %d';

//=== JclStatistics ==========================================================
resourcestring
  RsInvalidSampleSize = 'Invalid sample size (%d)';

//=== JclStreams =============================================================
resourcestring
  RsStreamsCreateError = 'Cannot create file %s';
  RsStreamsOpenError = 'Cannot open file %s';
  RsStreamsSetSizeError = 'Error setting stream size';
  RsStreamsSeekError = 'Error seeking stream';
  RsStreamsCRCError = 'Cyclic Redundency Check (CRC) error: data are damaged';

//=== JclStrHashMap ==========================================================
resourcestring
  RsStringHashMapMustBeEmpty = 'HashList: must be empty to set size to zero';
  RsStringHashMapDuplicate   = 'Duplicate hash list entry: %s';
  RsStringHashMapInvalidNode = 'Tried to remove invalid node: %s';
  RsStringHashMapNoTraits    = 'HashList must have traits';

//=== JclStrings =============================================================
resourcestring
  RsBlankSearchString       = 'Search string cannot be blank';
  RsInvalidEmptyStringItem  = 'String list passed to StringsToMultiSz cannot contain empty strings.';
  RsNumericConstantTooLarge = 'Numeric constant too large (%d) at position %d.';
  RsFormatException         = 'Format exception';
  RsDotNetFormatNullFormat  = 'Format string is null';
  RsArgumentIsNull          = 'Argument %d is null';
  RsDotNetFormatArgumentNotSupported = 'Argument type of %d is not supported';
  RsDotNetFormatObjectArgumentNotSupported = 'Argument type (%s) of %d is not supported';
  RsArgumentOutOfRange      = 'Argument out of range';
  RsTabs_DuplicatesNotAllowed = 'Duplicate tab stops are not allowed.';
  RsTabs_StopExpected = 'A tab stop was expected but not found.';
  RsTabs_CloseBracketExpected = 'Closing bracket expected.';
  RsTabs_TabWidthExpected = 'Tab width expected.';
  // Default text for the NullReferenceException in .NET
  RsArg_NullReferenceException = 'Object reference not set to an instance of an object.';

//=== JclStructStorage =======================================================
resourcestring
  RsIStreamNil = 'IStream is nil';

//=== JclSynch ===============================================================
resourcestring
  RsSynchAttachWin32Handle    = 'Invalid handle to TJclWin32HandleObject.Attach';
  RsSynchDuplicateWin32Handle = 'Invalid handle to TJclWin32HandleObject.Duplicate';
  RsSynchInitCriticalSection  = 'Failed to initalize critical section';
  RsSynchAttachDispatcher     = 'Invalid handle to TJclDispatcherObject.Attach';
  RsSynchCreateEvent          = 'Failed to create event';
  RsSynchOpenEvent            = 'Failed to open event';
  RsSynchCreateWaitableTimer  = 'Failed to create waitable timer';
  RsSynchOpenWaitableTimer    = 'Failed to open waitable timer';
  RsSynchCreateSemaphore      = 'Failed to create semaphore';
  RsSynchOpenSemaphore        = 'Failed to open semaphore';
  RsSynchCreateMutex          = 'Failed to create mutex';
  RsSynchOpenMutex            = 'Failed to open mutex';
  RsMetSectInvalidParameter   = 'An invalid parameter was passed to the constructor.';
  RsMetSectInitialize         = 'Failed to initialize the metered section.';
  RsMetSectNameEmpty          = 'Name cannot be empty when using the Open constructor.';

//=== JclSysInfo =============================================================
resourcestring
  RsSystemProcess     = 'System Process';
  RsSystemIdleProcess = 'System Idle Process';

  RsIntelUnknownCache = 'Unknown cache ID (%.2x)';
  RsIntelCacheDescr00 = 'Null descriptor';
  RsIntelCacheDescr01 = 'Instruction TLB: 4 KByte pages, 4-way set associative, 32 entries';
  RsIntelCacheDescr02 = 'Instruction TLB: 4 MByte pages, 4-way set associative, 2 entries';
  RsIntelCacheDescr03 = 'Data TLB: 4 KByte pages, 4-way set associative, 64 entries';
  RsIntelCacheDescr04 = 'Data TLB: 4 MByte pages, 4-way set associative, 8 entries';
  RsIntelCacheDescr05 = 'Data TLB1: 4 MByte pages, 4-way set associative, 32 entries';
  RsIntelCacheDescr06 = '1st level instruction cache: 8 KBytes, 4-way set associative, 32 byte line size';
  RsIntelCacheDescr08 = '1st level instruction cache: 16 KBytes, 4-way set associative, 32 byte line size';
  RsIntelCacheDescr09 = '1st level instruction cache: 32 KBytes, 4-way set associative, 64 byte line size';
  RsIntelCacheDescr0A = '1st level data cache: 8 KBytes, 2-way set associative, 32 byte line size';
  RsIntelCacheDescr0B = 'Instruction TLB: 4 MByte pages, 4-way set associative, 4 entries';
  RsIntelCacheDescr0C = '1st level data cache: 16 KBytes, 4-way set associative, 32 byte line size';
  RsIntelCacheDescr0D = '1st level data cache: 16 KBytes, 4-way set associative, 64 byte line size';
  RsIntelCacheDescr0E = '1st level data cache: 24 KBytes, 6-way set associative, 64 byte line size';
  RsIntelCacheDescr21 = '2nd level cache: 256 KBytes, 8-way set associative, 64 byte line size';
  RsIntelCacheDescr22 = '3rd level cache: 512 KBytes, 4-way set associative, 64 byte line size, 2 lines per sector';
  RsIntelCacheDescr23 = '3rd level cache: 1 MBytes, 8-way set associative, 64 byte line size, 2 lines per sector';
  RsIntelCacheDescr25 = '3rd level cache: 2 MBytes, 8-way set associative, 64 byte line size, 2 lines per sector';
  RsIntelCacheDescr29 = '3rd level cache: 4 MBytes, 8-way set associative, 64 byte line size, 2 lines per sector';
  RsIntelCacheDescr2C = '1st level data cache: 32 KBytes, 8-way set associative, 64 byte line size';
  RsIntelCacheDescr30 = '1st level instruction cache: 32 KBytes, 8-way set associative, 64 byte line size';
  RsIntelCacheDescr39 = '2nd-level cache: 128 KBytes, 4-way set associative, sectored cache, 64-byte line size';
  RsIntelCacheDescr3A = '2nd-level cache: 192 KBytes, 6-way set associative, sectored cache, 64-byte line size';
  RsIntelCacheDescr3B = '2nd-level cache: 128 KBytes, 2-way set associative, sectored cache, 64-byte line size';
  RsIntelCacheDescr3C = '2nd-level cache: 256 KBytes, 4-way set associative, sectored cache, 64-byte line size';
  RsIntelCacheDescr3D = '2nd-level cache: 384 KBytes, 6-way set associative, sectored cache, 64-byte line size';
  RsIntelCacheDescr3E = '2nd-level cache: 512 KBytes, 4-way set associative, sectored cache, 64-byte line size';
  RsIntelCacheDescr40 = 'No 2nd-level cache or, if processor contains a valid 2nd-level cache, no 3rd-level cache';
  RsIntelCacheDescr41 = '2nd-level cache: 128 KBytes, 4-way set associative, 32 byte line size';
  RsIntelCacheDescr42 = '2nd-level cache: 256 KBytes, 4-way set associative, 32 byte line size';
  RsIntelCacheDescr43 = '2nd-level cache: 512 KBytes, 4-way set associative, 32 byte line size';
  RsIntelCacheDescr44 = '2nd-level cache: 1 MBytes, 4-way set associative, 32 byte line size';
  RsIntelCacheDescr45 = '2nd-level cache: 2 MBytes, 4-way set associative, 32 byte line size';
  RsIntelCacheDescr46 = '3rd-level cache: 4 MBytes, 4-way set associative, 64 byte line size';
  RsIntelCacheDescr47 = '3rd-level cache: 8 MBytes, 4-way set associative, 64 byte line size';
  RsIntelCacheDescr48 = '3rd-level cache: 8 MByte, 8-way set associative, 64 byte line size';
  RsIntelCacheDescr49 = '2nd-level cache: 4 MBytes, 16-way set associative, 64 byte line size';
  RsIntelCacheDescr4A = '3rd-level cache: 6MByte, 12-way set associative, 64 byte line size';
  RsIntelCacheDescr4B = '3rd-level cache: 8MByte, 16-way set associative, 64 byte line size';
  RsIntelCacheDescr4C = '3rd-level cache: 12MByte, 12-way set associative, 64 byte line size';
  RsIntelCacheDescr4D = '3rd-level cache: 16MByte, 16-way set associative, 64 byte line size';
  RsIntelCacheDescr4E = '2nd-level cache: 6MByte, 24-way set associative, 64 byte line size';
  RsIntelCacheDescr4F = 'Instruction TLB: 4 KByte pages, 32 Entries';
  RsIntelCacheDescr50 = 'Instruction TLB: 4 KByte and 2 MByte or 4 MByte pages, 64 Entries';
  RsIntelCacheDescr51 = 'Instruction TLB: 4 KByte and 2 MByte or 4 MByte pages, 128 Entries';
  RsIntelCacheDescr52 = 'Instruction TLB: 4 KByte and 2 MByte or 4 MByte pages, 256 Entries';
  RsIntelCacheDescr55 = 'Instruction TLB: 2-MByte or 4-MByte pages, fully associative, 7 entries';
  RsIntelCacheDescr56 = 'Data TLB0: 4 MByte pages, 4-way set associative, 16 entries';
  RsIntelCacheDescr57 = 'Data TLB0: 4 KByte pages, 4-way associative, 16 entries';
  RsIntelCacheDescr59 = 'Data TLB0: 4 KByte pages, fully associative, 16 entries';
  RsIntelCacheDescr5A = 'Data TLB0: 2 MByte or 4 MByte pages, 4-way set associative, 32 entries';
  RsIntelCacheDescr5B = 'Data TLB: 4 KByte and 4 MByte pages, 64 Entries';
  RsIntelCacheDescr5C = 'Data TLB: 4 KByte and 4 MByte pages, 128 Entries';
  RsIntelCacheDescr5D = 'Data TLB: 4 KByte and 4 MByte pages, 256 Entries';
  RsIntelCacheDescr60 = '1st-level data cache: 16 KByte, 8-way set associative, 64 byte line size';
  RsIntelCacheDescr66 = '1st-level data cache: 8 KBytes, 4-way set associative, 64 byte line size';
  RsIntelCacheDescr67 = '1st-level data cache: 16 KBytes, 4-way set associative, 64 byte line size';
  RsIntelCacheDescr68 = '1st-level data cache: 32 KBytes, 4-way set associative, 64 byte line size';
  RsIntelCacheDescr70 = 'Trace cache: 12 K-Ops, 8-way set associative';
  RsIntelCacheDescr71 = 'Trace cache: 16 K-Ops, 8-way set associative';
  RsIntelCacheDescr72 = 'Trace cache: 32 K-Ops, 8-way set associative';
  RsIntelCacheDescr73 = 'Trace cache: 64 K-Ops, 8-way set associative';
  RsIntelCacheDescr76 = 'Instruction TLB: 2M/4M pages, fully associative, 8 entries';
  RsIntelCacheDescr78 = '2nd-level cache: 1 MBytes, 4-way set associative, 64 bytes line size';
  RsIntelCacheDescr79 = '2nd-level cache: 128 KBytes, 8-way set associative, 64 bytes line size, 2 lines per sector';
  RsIntelCacheDescr7A = '2nd-level cache: 256 KBytes, 8-way set associative, 64 bytes line size, 2 lines per sector';
  RsIntelCacheDescr7B = '2nd-level cache: 512 KBytes, 8-way set associative, 64 bytes line size, 2 lines per sector';
  RsIntelCacheDescr7C = '2nd-level cache: 1 MBytes, 8-way set associative, 64 bytes line size, 2 lines per sector';
  RsIntelCacheDescr7D = '2nd-level cache: 2 MBytes, 8-way set associative, 64 byte line size';
  RsIntelCacheDescr7F = '2nd-level cache: 512 KBytes, 2-way set associative, 64 byte line size';
  RsIntelCacheDescr80 = '2nd-level cache: 512 KBytes, 8-way set associative, 64 byte line size';
  RsIntelCacheDescr82 = '2nd-level cache: 256 KBytes, 8-way associative, 32 byte line size';
  RsIntelCacheDescr83 = '2nd-level cache: 512 KBytes, 8-way associative, 32 byte line size';
  RsIntelCacheDescr84 = '2nd-level cache: 1 MBytes, 8-way associative, 32 byte line size';
  RsIntelCacheDescr85 = '2nd-level cache: 2 MBytes, 8-way associative, 32 byte line size';
  RsIntelCacheDescr86 = '2nd-level cache: 512 KByte, 4-way set associative, 64 byte line size';
  RsIntelCacheDescr87 = '2nd-level cache: 1 MByte, 8-way set associative, 64 byte line size';
  RsIntelCacheDescrB0 = 'Instruction TLB: 4 KByte pages, 4-way set associative, 128 entries';
  RsIntelCacheDescrB1 = 'Instruction TLB: 2 MByte pages, 4-way, 8 entries or 4 MByte pages, 4-way, 4 entries';
  RsIntelCacheDescrB2 = 'Instruction TLB: 4 KByte pages, 4-way set associative, 64 entries';
  RsIntelCacheDescrB3 = 'Data TLB: 4 KByte pages, 4-way set associative, 128 entries';
  RsIntelCacheDescrB4 = 'Data TLB1: 4 KByte pages, 4-way set associative, 256 entries';
  RsIntelCacheDescrBA = 'Data TLB1: 4 KByte pages, 4-way set associative, 64 entries';
  RsIntelCacheDescrC0 = 'Data TLB: 4 KByte and 4 MByte pages, 4-way set associative, 8 entries';
  RsIntelCacheDescrCA = 'Shared 2nd-Level TLB: 4 KByte pages, 4-way associative, 512 entries';
  RsIntelCacheDescrD0 = '3rd-level cache: 512 KByte, 4-way set associative, 64 byte line size';
  RsIntelCacheDescrD1 = '3rd-level cache: 1 MByte, 4-way set associative, 64 byte line size';
  RsIntelCacheDescrD2 = '3rd-level cache: 2 MByte, 4-way set associative, 64 byte line size';
  RsIntelCacheDescrD6 = '3rd-level cache: 1 MByte, 8-way set associative, 64 byte line size';
  RsIntelCacheDescrD7 = '3rd-level cache: 2 MByte, 8-way set associative, 64 byte line size';
  RsIntelCacheDescrD8 = '3rd-level cache: 4 MByte, 8-way set associative, 64 byte line size';
  RsIntelCacheDescrDC = '3rd-level cache: 1.5 MByte, 12-way set associative, 64 byte line size';
  RsIntelCacheDescrDD = '3rd-level cache: 3 MByte, 12-way set associative, 64 byte line size';
  RsIntelCacheDescrDE = '3rd-level cache: 6 MByte, 12-way set associative, 64 byte line size';
  RsIntelCacheDescrE2 = '3rd-level cache: 2 MByte, 16-way set associative, 64 byte line size';
  RsIntelCacheDescrE3 = '3rd-level cache: 4 MByte, 16-way set associative, 64 byte line size';
  RsIntelCacheDescrE4 = '3rd-level cache: 8 MByte, 16-way set associative, 64 byte line size';
  RsIntelCacheDescrEA = '3rd-level cache: 12 MByte, 24-way set associative, 64 byte line size';
  RsIntelCacheDescrEB = '3rd-level cache: 18 MByte, 24-way set associative, 64 byte line size';
  RsIntelCacheDescrEC = '3rd-level cache: 24 MByte, 24-way set associative, 64 byte line size';
  RsIntelCacheDescrF0 = '64-Byte Prefetching';
  RsIntelCacheDescrF1 = '128-Byte Prefetching';
  RsIntelCacheDescrFF = 'CPUID leaf 2 does not report cache descriptor information, use CPUID leaf 4 to query cache parameters';

  RsUnknownAMDModel = 'Unknown AMD (Model %d)';

  RsOSVersionWin95              = 'Windows 95';
  RsOSVersionWin95OSR2          = 'Windows 95 OSR2';
  RsOSVersionWin98              = 'Windows 98';
  RsOSVersionWin98SE            = 'Windows 98 SE';
  RsOSVersionWinME              = 'Windows ME';
  RsOSVersionWinNT3             = 'Windows NT 3.%u';
  RsOSVersionWinNT4             = 'Windows NT 4.%u';
  RsOSVersionWin2000            = 'Windows 2000';
  RsOSVersionWinXP              = 'Windows XP';
  RsOSVersionWin2003            = 'Windows Server 2003';
  RsOSVersionWin2003R2          = 'Windows Server 2003 R2';
  RsOSVersionWinXP64            = 'Windows XP x64';
  RsOSVersionWinVista           = 'Windows Vista';
  RsOSVersionWinServer2008      = 'Windows Server 2008';
  RsOSVersionWin7               = 'Windows 7';
  RsOSVersionWinServer2008R2    = 'Windows Server 2008 R2';
  RsOSVersionWin8               = 'Windows 8';
  RsOSVersionWin8RT             = 'Windows RT';
  RsOSVersionWinServer2012      = 'Windows Server 2012';
  RsOSVersionWin81              = 'Windows 8.1';
  RsOSVersionWin81RT            = 'Windows RT 8.1';
  RsOSVersionWinServer2012R2    = 'Windows Server 2012 R2';

  RsEditionWinXPHome            = 'Home Edition';
  RsEditionWinXPPro             = 'Professional';
  RsEditionWinXPHomeN           = 'Home Edition N';
  RsEditionWinXPProN            = 'Professional N';
  RsEditionWinXPHomeK           = 'Home Edition K';
  RsEditionWinXPProK            = 'Professional K';
  RsEditionWinXPHomeKN          = 'Home Edition KN';
  RsEditionWinXPProKN           = 'Professional KN';
  RsEditionWinXPStarter         = 'Starter Edition';
  RsEditionWinXPMediaCenter     = 'Media Center Edition';
  RsEditionWinXPTablet          = 'Tablet PC Edition';
  RsEditionWinVistaStarter      = 'Starter';
  RsEditionWinVistaHomeBasic    = 'Home Basic';
  RsEditionWinVistaHomeBasicN   = 'Home Basic N';
  RsEditionWinVistaHomePremium  = 'Home Premium';
  RsEditionWinVistaBusiness     = 'Business';
  RsEditionWinVistaBusinessN    = 'Business N';
  RsEditionWinVistaEnterprise   = 'Enterprise';
  RsEditionWinVistaUltimate     = 'Ultimate';
  RsEditionWin7Starter          = 'Starter';
  RsEditionWin7HomeBasic        = 'Home Basic';
  RsEditionWin7HomePremium      = 'Home Premium';
  RsEditionWin7Professional     = 'Professional';
  RsEditionWin7Enterprise       = 'Enterprise';
  RsEditionWin7Ultimate         = 'Ultimate';
  RsEditionWin8Pro              = 'Pro';
  RsEditionWin8Enterprise       = 'Enterprise';
  RsEditionWin8RT               = 'RT';
  RsEditionWin81Pro             = 'Pro';
  RsEditionWin81Enterprise      = 'Enterprise';
  RsEditionWin81RT              = 'RT';

  RsProductTypeWorkStation      = 'Workstation';
  RsProductTypeServer           = 'Server';
  RsProductTypeAdvancedServer   = 'Advanced Server';
  RsProductTypePersonal         = 'Home Edition';
  RsProductTypeProfessional     = 'Professional';
  RsProductTypeDatacenterServer = 'Datacenter Server';
  RsProductTypeEnterprise       = 'Enterprise';
  RsProductTypeWebEdition       = 'Web Edition';

  RsEOpenGLInfo = 'GetOpenGLVersion: %s failed';
  RsENetWkstaGetInfo = 'NetWkstaGetInfo failed';

  {$IFDEF MSWINDOWS}
  RsSPInfo = 'SP%u';
  {$ENDIF MSWINDOWS}

  {$IFDEF UNIX}
  RsInvalidProcessID = 'Invalid process ID %d';
  {$ENDIF UNIX}

  RsOpenGLInfoError = 'Err';

//=== JclSysUtils ============================================================
resourcestring
  RsVMTMemoryWriteError  = 'Error writing VMT memory (%s)';
  RsCannotWriteRefStream = 'Can not write to a read-only memory stream';
  RsStringToBoolean      = 'Unable to convert the string "%s" to a boolean';
  RsInvalidDigit         = 'Invalid base %d digit ''%s'' encountered.';
  RsInvalidDigitValue    = 'There is no valid base %d digit for decimal value %d';

  {$IFDEF UNIX}
  RsReadKeyError         = 'ReadKey: Problem waiting on stdin';
  {$ENDIF UNIX}

  RsInvalidGUIDString    = 'Invalid conversion from string to GUID (%s).';

  RsInvalidMMFName = 'Invalid MMF name "%s"';
  RsInvalidMMFEmpty = 'The MMF named "%s" cannot be created empty';

//=== JclTD32 ================================================================
resourcestring
  RsHasNotTD32Info = 'File [%s] has not TD32 debug information!';

//=== JclTimeZones ===========================================================
resourcestring
  RsEDaylightSavingsNotSupported = 'Daylight Savings not supported by this timezone';
  RsEAutoAdjustNotEnabled = 'Auto adjust for Daylight Savings is not enabled.  Date is not available';
  RsENoCallbackFunc = 'No callback function assigned';

//=== JclUnicode =============================================================
resourcestring
  RsUREErrorFmt               = '%s%s%s';
  RsUREBaseString             = 'Error in regular expression: %s' + sLineBreak;
  RsUREUnexpectedEOS          = 'Unexpected end of pattern.';
  RsURECharacterClassOpen     = 'Character class not closed, '']'' is missing.';
  RsUREUnbalancedGroup        = 'Unbalanced group expression, '')'' is missing.';
  RsUREInvalidCharProperty    = 'A character property is invalid';
  RsUREInvalidRepeatRange     = 'Invalid repetition range.';
  RsURERepeatRangeOpen        = 'Repetition range not closed, ''}'' is missing.';
  RsUREExpressionEmpty        = 'Expression is empty.';
  RsCategoryUnicodeChar       = 'category Unicode character > $FFFFFF found';
  RsCasedUnicodeChar          = 'cased Unicode character > $FFFFFF found';
  RsDecomposedUnicodeChar     = 'decomposed Unicode character > $FFFFFF found';
  RsCombiningClassUnicodeChar = 'combining class for Unicode character > $FFFFFF found';
  RsEUnexpectedEOSeq          = 'Unexpected end of sequence';

//=== JclUnitConv ============================================================
resourcestring
  RsTempConvTypeError = 'An invalid type has been provided for the %s parameter';
  RsConvTempBelowAbsoluteZero = 'Temperature can not be below Absolute Zero!';

//=== JclWin32 ===============================================================
resourcestring
  RsWin32Error        = 'Win32 error: %s (%u)%s%s';
  RsELibraryNotFound  = 'Library not found: %s';
  RsEFunctionNotFound = 'Function not found: %s.%s';

//=== JclWinMidi =============================================================
resourcestring
  RsMidiInUnknownError  = 'Unknown MIDI-In error No. %d';
  RsMidiOutUnknownError = 'Unknown MIDI-Out error No. %d';

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

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
