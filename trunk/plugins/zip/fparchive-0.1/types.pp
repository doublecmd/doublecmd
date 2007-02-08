{
    $Id: types.pp,v 1.1 2003/08/16 22:44:37 michael Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002 by Florian Klaempfl,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$MODE OBJFPC}
unit types;

  interface

{$ifdef Win32}
    uses
       Windows;
{$endif Win32}

Const
   MAX_PATH = 4095;

Type
       BOOL = LongBool;
   
{$ifndef ver1_0}
    const
       RT_RCDATA = PChar(10);

    type
       DWORD = LongWord;
       
       PLongint = System.PLongint;
       PInteger = System.PInteger;
       PSmallInt = System.PSmallInt;
       PDouble = System.PDouble;
       PByte = System.PByte;

{$ifdef Win32}
       TPoint = Windows.TPoint;
{$else}
       TPoint = packed record
          X : Longint;
          Y : Longint;
       end;
{$endif}
       PPoint = ^TPoint;
       tagPOINT = TPoint;

{$ifdef Win32}
       TRect = Windows.TRect;
{$else}
       TRect = packed record
          case Integer of
             0: (Left,Top,Right,Bottom : Longint);
             1: (TopLeft,BottomRight : TPoint);
       end;
{$endif}
       PRect = ^TRect;

       TSize = packed record
          cx : Longint;
          cy : Longint;
       end;
       PSize = ^TSize;
       tagSIZE = TSize;
       SIZE = TSize;

       TSmallPoint = packed record
          x : SmallInt;
          y : SmallInt;
       end;
       PSmallPoint = ^TSmallPoint;


{$ifndef win32}
    const
      STGTY_STORAGE   = 1;
      STGTY_STREAM    = 2;
      STGTY_LOCKBYTES = 3;
      STGTY_PROPERTY  = 4;

      STREAM_SEEK_SET = 0;
      STREAM_SEEK_CUR = 1;
      STREAM_SEEK_END = 2;

      LOCK_WRITE     = 1;
      LOCK_EXCLUSIVE = 2;
      LOCK_ONLYONCE  = 4;

      E_FAIL = HRESULT($80004005);

      STG_E_INVALIDFUNCTION = HRESULT($80030001);
      STG_E_FILENOTFOUND = HRESULT($80030002);
      STG_E_PATHNOTFOUND = HRESULT($80030003);
      STG_E_TOOMANYOPENFILES = HRESULT($80030004);
      STG_E_ACCESSDENIED = HRESULT($80030005);
      STG_E_INVALIDHANDLE = HRESULT($80030006);
      STG_E_INSUFFICIENTMEMORY = HRESULT($80030008);
      STG_E_INVALIDPOINTER = HRESULT($80030009);
      STG_E_NOMOREFILES = HRESULT($80030012);
      STG_E_DISKISWRITEPROTECTED = HRESULT($80030013);
      STG_E_SEEKERROR = HRESULT($80030019);
      STG_E_WRITEFAULT = HRESULT($8003001D);
      STG_E_READFAULT = HRESULT($8003001E);
      STG_E_SHAREVIOLATION = HRESULT($80030020);
      STG_E_LOCKVIOLATION = HRESULT($80030021);
      STG_E_FILEALREADYEXISTS = HRESULT($80030050);
      STG_E_INVALIDPARAMETER = HRESULT($80030057);
      STG_E_MEDIUMFULL = HRESULT($80030070);
      STG_E_PROPSETMISMATCHED = HRESULT($800300F0);
      STG_E_ABNORMALAPIEXIT = HRESULT($800300FA);
      STG_E_INVALIDHEADER = HRESULT($800300FB);
      STG_E_INVALIDNAME = HRESULT($800300FC);
      STG_E_UNKNOWN = HRESULT($800300FD);
      STG_E_UNIMPLEMENTEDFUNCTION = HRESULT($800300FE);
      STG_E_INVALIDFLAG = HRESULT($800300FF);
      STG_E_INUSE = HRESULT($80030100);
      STG_E_NOTCURRENT = HRESULT($80030101);
      STG_E_REVERTED = HRESULT($80030102);
      STG_E_CANTSAVE = HRESULT($80030103);
      STG_E_OLDFORMAT = HRESULT($80030104);
      STG_E_OLDDLL = HRESULT($80030105);
      STG_E_SHAREREQUIRED = HRESULT($80030106);
      STG_E_EXTANTMARSHALLINGS = HRESULT($80030108);
      STG_E_DOCFILECORRUPT = HRESULT($80030109);
      STG_E_BADBASEADDRESS = HRESULT($80030110);
      STG_E_INCOMPLETE = HRESULT($80030201);
      STG_E_TERMINATED = HRESULT($80030202);

      STG_S_CONVERTED = $00030200;
      STG_S_BLOCK = $00030201;
      STG_S_RETRYNOW = $00030202;
      STG_S_MONITORING = $00030203;

      GUID_NULL: TGUID = '{00000000-0000-0000-0000-000000000000}';

    type
       TOleChar = WideChar;
       POleStr = PWideChar;
       PPOleStr = ^POleStr;

       PCLSID = PGUID;
       TCLSID = TGUID;

       LARGE_INT = Int64;
       Largeint = LARGE_INT;
       PDWord = ^DWord;

       PDisplay = Pointer;
       PEvent = Pointer;

       TXrmOptionDescRec = record
       end;
       XrmOptionDescRec = TXrmOptionDescRec;
       PXrmOptionDescRec = ^TXrmOptionDescRec;

       Widget = Pointer;
       WidgetClass = Pointer;
       ArgList = Pointer;
       Region = Pointer;

       _FILETIME = packed record
          dwLowDateTime : DWORD;
          dwHighDateTime : DWORD;
       end;
       TFileTime = _FILETIME;
       FILETIME = _FILETIME;
       PFileTime = ^TFileTime;

       tagSTATSTG = packed record
          pwcsName : POleStr;
          dwType : Longint;
          cbSize : Largeint;
          mtime : TFileTime;
          ctime : TFileTime;
          atime : TFileTime;
          grfMode : Longint;
          grfLocksSupported : Longint;
          clsid : TCLSID;
          grfStateBits : Longint;
          reserved : Longint;
       end;
       TStatStg = tagSTATSTG;
       STATSTG = TStatStg;
       PStatStg = ^TStatStg;


       IClassFactory = Interface(IUnknown) ['{00000001-0000-0000-C000-000000000046}']
          Function CreateInstance(Const unkOuter : IUnknown;Const riid : TGUID;Out vObject) : HResult;StdCall;
          Function LockServer(fLock : LongBool) : HResult;StdCall;
       End;

       ISequentialStream = interface(IUnknown) ['{0c733a30-2a1c-11ce-ade5-00aa0044773d}']
          function Read(pv : Pointer;cb : DWord;pcbRead : PDWord) : HRESULT;stdcall;
          function Write(pv : Pointer;cb : DWord;pcbWritten : PDWord) : HRESULT;stdcall;
       end;

       IStream = interface(ISequentialStream) ['{0000000C-0000-0000-C000-000000000046}']
          function Seek(dlibMove : LargeInt; dwOrigin : Longint;
            out libNewPosition : LargeInt) : HResult;stdcall;
          function SetSize(libNewSize : LargeInt) : HRESULT;stdcall;
          function CopyTo(stm: IStream;cb : LargeInt;out cbRead : LargeInt;
            out cbWritten : LargeInt) : HRESULT;stdcall;
          function Commit(grfCommitFlags : Longint) : HRESULT;stdcall;
          function Revert : HRESULT;stdcall;
          function LockRegion(libOffset : LargeInt;cb : LargeInt;
            dwLockType : Longint) : HRESULT;stdcall;
          function UnlockRegion(libOffset : LargeInt;cb : LargeInt;
            dwLockType : Longint) : HRESULT;stdcall;
          Function Stat(out statstg : TStatStg;grfStatFlag : Longint) : HRESULT;stdcall;
          function Clone(out stm : IStream) : HRESULT;stdcall;
       end;
{$endif win32}

    function EqualRect(const r1,r2 : TRect) : Boolean;
    function Rect(Left,Top,Right,Bottom : Integer) : TRect;
    function Bounds(ALeft,ATop,AWidth,AHeight : Integer) : TRect;
    function Point(x,y : Integer) : TPoint;
    function PtInRect(const Rect : TRect; const p : TPoint) : Boolean;
    function IntersectRect(var Rect : TRect; const R1,R2 : TRect) : Boolean;
    function UnionRect(var Rect : TRect; const R1,R2 : TRect) : Boolean;
    function IsRectEmpty(const Rect : TRect) : Boolean;
    function OffsetRect(var Rect : TRect;DX : Integer;DY : Integer) : Boolean;

{$endif ver1_0}
Type
  PByteArray = ^TByteArray;
  TByteArray = array[0..32767] of Byte;
    
  PWordArray = ^TWordArray;
  TWordArray = array[0..16383] of Word;
        
        
  implementation

{$ifndef ver1_0}
    function EqualRect(const r1,r2 : TRect) : Boolean;
      begin
         EqualRect:=(r1.left=r2.left) and (r1.right=r2.right) and (r1.top=r2.top) and (r1.bottom=r2.bottom);
      end;

    function Rect(Left,Top,Right,Bottom : Integer) : TRect;
      begin
         Rect.Left:=Left;
         Rect.Top:=Top;
         Rect.Right:=Right;
         Rect.Bottom:=Bottom;
      end;

    function Bounds(ALeft,ATop,AWidth,AHeight : Integer) : TRect;
      begin
         Bounds.Left:=ALeft;
         Bounds.Top:=ATop;
         Bounds.Right:=ALeft+AWidth;
         Bounds.Bottom:=ATop+AHeight;
      end;

    function Point(x,y : Integer) : TPoint;
      begin
         Point.x:=x;
         Point.y:=y;
      end;

    function PtInRect(const Rect : TRect;const p : TPoint) : Boolean;
      begin
         PtInRect:=(p.y>=Rect.Top) and (p.y<Rect.Bottom) and (p.x>=Rect.Left) and (p.x<Rect.Right);
      end;

    function IntersectRect(var Rect : TRect;const R1,R2 : TRect) : Boolean;
      begin
         Rect:=R1;
         with R2 do
           begin
              if Left>R1.Left then
                Rect.Left:=Left;
              if Top>R1.Top then
                Rect.Top:=Top;
              if Right<R1.Right then
                Rect.Right:=Right;
              if Bottom<R1.Bottom then
                Rect.Bottom:=Bottom;
           end;
        if IsRectEmpty(Rect) then
          begin
             FillChar(Rect,SizeOf(Rect),0);
             IntersectRect:=false;
          end
        else
          IntersectRect:=true;
      end;

    function UnionRect(var Rect : TRect;const R1,R2 : TRect) : Boolean;
      begin
         Rect:=R1;
         with R2 do
           begin
              if Left<R1.Left then
                Rect.Left:=Left;
              if Top<R1.Top then
                Rect.Top:=Top;
              if Right>R1.Right then
                Rect.Right:=Right;
              if Bottom>R1.Bottom then
                Rect.Bottom:=Bottom;
           end;
        if IsRectEmpty(Rect) then
          begin
             FillChar(Rect,SizeOf(Rect),0);
             UnionRect:=false;
          end
        else
          UnionRect:=true;
     end;

    function IsRectEmpty(const Rect : TRect) : Boolean;
      begin
         IsRectEmpty:=(Rect.Right<=Rect.Left) or (Rect.Bottom<=Rect.Top);
      end;

    function OffsetRect(var Rect : TRect;DX : Integer;DY : Integer) : Boolean;
      begin
         if assigned(@Rect) then
           begin
              with Rect do
                begin
                   inc(Left,dx);
                   inc(Top,dy);
                   inc(Right,dx);
                   inc(Bottom,dy);
                end;
              OffsetRect:=true;
           end
         else
           OffsetRect:=false;
      end;

{$endif ver1_0}

end.
{
  $Log: types.pp,v $
  Revision 1.1  2003/08/16 22:44:37  michael
  + Initial import

  Revision 1.3  2003/01/02 22:22:12  peter
    * reuse Windows types to fix incompatible parameters

  Revision 1.2  2002/07/27 07:28:10  florian
    * fixed errror on non win32 targets with 1.0.x when doing a make cycle

  Revision 1.1  2002/07/23 20:39:54  florian
    + types unit added
}
