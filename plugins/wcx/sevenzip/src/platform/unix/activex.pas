unit ActiveX;

{$mode delphi}

{.$define Z7_USE_VIRTUAL_DESTRUCTOR_IN_IUNKNOWN}

{
  p7zip and 7-Zip before v23 used virtual destructor in IUnknown,
  if _WIN32 is not defined.
  It used virtual destructor, because some compilers don't like virtual
  interfaces without virtual destructor.
  IUnknown in Windows (_WIN32) doesn't use virtual destructor in IUnknown.
  We still can define Z7_USE_VIRTUAL_DESTRUCTOR_IN_IUNKNOWN here,
  if we want to be compatible with old plugin interface of p7zip and 7-Zip before v23.

  v23:
    In new 7-Zip v23 we try to be more compatible with original IUnknown from _WIN32.
    So we do not define Z7_USE_VIRTUAL_DESTRUCTOR_IN_IUNKNOWN here
}

interface

uses
  SysUtils, Types, Windows;

const
  STREAM_SEEK_SET             = 0;
  STREAM_SEEK_CUR             = 1;
  STREAM_SEEK_END             = 2;

  E_ABORT = HRESULT($80004004);

  VT_EMPTY                    = 0;
  VT_NULL                     = 1;
  VT_I2                       = 2;
  VT_I4                       = 3;
  VT_R4                       = 4;
  VT_R8                       = 5;
  VT_CY                       = 6;
  VT_DATE                     = 7;
  VT_BSTR                     = 8;
  VT_DISPATCH                 = 9;
  VT_ERROR                    = 10;
  VT_BOOL                     = 11;
  VT_VARIANT                  = 12;
  VT_UNKNOWN                  = 13;
  VT_DECIMAL                  = 14;
// VBA reserves 15 for future use
  VT_I1                       = 16;
  VT_UI1                      = 17;
  VT_UI2                      = 18;
  VT_UI4                      = 19;
  VT_I8                       = 20;
  VT_UI8                      = 21;
  VT_INT                      = 22;
  VT_UINT                     = 23;
  VT_VOID                     = 24;
  VT_HRESULT                  = 25;
  VT_PTR                      = 26;
  VT_SAFEARRAY                = 27;
  VT_CARRAY                   = 28;
  VT_USERDEFINED              = 29;
  VT_LPSTR                    = 30;
  VT_LPWSTR                   = 31;
// VBA reserves 32-35 for future use
  VT_RECORD                   = 36;
  VT_INT_PTR                  = 37;
  VT_UINT_PTR                 = 38;

  VT_FILETIME                 = 64;
  VT_BLOB                     = 65;
  VT_STREAM                   = 66;
  VT_STORAGE                  = 67;
  VT_STREAMED_OBJECT          = 68;
  VT_STORED_OBJECT            = 69;
  VT_BLOB_OBJECT              = 70;
  VT_CF                       = 71;
  VT_CLSID                    = 72;
  VT_VERSIONED_STREAM         = 73;

  VT_BSTR_BLOB                = $0fff;

  VT_VECTOR                   = $1000;
  VT_ARRAY                    = $2000;
  VT_BYREF                    = $4000;
  VT_RESERVED                 = $8000;

  VT_ILLEGAL                  = $ffff;
  VT_ILLEGALMASKED            = $0fff;
  VT_TYPEMASK                 = $0fff;

type
  TOleChar = Types.TOleChar;
  POleStr = Types.POleStr;
  PPOleStr = Types.PPOleStr;
  TBStr = POleStr;

  BSTR  = POLESTR;
  PBStr = ^TBStr;

  PROPID  = UInt32;
  TPROPID = PROPID;
  PPROPID = ^PROPID;

  VARIANT_BOOL        = wordbool;
  _VARIANT_BOOL       = VARIANT_BOOL;

  SCODE               = Long;

  CY       = CURRENCY;
  DATE     = DOUBLE;

  PROPVAR_PAD1 = WORD;
  PROPVAR_PAD2 = WORD;
  PROPVAR_PAD3 = WORD;

  { size of this record must be 16, i.e. match Variant }
  TPROPVARIANT = packed record
         vt : TVarType;
         wReserved1 : PROPVAR_PAD1;
         wReserved2 : PROPVAR_PAD2;
         wReserved3 : PROPVAR_PAD3;
         case longint of
                0 : ( cVal : CHAR );
                1 : ( bVal : UCHAR );
                2 : ( iVal : SHORT );
                3 : ( uiVal : USHORT );
                4 : ( lVal : LONG );
                5 : ( ulVal : ULONG );
                6 : ( intVal : longINT );
                7 : ( uintVal : UINT );
                8 : ( hVal : LARGE_INTEGER );
                9 : ( uhVal : ULARGE_INTEGER );
                10 : ( fltVal : SINGLE );
                11 : ( dblVal : DOUBLE );
                12 : ( boolVal : VARIANT_BOOL );
                13 : ( bool : _VARIANT_BOOL );
                14 : ( scode : SCODE );
                15 : ( cyVal : CY );
                16 : ( date : DATE );
                17 : ( filetime : TFileTime );
                18 : ( puuid : ^CLSID );
                20 : ( bstrVal : BSTR );
                23 : ( pszVal : LPSTR );
                24 : ( pwszVal : LPWSTR );
            end;
  PROPVARIANT = TPROPVARIANT;
  TagPROPVARIANT = TPROPVARIANT;
  PPropVariant = ^TPROPVARIANT;

{$IFDEF Z7_USE_VIRTUAL_DESTRUCTOR_IN_IUNKNOWN}

type
  IUnknown = interface(System.IUnknown)
    ['{1E891736-3630-4316-A24F-B549F732452B}']
    procedure Dummy; winapi;
    procedure Dummy2; winapi;
  end;

  { TInterfacedObject }

  TInterfacedObject = class(System.TInterfacedObject)
    procedure Dummy; winapi;
    procedure Dummy2; winapi;
  end;

{$ENDIF}

function Succeeded(Res: HResult) : Boolean;inline;

implementation

function Succeeded(Res: HResult) : Boolean;inline;
  begin
    Result := Res and $80000000 = 0;
  end;

{$IFDEF Z7_USE_VIRTUAL_DESTRUCTOR_IN_IUNKNOWN}

{ TInterfacedObject }

procedure TInterfacedObject.Dummy;
begin

end;

procedure TInterfacedObject.Dummy2;
begin

end;

{$ENDIF}

end.
