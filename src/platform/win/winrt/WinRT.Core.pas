unit WinRT.Core;

{$mode delphi}

interface

uses
  Classes, SysUtils, Windows;

type
  HSTRING = HANDLE;
  PCNZWCH = PWideChar;

  TrustLevel = (
    BaseTrust = 0,
    PartialTrust = (BaseTrust + 1),
    FullTrust = (PartialTrust + 1)
  );

  IInspectable = interface(IUnknown)
    ['{AF86E2E0-B12D-4c6a-9C5A-D7AA65101E90}']
    function GetIids(out iidCount: ULONG; out iids: PIID): HRESULT; stdcall;
    function GetRuntimeClassName(out className: HSTRING): HRESULT; stdcall;
    function GetTrustLevel(out trustLevel: TrustLevel): HRESULT; stdcall;
  end;

  { TWindowsString }

  TWindowsString = class
  private
    FHandle: HSTRING;
  public
    constructor Create(AHandle: HSTRING); overload;
    constructor Create(const AString: String); overload;
    constructor Create(const AString: UnicodeString); overload;
    destructor Destroy; override;
    function ToString: AnsiString; override;
    property Handle: HSTRING read FHandle;
  end;

function RtActivateInstance(const AClassName: UnicodeString; out AInstance): HRESULT;
function RoCreateInstance(const AClassName: UnicodeString; constref AClassID: TIID; out AInstance): HRESULT;

implementation

uses
  ComObj;

type
  RO_INIT_TYPE = (
    RO_INIT_SINGLETHREADED = 0,
    RO_INIT_MULTITHREADED = 1
  );

var
  RoInitialize: function(initType: RO_INIT_TYPE): HRESULT; stdcall;
  RoUninitialize: procedure(); stdcall;

  RoActivateInstance: function(activatableClassId: HSTRING;
                               out instance): HRESULT; stdcall;
  RoGetActivationFactory: function(activatableClassId: HSTRING;
                                   constref iid: TIID;
                                   out factory: IInspectable): HRESULT; stdcall;


var
  WindowsCreateString: function(sourceString: PCNZWCH; length: UINT32;
                                out str: HSTRING): HRESULT; stdcall;
  WindowsDeleteString: function(str: HSTRING): HRESULT; stdcall;
  WindowsGetStringRawBuffer: function(str: HSTRING; length: PUINT32): PCWSTR; stdcall;

const
  libwinrt = 'api-ms-win-core-winrt-l1-1-0.dll';
  libwinrt_string = 'api-ms-win-core-winrt-string-l1-1-0.dll';

var
  hWinRT: TLibHandle;
  hWinRTString: TLibHandle;

procedure Initialize;
begin
  if CheckWin32Version(10) then
  begin
    hWinRT:= LoadLibrary(libwinrt);
    if (hWinRT <> NilHandle) then
    try
      @RoInitialize:= GetProcAddress(hWinRT, 'RoInitialize');
      @RoUninitialize:= GetProcAddress(hWinRT, 'RoUninitialize');
      @RoActivateInstance:= GetProcAddress(hWinRT, 'RoActivateInstance');
      @RoGetActivationFactory:= GetProcAddress(hWinRT, 'RoGetActivationFactory');

      RoInitialize(RO_INIT_MULTITHREADED);
    except
      FreeLibrary(hWinRT);
      hWinRT:= NilHandle;
    end;
    hWinRTString:= LoadLibrary(libwinrt_string);
    if (hWinRTString <> NilHandle) then
    try
      @WindowsCreateString:= GetProcAddress(hWinRTString, 'WindowsCreateString');
      @WindowsDeleteString:= GetProcAddress(hWinRTString, 'WindowsDeleteString');
      @WindowsGetStringRawBuffer:= GetProcAddress(hWinRTString, 'WindowsGetStringRawBuffer');
    except
      FreeLibrary(hWinRTString);
      hWinRTString:= NilHandle;
    end;
  end;
end;

procedure Finalize;
begin
  if (hWinRT <> NilHandle) then
  begin
    RoUninitialize;
    FreeLibrary(hWinRT);
  end;
  if (hWinRTString <> NilHandle) then
  begin
    FreeLibrary(hWinRTString);
  end;
end;

function RtActivateInstance(const AClassName: UnicodeString; out AInstance): HRESULT;
var
  AName: TWindowsString;
begin
  AName:= TWindowsString.Create(AClassName);
  try
    Result:= RoActivateInstance(AName.FHandle, AInstance);
  finally
    AName.Free;
  end;
end;

function RoCreateInstance(const AClassName: UnicodeString; constref AClassID: TIID; out AInstance): HRESULT;
var
  AName: TWindowsString;
  AFactory: IInspectable;
begin
  AName:= TWindowsString.Create(AClassName);
  try
    Result:= RoGetActivationFactory(AName.FHandle, AClassID, AFactory);
    if Succeeded(Result) then begin
      Result:= AFactory.QueryInterface(AClassID, AInstance);
    end;
  finally
    AName.Free;
  end;
end;

{ TWindowsString }

constructor TWindowsString.Create(const AString: String);
begin
  Create(UTF8Decode(AString));
end;

constructor TWindowsString.Create(AHandle: HSTRING);
begin
  FHandle:= AHandle;
end;

constructor TWindowsString.Create(const AString: UnicodeString);
begin
  OleCheck(WindowsCreateString(PWideChar(AString), Length(AString), FHandle));
end;

destructor TWindowsString.Destroy;
begin
  inherited Destroy;
  if (FHandle <> 0) then WindowsDeleteString(FHandle);
end;

function TWindowsString.ToString: AnsiString;
var
  P: PWideChar;
  L: UINT32 = 0;
  usString: UnicodeString;
begin
  P:= WindowsGetStringRawBuffer(FHandle, @L);
  SetString(usString, P, L);
  Result:= UTF8Encode(usString);
end;

initialization
  Initialize;

finalization
  Finalize;

end.
