

{fake unit just to compile - not used under non windows}

{$mode delphi}

interface

uses
  {$ifdef Windows} Windows, {$endif} Classes, SysUtils, Types;

const
  TYMED_HGLOBAL = 1;
  TYMED_ISTREAM = 4;
  DVASPECT_CONTENT = 1;
  CLSCTX_INPROC_SERVER = $0010;
  DROPEFFECT_COPY = 1;
  DROPEFFECT_LINK = 4;
  DROPEFFECT_MOVE = 2;
  DROPEFFECT_NONE = 0;
  DROPEFFECT_SCROLL = dword($80000000);
  DATADIR_GET = 1;

type
  //types from win unit
  Long = LongInt;
  WinBool = LongBool;
  Bool = WinBool;
  ULONG  = cardinal;
  PULONG = ^ULONG;
  LONGLONG  = int64;
  LPDWORD = ^DWORD;
  LPVOID  = pointer;

  TCOLORREF = cardinal;

  TIID = TGUID;

  LARGE_INTEGER = record
  case byte of
    0: (LowPart : DWORD;
        HighPart : LONG);
    1: (QuadPart : LONGLONG);
  end;
  PLARGE_INTEGER = ^LARGE_INTEGER;
  _LARGE_INTEGER = LARGE_INTEGER;

  TLargeInteger = Int64;
  PLargeInteger = ^TLargeInteger;

  ULARGE_INTEGER = record
  case byte of
    0: (LowPart : DWORD;
        HighPart : DWORD);
    1: (QuadPart : LONGLONG);
  end;
  PULARGE_INTEGER = ^ULARGE_INTEGER;
  _ULARGE_INTEGER = ULARGE_INTEGER;


  HANDLE = System.THandle;
  HWND = HANDLE;
  //HRESULT = System.HResult;

  HBITMAP = HANDLE;
  HENHMETAFILE = HANDLE;

  //activex types


  IMoniker            = Interface;

  WINOLEAPI = HResult;
  TLCID = DWORD;

  OleChar             = WChar;
  LPOLESTR            = ^OLECHAR;
  HMetaFilePict       = Pointer;


  tagBIND_OPTS                 = Record
                                  cvStruct,          //  sizeof(BIND_OPTS)
                                  grfFlags,
                                  grfMode,
                                  dwTickCountDeadline : DWord;
                                 End;
  TBind_Opts                   = tagBIND_OPTS;
  TCLIPFORMAT                  = Word;

  tagDVTARGETDEVICE            = Record
                                    tdSize                     : DWord;
                                    tdDriverNameOffset,
                                    tdDeviceNameOffset,
                                    tdPortNameOffset,
                                    tdExtDevmodeOffset         : Word;
                                    Data                       : Record End;
                                    End;
  DVTARGETDEVICE               = TagDVTARGETDEVICE;
  PDVTARGETDEVICE              = ^tagDVTARGETDEVICE;



  tagFORMATETC                 = Record
                                  CfFormat :  Word {TCLIPFORMAT};
                                  Ptd      : PDVTARGETDEVICE;
                                  dwAspect : DWORD;
                                  lindex   : Long;
                                  tymed    : DWORD;
                                  End;
  FORMATETC                    = TagFORMATETC;
  TFORMATETC                   = FORMATETC;
  LPFORMATETC                  = ^FORMATETC;
  PFormatEtc                   = LPFORMATETC;

  tagSTATDATA                  = Record
                                                                // field used by:
                                    FORMATETC   : Tformatetc;   // EnumAdvise, EnumData (cache), EnumFormats
                                    advf        : DWord;        // EnumAdvise, EnumData (cache)
                                    padvSink    : Pointer {IAdviseSink};  // EnumAdvise
                                    dwConnection: DWord;        // EnumAdvise
                                 End;
  STATDATA                     = TagStatData;


  TagSTGMEDIUM                 = Record
                                    Tymed : DWord;
                                    Case Integer Of
                                      0 : (HBITMAP             : hBitmap;       PUnkForRelease :  Pointer {IUnknown});
                                      1 : (HMETAFILEPICT       : hMetaFilePict );
                                      2 : (HENHMETAFILE        : hEnhMetaFile  );
                                      3 : (HGLOBAL             : hGlobal       );
                                      4 : (lpszFileName        : LPOLESTR    );
                                      5 : (pstm                : Pointer{IStream}  );
                                      6 : (pstg                : Pointer{IStorage} );
                                      End;
  USTGMEDIUM                   = TagSTGMEDIUM;
  STGMEDIUM                    = USTGMEDIUM;
  TStgMedium                                                                           = TagSTGMEDIUM;
  PStgMedium                   = ^TStgMedium;
  LPSTGMEDIUM                  = ^STGMEDIUM;

  IEnumString = Interface (IUnknown)
       ['{00000101-0000-0000-C000-000000000046}']
       Function Next(Celt:ULong;Out xcelt;Out Celtfetched:ULong):HResult; StdCall;
//     Function RemoteNext(Celt:ULong; Out celt;Out Celtfetched:ULong):HResult; StdCall;
       Function Skip (Celt:ULong):Hresult;StdCall;
       Function Reset:HResult;StdCall;
       Function Clone(Out penum:IEnumString):HResult;StdCall;
       End;


    IEnumMoniker = Interface (IUnknown)
       ['{00000102-0000-0000-C000-000000000046}']
       Function Next(celt:ULong; out Elt;out celftfetched: ULong):HResult; StdCall;
//     Function RemoteNext(Celt:ULong; Out rgelt;out celtfetched :ULong):Hresult; StdCall;
       Function Skip(celt:Ulong):HResult; StdCall;
       Function Reset:HResult; StdCall;
       Function Close(out penum:IEnumMoniker):HResult;StdCall;
       End;

   IEnumSTATDATA = Interface (IUnknown)
    ['{00000105-0000-0000-C000-000000000046}']
    Function Next (Celt:ULong;Out xcelt;pceltfetched : PUlong):HResult; StdCall;
//      Function RemoteNext(Celt:ULong;Out Rgelt:statdata;Out pceltFetched:ULong):HResult; StdCall;
    Function Skip(Celt:ULong):HResult;StdCall;
    Function Reset:HResult;StdCall;
    Function Clone(out penum:IEnumstatdata):HResult;StdCall;
    End;

   IEnumFORMATETC = Interface (IUnknown)
   ['{00000103-0000-0000-C000-000000000046}']
   Function Next(Celt:ULong;Out Rgelt:FormatEtc;pceltFetched:pULong=nil):HResult; StdCall;
//     Function RemoteNext(Celt:ULong;Out Rgelt:FormatEtc;Out pceltFetched:ULong):HResult; StdCall;
   Function Skip(Celt:ULong):HResult;StdCall;
   Function Reset:HResult;StdCall;
   Function Clone(out penum:IEnumFORMATETC):HResult;StdCall;
   End;



    IPersist = Interface (IUnknown)
       ['{0000010c-0000-0000-C000-000000000046}']
       Function GetClassId(clsid:TClsId):HResult; StdCall;
       End;

    IPersistStream = Interface(IPersist)
       ['{00000109-0000-0000-C000-000000000046}']
       Function IsDirty:HResult; StdCall;
       Function Load(Const stm: IStream):HResult; StdCall;
       Function Save(Const stm: IStream;fClearDirty:Bool):HResult;StdCall;
       Function GetSizeMax(Out cbSize:ULarge_Integer):HResult; StdCall;
       End;


    IRunningObjectTable = Interface (IUnknown)
       ['{00000010-0000-0000-C000-000000000046}']
       Function Register  (grfFlags :DWord;const unkobject:IUnknown;Const mkObjectName:IMoniker;Out dwregister:DWord):HResult;StdCall;
       Function Revoke    (dwRegister:DWord):HResult; StdCall;
       Function IsRunning (Const mkObjectName: IMoniker):HResult;StdCall;
       Function GetObject (Const mkObjectName: IMoniker; Out punkObject:IUnknown):HResult; StdCall;
       Function NoteChangeTime(dwRegister :DWord;Const FileTime: TFileTime):HResult;StdCall;
       Function GetTimeOfLastChange(Const mkObjectName:IMoniker;Out filetime:TFileTime):HResult; StdCall;
       Function EnumRunning (Out enumMoniker: IEnumMoniker):HResult; StdCall;
       End;


    IBindCtx = Interface (IUnknown)
       ['{0000000e-0000-0000-C000-000000000046}']
       Function RegisterObjectBound(Const punk:IUnknown):HResult; stdCall;
       Function RevokeObjectBound (Const Punk:IUnknown):HResult;  stdCall;
       Function ReleaseBoundObjects :HResult;  StdCall;
       Function SetBindOptions(Const bindOpts:TBind_Opts):HResult;  stdCall;
//       Function RemoteSetBindOptions(Const bind_opts: TBind_Opts2):HResult;StdCall;
       Function GetBindOptions(var BindOpts:TBind_Opts):HResult;  stdCall;
//       Function RemoteGetBindOptions(Var bind_opts: TBind_Opts2):HResult;StdCall;
       Function GetRunningObjectTable(Out rot : IRunningObjectTable):Hresult; StdCall;
       Function RegisterObjectParam(Const pszkey:LPOleStr;const punk:IUnknown):HResult;
       Function GetObjectParam(Const pszkey:LPOleStr; out punk: IUnknown):HResult; StdCall;
       Function EnumObjectParam (out enum:IEnumString):Hresult;StdCall;
       Function RevokeObjectParam(pszKey:LPOleStr):HResult;StdCall;
       End;


    PIMoniker = ^IMoniker;
    IMoniker = Interface (IPersistStream)
      ['{0000000f-0000-0000-C000-000000000046}']
      Function BindToObject (const pbc:IBindCtx;const mktoleft:IMoniker; RiidResult:TIID;Out vresult):HResult;StdCall;
//    Function RemoteBindToObject (const pbc:IBindCtx;const mktoleft:IMoniker;RiidResult:TIID;Out vresult):HResult;StdCall;
      Function BindToStorage(Const Pbc:IBindCtx;Const mktoLeft:IMoniker; Riid:TIID;Out vobj):HResult; StdCall;
//    Function RemoteBindToStorage(Const Pbc:IBindCtx;Const mktoLeft:IMoniker; Riid:TIID;Out vobj):HResult; StdCall;
      Function Reduce (const pbc:IBindCtx; dwReduceHowFar:DWord; mktoLeft: PIMoniker; Out mkReduced:IMoniker):HResult; StdCall;
      Function ComposeWith(Const MkRight:IMoniker;fOnlyIfNotGeneric:BOOL; OUT mkComposite:IMoniker):HResult; StdCall;
      Function Enum(fForward:Bool;Out enumMoniker:IEnumMoniker):HResult;StdCall;
      Function IsEqual(Const mkOtherMoniker:IMoniker):HResult;StdCall;
      Function Hash   (Out dwHash:Dword):HResult;StdCall;
      Function IsRunning(Const bc:IBindCtx;Const MkToLeft:IMoniker;Const mknewlyRunning:IMoniker):HResult;StdCall;
      Function GetTimeOfLastChange(Const bc:IBindCtx;Const mkToLeft:IMoniker; out ft : FileTime):HResult; StdCall;
      Function Inverse(out mk : IMoniker):HResult; StdCall;
      Function CommonPrefixWith (Const mkOther:IMoniker):HResult; StdCall;
      Function RelativePathTo(Const mkother:IMoniker; Out mkRelPath : IMoniker):HResult;StdCall;
      Function GetDisplayName(Const bc:IMoniker;const mktoleft:IMoniker;Out szDisplayName: pOleStr):HResult; StdCall;
      Function ParseDisplayName(Const bc:IBindCtx;Const mkToLeft:IMoniker;szDisplayName:POleStr;out cheaten:ULong;out mkOut:IMoniker):HResult; StdCall;
      Function IsSystemMonitor(Out dwMkSys:DWord):HResult;StdCall;
      End;


    IAdviseSink = Interface (IUnknown)
        ['{0000010f-0000-0000-C000-000000000046}']
    {$ifdef midl500} ['{00000150-0000-0000-C000-000000000046}'] {$endif}
        Procedure OnDataChange (Const pformatetc : Formatetc;const pstgmed : STGMEDIUM); StdCall;
        Procedure OnViewChange (dwAspect : DWord; lindex : Long); StdCall;
        Procedure OnRename (Const pmk : IMoniker); StdCall;
        Procedure OnSave; StdCall;
        Procedure OnClose; StdCall;
     End;


  //Fake interfaces
  IDataObject = Interface (IUnknown)
   ['{0000010e-0000-0000-C000-000000000046}']
   Function GetData(Const formatetcIn : FORMATETC;Out medium : STGMEDIUM):HRESULT; STDCALL;
   Function GetDataHere(CONST pformatetc : FormatETC; Out medium : STGMEDIUM):HRESULT; STDCALL;
   Function QueryGetData(const pformatetc : FORMATETC):HRESULT; STDCALL;
   Function GetCanonicalFormatEtc(const pformatetcIn : FORMATETC;Out pformatetcOut : FORMATETC):HResult; STDCALl;
   Function SetData (Const pformatetc : FORMATETC;const medium:STGMEDIUM;FRelease : BOOL):HRESULT; StdCall;
   Function EnumFormatEtc(dwDirection : DWord; OUT enumformatetcpara : IENUMFORMATETC):HRESULT; StdCall;
   Function DAdvise(const formatetc : FORMATETC;advf :DWORD; CONST AdvSink : IAdviseSink;OUT dwConnection:DWORD):HRESULT;StdCall;
   Function DUnadvise(dwconnection :DWord) :HRESULT;StdCall;
   Function EnumDAdvise(Out enumAdvise : IEnumStatData):HResult;StdCall;
   End;

 IDropTarget = interface(IUnknown)
    ['{00000122-0000-0000-C000-000000000046}']
    function DragEnter(const dataObj: IDataObject; grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD): HResult;StdCall;
    function DragOver(grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD): HResult;StdCall;
    function DragLeave: HResult;StdCall;
    function Drop(const dataObj: IDataObject; grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD):HResult;StdCall;
  end;


  IDropSource = interface(IUnknown)
    ['{00000121-0000-0000-C000-000000000046}']
    function QueryContinueDrag(fEscapePressed: BOOL;
      grfKeyState: DWORD):HResult;StdCall;
    function GiveFeedback(dwEffect: DWORD): HResult;StdCall;
  end;


  IDataAdviseHolder = Interface (IUnknown)
       ['{00000110-0000-0000-C000-000000000046}']
       Function Advise    (CONST pdataObject : IDataObject;CONST fetc:FORMATETC;advf : DWORD;Const pAdvise:IAdviseSink;Out DwConnection:DWord):HResult; StdCall;
       Function Unadvise  (dwConnection:Dword):HResult; StdCall;
       Function EnumAdvise(out penumAdvise : IEnumStatData):HResult;StdCall;
       Function SendOnDataChange(const pDataObject :IDataObject;DwReserved,advf : DWord):HResult; StdCall;
       End;



//Ole helper functions

  function Succeeded(Status : HRESULT) : BOOLEAN;

  function Failed(Status : HRESULT) : BOOLEAN;

  //ActiveX functions that have wrong calling convention in fpc

  function RegisterDragDrop(hwnd:HWND; pDropTarget:IDropTarget):WINOLEAPI;stdcall;

  function RevokeDragDrop(hwnd:HWND):WINOLEAPI;stdcall;

  function DoDragDrop(pDataObj:IDataObject; pDropSource:IDropSource; dwOKEffects:DWORD; pdwEffect:LPDWORD):WINOLEAPI;

  function OleInitialize(pvReserved:LPVOID):WINOLEAPI;stdcall;

  procedure OleUninitialize;stdcall;

  procedure ReleaseStgMedium(_para1:LPSTGMEDIUM);stdcall;

  function OleSetClipboard(pDataObj:IDataObject):WINOLEAPI;stdcall;

  function OleGetClipboard(out ppDataObj:IDataObject):WINOLEAPI;stdcall;

  function OleFlushClipboard:WINOLEAPI;stdcall;

  function OleIsCurrentClipboard(pDataObj:IDataObject):WINOLEAPI;stdcall;

  function CreateStreamOnHGlobal(hGlobal:HGLOBAL; fDeleteOnRelease:BOOL;out stm:IStream):WINOLEAPI;stdcall;

  function CoCreateInstance(const _para1:TCLSID; _para2:IUnknown; _para3:DWORD;const _para4:TIID;out _para5):HRESULT;stdcall;

implementation


function Succeeded(Status : HRESULT) : BOOLEAN;
  begin
     Succeeded:=Status and HRESULT($80000000)=0;
  end;

function Failed(Status : HRESULT) : BOOLEAN;
  begin
     Failed:=Status and HRESULT($80000000)<>0;
  end;
  
function RegisterDragDrop(hwnd: HWND; pDropTarget: IDropTarget): WINOLEAPI;
begin
  //Logger.SendError([lcOle],'Ole function called in Linux');
  //Logger.SendCallStack([lcOle],'Stack');
end;

function RevokeDragDrop(hwnd: HWND): WINOLEAPI;
begin
  //Logger.SendError([lcOle],'Ole function called in Linux');
  //Logger.SendCallStack([lcOle],'Stack');
end;

function DoDragDrop(pDataObj: IDataObject; pDropSource: IDropSource;
  dwOKEffects: DWORD; pdwEffect: LPDWORD): WINOLEAPI;
begin
  //Logger.SendError([lcOle],'Ole function called in Linux');
  //Logger.SendCallStack([lcOle],'Stack');
end;

function OleInitialize(pvReserved: LPVOID): WINOLEAPI;
begin
  //Logger.SendError([lcOle],'Ole function called in Linux');
  //Logger.SendCallStack([lcOle],'Stack');
end;

procedure OleUninitialize;
begin
  //Logger.SendError([lcOle],'Ole function called in Linux');
  //Logger.SendCallStack([lcOle],'Stack');
end;

procedure ReleaseStgMedium(_para1: LPSTGMEDIUM);
begin
  //Logger.SendError([lcOle],'Ole function called in Linux');
  //Logger.SendCallStack([lcOle],'Stack');
end;

function OleSetClipboard(pDataObj: IDataObject): WINOLEAPI;
begin
  //Logger.SendError([lcOle],'Ole function called in Linux');
  //Logger.SendCallStack([lcOle],'Stack');
end;

function OleGetClipboard(out ppDataObj: IDataObject): WINOLEAPI;
begin
  //Logger.SendError([lcOle],'Ole function called in Linux');
  //Logger.SendCallStack([lcOle],'Stack');
end;

function OleFlushClipboard: WINOLEAPI;
begin
 // Logger.SendError([lcOle],'Ole function called in Linux');
  //Logger.SendCallStack([lcOle],'Stack');
end;

function OleIsCurrentClipboard(pDataObj: IDataObject): WINOLEAPI;
begin
  //Logger.SendError([lcOle],'Ole function called in Linux');
  //Logger.SendCallStack([lcOle],'Stack');
end;

function CreateStreamOnHGlobal(hGlobal: HGLOBAL; fDeleteOnRelease: BOOL; out
  stm: IStream): WINOLEAPI;
begin
  //Logger.SendError([lcOle],'Ole function called in Linux');
  //Logger.SendCallStack([lcOle],'Stack');
end;

function CoCreateInstance(const _para1: TCLSID; _para2: IUnknown;
  _para3: DWORD; const _para4: TIID; out _para5): HRESULT;
begin
  //Logger.SendError([lcOle],'Ole function called in Linux');
  //Logger.SendCallStack([lcOle],'Stack');
end;
  

end.
