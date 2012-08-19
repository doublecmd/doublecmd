unit virtualdragmanager;
{fake unit just to compile - not used under non windows}

{$mode delphi}

interface

uses
  Classes, SysUtils, Types;

const
  // Drag image helpers for Windows 2000 and up.
  IID_IDropTargetHelper: TGUID = (D1: $4657278B; D2: $411B; D3: $11D2; D4: ($83, $9A, $00, $C0, $4F, $D9, $18, $D0));
  IID_IDragSourceHelper: TGUID = (D1: $DE5BF786; D2: $477A; D3: $11D2; D4: ($83, $9D, $00, $C0, $4F, $D9, $18, $D0));
  IID_IDropTarget: TGUID = (D1: $00000122; D2: $0000; D3: $0000; D4: ($C0, $00, $00, $00, $00, $00, $00, $46));
  CLSID_DragDropHelper: TGUID = (D1: $4657278A; D2: $411B; D3: $11D2; D4: ($83, $9A, $00, $C0, $4F, $D9, $18, $D0));

  SID_IDropTargetHelper = '{4657278B-411B-11D2-839A-00C04FD918D0}';
  SID_IDragSourceHelper = '{DE5BF786-477A-11D2-839D-00C04FD918D0}';
  SID_IDropTarget = '{00000122-0000-0000-C000-000000000046}';
  
  //Bridge to ActiveX constants
  
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
  WinBool= LongBool;
  Bool= WinBool;
  ULONG  = cardinal;
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
    Function Next(Celt:ULong;Out Rgelt:statdata;Out pceltFetched:ULong):HResult; StdCall;
//      Function RemoteNext(Celt:ULong;Out Rgelt:statdata;Out pceltFetched:ULong):HResult; StdCall;
    Function Skip(Celt:ULong):HResult;StdCall;
    Function Reset:HResult;StdCall;
    Function Clone(out penum:IEnumstatdata):HResult;StdCall;
    End;

   IEnumFORMATETC = Interface (IUnknown)
   ['{00000103-0000-0000-C000-000000000046}']
   Function Next(Celt:ULong;Out Rgelt:FormatEtc;Out pceltFetched:ULong):HResult; StdCall;
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
   Function GetCanonicalFormatTEtc(const pformatetcIn : FORMATETC;Out pformatetcOut : FORMATETC):HResult; STDCALl;
   Function SetData (Const pformatetc : FORMATETC;const medium:STGMEDIUM;FRelease : BOOL):HRESULT; StdCall;
   Function EnumFormatEtc(dwDirection : DWord; OUT enumformatetcpara : IENUMFORMATETC):HRESULT; StdCall;
   Function DAdvise(const formatetc : FORMATETC;advf :DWORD; CONST AdvSink : IAdviseSink;OUT dwConnection:DWORD):HRESULT;StdCall;
   Function DUnadvise(dwconnection :DWord) :HRESULT;StdCall;
   Function EnumDAvise(Out enumAdvise : IEnumStatData):HResult;StdCall;
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
      grfKeyState: LongWord):HResult;StdCall;
    function GiveFeedback(dwEffect: LongWord): HResult;StdCall;
  end;
  
  
  IDataAdviseHolder = Interface (IUnknown)
       ['{00000110-0000-0000-C000-000000000046}']
       Function Advise    (CONST pdataObject : IDataObject;CONST fetc:FORMATETC;advf : DWORD;Const pAdvise:IAdviseSink;Out DwConnection:DWord):HResult; StdCall;
       Function Unadvise  (dwConnection:Dword):HResult; StdCall;
       Function EnumAdvise(out penumAdvise : IEnumStatData):HResult;StdCall;
       Function SendOnDataChange(const pDataObject :IDataObject;DwReserved,advf : DWord):HResult; StdCall;
       End;

  

  // OLE drag'n drop support
  TFormatEtcArray = array of TFormatEtc;
  TFormatArray = array of Word;

  // IDataObject.SetData support
  TInternalStgMedium = packed record
    Format: TClipFormat;
    Medium: TStgMedium;
  end;
  TInternalStgMediumArray = array of TInternalStgMedium;

  TEnumFormatEtc = class(TInterfacedObject, IEnumFormatEtc)
  private
    FTree: TObject;
    FFormatEtcArray: TFormatEtcArray;
    FCurrentIndex: Integer;
  public
    constructor Create(Tree: TObject; AFormatEtcArray: TFormatEtcArray);
    function Clone(out Enum: IEnumFormatEtc): HResult; stdcall;
    function Next(celt: LongWord; out elt: FormatEtc; out pceltFetched: LongWord): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Skip(celt: LongWord): HResult; stdcall;
  end;

  IDropTargetHelper = interface(IUnknown)
    [SID_IDropTargetHelper]
    function DragEnter(hwndTarget: HWND; pDataObject: IDataObject; var ppt: TPoint; dwEffect: LongWord): HRESULT; stdcall;
    function DragLeave: HRESULT; stdcall;
    function DragOver(var ppt: TPoint; dwEffect: LongWord): HRESULT; stdcall;
    function Drop(pDataObject: IDataObject; var ppt: TPoint; dwEffect: LongWord): HRESULT; stdcall;
    function Show(fShow: Boolean): HRESULT; stdcall;
  end;

  PSHDragImage = ^TSHDragImage;
  TSHDragImage = packed record
    sizeDragImage: TSize;
    ptOffset: TPoint;
    hbmpDragImage: HBITMAP;
    ColorRef: TColorRef;
  end;

  IDragSourceHelper = interface(IUnknown)
    [SID_IDragSourceHelper]
    function InitializeFromBitmap(var SHDragImage: TSHDragImage; pDataObject: IDataObject): HRESULT; stdcall;
    function InitializeFromWindow(Window: HWND; var ppt: TPoint; pDataObject: IDataObject): HRESULT; stdcall;
  end;



  IVTDragManager = interface(IUnknown)
    ['{C4B25559-14DA-446B-8901-0C879000EB16}']
    procedure ForceDragLeave; stdcall;
    function GetDataObject: IDataObject; stdcall;
    function GetDragSource: TObject; stdcall;
    function GetDropTargetHelperSupported: Boolean; stdcall;
    function GetIsDropTarget: Boolean; stdcall;

    property DataObject: IDataObject read GetDataObject;
    property DragSource: TObject read GetDragSource;
    property DropTargetHelperSupported: Boolean read GetDropTargetHelperSupported;
    property IsDropTarget: Boolean read GetIsDropTarget;
  end;

  // This data object is used in two different places. One is for clipboard operations and the other while dragging.
  TVTDataObject = class(TInterfacedObject, IDataObject)
  private
    //FOwner: TBaseVirtualTree;          // The tree which provides clipboard or drag data.
    FOwner: TObject;          // The tree which provides clipboard or drag data.
    FForClipboard: Boolean;            // Determines which data to render with GetData.
    FFormatEtcArray: TFormatEtcArray;
    FInternalStgMediumArray: TInternalStgMediumArray;  // The available formats in the DataObject
    FAdviseHolder: IDataAdviseHolder;  // Reference to an OLE supplied implementation for advising.
  protected
    function CanonicalIUnknown(TestUnknown: IUnknown): IUnknown;
    function EqualFormatEtc(FormatEtc1, FormatEtc2: TFormatEtc): Boolean;
    function FindFormatEtc(TestFormatEtc: TFormatEtc; const FormatEtcArray: TFormatEtcArray): integer;
    function FindInternalStgMedium(Format: TClipFormat): PStgMedium;
    function HGlobalClone(HGlobal: THandle): THandle;
    function RenderInternalOLEData(const FormatEtcIn: TFormatEtc; var Medium: TStgMedium; var OLEResult: HResult): Boolean;
    function StgMediumIncRef(const InStgMedium: TStgMedium; var OutStgMedium: TStgMedium;
      CopyInMedium: Boolean; DataObject: IDataObject): HRESULT;

    property ForClipboard: Boolean read FForClipboard;
    property FormatEtcArray: TFormatEtcArray read FFormatEtcArray write FFormatEtcArray;
    property InternalStgMediumArray: TInternalStgMediumArray read FInternalStgMediumArray write FInternalStgMediumArray;
    property Owner: TObject read FOwner;
  public
    constructor Create(AOwner: TObject; ForClipboard: Boolean); virtual;
    destructor Destroy; override;

    function DAdvise(const FormatEtc: TFormatEtc; advf: DWord; const advSink: IAdviseSink; out dwConnection: DWord):
      HResult; virtual; stdcall;
    function DUnadvise(dwConnection: DWord): HResult; virtual; stdcall;
    Function EnumDAvise(Out enumAdvise : IEnumStatData):HResult;virtual;StdCall;
    function EnumFormatEtc(Direction: DWord; out EnumFormatEtc: IEnumFormatEtc): HResult; virtual; stdcall;
    Function GetCanonicalFormatTEtc(const pformatetcIn : FORMATETC;Out pformatetcOut : FORMATETC):HResult; virtual; STDCALl;
    function GetData(const FormatEtcIn: TFormatEtc; out Medium: TStgMedium): HResult; virtual; stdcall;
    function GetDataHere(const FormatEtc: TFormatEtc; out Medium: TStgMedium): HResult; virtual; stdcall;
    function QueryGetData(const FormatEtc: TFormatEtc): HResult; virtual; stdcall;
    function SetData(const FormatEtc: TFormatEtc;{$ifdef VER2_0}var{$else}const{$endif} Medium: TStgMedium; DoRelease: BOOL): HResult; virtual; stdcall;
  end;

  // TVTDragManager is a class to manage drag and drop in a Virtual Treeview.
  TVTDragManager = class(TInterfacedObject, IVTDragManager, IDropSource, IDropTarget)
  private
    FOwner,                            // The tree which is responsible for drag management.
    FDragSource: TObject;     // Reference to the source tree if the source was a VT, might be different than
                                       // the owner tree.
    FIsDropTarget: Boolean;            // True if the owner is currently the drop target.
    FDataObject: IDataObject;          // A reference to the data object passed in by DragEnter (only used when the owner
                                       // tree is the current drop target).
    FDropTargetHelper: IDropTargetHelper; // Win2k > Drag image support
    FFullDragging: BOOL;               // True, if full dragging is currently enabled in the system.

    function GetDataObject: IDataObject; stdcall;
    function GetDragSource: TObject; stdcall;
    function GetDropTargetHelperSupported: Boolean; stdcall;
    function GetIsDropTarget: Boolean; stdcall;
  public
    constructor Create(AOwner: TObject); virtual;
    destructor Destroy; override;

    function DragEnter(const DataObject: IDataObject; KeyState: LongWord; Pt: TPoint;
      var Effect: LongWord): HResult; stdcall;
    function DragLeave: HResult; stdcall;
    function DragOver(KeyState: LongWord; Pt: TPoint; var Effect: LongWord): HResult; stdcall;
    function Drop(const DataObject: IDataObject; KeyState: LongWord; Pt: TPoint; var Effect: LongWord): HResult; stdcall;
    procedure ForceDragLeave; stdcall;
    function GiveFeedback(Effect: LongWord): HResult; stdcall;
    function QueryContinueDrag(EscapePressed: BOOL; KeyState: LongWord): HResult; stdcall;
  end;
  
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

  //helper functions to isolate windows/OLE specific code
  
  function RenderOLEData(Tree: TObject; const FormatEtcIn: TFormatEtc; out Medium: TStgMedium;
    ForClipboard: Boolean): HResult;
    
  function GetStreamFromMedium(Medium:TStgMedium):TStream;
  
  procedure UnlockMediumData(Medium:TStgMedium);
  
  function GetTreeFromDataObject(const DataObject: IDataObject; var Format: TFormatEtc): TObject;
  
  function AllocateGlobal(Data: Pointer; DataSize:Cardinal): HGLOBAL;

implementation

uses
  VirtualTrees, Controls {$ifdef DEBUG_VTV}, vtlogger{$endif};
  
type
  TVirtualTreeAccess = class (TBaseVirtualTree)
  end;

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
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
end;

function RevokeDragDrop(hwnd: HWND): WINOLEAPI;
begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
end;

function DoDragDrop(pDataObj: IDataObject; pDropSource: IDropSource;
  dwOKEffects: DWORD; pdwEffect: LPDWORD): WINOLEAPI;
begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
end;

function OleInitialize(pvReserved: LPVOID): WINOLEAPI;
begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
end;

procedure OleUninitialize;
begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
end;

procedure ReleaseStgMedium(_para1: LPSTGMEDIUM);
begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
end;

function OleSetClipboard(pDataObj: IDataObject): WINOLEAPI;
begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
end;

function OleGetClipboard(out ppDataObj: IDataObject): WINOLEAPI;
begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
end;

function OleFlushClipboard: WINOLEAPI;
begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
end;

function OleIsCurrentClipboard(pDataObj: IDataObject): WINOLEAPI;
begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
end;

function CreateStreamOnHGlobal(hGlobal: HGLOBAL; fDeleteOnRelease: BOOL; out
  stm: IStream): WINOLEAPI;
begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
end;

function CoCreateInstance(const _para1: TCLSID; _para2: IUnknown;
  _para3: DWORD; const _para4: TIID; out _para5): HRESULT;
begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
end;


function RenderOLEData(Tree: TObject; const FormatEtcIn: TFormatEtc; out
  Medium: TStgMedium; ForClipboard: Boolean): HResult;
{
  //--------------- local function --------------------------------------------

  procedure WriteNodes(Stream: TStream);

  var
    Selection: TNodeArray;
    I: Integer;

  begin
    with TVirtualTreeAccess(Tree) do
    begin
      if ForClipboard then
        Selection := GetSortedCutCopySet(True)
      else
        Selection := GetSortedSelection(True);
      for I := 0 to High(Selection) do
        WriteNode(Stream, Selection[I]);
    end;
  end;

  //--------------- end local function ----------------------------------------
}
var
  Data: PCardinal;
  ResPointer: Pointer;
  ResSize: Integer;
  OLEStream: IStream;
  VCLStream: TStream;
  
begin
 {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
  {
  VCLStream := nil;
  try
    Medium.PunkForRelease := nil;
    // Return data in one of the supported storage formats, prefer IStream.
    if FormatEtcIn.tymed and TYMED_ISTREAM <> 0 then
    begin
      // Create an IStream on a memory handle (here it is 0 which indicates to implicitely allocated a handle).
      // Do not use TStreamAdapter as it is not compatible with OLE (when flushing the clipboard OLE wants the HGlobal
      // back which is not supported by TStreamAdapater).
      CreateStreamOnHGlobal(0, True, OLEStream);

      VCLStream := TOLEStream.Create(OLEStream);
      WriteNodes(VCLStream);
      // Rewind stream.
      VCLStream.Position := 0;
      Medium.tymed := TYMED_ISTREAM;
      IUnknown(Medium.Pstm) := OLEStream;
      Result := S_OK;
    end
    else
    begin
      VCLStream := TMemoryStream.Create;
      WriteNodes(VCLStream);
      ResPointer := TMemoryStream(VCLStream).Memory;
      ResSize := VCLStream.Position;

      // Allocate memory to hold the string.
      if ResSize > 0 then
      begin
        Medium.hGlobal := GlobalAlloc(GHND or GMEM_SHARE, ResSize + SizeOf(Cardinal));
        Data := GlobalLock(Medium.hGlobal);
        // Store the size of the data too, for easy retrival.
        Data^ := ResSize;
        Inc(Data);
        Move(ResPointer^, Data^, ResSize);
        GlobalUnlock(Medium.hGlobal);
        Medium.tymed := TYMED_HGLOBAL;

        Result := S_OK;
      end
      else
        Result := E_FAIL;
    end;
  finally
    // We can free the VCL stream here since it was either a pure memory stream or only a wrapper around
    // the OLEStream which exists independently.
    VCLStream.Free;
  end;
  }
end;


type
  // needed to handle OLE global memory objects
  TOLEMemoryStream = class(TCustomMemoryStream)
  public
    function Write(const Buffer; Count: Integer): Longint; override;
  end;

//----------------------------------------------------------------------------------------------------------------------

function TOLEMemoryStream.Write(const Buffer; Count: Integer): Integer;

begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
   // raise EStreamError.CreateRes(PResStringRec(@SCantWriteResourceStreamError));
end;


function GetStreamFromMedium(Medium: TStgMedium): TStream;

var
  Data: Pointer;
  I: Integer;
begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
{
  Result := nil;
  if Medium.tymed = TYMED_ISTREAM then
    Result := TOLEStream.Create(IUnknown(Medium.Pstm) as IStream)
  else
  begin
    Data := GlobalLock(Medium.hGlobal);
    if Assigned(Data) then
    begin
      // Get the total size of data to retrieve.
      I := PCardinal(Data)^;
      Inc(PCardinal(Data));
      Result := TOLEMemoryStream.Create;
      TOLEMemoryStream(Result).SetPointer(Data, I);
    end;
  end;
}
end;

procedure UnlockMediumData(Medium: TStgMedium);
begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
{
  if Medium.tymed = TYMED_HGLOBAL then
    GlobalUnlock(Medium.hGlobal);
 }
end;

function GetTreeFromDataObject(const DataObject: IDataObject;
  var Format: TFormatEtc): TObject;
  
var
  Medium: TStgMedium;
  Data: PVTReference;
  
begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
  {
  Result := nil;
  if Assigned(DataObject) then
  begin
    Format.cfFormat := CF_VTREFERENCE;
    if DataObject.GetData(Format, Medium) = S_OK then
    begin
      Data := GlobalLock(Medium.hGlobal);
      if Assigned(Data) then
      begin
        if Data.Process = GetCurrentProcessID then
          Result := Data.Tree;
        GlobalUnlock(Medium.hGlobal);
      end;
      ReleaseStgMedium(@Medium);
    end;
  end;
  }
end;

function AllocateGlobal(Data: Pointer; DataSize: Cardinal): HGLOBAL;
var
  P:Pointer;
begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
  {
  Result := GlobalAlloc(GHND or GMEM_SHARE, DataSize);
  P := GlobalLock(Result);
  Move(Data^, P^, DataSize);
  GlobalUnlock(Result);
  }
end;

//----------------------------------------------------------------------------------------------------------------------

// OLE drag and drop support classes
// This is quite heavy stuff (compared with the VCL implementation) but is much better suited to fit the needs
// of DD'ing various kinds of virtual data and works also between applications.

//----------------- TEnumFormatEtc -------------------------------------------------------------------------------------

constructor TEnumFormatEtc.Create(Tree: TObject; AFormatEtcArray: TFormatEtcArray);

var
  I: Integer;

begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
  {
  inherited Create;

  FTree := Tree;
  // Make a local copy of the format data.
  SetLength(FFormatEtcArray, Length(AFormatEtcArray));
  for I := 0 to High(AFormatEtcArray) do
    FFormatEtcArray[I] := AFormatEtcArray[I];
  }
end;

//----------------------------------------------------------------------------------------------------------------------

function TEnumFormatEtc.Clone(out Enum: IEnumFormatEtc): HResult;

var
  AClone: TEnumFormatEtc;

begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
  {
   Result := S_OK;
  try
    AClone := TEnumFormatEtc.Create(nil, FFormatEtcArray);
    AClone.FCurrentIndex := FCurrentIndex;
    Enum := AClone as IEnumFormatEtc;
  except
    Result := E_FAIL;
  end;
  }
end;

//----------------------------------------------------------------------------------------------------------------------

function TEnumFormatEtc.Next(celt: LongWord; out elt: FormatEtc; out pceltFetched: LongWord): HResult;

var
  CopyCount: LongWord;

begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
  {
   Result := S_FALSE;
  CopyCount := Length(FFormatEtcArray) - FCurrentIndex;
  if celt < CopyCount then
    CopyCount := celt;
  if CopyCount > 0 then
  begin
    Move(FFormatEtcArray[FCurrentIndex], elt, CopyCount * SizeOf(TFormatEtc));
    Inc(FCurrentIndex, CopyCount);
    Result := S_OK;
  end;
  //todo_lcl_check Delphi treats pceltFetched an PInteger. Implemented like in fpc.activex. What heappens with
  // a C Program call with a NULL in pCeltFetcjed??
  //Answer: Yes. Is necessary a check here
  if @pceltFetched <> nil then
    pceltFetched := CopyCount;
  }
end;

//----------------------------------------------------------------------------------------------------------------------

function TEnumFormatEtc.Reset: HResult;

begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
  {
  FCurrentIndex := 0;
  Result := S_OK;
  }
end;

//----------------------------------------------------------------------------------------------------------------------

function TEnumFormatEtc.Skip(celt: LongWord): HResult;

begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
  {
  if FCurrentIndex + celt < High(FFormatEtcArray) then
  begin
    Inc(FCurrentIndex, celt);
    Result := S_Ok;
  end
  else
    Result := S_FALSE;
  }
end;


//----------------- TVTDataObject --------------------------------------------------------------------------------------

constructor TVTDataObject.Create(AOwner: TObject; ForClipboard: Boolean);

begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
  {
  inherited Create;

  FOwner := AOwner;
  FForClipboard := ForClipboard;
  TVirtualTreeAccess(FOwner).GetNativeClipboardFormats(FFormatEtcArray);
  }
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TVTDataObject.Destroy;

var
  I: Integer;
  StgMedium: PStgMedium;

begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
  {
  // Cancel a pending clipboard operation if this data object was created for the clipboard and
  // is freed because something else is placed there.
  if FForClipboard and not (tsClipboardFlushing in TVirtualTreeAccess(FOwner).TreeStates) then
    TVirtualTreeAccess(FOwner).CancelCutOrCopy;

  // Release any internal clipboard formats
  for I := 0 to High(FormatEtcArray) do
  begin
    StgMedium := FindInternalStgMedium(FormatEtcArray[I].cfFormat);
    if Assigned(StgMedium) then
      ReleaseStgMedium(StgMedium);
  end;

  FormatEtcArray := nil;
  inherited;
  }
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDataObject.CanonicalIUnknown(TestUnknown: IUnknown): IUnknown;

// Uses COM object identity: An explicit call to the IUnknown::QueryInterface method, requesting the IUnknown
// interface, will always return the same pointer.

begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
  {
  if Assigned(TestUnknown) then
  begin
    if TestUnknown.QueryInterface(IUnknown, Result) = 0 then
      Result._Release // Don't actually need it just need the pointer value
    else
      Result := TestUnknown
  end
  else
    Result := TestUnknown
  }
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDataObject.EqualFormatEtc(FormatEtc1, FormatEtc2: TFormatEtc): Boolean;

begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
  {
  Result := (FormatEtc1.cfFormat = FormatEtc2.cfFormat) and (FormatEtc1.ptd = FormatEtc2.ptd) and
    (FormatEtc1.dwAspect = FormatEtc2.dwAspect) and (FormatEtc1.lindex = FormatEtc2.lindex) and
    (FormatEtc1.tymed and FormatEtc2.tymed <> 0);
    
  }
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDataObject.FindFormatEtc(TestFormatEtc: TFormatEtc; const FormatEtcArray: TFormatEtcArray): integer;

var
  I: integer;

begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
  {
  Result := -1;
  for I := 0 to High(FormatEtcArray) do
  begin
    if EqualFormatEtc(TestFormatEtc, FormatEtcArray[I]) then
    begin
      Result := I;
      Break;
    end
  end;
  }
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDataObject.FindInternalStgMedium(Format: TClipFormat): PStgMedium;

var
  I: integer;
begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
  {
  Result := nil;
  for I := 0 to High(InternalStgMediumArray) do
  begin
    if Format = InternalStgMediumArray[I].Format then
    begin
      Result := @InternalStgMediumArray[I].Medium;
      Break;
    end
  end;
  }
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDataObject.HGlobalClone(HGlobal: THandle): THandle;

// Returns a global memory block that is a copy of the passed memory block.

var
  Size: Cardinal;
  Data,
  NewData: PChar;

begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
  {
  Size := GlobalSize(HGlobal);
  Result := GlobalAlloc(GPTR, Size);
  Data := GlobalLock(hGlobal);
  try
    NewData := GlobalLock(Result);
    try
      Move(Data^, NewData^, Size);
    finally
      GlobalUnLock(Result);
    end
  finally
    GlobalUnLock(hGlobal);
  end;
  }
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDataObject.RenderInternalOLEData(const FormatEtcIn: TFormatEtc; var Medium: TStgMedium;
  var OLEResult: HResult): Boolean;

// Tries to render one of the formats which have been stored via the SetData method.
// Since this data is already there it is just copied or its reference count is increased (depending on storage medium).

var
  InternalMedium: PStgMedium;

begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
  {
  
  Result := True;
  InternalMedium := FindInternalStgMedium(FormatEtcIn.cfFormat);
  if Assigned(InternalMedium) then
    OLEResult := StgMediumIncRef(InternalMedium^, Medium, False, Self as IDataObject)
  else
    Result := False;
   }
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDataObject.StgMediumIncRef(const InStgMedium: TStgMedium; var OutStgMedium: TStgMedium;
  CopyInMedium: Boolean; DataObject: IDataObject): HRESULT;

// InStgMedium is the data that is requested, OutStgMedium is the data that we are to return either a copy of or
// increase the IDataObject's reference and send ourselves back as the data (unkForRelease). The InStgMedium is usually
// the result of a call to find a particular FormatEtc that has been stored locally through a call to SetData.
// If CopyInMedium is not true we already have a local copy of the data when the SetData function was called (during
// that call the CopyInMedium must be true). Then as the caller asks for the data through GetData we do not have to make
// copy of the data for the caller only to have them destroy it then need us to copy it again if necessary.
// This way we increase the reference count to ourselves and pass the STGMEDIUM structure initially stored in SetData.
// This way when the caller frees the structure it sees the unkForRelease is not nil and calls Release on the object
// instead of destroying the actual data.

var
  Len: Integer;

begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
  {
  Result := S_OK;

  // Simply copy all fields to start with.
  OutStgMedium := InStgMedium;
  // The data handled here always results from a call of SetData we got. This ensures only one storage format
  // is indicated and hence the case statement below is safe (IDataObject.GetData can optionally use several
  // storage formats).
  case InStgMedium.tymed of
    TYMED_HGLOBAL:
      begin
        if CopyInMedium then
        begin
          // Generate a unique copy of the data passed
          OutStgMedium.hGlobal := HGlobalClone(InStgMedium.hGlobal);
          if OutStgMedium.hGlobal = 0 then
            Result := E_OUTOFMEMORY
        end
        else
          // Don't generate a copy just use ourselves and the copy previously saved.
          OutStgMedium.PunkForRelease := Pointer(DataObject); // Does not increase RefCount.
      end;
    TYMED_FILE:
      begin
        //todo_lcl_check
        Len := Length(WideString(InStgMedium.lpszFileName)) + 1; // Don't forget the terminating null character.
        OutStgMedium.lpszFileName := CoTaskMemAlloc(2 * Len);
        Move(InStgMedium.lpszFileName^, OutStgMedium.lpszFileName^, 2 * Len);
      end;
    TYMED_ISTREAM:
      IUnknown(OutStgMedium.Pstm)._AddRef;
    TYMED_ISTORAGE:
      IUnknown(OutStgMedium.Pstg)._AddRef;
    TYMED_GDI:
      if not CopyInMedium then
        // Don't generate a copy just use ourselves and the previously saved data.
        OutStgMedium.PunkForRelease := Pointer(DataObject) // Does not increase RefCount.
      else
        Result := DV_E_TYMED; // Don't know how to copy GDI objects right now.
    TYMED_MFPICT:
      if not CopyInMedium then
        // Don't generate a copy just use ourselves and the previously saved data.
        OutStgMedium.PunkForRelease := Pointer(DataObject) // Does not increase RefCount.
      else
        Result := DV_E_TYMED; // Don't know how to copy MetaFile objects right now.
    TYMED_ENHMF:
      if not CopyInMedium then
        // Don't generate a copy just use ourselves and the previously saved data.
        OutStgMedium.PunkForRelease := Pointer(DataObject) // Does not increase RefCount.
      else
        Result := DV_E_TYMED; // Don't know how to copy enhanced metafiles objects right now.
  else
    Result := DV_E_TYMED;
  end;

  if (Result = S_OK) and Assigned(OutStgMedium.PunkForRelease) then
    IUnknown(OutStgMedium.PunkForRelease)._AddRef;
    }
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDataObject.DAdvise(const FormatEtc: TFormatEtc; advf: DWord; const advSink: IAdviseSink;
  out dwConnection: DWord): HResult;

// Advise sink management is greatly simplified by the IDataAdviseHolder interface.
// We use this interface and forward all concerning calls to it.

begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
  {
  Result := S_OK;
  if FAdviseHolder = nil then
    Result := CreateDataAdviseHolder(FAdviseHolder);
  if Result = S_OK then
    Result := FAdviseHolder.Advise(Self as IDataObject, FormatEtc, advf, advSink, dwConnection);
  }
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDataObject.DUnadvise(dwConnection: DWord): HResult;

begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
  {
  if FAdviseHolder = nil then
    Result := E_NOTIMPL
  else
    Result := FAdviseHolder.Unadvise(dwConnection);
  }
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDataObject.EnumDAvise(Out enumAdvise : IEnumStatData):HResult;

begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
  {
  if FAdviseHolder = nil then
    Result := OLE_E_ADVISENOTSUPPORTED
  else
    Result := FAdviseHolder.EnumAdvise(enumAdvise);
  }
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDataObject.EnumFormatEtc(Direction: DWord; out EnumFormatEtc: IEnumFormatEtc): HResult;

var
  NewList: TEnumFormatEtc;

begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
  {
  Result := E_FAIL;
  if Direction = DATADIR_GET then
  begin
    NewList := TEnumFormatEtc.Create(TVirtualTreeAccess(FOwner), FormatEtcArray);
    EnumFormatEtc := NewList as IEnumFormatEtc;
    Result := S_OK;
  end
  else
    EnumFormatEtc := nil;
  if EnumFormatEtc = nil then
    Result := OLE_S_USEREG;
  }
end;

//----------------------------------------------------------------------------------------------------------------------

Function TVTDataObject.GetCanonicalFormatTEtc(const pformatetcIn : FORMATETC;Out pformatetcOut : FORMATETC):HResult;

begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
  //Result := DATA_S_SAMEFORMATETC;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDataObject.GetData(const FormatEtcIn: TFormatEtc; out Medium: TStgMedium): HResult;

// Data is requested by clipboard or drop target. This method dispatchs the call
// depending on the data being requested.

var
  I: Integer;
  Data: PVTReference;

begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
  // The tree reference format is always supported and returned from here.
  {
  if FormatEtcIn.cfFormat = CF_VTREFERENCE then
  begin
    // Note: this format is not used while flushing the clipboard to avoid a dangling reference
    //       when the owner tree is destroyed before the clipboard data is replaced with something else.
    if tsClipboardFlushing in TVirtualTreeAccess(FOwner).TreeStates then
      Result := E_FAIL
    else
    begin
      Medium.hGlobal := GlobalAlloc(GHND or GMEM_SHARE, SizeOf(TVTReference));
      Data := GlobalLock(Medium.hGlobal);
      Data.Process := GetCurrentProcessID;
      Data.Tree := TBaseVirtualTree(FOwner);
      GlobalUnlock(Medium.hGlobal);
      Medium.tymed := TYMED_HGLOBAL;
      Medium.PunkForRelease := nil;
      Result := S_OK;
    end;
  end
  else
  begin
    try
      // See if we accept this type and if not get the correct return value.
      Result := QueryGetData(FormatEtcIn);
      if Result = S_OK then
      begin
        for I := 0 to High(FormatEtcArray) do
        begin
          if EqualFormatEtc(FormatEtcIn, FormatEtcArray[I]) then
          begin
            if not RenderInternalOLEData(FormatEtcIn, Medium, Result) then
              Result := TVirtualTreeAccess(FOwner).RenderOLEData(FormatEtcIn, Medium, FForClipboard);
            Break;
          end;
        end
      end
    except
      FillChar(Medium, SizeOf(Medium), #0);
      Result := E_FAIL;
    end;
  end;
  }
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDataObject.GetDataHere(const FormatEtc: TFormatEtc; out Medium: TStgMedium): HResult;

begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
  //Result := E_NOTIMPL;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDataObject.QueryGetData(const FormatEtc: TFormatEtc): HResult;

var
  I: Integer;

begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
  {
  Result := DV_E_CLIPFORMAT;
  for I := 0 to High(FFormatEtcArray) do
  begin
    if FormatEtc.cfFormat = FFormatEtcArray[I].cfFormat then
    begin
      if (FormatEtc.tymed and FFormatEtcArray[I].tymed) <> 0 then
      begin
        if FormatEtc.dwAspect = FFormatEtcArray[I].dwAspect then
        begin
          if FormatEtc.lindex = FFormatEtcArray[I].lindex then
          begin
            Result := S_OK;
            Break;
          end
          else
            Result := DV_E_LINDEX;
        end
        else
          Result := DV_E_DVASPECT;
      end
      else
        Result := DV_E_TYMED;
    end;
  end
  }
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDataObject.SetData(const FormatEtc: TFormatEtc;{$ifdef VER2_0}var{$else}const{$endif} Medium: TStgMedium; DoRelease: BOOL): HResult;

// Allows dynamic adding to the IDataObject during its existance. Most noteably it is used to implement
// IDropSourceHelper and allows to set a special format for optimized moves during a shell transfer.

var
  Index: Integer;
  LocalStgMedium: PStgMedium;

begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
  {
  // See if we already have a format of that type available.
  Index := FindFormatEtc(FormatEtc, FormatEtcArray);
  if Index > - 1 then
  begin
    // Just use the TFormatEct in the array after releasing the data.
    LocalStgMedium := FindInternalStgMedium(FormatEtcArray[Index].cfFormat);
    if Assigned(LocalStgMedium) then
    begin
      ReleaseStgMedium(LocalStgMedium);
      FillChar(LocalStgMedium^, SizeOf(LocalStgMedium^), #0);
    end;
  end
  else
  begin
    // It is a new format so create a new TFormatCollectionItem, copy the
    // FormatEtc parameter into the new object and and put it in the list.
    SetLength(FFormatEtcArray, Length(FormatEtcArray) + 1);
    FormatEtcArray[High(FormatEtcArray)] := FormatEtc;

    // Create a new InternalStgMedium and initialize it and associate it with the format.
    SetLength(FInternalStgMediumArray, Length(InternalStgMediumArray) + 1);
    InternalStgMediumArray[High(InternalStgMediumArray)].Format := FormatEtc.cfFormat;
    LocalStgMedium := @InternalStgMediumArray[High(InternalStgMediumArray)].Medium;
    FillChar(LocalStgMedium^, SizeOf(LocalStgMedium^), #0);
  end;

  if DoRelease then
  begin
    // We are simply being given the data and we take control of it.
    LocalStgMedium^ := Medium;
    Result := S_OK
  end
  else
  begin
    // We need to reference count or copy the data and keep our own references to it.
    Result := StgMediumIncRef(Medium, LocalStgMedium^, True, Self as IDataObject);

    // Can get a circular reference if the client calls GetData then calls SetData with the same StgMedium.
    // Because the unkForRelease for the IDataObject can be marshalled it is necessary to get pointers that
    // can be correctly compared. See the IDragSourceHelper article by Raymond Chen at MSDN.
    if Assigned(LocalStgMedium.PunkForRelease) then
    begin
      if CanonicalIUnknown(Self) = CanonicalIUnknown(IUnknown(LocalStgMedium.PunkForRelease)) then
        IUnknown(LocalStgMedium.PunkForRelease) := nil; // release the interface
    end;
  end;

  // Tell all registered advice sinks about the data change.
  if Assigned(FAdviseHolder) then
    FAdviseHolder.SendOnDataChange(Self as IDataObject, 0, 0);
  }
end;


//----------------- TVTDragManager -------------------------------------------------------------------------------------

constructor TVTDragManager.Create(AOwner: TObject);

begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
  {
  inherited Create;
  FOwner := AOwner;

  // Create an instance  of the drop target helper interface. This will fail but not harm on systems which do
  // not support this interface (everything below Windows 2000);
  CoCreateInstance(CLSID_DragDropHelper, nil, CLSCTX_INPROC_SERVER, IID_IDropTargetHelper, FDropTargetHelper);
  }
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TVTDragManager.Destroy;

begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
  {
  // Set the owner's reference to us to nil otherwise it will access an invalid pointer
  // after our desctruction is complete.
  TVirtualTreeAccess(FOwner).FreeDragManager;
  inherited;
  }
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDragManager.GetDataObject: IDataObject;

begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
  {
  // When the owner tree starts a drag operation then it gets a data object here to pass it to the OLE subsystem.
  // In this case there is no local reference to a data object and one is created (but not stored).
  // If there is a local reference then the owner tree is currently the drop target and the stored interface is
  // that of the drag initiator.
  if Assigned(FDataObject) then
    Result := FDataObject
  else
  begin
    Result := TVirtualTreeAccess(FOwner).DoCreateDataObject;
    if Result = nil then
      Result := TVTDataObject.Create(FOwner, False) as IDataObject;
  end;
  }
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDragManager.GetDragSource: TObject;

begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
  //Result := FDragSource;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDragManager.GetDropTargetHelperSupported: Boolean;

begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
  //Result := Assigned(FDropTargetHelper);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDragManager.GetIsDropTarget: Boolean;

begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
  //Result := FIsDropTarget;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDragManager.DragEnter(const DataObject: IDataObject; KeyState: LongWord; Pt: TPoint;
  var Effect: LongWord): HResult;

begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
  {
  FDataObject := DataObject;
  FIsDropTarget := True;

  SystemParametersInfo(SPI_GETDRAGFULLWINDOWS, 0, @FFullDragging, 0);
  // If full dragging of window contents is disabled in the system then our tree windows will be locked
  // and cannot be updated during a drag operation. With the following call painting is again enabled.
  if not FFullDragging then
    LockWindowUpdate(0);
  if Assigned(FDropTargetHelper) and FFullDragging then
    FDropTargetHelper.DragEnter(TBaseVirtualTree(FOwner).Handle, DataObject, Pt, Effect);

  FDragSource := TVirtualTreeAccess(FOwner).GetTreeFromDataObject(DataObject);
  Result := TVirtualTreeAccess(FOwner).DragEnter(KeyState, Pt, Effect);
  }
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDragManager.DragLeave: HResult;

begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
  {
  if Assigned(FDropTargetHelper) and FFullDragging then
    FDropTargetHelper.DragLeave;

  TVirtualTreeAccess(FOwner).DragLeave;
  FIsDropTarget := False;
  FDragSource := nil;
  FDataObject := nil;
  Result := NOERROR;
  }
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDragManager.DragOver(KeyState: LongWord; Pt: TPoint; var Effect: LongWord): HResult;

begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
  {
  if Assigned(FDropTargetHelper) and FFullDragging then
    FDropTargetHelper.DragOver(Pt, Effect);

  Result := TVirtualTreeAccess(FOwner).DragOver(FDragSource, KeyState, dsDragMove, Pt, Effect);
  }
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDragManager.Drop(const DataObject: IDataObject; KeyState: LongWord; Pt: TPoint;
  var Effect: LongWord): HResult;

begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
  {
  if Assigned(FDropTargetHelper) and FFullDragging then
    FDropTargetHelper.Drop(DataObject, Pt, Effect);

  Result := TVirtualTreeAccess(FOwner).DragDrop(DataObject, KeyState, Pt, Effect);
  FIsDropTarget := False;
  FDataObject := nil;
  }
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTDragManager.ForceDragLeave;

// Some drop targets, e.g. Internet Explorer leave a drag image on screen instead removing it when they receive
// a drop action. This method calls the drop target helper's DragLeave method to ensure it removes the drag image from
// screen. Unfortunately, sometimes not even this does help (e.g. when dragging text from VT to a text field in IE).

begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
  {
  if Assigned(FDropTargetHelper) and FFullDragging then
    FDropTargetHelper.DragLeave;
    }
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDragManager.GiveFeedback(Effect: LongWord): HResult;

begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
  //Result := DRAGDROP_S_USEDEFAULTCURSORS;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDragManager.QueryContinueDrag(EscapePressed: BOOL; KeyState: LongWord): HResult;

var
  RButton,
  LButton: Boolean;

begin
  {$ifdef DEBUG_VTV}Logger.SendError([lcOle],'Ole function called in Linux');{$endif}
  {$ifdef DEBUG_VTV}Logger.SendCallStack([lcOle],'Stack');{$endif}
  {
  LButton := (KeyState and MK_LBUTTON) <> 0;
  RButton := (KeyState and MK_RBUTTON) <> 0;

  // Drag'n drop canceled by pressing both mouse buttons or Esc?
  if (LButton and RButton) or EscapePressed then
    Result := DRAGDROP_S_CANCEL
  else
    // Drag'n drop finished?
    if not (LButton or RButton) then
      Result := DRAGDROP_S_DROP
    else
      Result := S_OK;
  }
end;


end.

