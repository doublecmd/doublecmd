(*
   Daniel U. Thibault
   <D.U.Thibault@Bigfoot.com>
   19 August 1999
   Updated 15 September 1999

   Constants, types that have appeared since ShlObj.pas.

   The values marked //MISSING VALUES remain unidentified (two sets).

   Koblov Alexander (Alexx2000@mail.ru)
   15 July 2007
   Add some functions, constants and types for Lazarus compability
*)

unit uShlObjAdditional;

{$mode delphi}
{$WEAKPACKAGEUNIT}

interface

uses
   Windows,
   ShlObj,
   ActiveX; { IBindCtx }

{
   IContextMenu::InvokeCommand
   Parameters in lpci:
      hMonitor -- Specifies the default monitor (optional).
      When SEE_MASK_HMONITOR is set in TCMInvokeCommandInfo,
      hIcon is treated as hMonitor
}

const
  TPM_RETURNCMD = $100;
  CSIDL_DRIVES                        = $0011;
  { GetCommandString uFlags }
  GCS_VERBA            = $00000000;
const
   // QueryContextMenu uFlags
   CMF_EXPLORE             = $00000004;
   CMF_CANRENAME           = $00000010;
   CMF_FINDHACK            = $00000080;
   CMF_EXTENDEDVERBS       = $00000100; //Rarely used verbs


   SEE_MASK_UNICODE           = $00004000; //Correction
   SEE_MASK_HMONITOR          = $00200000;
   //MISSING VALUES
// SEE_MASK_HASLINKNAME       = $00000800; ??  Other possible values:
// SEE_MASK_FLAG_SEPVDM       = $00001000; ??  $00010000 $00020000 $00040000 $00080000
// SEE_MASK_HASTITLE          = $00002000; ??  $00400000 $02000000 $04000000 $08000000
//                                             $10000000 $40000000 $80000000
   //Shell 5+ only
   SEE_MASK_USERLOGON         = $00800000;
   SEE_MASK_NOQUERYCLASSSTORE = $01000000;

// CMIC_MASK_HASLINKNAME  = SEE_MASK_HASLINKNAME; ??
// CMIC_MASK_FLAG_SEP_VDM = SEE_MASK_FLAG_SEPVDM; ??
// CMIC_MASK_HASTITLE     = SEE_MASK_HASTITLE;    ??
   CMIC_MASK_PTINVOKE     = $20000000; // Shell 4+

type
   //Shell 4+ version appends a ptInvoke field
   TCMInvokeCommandInfoEx4 = packed record
      cbSize,
      fMask        : DWORD;
      hwnd         : HWND;
      lpVerb,
      lpParameters,
      lpDirectory  : LPCSTR;
      nShow        : Integer;
      dwHotKey     : DWORD;
      hIcon        : THandle;
      lpTitle      : LPCSTR;
      lpVerbW,
      lpParametersW,
      lpDirectoryW,
      lpTitleW     : LPCWSTR;
      ptInvoke     : TPoint;  // Point where it's invoked
   end; { TCMInvokeCommandInfoEx4 }
   PCMInvokeCommandInfoEx4 = ^TCMInvokeCommandInfoEx4;

const
  SID_IContextMenu       = '{000214E4-0000-0000-C000-000000000046}';
  SID_IContextMenu2      = '{000214F4-0000-0000-C000-000000000046}';

type
  IContextMenu = interface(IUnknown)
    [SID_IContextMenu]
    function QueryContextMenu(Menu: HMENU;
      indexMenu, idCmdFirst, idCmdLast, uFlags: UINT): HResult; stdcall;
    function InvokeCommand(var lpici: TCMInvokeCommandInfo): HResult; stdcall;
    function GetCommandString(idCmd, uType: UINT; pwReserved: PUINT;
      pszName: LPSTR; cchMax: UINT): HResult; stdcall;
  end;

  IContextMenu2 = interface(IContextMenu)
    [SID_IContextMenu2]
    function HandleMenuMsg(uMsg: UINT; WParam, LParam: Integer): HResult; stdcall;
  end;

{ IContextMenu3 Interface }
{
   IContextMenu3 (IContextMenu2 with one new member)

   IContextMenu3::HandleMenuMsg2

   This function is called if the client of IContextMenu is aware of
   the IContextMenu3 interface and receives a WM_MENUCHAR message while
   it is calling TrackPopupMenu (in the window proc of hwndOwner)
   (typically when an accelerator is used to select a menu item that displays a bitmap).
   WM_MENUCHAR is sent when the user presses a key that does not correspond to any
   mnemonic or accelerator key; an application should process this message when an
   accelerator is used to select a menu item that displays a bitmap.
}
const
   IID_IContextMenu3 : TGUID = (
   D1:$BCFCE0A0; D2:$EC17; D3:$11D0; D4:($8D,$10,$00,$A0,$C9,$0F,$27,$19));
   SID_IContextMenu3 = '{BCFCE0A0-EC17-11d0-8D10-00A0C90F2719}';

type
   IContextMenu3 = interface(IContextMenu2)
      [SID_IContextMenu3]
      function HandleMenuMsg2(uMsg : UINT; wParam : WPARAM; lParam : LPARAM; var MsgResult : LongInt) : HResult; stdcall;
   end; { IContextMenu3 }

{ IPersistFolder2 Interface }
//Shell 4+ only
{
   The IPersistFolder2 interface is used by the Shell 4+ file system implementation
   of IShellFolder::BindToObject when it is initializing a shell folder object.

   IPersistFolder2::GetCurFolder
   Parameters:
   ppidl --
}
const
   IID_IPersistFolder2 : TGUID = (
   D1:$1AC3D9F0; D2:$175C; D3:$11D1; D4:($95,$BE,$00,$60,$97,$97,$EA,$4F));
   SID_IPersistFolder  = '{000214EA-0000-0000-C000-000000000046}';
   SID_IPersistFolder2 = '{1AC3D9F0-175C-11D1-95BE-00609797EA4F}';

type
   IPersistFolder = interface(IPersist)
    [SID_IPersistFolder]
    function Initialize(pidl: PItemIDList): HResult; stdcall;
  end;

   IPersistFolder2 = interface(IPersistFolder)
      [SID_IPersistFolder2]
      function GetCurFolder(var ppidl: PItemIDList) : HResult; stdcall;
   end; { IPersistFolder2 }

{ IPersistFolder3 Interface }
{
   The PERSIST_FOLDER_TARGET_INFO (TPersistFolderTargetInfo) stucture is used
   for Folder Shortcuts which allow the shell to have a file system folder act
   like another area in the name space.
   One of pidlTargetFolder, szTargetParsingName, or csidl needs to specify the
   destination name space.

   pidlTargetFolder:
      NULL if not specified.  This is a full pidl to the target folder.

   szTargetParsingName:
      Empty string if not specified. Ortherwise, it is the parsable name
      of the target.  This name can be parsed by IShellFolder::ParseDisplayName()
      from the desktop.

   szNetworkProvider:
      Can be an empty string.  If not empty, it specifies the type of network
      provider that will be used when binding to the target.  This is used for
      performance optimizations for the WNet APIs.

   dwAttributes:
      -1 if not known.  These are the SFGAO_ flags for IShellFolder::GetAttributesOf()

   csidl:
      This is -1 if not used.  This can be used instead of pidlTargetFolder or
      szTargetParsingName to indicate the TargetFolder.  See the list of CSIDL_ folders
      below.  CSIDL_FLAG_PFTI_TRACKTARGET means that the IShellFolder's target folder
      should change if the user changes the target of the underlying CSIDL value.
      You can also pass CSIDL_FLAG_CREATE to indicate that the target folder should
      be created if it does not exist.  No other CSIDL_FLAG_* values are supported.
}
type
   TPersistFolderTargetInfo = packed record
      pidlTargetFolder     : PItemIDList;                     // pidl for the folder we want to initialize
      szTargetParsingName,                                    // optional parsing name for the target
      szNetworkProvider    : array [1..MAX_PATH] of WideChar; // optional network provider
      dwAttributes         : DWord;                           // optional FILE_ATTRIBUTES_ flags (-1 if not used)
      csidl                : Integer;                         // optional folder index (SHGetFolderPath()) -1 if not used
   end; { TPersistFolderTargetInfo }

{
   The IPersistFolder3 interface is implemented by an IShellFolder object that
   wants non-default handling of Folder Shortcuts.

   IPersistFolder3::InitializeEx
   This method initializes an IShellFolder and specifies where it is rooted in the name space.
   Parameters:
   pbc:      May be NULL.
   pidlRoot: This is the same parameter as IPersistFolder::Initialize(). Caller allocates
             and frees this parameter.
   ppfti:    May be NULL, in which case this is the same as a call to IPersistFolder::Initialize().
             Otherwise this is a Folder Shortcut and this struct specifies the target
             folder and its attributes.

   IPersistFolder3::GetFolderTargetInfo
   This is used by the caller to find information about the folder shortcut.
   Parameters:
   ppfti:    This is a Folder Shortcut and this struct specifies the target
             folder and its attributes.  This structure may not be initialized
             by the caller, so the callee needs to initialize every member.
             The callee allocates pidlTargetFolder and the caller will free it.
}
const
   IID_IPersistFolder3 : TGUID = (
   D1:$CEF04FDF; D2:$FE72; D3:$11D2; D4:($87,$A5,$00,$C0,$4F,$68,$37,$CF));
   SID_IPersistFolder3 = '{CEF04FDF-FE72-11D2-87A5-00C04F6837CF}';

type
   IPersistFolder3 = interface(IPersistFolder2)
      [SID_IPersistFolder3]
      function InitializeEx(var pbc : IBindCtx; pidlRoot : PItemIDList; var pfti : TPersistFolderTargetInfo) : HResult; stdcall;
      function GetFolderTargetInfo(var pfti : TPersistFolderTargetInfo) : HResult; stdcall;
   end; { IPersistFolder3 }

{ IPersistFreeThreadedObject Interface }
{
   This interface is just the IID.  Return back
   a pointer to the IPersist interface if the object
   implementation is free threaded.  This is used
   for performance on free threaded objects.
}
const
   IID_IPersistFreeThreadedObject : TGUID = (
   D1:$C7264BF0; D2:$EDB6; D3:$11D1; D4:($85,$46,$00,$60,$08,$05,$93,$68));
   SID_IPersistFreeThreadedObject = '{C7264BF0-EDB6-11D1-8546-006008059368}';

type
   IPersistFreeThreadedObject = interface(IPersist)
      [SID_IPersistFreeThreadedObject]
   end; { IPersistFreeThreadedObject }

{ IRemoteComputer Interface }
{
   The IRemoteComputer interface is used to initialize a name space
   extension invoked on a remote computer object.

   [Member functions]
   IRemoteComputer::Initialize
   This member function is called when the explorer is initializing or
   enumerating the name space extension. If failure is returned during
   enumeration, the extension won't appear for this computer. Otherwise,
   the extension will appear, and should target the given machine.

   Parameters:
   pszMachine -- Specifies the name of the machine to target.
}
const
   IID_IRemoteComputer : TGUID = (
   D1:$000214FE; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
   SID_IRemoteComputer = '{000214FE-0000-0000-C000-000000000046}';

type
   IRemoteComputer = interface(IUnknown)
      [SID_IRemoteComputer]
      function Initialize(pszMachine : PWideChar; bEnumerating : BOOL) : HResult; stdcall;
   end; { IRemoteComputer }

{ IShellIconOverlayIdentifier Interface }
{
   Used to identify a file as a member of the group of files that have this specific
   icon overlay.

   Users can create new IconOverlayIdentifiers and place them in the following registry
   location together with the Icon overlay image and their priority.
   HKEY_LOCAL_MACHINE\Software\Microsoft\Windows\CurrentVersion\ShellIconOverlayIdentifiers

   The shell will enumerate through all IconOverlayIdentifiers at start, and prioritize
   them according to internal rules, in case the internal rules don't apply, we use their
   input priority.

   [Member functions]
   IShellIconOverlayIdentifier::IsMemberOf
   Returns:
      S_OK,    if the file is a member
      S_FALSE, if the file is not a member
      E_FAIL,  if the operation failed due to bad WIN32_FIND_DATA

   Parameters:
   pwszPath --     full path of the file
   dwAttrib --     attribute of this file

   IShellIconOverlayIdentifier::GetOverlayInfo

   Parameters:
     pszIconFile -- the path of the icon file
     pIndex      -- Depend on the flags, this could contain the IconIndex
     dwFlags     -- defined below

   IShellIconOverlayIdentifier::GetPriority

   Parameters:
     pIPriority --  the priority of this Overlay Identifier
}
const
   IID_IShellIconOverlayIdentifier : TGUID = (
   D1:$0C6C4200; D2:$C589; D3:$11D0; D4:($99,$9A,$00,$C0,$4F,$D6,$55,$E1));
   SID_IShellIconOverlayIdentifier = '{0C6C4200-C589-11D0-999A-00C04FD655E1}';

type
   IShellIconOverlayIdentifier = interface(IUnknown)
      [SID_IShellIconOverlayIdentifier]
      function IsMemberOf(pwszPath : PWideChar; dwAttrib : DWORD) : HResult; stdcall;
      function GetOverlayInfo(pwszIconFile : PWideChar; cchMax : Integer; var Index : Integer; var dwFlags : DWORD) : HResult; stdcall;
      function GetPriority(var IPriority : Integer) : HResult; stdcall;
   end; { IShellIconOverlayIdentifier }

const
   ISIOI_ICONFILE  = $00000001; // path is returned through pwszIconFile
   ISIOI_ICONINDEX = $00000002; // icon index in pwszIconFile is returned through Index

{ IShellIconOverlay Interface }
{
   Used to return the icon overlay index or its icon index for an IShellFolder object,
   this is always implemented with IShellFolder

   [Member functions]
   IShellIconOverlay::GetOverlayIndex

   Parameters:
     pidl            object to identify icon overlay for.
     pdwIndex        the Overlay Index in the system image list

   IShellIconOverlay::GetOverlayIconIndex
   This method is only used for those who are interested in seeing the real bits
   of the Overlay Icon
   Returns:
      S_OK,  if the index of an Overlay is found
      S_FALSE, if no Overlay exists for this file
      E_FAIL, if pidl is bad
   Parameters:
     pdwIconIndex    the Overlay Icon index in the system image list
}
const
   IID_IShellIconOverlay : TGUID = (
   D1:$7D688A70; D2:$C613; D3:$11D0; D4:($99,$9B,$00,$C0,$4F,$D6,$55,$E1));
   SID_IShellIconOverlay = '{7D688A70-C613-11D0-999B-00C04FD655E1}';

type
   IShellIconOverlay = interface(IUnknown)
      [SID_IShellIconOverlay]
      function GetOverlayIndex(pidl : PItemIDList; var Index : Integer) : HResult; stdcall;
      function GetOverlayIconIndex(pidl : PItemIDList; var IconIndex : Integer) : HResult; stdcall;
   end; { IShellIconOverlay }

const
   OI_ASYNC = $FFFFEEEE;

{ SHGetIconOverlayIndex }
{
   This function takes the path and icon/res id to the icon and converts it into
   an overlay index in the system image list.
   Note: there are only 15 slots for system image overlays, some of which were
   reserved by the system, or taken by the overlayidentifiers, so it's possible
   for this function to fail and return -1.

   To get the default overlays in the system, such as the share hand, link shortcut
   and slow files, pass NULL as the file name, then the IDO_SHGIOI_* flags as the
   icon index.
}
   IDO_SHGIOI_SHARE    = $0FFFFFFF;
   IDO_SHGIOI_LINK     = $0FFFFFFE;
   IDO_SHGIOI_SLOWFILE = $0FFFFFFD;

function SHGetIconOverlayIndexA(pszIconPath : PANSIChar; iIconIndex : Integer) : Integer; stdcall;
function SHGetIconOverlayIndexW(pszIconPath : PWideChar; iIconIndex : Integer) : Integer; stdcall;
function SHGetIconOverlayIndex (pszIconPath : PChar;     iIconIndex : Integer) : Integer; stdcall;

{ IShellLinkDataList Interface }
// Shell 4+ only

// IShellLinkDataList::GetFlags()/SetFlags()
const
   //SHELL_LINK_DATA_FLAGS
   SLDF_HAS_ID_LIST       = $00000001; // Shell link saved with ID list
   SLDF_HAS_LINK_INFO     = $00000002; // Shell link saved with LinkInfo
   SLDF_HAS_NAME          = $00000004;
   SLDF_HAS_RELPATH       = $00000008;
   SLDF_HAS_WORKINGDIR    = $00000010;
   SLDF_HAS_ARGS          = $00000020;
   SLDF_HAS_ICONLOCATION  = $00000040;
   SLDF_UNICODE           = $00000080; // the strings are unicode
   SLDF_FORCE_NO_LINKINFO = $00000100; // don't create a LINKINFO (make a dumb link)
   SLDF_HAS_EXP_SZ        = $00000200; // the link contains expandable env strings
   SLDF_RUN_IN_SEPARATE   = $00000400; // Run the 16-bit target exe in a separate VDM/WOW
   SLDF_HAS_LOGO3ID       = $00000800; // this link is a special Logo3/MSICD link
   SLDF_HAS_DARWINID      = $00001000; // this link is a special Darwin link
   SLDF_RUNAS_USER        = $00002000; // Run this link as a different user
   SLDF_HAS_EXP_ICON_SZ   = $00004000; // contains expandable env string for icon path

// Also defined in ShlWAPI.h
type
   TDataBlockHeader = packed record
      cbSize,              // Size of this extra data block
      dwSignature : DWORD; // signature of this extra data block
   end; { TDataBlockHeader }
   PDataBlockHeader = ^TDataBlockHeader;
   PDBList          = PDataBlockHeader;

   TNTConsoleProps = packed record
      dbh                     : TDataBlockHeader;
      wFillAttribute,                   // fill attribute for console
      wPopupFillAttribute     : Word;   // fill attribute for console popups
      dwScreenBufferSize,               // screen buffer size for console
      dwWindowSize,                     // window size for console
      dwWindowOrigin          : TCoord; // window origin for console
      nFont,
      nInputBufferSize        : DWord;
      dwFontSize              : TCoord;
      uFontFamily,
      uFontWeight             : UInt;
      FaceName                : array [1..LF_FACESIZE] of WideChar;
      uCursorSize             : UInt;
      bFullScreen,
      bQuickEdit,
      bInsertMode,
      bAutoPosition           : BOOL;
      uHistoryBufferSize,
      uNumberOfHistoryBuffers : UInt;
      bHistoryNoDup           : BOOL;
      ColorTable              : array[0..15] of TColorRef;
   end; { TNTConsoleProps }
   PNTConsoleProps = ^TNTConsoleProps;

const
   NT_CONSOLE_PROPS_SIG = $A0000002;

   //This is an FE Console property
type
   TNTFEConsoleProps = packed record
      dbh       : TDataBlockHeader;
      uCodePage : UInt;
   end; { TNTFEConsoleProps }
   PTNTFEConsoleProps = ^TNTFEConsoleProps;

const
   NT_FE_CONSOLE_PROPS_SIG = $A0000004;

//Shell 5+ only
type
   TExpDarwinLink = packed record
      dbh         : TDataBlockHeader;
      szDarwinID  : array [1..MAX_PATH] of Char;     // ANSI Darwin ID associated with link
      szwDarwinID : array [1..MAX_PATH] of WideChar; // UNICODE Darwin ID associated with link
   end; { TExpDarwinLink }
   PExpDarwinLink = ^TExpDarwinLink;

const
   EXP_DARWIN_ID_SIG = $A0000006;
   EXP_LOGO3_ID_SIG  = $A0000007;

   EXP_SPECIAL_FOLDER_SIG = $A0000005; // LPEXP_SPECIAL_FOLDER

type
   TExpSpecialFolder = packed record
      cbSize,                  // Size of this extra data block
      dwSignature,             // Signature of this extra data block
      idSpecialFolder,         // Special folder ID this link points into
      cbOffset        : DWord; // Offset into pidl from SLDF_HAS_ID_LIST for child
   end; { TExpSpecialFolder }
   PExpSpecialFolder = ^TExpSpecialFolder;

   TExpSzLink = packed record
      cbSize,                                        // Size of this extra data block
      dwSignature : DWord;                           // Signature of this extra data block
      szTarget    : array [1..MAX_PATH] of Char;     // ANSI target name w/EXP_SZ in it
      swzTarget   : array [1..MAX_PATH] of WideChar; // UNICODE target name w/EXP_SZ in it
   end; { TExpSzLink }
   PExpSzLink = ^TExpSzLink;

const
   EXP_SZ_LINK_SIG = $A0000001; // LPEXP_SZ_LINK (target)
   EXP_SZ_ICON_SIG = $A0000007; // LPEXP_SZ_LINK (icon)

const
   IID_IShellLinkDataList : TGUID = (
   D1:$45E2B4AE; D2:$B1C3; D3:$11D0; D4:($B9,$2F,$00,$A0,$C9,$03,$12,$E1));
   SID_IShellLinkDataList = '{45E2B4AE-B1C3-11D0-B92F-00A0C90312E1}';

type
   IShellLinkDataList = interface(IUnknown)
      [SID_IShellLinkDataList]
      function AddDataBlock(pDataBlock : Pointer) : HResult; stdcall;
      function CopyDataBlock(dwSig : DWord; var pDataBlock : Pointer) : HResult; stdcall;
      function RemoveDataBlock(dwSig : DWord) : HResult; stdcall;
      function GetFlags(var dwFlags : DWord) : HResult; stdcall;
      function SetFlags(dwFlags : DWord) : HResult; stdcall;
   end; { IShellLinkDataList }

{ IResolveShellLink Interface }
//Shell 5+ only
const
   IID_IResolveShellLink : TGUID = (
   D1:$5CD52983; D2:$9449; D3:$11D2; D4:($96,$3A,$00,$C0,$4F,$79,$AD,$F0));
   SID_IResolveShellLink = '{5CD52983-9449-11D2-963A-00C04F79ADF0}';

type
   IResolveShellLink = interface(IUnknown)
      [SID_IResolveShellLink]
      function ResolveShellLink(punk : IUnknown; hWndOwner : HWND; fFlags : DWord) : HResult; stdcall;
   end; { IResolveShellLink }

{ IShellLink Interface }
const
   // IShellLink.Resolve fFlags
   SLR_NOUPDATE   = $0008;
   SLR_NOSEARCH   = $0010; // don't execute the search heuristics
   SLR_NOTRACK    = $0020; // don't use NT5 object ID to track the link
   SLR_NOLINKINFO = $0040; // don't use the net and volume relative info
   SLR_INVOKE_MSI = $0080; // if we have a darwin link, then call msi to fault in the applicaion

   // IShellLink::GetPath fFlags
   SLGP_RAWPATH   = $0004;

{ IURLSearchHook Interface }
const
   IID_IURLSearchHook : TGUID = (
   D1:$AC60F6A0; D2:$0FD9; D3:$11D0; D4:($99,$CB,$00,$C0,$4F,$D6,$44,$97));
   SID_IURLSearchHook = '{AC60F6A0-0FD9-11D0-99CB-00C04FD64497}';

type
   IURLSearchHook = interface(IUnknown)
      [SID_IURLSearchHook]
      function Translate(lpwszSearchURL : PWideChar; cchBufferSize : DWORD) : HResult; stdcall;
   end; { IURLSearchHook }

{ Shell File Operations }
const
   FOF_NOCOPYSECURITYATTRIBS = $0800; // dont copy NT file Security Attributes
   FOF_NORECURSION           = $1000; // don't recurse into directories.
   //Shell 5+
   FOF_NO_CONNECTED_ELEMENTS = $2000; // don't operate on connected file elements.
   FOF_WANTNUKEWARNING       = $4000; // during delete operation, warn if nuking instead of recycling (partially overrides FOF_NOCONFIRMATION)

{ IShellBrowser/IShellView/IShellFolder interfaces }
//Shell 4+
   // The resource id of the offline cursor
   // This cursor is available in ShDocVw.dll
   IDC_OFFLINE_HAND          = 103;

{ FOLDERSETTINGS }
   // NB Bitfields
   FWF_NOICONS       = $1000;
   FWF_SHOWSELALWAYS = $2000;
   FWF_NOVISIBLE     = $4000;

   { Values for wFlags parameter of IShellBrwoser.BrowseObject member }
   SBSP_HELPMODE           = $0040; // IEUNIX : Help window uses this.
   SBSP_NOTRANSFERHIST     = $0080; // IEUNIX only mode.
   SBSP_NAVIGATEBACK       = $4000;
   SBSP_NAVIGATEFORWARD    = $8000;
   SBSP_ALLOW_AUTONAVIGATE = $00010000;
   SBSP_WRITENOHISTORY     = $08000000;
   SBSP_NOAUTOSELECT       = $04000000;

   { Values for id parameter of IShellBrwoser.GetWindow/SendControlMsg members }
   FCW_INTERNETBAR = $0006;
   FCW_PROGRESS    = $0008;

   SBO_DEFAULT        = 0;
   SBO_NOBROWSERPAGES = 1;

{ ICommDlgBrowser2 Interface }
{
   [Member functions]
   ICommDlgBrowser2::Notify
   Called when the view wants to notify common dialog when an event occurs.
   CDB2N_CONTEXTMENU_START indicates the context menu has started.
   CDB2N_CONTEXTMENU_DONE  indicates the context menu has completed.

   ICommDlgBrowser2::GetDefaultMenuText
   Called when the view wants to get the default context menu text.
   pszText points to buffer and cchMax specifies the size of the
   buffer in characters.  The browser on return has filled the buffer
   with the default context menu text.  The Shell will call this method
   with at least a buffer size of MAX_PATH.  The browser should return
   S_OK if it returned a new default menu text, S_FALSE to let the view
   to use the normal default menu text.

   ICommDlgBrowser2::GetViewFlags
   Called when the view wants to determine  if special customization needs to
   be done for the common dialog browser. For example View calls this function to
   determine if all files(hidden and system) need to be shown.
   If the GetViewFlags returns a DWORD with
   CDB2GVF_SHOWALLFILES flag set then it will show all the files.
}
   CDB2N_CONTEXTMENU_DONE  = $00000001;
   CDB2N_CONTEXTMENU_START = $00000002;

   //GetViewFlags
   CDB2GVF_SHOWALLFILES    = $00000001;
(*
const
   IID_ICommDlgBrowser2 : TGUID = (
   D1:$6D71532E; D2:$889E; D3:$11D1; D4:($99,$7F,$08,$00,$36,$AF,$3F,$03));
   SID_ICommDlgBrowser  = '{000214F1-0000-0000-C000-000000000046}';
   SID_ICommDlgBrowser2 = '{6D71532E-889E-11D1-997F-080036AF3F03}';

type
   ICommDlgBrowser = interface(IUnknown)
    [SID_ICommDlgBrowser]
    function OnDefaultCommand(const ppshv: IShellView): HResult; stdcall;
    function OnStateChange(const ppshv: IShellView; Change: ULONG): HResult; stdcall;
    function IncludeObject(const ppshv: IShellView; pidl: PItemIDList): HResult; stdcall;
  end;

   ICommDlgBrowser2 = interface(ICommDlgBrowser)
      [SID_ICommDlgBrowser2]
      function Notify(var ShellView: IShellView; dwNotifyType : DWORD) : HResult; stdcall;
      function GetDefaultMenuText(var ShellView: IShellView; pszText : PWideChar; cchMax : Integer) : HResult; stdcall;
      function GetViewFlags(var dwFlags : DWORD) : HResult; stdcall;
   end; { ICommDlgBrowser2 }
*)
const
   { IShellView select item flags }
   SVSI_TRANSLATEPT   = $0020;

function SHCreateDirectoryExA(hWndOwner : HWND; pszPath : PANSIChar; var sa : TSecurityAttributes) : Integer; stdcall;
function SHCreateDirectoryExW(hWndOwner : HWND; pszPath : PWideChar; var sa : TSecurityAttributes) : Integer; stdcall;
function SHCreateDirectoryEx (hWndOwner : HWND; pszPath : PChar    ; var sa : TSecurityAttributes) : Integer; stdcall;

const
   //Object identifiers in the explorer's name space (ItemID and IDList)
   CSIDL_INTERNET          = $0001;  // Internet Explorer desktop icon
   CSIDL_LOCAL_APPDATA     = $001c; // <user name>\Local Settings\Application Data (non roaming)
   CSIDL_ALTSTARTUP        = $001d; // non localized startup
   CSIDL_COMMON_ALTSTARTUP = $001e; // non localized common startup
   CSIDL_COMMON_FAVORITES  = $001f;
   CSIDL_INTERNET_CACHE    = $0020; // C:\Windows\Temporary Internet Files
   CSIDL_COOKIES           = $0021; // C:\Windows\Cookies
   CSIDL_HISTORY           = $0022; // C:\Windows\History (URL History Folder)
   CSIDL_COMMON_APPDATA    = $0023; // All Users\Application Data
   CSIDL_WINDOWS           = $0024; // GetWindowsDirectory()or SYSROOT
   CSIDL_SYSTEM            = $0025; // GetSystemDirectory()
   CSIDL_PROGRAM_FILES     = $0026; // C:\Program Files
   CSIDL_MYPICTURES        = $0027; // My Pictures
   CSIDL_PROFILE           = $0028; // USERPROFILE
   CSIDL_SYSTEMX86         = $0029; // Get the x86 system directory on RISC
   CSIDL_PROGRAM_FILESX86  = $002a; // x86 Program Files directory on RISC
   CSIDL_PROGRAM_FILES_COMMON    = $002b; // C:\Program Files\Common
   CSIDL_PROGRAM_FILES_COMMONX86 = $002c; // x86 Program Files\Common on RISC
   CSIDL_COMMON_TEMPLATES        = $002d; // All Users\Templates
   CSIDL_COMMON_DOCUMENTS        = $002e; // All Users\Documents
   CSIDL_COMMON_ADMINTOOLS       = $002f; // All Users\Start Menu\Programs\Administrative Tools
   CSIDL_ADMINTOOLS              = $0030; // <user name>\Start Menu\Programs\Administrative Tools

   CSIDL_FLAG_CREATE       = $8000; // combine with CSIDL_ value to force create on SHGetSpecialFolderLocation()
   CSIDL_FLAG_DONT_VERIFY  = $4000; // combine with CSIDL_ value to return an unverified folder path
   CSIDL_FLAG_PFTI_TRACKTARGET = CSIDL_FLAG_DONT_VERIFY;
   CSIDL_FLAG_MASK         = $FF00; // mask for all possible flag values
(*
   What about C:\Windows\Subscriptions ? (Subscription Folder)
   What about C:\Windows\Downloaded Program Files ? (ActiveX Cache Folder)
*)

//Shell 4+ only
   function SHGetSpecialFolderPathA(hWndOwner : HWND; lpszPath : PANSIChar; csidl : Integer; fCreate : BOOL) : BOOL; stdcall;
   function SHGetSpecialFolderPathW(hWndOwner : HWND; lpszPath : PWideChar; csidl : Integer; fCreate : BOOL) : BOOL; stdcall;
   function SHGetSpecialFolderPath (hWndOwner : HWND; lpszPath : PChar;     csidl : Integer; fCreate : BOOL) : BOOL; stdcall;

//Shell 5+ only
const
   SHGFP_TYPE_CURRENT = 0; // current value for user, verify it exists
   SHGFP_TYPE_DEFAULT = 1; // default value, may not exist

   function SHGetFolderPathA(hWndOwner : HWND; csidl : Integer; hToken : THandle; dwReserved : DWORD; lpszPath : PANSIChar) : HResult; stdcall;
   function SHGetFolderPathW(hWndOwner : HWND; csidl : Integer; hToken : THandle; dwReserved : DWORD; lpszPath : PWideChar) : HResult; stdcall;
   function SHGetFolderPath (hWndOwner : HWND; csidl : Integer; hToken : THandle; dwReserved : DWORD; lpszPath : PChar    ) : HResult; stdcall;
   function SHGetFolderLocation(hWndOwner : HWND; csidl : Integer; hToken : THandle; dwReserved : DWORD; var pidl : PItemIDList) : HResult; stdcall;

const
{ SHBrowseForFolder API }
   { Browsing for directory }
   {
   BIF_STATUSTEXT        = $0004;
   The top of the dialog has 2 lines of text for BROWSEINFO.lpszTitle
   and one line if this flag is set.  Passing the message
   BFFM_SETSTATUSTEXTA to the hwnd can set the rest of the text.
   This is not used with BIF_USENEWUI and BROWSEINFO.lpszTitle then
   gets all three lines of text.
   }
   BIF_EDITBOX           = $0010; // Add an editbox to the dialog.  Always on with BIF_USENEWUI.
   BIF_VALIDATE          = $0020; // insist on valid result (or CANCEL)
   BIF_USENEWUI          = $0040; // Use the new dialog layout with the ability to resize.
                                  // Caller needs to call OleInitialize() before using this API.
   BIF_BROWSEINCLUDEURLS = $0080; // Allow URLs to be displayed or entered. (Requires BIF_USENEWUI)

   BIF_SHAREABLE         = $8000; // sharable resources displayed (remote shares, requires BIF_USENEWUI)

   { message from browser }
   BFFM_VALIDATEFAILEDA   = 3;   // lParam:szPath ret:1(cont),0(EndDialog)
   BFFM_VALIDATEFAILEDW   = 4;   // lParam:wzPath ret:1(cont),0(EndDialog)
   BFFM_VALIDATEFAILED    = BFFM_VALIDATEFAILEDA;

   //IShellFolder.GetDisplayNameOf/SetNameOf uFlags
   SHGDN_FOREDITING         = $1000; // for in-place editing
   SHGDN_INCLUDE_NONFILESYS = $2000; // if not set, display names for shell name space items that are not in the file system will fail

   //IShellFolder::EnumObjects flags
   SHCONTF_INIT_ON_FIRST_NEXT = $0100; // allow EnumObject() to return before validating enum
   SHCONTF_NETPRINTERSRCH     = $0200; // hint that client is looking for printers
   SHCONTF_SHAREABLE          = $0400; // hint that client is looking for sharable resources (remote shares)

   //IShellFolder::CompareIDs lParam flags
   SHCIDS_ALLFIELDS  = $80000000;
   SHCIDS_COLUMNMASK = $0000FFFF;

   //IShellFolder::GetAttributesOf flags
// SFGAO_CANLINK        = DROPEFFECT_LINK; // Objects can be linked    (0x4)
{
   If this bit is set on an item in the shell folder, a "Create Shortcut"
   menu item will be added to the File and context menus for the item.
   If the user selects that command, your IContextMenu::InvokeCommand()
   will be called with 'link'.
   That flag will also be used to determine if "Create Shortcut" should be
   added when the item in your folder is dragged to another folder.
}
   SFGAO_HIDDEN         = $00080000; // hidden object
   SFGAO_BROWSABLE      = $08000000; // is in-place browsable
   SFGAO_NONENUMERATED  = $00100000; // is a non-enumerated object
   SFGAO_NEWCONTENT     = $00200000; // should show bold in explorer tree
   SFGAO_CANMONIKER     = $00400000; // can create monikers for its objects

   // IShellFolder BindCtx parameters.
   //the IUnknown for these are accessed through IBindCtx::RegisterObjectParam/GetObjectParam
   //This object will support IPersist to query a CLSID that should be skipped
   //in the binding process. This is to avoid loops or to allow delegation to
   //base name space functionality. See SHSkipJunction
   STR_SKIP_BINDING_CLSID     = 'Skip Binding CLSID';

{ IFileSystemBindData Interface }
{
   IShellFolder IBindCtx* parameters.  The IUnknown for these are
   accessed through IBindCtx::RegisterObjectParam/GetObjectParam
   use this to provide the data needed to create IDLists through
   IShellFolder::ParseDisplayName().  This data applies to the last
   element of the name that is parsed (e.g. 'c:\foo\bar.txt', data
   applies to bar.txt) this makes creating these IDLists much faster
   that suppling the name only.
}
   STR_FILE_SYS_BIND_DATA     = 'File System Bind Data';

const
   IID_IFileSystemBindData : TGUID = (
   D1:$01E18D10; D2:$4D8B; D3:$11D2; D4:($85,$5D,$00,$60,$08,$05,$93,$67));
   SID_IFileSystemBindData = '{01E18D10-4D8B-11D2-855D-006008059367}';

type
   IFileSystemBindData = interface(IUnknown)
      [SID_IFileSystemBindData]
      function SetFindData(var fd : TWin32FindDataW) : HResult; stdcall;
      function GetFindData(var fd : TWin32FindDataW) : HResult; stdcall;
   end; { IFileSystemBindData }

{ IShellDetails Interface }

type
   TShellDetails = packed record
      fmt,              // LVCFMT_* value (header only)
      cxChar : Integer; // Number of "average" characters (header only)
      str    : TStrRet; // String information
   end; { TShellDetails }
   PShellDetails = ^TShellDetails;

const
   IID_IShellDetails : TGUID = (
   D1:$000214EC; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
   SID_IShellDetails = '{000214EC-0000-0000-C000-000000000046}';

type
   IShellDetails = interface(IUnknown)
      [SID_IShellDetails]
      function GetDetailsOf(pidl : PItemIDList; iColumn : UInt; var Details : TShellDetails) : HResult; stdcall;
      function ColumnClick(iColumn : UInt) : HResult; stdcall;
   end; { IShellDetails }

{ IEnumExtraSearch Interface }
{
   IShellFolder2::EnumSearches member returns an IEnumExtraSearch object.
}
type
   TExtraSearch = packed record
      guidSearch : TGUID;
(*
   //This is the Sep 1998 declaration
      wszMenuText,
      wszHelpText : Array[1..MAX_PATH] of WideChar;
      wszUrl      : Array[1..2084] of WideChar;
      wszIcon,    // "name.dll,#" where # is icon index in the file name.dll
      wszGreyIcon,
      wszClrIcon  : Array[1..MAX_PATH+10] of WideChar;
*)
   //This is the Jul 1999 declaration
      wszFriendlyName : array [1..80] of WideChar;
      wszUrl          : array [1..2084] of WideChar;
   end; { TExtraSearch}
   PExtraSearch = ^TExtraSearch;

// typedef struct IEnumExtraSearch      *LPENUMEXTRASEARCH;

const
   IID_IEnumExtraSearch : TGUID = (
   D1:$0E700BE1; D2:$9DB6; D3:$11D1; D4:($A1,$CE,$00,$C0,$4F,$D7,$5D,$13));
   SID_IEnumExtraSearch = '{0E700BE1-9DB6-11d1-A1CE-00C04FD75D13}';

type
   IEnumExtraSearch = interface(IUnknown)
      [SID_IEnumExtraSearch]
      function Next(celt : ULONG; var rgelt : TExtraSearch; var celtFetched : ULONG) : HResult; stdcall;
      function Skip(celt : ULONG) : HResult; stdcall;
      function Reset : HResult; stdcall;
      function Clone(out ppenum: IEnumExtraSearch) : HResult; stdcall;
   end; { IEnumExtraSearch }

{ IShellFolder2 Interface }
{
   [Member Functions]
   IShellFolder2::GetDefaultSearchGUID
   Returns the guid of the search that is to be invoked when user clicks
   on the search toolbar button.

   IShellFolder2::EnumSearches
   Gives an enumerator of the searches to be added to the search menu.

   IShellFolder2::GetDefaultColumn

   IShellFolder2::GetDefaultColumnState

   IShellFolder2::GetDetailsEx

   IShellFolder2::GetDetailsOf

   IShellFolder2::MapColumnToSCID
}
const
   // IShellFolder2::GetDefaultColumnState values
   SHCOLSTATE_TYPE_STR    = $00000001;
   SHCOLSTATE_TYPE_INT    = $00000002;
   SHCOLSTATE_TYPE_DATE   = $00000003;
   SHCOLSTATE_TYPEMASK    = $0000000F;
   SHCOLSTATE_ONBYDEFAULT = $00000010; // should be on by default in details view
   SHCOLSTATE_SLOW        = $00000020; // will be slow to compute, do on a background thread
   SHCOLSTATE_EXTENDED    = $00000040; // provided by a handler, not the folder
   SHCOLSTATE_SECONDARYUI = $00000080; // not displayed in context menu, but listed in the "More..." dialog
   SHCOLSTATE_HIDDEN      = $00000100; // not displayed in the UI

type
   TSHColumnID = packed record
      fmtid : TGUID;
      pid   : DWORD;
   end; { TSHColumnID }
   PSHColumnID = ^TSHColumnID;

const
   IID_IShellFolder2 : TGUID = (
   D1:$93F2F68C; D2:$1D1B; D3:$11D3; D4:($A3,$0E,$00,$C0,$4F,$79,$AB,$D1));
   SID_IShellFolder2 = '{93F2F68C-1D1B-11D3-A30E-00C04F79ABD1}';

type
   IShellFolder2 = interface(IShellFolder)
      [SID_IShellFolder2]
      function GetDefaultSearchGUID(var lpGuid : TGUID) : HResult; stdcall;
      function EnumSearches(ppenum : IEnumExtraSearch) : HResult; stdcall;
      function GetDefaultColumn(dwRes : DWord; var Sort, Display : ULONG) : HResult; stdcall;
      function GetDefaultColumnState(iColumn : UInt; var csFlags : DWord) : HResult; stdcall;
      function GetDetailsEx(pidl : PItemIDList; var scid : TShColumnID; var v : OLEVariant {VARIANT}) : HResult; stdcall;
      function GetDetailsOf(pidl : PItemIDList; iColumn : UInt; var sd : TShellDetails) : HResult; stdcall;
      function MapColumnToSCID(iColumn : UInt; var scid : TShColumnID) : HResult; stdcall;
   end; { IShellFolder2 }

{ ITaskbarList Interface }
{
   [Member functions]
   ITaskbarList::HrInit
   This function must be called first to validate use of other members.

   ITaskbarList::AddTab
   This function adds a tab for hwnd to the taskbar.

   ITaskbarList::DeleteTab
   This function deletes a tab for hwnd from the taskbar.

   ITaskbarList::ActivateTab
   This function activates the tab associated with hwnd on the taskbar.

   ITaskbarList::SetActivateAlt
   This function marks hwnd in the taskbar as the active tab.
}

const
   IID_ITaskbarList : TGUID = (
   D1:$56FDF342; D2:$FD6D; D3:$11D0; D4:($95,$8A,$00,$60,$97,$C9,$A0,$90));
   SID_ITaskbarList = '{56FDF342-FD6D-11d0-958A-006097C9A090}';

type
   ITaskbarList = interface(IUnknown)
      [SID_ITaskbarList]
      function HrInit : HResult; stdcall;
      function AddTab(hWndOwner : HWND) : HResult; stdcall;
      function DeleteTab(hWndOwner : HWND) : HResult; stdcall;
      function ActivateTab(hWndOwner : HWND) : HResult; stdcall;
      function SetActiveAlt(hWndOwner : HWND) : HResult; stdcall;
   end; { ITaskbarList }

{ IInputObjectSite/IInputObject interfaces }
{
   These interfaces allow us (or ISVs) to install/update external Internet
   Toolbar for IE and the shell. The frame will simply get the CLSID from
   registry (to be defined) and CoCreateInstance it.
}

{ IInputObjectSite interface }
{
   A site implements this interface so the object can communicate
   focus change to it.

   [Member functions]

   IInputObjectSite::OnFocusChangeIS(punkObj, fSetFocus)
     Object (punkObj) is getting or losing the focus.
}

const
   IID_IInputObjectSite : TGUID = (
   D1:$F1DB8392; D2:$7331; D3:$11D0; D4:($8C,$99,$00,$A0,$C9,$2D,$BF,$E8));
   SID_IInputObjectSite = '{F1DB8392-7331-11D0-8C99-00A0C92DBFE8}';

type
   IInputObjectSite = interface(IUnknown)
      [SID_IInputObjectSite]
      function OnFocusChangeIS(punkObj : IUnknown; fSetFocus : BOOL) : HResult; stdcall;
   end; { IInputObjectSite }

{ IInputObject interface }
{
   An object implements this interface so the site can communicate
   activation and accelerator events to it.

   [Member functions]

   IInputObject::UIActivateIO(fActivate, lpMsg)
     Activates or deactivates the object.  lpMsg may be NULL.  Returns
     S_OK if the activation succeeded.

   IInputObject::HasFocusIO()
     Returns S_OK if the object has the focus, S_FALSE if not.

   IInputObject::TranslateAcceleratorIO(lpMsg)
     Allow the object to process the message.  Returns S_OK if the
     message was processed (eaten).
}

const
   IID_IInputObject : TGUID = (
   D1:$68284FAA; D2:$6A48; D3:$11D0; D4:($8C,$78,$00,$C0,$4F,$D9,$18,$B4));
   SID_IInputObject = '{68284FAA-6A48-11D0-8C78-00C04FD918B4}';

type
   IInputObject = interface(IUnknown)
      [SID_IInputObject]
      function UIActivateIO(fActivate : BOOL; var Msg: TMsg {lpMsg : LPMSG}) : HResult; stdcall;
      function HasFocusIO : HResult; stdcall;
      function TranslateAcceleratorIO(var Msg: TMsg {lpMsg : LPMSG}) : HResult; stdcall;
   end; { IInputObject }

{ IDockingWindowSite/IDockingWindow/IDockingWindowFrame Interfaces }
{
   These interfaces allow us (or ISVs) to install/update external Internet
   Toolbar for IE and the shell. The frame will simply get the CLSID from
   registry (to be defined) and CoCreateInstance it.
}

{ IDockingWindowSite Interface }
{
   A site implements this interface so the object can negotiate for
   and inquire about real estate on the site.

   [Member functions]
   IDockingWindowSite::GetBorderDW
   Site returns the bounding rectangle of the given source object (punkObj).

   IDockingWindowSite::RequestBorderSpaceDW
   Object requests that the site makes room for it, as specified in
   pborderwidths.

   IDockingWindowSite::SetBorderSpaceDW
   Object requests that the site set the border spacing to the size
   specified in pborderwidths.
}

const
   IID_IDockingWindowSite : TGUID = (
   D1:$2A342FC2; D2:$7B26; D3:$11D0; D4:($8C,$A9,$00,$A0,$C9,$2D,$BF,$E8));
   SID_IDockingWindowSite = '{2A342FC2-7B26-11D0-8CA9-00A0C92DBFE8}';

type
   IDockingWindowSite = interface(IOLEWindow)
      [SID_IDockingWindowSite]
      function GetBorderDW(punkObj : IUnknown; prcBorder : PRECT) : HResult; stdcall;
      function RequestBorderSpaceDW(punkObj : IUnknown; pborderwidths : PRect) : HResult; stdcall;
      function SetBorderSpaceDW(punkObj : IUnknown; pborderwidths : PRect) : HResult; stdcall;
   end; { IDockingWindowSite }

{ IDockingWindowFrame Interface }
{
   [Member functions]
   IDockingWindowFrame::AddToolbar

   IDockingWindowFrame::RemoveToolbar

   IDockingWindowFrame::FindToolbar
}

const
   // flags for RemoveToolbar
   DWFRF_NORMAL           = $0000;
   DWFRF_DELETECONFIGDATA = $0001;

   // flags for AddToolbar
   DWFAF_HIDDEN           = $0001; // add hidden

const
   IID_IDockingWindowFrame : TGUID = (
   D1:$47D2657A; D2:$7B27; D3:$11D0; D4:($8C,$A9,$00,$A0,$C9,$2D,$BF,$E8));
   SID_IDockingWindowFrame = '{47D2657A-7B27-11D0-8CA9-00A0C92DBFE8}';

type
   IDockingWindowFrame = interface(IOLEWindow)
      [SID_IDockingWindowFrame]
      function AddToolbar(punkObj : IUnknown; pwszItem : PWideChar; dwAddFlags : DWORD) : HResult; stdcall;
      function RemoveToolbar(punkObj : IUnknown; dwRemoveFlags : DWORD) : HResult; stdcall;
      function FindToolbar(pwszItem : PWideChar; riid : TIID {REFIID}; var pvObj : Pointer) : HResult; stdcall;
   end; { IDockingWindowFrame }

{ IDockingWindow Interface }
{
   An object (docking window) implements this interface so the site can
   communicate with it.  An example of a docking window is a toolbar.

   [Member functions]
   IDockingWindow::ShowDW
   Shows or hides the docking window.

   IDockingWindow::CloseDW
   Closes the docking window.  dwReserved must be 0.

   IDockingWindow::ResizeBorderDW
   Resizes the docking window's border to *prcBorder.  fReserved must
   be 0.

   IObjectWithSite::SetSite(punkSite)
   IDockingWindow usually paired with IObjectWithSite.
   Provides the IUnknown pointer of the site to the docking window.
}

const
   IID_IDockingWindow : TGUID = (
   D1:$012DD920; D2:$7B26; D3:$11D0; D4:($8C,$A9,$00,$A0,$C9,$2D,$BF,$E8));
   SID_IDockingWindow = '{012DD920-7B26-11D0-8CA9-00A0C92DBFE8}';

type
   IDockingWindow = interface(IOLEWindow)
      [SID_IDockingWindow]
      function ShowDW(fShow : BOOL) : HResult; stdcall;
      function CloseDW(dwReserved : DWORD) : HResult; stdcall;
      function ResizeBorderDW(prcBorder : PRECT; punkToolbarSite : IUnknown; fReserved : BOOL) : HResult; stdcall;
   end; { IDockingWindow }

{ IDeskBand Interface }
{
   [Member functions]

   IDeskBand::GetBandInfo
   Returns info on the given band in *pdbi, according to the mask
   field in the DESKBANDINFO structure and the given viewmode.
}

const
   // Mask values for DESKBANDINFO
   DBIM_MINSIZE   = $0001;
   DBIM_MAXSIZE   = $0002;
   DBIM_INTEGRAL  = $0004;
   DBIM_ACTUAL    = $0008;
   DBIM_TITLE     = $0010;
   DBIM_MODEFLAGS = $0020;
   DBIM_BKCOLOR   = $0040;

type
   TDeskbandInfo = packed record
      dwMask      : DWORD;
      ptMinSize,
      ptMaxSize,
      ptIntegral,
      ptActual    : TPoint;
      wszTitle    : Array[1..256] of WideChar;
      dwModeFlags : DWORD;
      crBkgnd     : TColorRef;
   end; { TDeskbandInfo }
   PDeskbandInfo = ^TDeskbandInfo;

const
   // DESKBANDINFO dwModeFlags values
   DBIMF_NORMAL         = $0000;
   DBIMF_VARIABLEHEIGHT = $0008;
   DBIMF_DEBOSSED       = $0020;
   DBIMF_BKCOLOR        = $0040;

   // GetBandInfo view mode values
   DBIF_VIEWMODE_NORMAL      = $0000;
   DBIF_VIEWMODE_VERTICAL    = $0001;
   DBIF_VIEWMODE_FLOATING    = $0002;
   DBIF_VIEWMODE_TRANSPARENT = $0004;

const
   IID_IDeskBand : TGUID = (
   D1:$EB0FE172; D2:$1A3A; D3:$11D0; D4:($89,$B3,$00,$A0,$C9,$0A,$90,$AC));
   SID_IDeskBand = '{EB0FE172-1A3A-11D0-89B3-00A0C90A90AC}';

type
   IDeskBand = interface(IDockingWindow)
      [SID_IDeskBand]
      function GetBandInfo(dwBandID, dwViewMode : DWORD; pdbi : PDeskbandInfo) : HResult; stdcall;
   end; { IDeskBand }

const
   // Command Target IDs
    DBID_BANDINFOCHANGED = 0;
    DBID_SHOWONLY        = 1;
    DBID_MAXIMIZEBAND    = 2; // Maximize the specified band (VT_UI4 == dwID)
    DBID_PUSHCHEVRON     = 3;
    DBID_DELAYINIT       = 4; // Note: _bandsite_ calls _band_ with this code
    DBID_FINISHINIT      = 5; // Note: _bandsite_ calls _band_ with this code

{ IRunnableTask Interface }
//Shell 4+ only
{
   This is a free threaded interface used for putting items on a background
   scheduler for execution within the view.  It allows a scheduler to start and
   stop tasks on as many worker threads as it deems necessary.

   Run(), Kill() and Suspend() may be called from different threads.

   [Member functions]
   IRunnableTask::Run
   Initiate the task to run.  This should return E_PENDING if the task
   has been suspended.

   IRunnableTask::Kill

   IRunnableTask::Suspend

   IRunnableTask::Resume

   IRunnableTask::IsRunning
}

const
   // Convenient state values
   IRTIR_TASK_NOT_RUNNING = 0;
   IRTIR_TASK_RUNNING     = 1;
   IRTIR_TASK_SUSPENDED   = 2;
   IRTIR_TASK_PENDING     = 3;
   IRTIR_TASK_FINISHED    = 4;

const
   IID_IRunnableTask : TGUID = (
   D1:$85788D00; D2:$6807; D3:$11D0; D4:($B8,$10,$00,$C0,$4F,$D7,$06,$EC));
   SID_IRunnableTask = '{85788D00-6807-11d0-B810-00C04FD706EC}';

type
   IRunnableTask = interface(IUnknown)
      [SID_IRunnableTask]
      function Run : HResult; stdcall;
      function Kill(fWait : BOOL) : HResult; stdcall;
      function Suspend : HResult; stdcall;
      function Resume : HResult; stdcall;
      function IsRunning : ULONG; stdcall;
   end; { IRunnableTask }

{ IExtractImage Interface }
{
   This interface is provided for objects to provide a thumbnail image.

   [Member functions]
   IExtractImage::GetLocation
   Gets a path description of the image that is to be extracted. This is used to
   identify the image in the view so that multiple instances of the same image can reuse the
   original image. If *pdwFlags == IEIFLAG_ASYNC and the result is E_PENDING, then *pdwPriority
   is used to return the priority of the item, this is usually a measure of how long it will take
   to perform the extraction. *pdwFlags can return IEIFLAG_CACHE if the view should cache a copy
   of the image for future reference and faster access. This flag is use dto tell the difference
   between file formats that cache a thumbnail image  such as Flashpix or Office documents, and those
   that don't cache one.

   IExtractImage::Extract
   Extract the thumbnail of the specified size. If GetLocation() returned the values indicating
   it is free-threaded and can be placed on a background thread. If the object
   supports IRunnableTask as well, then long extractions can be started and paused as appropriate.
   At this point it is asssumed the object is free-threaded.
   If dwRecClrDepth contains a recommended Colour depth
   If *phBmpthumbnail is non NULL, then it contains the destination bitmap that should be used.
}

const
   //MISSING VALUES
// IEI_PRIORITY_MAX     = ITSAT_MAX_PRIORITY;     // 99? FF00? FFFF?
// IEI_PRIORITY_MIN     = ITSAT_MIN_PRIORITY;     // 01?
// IEIT_PRIORITY_NORMAL = ITSAT_DEFAULT_PRIORITY; // 01?

   IEIFLAG_ASYNC    = $0001; // ask the extractor if it supports ASYNC extract (free threaded)
   IEIFLAG_CACHE    = $0002; // returned from the extractor if it does NOT cache the thumbnail
   IEIFLAG_ASPECT   = $0004; // passed to the extractor to beg it to render to the aspect ratio of the supplied rect
   IEIFLAG_OFFLINE  = $0008; // if the extractor shouldn't hit the net to get any content neede for the rendering
   IEIFLAG_GLEAM    = $0010; // does the image have a gleam ? this will be returned if it does
   IEIFLAG_SCREEN   = $0020; // render as if for the screen  (this is exlusive with IEIFLAG_ASPECT )
   IEIFLAG_ORIGSIZE = $0040; // render to the approx size passed, but crop if neccessary

const
   IID_IExtractImage : TGUID = (
   D1:$BB2E617C; D2:$0920; D3:$11D1; D4:($9A,$0B,$00,$C0,$4F,$C2,$D6,$C1));
   SID_IExtractImage = '{BB2E617C-0920-11d1-9A0B-00C04FC2D6C1}';

type
   IExtractImage = interface(IUnknown)
      [SID_IExtractImage]
      function GetLocation(pszPathBuffer : PWideChar; cch : DWORD; var dwPriority : DWORD; var rgSize : TPoint { SIZE }; dwRecClrDepth : DWORD; var dwFlags : DWORD) : HResult; stdcall;
      function Extract(var hBmpThumbnail : HBITMAP) : HResult; stdcall;
   end; { IExtractImage }

{ IExtractImage2 Interface }
//Shell 5+ only
{
   [Member functions]
   IExtractImage2::GetDateStamp
   Returns the date stamp associated with the image. If this image is already cached,
   then it is easy to find out if the image is out of date.
}

const
   IID_IExtractImage2 : TGUID = (
   D1:$953BB1EE; D2:$93B4; D3:$11D1; D4:($98,$A3,$00,$C0,$4F,$B6,$87,$DA));
   SID_IExtractImage2 = '{953BB1EE-93B4-11D1-98A3-00C04FB687DA}';

type
   IExtractImage2 = interface(IExtractImage)
      [SID_IExtractImage2]
      function GetDateStamp(var DateStamp : TFileTime) : HResult; stdcall;
   end; { IExtractImage2 }

{ IActiveDesktop Interface }
//Shell 4+ only
{
   The COMPONENT structure uses INTERNET_MAX_URL_LENGTH from WinInet.h
   Flags and structures used by IActiveDesktop
}
type
   TWallpaperOpt = packed record
      dwSize,          // size of this Structure.
      dwStyle : DWORD; // WPSTYLE_* mentioned above
   end; { TWallpaperOpt }
   PWallpaperOpt = ^TWallpaperOpt;

   TComponentsOpt = packed record
      dwSize         : DWORD; //Size of this structure
      fEnableComponents,      //Enable components?
      fActiveDesktop : BOOL;  // Active desktop enabled?
   end; { TComponentsOpt }
   PComponentsOpt = ^TComponentsOpt;

   TCompPos = packed record
      dwSize               : DWORD;   //Size of this structure
      iLeft,                          //Left of top-left corner in screen co-ordinates.
      iTop                 : Integer; //Top of top-left corner in screen co-ordinates.
      dwWidth,                        //Width in pixels.
      dwHeight             : DWORD;   //Height in pixels.
      izIndex              : Integer; //Indicates the Z-order of the component.
      fCanResize,                     //Is the component resizable?
      fCanResizeX,                    //Resizable in X-direction?
      fCanResizeY          : BOOL;    //Resizable in Y-direction?
      iPreferredLeftPercent,          //Left of top-left corner as percent of screen width
      iPreferredTopPercent : Integer; //Top of top-left corner as percent of screen height
   end; { TCompPos }
   PCompPos = ^TCompPos;

   TCompStateInfo = packed record
      dwSize      : DWORD;   //Size of this structure.
      iLeft,                 //Left of the top-left corner in screen co-ordinates.
      iTop        : Integer; //Top of top-left corner in screen co-ordinates.
      dwWidth,               //Width in pixels.
      dwHeight,              //Height in pixels.
      dwItemState : DWORD;   //State of the component (full-screen mode or split-screen or normal state.
   end; { TCompStateInfo }
   PCompStateInfo = ^TCompStateInfo;

const
// COMPONENT_TOP = $7fffffff; // Sep 1998 value
   COMPONENT_TOP = $3fffffff; // izOrder value meaning component is at the top

   // iCompType values
   COMP_TYPE_HTMLDOC = 0;
   COMP_TYPE_PICTURE = 1;
   COMP_TYPE_WEBSITE = 2;
   COMP_TYPE_CONTROL = 3;
   COMP_TYPE_CFHTML  = 4;
   COMP_TYPE_MAX     = 4;

// From WinInet.h:
// Maximum field lengths (arbitrary)
   INTERNET_MAX_HOST_NAME_LENGTH   =   256;
   INTERNET_MAX_USER_NAME_LENGTH   =   128;
   INTERNET_MAX_PASSWORD_LENGTH    =   128;
   INTERNET_MAX_PORT_NUMBER_LENGTH =     5; // INTERNET_PORT is unsigned short
   INTERNET_MAX_PORT_NUMBER_VALUE  = 65535; // maximum unsigned short value
   INTERNET_MAX_PATH_LENGTH        =  2048;
   INTERNET_MAX_SCHEME_LENGTH      =    32; // longest protocol name length
   INTERNET_MAX_URL_LENGTH         = (INTERNET_MAX_SCHEME_LENGTH +
                                      SizeOf('://') +
                                      INTERNET_MAX_PATH_LENGTH);
type
   // The following is the COMPONENT structure used in IE4.01, IE4.0 and Memphis.
   // It is kept here for compatibility reasons.
   TIE4Component = packed record
      dwSize,                      //Size of this structure
      dwID             : DWORD;    //Reserved: Set it always to zero
      iComponentType   : Integer;  //One of COMP_TYPE_*
      fChecked,                    // Is this component enabled?
      fDirty,                      // Had the component been modified and not yet saved to disk?
      fNoScroll        : BOOL;     // Is the component scrollable?
      cpPos            : TCompPos; // Width, height etc.
      // Friendly name of component
      wszFriendlyName  : Array[1..MAX_PATH] of WideChar;
      //URL of the component
      wszSource,
      //Subscribed URL
      wszSubscribedURL : Array[1..INTERNET_MAX_URL_LENGTH] of WideChar;
   end; { TIE4Component }
   PIE4Component = ^TIE4Component;

// The following is the new NT5 component structure. Note that the initial portion of this component exactly
// matches the IE4COMPONENT structure. All new fields are added at the bottom and the dwSize field is used to
// distinguish between IE4COMPONENT and the new COMPONENT structures.
   TIE5Component { TComponent! } = packed record
      dwSize,                      //Size of this structure
      dwID             : DWORD;    //Reserved: Set it always to zero
      iComponentType   : Integer;  //One of COMP_TYPE_*
      fChecked,                    // Is this component enabled?
      fDirty,                      // Had the component been modified and not yet saved to disk?
      fNoScroll        : BOOL;     // Is the component scrollable?
      cpPos            : TCompPos; // Width, height etc.
      // Friendly name of component
      wszFriendlyName  : Array[1..MAX_PATH] of WideChar;
      //URL of the component
      wszSource,
      //Subscribed URL
      wszSubscribedURL : Array[1..INTERNET_MAX_URL_LENGTH] of WideChar;
      //New fields are added below
      dwCurItemState   : DWORD;     // Current state of the Component
      csiOriginal,                  // Original state of the component when it was first added
      csiRestored : TCompStateInfo; // Restored state of the component
   end; { TIE5Component }
   PIE5Component = ^TIE5Component;

const
   // Defines for dwCurItemState
   IS_NORMAL             = $00000001;
   IS_FULLSCREEN         = $00000002;
   IS_SPLIT              = $00000004;
   // The set of IS_* state bits which define the "size" of the component
   // These bits are mutually exclusive
   IS_VALIDSIZESTATEBITS = IS_NORMAL or IS_SPLIT or IS_FULLSCREEN;
   // All of the currently defined IS_* bits
   IS_VALIDSTATEBITS     = IS_NORMAL or IS_SPLIT or IS_FULLSCREEN or $80000000 or $40000000;

   // Flags for IActiveDesktop::ApplyChanges
   AD_APPLY_SAVE             = $00000001;
   AD_APPLY_HTMLGEN          = $00000002;
   AD_APPLY_REFRESH          = $00000004;
   AD_APPLY_ALL              = AD_APPLY_SAVE or AD_APPLY_HTMLGEN or AD_APPLY_REFRESH;
   AD_APPLY_FORCE            = $00000008;
   AD_APPLY_BUFFERED_REFRESH = $00000010;
   AD_APPLY_DYNAMICREFRESH   = $00000020;

   // Flags for IActiveDesktop::GetWallpaperOptions
   //           IActiveDesktop::SetWallpaperOptions
   WPSTYLE_CENTER  = 0;
   WPSTYLE_TILE    = 1;
   WPSTYLE_STRETCH = 2;
   WPSTYLE_MAX     = 3;

   // Flags for IActiveDesktop::ModifyComponent
   COMP_ELEM_TYPE          = $00000001;
   COMP_ELEM_CHECKED       = $00000002;
   COMP_ELEM_DIRTY         = $00000004;
   COMP_ELEM_NOSCROLL      = $00000008;
   COMP_ELEM_POS_LEFT      = $00000010;
   COMP_ELEM_POS_TOP       = $00000020;
   COMP_ELEM_SIZE_WIDTH    = $00000040;
   COMP_ELEM_SIZE_HEIGHT   = $00000080;
   COMP_ELEM_POS_ZINDEX    = $00000100;
   COMP_ELEM_SOURCE        = $00000200;
   COMP_ELEM_FRIENDLYNAME  = $00000400;
   COMP_ELEM_SUBSCRIBEDURL = $00000800;
   COMP_ELEM_ORIGINAL_CSI  = $00001000;
   COMP_ELEM_RESTORED_CSI  = $00002000;
   COMP_ELEM_CURITEMSTATE  = $00004000;

   COMP_ELEM_ALL = COMP_ELEM_TYPE or COMP_ELEM_CHECKED or COMP_ELEM_DIRTY or
                   COMP_ELEM_NOSCROLL or COMP_ELEM_POS_LEFT or COMP_ELEM_SIZE_WIDTH  or
                   COMP_ELEM_SIZE_HEIGHT or COMP_ELEM_POS_ZINDEX or COMP_ELEM_SOURCE or
                   COMP_ELEM_FRIENDLYNAME or COMP_ELEM_POS_TOP or COMP_ELEM_SUBSCRIBEDURL or
                   COMP_ELEM_ORIGINAL_CSI or COMP_ELEM_RESTORED_CSI or COMP_ELEM_CURITEMSTATE;

   // Flags for IActiveDesktop::AddDesktopItemWithUI
   //DTI_ADTIWUI
   DTI_ADDUI_DEFAULT       = $00000000;
   DTI_ADDUI_DISPSUBWIZARD = $00000001;
   DTI_ADDUI_POSITIONITEM  = $00000002;

   // Flags for IActiveDesktop::AddUrl
   ADDURL_SILENT           = $0001;

   // Default positions for ADI
   COMPONENT_DEFAULT_LEFT  = $FFFF;
   COMPONENT_DEFAULT_TOP   = $FFFF;

{ IActiveDesktop Interface }
const
   IID_IActiveDesktop : TGUID = (
   D1:$F490EB00; D2:$1240; D3:$11D1; D4:($98,$88,$00,$60,$97,$DE,$AC,$F9));
   SID_IActiveDesktop = '{F490EB00-1240-11D1-9888-006097DEACF9}';

type
   IActiveDesktop = interface(IUnknown)
      [SID_IActiveDesktop]
      function ApplyChanges(dwFlags : DWORD) : HResult; stdcall;
      function GetWallpaper(pwszWallpaper : PWideChar; cchWallpaper : UINT; dwReserved : DWORD) : HResult; stdcall;
      function SetWallpaper(pwszWallpaper : PWideChar; dwReserved : DWORD) : HResult; stdcall;
      function GetWallpaperOptions(pwpo : PWallpaperOpt; dwReserved : DWORD) : HResult; stdcall;
      function SetWallpaperOptions(pwpo : PWallpaperOpt; dwReserved : DWORD) : HResult; stdcall;
      function GetPattern(pwszPattern : PWideChar; cchPattern : UINT; dwReserved : DWORD) : HResult; stdcall;
      function SetPattern(pwszPattern : PWideChar; dwReserved : DWORD) : HResult; stdcall;
      function GetDesktopItemOptions(pco : PComponentsOpt; dwReserved : DWORD) : HResult; stdcall;
      function SetDesktopItemOptions(pco : PComponentsOpt; dwReserved : DWORD) : HResult; stdcall;
      function AddDesktopItem(pcomp : PIE5Component; dwReserved : DWORD) : HResult; stdcall;
      function AddDesktopItemWithUI(hWndOwner : HWND; pcomp : PIE5Component; dwReserved : DWORD) : HResult; stdcall;
      function ModifyDesktopItem(pcomp : PIE5Component; dwFlags : DWORD) : HResult; stdcall;
      function RemoveDesktopItem(pcomp : PIE5Component; dwReserved : DWORD) : HResult; stdcall;
      function GetDesktopItemCount(var iCount : Integer; dwReserved : DWORD) : HResult; stdcall;
      function GetDesktopItem(nComponent : Integer; pcomp : PIE5Component; dwReserved : DWORD) : HResult; stdcall;
      function GetDesktopItemByID(dwID : DWORD {ULONG_PTR}; pcomp : PIE5Component; dwReserved : DWORD) : HResult; stdcall;
      function GenerateDesktopItemHtml(pwszFileName : PWideChar; pcomp : PIE5Component; dwReserved : DWORD) : HResult; stdcall;
      function AddUrl(hWndOwner : HWND; pszSource : PWideChar; pcomp : PIE5Component; dwFlags : DWORD) : HResult; stdcall;
      function GetDesktopItemBySource(pwszSource : PWideChar; pcomp : PIE5Component; dwReserved : DWORD) : HResult; stdcall;
   end; { IActiveDesktop }

//Shell 5+ only
const
   MAX_COLUMN_NAME_LEN =  80;
   MAX_COLUMN_DESC_LEN = 128;

type
   TSHColumnInit = packed record
      dwFlags,                                      // initialization flags
      dwReserved : ULong;                           // reserved for future use.
      wszFolder  : array [1..MAX_PATH] of WideChar; // fully qualified folder path (or empty if multiple folders)
   end; { TSHColumnInit }
   PSHColumnInit = ^TSHColumnInit;

   TSHColumnInfo = packed record
      scid     : TSHColumnID; // OUT the unique identifier of this column
      vt       : TVarType;    // OUT the native type of the data returned
      fmt      : DWORD;       // OUT this listview format (LVCFMT_LEFT, usually)
      cChars   : UINT;        // OUT the default width of the column, in characters
(*
   //Sep 1998 definition
      bDefault : BOOL;        // OUT is column on by default?
      // OUT the title of the column
      wszTitle : Array[1..MAX_COLUMN_NAME_LEN] of WideChar;
*)
   //Jul 1999 definition
      csFlags        : DWord;                                      // OUT SHCOLSTATE flags
      wszTitle       : array [1..MAX_COLUMN_NAME_LEN] of WideChar; // OUT the title of the column
      wszDescription : array [1..MAX_COLUMN_DESC_LEN] of WideChar; // OUT full description of this column
   end; { TSHColumnInfo }
   PSHColumnInfo = ^TSHColumnInfo;

const
   SHCDF_UPDATEITEM = $00000001; // this flag is a hint that the file has changed since the last call to GetItemData

type
   TSHColumnData = packed record
(*
   //Sep 1998 definition
      pidl  : PItemIDList;          // IN  the absolute pidl (caller still owns!)
      vData : OLEVariant {VARIANT}; // OUT the data for the pidl
*)
   //Jul 1999 definition
      dwFlags          : ULong;                           // combination of SHCDF_ flags
      dwFileAttributes : DWord;                           // file attributes
      dwReserved       : ULong;                           // reserved for future use
      pwszExt          : PWideChar;                       // address of file name extension
      wszFile          : array [1..MAX_PATH] of WideChar; // Absolute path of file
   end; { TSHColumnData }
   PSHColumnData = ^TSHColumnData;

{ IColumnProvider Interface }
// Note: People who implement this interface must be threadsafe!
//  GetItemData _will_ be called simultaneously from multiple threads.
const
   IID_IColumnProvider : TGUID = (
   D1:$E8025004; D2:$1C42; D3:$11D2; D4:($BE,$2C,$00,$A0,$C9,$A8,$3D,$A1));
   SID_IColumnProvider = '{E8025004-1C42-11d2-BE2C-00A0C9A83DA1}';

type
   IColumnProvider = interface(IUnknown)
      [SID_IColumnProvider]
      function GetColumnCount : ULONG; stdcall;
      function GetColumnInfo(dwIndex : DWORD; psci : PSHColumnInfo) : HResult; stdcall;
      function GetItemData(pscid : PSHColumnID; pscd : PSHColumnData) : HResult; stdcall;
   end; { IColumnProvider }

const
   //Clipboard formats which may be supported by IDataObject
   CFSTR_SHELLURL             = 'UniformResourceLocator';
   CFSTR_PERFORMEDDROPEFFECT  = 'Performed DropEffect';
   CFSTR_PASTESUCCEEDED       = 'Paste Succeeded';
   CFSTR_INDRAGLOOP           = 'InShellDragLoop';
   CFSTR_DRAGCONTEXT          = 'DragContext';
   CFSTR_MOUNTEDVOLUME        = 'MountedVolume';

   //FILEDESCRIPTOR.dwFlags flags
   FD_PROGRESSUI        = $4000; // Show Progress UI w/Drag and Drop

{ format of CF_FILEGROUPDESCRIPTOR }
type

  TFileDescriptorA = record
    dwFlags: DWORD;
    clsid: TCLSID;
    sizel: TSize;
    pointl: TPoint;
    dwFileAttributes: DWORD;
    ftCreationTime: TFileTime;
    ftLastAccessTime: TFileTime;
    ftLastWriteTime: TFileTime;
    nFileSizeHigh: DWORD;
    nFileSizeLow: DWORD;
    cFileName: array[0..MAX_PATH-1] of AnsiChar;
  end;

  TFileDescriptorW = record
    dwFlags: DWORD;
    clsid: TCLSID;
    sizel: TSize;
    pointl: TPoint;
    dwFileAttributes: DWORD;
    ftCreationTime: TFileTime;
    ftLastAccessTime: TFileTime;
    ftLastWriteTime: TFileTime;
    nFileSizeHigh: DWORD;
    nFileSizeLow: DWORD;
    cFileName: array[0..MAX_PATH-1] of WideChar;
  end;
  TFileDescriptor = TFileDescriptorA;

  TFileGroupDescriptorA = record
    cItems: UINT;
    fgd: array[0..0] of TFileDescriptorA;
  end;

   TFileGroupDescriptorW = packed record { fgd }
      cItems : UINT;
      fgd    : array[0..0] of TFileDescriptorW;
   end; { TFileGroupDescriptorW }
   
   TFileGroupDescriptor  = TFileGroupDescriptorA;
   PFileGroupDescriptorA = ^TFileGroupDescriptorA;
   PFileGroupDescriptorW = ^TFileGroupDescriptorW;

const
   //File System Notification flags
   SHCNE_EXTENDED_EVENT      = $04000000; //Shell 4+ THIS IS A CORRECTION

   //Shell 4+ only
   SHCNEE_ORDERCHANGED  = $00000002;  //dwItem2 is the PIDL of the changed folder
   SHCNEE_MSI_CHANGE    = $00000004;  //dwItem2 is the product code
   SHCNEE_MSI_UNINSTALL = $00000005;  //dwItem2 is the product code

{ IShellChangeNotify Interface }
//Shell 5+ only
{
   IShellChangeNotify is used to notify a shell namespace extension when
   the ID of an item has changed.
   This interface is implemented by all namespace extensions.
   You do not call this interface directly.  IShellChangeNotify is used by
   the operating system only when it has confirmed that your application is
   aware of this interface.

   [Member Methods]
   OnChange    Called when an item in the namespace extension changes.
}
const
   //MISSING VALUE for IID_/SID_IShellChangeNotify obtained from newsgroups
   IID_IShellChangeNotify : TGUID = (
   D1:$D82BE2B1; D2:$5764; D3:$11D0; D4:($A9,$6E,$00,$C0,$4F,$D7,$05,$A2));
   SID_IShellChangeNotify = '{D82BE2B1-5764-11D0-A96E-00C04FD705A2}';

type
   IShellChangeNotify = interface(IUnknown)
      [SID_IShellChangeNotify]
      function OnChange(lEvent : LongInt {LONG}; pidl1, pidl2 : PItemIDList) : HResult; stdcall;
   end; { IShellChangeNotify }

{ IQueryInfo Interface }

const
  QITIPF_DEFAULT       = $00000000;
  QITIPF_USENAME       = $00000001;
  QITIPF_LINKNOTARGET  = $00000002;
  QITIPF_LINKUSETARGET = $00000004;
  QITIPF_USESLOWTIP    = $00000008;

const
   IID_IQueryInfo : TGUID = (
   D1:$00021500; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
   SID_IQueryInfo = '{00021500-0000-0000-C000-000000000046}';

type
   IQueryInfo = interface(IUnknown)
      [SID_IQueryInfo]
      function GetInfoTip(dwFlags : DWORD; var pwszTip : PWideChar) : HResult; stdcall;
      function GetInfoFlags(var dwFlags : DWORD) : HResult; stdcall;
   end; { IQueryInfo }

const
   QIF_CACHED           = $00000001;
   QIF_DONTEXPANDFOLDER = $00000002;

const
   // PROPIDs for Internet Shortcuts (FMTID_Intshcut)
   // to be used with IPropertySetStorage/IPropertyStorage.
   // The known property ids and their variant types are:
   PID_IS_URL         =   2; // [VT_LPWSTR]   URL
   PID_IS_NAME        =   4; // [VT_LPWSTR]   Name of the internet shortcut
   PID_IS_WORKINGDIR  =   5; // [VT_LPWSTR]   Working directory for the shortcut
   PID_IS_HOTKEY      =   6; // [VT_UI2]      Hotkey for the shortcut
   PID_IS_SHOWCMD     =   7; // [VT_I4]       Show command for shortcut
   PID_IS_ICONINDEX   =   8; // [VT_I4]       Index into file that has icon
   PID_IS_ICONFILE    =   9; // [VT_LPWSTR]   File that has the icon
   PID_IS_WHATSNEW    =  10; // [VT_LPWSTR]   What's New text
   PID_IS_AUTHOR      =  11; // [VT_LPWSTR]   Author
   PID_IS_DESCRIPTION =  12; // [VT_LPWSTR]   Description text of site
   PID_IS_COMMENT     =  13; // [VT_LPWSTR]   User annotated comment

   // PROPIDs for Internet Sites (FMTID_InternetSite)
   // to be used with IPropertySetStorage/IPropertyStorage.
   // The known property ids and their variant types are:
   PID_INTSITE_WHATSNEW     =  2; // [VT_LPWSTR]   What's New text
   PID_INTSITE_AUTHOR       =  3; // [VT_LPWSTR]   Author
   PID_INTSITE_LASTVISIT    =  4; // [VT_FILETIME] Time site was last visited
   PID_INTSITE_LASTMOD      =  5; // [VT_FILETIME] Time site was last modified
   PID_INTSITE_VISITCOUNT   =  6; // [VT_UI4]      Number of times user has visited
   PID_INTSITE_DESCRIPTION  =  7; // [VT_LPWSTR]   Description text of site
   PID_INTSITE_COMMENT      =  8; // [VT_LPWSTR]   User annotated comment
   PID_INTSITE_FLAGS        =  9;
   PID_INTSITE_CONTENTLEN   = 10;
   PID_INTSITE_CONTENTCODE  = 11;
   PID_INTSITE_RECURSE      = 12; // [VT_UI4]      Levels to recurse (0-3)
   PID_INTSITE_WATCH        = 13; // [VT_UI4]      PIDISM_ flags
   PID_INTSITE_SUBSCRIPTION = 14; // [VT_UI8]      Subscription cookie
   PID_INTSITE_URL          = 15; // [VT_LPWSTR]   URL
   PID_INTSITE_TITLE        = 16; // [VT_LPWSTR]   Title
   PID_INTSITE_CODEPAGE     = 18; // [VT_UI4]      Codepage of the document
   PID_INTSITE_TRACKING     = 19; // [VT_UI4]      Tracking
   PID_INTSITE_ICONINDEX    = 20;
   PID_INTSITE_ICONFILE     = 21;

   // Flags for PID_IS_FLAGS
   PIDISF_RECENTLYCHANGED = $00000001;
   PIDISF_CACHEDSTICKY    = $00000002;
   PIDISF_CACHEIMAGES     = $00000010;
   PIDISF_FOLLOWALLLINKS  = $00000020;

   // Values for PID_INTSITE_WATCH
   PIDISM_GLOBAL          = 0; // Monitor based on global setting
   PIDISM_WATCH           = 1; // User says watch
   PIDISM_DONTWATCH       = 2; // User says don't watch

// The shell keeps track of some per-user state to handle display
// options that is of major interest to ISVs.
// The key one requested right now is "DoubleClickInWebView".
type
   TShellFlag = (fShowAllObjects, fShowExtensions,       fNoConfirmRecycle, fShowSysFiles,
                 fShowCompColor,  fDoubleClickInWebView, fDesktopHTML,      fWin95Classic,
                 fDontPrettyPath, fShowAttribCol,        fMapNetDrvBtn,     fShowInfoTip,
                 fHideIcons); //13 bits
   TShellFlags = set of TShellFlag;
   TShellFlagState = packed record
      Flags : TShellFlags; //16 bits wide
   end; { TShellFlagState }
   PShellFlagState = ^TShellFlagState;

const
   SSF_SHOWALLOBJECTS        = $00000001;
   SSF_SHOWEXTENSIONS        = $00000002;
   SSF_SHOWCOMPCOLOR         = $00000008;
   SSF_SHOWSYSFILES          = $00000020;
   SSF_DOUBLECLICKINWEBVIEW  = $00000080;
   SSF_SHOWATTRIBCOL         = $00000100;
   SSF_DESKTOPHTML           = $00000200;
   SSF_WIN95CLASSIC          = $00000400;
   SSF_DONTPRETTYPATH        = $00000800;
   SSF_SHOWINFOTIP           = $00002000;
   SSF_MAPNETDRVBUTTON       = $00001000;
   SSF_NOCONFIRMRECYCLE      = $00008000;
   SSF_HIDEICONS             = $00004000;

{
   Use dwMask to specify the bits you are interested in
   and they will be filled out in the lpsfs structure.

   When these settings change, a WM_SETTINGCHANGE message is sent
   with the string lParam value of "ShellState".
}
   procedure SHGetSettings(lpsfs : PShellFlagState; dwMask : DWORD); stdcall;

{ SoftwareUpdateMessageBox }
// From URLMon.h
type
   TSoftDistInfo = packed record
      cbSize                : ULONG;
      dwFlags,
      dwAdState             : DWORD;
      szTitle,
      szAbstract,
      szHREF                : PWideChar;
      dwInstalledVersionMS,
      dwInstalledVersionLS,
      dwUpdateVersionMS,
      dwUpdateVersionLS,
      dwAdvertisedVersionMS,
      dwAdvertisedVersionLS,
      dwReserved            : DWORD;
   end; { TSoftDistInfo }
   PSoftDistInfo = ^TSoftDistInfo;

{
   Provides a standard message box for alerting the user that a software
   update is available or installed. No UI will be displayed if there is no
   update available or if the available update version is less than or equal
   to the Advertised update version.

   hWnd                - [in] Handle of owner window
   szDistUnit          - [in] Unique identifier string for a code distribution unit. For
                              ActiveX controls and Active Setup installed components, this
                              is typically a GUID string.
   dwFlags             - [in] Must be 0.
   psdi                - [in,out] Pointer to SOFTDISTINFO ( see URLMon.h ). May be NULL.
                         cbSize should be initialized
                         by the caller to sizeof(SOFTDISTINFO), dwReserved should be set to 0.

   RETURNS:

   IDNO     - The user chose cancel. If *pbRemind is FALSE, the caller should save the
              update version from the SOFTDISTINFO and pass it in as the Advertised
              version in future calls.

   IDYES    - The user has selected Update Now/About Update. The caller should navigate to
              the SOFTDISTINFO's pszHREF to initiate the install or learn about it.
              The caller should save the update version from the SOFTDISTINFO and pass
              it in as the Advertised version in future calls.

   IDIGNORE - There is no pending software update. Note: There is
              no Ignore button in the standard UI. This occurs if the available
              version is less than the installed version or is not present or if the
              Advertised version is greater than or equal to the update version.

   IDABORT  - An error occured. Call GetSoftwareUpdateInfo() for a more specific HRESULT.
              Note: There is no Abort button in the standard UI.
}
function SoftwareUpdateMessageBox(hWndOwner : HWND; szDistUnit : PWideChar; dwFlags : DWORD; psdi : PSoftDistInfo) : DWORD; stdcall;

function SHGetMalloc(var ppMalloc: IMalloc): HResult; stdcall;
function SHGetDesktopFolder(var ppshf: IShellFolder): HResult; stdcall;

function SHChangeIconDialog(hOwner: THandle; var FileName: UTF8String; var IconIndex: Integer): Boolean;
function SHGetOverlayIconIndex(const sFilePath, sFileName: UTF8String): Integer;
function SHGetInfoTip(const sFilePath, sFileName: UTF8String): UTF8String;

procedure OleErrorUTF8(ErrorCode: HResult);
procedure OleCheckUTF8(Result: HResult);

implementation

uses
  SysUtils, ComObj;

const
   shell32 = 'Shell32.dll'; //from ShellAPI, ShlObj

function SHGetIconOverlayIndexA; external shell32 name 'SHGetIconOverlayIndexA';
function SHGetIconOverlayIndexW; external shell32 name 'SHGetIconOverlayIndexW';
function SHGetIconOverlayIndex ; external shell32 name 'SHGetIconOverlayIndexA';

function SHCreateDirectoryExA; external shell32 name 'SHCreateDirectoryExA';
function SHCreateDirectoryExW; external shell32 name 'SHCreateDirectoryExW';
function SHCreateDirectoryEx ; external shell32 name 'SHCreateDirectoryExA';

function SHGetSpecialFolderPathA; external shell32 name 'SHGetSpecialFolderPathA';
function SHGetSpecialFolderPathW; external shell32 name 'SHGetSpecialFolderPathW';
function SHGetSpecialFolderPath;  external shell32 name 'SHGetSpecialFolderPathA';

function SHGetFolderPathA; external shell32 name 'SHGetFolderPathA';
function SHGetFolderPathW; external shell32 name 'SHGetFolderPathW';
function SHGetFolderPath;  external shell32 name 'SHGetFolderPathA';

function SHGetFolderLocation; external shell32 name 'SHGetFolderLocation';

procedure SHGetSettings; external shell32 name 'SHGetSettings';

function SoftwareUpdateMessageBox; external shell32 name 'SoftwareUpdateMessageBox';

function SHGetMalloc;                   external shell32 name 'SHGetMalloc';
function SHGetDesktopFolder;            external shell32 name 'SHGetDesktopFolder';

{ **** UBPFD *********** by delphibase.endimus.com ****
>> Вызывает диалог выбора иконки. Доработанная

Функция вызова диалогового окно "Изменение иконки"

Зависимости: Windows, SysUtils
Автор:       Alex Sal'nikov, alex-co@narod.ru, Москва
Copyright:   Доработка библиотеки JVCL
Дата:        15 июля 2003 г.
***************************************************** }

function SHChangeIconDialog(hOwner: THandle; var FileName: UTF8String; var IconIndex: Integer): Boolean;
type
  TSHChangeIconProc = function(Wnd: HWND; szFileName: PChar; Reserved: Integer;
                               var lpIconIndex: Integer): DWORD; stdcall;
  TSHChangeIconProcW = function(Wnd: HWND; szFileName: PWideChar;Reserved: Integer;
                                var lpIconIndex: Integer): DWORD; stdcall;
var
  ShellHandle: THandle;
  SHChangeIcon: TSHChangeIconProc;
  SHChangeIconW: TSHChangeIconProcW;
  Buf: array[0..MAX_PATH] of AnsiChar;
  BufW: array[0..MAX_PATH] of WideChar;
begin
  Result := False;
  SHChangeIcon := nil;
  SHChangeIconW := nil;
  ShellHandle := Windows.LoadLibrary(PChar(Shell32));
  try
    if ShellHandle <> 0 then
    begin
      if Win32Platform = VER_PLATFORM_WIN32_NT then
        SHChangeIconW := TSHChangeIconProcW(Windows.GetProcAddress(ShellHandle, PChar(62)))
      else
        SHChangeIcon := TSHChangeIconProc(Windows.GetProcAddress(ShellHandle, PChar(62)));
    end;

    if Assigned(SHChangeIconW) then
    begin
      BufW := UTF8Decode(FileName);
      Result := SHChangeIconW(hOwner, BufW, SizeOf(BufW), IconIndex) = 1;
      if Result then
        FileName := UTF8Encode(WideString(BufW));
    end
    else if Assigned(SHChangeIcon) then
    begin
      Buf := UTF8ToAnsi(FileName);
      Result := SHChangeIcon(hOwner, Buf, SizeOf(Buf), IconIndex) = 1;
      if Result then
        FileName := AnsiToUTF8(Buf);
    end
    else
      begin
        IconIndex := 0;
        Result := True;
      end;
  finally
    if ShellHandle <> 0 then
      FreeLibrary(ShellHandle);
  end;
end;

function SHGetOverlayIconIndex(const sFilePath, sFileName: UTF8String): Integer;
var
  Folder,
  DesktopFolder: IShellFolder;
  Pidl,
  ParentPidl: PItemIDList;
  IconOverlay: IShellIconOverlay;
  pchEaten: ULONG;
  dwAttributes: ULONG = 0;
  wsTemp: WideString;
begin
  Result:= -1;

  if SHGetDesktopFolder(DesktopFolder) = S_OK then
  begin
    wsTemp:= UTF8Decode(sFilePath);
    if DesktopFolder.ParseDisplayName(0, nil, PWideChar(wsTemp), pchEaten, ParentPidl, dwAttributes) = S_OK then
    begin
      if DesktopFolder.BindToObject(ParentPidl, nil, IID_IShellFolder, Folder) = S_OK then
      begin
        // Get an IShellIconOverlay interface for the folder.
        // If this fails then this version of
        // the shell does not have this
        // interface.
        if Folder.QueryInterface(IID_IShellIconOverlay, IconOverlay) = S_OK then
          begin
            // Get a pidl for the file.
            wsTemp:= UTF8Decode(sFileName);
            if Folder.ParseDisplayName(0, nil, PWideChar(wsTemp), pchEaten, Pidl, dwAttributes) = S_OK then
            begin
              // Get the overlay icon index.
              if IconOverlay.GetOverlayIconIndex(Pidl, Result) <> S_OK then
                Result:= -1;

              CoTaskMemFree(Pidl);
            end;
        end;
      end;

      CoTaskMemFree(ParentPidl);
    end;

    DesktopFolder:= nil;
  end; // SHGetDesktopFolder
end;

function SHGetInfoTip(const sFilePath, sFileName: UTF8String): UTF8String;
var
  DesktopFolder, Folder: IShellFolder;
  pidlFolder: PItemIDList = nil;
  pidlFile: PItemIDList = nil;
  queryInfo: IQueryInfo;
  ppwszTip: PWideChar = nil;
  pchEaten: ULONG;
  dwAttributes: ULONG = 0;
  wsTemp: WideString;
begin
  Result:= EmptyStr;
  if Succeeded(SHGetDesktopFolder(DesktopFolder)) then
    try
      wsTemp:= UTF8Decode(sFilePath);
      if Succeeded(DesktopFolder.ParseDisplayName(0, nil, PWideChar(wsTemp), pchEaten, pidlFolder, dwAttributes)) then
        if Succeeded(DesktopFolder.BindToObject(pidlFolder, nil, IID_IShellFolder, Folder)) then
          try
            wsTemp:= UTF8Decode(sFileName);
            if Succeeded(Folder.ParseDisplayName(0, nil, PWideChar(wsTemp), pchEaten, pidlFile, dwAttributes)) then
              if Succeeded(Folder.GetUIObjectOf(0, 1, pidlFile, IID_IQueryInfo, nil, queryInfo)) then
                if Succeeded(queryInfo.GetInfoTip(QITIPF_USESLOWTIP, ppwszTip)) then
                  Result:= UTF8Encode(WideString(ppwszTip));
          finally
            Folder:= nil;
            queryInfo:= nil;
            if Assigned(ppwszTip) then
              CoTaskMemFree(ppwszTip);
            if Assigned(pidlFile) then
              CoTaskMemFree(pidlFile);
          end;
    finally
      DesktopFolder:= nil;
      if Assigned(pidlFolder) then
        CoTaskMemFree(pidlFolder);
    end;
end;

procedure OleErrorUTF8(ErrorCode: HResult);
begin
  raise EOleError.Create(UTF8Encode(SysErrorMessage(ErrorCode)));
end;

procedure OleCheckUTF8(Result: HResult);
begin
  if not Succeeded(Result) then OleErrorUTF8(Result);
end;

end. { ShlObjAdditional }
