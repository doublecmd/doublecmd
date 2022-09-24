unit Extension;

interface

const
  // dialog messages
  DM_FIRST                = 0;
  DM_CLOSE                = DM_FIRST+1; // A signal that the dialog is about to close
  DM_ENABLE               = DM_FIRST+2;
  DM_GETDLGDATA           = DM_FIRST+3;
  DM_GETDLGBOUNDS         = DM_FIRST+4;
  DM_GETITEMBOUNDS        = DM_FIRST+5;
  DM_GETTEXT              = DM_FIRST+6; // Retrieve the text of an edit string or the caption of an item
  DM_KEYDOWN              = DM_FIRST+7;
  DM_KEYUP                = DM_FIRST+8;
  DM_SETDLGDATA           = DM_FIRST+9;
  DM_SETFOCUS             = DM_FIRST+10; // Set the keyboard focus to the given dialog item
  DM_REDRAW               = DM_FIRST+11; // Redraw the whole dialog
  DM_SETTEXT              = DM_FIRST+12; // Set a new string value for an edit line or a new caption for an item
  DM_SETMAXTEXTLENGTH     = DM_FIRST+13; // Set the maximum length of an edit string
  DM_SHOWDIALOG           = DM_FIRST+14; // Show/hide the dialog window
  DM_SHOWITEM             = DM_FIRST+15; // Show/hide a dialog item
  DM_GETCHECK             = DM_FIRST+16; // Retrieve the state of TCheckBox or TRadioButton items
  DM_SETCHECK             = DM_FIRST+17; // Change the state of TCheckBox and TRadioButton items
  DM_LISTGETITEM          = DM_FIRST+18; // Retrieve a list item
  DM_LISTGETITEMINDEX     = DM_FIRST+19; // Get current item index in a list
  DM_LISTSETITEMINDEX     = DM_FIRST+20; // Set current item index in a list
  DM_LISTDELETE           = DM_FIRST+21;
  DM_LISTADD              = DM_FIRST+22;
  DM_LISTADDSTR           = DM_FIRST+23;
  DM_LISTUPDATE           = DM_FIRST+24;
  DM_LISTINSERT           = DM_FIRST+25;
  DM_LISTINDEXOF          = DM_FIRST+26;
  DM_LISTGETCOUNT         = DM_FIRST+27;
  DM_LISTGETDATA          = DM_FIRST+28;
  DM_LISTSETDATA          = DM_FIRST+29;
  DM_SETDLGBOUNDS         = DM_FIRST+30;
  DM_SETITEMBOUNDS        = DM_FIRST+31;
  DM_GETDROPPEDDOWN       = DM_FIRST+32;
  DM_SETDROPPEDDOWN       = DM_FIRST+33;
  DM_GETITEMDATA          = DM_FIRST+34;
  DM_SETITEMDATA          = DM_FIRST+35;
  DM_LISTSET              = DM_FIRST+36;
  DM_SETPROGRESSVALUE     = DM_FIRST+37;
  DM_SETPROGRESSSTYLE     = DM_FIRST+38;
  DM_SETPASSWORDCHAR      = DM_FIRST+39;

  // events messages
  DN_FIRST                = $1000;
  DN_CLICK                = DN_FIRST+1; // Sent after mouse click
  DN_DBLCLICK             = DN_FIRST+2; // Sent after mouse double click
  DN_CHANGE               = DN_FIRST+3; // Sent after the dialog item is changed
  DN_GOTFOCUS             = DN_FIRST+4; // Sent when the dialog item gets input focus
  DN_INITDIALOG           = DN_FIRST+5; // Sent before showing the dialog
  DN_KILLFOCUS            = DN_FIRST+6; // Sent before a dialog item loses the input focus

  DN_KEYDOWN              = DM_KEYDOWN;
  DN_KEYUP                = DM_KEYUP;
  DN_CLOSE                = DM_CLOSE; // Sent before the dialog is closed

  DM_USER                 = $4000; // Starting value for user defined messages

const
  // MessageBox: To indicate the buttons displayed in the message box,
  // specify one of the following values.
  MB_OK                   = $00000000;
  MB_OKCANCEL             = $00000001;
  MB_ABORTRETRYIGNORE     = $00000002;
  MB_YESNOCANCEL          = $00000003;
  MB_YESNO                = $00000004;
  MB_RETRYCANCEL          = $00000005;
  MB_ICONHAND             = $00000010;
  MB_ICONQUESTION         = $00000020;
  MB_ICONEXCLAMATION      = $00000030;
  MB_ICONASTERICK         = $00000040;
  MB_ICONWARNING          = MB_ICONEXCLAMATION;
  MB_ICONERROR            = MB_ICONHAND;
  MB_ICONSTOP             = MB_ICONHAND;
  MB_ICONINFORMATION      = MB_ICONASTERICK;
  // MessageBox: To indicate the default button, specify one of the following values.
  MB_DEFBUTTON1           = $00000000;
  MB_DEFBUTTON2           = $00000100;
  MB_DEFBUTTON3           = $00000200;
  MB_DEFBUTTON4           = $00000300;
  // MessageBox: Return values
  ID_OK         = 1;
  ID_CANCEL     = 2;
  ID_ABORT      = 3;
  ID_RETRY      = 4;
  ID_IGNORE     = 5;
  ID_YES        = 6;
  ID_NO         = 7;
  ID_CLOSE      = 8;
  ID_HELP       = 9;

const
  EXT_MAX_PATH = 16384; // 16 Kb

{ For compatibility with Delphi use $IFDEF's to set calling convention }

type
  { Dialog window callback function }
  TDlgProc = function(pDlg: PtrUInt; DlgItemName: PAnsiChar; Msg, wParam, lParam: PtrInt): PtrInt; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  { Definition of callback functions called by the DLL }
  TInputBoxProc = function(Caption, Prompt: PAnsiChar; MaskInput: LongBool; Value: PAnsiChar; ValueMaxLen: Integer): LongBool; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  TMessageBoxProc = function(Text, Caption: PAnsiChar; Flags: Longint): Integer; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  TDialogBoxLFMProc = function(LFMData: Pointer; DataSize: LongWord; DlgProc: TDlgProc): LongBool; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  TDialogBoxLRSProc = function(LRSData: Pointer; DataSize: LongWord; DlgProc: TDlgProc): LongBool; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  TDialogBoxLFMFileProc = function(lfmFileName: PAnsiChar; DlgProc: TDlgProc): LongBool; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

type
  PExtensionStartupInfo = ^TExtensionStartupInfo;
  TExtensionStartupInfo = packed record
    // The size of the structure, in bytes
    StructSize: LongWord;
    // Directory where plugin is located (UTF-8 encoded)
    PluginDir: packed array [0..Pred(EXT_MAX_PATH)] of AnsiChar;
    // Directory where plugin configuration file must be located (UTF-8 encoded)
    PluginConfDir: packed array [0..Pred(EXT_MAX_PATH)] of AnsiChar;
    // Dialog API
    InputBox: TInputBoxProc;
    MessageBox: TMessageBoxProc;
    DialogBoxLFM: TDialogBoxLFMProc;
    DialogBoxLRS: TDialogBoxLRSProc;
    DialogBoxLFMFile: TDialogBoxLFMFileProc;
    SendDlgMsg: TDlgProc;
    // Reserved for future API extension
    Reserved: packed array [0..Pred(4096 * SizeOf(Pointer))] of Byte;
  end;

type
  TExtensionInitializeProc = procedure(StartupInfo: PExtensionStartupInfo); {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  TExtensionFinalizeProc   = procedure(Reserved: Pointer); {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

implementation

(* Plugin must implement this function for working with Extension API

procedure ExtensionInitialize(StartupInfo: PExtensionStartupInfo); {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

procedure ExtensionFinalize(Reserved: Pointer); {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

*)

end.

