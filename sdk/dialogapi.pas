unit DialogAPI;

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

type
  { Dialog window callback function }
  TDlgProc = function(pDlg: PtrUInt; DlgItemName: PAnsiChar; Msg, wParam, lParam: PtrInt): PtrInt; stdcall;
  { Definition of callback functions called by the DLL }
  TInputBoxProc = function(Caption, Prompt: PWideChar; MaskInput: LongBool; Value: PWideChar; ValueMaxLen: Integer): LongBool; stdcall;
  TMessageBoxProc = function(Text, Caption: PWideChar; Flags: Longint): Integer; stdcall;
  TDialogBoxLFMProc = function(LFMData: Pointer; DataSize: LongWord; DlgProc: TDlgProc): LongBool; stdcall;
  TDialogBoxLRSProc = function(LRSData: Pointer; DataSize: LongWord; DlgProc: TDlgProc): LongBool; stdcall;
  TDialogBoxLFMFileProc = function(lfmFileName: PWideChar; DlgProc: TDlgProc): LongBool; stdcall;

type
  TSetDlgProcInfo = packed record
    PluginDir: PWideChar;
    PluginConfDir: PWideChar;
    InputBox: TInputBoxProc;
    MessageBox: TMessageBoxProc;
    DialogBoxLFM: TDialogBoxLFMProc;
    DialogBoxLRS: TDialogBoxLRSProc;
    DialogBoxLFMFile: TDialogBoxLFMFileProc;
    SendDlgMsg: TDlgProc;
  end;

type
   TSetDlgProc = procedure(var SetDlgProcInfo: TSetDlgProcInfo);stdcall;

implementation

{ Plugin must implement this function for working with Dialog API

procedure SetDlgProc(var SetDlgProcInfo: TSetDlgProcInfo);stdcall;

}

end.

