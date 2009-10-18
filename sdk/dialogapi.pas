unit DialogAPI;

interface

const
  // dialog messages
  DM_FIRST                = 0;
  DM_CLOSE                = DM_FIRST+1;
  DM_ENABLE               = DM_FIRST+2;
  DM_GETDLGDATA           = DM_FIRST+3;
  DM_GETDLGBOUNDS         = DM_FIRST+4;
  DM_GETITEMBOUNDS        = DM_FIRST+5;
  DM_GETTEXT              = DM_FIRST+6;
  DM_KEYDOWN              = DM_FIRST+7;
  DM_KEYUP                = DM_FIRST+8;
  DM_SETDLGDATA           = DM_FIRST+9;
  DM_SETFOCUS             = DM_FIRST+10;
  DM_REDRAW               = DM_FIRST+11;
  DM_SETTEXT              = DM_FIRST+12;
  DM_SETMAXTEXTLENGTH     = DM_FIRST+13;
  DM_SHOWDIALOG           = DM_FIRST+14;
  DM_SHOWITEM             = DM_FIRST+15;
  DM_GETCHECK             = DM_FIRST+16;
  DM_SETCHECK             = DM_FIRST+17;
  DM_LISTGETITEM          = DM_FIRST+18;
  DM_LISTGETITEMINDEX     = DM_FIRST+19;
  DM_LISTSETITEMINDEX     = DM_FIRST+20;
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
  DN_CLICK                = DN_FIRST+1;
  DN_DBLCLICK             = DN_FIRST+2;
  DN_CHANGE               = DN_FIRST+3;
  DN_GOTFOCUS             = DN_FIRST+4;
  DN_INITDIALOG           = DN_FIRST+5;
  DN_KILLFOCUS            = DN_FIRST+6;

  DN_KEYDOWN              = DM_KEYDOWN;
  DN_KEYUP                = DM_KEYUP;
  DN_CLOSE                = DM_CLOSE;

  DM_USER                 = $4000;
type
  { Dialog window callback function }
  TDlgProc = function(pDlg: PtrUInt; DlgItemName: PChar; Msg, wParam, lParam: PtrInt): PtrInt; stdcall;
  { Definition of callback functions called by the DLL }
  TInputBoxProc = function(Caption, Prompt, DefaultText : PWideChar): PWideChar;stdcall;
  TMessageBoxProc = function(Text, Caption: PWideChar; Flags: Longint): Integer;stdcall;
  TDialogBoxProc = function(DlgData: PWideChar; DlgProc: TDlgProc): Boolean;stdcall;
  TDialogBoxExProc = function(lfmFileName: PWideChar; DlgProc: TDlgProc): Boolean;stdcall;

type
  TSetDlgProcInfo = packed record
    PluginDir: PWideChar;
    PluginConfDir: PWideChar;
    InputBox: TInputBoxProc;
    MessageBox: TMessageBoxProc;
    DialogBox: TDialogBoxProc;
    DialogBoxEx: TDialogBoxExProc;
    SendDlgMsg: TDlgProc;
  end;

type
   TSetDlgProc = procedure(var SetDlgProcInfo: TSetDlgProcInfo);stdcall;

implementation

{ Plugin must implement this function for working with Dialog API

procedure SetDlgProc(var SetDlgProcInfo: TSetDlgProcInfo);stdcall;

}

end.

