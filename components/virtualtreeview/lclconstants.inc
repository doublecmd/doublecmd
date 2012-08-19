// Message constants that are not defined in LCL

  WM_APP  = $8000;

// ExtTextOut Options

  ETO_RTLREADING = 128;

//DrawText options

  DT_RTLREADING = 131072;

// Clipboard constants

  CF_BITMAP = 2;
  CF_DIB = 8;
  CF_PALETTE = 9;
  CF_ENHMETAFILE = 14;
  CF_METAFILEPICT = 3;
  CF_OEMTEXT = 7;
  CF_TEXT = 1;
  CF_UNICODETEXT = 13;
  CF_DIF = 5;
  CF_DSPBITMAP = 130;
  CF_DSPENHMETAFILE = 142;
  CF_DSPMETAFILEPICT = 131;
  CF_DSPTEXT = 129;
  CF_GDIOBJFIRST = 768;
  CF_GDIOBJLAST = 1023;
  CF_HDROP = 15;
  CF_LOCALE = 16;
  CF_OWNERDISPLAY = 128;
  CF_PENDATA = 10;
  CF_PRIVATEFIRST = 512;
  CF_PRIVATELAST = 767;
  CF_RIFF = 11;
  CF_SYLK = 4;
  CF_WAVE = 12;
  CF_TIFF = 6;
  CF_MAX = 17;
  
// Win32 colors
  CLR_NONE = $ffffffff;
  CLR_DEFAULT = $ff000000;

//DrawFrameControl constants
  DFCS_HOT = $1000;
  
//Thread support
  //This values is for win32, how about others??
  INFINITE = $FFFFFFFF;
  
//OLE Support
  E_OUTOFMEMORY                                = HRESULT($8007000E);
  E_INVALIDARG                                 = HRESULT($80070057);
  E_NOINTERFACE                                = HRESULT($80004002);
  E_POINTER                                    = HRESULT($80004003);
  E_HANDLE                                     = HRESULT($80070006);
  E_ABORT                                      = HRESULT($80004004);
  E_FAIL                                       = HRESULT($80004005);
  E_ACCESSDENIED                               = HRESULT($80070005);
  
  DV_E_TYMED                                   = HRESULT($80040069);
  DV_E_CLIPFORMAT                              = HRESULT($8004006A);
  DV_E_LINDEX                                  = HRESULT($80040068);
  DV_E_DVASPECT                                = HRESULT($8004006B);
  
  OLE_E_ADVISENOTSUPPORTED                     = HRESULT($80040003);
  OLE_S_USEREG                                 = HRESULT($00040000);
  
  DATA_S_SAMEFORMATETC                         = HRESULT($00040130);
  
  DRAGDROP_S_DROP                              = HRESULT($00040100);
  DRAGDROP_S_CANCEL                            = HRESULT($00040101);
  DRAGDROP_S_USEDEFAULTCURSORS                 = HRESULT($00040102);
  
  NOERROR = 0;
  
  SPI_GETDRAGFULLWINDOWS = 38;
  
// windows management
  SWP_HIDEWINDOW  = 128;
  SWP_SHOWWINDOW = 64;
  
//Imagelists
  ILD_NORMAL = 0;
  
// Set WindowPos
  SWP_FRAMECHANGED = 32;
  SWP_NOOWNERZORDER = 512;
  SWP_NOSENDCHANGING = 1024;
  
 { RedrawWindow  }
  RDW_ERASE = 4;
  RDW_FRAME = 1024;
  RDW_INTERNALPAINT = 2;
  RDW_INVALIDATE = 1;
  RDW_NOERASE = 32;
  RDW_NOFRAME = 2048;
  RDW_NOINTERNALPAINT = 16;
  RDW_VALIDATE = 8;
  RDW_ERASENOW = 512;
  RDW_UPDATENOW = 256;
  RDW_ALLCHILDREN = 128;
  RDW_NOCHILDREN = 64;
  
//SetRedraw
  WM_SETREDRAW = 11;
  
//Dummy
  CM_PARENTFONTCHANGED = 1999;
  
//Wheel
  WHEEL_DELTA = 120;
  WHEEL_PAGESCROLL = High(DWord);
  SPI_GETWHEELSCROLLLINES = 104;
  
//MultiByte
  MB_USEGLYPHCHARS = 4;
  LOCALE_IDEFAULTANSICODEPAGE = 4100;
  
//Image list
  ILD_TRANSPARENT                = $00000001;
  ILD_MASK                       = $00000010;
  ILD_IMAGE                      = $00000020;

  ILD_ROP                        = $00000040;

  ILD_BLEND25                    = $00000002;
  ILD_BLEND50                    = $00000004;
  ILD_OVERLAYMASK                = $00000F00;
  
  { GetDCEx  }
  DCX_WINDOW = $1;
  DCX_CACHE = $2;
  DCX_PARENTCLIP = $20;
  DCX_CLIPSIBLINGS = $10;
  DCX_CLIPCHILDREN = $8;
  DCX_NORESETATTRS = $4;
  DCX_LOCKWINDOWUPDATE = $400;
  DCX_EXCLUDERGN = $40;
  DCX_INTERSECTRGN = $80;
  DCX_VALIDATE = $200000;
  
  SCantWriteResourceStreamError = 'CantWriteResourceStreamError';
  
  //command
  EN_UPDATE = 1024;
  
  ES_AUTOHSCROLL = $80;
  ES_AUTOVSCROLL = $40;
  ES_CENTER = $1;
  ES_LEFT = 0;
  ES_LOWERCASE = $10;
  ES_MULTILINE = $4;
  ES_NOHIDESEL = $100;
  
  
  EM_SETRECTNP = 180;
  
  DT_END_ELLIPSIS = 32768;
