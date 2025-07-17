local export =
{
  DM_CLOSE = 1, -- A signal that the dialog is about to close
  DM_ENABLE = 2,
  DM_GETDLGDATA = 3,
  DM_GETDLGBOUNDS = 4,
  DM_GETITEMBOUNDS = 5,
  DM_GETTEXT = 6, -- Retrieve the text of an edit string or the caption of an item
  DM_KEYDOWN = 7,
  DM_KEYUP = 8,
  DM_SETDLGDATA = 9,
  DM_SETFOCUS = 10, -- Set the keyboard focus to the given dialog item
  DM_REDRAW = 11, -- Redraw the whole dialog
  DM_SETTEXT = 12, -- Set a new string value for an edit line or a new caption for an item
  DM_SETMAXTEXTLENGTH = 13, -- Set the maximum length of an edit string
  DM_SHOWDIALOG = 14, -- Show/hide the dialog window
  DM_SHOWITEM = 15, -- Show/hide a dialog item
  DM_GETCHECK = 16, -- Retrieve the state of TCheckBox or TRadioButton items
  DM_SETCHECK = 17, -- Change the state of TCheckBox and TRadioButton items
  DM_LISTGETITEM = 18, -- Retrieve a list item
  DM_LISTGETITEMINDEX = 19, -- Get current item index in a list
  DM_LISTSETITEMINDEX = 20, -- Set current item index in a list
  DM_LISTDELETE = 21,
  DM_LISTADD = 22,
  DM_LISTADDSTR = 23,
  DM_LISTUPDATE = 24,
  DM_LISTINSERT = 25,
  DM_LISTINDEXOF = 26,
  DM_LISTGETCOUNT = 27,
  DM_LISTGETDATA = 28,
  DM_LISTSETDATA = 29,
  DM_SETDLGBOUNDS = 30,
  DM_SETITEMBOUNDS = 31,
  DM_GETDROPPEDDOWN = 32,
  DM_SETDROPPEDDOWN = 33,
  DM_GETITEMDATA = 34,
  DM_SETITEMDATA = 35,
  DM_LISTSET = 36,
  DM_SETPROGRESSVALUE = 37,
  DM_SETPROGRESSSTYLE = 38,
  DM_SETPASSWORDCHAR = 39,
  DM_LISTCLEAR = 40,
  DM_TIMERSETINTERVAL = 41,

  -- events messages
  DN_CLICK = 0x1001, -- Sent after mouse click
  DN_DBLCLICK = 0x1002, -- Sent after mouse double click
  DN_CHANGE = 0x1003, -- Sent after the dialog item is changed
  DN_GOTFOCUS = 0x1004, -- Sent when the dialog item gets input focus
  DN_INITDIALOG = 0x1005, -- Sent before showing the dialog
  DN_KILLFOCUS = 0x1006, -- Sent before a dialog item loses the input focus
  DN_TIMER = 0x1007, -- Sent when a timer expires

  DN_KEYDOWN = 7,
  DN_KEYUP = 8,
  DN_CLOSE = 1, -- Sent before the dialog is closed

  DM_USER = 0x4000, -- Starting value for user defined messages

  -- MessageBox: To indicate the buttons displayed in the message box,
  -- specify one of the following values.
  MB_OK = 0x00000000,
  MB_OKCANCEL = 0x00000001,
  MB_ABORTRETRYIGNORE = 0x00000002,
  MB_YESNOCANCEL = 0x00000003,
  MB_YESNO = 0x00000004,
  MB_RETRYCANCEL = 0x00000005,
  MB_ICONHAND = 0x00000010,
  MB_ICONQUESTION = 0x00000020,
  MB_ICONEXCLAMATION = 0x00000030,
  MB_ICONASTERICK = 0x00000040,
  MB_ICONWARNING = 0x00000030,
  MB_ICONERROR = 0x00000010,
  MB_ICONSTOP = 0x00000010,
  MB_ICONINFORMATION = 0x00000040,
  -- MessageBox: To indicate the default button, specify one of the following values.
  MB_DEFBUTTON1 = 0x00000000,
  MB_DEFBUTTON2 = 0x00000100,
  MB_DEFBUTTON3 = 0x00000200,
  MB_DEFBUTTON4 = 0x00000300,
  -- MessageBox: Return values
  ID_OK = 1,
  ID_CANCEL = 2,
  ID_ABORT = 3,
  ID_RETRY = 4,
  ID_IGNORE = 5,
  ID_YES = 6,
  ID_NO = 7,
  ID_CLOSE = 8,
  ID_HELP = 9,

  -- Set/Get Property
  TK_STRING = 1,
  TK_FLOAT = 2,
  TK_INT32 = 3,
  TK_INT64 = 4,
  TK_BOOL = 5,

  -- LogWrite
  lmsgInfo = 0,
  lmsgSuccess = 1,
  lmsgError = 2,
  -- Active panel
  apLeft = 0,
  apRight = 1,

  -- Mask options
  moCaseSensitive = 1,
  moIgnoreAccents = 2,
  moWindowsMask = 4,
  moPinyin = 8,

  -- Attr
  faReadOnly = 0x00000001, -- The file is read-only.
  faHidden = 0x00000002, -- The file is hidden. In Unix/Linux, this means that the filename starts with a dot.
  faSysFile = 0x00000004, -- The file is a system file. In Unix/Linux, this means that the file is a character, block or FIFO file.
  faVolumeId = 0x00000008, -- Volume Label. Only for DOS/Windows on a plain FAT (not VFAT or FAT32) filesystem.
  faDirectory = 0x00000010, -- File is a directory.
  faArchive = 0x00000020, -- File is archived. Not possible in Unix/Linux.
  faSymLink = 0x00000400, -- File is a symbolic link.

  -- File Properties
  fsfSize = 0,
  fsfAttr = 1,
  fsfGroup = 2,
  fsfOwner = 3,
  fsfModificationTime = 4,
  fsfCreationTime = 5,
  fsfLastAccessTime = 6,
  fsfChangeTime = 7,
  fsfType = 8,
  fsfComment = 9,

  -- WDX

  ft_nomorefields = 0,
  ft_numeric_32, 1,
  ft_numeric_64, 2,
  ft_numeric_floating = 3,
  ft_boolean = 6,
  ft_multiplechoice = 7,
  ft_string = 8,
  ft_fulltext = 9,
  ft_datetime = 10,

  CONTENT_DELAYIFSLOW = 1, -- ContentGetValue called in foreground
  CONTENT_PASSTHROUGH = 2, -- If requested via contflags_passthrough_size_float: The size is passed in as floating value, TC expects correct value m from the given units value, and optionally a text string
}

return export
