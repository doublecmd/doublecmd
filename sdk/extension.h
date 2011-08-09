/* dialog messages */
#define DM_FIRST 0
#define DM_CLOSE DM_FIRST+1 /* A signal that the dialog is about to close */
#define DM_ENABLE DM_FIRST+2
#define DM_GETDLGDATA DM_FIRST+3
#define DM_GETDLGBOUNDS DM_FIRST+4
#define DM_GETITEMBOUNDS DM_FIRST+5
#define DM_GETTEXT DM_FIRST+6 /* Retrieve the text of an edit string or the caption of an item */
#define DM_KEYDOWN DM_FIRST+7
#define DM_KEYUP DM_FIRST+8
#define DM_SETDLGDATA DM_FIRST+9
#define DM_SETFOCUS DM_FIRST+10 /* Set the keyboard focus to the given dialog item */
#define DM_REDRAW DM_FIRST+11 /* Redraw the whole dialog */
#define DM_SETTEXT DM_FIRST+12 /* Set a new string value for an edit line or a new caption for an item */
#define DM_SETMAXTEXTLENGTH DM_FIRST+13 /* Set the maximum length of an edit string */
#define DM_SHOWDIALOG DM_FIRST+14 /* Show/hide the dialog window */
#define DM_SHOWITEM DM_FIRST+15 /* Show/hide a dialog item */
#define DM_GETCHECK DM_FIRST+16 /* Retrieve the state of TCheckBox or TRadioButton items */
#define DM_SETCHECK DM_FIRST+17 /* Change the state of TCheckBox and TRadioButton items */
#define DM_LISTGETITEM DM_FIRST+18 /* Retrieve a list item */
#define DM_LISTGETITEMINDEX DM_FIRST+19 /* Get current item index in a list */
#define DM_LISTSETITEMINDEX DM_FIRST+20 /* Set current item index in a list */
#define DM_LISTDELETE DM_FIRST+21
#define DM_LISTADD DM_FIRST+22
#define DM_LISTADDSTR DM_FIRST+23
#define DM_LISTUPDATE DM_FIRST+24
#define DM_LISTINSERT DM_FIRST+25
#define DM_LISTINDEXOF DM_FIRST+26
#define DM_LISTGETCOUNT DM_FIRST+27
#define DM_LISTGETDATA DM_FIRST+28
#define DM_LISTSETDATA DM_FIRST+29
#define DM_SETDLGBOUNDS DM_FIRST+30
#define DM_SETITEMBOUNDS DM_FIRST+31
#define DM_GETDROPPEDDOWN DM_FIRST+32
#define DM_SETDROPPEDDOWN DM_FIRST+33
#define DM_GETITEMDATA DM_FIRST+34
#define DM_SETITEMDATA DM_FIRST+35
#define DM_LISTSET DM_FIRST+36

/* events messages */
#define DN_FIRST 0x1000
#define DN_CLICK DN_FIRST+1 /* Sent after mouse click */
#define DN_DBLCLICK DN_FIRST+2 /* Sent after mouse double click */
#define DN_CHANGE DN_FIRST+3 /* Sent after the dialog item is changed */
#define DN_GOTFOCUS DN_FIRST+4 /* Sent when the dialog item gets input focus */
#define DN_INITDIALOG DN_FIRST+5 /* Sent before showing the dialog */
#define DN_KILLFOCUS DN_FIRST+6 /* Sent before a dialog item loses the input focus */

#define DN_KEYDOWN DM_KEYDOWN
#define DN_KEYUP DM_KEYUP
#define DN_CLOSE DM_CLOSE /* Sent before the dialog is closed */

#define DM_USER 0x4000 /* Starting value for user defined messages */

/* other */
#define EXT_MAX_PATH  16384 /* 16 Kb */

/* Dialog window callback function */
typedef intptr_t (__stdcall *tDlgProc)(uintptr_t pDlg, char* DlgItemName, intptr_t Msg, intptr_t wParam, intptr_t lParam);
/* Definition of callback functions called by the DLL */
typedef BOOL (__stdcall *tInputBoxProc)(char* Caption, char* Prompt, BOOL MaskInput, char* Value, int ValueMaxLen);
typedef int (__stdcall *tMessageBoxProc)(char* Text, char* Caption, long Flags);
typedef BOOL (__stdcall *tDialogBoxLFMProc)(intptr_t LFMData, unsigned long DataSize, tDlgProc DlgProc);
typedef BOOL (__stdcall *tDialogBoxLRSProc)(intptr_t LRSData, unsigned long DataSize, tDlgProc DlgProc);
typedef BOOL (__stdcall *tDialogBoxLFMFileProc)(char* LFMFileName, tDlgProc DlgProc);


#pragma pack(push)
#pragma pack(1)
typedef struct {
unsigned long StructSize;
char PluginDir[EXT_MAX_PATH];
char PluginConfDir[EXT_MAX_PATH];
tInputBoxProc InputBox;
tMessageBoxProc MessageBox;
tDialogBoxLFMProc DialogBoxLFM;
tDialogBoxLRSProc DialogBoxLRS;
tDialogBoxLFMFileProc DialogBoxLFMFile;
tDlgProc SendDlgMsg;
unsigned char Reserved[4096 * sizeof(void *)];
} tExtensionStartupInfo;
#pragma pack(pop)

typedef void (__stdcall tExtensionInitializeProc)(tExtensionStartupInfo* StartupInfo);
typedef void (__stdcall tExtensionFinalizeProc)(void* Reserved);

/* Plugin must implement this function for working with Extension API

void __stdcall ExtensionInitialize(tExtensionStartupInfo* StartupInfo);

void __stdcall ExtensionFinalize(void* Reserved);

*/
