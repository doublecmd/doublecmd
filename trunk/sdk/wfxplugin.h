#ifdef __GNUC__

#ifndef __stdcall
#define __stdcall __attribute__((stdcall))
#endif

#define MAX_PATH 260

typedef unsigned long DWORD;
typedef unsigned short WORD;
typedef void *HANDLE;
typedef HANDLE HICON;
typedef HANDLE HBITMAP;
typedef HANDLE HWND;
typedef int BOOL;
typedef char CHAR;
typedef wchar_t WCHAR; 

typedef struct _FILETIME {
	DWORD dwLowDateTime;
	DWORD dwHighDateTime;
} FILETIME,*PFILETIME,*LPFILETIME;

typedef struct _WIN32_FIND_DATAA {
	DWORD dwFileAttributes;
	FILETIME ftCreationTime;
	FILETIME ftLastAccessTime;
	FILETIME ftLastWriteTime;
	DWORD nFileSizeHigh;
	DWORD nFileSizeLow;
	DWORD dwReserved0;
	DWORD dwReserved1;
	CHAR cFileName[MAX_PATH];
	CHAR cAlternateFileName[14];
} WIN32_FIND_DATAA,*LPWIN32_FIND_DATAA;

typedef struct _WIN32_FIND_DATAW {
	DWORD dwFileAttributes;
	FILETIME ftCreationTime;
	FILETIME ftLastAccessTime;
	FILETIME ftLastWriteTime;
	DWORD nFileSizeHigh;
	DWORD nFileSizeLow;
	DWORD dwReserved0;
	DWORD dwReserved1;
	WCHAR cFileName[MAX_PATH];
	WCHAR cAlternateFileName[14];
} WIN32_FIND_DATAW,*LPWIN32_FIND_DATAW;

#endif

// contents of fsplugin.h  version 2.0 (30.Jan.2009)

// ids for FsGetFile

#define FS_FILE_OK 0

#define FS_FILE_EXISTS 1

#define FS_FILE_NOTFOUND 2

#define FS_FILE_READERROR 3

#define FS_FILE_WRITEERROR 4

#define FS_FILE_USERABORT 5

#define FS_FILE_NOTSUPPORTED 6

#define FS_FILE_EXISTSRESUMEALLOWED 7

#define FS_EXEC_OK 0

#define FS_EXEC_ERROR 1

#define FS_EXEC_YOURSELF -1

#define FS_EXEC_SYMLINK -2

#define FS_COPYFLAGS_OVERWRITE 1

#define FS_COPYFLAGS_RESUME 2

#define FS_COPYFLAGS_MOVE 4

#define FS_COPYFLAGS_EXISTS_SAMECASE 8

#define FS_COPYFLAGS_EXISTS_DIFFERENTCASE 16

 

// flags for tRequestProc

#define RT_Other 0

#define RT_UserName 1

#define RT_Password 2

#define RT_Account 3

#define RT_UserNameFirewall 4

#define RT_PasswordFirewall 5

#define RT_TargetDir 6

#define RT_URL 7

#define RT_MsgOK 8

#define RT_MsgYesNo 9

#define RT_MsgOKCancel 10

// flags for tLogProc

#define MSGTYPE_CONNECT 1

#define MSGTYPE_DISCONNECT 2

#define MSGTYPE_DETAILS 3

#define MSGTYPE_TRANSFERCOMPLETE 4

#define MSGTYPE_CONNECTCOMPLETE 5

#define MSGTYPE_IMPORTANTERROR 6

#define MSGTYPE_OPERATIONCOMPLETE 7

// flags for FsStatusInfo

#define FS_STATUS_START 0

#define FS_STATUS_END 1

#define FS_STATUS_OP_LIST 1

#define FS_STATUS_OP_GET_SINGLE 2

#define FS_STATUS_OP_GET_MULTI 3

#define FS_STATUS_OP_PUT_SINGLE 4

#define FS_STATUS_OP_PUT_MULTI 5

#define FS_STATUS_OP_RENMOV_SINGLE 6

#define FS_STATUS_OP_RENMOV_MULTI 7

#define FS_STATUS_OP_DELETE 8

#define FS_STATUS_OP_ATTRIB 9

#define FS_STATUS_OP_MKDIR 10

#define FS_STATUS_OP_EXEC 11

#define FS_STATUS_OP_CALCSIZE 12

#define FS_STATUS_OP_SEARCH 13

#define FS_STATUS_OP_SEARCH_TEXT 14

#define FS_STATUS_OP_SYNC_SEARCH 15

#define FS_STATUS_OP_SYNC_GET 16

#define FS_STATUS_OP_SYNC_PUT 17

#define FS_STATUS_OP_SYNC_DELETE 18

#define FS_ICONFLAG_SMALL 1

#define FS_ICONFLAG_BACKGROUND 2

#define FS_ICON_USEDEFAULT 0

#define FS_ICON_EXTRACTED 1

#define FS_ICON_EXTRACTED_DESTROY 2

#define FS_ICON_DELAYED 3

#define FS_BITMAP_NONE 0

#define FS_BITMAP_EXTRACTED 1

#define FS_BITMAP_EXTRACT_YOURSELF 2

#define FS_BITMAP_EXTRACT_YOURSELF_ANDDELETE 3

#define FS_BITMAP_CACHE 256

#define FS_CRYPT_SAVE_PASSWORD 1

#define FS_CRYPT_LOAD_PASSWORD 2

#define FS_CRYPT_LOAD_PASSWORD_NO_UI 3 // Load password only if master password has already been entered!

#define FS_CRYPT_COPY_PASSWORD 4       // Copy encrypted password to new connection name

#define FS_CRYPT_MOVE_PASSWORD 5       // Move password when renaming a connection

#define FS_CRYPT_DELETE_PASSWORD 6     // Delete password

#define FS_CRYPTOPT_MASTERPASS_SET 1   // The user already has a master password defined

typedef struct {

    DWORD SizeLow,SizeHigh;

    FILETIME LastWriteTime;

    int Attr;

} RemoteInfoStruct;

typedef struct {

int size;
	DWORD PluginInterfaceVersionLow;
	DWORD PluginInterfaceVersionHi;
	char DefaultIniName[MAX_PATH];
} FsDefaultParamStruct;

// callback functions
typedef int (__stdcall *tProgressProc)(int PluginNr,char* SourceName,
             char* TargetName,int PercentDone);
typedef int (__stdcall *tProgressProcW)(int PluginNr,WCHAR* SourceName,
             WCHAR* TargetName,int PercentDone);
typedef void (__stdcall *tLogProc)(int PluginNr,int MsgType,char* LogString);
typedef void (__stdcall *tLogProcW)(int PluginNr,int MsgType,WCHAR* LogString);

typedef BOOL (__stdcall *tRequestProc)(int PluginNr,int RequestType,char* CustomTitle,
              char* CustomText,char* ReturnedText,int maxlen);
typedef BOOL (__stdcall *tRequestProcW)(int PluginNr,int RequestType,WCHAR* CustomTitle,
              WCHAR* CustomText,WCHAR* ReturnedText,int maxlen);
typedef int (__stdcall *tCryptProc)(int PluginNr,int CryptoNr,int Mode,
			  char* ConnectionName,char* Password,int maxlen);
typedef int (__stdcall *tCryptProcW)(int PluginNr,int CryptoNr,int Mode,
			  WCHAR* ConnectionName,WCHAR* Password,int maxlen);

// Function prototypes
int __stdcall FsInit(int PluginNr,tProgressProc pProgressProc,
                     tLogProc pLogProc,tRequestProc pRequestProc);
int __stdcall FsInitW(int PluginNr,tProgressProcW pProgressProcW,
                     tLogProcW pLogProcW,tRequestProcW pRequestProcW);
void __stdcall FsSetCryptCallback(tCryptProc pCryptProc,int CryptoNr,int Flags);
void __stdcall FsSetCryptCallbackW(tCryptProcW pCryptProcW,int CryptoNr,int Flags);
HANDLE __stdcall FsFindFirst(char* Path,WIN32_FIND_DATAA *FindData);
HANDLE __stdcall FsFindFirstW(WCHAR* Path,WIN32_FIND_DATAW *FindData);

BOOL __stdcall FsFindNext(HANDLE Hdl,WIN32_FIND_DATAA *FindData);
BOOL __stdcall FsFindNextW(HANDLE Hdl,WIN32_FIND_DATAW *FindData);
int __stdcall FsFindClose(HANDLE Hdl);
BOOL __stdcall FsMkDir(char* Path);
BOOL __stdcall FsMkDirW(WCHAR* Path);
int __stdcall FsExecuteFile(HWND MainWin,char* RemoteName,char* Verb);
int __stdcall FsExecuteFileW(HWND MainWin,WCHAR* RemoteName,WCHAR* Verb);
int __stdcall FsRenMovFile(char* OldName,char* NewName,BOOL Move,
                           BOOL OverWrite,RemoteInfoStruct* ri);
int __stdcall FsRenMovFileW(WCHAR* OldName,WCHAR* NewName,BOOL Move,
                           BOOL OverWrite,RemoteInfoStruct* ri);
int __stdcall FsGetFile(char* RemoteName,char* LocalName,int CopyFlags,
                        RemoteInfoStruct* ri);

int __stdcall FsGetFileW(WCHAR* RemoteName,WCHAR* LocalName,int CopyFlags,
                        RemoteInfoStruct* ri);
int __stdcall FsPutFile(char* LocalName,char* RemoteName,int CopyFlags);
int __stdcall FsPutFileW(WCHAR* LocalName,WCHAR* RemoteName,int CopyFlags);
BOOL __stdcall FsDeleteFile(char* RemoteName);
BOOL __stdcall FsDeleteFileW(WCHAR* RemoteName);
BOOL __stdcall FsRemoveDir(char* RemoteName);
BOOL __stdcall FsRemoveDirW(WCHAR* RemoteName);
BOOL __stdcall FsDisconnect(char* DisconnectRoot);
BOOL __stdcall FsDisconnectW(WCHAR* DisconnectRoot);
BOOL __stdcall FsSetAttr(char* RemoteName,int NewAttr);
BOOL __stdcall FsSetAttrW(WCHAR* RemoteName,int NewAttr);
BOOL __stdcall FsSetTime(char* RemoteName,FILETIME *CreationTime,

      FILETIME *LastAccessTime,FILETIME *LastWriteTime);
BOOL __stdcall FsSetTimeW(WCHAR* RemoteName,FILETIME *CreationTime,
      FILETIME *LastAccessTime,FILETIME *LastWriteTime);
void __stdcall FsStatusInfo(char* RemoteDir,int InfoStartEnd,int InfoOperation);
void __stdcall FsStatusInfoW(WCHAR* RemoteDir,int InfoStartEnd,int InfoOperation);
void __stdcall FsGetDefRootName(char* DefRootName,int maxlen);
int __stdcall FsExtractCustomIcon(char* RemoteName,int ExtractFlags,HICON* TheIcon);
int __stdcall FsExtractCustomIconW(WCHAR* RemoteName,int ExtractFlags,HICON* TheIcon);
void __stdcall FsSetDefaultParams(FsDefaultParamStruct* dps);

int __stdcall FsGetPreviewBitmap(char* RemoteName,int width,int height,HBITMAP* ReturnedBitmap);
int __stdcall FsGetPreviewBitmapW(WCHAR* RemoteName,int width,int height,HBITMAP* ReturnedBitmap);
BOOL __stdcall FsLinksToLocalFiles(void);
BOOL __stdcall FsGetLocalName(char* RemoteName,int maxlen);
BOOL __stdcall FsGetLocalNameW(WCHAR* RemoteName,int maxlen);

// ************************** content plugin extension ****************************

// 
#define ft_nomorefields 0

#define ft_numeric_32 1
#define ft_numeric_64 2
#define ft_numeric_floating 3
#define ft_date 4
#define ft_time 5
#define ft_boolean 6
#define ft_multiplechoice 7
#define ft_string 8
#define ft_fulltext 9
#define ft_datetime 10
#define ft_stringw 11       // Should only be returned by Unicode function

// for FsContentGetValue
#define ft_nosuchfield -1   // error, invalid field number given
#define ft_fileerror -2     // file i/o error
#define ft_fieldempty -3    // field valid, but empty

#define ft_ondemand -4      // field will be retrieved only when user presses <SPACEBAR>
#define ft_delayed 0        // field takes a long time to extract -> try again in background

// for FsContentSetValue
#define ft_setsuccess 0     // setting of the attribute succeeded

// for FsContentGetSupportedFieldFlags
#define contflags_edit 1
#define contflags_substsize 2
#define contflags_substdatetime 4
#define contflags_substdate 6
#define contflags_substtime 8
#define contflags_substattributes 10

#define contflags_substattributestr 12
#define contflags_substmask 14

// for FsContentSetValue
#define setflags_first_attribute 1     // First attribute of this file
#define setflags_last_attribute  2     // Last attribute of this file
#define setflags_only_date       4     // Only set the date of the datetime value!


#define CONTENT_DELAYIFSLOW 1  // ContentGetValue called in foreground

typedef struct {
    int size;
    DWORD PluginInterfaceVersionLow;
    DWORD PluginInterfaceVersionHi;

    char DefaultIniName[MAX_PATH];
} ContentDefaultParamStruct;

typedef struct {
	WORD wYear;
	WORD wMonth;
	WORD wDay;
} tdateformat,*pdateformat;

typedef struct {
	WORD wHour;
	WORD wMinute;
	WORD wSecond;
} ttimeformat,*ptimeformat;

int __stdcall FsContentGetSupportedField(int FieldIndex,char* FieldName,char* Units,int maxlen);
int __stdcall FsContentGetValue(char* FileName,int FieldIndex,int UnitIndex,void* FieldValue,int maxlen,int flags);
int __stdcall FsContentGetValueW(WCHAR* FileName,int FieldIndex,int UnitIndex,void* FieldValue,int maxlen,int flags);

void __stdcall FsContentStopGetValue(char* FileName);
void __stdcall FsContentStopGetValueW(WCHAR* FileName);
int __stdcall FsContentGetDefaultSortOrder(int FieldIndex);
void __stdcall FsContentPluginUnloading(void);
int __stdcall FsContentGetSupportedFieldFlags(int FieldIndex);
int __stdcall FsContentSetValue(char* FileName,int FieldIndex,int UnitIndex,int FieldType,void* FieldValue,int flags);
int __stdcall FsContentSetValueW(WCHAR* FileName,int FieldIndex,int UnitIndex,int FieldType,void* FieldValue,int flags);

BOOL __stdcall FsContentGetDefaultView(char* ViewContents,char* ViewHeaders,char* ViewWidths,char* ViewOptions,int maxlen);
BOOL __stdcall FsContentGetDefaultViewW(WCHAR* ViewContents,WCHAR* ViewHeaders,WCHAR* ViewWidths,WCHAR* ViewOptions,int maxlen);