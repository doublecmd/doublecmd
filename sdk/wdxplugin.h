#include "common.h"

// Contents of file contplug.h version 2.0

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
#define ft_stringw 11

// for ContentGetValue
#define ft_nosuchfield -1   // error, invalid field number given
#define ft_fileerror -2     // file i/o error
#define ft_fieldempty -3    // field valid, but empty

#define ft_ondemand -4      // field will be retrieved only when user presses <SPACEBAR>
#define ft_notsupported -5  // function not supported
#define ft_setcancel -6     // user clicked cancel in field editor
#define ft_delayed 0        // field takes a long time to extract -> try again in background

// for ContentSetValue
#define ft_setsuccess 0     // setting of the attribute succeeded

// for ContentGetSupportedFieldFlags
#define contflags_edit 1
#define contflags_substsize 2

#define contflags_substdatetime 4
#define contflags_substdate 6
#define contflags_substtime 8
#define contflags_substattributes 10
#define contflags_substattributestr 12
#define contflags_passthrough_size_float 14
#define contflags_substmask 14
#define contflags_fieldedit 16

#define contst_readnewdir 1
#define contst_refreshpressed 2
#define contst_showhint 4

#define setflags_first_attribute 1     // First attribute of this file
#define setflags_last_attribute  2     // Last attribute of this file

#define setflags_only_date       4     // Only set the date of the datetime value!

#define editflags_initialize     1     // The data passed to the plugin may be used to
                                       // initialize the edit dialog

#define CONTENT_DELAYIFSLOW 1  // ContentGetValue called in foreground
#define CONTENT_PASSTHROUGH 2  // If requested via contflags_passthrough_size_float: The size
                               // is passed in as floating value, TC expects correct value

                               // from the given units value, and optionally a text string

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

int __stdcall ContentGetDetectString(char* DetectString,int maxlen);
int __stdcall ContentGetSupportedField(int FieldIndex,char* FieldName,char* Units,int maxlen);
int __stdcall ContentGetValue(char* FileName,int FieldIndex,int UnitIndex,void* FieldValue,int maxlen,int flags);
int __stdcall ContentGetValueW(WCHAR* FileName,int FieldIndex,int UnitIndex,void* FieldValue,int maxlen,int flags);

void __stdcall ContentSetDefaultParams(ContentDefaultParamStruct* dps);
void __stdcall ContentPluginUnloading(void);
void __stdcall ContentStopGetValue(char* FileName);
void __stdcall ContentStopGetValueW(WCHAR* FileName);
int __stdcall ContentGetDefaultSortOrder(int FieldIndex);
int __stdcall ContentGetSupportedFieldFlags(int FieldIndex);
int __stdcall ContentSetValue(char* FileName,int FieldIndex,int UnitIndex,int FieldType,void* FieldValue,int flags);
int __stdcall ContentSetValueW(WCHAR* FileName,int FieldIndex,int UnitIndex,int FieldType,void* FieldValue,int flags);

int __stdcall ContentEditValue(HWND ParentWin,int FieldIndex,int UnitIndex,int FieldType,
                void* FieldValue,int maxlen,int flags,char* langidentifier);
void __stdcall ContentSendStateInformation(int state,char* path);
void __stdcall ContentSendStateInformationW(int state,WCHAR* path);