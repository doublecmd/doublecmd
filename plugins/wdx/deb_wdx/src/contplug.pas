unit contplug; { Contents of file contplug.pas }

interface
uses
    SysUtils;
    
const ft_nomorefields=0;
      ft_numeric_32=1;
      ft_numeric_64=2;
      ft_numeric_floating=3;
      ft_date=4;
      ft_time=5;
      ft_boolean=6;
      ft_multiplechoice=7;
      ft_string=8;
      ft_fulltext=9;
      ft_datetime=10;

// for ContentGetValue
      ft_nosuchfield=-1;
      ft_fileerror=-2;
      ft_fieldempty=-3;
      ft_ondemand=-4;
      ft_delayed=0;

      CONTENT_DELAYIFSLOW=1;  // ContentGetValue called in foreground

type tContentDefaultParamStruct=record
      size,
      PluginInterfaceVersionLow,
      PluginInterfaceVersionHi:longint;
      DefaultIniName:array[0..MAX_PATH-1] of char;
    end;

    pContentDefaultParamStruct=^tContentDefaultParamStruct;

type tdateformat=record
       wYear,wMonth,wDay:word;
     end;
     pdateformat=^tdateformat;

type ttimeformat=record
       wHour,wMinute,wSecond:word;
     end;
     ptimeformat=^ttimeformat;

{ Function prototypes: }

{
procedure ContentGetDetectString(DetectString:pchar;maxlen:integer); stdcall;
function ContentGetSupportedField(FieldIndex:integer;FieldName:pchar;
  Units:pchar;maxlen:integer):integer; stdcall;
function ContentGetValue(FileName:pchar;FieldIndex,UnitIndex:integer;FieldValue:pbyte;
  maxlen,flags:integer):integer; stdcall;
procedure ContentSetDefaultParams(dps:pContentDefaultParamStruct); stdcall;

procedure ContentStopGetValue(FileName:pchar); stdcall;
function ContentGetDefaultSortOrder(FieldIndex:integer):integer; stdcall;
}

implementation
end.
