unit uwdxprototypes; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WdxPlugin;
  
  type
  //   Mandatory (must be implemented)
  TContentGetSupportedField = function (FieldIndex:integer;FieldName:pchar;
                                         Units:pchar;maxlen:integer):integer; stdcall;
  
  TContentGetValue = function (FileName:pchar;FieldIndex,UnitIndex:integer;
                               FieldValue:pbyte;  maxlen,flags:integer):integer; stdcall;
                               
  // Optional (must NOT be implemented if unsupported!)
  TContentGetDetectString = procedure (DetectString:pchar;maxlen:integer); stdcall;
  
  TContentSetDefaultParams = procedure (dps:pContentDefaultParamStruct); stdcall;
  
  TContentStopGetValue = procedure (FileName:pchar); stdcall;
  
  TContentGetDefaultSortOrder = function (FieldIndex:integer):integer; stdcall;
  
  TContentPluginUnloading = procedure; stdcall;
  
  TContentGetSupportedFieldFlags = function (FieldIndex:integer):integer; stdcall;
  
  TContentSetValue = function (FileName:pchar;FieldIndex,UnitIndex,FieldType:integer;
                               FieldValue:pbyte;flags:integer):integer; stdcall;
                               
  TContentEditValue = function (handle:thandle;FieldIndex,UnitIndex,FieldType:integer;
                                FieldValue:pchar;maxlen:integer;flags:integer;
                                langidentifier:pchar):integer; stdcall;
                                
  TContentSendStateInformation = procedure (state:integer;path:pchar); stdcall;

implementation

end.

