unit uwdxprototypes; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WdxPlugin;

{$IFDEF MSWINDOWS}{$CALLING STDCALL}{$ELSE}{$CALLING CDECL}{$ENDIF}

type
  { Mandatory (must be implemented) }
  TContentGetSupportedField = function (FieldIndex:integer;FieldName:pchar;
                                         Units:pchar;maxlen:integer):integer;
  
  TContentGetValue = function (FileName:pchar;FieldIndex,UnitIndex:integer;
                               FieldValue:pbyte;  maxlen,flags:integer):integer;
                               
  { Optional (must NOT be implemented if unsupported!) }
  TContentGetDetectString = procedure (DetectString:pchar;maxlen:integer);
  
  TContentSetDefaultParams = procedure (dps:pContentDefaultParamStruct);
  
  TContentStopGetValue = procedure (FileName:pchar);
  
  TContentGetDefaultSortOrder = function (FieldIndex:integer):integer;
  
  TContentPluginUnloading = procedure;
  
  TContentGetSupportedFieldFlags = function (FieldIndex:integer):integer;
  
  TContentSetValue = function (FileName:pchar;FieldIndex,UnitIndex,FieldType:integer;
                               FieldValue:pbyte;flags:integer):integer;
                               
  TContentEditValue = function (handle:thandle;FieldIndex,UnitIndex,FieldType:integer;
                                FieldValue:pchar;maxlen:integer;flags:integer;
                                langidentifier:pchar):integer;
                                
  TContentSendStateInformation = procedure (state:integer;path:pchar);

  { Unicode }
  TContentGetValueW = function (FileName:pwidechar;FieldIndex,UnitIndex:integer;
                                FieldValue:pbyte; maxlen,flags:integer):integer;

  TContentStopGetValueW = procedure (FileName:pwidechar);

  TContentSetValueW = function (FileName:pwidechar;FieldIndex,UnitIndex,FieldType:integer;
                                FieldValue:pbyte;flags:integer):integer;

  TContentSendStateInformationW = procedure (state:integer;path:pwidechar);

{$CALLING DEFAULT}

implementation

end.

