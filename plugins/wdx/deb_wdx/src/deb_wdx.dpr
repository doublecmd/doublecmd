//***************************************************************
// This file is part of DEBWDX, a content plugin for
// Total Commander handling Debian Linux package.
//
// Copyright (C) 2005 Ralgh Young (yang.guilong@gmail.com)
//***************************************************************
// Add some changes for Lazarus and Linux compatibility
//
// Copyright (C) 2009  Koblov Alexander (Alexx2000@mail.ru)
//***************************************************************

library deb_wdx;

uses
  SysUtils,
  Classes,
  WdxPlugin,
  deb_wdx_intf in 'deb_wdx_intf.pas';

{$E wdx}

{$IFDEF WINDOWS}{$R deb_wdx.rc}{$ENDIF}

exports
  ContentGetDetectString    name 'ContentGetDetectString',
  ContentGetSupportedField  name 'ContentGetSupportedField',
  ContentGetValue           name 'ContentGetValue';
  
begin
end.
