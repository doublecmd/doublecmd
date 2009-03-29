(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower Abbrevia
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* ABBREVIA: AbPeFn.Inc 3.05                             *}
{*********************************************************}
{* ABBREVIA: Property Editor - FileName (Source)         *}
{*   See AbPeFn.pas for the VCL header                   *}
{*   See AbQPeFn.pas for the CLX header                  *}
{*********************************************************}

{$I AbDefine.inc}

interface

uses
{$IFDEF UsingClx }
  QDialogs, QForms,
{$ELSE}
  Dialogs, Forms,
{$ENDIF}

{$IFDEF LINUX}
  DesignIntf,
  DesignEditors,
{$ELSE}
{$IFDEF VERSION6}
  DesignIntf,
  DesignEditors,
{$ELSE}
  DsgnIntf,
{$ENDIF VERSION6}
{$ENDIF LINUX}
  SysUtils;


type
  TAbFileNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes;
      override;
    procedure Edit;
      override;
  end;

  TAbExeNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes;
      override;
    procedure Edit;
      override;
  end;

  TAbCabNameProperty = class( TStringProperty )
  public
    function GetAttributes: TPropertyAttributes;
      override;
    procedure Edit;
      override;
  end;

  TAbLogNameProperty = class( TStringProperty )
  public
    function GetAttributes: TPropertyAttributes;
      override;
    procedure Edit;
      override;
  end;

implementation

uses
  AbConst,
  AbArcTyp;

{ -------------------------------------------------------------------------- }
procedure AbGetFilename(const Ext : string;
                        const Filter : string;
                        const Title : string;
                          var aFilename : string);
var
  D : TOpenDialog;
begin
  D := TOpenDialog.Create( Application );
  try
    D.DefaultExt := Ext;
    D.Filter := Filter;
    D.FilterIndex := 0;
    D.Options := [];
    D.Title := Title;
    D.FileName := aFilename;
    if D.Execute then
      aFilename := D.FileName;
  finally
    D.Free;
  end;
end;

{ == for zip files ========================================================= }
function TAbFileNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;
{ -------------------------------------------------------------------------- }
procedure TAbFileNameProperty.Edit;
var
  FN : string;
begin
  FN := Value;
{$IFDEF WIN32}
  AbGetFilename(AbStrRes(AbDefaultExt), AbStrRes(AbFilter),
    AbStrRes(AbFileNameTitle), FN);
{$ENDIF}
{$IFDEF LINUX}
  AbGetFilename(AbStrRes(AbDefaultExt), AbStrRes(AbFilter),
    AbStrRes(AbFileNameTitle), FN);
{$ENDIF}
  Value := FN;
end;

{ == for exe files ========================================================= }
function TAbExeNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;
{ -------------------------------------------------------------------------- }
procedure TAbExeNameProperty.Edit;
var
  FN : string;
begin
  FN := Value;
{$IFDEF WIN32}
  AbGetFilename(AbStrRes(AbExeExt), AbStrRes(AbExeFilter),
    AbStrRes(AbFileNameTitle), FN);
{$ENDIF}
{$IFDEF LINUX}
  AbGetFilename(AbStrRes(AbExeExt), AbStrRes(AbExeFilter),
    AbStrRes(AbFileNameTitle), FN);
{$ENDIF}
  Value := FN;
end;

{ == for cab files ========================================================= }
function TAbCabNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;
{ -------------------------------------------------------------------------- }
procedure TAbCabNameProperty.Edit;
var
  FN : string;
begin
  FN := Value;
{$IFDEF WIN32}
  AbGetFilename(AbStrRes(AbCabExt), AbStrRes(AbCabFilter),
    AbStrRes(AbFileNameTitle), FN);
{$ENDIF}
{$IFDEF LINUX}
  AbGetFilename(AbStrRes(AbCabExt), AbStrRes(AbCabFilter),
    AbStrRes(AbFileNameTitle), FN);
{$ENDIF}
  Value := FN;
end;

{ == for log files ========================================================= }
function TAbLogNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;
{ -------------------------------------------------------------------------- }
procedure TAbLogNameProperty.Edit;
var
  FN : string;
begin
  FN := Value;
{$IFDEF WIN32}
  AbGetFilename(AbStrRes(AbLogExt), AbStrRes(AbLogFilter),
    AbStrRes(AbFileNameTitle), FN);
{$ENDIF}
{$IFDEF LINUX}
  AbGetFilename(AbStrRes(AbLogExt), AbStrRes(AbLogFilter),
    AbStrRes(AbFileNameTitle), FN);
{$ENDIF}
  Value := FN;
end;
{ -------------------------------------------------------------------------- }

end.


