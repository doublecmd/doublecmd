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
{* ABBREVIA: AbPeDir.inc 3.05                            *}
{*********************************************************}
{* ABBREVIA: Property Editor - Directory (Source)        *}
{*   See AbPeDir.pas for the VCL header                  *}
{*   See AbQPeDir.pas for the CLX header                 *}
{*********************************************************}

{$I AbDefine.inc}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF UsingClx}
  QGraphics,
  QForms,
  QControls,
  QStdCtrls,
  QButtons,
  QExtCtrls,
{$ELSE}
  Graphics,
  Forms,
  Controls,
  StdCtrls,
  Buttons,
  ExtCtrls,
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
  SysUtils,
  Classes;

type
  TAbDirectoryProperty = class( TStringProperty )
  public
    function GetAttributes: TPropertyAttributes;
             override;
    procedure Edit;
              override;
  end;

implementation

uses
{$IFDEF UsingClx}
  AbQDgDir;
{$ELSE}
  AbDlgDir;
{$ENDIF}


function TAbDirectoryProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

{$IFDEF MSWINDOWS}
procedure TAbDirectoryProperty.Edit;
var
  D : TAbDirDlg;
begin
  D := TAbDirDlg.Create(Application);
  try
    D.Caption := 'Directory';
    D.AdditionalText := 'Select Directory';
    if D.Execute then
      Value := D.SelectedFolder;
  finally
    D.Free;
  end;
end;
{$ELSE}
procedure TAbDirectoryProperty.Edit;
var
  D : TDirDlg;
begin
  D := TDirDlg.Create(Application);
  try
{$IFDEF MSWINDOWS} 
    D.DirectoryListBox1.Directory := Value;
{$ENDIF}
    D.ShowModal;
    if D.ModalResult = mrOK then
      Value := D.SelectedFolder;
  finally
    D.Free;
  end;
end;
{$ENDIF}

end.
