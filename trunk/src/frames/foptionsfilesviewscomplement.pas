{
   Double Commander
   -------------------------------------------------------------------------
   Files views complement options page

   Copyright (C) 2018 Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit fOptionsFilesViewsComplement;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Graphics, ExtCtrls, fOptionsFrame;

type

  { TfrmOptionsFilesViewsComplement }

  TfrmOptionsFilesViewsComplement = class(TOptionsEditor)
    btnAddAttribute: TButton;
    btnAttrsHelp: TButton;
    cbDblClickToParent: TCheckBox;
    cbHighlightUpdatedFiles: TCheckBox;
    cbDirBrackets: TCheckBox;
    cbListFilesInThread: TCheckBox;
    cbLoadIconsSeparately: TCheckBox;
    cbDelayLoadingTabs: TCheckBox;
    cbShowSystemFiles: TCheckBox;
    cbSpaceMovesDown: TCheckBox;
    cbInplaceRename: TCheckBox;
    gbMisc: TGroupBox;
    pnlDefaultAttribute: TPanel;
    chkMarkMaskFilterWindows: TCheckBox;
    gbMarking: TGroupBox;
    lbAttributeMask: TLabel;
    edtDefaultAttribute: TEdit;
    chkMarkMaskShowAttribute: TCheckBox;
    procedure btnAddAttributeClick(Sender: TObject);
    procedure btnAttrsHelpClick(Sender: TObject);
  private
    procedure OnAddAttribute(Sender: TObject);
  protected
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  public
    class function GetIconIndex: Integer; override;
    class function GetTitle: String; override;
  end;

implementation

{$R *.lfm}

uses
  HelpIntfs, fAttributesEdit, uGlobs, uLng;

{ TfrmOptionsFilesViewsComplement }

procedure TfrmOptionsFilesViewsComplement.Load;
begin
  cbSpaceMovesDown.Checked := gSpaceMovesDown;
  cbDirBrackets.Checked := gDirBrackets;
  cbShowSystemFiles.Checked:= gShowSystemFiles;
  {$IFDEF LCLCARBON}
  // Under Mac OS X loading file list in separate thread are very very slow
  // so disable and hide this option under Mac OS X Carbon
  cbListFilesInThread.Visible:= False;
  {$ELSE}
  cbListFilesInThread.Checked:= gListFilesInThread;
  {$ENDIF}
  cbLoadIconsSeparately.Checked:= gLoadIconsSeparately;
  cbDelayLoadingTabs.Checked:= gDelayLoadingTabs;
  cbHighlightUpdatedFiles.Checked:= gHighlightUpdatedFiles;
  cbInplaceRename.Checked := gInplaceRename;
  cbDblClickToParent.Checked := gDblClickToParent;

  chkMarkMaskFilterWindows.Checked := gMarkMaskFilterWindows;
  chkMarkMaskShowAttribute.Checked := gMarkShowWantedAttribute;
  edtDefaultAttribute.Text := gMarkDefaultWantedAttribute;
end;

function TfrmOptionsFilesViewsComplement.Save: TOptionsEditorSaveFlags;
begin
  gSpaceMovesDown := cbSpaceMovesDown.Checked;
  gDirBrackets := cbDirBrackets.Checked;
  gShowSystemFiles:= cbShowSystemFiles.Checked;
  gListFilesInThread:= cbListFilesInThread.Checked;
  gLoadIconsSeparately:= cbLoadIconsSeparately.Checked;
  gDelayLoadingTabs := cbDelayLoadingTabs.Checked;
  gHighlightUpdatedFiles := cbHighlightUpdatedFiles.Checked;
  gInplaceRename := cbInplaceRename.Checked;
  gDblClickToParent := cbDblClickToParent.Checked;

  gMarkMaskFilterWindows := chkMarkMaskFilterWindows.Checked;
  gMarkShowWantedAttribute := chkMarkMaskShowAttribute.Checked;
  gMarkDefaultWantedAttribute := edtDefaultAttribute.Text;

  Result := [];
end;

class function TfrmOptionsFilesViewsComplement.GetIconIndex: Integer;
begin
  Result := 29;
end;

class function TfrmOptionsFilesViewsComplement.GetTitle: String;
begin
  Result := rsOptionsEditorFilesViewsComplement;
end;


procedure TfrmOptionsFilesViewsComplement.btnAddAttributeClick(Sender: TObject);
var
  FFrmAttributesEdit: TfrmAttributesEdit;
begin
  FFrmAttributesEdit := TfrmAttributesEdit.Create(Self);
  try
  FFrmAttributesEdit.OnOk := @OnAddAttribute;
  FFrmAttributesEdit.Reset;
  FFrmAttributesEdit.ShowModal;
  finally
    FFrmAttributesEdit.Free;
  end;
end;

procedure TfrmOptionsFilesViewsComplement.btnAttrsHelpClick(Sender: TObject);
begin
  ShowHelpOrErrorForKeyword('', edtDefaultAttribute.HelpKeyword);
end;

procedure TfrmOptionsFilesViewsComplement.OnAddAttribute(Sender: TObject);
var
  sAttr: String;
begin
  sAttr := edtDefaultAttribute.Text;
  if edtDefaultAttribute.SelStart > 0 then
    Insert((Sender as TfrmAttributesEdit).AttrsAsText, sAttr, edtDefaultAttribute.SelStart + 1) // Insert at caret position.
  else
    sAttr := sAttr + (Sender as TfrmAttributesEdit).AttrsAsText;
  edtDefaultAttribute.Text := sAttr;
end;

end.

