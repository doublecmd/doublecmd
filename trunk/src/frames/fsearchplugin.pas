{
   Double Commander
   -------------------------------------------------------------------------
   Content plugins search frame

   Copyright (C) 2014-2016 Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit fSearchPlugin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls,
  ComCtrls, Buttons, uFindFiles;

type

  { TfrmSearchPlugin }

  TfrmSearchPlugin = class(TFrame)
    btnDelete: TBitBtn;
    btnAdd: TButton;
    chkUsePlugins: TCheckBox;
    HeaderControl: THeaderControl;
    pnlHeader: TPanel;
    pnlButtons: TPanel;
    pnlTable: TScrollBox;
    rbAnd: TRadioButton;
    rbOr: TRadioButton;
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure chkUsePluginsChange(Sender: TObject);
    procedure pnlTableResize(Sender: TObject);
  private
    { private declarations }
  public
    procedure Save(var SearchTemplate: TSearchTemplateRec);
    procedure Load(const SearchTemplate: TSearchTemplateRec);
  end;

implementation

{$R *.lfm}

uses
  uSearchContent;

{ TfrmSearchPlugin }

procedure TfrmSearchPlugin.Save(var SearchTemplate: TSearchTemplateRec);
var
  I: Integer;
  Plugin: TPluginPanel;
begin
  SearchTemplate.ContentPlugin:= chkUsePlugins.Checked;
  if not SearchTemplate.ContentPlugin then Exit;
  SearchTemplate.ContentPluginCombine:= rbAnd.Checked;
  SetLength(SearchTemplate.ContentPlugins, pnlTable.ControlCount);
  for I:= 0 to pnlTable.ControlCount - 1 do
  begin
    Plugin:= TPluginPanel(pnlTable.Controls[I]);
    SearchTemplate.ContentPlugins[I].Plugin:= Plugin.Plugin;
    SearchTemplate.ContentPlugins[I].FieldType:= Plugin.FieldType;
    SearchTemplate.ContentPlugins[I].Field:= Plugin.Field;
    SearchTemplate.ContentPlugins[I].Compare:= Plugin.Compare;
    SearchTemplate.ContentPlugins[I].Value:= Plugin.Value;
    SearchTemplate.ContentPlugins[I].UnitName:= Plugin.UnitName; //Set the unit *after* the field has been set so if we have error setting the unit, the error message gives the "field" has a hint.
  end;
end;

procedure TfrmSearchPlugin.Load(const SearchTemplate: TSearchTemplateRec);
var
  I: Integer;
  Panel: TPluginPanel;
begin
  chkUsePlugins.Checked:= SearchTemplate.ContentPlugin;
  rbAnd.Checked:= SearchTemplate.ContentPluginCombine;
  for I:= pnlTable.ControlCount - 1 downto 0 do pnlTable.Controls[I].Free;
  for I:= Low(SearchTemplate.ContentPlugins) to High(SearchTemplate.ContentPlugins) do
  begin
    Panel:= TPluginPanel.Create(pnlTable);
    Panel.Parent:= pnlTable;
    Panel.Plugin:= SearchTemplate.ContentPlugins[I].Plugin;
    Panel.Field:= SearchTemplate.ContentPlugins[I].Field;
    Panel.Compare:= SearchTemplate.ContentPlugins[I].Compare;
    Panel.Value:= SearchTemplate.ContentPlugins[I].Value;
    Panel.UnitName:= SearchTemplate.ContentPlugins[I].UnitName;
  end;
end;

procedure TfrmSearchPlugin.btnAddClick(Sender: TObject);
var
  Panel: TPluginPanel;
begin
  Panel:= TPluginPanel.Create(pnlTable);
  Panel.Parent:= pnlTable;
end;

procedure TfrmSearchPlugin.btnDeleteClick(Sender: TObject);
var
  Index: Integer;
begin
  Index:= pnlTable.ControlCount - 1;
  if Index >= 0 then pnlTable.Controls[Index].Free;
end;

procedure TfrmSearchPlugin.chkUsePluginsChange(Sender: TObject);
begin
  rbAnd.Enabled:= chkUsePlugins.Checked;
  rbOr.Enabled:= chkUsePlugins.Checked;
  HeaderControl.Enabled:= chkUsePlugins.Checked;
  pnlTable.Enabled:= chkUsePlugins.Checked;
  pnlButtons.Enabled:= chkUsePlugins.Checked;
end;

procedure TfrmSearchPlugin.pnlTableResize(Sender: TObject);
var
  I, ColumnWidth: Integer;
begin
  ColumnWidth:= pnlTable.ClientWidth div HeaderControl.Sections.Count;
  for I:= 0 to HeaderControl.Sections.Count - 1 do
  begin
    HeaderControl.Sections[I].Width:= ColumnWidth;
  end;
end;

end.

