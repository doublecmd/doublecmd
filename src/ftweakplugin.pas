{
   Double commander
   -------------------------------------------------------------------------
   Plugin tweak window

   Copyright (C) 2008-2018 Alexander Koblov (alexx2000@mail.ru)

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

unit fTweakPlugin; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, EditBtn, Buttons,
  Menus, uWCXModule, uGlobs;

type

  { TfrmTweakPlugin }

  TfrmTweakPlugin = class(TForm)
    btnAdd: TButton;
    btnCancel: TButton;
    btnChange: TButton;
    btnDefault: TButton;
    btnOK: TButton;
    btnRelativePlugin2: TSpeedButton;
    btnRelativePlugin1: TSpeedButton;
    btnRemove: TButton;
    cbExt: TComboBox;
    cbPK_CAPS_BY_CONTENT: TCheckBox;
    cbPK_CAPS_DELETE: TCheckBox;
    cbPK_CAPS_ENCRYPT: TCheckBox;
    cbPK_CAPS_HIDE: TCheckBox;
    cbPK_CAPS_MEMPACK: TCheckBox;
    cbPK_CAPS_MODIFY: TCheckBox;
    cbPK_CAPS_MULTIPLE: TCheckBox;
    cbPK_CAPS_NEW: TCheckBox;
    cbPK_CAPS_OPTIONS: TCheckBox;
    cbPK_CAPS_SEARCHTEXT: TCheckBox;
    edtDescription: TEdit;
    edtDetectStr: TEdit;
    edtName: TEdit;
    fnePlugin2: TFileNameEdit;
    fnePlugin1: TFileNameEdit;
    pmPathHelper: TPopupMenu;
    pnlTweakOther: TPanel;
    lblDescription: TLabel;
    lblDetectStr: TLabel;
    lblName: TLabel;
    lblExtension: TLabel;
    lblFlags: TLabel;
    lblFlagsValue: TLabel;
    lblPlugin2: TLabel;
    lblPackerPlugin: TLabel;
    lblPlugin: TLabel;
    nbTweakAll: TNotebook;
    pnlButtons: TPanel;
    pnlFlags: TPanel;
    pnlPackerExtsButtons: TPanel;
    pgTweakPacker: TPage;
    pgTweakOther: TPage;
    pnlTweak: TPanel;
    procedure btnAddClick(Sender: TObject);
    procedure btnChangeClick(Sender: TObject);
    procedure btnDefaultClick(Sender: TObject);
    procedure btnRelativePlugin1Click(Sender: TObject);
    procedure btnRelativePlugin2Click(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure cbExtChange(Sender: TObject);
    procedure cbPackerFlagsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FWCXPlugins: TWCXModuleList;
    FPluginFileName: String;
    iPrevIndex: Integer;
    function GetDefaultFlags(PluginFileName: String): PtrInt;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure LoadConfiguration(PluginIndex:integer);
    procedure SaveConfiguration(PluginIndex:integer);
    destructor Destroy; override;
  end; 

function ShowTweakPluginDlg(PluginType: TPluginType; PluginIndex: Integer): Boolean;

implementation

{$R *.lfm}

uses
  //Lazarus, Free-Pascal, etc.
  Math, Dialogs, LCLVersion,

  //DC
  fOptionsPluginsDSX, fOptionsPluginsWCX, fOptionsPluginsWDX,
  fOptionsPluginsWFX, fOptionsPluginsWLX, WcxPlugin, uDCUtils, uLng,
  uSpecialDir;

function ShowTweakPluginDlg(PluginType: TPluginType; PluginIndex: Integer): Boolean;
var
  I, iIndex: Integer;
begin
  with TfrmTweakPlugin.Create(Application) do
  try
    case PluginType of
    ptDSX:
      begin
        nbTweakAll.PageIndex:= 1;
        fnePlugin2.Text:= tmpDSXPlugins.GetDsxModule(PluginIndex).FileName;
        edtDescription.Text:= tmpDSXPlugins.GetDsxModule(PluginIndex).Descr;
        edtName.Text:= tmpDSXPlugins.GetDsxModule(PluginIndex).Name;
        lblDetectStr.Visible:= False;
        edtDetectStr.Visible:= False;
        ActiveControl:=fnePlugin2;
      end;
    ptWCX:
      begin
        nbTweakAll.PageIndex:= 0;
        FWCXPlugins:= TWCXModuleList.Create;
        FWCXPlugins.Assign(tmpWCXPlugins);
        FPluginFileName := FWCXPlugins.FileName[PluginIndex];
        fnePlugin1.FileName:= FPluginFileName;
        for I:= 0 to FWCXPlugins.Count - 1 do
          if FWCXPlugins.FileName[I] = fnePlugin1.FileName then
            begin
              if cbExt.Items.Count=0 then lblPlugin.Tag:=IfThen(FWCXPlugins.Enabled[I],1,0);
              cbExt.Items.AddObject(FWCXPlugins.Ext[I], TObject(FWCXPlugins.Flags[I]));
            end;
        iPrevIndex:= -1;
        cbExt.ItemIndex := cbExt.Items.IndexOf(FWCXPlugins.Ext[PluginIndex]);
        if (cbExt.ItemIndex = -1) then cbExt.ItemIndex := 0;
        cbExtChange(cbExt);
        btnRemove.Enabled:= (cbExt.Items.Count > 1);
      end;
    ptWDX:
      begin
        nbTweakAll.PageIndex:= 1;
        fnePlugin2.Text:= tmpWDXPlugins.GetWdxModule(PluginIndex).FileName;
        edtDetectStr.Text:= tmpWDXPlugins.GetWdxModule(PluginIndex).DetectStr;
        edtName.Text:= tmpWDXPlugins.GetWdxModule(PluginIndex).Name;
        lblDescription.Visible:= False;
        edtDescription.Visible:= False;
        ActiveControl:=fnePlugin2;
      end;
    ptWFX:
      begin
        nbTweakAll.PageIndex:= 1;
        fnePlugin2.Text:= tmpWFXPlugins.FileName[PluginIndex];
        edtName.Text:= tmpWFXPlugins.Name[PluginIndex];
        lblDetectStr.Visible:= False;
        edtDetectStr.Visible:= False;
        lblDescription.Visible:= False;
        edtDescription.Visible:= False;
        ActiveControl:=fnePlugin2;
      end;
    ptWLX:
      begin
        nbTweakAll.PageIndex:= 1;
        fnePlugin2.Text:= tmpWLXPlugins.GetWlxModule(PluginIndex).FileName;
        edtDetectStr.Text:= tmpWLXPlugins.GetWlxModule(PluginIndex).DetectStr;
        edtName.Text:= tmpWLXPlugins.GetWlxModule(PluginIndex).Name;
        lblDescription.Visible:= False;
        edtDescription.Visible:= False;
        ActiveControl:=fnePlugin2;
      end;
    end;
    LoadConfiguration(ord(PluginType));
    gSpecialDirList.PopulateMenuWithSpecialDir(pmPathHelper,mp_PATHHELPER,nil);
    Result:= (ShowModal = mrOK);
    if Result then
      case PluginType of
      ptDSX:
        begin
          tmpDSXPlugins.GetDsxModule(PluginIndex).FileName:= fnePlugin2.Text;
          tmpDSXPlugins.GetDsxModule(PluginIndex).Descr := edtDescription.Text;
          tmpDSXPlugins.GetDsxModule(PluginIndex).Name:= edtName.Text;
        end;
      ptWCX:
        begin
          for I:= 0 to cbExt.Items.Count - 1 do
            begin
              iIndex:= FWCXPlugins.Find(FPluginFileName, cbExt.Items[I]);
              if iIndex >= 0 then
                begin
                  FWCXPlugins.FileName[iIndex]:= fnePlugin1.FileName;
                  FWCXPlugins.Flags[iIndex]:= PtrInt(cbExt.Items.Objects[I]);
                end;
            end;
          tmpWCXPlugins.Assign(FWCXPlugins);
        end;
      ptWDX:
        begin
          tmpWDXPlugins.GetWdxModule(PluginIndex).FileName:= fnePlugin2.Text;
          tmpWDXPlugins.GetWdxModule(PluginIndex).DetectStr:= edtDetectStr.Text;
          tmpWDXPlugins.GetWdxModule(PluginIndex).Name:= edtName.Text;
        end;
      ptWFX:
        begin
          tmpWFXPlugins.FileName[PluginIndex]:= fnePlugin2.Text;
          tmpWFXPlugins.Name[PluginIndex]:= edtName.Text;
        end;
      ptWLX:
        begin
          tmpWLXPlugins.GetWlxModule(PluginIndex).FileName:= fnePlugin2.Text;
          tmpWLXPlugins.GetWlxModule(PluginIndex).DetectStr:= edtDetectStr.Text;
          tmpWLXPlugins.GetWlxModule(PluginIndex).Name:= edtName.Text;
        end;
      end;
    SaveConfiguration(ord(PluginType));
  finally
    Free;
  end;
end;

{ TfrmTweakPlugin }

constructor TfrmTweakPlugin.Create(TheOwner: TComponent);
begin
  FWCXPlugins := nil;
  iPrevIndex := -1;
  inherited;
end;


{ TfrmTweakPlugin.LoadConfiguration }
// Just to save width.
// Firt time it opens according to "autosize" system will determine, then when we exit it will be saved and then it will be restore to next session.
procedure TfrmTweakPlugin.LoadConfiguration(PluginIndex:integer);
begin
  if (gTweakPluginWidth[PluginIndex]<>0) AND (gTweakPluginHeight[PluginIndex]<>0) then
  begin
    AutoSize:=False;
    width := gTweakPluginWidth[PluginIndex];
    height := gTweakPluginHeight[PluginIndex];
  end;
end;

procedure TfrmTweakPlugin.SaveConfiguration(PluginIndex:integer);
begin
  gTweakPluginWidth[PluginIndex] := width;
  gTweakPluginHeight[PluginIndex] := height;
end;

destructor TfrmTweakPlugin.Destroy;
begin
  inherited;
  if Assigned(FWCXPlugins) then
    FreeAndNil(FWCXPlugins);
end;

procedure TfrmTweakPlugin.cbExtChange(Sender: TObject);
var
  iFlags: PtrInt;
begin
  iPrevIndex:= cbExt.ItemIndex;
  iFlags:= PtrInt(cbExt.Items.Objects[cbExt.ItemIndex]);
  lblFlagsValue.Caption:= '('+IntToStr(iFlags)+')';

  cbPK_CAPS_NEW.Checked        := (iFlags and PK_CAPS_NEW) <> 0;
  cbPK_CAPS_MODIFY.Checked     := (iFlags and PK_CAPS_MODIFY) <> 0;
  cbPK_CAPS_MULTIPLE.Checked   := (iFlags and PK_CAPS_MULTIPLE) <> 0;
  cbPK_CAPS_DELETE.Checked     := (iFlags and PK_CAPS_DELETE) <> 0;
  cbPK_CAPS_OPTIONS.Checked    := (iFlags and PK_CAPS_OPTIONS) <> 0;
  cbPK_CAPS_MEMPACK.Checked    := (iFlags and PK_CAPS_MEMPACK) <> 0;
  cbPK_CAPS_BY_CONTENT.Checked := (iFlags and PK_CAPS_BY_CONTENT) <> 0;
  cbPK_CAPS_SEARCHTEXT.Checked := (iFlags and PK_CAPS_SEARCHTEXT) <> 0;
  cbPK_CAPS_HIDE.Checked       := (iFlags and PK_CAPS_HIDE) <> 0;
  cbPK_CAPS_ENCRYPT.Checked    := (iFlags and PK_CAPS_ENCRYPT) <> 0;
end;

procedure TfrmTweakPlugin.cbPackerFlagsClick(Sender: TObject);
var
  iFlags: PtrInt;
begin
  if iPrevIndex >= 0 then // save new flags
    begin
      iFlags:= 0;
      if cbPK_CAPS_NEW.Checked then
        iFlags:= iFlags or PK_CAPS_NEW;
      if cbPK_CAPS_MODIFY.Checked then
        iFlags:= iFlags or PK_CAPS_MODIFY;
      if cbPK_CAPS_MULTIPLE.Checked then
        iFlags:= iFlags or PK_CAPS_MULTIPLE;
      if cbPK_CAPS_DELETE.Checked then
        iFlags:= iFlags or PK_CAPS_DELETE;
      if cbPK_CAPS_OPTIONS.Checked then
        iFlags:= iFlags or PK_CAPS_OPTIONS;
      if cbPK_CAPS_MEMPACK.Checked then
        iFlags:= iFlags or PK_CAPS_MEMPACK;
      if cbPK_CAPS_BY_CONTENT.Checked then
        iFlags:= iFlags or PK_CAPS_BY_CONTENT;
      if cbPK_CAPS_SEARCHTEXT.Checked then
        iFlags:= iFlags or PK_CAPS_SEARCHTEXT;
      if cbPK_CAPS_HIDE.Checked then
        iFlags:= iFlags or PK_CAPS_HIDE;
      if cbPK_CAPS_ENCRYPT.Checked then
        iFlags:= iFlags or PK_CAPS_ENCRYPT;
      cbExt.Items.Objects[iPrevIndex]:= TObject(iFlags);
      lblFlagsValue.Caption:= '('+IntToStr(iFlags)+')';
    end;
end;

procedure TfrmTweakPlugin.FormCreate(Sender: TObject);
begin
  {$if not declared(lcl_fullversion) or (lcl_fullversion < 093100)}
  nbTweakAll.ShowTabs := False;
  nbTweakAll.TabStop := True;
  {$endif}
end;

procedure TfrmTweakPlugin.btnDefaultClick(Sender: TObject);
begin
  cbExt.Items.Objects[cbExt.ItemIndex]:= TObject(GetDefaultFlags(fnePlugin1.FileName));
  iPrevIndex:= -1;
  cbExtChange(cbExt);
end;

procedure TfrmTweakPlugin.btnRelativePlugin1Click(Sender: TObject);
begin
  fnePlugin1.SetFocus;
  gSpecialDirList.SetSpecialDirRecipientAndItsType(fnePlugin1, pfFILE);
  pmPathHelper.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

procedure TfrmTweakPlugin.btnRelativePlugin2Click(Sender: TObject);
begin
  fnePlugin2.SetFocus;
  gSpecialDirList.SetSpecialDirRecipientAndItsType(fnePlugin2, pfFILE);
  pmPathHelper.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

procedure TfrmTweakPlugin.btnRemoveClick(Sender: TObject);
var
  I, OldIndex: Integer;
begin
  iPrevIndex:= -1;  // Must be before cbExt.Items.Delete, because it may trigger cbExtChange.
  OldIndex := cbExt.ItemIndex;
  I:= FWCXPlugins.Find(FPluginFileName, cbExt.Text);
  if I >= 0 then
    FWCXPlugins.Delete(I);
  cbExt.Items.Delete(cbExt.ItemIndex);
  if OldIndex >= cbExt.Items.Count then
    OldIndex := OldIndex - 1;
  cbExt.ItemIndex := OldIndex;
  if iPrevIndex = -1 then // Call only if not already triggerred.
    cbExtChange(cbExt);
  btnRemove.Enabled:= (cbExt.Items.Count > 1);
end;

procedure TfrmTweakPlugin.btnAddClick(Sender: TObject);
var
  sExt: String = '';
  iFlags: PtrInt;
  I: Integer;
begin
  if InputQuery(rsOptEnterExt,Format(rsOptAssocPluginWith, [fnePlugin1.FileName]), sExt) then
    begin
      iFlags:= GetDefaultFlags(fnePlugin1.FileName);
      cbExt.ItemIndex:= cbExt.Items.AddObject(sExt, TObject(iFlags));
      I := FWCXPlugins.Add(cbExt.Items[cbExt.ItemIndex], iFlags, FPluginFileName);
      FWCXPlugins.Enabled[I] := (lblPlugin.Tag=1);
      iPrevIndex:= -1;
      cbExtChange(cbExt);
      btnRemove.Enabled:= (cbExt.Items.Count > 1);
    end;
end;

procedure TfrmTweakPlugin.btnChangeClick(Sender: TObject);
var
  I: Integer;
  sExt: String;
begin
  sExt:= cbExt.Items[cbExt.ItemIndex];
  I:= FWCXPlugins.Find(FPluginFileName, sExt);
  if (I >= 0) and
     InputQuery(rsOptEnterExt,Format(rsOptAssocPluginWith, [fnePlugin1.FileName]), sExt) then
    begin
      FWCXPlugins.Ext[I]:= sExt;
      cbExt.Items[cbExt.ItemIndex]:= sExt;
    end;
end;

function TfrmTweakPlugin.GetDefaultFlags(PluginFileName: String): PtrInt;
var
  WcxModule: TWcxModule;
begin
  WcxModule := gWCXPlugins.LoadModule(PluginFileName);
  if not Assigned(WcxModule) then Exit(0);
  Result := WcxModule.GetPluginCapabilities;
end;

end.

