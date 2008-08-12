unit fTweakPlugin; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, uTypes;

type

  { TfrmTweakPlugin }

  TfrmTweakPlugin = class(TForm)
    btnAdd: TButton;
    btnCancel: TButton;
    btnChange: TButton;
    btnDefault: TButton;
    btnOK: TButton;
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
    edtDetectStr: TEdit;
    edtName: TEdit;
    edtPlugin: TEdit;
    GroupBox1: TGroupBox;
    lblDetectStr: TLabel;
    lblName: TLabel;
    ledPlugin: TLabeledEdit;
    lblExtension: TLabel;
    lblFlags: TLabel;
    lblFlagsValue: TLabel;
    lblPackerPlugin: TLabel;
    lblPlugin: TLabel;
    nbTweakAll: TNotebook;
    pgTweakPacker: TPage;
    pgTweakOther: TPage;
    pnlTweak: TGroupBox;
  private
    { private declarations }
  public
    { public declarations }
  end; 

function ShowTweakPluginDlg(PluginType: TPluginType; PluginIndex: Integer): Boolean;

implementation
uses
  uGlobs;

function ShowTweakPluginDlg(PluginType: TPluginType; PluginIndex: Integer): Boolean;
begin
  with TfrmTweakPlugin.Create(Application) do
  try
    case PluginType of
    ptWCX:
      begin
        nbTweakAll.PageIndex:= 0;
      end;
    ptWDX:
      begin
        nbTweakAll.PageIndex:= 1;
        ledPlugin.Text:= WDXPlugins.GetWdxModule(PluginIndex).FileName;
        edtDetectStr.Text:= WDXPlugins.GetWdxModule(PluginIndex).DetectStr;
        edtName.Text:= WDXPlugins.GetWdxModule(PluginIndex).Name;
      end;
    ptWFX:
      begin
        nbTweakAll.PageIndex:= 1;
        ledPlugin.Text:= gWFXPlugins.FileName[PluginIndex];
        lblDetectStr.Visible:= False;
        edtDetectStr.Visible:= False;
        edtName.Text:= gWFXPlugins.Name[PluginIndex];
      end;
    end;
    Result:= (ShowModal = mrOK);
    if Result then
      case PluginType of
      ptWCX:
        begin

        end;
      ptWDX:
        begin
          WDXPlugins.GetWdxModule(PluginIndex).FileName:= ledPlugin.Text;
          WDXPlugins.GetWdxModule(PluginIndex).DetectStr:= edtDetectStr.Text;
          WDXPlugins.GetWdxModule(PluginIndex).Name:= edtName.Text;
        end;
      ptWFX:
        begin
          gWFXPlugins.FileName[PluginIndex]:= ledPlugin.Text;
          gWFXPlugins.Name[PluginIndex]:= edtName.Text;
        end;
      end;
  finally
    Free;
  end;
end;

initialization
  {$I ftweakplugin.lrs}

end.

