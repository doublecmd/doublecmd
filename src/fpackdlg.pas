{
   Double Commander
   -------------------------------------------------------------------------
   File packing window

   Copyright (C) 2007-2010  Koblov Alexander (Alexx2000@mail.ru)

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

unit fPackDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, EditBtn, ExtCtrls, uWcxArchiveFileSource,
  uArchiveFileSource, uFile, uFileSource;

type

  { TfrmPackDlg }

  TfrmPackDlg = class(TForm)
    btnConfig: TButton;
    btnHelp: TButton;
    btnCancel: TButton;
    cbCreateSeparateArchives: TCheckBox;
    cbCreateSFX: TCheckBox;
    cbEncrypt: TCheckBox;
    cbMoveToArchive: TCheckBox;
    cbMultivolume: TCheckBox;
    btnOk: TButton;
    cbPackerList: TComboBox;
    cbOtherPlugins: TCheckBox;
    edtPackCmd: TDirectoryEdit;
    lblPrompt: TLabel;
    cbRecurse: TCheckBox;
    cbStoredir: TCheckBox;
    rgPacker: TRadioGroup;
    pnlOptions: TPanel;
    procedure btnConfigClick(Sender: TObject);
    procedure cbCreateSFXChange(Sender: TObject);
    procedure cbOtherPluginsChange(Sender: TObject);
    procedure edtPackCmdAcceptDirectory(Sender: TObject; var Value: String);
    procedure FormShow(Sender: TObject);
    procedure arbChange(Sender: TObject);

  private
    FArchiveType: String;
    FArchiveTypeCount: Integer;
    FExistsArchive : Boolean;
    procedure AddArchiveType(const FileExt, ArcType: UTF8String);
  public
    { public declarations }
  end;

  // Frees 'Files'.
  function ShowPackDlg(const SourceFileSource: IFileSource;
                       const TargetFileSource: IArchiveFileSource;
                       var Files: TFiles;
                       TargetArchivePath: String;
                       TargetPathInArchive: String;
                       bNewArchive : Boolean = True): Boolean;

implementation

uses
  WcxPlugin, uGlobs, uDCUtils, uFileSourceOperation, uLng, uOSUtils,
  uOperationsManager, fFileOpDlg, uArchiveFileSourceUtil, uMultiArchiveFileSource,
  uWcxArchiveCopyInOperation, uMultiArchiveCopyInOperation;

function ShowPackDlg(const SourceFileSource: IFileSource;
                     const TargetFileSource: IArchiveFileSource;
                     var Files: TFiles;
                     TargetArchivePath: String;
                     TargetPathInArchive: String;
                     bNewArchive : Boolean = True): Boolean;
var
  NewTargetFileSource: IArchiveFileSource = nil;
  aFlags : PtrInt;
  Operation: TFileSourceOperation;
  OperationHandle: TOperationHandle;
  ProgressDialog: TfrmFileOp;
  PackDialog: TfrmPackDlg;
begin
  PackDialog := TfrmPackDlg.Create(nil);
  try
    with PackDialog do
      begin
        FArchiveType:= 'none';
        if bNewArchive then  // create new archive
          (* if one file selected *)
          if Files.Count = 1 then
            begin
              edtPackCmd.Text := TargetArchivePath + Files[0].Name;
              edtPackCmd.Text := ChangeFileExt(edtPackCmd.Text, ExtensionSeparator + FArchiveType);
            end
          else
          (* if some files selected *)
            begin
              edtPackCmd.Text := TargetArchivePath + MakeFileName(Files.Path, 'archive') + ExtensionSeparator + FArchiveType;
            end
        else  // pack in exsists archive
        begin
          if Assigned(TargetFileSource) then
            edtPackCmd.Text := TargetFileSource.ArchiveFileName;
        end;

        Result:= (ShowModal = mrOK);

        if Result then
          begin
            if Assigned(TargetFileSource) then
            begin
              // Already have a target file source.
              // It must be an archive file source.
              if not (TargetFileSource.IsClass(TArchiveFileSource)) then
                raise Exception.Create('Invalid target file source type');

              NewTargetFileSource := TargetFileSource;
            end
            else
            begin
              // Create a new target file source.

              try
                // Check if there is an ArchiveFileSource for possible archive.
                NewTargetFileSource := GetArchiveFileSource(edtPackCmd.Text, FArchiveType);
              except
                on e: EModuleNotLoadedException do
                  begin
                    MessageDlg(e.Message, mtError, [mbOK], 0);
                    Exit;
                  end;
              end;
            end;

            if Assigned(NewTargetFileSource) then
              begin
                // Set flags according to user selection in the pack dialog.
                aFlags := 0;
                if cbMoveToArchive.Checked then aFlags := aFlags or PK_PACK_MOVE_FILES;
                if cbStoredir.Checked then aFlags := aFlags or PK_PACK_SAVE_PATHS;
                if cbEncrypt.Checked then aFlags := aFlags or PK_PACK_ENCRYPT;

                Operation := NewTargetFileSource.CreateCopyInOperation(
                                 SourceFileSource,
                                 Files,
                                 TargetPathInArchive);

                if Assigned(Operation) then
                begin
                  // TODO: Check if another operation is not running first (for WCX).

                  if NewTargetFileSource.IsInterface(IWcxArchiveFileSource) then
                    begin
                      with Operation as TWcxArchiveCopyInOperation do
                      begin
                        PackingFlags := aFlags;
                      end;
                    end
                  else if NewTargetFileSource.IsInterface(IMultiArchiveFileSource) then
                    begin
                      with Operation as TMultiArchiveCopyInOperation do
                      begin
                        if cbEncrypt.Checked then
                          Password:= InputBox(Caption, rsMsgPasswordEnter, EmptyStr);
                        if cbMultivolume.Checked then
                          VolumeSize:= StrToIntDef(InputBox(Caption, rsMsgVolumeSizeEnter, EmptyStr), 0);
                        PackingFlags := aFlags;
                      end;
                    end;

                  // Start operation.
                  OperationHandle := OperationsManager.AddOperation(Operation, ossAutoStart);

                  ProgressDialog := TfrmFileOp.Create(OperationHandle);
                  ProgressDialog.Show;
                end;
              end;
          end;
      end;

  finally
    FreeAndNil(PackDialog);
    if Assigned(Files) then
      FreeAndNil(Files);
  end;
end;

{ TfrmPackDlg }

procedure TfrmPackDlg.FormShow(Sender: TObject);
var
 I : Integer;
 sExt : String;
begin
  FArchiveTypeCount := 0;
  FExistsArchive := (FArchiveType <> 'none');

  // WCX plugins
  for I:=0 to gWCXPlugins.Count - 1 do
    if gWCXPlugins.Enabled[I] then
    begin
      if (gWCXPlugins.Flags[I] and PK_CAPS_NEW) = PK_CAPS_NEW then
        begin
          AddArchiveType(sExt, gWCXPlugins.Ext[I]);
        end;
    end;
  // MultiArc addons
  for I:= 0 to gMultiArcList.Count - 1 do
    if gMultiArcList[I].FEnabled and (gMultiArcList[I].FAdd <> EmptyStr) then
    begin
      AddArchiveType(sExt, gMultiArcList[I].FExtension);
    end;

    if (rgPacker.Items.Count > 0) and (rgPacker.ItemIndex < 0) then
      rgPacker.ItemIndex := 0;
    if cbPackerList.Items.Count > 0 then
      begin
        cbOtherPlugins.Visible := True;
        cbPackerList.Visible := True;

        if FExistsArchive then
          cbPackerList.Enabled:= False
        else
          cbOtherPlugins.Enabled := True;

        if cbPackerList.ItemIndex < 0 then
          cbPackerList.ItemIndex := 0;
      end
    else
      btnConfig.AnchorToCompanion(akTop, 6, rgPacker);
end;

procedure TfrmPackDlg.btnConfigClick(Sender: TObject);
var
  WcxFileSource: IWcxArchiveFileSource;
begin
  WcxFileSource := TWcxArchiveFileSource.CreateByArchiveName(edtPackCmd.Text);
  if Assigned(WcxFileSource) then
  try
    WcxFileSource.WcxModule.VFSConfigure(Handle);
  finally
    WcxFileSource := nil; // free interface
  end;
end;

procedure TfrmPackDlg.cbCreateSFXChange(Sender: TObject);
begin
  if cbCreateSFX.Checked then
    edtPackCmd.Text := ChangeFileExt(edtPackCmd.Text, GetSfxExt)
  else
    edtPackCmd.Text := ChangeFileExt(edtPackCmd.Text, ExtensionSeparator + FArchiveType);
end;

procedure TfrmPackDlg.cbOtherPluginsChange(Sender: TObject);
begin
  if cbOtherPlugins.Checked then
    begin
      FArchiveType:= cbPackerList.Text;
      edtPackCmd.Text := ChangeFileExt(edtPackCmd.Text, ExtensionSeparator + FArchiveType);
      rgPacker.ItemIndex := -1;
    end
  else
    begin
      if rgPacker.ItemIndex = -1 then
        rgPacker.ItemIndex := 0;
    end;
  cbPackerList.Enabled := cbOtherPlugins.Checked;
end;

procedure TfrmPackDlg.edtPackCmdAcceptDirectory(Sender: TObject; var Value: String);
begin
  Value := IncludeTrailingPathDelimiter(Value) + ExtractFileName(edtPackCmd.Text);
end;

procedure TfrmPackDlg.arbChange(Sender: TObject);
begin
  if rgPacker.ItemIndex >= 0 then
    begin
      FArchiveType:= rgPacker.Items[rgPacker.ItemIndex];
      edtPackCmd.Text := ChangeFileExt(edtPackCmd.Text, ExtensionSeparator + FArchiveType);
      cbOtherPlugins.Checked := False;
    end;
end;

procedure TfrmPackDlg.AddArchiveType(const FileExt, ArcType: UTF8String);
var
  iIndex: Integer;
begin
  // First 9 plugins we display as  RadioButtons
  if FArchiveTypeCount < 9 then
    begin
      iIndex := rgPacker.Items.Add(ArcType);
      if FExistsArchive then
        if (FileExt = ArcType) then
          rgPacker.ItemIndex := iIndex
        else
          rgPacker.Controls[iIndex + 1].Enabled := False;
      FArchiveTypeCount := FArchiveTypeCount + 1;
     end
   else  // Other plugins we add in ComboBox
    begin
      iIndex := cbPackerList.Items.Add(ArcType);
      if FExistsArchive and (FileExt = ArcType) then
        cbPackerList.ItemIndex := iIndex;
    end;
end;


initialization
  {$I fpackdlg.lrs}

end.

