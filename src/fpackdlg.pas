{
   Double Commander
   -------------------------------------------------------------------------
   File packing window

   Copyright (C) 2007-2011  Koblov Alexander (Alexx2000@mail.ru)

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
  SysUtils, Forms, Controls, Dialogs, StdCtrls, EditBtn, ExtCtrls, uWcxArchiveFileSource,
  uArchiveFileSource, uFile, uFileSource, Classes;

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
    cbPutInTarFirst: TCheckBox;
    edtPackCmd: TDirectoryEdit;
    lblPrompt: TLabel;
    cbStoreDir: TCheckBox;
    rgPacker: TRadioGroup;
    pnlOptions: TPanel;
    procedure btnConfigClick(Sender: TObject);
    procedure cbCreateSeparateArchivesChange(Sender: TObject);
    procedure cbCreateSFXClick(Sender: TObject);
    procedure cbOtherPluginsChange(Sender: TObject);
    procedure cbPutInTarFirstChange(Sender: TObject);
    procedure edtPackCmdAcceptDirectory(Sender: TObject; var Value: String);
    procedure FormShow(Sender: TObject);
    procedure arbChange(Sender: TObject);

  private
    FTarExt,
    FArchiveExt,
    FArchiveName,
    FArchiveType: UTF8String;
    FArchiveTypeCount: Integer;
    FHasFolder,
    FExistsArchive : Boolean;
    FSourceFileSource: IFileSource;
    FCustomParams: UTF8String;
    procedure SwitchOptions(ArcTypeChange: Boolean);
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

{$R *.lfm}

uses
  StrUtils, WcxPlugin, uGlobs, uDCUtils, uFileSourceOperation, uLng, uOSUtils,
  uOperationsManager, fFileOpDlg, uArchiveFileSourceUtil, uMultiArchiveFileSource,
  uWcxArchiveCopyInOperation, uMultiArchiveCopyInOperation, uMasks;

function ShowPackDlg(const SourceFileSource: IFileSource;
                     const TargetFileSource: IArchiveFileSource;
                     var Files: TFiles;
                     TargetArchivePath: String;
                     TargetPathInArchive: String;
                     bNewArchive : Boolean = True): Boolean;
var
  I: Integer;
  NewTargetFileSource: IArchiveFileSource = nil;
  aFlags : PtrInt;
  Operation: TFileSourceOperation;
  OperationHandle: TOperationHandle;
  ProgressDialog: TfrmFileOp;
  PackDialog: TfrmPackDlg;
  aFile: TFile = nil;
  aFiles: TFiles = nil;

  procedure Pack(var FilesToPack: TFiles; StartingState: TOperationStartingState);
  begin
    with PackDialog do
    begin
      if Assigned(NewTargetFileSource) then
        begin
          // Set flags according to user selection in the pack dialog.
          aFlags := 0;
          if cbMoveToArchive.Checked then aFlags := aFlags or PK_PACK_MOVE_FILES;
          if cbStoreDir.Checked then aFlags := aFlags or PK_PACK_SAVE_PATHS;
          if cbEncrypt.Checked then aFlags := aFlags or PK_PACK_ENCRYPT;

          Operation := NewTargetFileSource.CreateCopyInOperation(
                                 SourceFileSource,
                                 FilesToPack,
                                 TargetPathInArchive);

          if Assigned(Operation) then
            begin
              // TODO: Check if another operation is not running first (for WCX).

              if NewTargetFileSource.IsInterface(IWcxArchiveFileSource) then
                begin
                  with Operation as TWcxArchiveCopyInOperation do
                  begin
                    PackingFlags := aFlags;
                    TarBefore:= cbPutInTarFirst.Checked;
                  end;
                end
              else if NewTargetFileSource.IsInterface(IMultiArchiveFileSource) then
                begin
                  with Operation as TMultiArchiveCopyInOperation do
                  begin
                    if cbEncrypt.Checked then
                      Password:= InputBox(Caption, rsMsgPasswordEnter, EmptyStr);
                    if cbMultivolume.Checked then
                      VolumeSize:= InputBox(Caption, rsMsgVolumeSizeEnter, EmptyStr);
                    PackingFlags := aFlags;
                    CustomParams:= FCustomParams;
                    TarBefore:= cbPutInTarFirst.Checked;
                  end;
                end;

                // Start operation.
                OperationHandle := OperationsManager.AddOperation(Operation, StartingState);

                ProgressDialog := TfrmFileOp.Create(OperationHandle);
                ProgressDialog.Show;
              end;
            end;
    end;
  end;

begin
  PackDialog := TfrmPackDlg.Create(nil);
  try
    with PackDialog do
      begin
        FArchiveType:= 'none';
        FSourceFileSource:= SourceFileSource;
        if bNewArchive then  // create new archive
          begin
            if Files.Count = 1 then // if one file selected
              begin
                FHasFolder:= Files[0].IsDirectory;
                FArchiveName:= Files[0].NameNoExt;
                edtPackCmd.Text := TargetArchivePath + FArchiveName + ExtensionSeparator + FArchiveType;
              end
            else   // if some files selected
              begin
                FHasFolder:= False;
                for I:= 0 to Files.Count - 1 do
                begin
                  if Files[I].IsDirectory then
                  begin
                    FHasFolder:= True;
                    Break;
                  end;
                end;
                FArchiveName:= MakeFileName(Files.Path, 'archive');
                edtPackCmd.Text := TargetArchivePath + FArchiveName + ExtensionSeparator + FArchiveType;
              end
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
            else // Create a new target file source.
            begin
              // If create separate archives, one per selected file/dir
              if cbCreateSeparateArchives.Checked then
                try
                  for I:= 0 to Files.Count - 1 do
                  begin
                    // Fill files to pack
                    aFiles:= TFiles.Create(Files.Path);
                    aFiles.Add(Files[I].Clone);

                    try
                      try
                        // Check if there is an ArchiveFileSource for possible archive.
                        aFile := SourceFileSource.CreateFileObject(ExtractFilePath(edtPackCmd.Text));
                        aFile.Name := Files[I].Name + FTarExt + FArchiveExt;
                        NewTargetFileSource := GetArchiveFileSource(SourceFileSource, aFile, FArchiveType);
                      except
                        on e: EFileSourceException do
                          begin
                            MessageDlg(e.Message, mtError, [mbOK], 0);
                            Exit;
                          end;
                      end;

                      // Pack current item, if files count > 1 then put to queue
                      if (I > 0) then
                        Pack(aFiles, ossQueueLast)
                      else
                        Pack(aFiles, ossAutoStart);
                    finally
                      FreeAndNil(aFile);
                    end;
                  end;

                finally
                  FreeAndNil(aFiles);
                end
              else
                begin
                  try
                    // Check if there is an ArchiveFileSource for possible archive.
                    aFile := SourceFileSource.CreateFileObject(ExtractFilePath(edtPackCmd.Text));
                    aFile.Name := ExtractFileName(edtPackCmd.Text);
                    NewTargetFileSource := GetArchiveFileSource(SourceFileSource, aFile, FArchiveType);
                  except
                    on e: EFileSourceException do
                      begin
                        MessageDlg(e.Message, mtError, [mbOK], 0);
                        Exit;
                      end;
                  end;

                  // Pack files
                  Pack(Files, ossAutoStart);
                end;
            end;

            // Save last used packer
            gLastUsedPacker:= FArchiveType;
          end;
      end;

  finally
    FreeAndNil(PackDialog);
    if Assigned(Files) then
      FreeAndNil(Files);
    if Assigned(aFile) then
      FreeAndNil(aFile);
  end;
end;

const
  TAR_EXT = '.tar';

{ TfrmPackDlg }

procedure TfrmPackDlg.FormShow(Sender: TObject);
var
 I, J : Integer;
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
          AddArchiveType(FArchiveType, gWCXPlugins.Ext[I]);
        end;
    end;
  // MultiArc addons
  for I:= 0 to gMultiArcList.Count - 1 do
    if gMultiArcList[I].FEnabled and (gMultiArcList[I].FAdd <> EmptyStr) then
    begin
      J:= 1;
      repeat
        sExt:= ExtractDelimited(J, gMultiArcList[I].FExtension, [',']);
        if Length(sExt) = 0 then Break;
        AddArchiveType(FArchiveType, sExt);
        Inc(J);
      until False;
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
  WcxFileSource := TWcxArchiveFileSource.CreateByArchiveName(FSourceFileSource, edtPackCmd.Text);
  if Assigned(WcxFileSource) then // WCX plugin
    try
      WcxFileSource.WcxModule.VFSConfigure(Handle);
    finally
      WcxFileSource := nil; // free interface
    end
  else // MultiArc addon
    begin
      FCustomParams:= InputBox(Caption, rsMsgArchiverCustomParams, FCustomParams);
    end;
end;

procedure TfrmPackDlg.cbCreateSeparateArchivesChange(Sender: TObject);
begin
  if cbCreateSeparateArchives.Checked then
    edtPackCmd.Text:= ExtractFilePath(edtPackCmd.Text) + '*.*' + FTarExt + FArchiveExt
  else
    edtPackCmd.Text:= ExtractFilePath(edtPackCmd.Text) + FArchiveName + FTarExt + FArchiveExt;
end;

procedure TfrmPackDlg.cbCreateSFXClick(Sender: TObject);
var
  State: Boolean;
begin
  if cbCreateSFX.Tag = 0 then
  begin
    cbCreateSFX.Tag:= 1;
    // Save check box state
    State:= cbCreateSFX.Checked;
    if State then
      FArchiveExt:= GetSfxExt
    else
      FArchiveExt:= ExtensionSeparator + FArchiveType;
    edtPackCmd.Text := ChangeFileExt(edtPackCmd.Text, FArchiveExt);
    // Switch archiver options
    SwitchOptions(False);
    // Restore check box state
    cbCreateSFX.Checked:= State;
    cbCreateSFX.Tag:= 0;
  end;
end;

procedure TfrmPackDlg.cbOtherPluginsChange(Sender: TObject);
begin
  if cbOtherPlugins.Checked then
    begin
      FArchiveType:= cbPackerList.Text;
      FArchiveExt:= ExtensionSeparator + FArchiveType;
      edtPackCmd.Text := ChangeFileExt(edtPackCmd.Text, FArchiveExt);
      rgPacker.ItemIndex := -1;
    end
  else
    begin
      if rgPacker.ItemIndex = -1 then
        rgPacker.ItemIndex := 0;
    end;
  FCustomParams:= EmptyStr;
  cbPackerList.Enabled := cbOtherPlugins.Checked;
  SwitchOptions(True);
end;

procedure TfrmPackDlg.cbPutInTarFirstChange(Sender: TObject);
begin
  if cbPutInTarFirst.Checked then
    begin
      FTarExt:= TAR_EXT;
      edtPackCmd.Text := ChangeFileExt(edtPackCmd.Text, TAR_EXT + FArchiveExt);
    end
  else
    begin
      FTarExt:= EmptyStr;
      edtPackCmd.Text := StringReplace(edtPackCmd.Text, TAR_EXT + FArchiveExt, FArchiveExt, [rfIgnoreCase]);
    end;
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
      FArchiveExt:= ExtensionSeparator + FArchiveType;
      edtPackCmd.Text := ChangeFileExt(edtPackCmd.Text, FArchiveExt);
      cbOtherPlugins.Checked := False;
    end;
  FCustomParams:= EmptyStr;
  SwitchOptions(True);
end;

procedure TfrmPackDlg.SwitchOptions(ArcTypeChange: Boolean); // Ugly but working
var
  I: LongInt;
  sCmd: String;
begin
  if ArcTypeChange then
  begin
    // Reset some options
    cbCreateSFX.Checked:= False;
  end;

  // WCX plugins
  for I:= 0 to gWCXPlugins.Count - 1 do
    if gWCXPlugins.Enabled[I] and (gWCXPlugins.Ext[I] = FArchiveType) then
    begin
      // If plugin supports packing with password
      EnableControl(cbEncrypt, ((gWCXPlugins.Flags[I] and PK_CAPS_ENCRYPT) <> 0));
      // If archive can not contain multiple files
      if ((gWCXPlugins.Flags[I] and PK_CAPS_MULTIPLE) = 0) then
      begin
        // If file list contain directory then
        // put to the tar archive first is needed
        if not FHasFolder then
          cbCreateSeparateArchives.Checked:= True
        else
          begin
            cbPutInTarFirst.Checked:= True;
            EnableControl(cbPutInTarFirst, False);
          end;
      end
      else
        begin
          cbPutInTarFirst.Checked:= False;
          EnableControl(cbPutInTarFirst, True);
          cbCreateSeparateArchives.Checked:= False;
        end;
      // Options that supported by plugins
      EnableControl(cbStoreDir, True);
      // Options that don't supported by plugins
      EnableControl(cbMultivolume, False);
      Exit;
    end;

  // MultiArc addons
  for I := 0 to gMultiArcList.Count - 1 do
    with gMultiArcList.Items[I] do
    begin
      if FEnabled and MatchesMaskList(FArchiveType, FExtension, ',') then
      begin
        // Archive can contain multiple files
        cbCreateSeparateArchives.Checked:= False;
        // If addon supports create self extracting archive
        EnableControl(cbCreateSFX, (Length(FAddSelfExtract) <> 0));

        if cbCreateSFX.Enabled and cbCreateSFX.Checked then
          sCmd:= FAddSelfExtract
        else
          sCmd:= FAdd;

        // If addon supports create multi volume archive
        EnableControl(cbMultivolume, (Pos('%V', sCmd) <> 0));
        // If addon supports packing with password
        EnableControl(cbEncrypt, (Pos('%W', sCmd) <> 0));

        // Options that supported by addons
        EnableControl(cbPutInTarFirst, True);
        // Options that don't supported by addons
        cbStoreDir.Checked:= True;
        EnableControl(cbStoreDir, False);
        Exit;
      end;
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
        begin
          if (FileExt = ArcType) then
            rgPacker.ItemIndex := iIndex
          else
            rgPacker.Controls[iIndex + 1].Enabled := False;
        end
      else if (gLastUsedPacker = ArcType) then
        begin
           rgPacker.ItemIndex := iIndex;
        end;
      FArchiveTypeCount := FArchiveTypeCount + 1;
     end
   else  // Other plugins we add in ComboBox
    begin
      iIndex := cbPackerList.Items.Add(ArcType);
      if (gLastUsedPacker = ArcType) or (FExistsArchive and (FileExt = ArcType)) then
        cbPackerList.ItemIndex := iIndex;
    end;
end;

end.

