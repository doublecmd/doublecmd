{
   Double Commander
   -------------------------------------------------------------------------
   File packing window

   Copyright (C) 2007-2020 Alexander Koblov (alexx2000@mail.ru)

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

unit fPackDlg;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, Controls, Dialogs, StdCtrls, EditBtn, ExtCtrls, Buttons,
  Menus, DividerBevel, uWcxArchiveFileSource, uArchiveFileSource, uFile,
  uFileSource, Classes, fButtonForm, uFileSourceOperation;

type

  { TfrmPackDlg }

  TfrmPackDlg = class(TfrmButtonForm)
    btnConfig: TButton;
    btnHelp: TButton;
    cbCreateSeparateArchives: TCheckBox;
    cbCreateSFX: TCheckBox;
    cbEncrypt: TCheckBox;
    cbMoveToArchive: TCheckBox;
    cbMultivolume: TCheckBox;
    cbPackerList: TComboBox;
    cbOtherPlugins: TCheckBox;
    cbPutInTarFirst: TCheckBox;
    DividerBevel: TDividerBevel;
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
    FArchiveExt,
    FArchiveName,
    FArchiveType: String;
    FArchiveTypeCount: Integer;
    FHasFolder,
    FNewArchive,
    FExistsArchive : Boolean;
    FSourceFileSource: IFileSource;
    FTargetFileSource:  IArchiveFileSource;
    FCustomParams: String;
    FTargetPathInArchive: String;
    procedure SwitchOptions(ArcTypeChange: Boolean);
    procedure ChangeArchiveExt(const NewArcExt: String);
    procedure AddArchiveType(const FileExt, ArcType: String);
    procedure OnPackCopyOutStateChanged(Operation: TFileSourceOperation;
                                        State: TFileSourceOperationState);
    procedure PackFiles(const SourceFileSource: IFileSource; var Files: TFiles);
  public
    { public declarations }
  end;

  // Frees 'Files'.
  procedure ShowPackDlg(TheOwner: TComponent;
                       const SourceFileSource: IFileSource;
                       const TargetFileSource: IArchiveFileSource;
                       var Files: TFiles;
                       TargetArchivePath: String;
                       TargetPathInArchive: String;
                       bNewArchive : Boolean = True);

implementation

{$R *.lfm}

uses
  StrUtils, WcxPlugin, uGlobs, uDCUtils, uLng, uOSUtils,
  uOperationsManager, uArchiveFileSourceUtil, uMultiArchiveFileSource,
  uWcxArchiveCopyInOperation, uMultiArchiveCopyInOperation, uMasks,
  DCStrUtils, uMultiArc, uWcxModule, uTempFileSystemFileSource,
  uFileSourceCopyOperation, uShowForm;

procedure ShowPackDlg(TheOwner: TComponent;
                     const SourceFileSource: IFileSource;
                     const TargetFileSource: IArchiveFileSource;
                     var Files: TFiles;
                     TargetArchivePath: String;
                     TargetPathInArchive: String;
                     bNewArchive : Boolean = True);
var
  I: Integer;
  PackDialog: TfrmPackDlg;
begin
  PackDialog := TfrmPackDlg.Create(TheOwner);
{$IF DEFINED(LCLGTK2)}
  // TRadioGroup.ItemIndex:= -1 will not work under Gtk2
  // if items have been added dynamically, this workaround fixes it
  PackDialog.rgPacker.Items.Add(EmptyStr);
  PackDialog.rgPacker.Items.Clear;
{$ENDIF}
  try
    with PackDialog do
    begin
      FArchiveType:= 'none';
      FNewArchive:= bNewArchive;
      FSourceFileSource:= SourceFileSource;
      FTargetFileSource:= TargetFileSource;
      FTargetPathInArchive:= TargetPathInArchive;
      FArchiveExt:= ExtensionSeparator + FArchiveType;
      if bNewArchive then  // create new archive
        begin
          if Files.Count = 1 then // if one file selected
            begin
              FArchiveName:= Files[0].NameNoExt;
              FHasFolder:= Files[0].IsDirectory or Files[0].IsLinkToDirectory;
              edtPackCmd.Text := TargetArchivePath + FArchiveName + ExtensionSeparator + FArchiveType;
            end
          else   // if some files selected
            begin
              FHasFolder:= False;
              for I:= 0 to Files.Count - 1 do
              begin
                if Files[I].IsDirectory or Files[I].IsLinkToDirectory then
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

      if (ShowModal = mrOK) then
      begin
        case PrepareData(SourceFileSource, Files, @OnPackCopyOutStateChanged) of
          pdrInCallback:
            PackDialog:= nil;
          pdrSynchronous:
            PackFiles(SourceFileSource, Files);
        end;
      end;
    end;
  finally
    FreeAndNil(PackDialog);
    FreeAndNil(Files);
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

    if (rgPacker.Items.Count > 0) and (rgPacker.ItemIndex < 0) and (not cbOtherPlugins.Checked) then
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
  try
    WcxFileSource := TWcxArchiveFileSource.CreateByArchiveName(FSourceFileSource, edtPackCmd.Text, True);
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
  except
    on e: Exception do
      MessageDlg(e.Message, mtError, [mbOK], 0);
  end;
end;

procedure TfrmPackDlg.cbCreateSeparateArchivesChange(Sender: TObject);
begin
  if cbCreateSeparateArchives.Checked then
    edtPackCmd.Text:= ExtractFilePath(edtPackCmd.Text) + '*.*' + FArchiveExt
  else
    edtPackCmd.Text:= ExtractFilePath(edtPackCmd.Text) + FArchiveName + FArchiveExt;
end;

procedure TfrmPackDlg.cbCreateSFXClick(Sender: TObject);
var
  State: Boolean;
  ANewExt: String;
begin
  if cbCreateSFX.Tag = 0 then
  begin
    cbCreateSFX.Tag:= 1;
    // Save check box state
    State:= cbCreateSFX.Checked;
    if State then
      ANewExt:= GetSfxExt
    else begin
      ANewExt:= ExtensionSeparator + FArchiveType;
    end;
    ChangeArchiveExt(ANewExt);
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
      SwitchOptions(True);
      ChangeArchiveExt(FArchiveType);
      rgPacker.ItemIndex := -1;
    end
  else begin
    if rgPacker.ItemIndex = -1 then
      rgPacker.ItemIndex := 0;
  end;
  FCustomParams:= EmptyStr;
  cbPackerList.Enabled := cbOtherPlugins.Checked;
end;

procedure TfrmPackDlg.cbPutInTarFirstChange(Sender: TObject);
begin
  if cbPutInTarFirst.Checked then
    ChangeArchiveExt(FArchiveExt)
  else if AnsiStartsText(TAR_EXT, FArchiveExt) then begin
    ChangeArchiveExt(Copy(FArchiveExt, Length(TAR_EXT) + 1, MaxInt));
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
    SwitchOptions(True);
    ChangeArchiveExt(FArchiveType);
    cbOtherPlugins.Checked := False;
  end;
  FCustomParams:= EmptyStr;
end;

procedure TfrmPackDlg.SwitchOptions(ArcTypeChange: Boolean); // Ugly but working
var
  I: LongInt;
  sCmd: String;
begin
  cbPutInTarFirst.OnChange:= nil;

  try
    if ArcTypeChange then
    begin
      // Reset some options
      cbCreateSFX.Checked:= False;
    end;

    // WCX plugins
    for I:= 0 to gWCXPlugins.Count - 1 do
    begin
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
            sCmd:= LowerCase(FArchiveType);
            cbPutInTarFirst.Checked:= False;
            EnableControl(cbPutInTarFirst, not ((sCmd = 'tar') or StrBegins(sCmd, 'tar.')));
            cbCreateSeparateArchives.Checked:= False;
          end;
        // Options that supported by plugins
        EnableControl(cbStoreDir, True);
        // Options that don't supported by plugins
        EnableControl(cbMultivolume, False);
        Exit;
      end;
    end;

    // MultiArc addons
    for I := 0 to gMultiArcList.Count - 1 do
    begin
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

          // If archive can not contain multiple files
          if (mafFileNameList in FFlags) then
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
          else begin
            sCmd:= LowerCase(FArchiveType);
            cbPutInTarFirst.Checked:= False;
            EnableControl(cbPutInTarFirst, not ((sCmd = 'tar') or StrBegins(sCmd, 'tar.')));
            cbCreateSeparateArchives.Checked:= False;
          end;

          // Options that don't supported by addons
          cbStoreDir.Checked:= True;
          EnableControl(cbStoreDir, False);
          Exit;
        end;
      end;
    end;
  finally
    cbPutInTarFirst.OnChange:= @cbPutInTarFirstChange;
  end;
end;

procedure TfrmPackDlg.ChangeArchiveExt(const NewArcExt: String);
var
  AOldExt, ATarExt: String;
begin
  AOldExt:= FArchiveExt;
  ATarExt:= IfThen(cbPutInTarFirst.Checked, TAR_EXT);
  if StrBegins(NewArcExt, ExtensionSeparator) then
  begin
    if AnsiStartsText(ATarExt, NewArcExt) then
      FArchiveExt:= NewArcExt
    else
      FArchiveExt:= ATarExt + NewArcExt;
  end
  else begin
    FArchiveExt:= ATarExt + ExtensionSeparator + NewArcExt;
  end;
  if AnsiEndsText(AOldExt, edtPackCmd.Text) then
  begin
    edtPackCmd.Text:= Copy(edtPackCmd.Text, 1, Length(edtPackCmd.Text) - Length(AOldExt)) + FArchiveExt;
  end;
end;

procedure TfrmPackDlg.AddArchiveType(const FileExt, ArcType: String);
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
      begin
        cbPackerList.ItemIndex := iIndex;
        cbOtherPlugins.Checked := True;
      end;
    end;
end;

procedure TfrmPackDlg.OnPackCopyOutStateChanged(Operation: TFileSourceOperation;
                                                State: TFileSourceOperationState);
var
  aFiles: TFiles;
  aFileSource: ITempFileSystemFileSource;
  aCopyOutOperation: TFileSourceCopyOperation absolute Operation;
begin
  if (State = fsosStopped) then
  try
    if (Operation.Result = fsorFinished) then
    begin
      aFileSource := aCopyOutOperation.TargetFileSource as ITempFileSystemFileSource;
      aFiles := aCopyOutOperation.SourceFiles.Clone;
      ChangeFileListRoot(aFileSource.FileSystemRoot, aFiles);
      PackFiles(aFileSource, aFiles);
    end;
  finally
    Free;
  end;
end;

procedure TfrmPackDlg.PackFiles(const SourceFileSource: IFileSource;
  var Files: TFiles);
var
  I: Integer;
  aFlags : PtrInt;
  aFile: TFile = nil;
  aFiles: TFiles = nil;
  Operation: TFileSourceOperation;
  NewTargetFileSource: IArchiveFileSource = nil;

  procedure Pack(var FilesToPack: TFiles; QueueId: TOperationsManagerQueueIdentifier);
  var
    sPassword,
    sPasswordTmp: String;
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
                               FTargetPathInArchive);

        if Assigned(Operation) then
          begin
            if NewTargetFileSource.IsInterface(IWcxArchiveFileSource) then
              begin
                with Operation as TWcxArchiveCopyInOperation do
                begin
                  PackingFlags:= aFlags;
                  CreateNew:= FNewArchive;
                  TarBefore:= cbPutInTarFirst.Checked;
                end;
              end
            else if NewTargetFileSource.IsInterface(IMultiArchiveFileSource) then
              begin
                with Operation as TMultiArchiveCopyInOperation do
                begin
                  if cbEncrypt.Checked then
                    repeat
                      if not InputQuery(Caption, rsMsgPasswordEnter, True, sPassword) then
                        Exit;
                      if gRepeatPassword then
                        begin
                          if not InputQuery(Caption, rsMsgPasswordVerify, True, sPasswordTmp) then
                            Exit;
                        end
                      else
                        sPasswordTmp:= sPassword;
                      if sPassword <> sPasswordTmp then
                        ShowMessage(rsMsgPasswordDiff)
                      else
                        Password:= sPassword;
                    until sPassword = sPasswordTmp;
                  if cbMultivolume.Checked then
                    VolumeSize:= InputBox(Caption, rsMsgVolumeSizeEnter, EmptyStr);
                  PackingFlags := aFlags;
                  CreateNew:= FNewArchive;
                  CustomParams:= FCustomParams;
                  TarBefore:= cbPutInTarFirst.Checked;
                end;
              end;

            // Start operation.
            OperationsManager.AddOperation(Operation, QueueId, False, True);
          end;
      end;
  end;

var
  QueueId: TOperationsManagerQueueIdentifier;
begin
  if Assigned(FTargetFileSource) then
  begin
    // Already have a target file source.
    // It must be an archive file source.
    if not (FTargetFileSource.IsClass(TArchiveFileSource)) then
      raise Exception.Create('Invalid target file source type');

    NewTargetFileSource := FTargetFileSource;
  end
  else // Create a new target file source.
  begin
    // If create separate archives, one per selected file/dir
    if cbCreateSeparateArchives.Checked then
      begin
        // If files count > 1 then put to queue
        if (Files.Count > 1) and (QueueIdentifier = FreeOperationsQueueId) then
          QueueId := OperationsManager.GetNewQueueIdentifier
        else begin
          QueueId := QueueIdentifier;
        end;
        // Pack all selected files
        for I:= 0 to Files.Count - 1 do
        begin
          // Fill files to pack
          aFiles:= TFiles.Create(Files.Path);
          try
            aFiles.Add(Files[I].Clone);
            FArchiveName:= GetAbsoluteFileName(Files.Path, edtPackCmd.Text);
            try
              // Check if there is an ArchiveFileSource for possible archive.
              aFile := SourceFileSource.CreateFileObject(ExtractFilePath(FArchiveName));
              try
                aFile.Name := Files[I].Name + FArchiveExt;
                NewTargetFileSource := GetArchiveFileSource(SourceFileSource, aFile, FArchiveType, False, True);
              finally
                FreeAndNil(aFile);
              end;
            except
              on E: Exception do
              begin
                if (E is EFileSourceException) or (E is EWcxModuleException) then
                begin
                  if MessageDlg(E.Message, mtError, [mbIgnore, mbAbort], 0) = mrIgnore then
                    Continue;
                  Exit;
                end;
                raise;
              end;
            end;
            // Pack current item
            Pack(aFiles, QueueId);
          finally
            FreeAndNil(aFiles);
          end;
        end; // for
      end
    else
      begin
        FArchiveName:= GetAbsoluteFileName(Files.Path, edtPackCmd.Text);
        try
          // Check if there is an ArchiveFileSource for possible archive.
          aFile := SourceFileSource.CreateFileObject(ExtractFilePath(FArchiveName));
          try
            aFile.Name := ExtractFileName(FArchiveName);
            NewTargetFileSource := GetArchiveFileSource(SourceFileSource, aFile, FArchiveType, False, True);
          finally
            FreeAndNil(aFile);
          end;
        except
          on E: Exception do
          begin
            if (E is EFileSourceException) or (E is EWcxModuleException) then
            begin
              MessageDlg(E.Message, mtError, [mbOK], 0);
              Exit;
            end;
            raise;
          end;
        end;
        // Pack files
        Pack(Files, QueueIdentifier);
      end;
  end;
  // Save last used packer
  gLastUsedPacker:= FArchiveType;
end;

end.

