unit uGioFileSourceUtil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uFile, uGioFileSource, uGLib2, uGio2;


const
  CONST_DEFAULT_QUERY_INFO_ATTRIBUTES  =   FILE_ATTRIBUTE_STANDARD_TYPE +','+ FILE_ATTRIBUTE_STANDARD_NAME +','+
                                                FILE_ATTRIBUTE_STANDARD_DISPLAY_NAME +','+ FILE_ATTRIBUTE_STANDARD_SIZE +','+
                                                FILE_ATTRIBUTE_STANDARD_SYMLINK_TARGET +','+ FILE_ATTRIBUTE_TIME_MODIFIED +','+
                                                FILE_ATTRIBUTE_TIME_ACCESS +','+ FILE_ATTRIBUTE_TIME_CREATED +','+
                                                FILE_ATTRIBUTE_UNIX_MODE +','+ FILE_ATTRIBUTE_UNIX_UID +','+
                                                FILE_ATTRIBUTE_UNIX_GID;

procedure ShowError(AError: PGError);

procedure FillAndCount(Files: TFiles; CountDirs: Boolean; out NewFiles: TFiles;
                       out FilesCount: Int64; out FilesSize: Int64);

implementation

uses
  uShowMsg;

procedure ShowError(AError: PGError);
begin
  msgError(nil, AError^.message);
  g_error_free(AError);
end;

procedure FillAndCount(Files: TFiles; CountDirs: Boolean; out NewFiles: TFiles;
  out FilesCount: Int64; out FilesSize: Int64);
var
  I: Integer;
  aFile: TFile;

  procedure FillAndCountRec(const srcPath: UTF8String);
  var
    AFolder: PGFile;
    info: PGFileInfo;
    en: PGFileEnumerator;
    error, error_shortcut: PGError;
    AFileName: Pgchar;
  begin
    AFolder:= g_file_new_for_commandline_arg (Pgchar(srcPath));
    en := g_file_enumerate_children (AFolder, CONST_DEFAULT_QUERY_INFO_ATTRIBUTES,
                                       G_FILE_QUERY_INFO_NOFOLLOW_SYMLINKS, nil, @error);

    info:= g_file_enumerator_next_file (en, nil, @error);
          while Assigned(info) do
      begin
        AFileName:= g_file_info_get_name(info);

                if (aFileName = '.') or (aFileName = '..') then Continue;

        aFile:= TGioFileSource.CreateFile(srcPath, AFolder, info);
        NewFiles.Add(aFile);

        if aFile.IsDirectory then
          begin
            if CountDirs then Inc(FilesCount);
            FillAndCountRec(srcPath + aFileName + PathDelim);
          end
        else
          begin
            Inc(FilesSize, aFile.Size);
            Inc(FilesCount);
          end;

                info:= g_file_enumerator_next_file (en, nil, @error);
      end;
  end;

begin
  FilesCount:= 0;
  FilesSize:= 0;

  NewFiles := TFiles.Create(Files.Path);
  for I := 0 to Files.Count - 1 do
  begin
    aFile := Files[I];

    NewFiles.Add(aFile.Clone);

    if aFile.AttributesProperty.IsDirectory {and (not aFile.LinkProperty.IsLinkToDirectory)} then
      begin
        if CountDirs then
          Inc(FilesCount);
        FillAndCountRec(aFile.FullPath + DirectorySeparator);  // recursive browse child dir
      end
    else
      begin
        Inc(FilesCount);
        Inc(FilesSize, aFile.Size); // in first level we know file size -> use it
      end;
  end;
end;

end.

