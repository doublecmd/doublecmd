unit uThumbnails;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Types, fgl, DCClassesUtf8, uFile;

type

  { TCreatePreviewHandler }

  TCreatePreviewHandler = function(const aFileName: UTF8String; aSize: TSize): TBitmap;

  { TBitmapList }

  TBitmapList = specialize TFPGObjectList<TBitmap>;

  { TThumbnailManager }

  TThumbnailManager = class
  private
    FThumbPath: UTF8String;
    FBackColor: TColor;
    FProviderList: array of TCreatePreviewHandler; static;
    function GetPreviewFileExt(const sFileExt: UTF8String): UTF8String;
    function GetPreviewFileName(const sFileName: UTF8String): UTF8String;
    function CreatePreviewImage(const Graphic: TGraphic): TBitmap;
    function CreatePreviewText(const sFileName: UTF8String): TBitmap;
    function ReadMetaData(const aFile: TFile; FileStream: TFileStreamEx): Boolean;
    function WriteMetaData(const aFile: TFile; FileStream: TFileStreamEx): Boolean;
    class function ReadFileName(const aThumb: UTF8String; out aFileName: UTF8String): Boolean;
  public
    constructor Create(BackColor: TColor);
    function CreatePreview(const aFile: TFile): TBitmap;
    function CreatePreview(const FullPathToFile: UTF8String): TBitmap;
    function RemovePreview(const FullPathToFile: UTF8String): Boolean;
  public
    class procedure CompactCache;
    class procedure RegisterProvider(Provider: TCreatePreviewHandler);
  end;

implementation

uses
  LCLProc, FileUtil, uDebug, DCOSUtils, uFileProcs, DCStrUtils, uReSample,
  uGlobsPaths, uGlobs, uPixmapManager, URIParser, md5, uFileSystemFileSource;

const
  ThumbSign: QWord = $0000235448554D42; // '#0 #0 # T H U M B'

function TThumbnailManager.GetPreviewFileExt(const sFileExt: UTF8String): UTF8String;
begin
  if (sFileExt = 'jpg') or (sFileExt = 'jpeg') or (sFileExt = 'bmp') then
    Result:= 'jpg'
  else
    Result:= 'png';
end;

function TThumbnailManager.GetPreviewFileName(const sFileName: UTF8String): UTF8String;
begin
  Result:= MD5Print(MD5String(sFileName));
end;

function TThumbnailManager.CreatePreviewImage(const Graphic: TGraphic): TBitmap;
var
  x, y: LongInt;
  bmpTemp: TBitmap = nil;
begin
  try
    // Width and height of thumb
    if  Graphic.Width > Graphic.Height then
      begin
        x:= gThumbSize.cx;
        y:= x * Graphic.Height div Graphic.Width;
        if y > gThumbSize.cy then
          begin
            y:= gThumbSize.cy;
            x:= y * Graphic.Width div Graphic.Height;
          end;
      end
    else
      begin
        y:= gThumbSize.cy;
        x:= y * Graphic.Width div Graphic.Height;
      end;
    bmpTemp:= TBitMap.Create;
    bmpTemp.Assign(Graphic);
    Result:= TBitMap.Create;
    Result.SetSize(x, y);
    Stretch(bmpTemp, Result, ResampleFilters[2].Filter, ResampleFilters[2].Width);
  finally
    FreeThenNil(bmpTemp);
  end;
end;

function TThumbnailManager.CreatePreviewText(const sFileName: UTF8String): TBitmap;
var
  x: LongInt;
  ARect: TRect;
  sStr: String;
  tFile: THandle;
begin
  Result:= TBitmap.Create;
  ARect:= Rect(0, 0, gThumbSize.cx, gThumbSize.cy);
  with Result do
  begin
    SetSize(gThumbSize.cx, gThumbSize.cy);
    Canvas.Brush.Color:= clWhite;
    Canvas.FillRect(ARect);
    Canvas.Font.Color:= clBlack;
    Canvas.Font.Size := gThumbSize.cy div 16;
    tFile:= mbFileOpen(sFileName, fmOpenRead or fmShareDenyNone);
    if (tFile <> feInvalidHandle) then
    begin
      for x:= 0 to 8 do
      begin
        if not FileReadLn(tFile, sStr) then Break;
        Canvas.TextOut(0, x * Canvas.Font.Size * 3 div 2, sStr);
      end;
      FileClose(tFile);
    end;
  end;
end;

function TThumbnailManager.ReadMetaData(const aFile: TFile; FileStream: TFileStreamEx): Boolean;
var
  sFileName: AnsiString;
begin
  Result:= True;
  try
    // Read metadata position from last 4 byte of file
    FileStream.Seek(-4, soEnd);
    FileStream.Seek(FileStream.ReadDWord, soBeginning);
    // Check signature
    if (FileStream.ReadQWord <> NtoBE(ThumbSign)) then
      Exit(False);
    // Read thumbnail metadata
    Result:= (URIToFilename(FileStream.ReadAnsiString, sFileName) and SameText(sFileName, aFile.FullPath));
    if not Result then Exit;
    Result:= (aFile.Size = FileStream.ReadQWord) and (QWord(aFile.ModificationTime) = FileStream.ReadQWord);
    if not Result then Exit;
    Result:= (gThumbSize.cx = FileStream.ReadWord) and (gThumbSize.cy = FileStream.ReadWord);
  except
    Result:= False;
  end;
end;

function TThumbnailManager.WriteMetaData(const aFile: TFile; FileStream: TFileStreamEx): Boolean;
var
  iEnd: Int64;
begin
  Result:= True;
  try
    // Get original file size
    iEnd:= FileStream.Seek(0, soEnd);
    // Write signature
    FileStream.WriteQWord(NtoBE(ThumbSign));
    // Write thumbnail meta data
    FileStream.WriteAnsiString(FilenameToURI(aFile.FullPath));
    FileStream.WriteQWord(aFile.Size);
    FileStream.WriteQWord(QWord(aFile.ModificationTime));
    FileStream.WriteWord(gThumbSize.cx);
    FileStream.WriteWord(gThumbSize.cy);
    // Write original file size
    FileStream.WriteDWord(iEnd);
  except
    Result:= False;
  end;
end;

class function TThumbnailManager.ReadFileName(const aThumb: UTF8String;
                                              out  aFileName: UTF8String): Boolean;
var
  fsFileStream: TFileStreamEx;
begin
  try
    fsFileStream:= TFileStreamEx.Create(aThumb, fmOpenRead or fmShareDenyNone);
    try
      // Read metadata position from last 4 byte of file
      fsFileStream.Seek(-4, soEnd);
      fsFileStream.Seek(fsFileStream.ReadDWord, soBeginning);
      // Check signature
      if (fsFileStream.ReadQWord <> NtoBE(ThumbSign)) then
        Exit(False);
      // Read source file name
      Result:= URIToFilename(fsFileStream.ReadAnsiString, aFileName);
    finally
      fsFileStream.Free;
    end;
  except
    Result:= False;
  end;
end;

constructor TThumbnailManager.Create(BackColor: TColor);
begin
  FBackColor:= BackColor;
  FThumbPath:= gpCacheDir + PathDelim + 'thumbnails';
  // If directory not exists then create it
  if not mbDirectoryExists(FThumbPath) then mbForceDirectory(FThumbPath);
end;

function TThumbnailManager.RemovePreview(const FullPathToFile: UTF8String): Boolean;
var
  sExt, sName: UTF8String;
begin
  sExt:= GetPreviewFileExt(ExtractOnlyFileExt(FullPathToFile));
  sName:= GetPreviewFileName(FullPathToFile);
  // Delete thumb from cache
  Result:= mbDeleteFile(FThumbPath + PathDelim + sName + '.' + sExt);
end;

function TThumbnailManager.CreatePreview(const aFile: TFile): TBitmap;
var
  I: Integer;
  sFullPathToFile, sThumbFileName,
  sExt: UTF8String;
  fsFileStream: TFileStreamEx = nil;
  Picture: TPicture = nil;
begin
  Result:= nil;
  try
    Picture:= TPicture.Create;
    try
      sFullPathToFile:= aFile.FullPath;
      sExt:= GetPreviewFileExt(ExtractOnlyFileExt(sFullPathToFile));
      sThumbFileName:= FThumbPath + PathDelim + GetPreviewFileName(sFullPathToFile) + '.' + sExt;
      // If thumbnail already exists in cache for this file then load it
      if mbFileExists(sThumbFileName) then
      begin
        fsFileStream:= TFileStreamEx.Create(sThumbFileName, fmOpenRead or fmShareDenyNone);
        try
          if ReadMetaData(aFile, fsFileStream) then
          begin
            fsFileStream.Position:= 0;
            Picture.LoadFromStreamWithFileExt(fsFileStream, sExt);
            Result:= TBitmap.Create;
            Result.Assign(Picture.Graphic);
            Exit;
          end;
        finally
          FreeAndNil(fsFileStream);
        end;
      end;
      // Create thumb if not exist
      sExt:= ExtractOnlyFileExt(sFullPathToFile);
      if GetGraphicClassForFileExtension(sExt) <> nil then
        begin
          fsFileStream:= TFileStreamEx.Create(sFullPathToFile, fmOpenRead or fmShareDenyNone);
          try
            Picture.LoadFromStreamWithFileExt(fsFileStream, sExt);
            Result:= CreatePreviewImage(Picture.Graphic);
          finally
            FreeAndNil(fsFileStream);
          end
        end
      // Create thumb for text files
      else if (mbFileAccess(sFullPathToFile, fmOpenRead)) and (FileIsText(sFullPathToFile)) then
        begin
          Result:= CreatePreviewText(sFullPathToFile);
          Exit; // No need to save in cache
        end
      // Try to create thumnail using providers
      else
        for I:= Low(FProviderList) to High(FProviderList) do
        begin
          Result:= FProviderList[I](sFullPathToFile, gThumbSize);
          if Assigned(Result) then Break;
        end;
      // Save created thumb to cache
      if gThumbSave and Assigned(Result) then
      begin
        Picture.Bitmap.Assign(Result);
        sExt:= GetPreviewFileExt(sExt);
        try
          fsFileStream:= TFileStreamEx.Create(sThumbFileName, fmCreate);
          try
            Picture.SaveToStreamWithFileExt(fsFileStream, sExt);
            WriteMetaData(aFile, fsFileStream);
          finally
            FreeAndNil(fsFileStream);
          end;
        except
          on e: EStreamError do
            DCDebug(['Cannot save thumbnail to file "', sThumbFileName, '": ', e.Message]);
        end;
      end;
      if not Assigned(Result) then Raise Exception.Create(EmptyStr);
    finally
      FreeAndNil(Picture);
    end;
  except
    Result:= PixMapManager.LoadBitmapEnhanced(sFullPathToFile, gIconsSize, True, FBackColor);
  end;
end;

function TThumbnailManager.CreatePreview(const FullPathToFile: UTF8String): TBitmap;
var
  aFile: TFile;
begin
  aFile := TFileSystemFileSource.CreateFileFromFile(FullPathToFile);
  try
    Result:= CreatePreview(aFile);
  finally
    FreeAndNil(AFile);
  end;
end;

class procedure TThumbnailManager.CompactCache;
var
  I: Integer;
  aFileName: UTF8String;
  aFileList: TStringList;
begin
  aFileList:= FindAllFiles(gpCacheDir + PathDelim + 'thumbnails');
  for I:= 0 to Pred(aFileList.Count) do
  begin
    if not (ReadFileName(aFileList[I], aFileName) and mbFileExists(aFileName)) then
    begin
      mbDeleteFile(aFileList[I]);
    end;
  end;
  aFileList.Free;
end;

class procedure TThumbnailManager.RegisterProvider(Provider: TCreatePreviewHandler);
begin
  SetLength(FProviderList, Length(FProviderList) + 1);
  FProviderList[High(FProviderList)]:= Provider;
end;

end.

