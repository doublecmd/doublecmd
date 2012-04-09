unit uThumbnails;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, fgl, DCClassesUtf8, uFile;

type

  { TBitmapList }

  TBitmapList = specialize TFPGObjectList<TBitmap>;

  { TThumbnailManager }

  TThumbnailManager = class
  private
    FWidth,
    FHeight: LongInt;
    FThumbPath: UTF8String;
    FBackColor: TColor;
    function CheckGraphics(const sFileExt: UTF8String): Boolean;
    function GetPreviewFileExt(const sFileExt: UTF8String): UTF8String;
    function GetPreviewFileName(const sFileName: UTF8String): UTF8String;
    function CreatePreviewImage(const Graphic: TGraphic): TBitmap;
    function CreatePreviewText(const sFileName: UTF8String): TBitmap;
    function ReadMetaData(const aFile: TFile; FileStream: TFileStreamEx): Boolean;
    function WriteMetaData(const aFile: TFile; FileStream: TFileStreamEx): Boolean;
  public
    constructor Create(aWidth, aHeight: LongInt; BackColor: TColor);
    function CreatePreview(const aFile: TFile): TBitmap;
    function CreatePreview(const FullPathToFile: UTF8String): TBitmap;
    function RemovePreview(const FullPathToFile: UTF8String): Boolean;
  end;

implementation

uses
  LCLProc, FileUtil, uDebug, DCOSUtils, uFileProcs, DCStrUtils, uReSample, uGlobsPaths,
  uGlobs, uPixmapManager, URIParser, md5, uFileSystemFileSource;

const
  ThumbSign: QWord = $0000235448554D42; // '#0 #0 # T H U M B'

function TThumbnailManager.CheckGraphics(const sFileExt: UTF8String): Boolean;
var
  sExt: UTF8String;
begin
  sExt:= LowerCase(sFileExt);
  Result:= (sExt = 'bmp') or (sExt = 'xpm') or (sExt = 'png') or
           (sExt = 'jpg') or (sExt = 'jpeg') or (sExt = 'ico') or (sExt = 'icns') or
           (sExt = 'ddw') or (sExt = 'tga') or (sExt = 'cur') or (sExt = 'gif');
end;

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
    // width and height of thumb
    if  Graphic.Width > Graphic.Height then
      begin
        x:= FWidth;
        y:= x * Graphic.Height div Graphic.Width;
        if y > FHeight then
          begin
            y:= FHeight;
            x:= y * Graphic.Width div Graphic.Height;
          end;
      end
    else
      begin
        y:= FHeight;
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
  ARect:= Rect(0, 0, FWidth, FHeight);
  with Result do
  begin
    SetSize(FWidth, FHeight);
    Canvas.Brush.Color:= clWhite;
    Canvas.FillRect(ARect);
    Canvas.Font.Color:= clBlack;
    Canvas.Font.Size := FHeight div 16;
    tFile:= mbFileOpen(sFileName, fmOpenRead or fmShareDenyNone);
    for x:= 0 to 8 do
    begin
      if not FileReadLn(tFile, sStr) then Break;
      Canvas.TextOut(0, x * Canvas.Font.Size * 3 div 2, sStr);
    end;
    FileClose(tFile);
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
    Result:= (FWidth = FileStream.ReadWord) and (FHeight = FileStream.ReadWord);
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
    FileStream.WriteWord(FWidth);
    FileStream.WriteWord(FHeight);
    // Write original file size
    FileStream.WriteDWord(iEnd);
  except
    Result:= False;
  end;
end;

constructor TThumbnailManager.Create(aWidth, aHeight: LongInt; BackColor: TColor);
begin
  FWidth:= aWidth;
  FHeight:= aHeight;
  FBackColor:= BackColor;
  FThumbPath:= gpCacheDir + PathDelim + 'thumbnails';
  // if not directory create it
  if not mbDirectoryExists(FThumbPath) then mbForceDirectory(FThumbPath);
end;

function TThumbnailManager.RemovePreview(const FullPathToFile: UTF8String): Boolean;
var
  sExt, sName: UTF8String;
begin
  sExt:= GetPreviewFileExt(ExtractOnlyFileExt(FullPathToFile));
  sName:= GetPreviewFileName(FullPathToFile);
  // delete thumb from cache
  Result:= mbDeleteFile(FThumbPath + PathDelim + sName + '.' + sExt);
end;

function TThumbnailManager.CreatePreview(const aFile: TFile): TBitmap;
var
  sFullPathToFile, sThumbFileName,
  sExt: UTF8String;
  fsFileStream: TFileStreamEx = nil;
  Picture: TPicture = nil;
begin
  Result:= nil;
  try
    Picture:= TPicture.Create;
    sFullPathToFile:= aFile.FullPath;
    sExt:= GetPreviewFileExt(ExtractOnlyFileExt(sFullPathToFile));
    sThumbFileName:= FThumbPath + PathDelim + GetPreviewFileName(sFullPathToFile) + '.' + sExt;
    // If thumbnail already exists in cache for this file then load it
    if mbFileExists(sThumbFileName) then
      try
        fsFileStream:= TFileStreamEx.Create(sThumbFileName, fmOpenRead or fmShareDenyNone);
        try
          if ReadMetaData(aFile, fsFileStream) then
          begin
            fsFileStream.Position:= 0;
            Picture.LoadFromStreamWithFileExt(fsFileStream, sExt); // load from thumb if exist
            Result:= TBitmap.Create;
            Result.Assign(Picture.Graphic);
            Exit;
          end;
        except
          // if can not load thumbnail then return default icon
          Result:= PixMapManager.LoadBitmapEnhanced(sFullPathToFile, gIconsSize, True, FBackColor);
          Exit;
        end;
      finally
        FreeThenNil(fsFileStream);
      end;
      // create thumb if not exist
      sExt:= ExtractOnlyFileExt(sFullPathToFile);
      if CheckGraphics(sExt) then
        begin
          try
            fsFileStream:= TFileStreamEx.Create(sFullPathToFile, fmOpenRead or fmShareDenyNone);
            try
              Picture.LoadFromStreamWithFileExt(fsFileStream, sExt);
              Result:= CreatePreviewImage(Picture.Graphic);
            except
              // if can not load thumbnail then return default icon
              Result:= PixMapManager.LoadBitmapEnhanced(sFullPathToFile, gIconsSize, True, FBackColor);
              Exit;
            end;
          finally
            FreeThenNil(fsFileStream);
          end;
        end
      else
      // create thumb for text files
      if (FileIsText(sFullPathToFile)) and (mbFileAccess(sFullPathToFile, fmOpenRead)) then
        begin
          Result:= CreatePreviewText(sFullPathToFile);
          Exit;
        end
      else
        begin
          // load thumb for unknown file
          Result:= PixMapManager.LoadBitmapEnhanced(sFullPathToFile, gIconsSize, True, FBackColor);
          Exit;
        end;
      if not Assigned(Result) then Exit;
      // save created thumb to cache
      if gSaveThumb then
        begin
          Picture.Bitmap.Assign(Result);
          sExt:= GetPreviewFileExt(sExt);
          try
            try
              fsFileStream:= TFileStreamEx.Create(sThumbFileName, fmCreate);
              Picture.SaveToStreamWithFileExt(fsFileStream, sExt);
              WriteMetaData(aFile, fsFileStream);
            except
              on e: EStreamError do
                DCDebug(['Cannot save thumbnail to file "', sThumbFileName, '": ', e.Message]);
            end;
          finally
            FreeThenNil(fsFileStream);
          end;
        end;
  finally
    FreeThenNil(Picture);
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

end.
