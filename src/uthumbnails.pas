unit uThumbnails;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, fgl;

type

  { TBitmapList }

  TBitmapList = specialize TFPGList<TBitmap>;

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
  public
    constructor Create(aWidth, aHeight: LongInt; BackColor: TColor);
    function CreatePreview(const FullPathToFile: UTF8String): TBitmap;
    function RemovePreview(const FullPathToFile: UTF8String): Boolean;
  end;

implementation

uses
  LCLProc, FileUtil, uClassesEx, uOSUtils, uFileProcs, uDCUtils, uReSample, uGlobsPaths,
  uGlobs, uPixmapManager;

function TThumbnailManager.CheckGraphics(const sFileExt: UTF8String): Boolean;
var
  sExt: UTF8String;
begin
  sExt:= LowerCase(sFileExt);
  Result:= (sExt = 'bmp') or (sExt = 'xpm') or (sExt = 'png') or
           (sExt = 'jpg') or (sExt = 'jpeg') or (sExt = 'ico') or (sExt = 'icns') or
           (sExt = 'ddw') or (sExt = 'tga') or (sExt = 'gif');
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
  Result:= IntToStr(mbFileAge(sFileName)) + '_' + IntToStr(mbFileSize(sFileName)) + '_' + ExtractOnlyFileName(sFileName);
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

constructor TThumbnailManager.Create(aWidth, aHeight: LongInt; BackColor: TColor);
begin
  FWidth:= aWidth;
  FHeight:= aHeight;
  FBackColor:= BackColor;
  FThumbPath:= gpCacheDir + PathDelim + 'thumbnails';
  // if not directory create it
  if mbDirectoryExists(FThumbPath) then mbForceDirectory(FThumbPath);
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

function TThumbnailManager.CreatePreview(const FullPathToFile: UTF8String): TBitmap;
var
  sThumbFileName,
  sExt, sOnlyFileName: UTF8String;
  fsFileStream: TFileStreamEx = nil;
  Picture: TPicture = nil;
begin
  Result:= nil;
  try
    Picture:= TPicture.Create;
    sExt:= GetPreviewFileExt(ExtractOnlyFileExt(FullPathToFile));
    sThumbFileName:= FThumbPath + PathDelim + GetPreviewFileName(FullPathToFile) + '.' + sExt;
    // If thumbnail already exists in cache for this file then load it
    if mbFileExists(sThumbFileName) then
      try
        fsFileStream:= TFileStreamEx.Create(sThumbFileName, fmOpenRead or fmShareDenyNone);
        try
          Picture.LoadFromStreamWithFileExt(fsFileStream, sExt); // load from thumb if exist
          Result:= TBitmap.Create;
          Result.Assign(Picture.Graphic);
        except
          // if can not load thumbnail then return default icon
          Result:= PixMapManager.LoadBitmapEnhanced(FullPathToFile, gIconsSize, FBackColor);
          Exit;
        end;
      finally
        FreeThenNil(fsFileStream);
      end
    else
      // create thumb if not exist
      begin
        sExt:= ExtractOnlyFileExt(FullPathToFile);
        if CheckGraphics(sExt) then
          begin
            try
              fsFileStream:= TFileStreamEx.Create(FullPathToFile, fmOpenRead or fmShareDenyNone);
              try
                Picture.LoadFromStreamWithFileExt(fsFileStream, sExt);
                Result:= CreatePreviewImage(Picture.Graphic);
              except
                // if can not load thumbnail then return default icon
                Result:= PixMapManager.LoadBitmapEnhanced(FullPathToFile, gIconsSize, FBackColor);
                Exit;
              end;
            finally
              FreeThenNil(fsFileStream);
            end;
          end
        else
        // create thumb for text files
        if (FileIsText(FullPathToFile)) and (mbFileAccess(FullPathToFile, fmOpenRead)) then
          begin
            Result:= CreatePreviewText(FullPathToFile);
            Exit;
          end
        else
          begin
            // load thumb for unknown file
            Result:= PixMapManager.LoadBitmapEnhanced(FullPathToFile, gIconsSize, FBackColor);
            Exit;
          end;
        if not Assigned(Result) then Exit;
        // save created thumb to cache
        Picture.Bitmap.Assign(Result);
        sExt:= GetPreviewFileExt(sExt);
        try
          fsFileStream:= TFileStreamEx.Create(sThumbFileName, fmCreate);
          Picture.SaveToStreamWithFileExt(fsFileStream, sExt);
        finally
          FreeThenNil(fsFileStream);
        end;
      end;
  finally
    FreeThenNil(Picture);
  end;
end;

end.

