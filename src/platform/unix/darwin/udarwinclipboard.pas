unit uDarwinClipboard;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils,
  CocoaAll,
  DCStrUtils,
  uDarwinUtil, uDarwinImage;

type

  {$scopedEnums on}
  TDarwinClipboardOperation = ( copy, cut );

  { TDarwinClipboardUtil }

  TDarwinClipboardUtil = class
  private
    class procedure addText( const value: String; const pbType: NSString );
    class procedure addText( const value: String );
    class procedure addFiles( const filenames: TStringList; const pb: NSPasteboard );
    class procedure addFiles( const filenames:TStringList );
    class function getImageData: NSData;
  private
    class function filenamesToString( const filenames:TStringList ): String;
  public
    class procedure setText( const value: String );
    class function getText( const pbType: String ): String;
    class function setFiles(
      const clipboardOp: TDarwinClipboardOperation;
      const filenames: TStringList ): Boolean;
    class function getFiles(
      out clipboardOp: TDarwinClipboardOperation;
      out filenames:TStringList ): Boolean;
    class function hasImage: Boolean;
    class procedure pasteImageToFile( const path: String );
    class procedure clear;
  end;

implementation

const

  OPERATION_NAME: array[TDarwinClipboardOperation] of String =
    ( 'copy', 'cut' );

  PASTEBOARD_OP_MIME = 'application/x-darwin-doublecmd-PbOp';

var

  PASTEBOARD_IMAGE_TYPES: NSMutableArray;

// macOS 10.5 compatibility

{ TDarwinClipboardUtil }

class procedure TDarwinClipboardUtil.addText( const value: String; const pbType: NSString );
var
  pb: NSPasteboard;
begin
  pb:= NSPasteboard.generalPasteboard;
  pb.addTypes_owner(NSArray.arrayWithObject(pbType), nil);
  pb.setString_forType(StringToNSString(value), pbType);
end;

class procedure TDarwinClipboardUtil.addText( const value:String );
begin
  TDarwinClipboardUtil.addText( value , NSStringPboardType );
end;

class procedure TDarwinClipboardUtil.setText( const value: String );
begin
  TDarwinClipboardUtil.clear;
  TDarwinClipboardUtil.addText( value );
end;

class function TDarwinClipboardUtil.getText( const pbType: String ): String;
var
  pb: NSPasteboard;
begin
  pb:= NSPasteboard.generalPasteboard;
  Result:= pb.stringForType(NSSTR(pbType)).UTF8String;
end;

class procedure TDarwinClipboardUtil.addFiles( const filenames: TStringList; const pb: NSPasteboard );
begin
  pb.addTypes_owner(NSArray.arrayWithObject(NSFileNamesPboardType), nil);
  pb.setPropertyList_forType(ListToNSArray(filenames), NSFileNamesPboardType);
end;

class procedure TDarwinClipboardUtil.addFiles( const filenames:TStringList );
begin
  TDarwinClipboardUtil.addFiles( filenames, NSPasteboard.generalPasteboard );
end;

class function TDarwinClipboardUtil.filenamesToString( const filenames:TStringList ): String;
begin
  Result:= TrimRightLineEnding( filenames.Text, filenames.TextLineBreakStyle );
end;

class function TDarwinClipboardUtil.setFiles(
  const clipboardOp: TDarwinClipboardOperation;
  const filenames: TStringList ): Boolean;
begin
  Result:= False;
  if filenames.Count = 0 then Exit;

  TDarwinClipboardUtil.clear;
  TDarwinClipboardUtil.addFiles( filenames );
  TDarwinClipboardUtil.addText( filenamesToString(filenames) );
  TDarwinClipboardUtil.addText( OPERATION_NAME[ClipboardOp] , NSSTR(PASTEBOARD_OP_MIME) );

  Result:= True;
end;

class function TDarwinClipboardUtil.getFiles(
  out clipboardOp: TDarwinClipboardOperation;
  out filenames: TStringList ): Boolean;

  function getFilenamesFromPasteboard(): TStringList;
  var
    pb: NSPasteboard;
    filenameArray: NSArray;
  begin
    Result:= nil;
    pb:= NSPasteboard.generalPasteboard;
    filenameArray:= pb.propertyListForType(NSFilenamesPboardType);
    if filenameArray <> nil then
      Result:= NSArrayToList( filenameArray );
  end;

  function getOpFromPasteboard(): TDarwinClipboardOperation;
  var
    opString: String;
  begin
    Result:= TDarwinClipboardOperation.copy;
    opString:= TDarwinClipboardUtil.getText( PASTEBOARD_OP_MIME );
    if OPERATION_NAME[TDarwinClipboardOperation.cut].CompareTo(opString) = 0 then
      Result:= TDarwinClipboardOperation.cut;
  end;

begin
  Result:= False;
  clipboardOp:= TDarwinClipboardOperation.copy;
  filenames:= getFilenamesFromPasteboard();
  if filenames <> nil then begin
    clipboardOp:= getOpFromPasteboard();
    Result:= True;
  end;
end;

class function TDarwinClipboardUtil.hasImage: Boolean;
var
  pb: NSPasteboard;
  imageType: NSString;
begin
  pb:= NSPasteboard.generalPasteboard;
  imageType:= pb.availableTypeFromArray( PASTEBOARD_IMAGE_TYPES );
  Result:= Assigned( imageType );
end;

class procedure TDarwinClipboardUtil.pasteImageToFile(const path: String);
var
  data: NSData;
begin
  data:= TDarwinClipboardUtil.getImageData;
  if data = nil then
    Exit;
  NSFileManager.defaultManager.createFileAtPath_contents_attributes(
    StringToNSString(path),
    data,
    nil );
end;

class function TDarwinClipboardUtil.getImageData: NSData;
var
  pb: NSPasteboard;
  imageType: NSString;
begin
  Result:= nil;
  pb:= NSPasteboard.generalPasteboard;
  imageType:= pb.availableTypeFromArray( PASTEBOARD_IMAGE_TYPES );
  if imageType.isEqualToString(NSPasteboardTypePNG) then begin
    Result:= pb.dataForType( NSPasteboardTypePNG );
  end else if imageType.isEqualToString(NSPasteboardTypeTIFF) then begin
    Result:= pb.dataForType( NSPasteboardTypeTIFF );
    Result:= TDarwinImageUtil.toPNGData( Result );
  end;
end;

class procedure TDarwinClipboardUtil.clear;
begin
  NSPasteboard.generalPasteboard.clearContents;
end;

initialization
  PASTEBOARD_IMAGE_TYPES:= NSMutableArray.new;
  PASTEBOARD_IMAGE_TYPES.addObject( NSPasteboardTypePNG );
  PASTEBOARD_IMAGE_TYPES.addObject( NSPasteboardTypeTIFF );

finalization
  PASTEBOARD_IMAGE_TYPES.release;

end.

