unit uDarwinClipboard;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils,
  CocoaAll,
  DCStrUtils,
  uDarwinUtil;

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
    class procedure clear;
  end;

implementation

const

  OPERATION_NAME: array[TDarwinClipboardOperation] of String =
    ( 'copy', 'cut' );

  PASTEBOARD_OP_MIME = 'application/x-darwin-doublecmd-PbOp';

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

class procedure TDarwinClipboardUtil.clear;
begin
  NSPasteboard.generalPasteboard.clearContents;
end;

end.

