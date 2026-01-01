unit uDarwinClipboard;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils,
  CocoaAll, CocoaUtils,
  DCStrUtils, uClipboard,
  uDarwinUtil;

type

  { TDarwinClipboardUtil }

  TDarwinClipboardUtil = class
  public
    class function writeToClipboard(
      const filenames: TStringList;
      const clipboardOp: TClipboardOperation ): Boolean;
    class function readFromClipboard(
      out ClipboardOp: TClipboardOperation;
      out filenames:TStringList):Boolean;
    class procedure clear;
    class procedure setText( const s: String );
  end;

implementation

const

  TClipboardOperationName : array[TClipboardOperation] of string = (
      'copy', 'cut'
    );

  darwinPasteboardOpMime = 'application/x-darwin-doublecmd-PbOp';

// MacOs 10.5 compatibility

function FilenamesToString(const filenames:TStringList): String;
begin
  Result := TrimRightLineEnding( filenames.Text, filenames.TextLineBreakStyle);
end;

procedure NSPasteboardAddFiles(const filenames:TStringList; pb:NSPasteboard);
begin
  pb.addTypes_owner(NSArray.arrayWithObject(NSFileNamesPboardType), nil);
  pb.setPropertyList_forType(ListToNSArray(filenames), NSFileNamesPboardType);
end;

procedure NSPasteboardAddFiles(const filenames:TStringList);
begin
  NSPasteboardAddFiles( filenames, NSPasteboard.generalPasteboard );
end;

procedure NSPasteboardAddString(const value:String; const pbType:NSString );
var
  pb: NSPasteboard;
begin
  pb:= NSPasteboard.generalPasteboard;
  pb.addTypes_owner(NSArray.arrayWithObject(pbType), nil);
  pb.setString_forType(StringToNSString(value), pbType);
end;

procedure NSPasteboardAddString(const value:String);
begin
  NSPasteboardAddString( value , NSStringPboardType );
end;

function getStringFromPasteboard( pbType : NSString ) : String;
var
  pb : NSPasteboard;
begin
  pb := NSPasteboard.generalPasteboard;
  Result := NSStringToString( pb.stringForType( pbType ) );
end;

function getOpFromPasteboard() : TClipboardOperation;
var
  opString : String;
begin
  Result := ClipboardCopy;
  opString := getStringFromPasteboard( StringToNSString(darwinPasteboardOpMime) );
  if TClipboardOperationName[ClipboardCut].CompareTo(opString) = 0 then Result := ClipboardCut;
end;

function getFilenamesFromPasteboard() : TStringList;
var
  pb : NSPasteboard;
  filenameArray{, lClasses}: NSArray;
begin
  Result := nil;
  pb := NSPasteboard.generalPasteboard;
  filenameArray := pb.propertyListForType(NSFilenamesPboardType);
  if filenameArray <> nil then Result := NSArrayToList( filenameArray );
end;

procedure ClearClipboard( pb:NSPasteboard );
begin
  pb.clearContents;
end;

procedure ClearClipboard;
begin
  ClearClipboard( NSPasteboard.generalPasteboard );
end;

procedure ClipboardSetText(AText: String);
begin
  ClearClipboard;
  NSPasteboardAddString(AText);
end;

{ TDarwinClipboardUtil }

class function TDarwinClipboardUtil.writeToClipboard(
  const filenames: TStringList;
  const clipboardOp: TClipboardOperation ): Boolean;
begin
  Result:= False;
  if filenames.Count = 0 then Exit;

  ClearClipboard;
  NSPasteboardAddFiles( filenames );
  NSPasteboardAddString( FilenamesToString(filenames) );
  NSPasteboardAddString( TClipboardOperationName[ClipboardOp] , StringToNSString(darwinPasteboardOpMime) );

  Result:= True;
end;

class function TDarwinClipboardUtil.readFromClipboard(out
  ClipboardOp: TClipboardOperation; out filenames: TStringList): Boolean;
begin
  Result := False;
  clipboardOp := ClipboardCopy;
  filenames := getFilenamesFromPasteboard();
  if filenames <> nil then begin
    clipboardOp := getOpFromPasteboard();
    Result := True;
  end;
end;

class procedure TDarwinClipboardUtil.clear;
begin
  ClearClipboard( NSPasteboard.generalPasteboard );
end;

class procedure TDarwinClipboardUtil.setText( const s: String );
begin
  TDarwinClipboardUtil.clear;
  NSPasteboardAddString( s );
end;

end.

