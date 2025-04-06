{
   Notes:
   1. the most basic Http Client library
   2. based on NSURLConnection and does not rely on third-party libraries
   3. and avoid falling into version hell on macOS of libcrypto, openssl, etc.
   4. NSURLSession is a better choice, but it is prone to problems when compiled
      for Aarch64. the same code works when compiled for x86_64.
      it seems to be a bug in FPC.
}

unit uMiniHttpClient;

{$mode ObjFPC}{$H+}
{$interfaces corba}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils,
  MacOSAll, CocoaAll, uMiniUtil, uMiniCocoa;

type

  HttpMethodConst = class
  class var
    POST: NSString;
    GET: NSString;
  end;

  HttpHeaderConst = class
  class var
    ContentType: NSString;
    ContentLength: NSString;
  end;

  HttpContentTypeConst = class
  class var
    UrlEncoded: NSString;
    JSON: NSString;
    OctetStream: NSString;
  end;

  HttpConst = class
  class var
    Method: HttpMethodConst;
    Header: HttpHeaderConst;
    ContentType: HttpContentTypeConst;
  end;

  { TMiniHttpResult }

  TMiniHttpResult = class
  public
    response: NSHTTPURLResponse;
    error: NSError;
    resultCode: Integer;
    body: String;
  public
    destructor Destroy; override;
  public
    function getHeader( const name: String ): String;
  end;

  IMiniHttpDataCallback = interface
    function progress( const accumulatedBytes: Integer ): Boolean;
  end;

  { TMiniHttpDataProcessor }

  TMiniHttpDataProcessor = class
    function receive( const newData: NSData; const buffer: NSMutableData; const connection: NSURLConnection ): Boolean; virtual; abstract;
    function send( const accumulatedBytes: Integer; const connection: NSURLConnection ): Boolean; virtual; abstract;
    function getHttpBodyStream: NSInputStream; virtual; abstract;
    procedure complete( const response: NSURLResponse; const error: NSError ); virtual; abstract;
  end;

  {$scopedEnums on}
  TMiniHttpConnectionState = ( running, aborted, successful, failed );

  { TMiniHttpConnectionDataDelegate }

  TMiniHttpConnectionDataDelegate = objcclass( NSObject, NSURLConnectionDataDelegateProtocol )
  private
    _data: NSMutableData;
    _processor: TMiniHttpDataProcessor;
    _result: TMiniHttpResult;
    _runloop: CFRunLoopRef;
    _state: TMiniHttpConnectionState;
  private
    procedure cancelConnection( const connection: NSURLConnection; const name: NSString ); message 'HttpClient_cancelConnection:connection:';
  public
    function init: id; override;
    procedure dealloc; override;
    procedure connection_didReceiveResponse (connection: NSURLConnection; response: NSURLResponse);
    procedure connection_didReceiveData (connection: NSURLConnection; data: NSData);
    procedure connection_didSendBodyData_totalBytesWritten_totalBytesExpectedToWrite (connection: NSURLConnection; bytesWritten: NSInteger; totalBytesWritten: NSInteger; totalBytesExpectedToWrite: NSInteger);
    procedure connectionDidFinishLoading (connection: NSURLConnection);
    procedure connection_didFailWithError (connection: NSURLConnection; error: NSError);
  public
    procedure setProcessor( const processor: TMiniHttpDataProcessor ); message 'HttpClient_setProcessor:';
    function state: TMiniHttpConnectionState; message 'HttpClient_state';
    function isRunning: Boolean; message 'HttpClient_isRunning';
    function getResult: TMiniHttpResult; message 'HttpClient_getResult';
  end;

  { TMiniHttpClient }

  TMiniHttpClient = class
  private
    _request: NSMutableURLRequest;
  public
    constructor Create;
    destructor Destroy; override;
  public
    procedure addHeader( const name: NSString; const value: NSString ); overload;
    procedure addHeader( const name: String; const value: String ); overload;
    procedure setBody( const body: String );
    procedure setContentType( const contentType: NSString );
    procedure setContentLength( const length: Integer );
  protected
    procedure doRunloop( const delegate: TMiniHttpConnectionDataDelegate );
    function doPost( const urlPart: String; lclItems: TQueryItemsDictonary;
      const processor: TMiniHttpDataProcessor ): TMiniHttpResult;
    function doUpload( const urlPart: String; const localPath: String;
      const range: NSRangePtr; const callback: IMiniHttpDataCallback ): TMiniHttpResult;
  public
    function post( const urlPart: String; lclItems: TQueryItemsDictonary ): TMiniHttpResult;
    function download( const urlPart: String; const localPath: String;
      const callback: IMiniHttpDataCallback ): TMiniHttpResult;
    function upload( const urlPart: String; const localPath: String;
      const callback: IMiniHttpDataCallback ): TMiniHttpResult;
    function uploadRange( const urlPart: String; const localPath: String;
      const range: NSRange; const callback: IMiniHttpDataCallback ): TMiniHttpResult;
  end;

implementation

type
  { TDefaultHttpDataProcessor }

  TDefaultHttpDataProcessor = class( TMiniHttpDataProcessor )
    function receive( const newData: NSData; const buffer: NSMutableData; const connection: NSURLConnection ): Boolean; override;
    function send( const accumulatedBytes: Integer; const connection: NSURLConnection ): Boolean; override;
    function getHttpBodyStream: NSInputStream; override;
    procedure complete( const response: NSURLResponse; const error: NSError ); override;
  end;

function TDefaultHttpDataProcessor.receive(const newData: NSData;
  const buffer: NSMutableData; const connection: NSURLConnection): Boolean;
begin
  buffer.appendData( newData );
  Result:= True;
end;

function TDefaultHttpDataProcessor.send(const accumulatedBytes: Integer;
  const connection: NSURLConnection): Boolean;
begin
  Result:= True;
end;

function TDefaultHttpDataProcessor.getHttpBodyStream: NSInputStream;
begin
  Result:= nil;
end;

procedure TDefaultHttpDataProcessor.complete(const response: NSURLResponse;
  const error: NSError);
begin
end;

type
  { TMiniHttpDownloadProcessor }

  TMiniHttpDownloadProcessor = class( TDefaultHttpDataProcessor )
  private
    _localPath: NSString;
    _stream: NSOutputStream;
  private
    _callback: IMiniHttpDataCallback;
    _accumulatedBytes: Integer;
  public
    constructor Create( const localPath: String; const callback: IMiniHttpDataCallback );
    destructor Destroy; override;
  public
    function receive( const newData: NSData; const buffer: NSMutableData; const connection: NSURLConnection ): Boolean; override;
    function send( const accumulatedBytes: Integer; const connection: NSURLConnection ): Boolean; override;
    procedure complete( const response: NSURLResponse; const error: NSError ); override;
  end;

constructor TMiniHttpDownloadProcessor.Create(
  const localPath: String;
  const callback: IMiniHttpDataCallback );
begin
  _localPath:= StringToNSString( localPath );
  _localPath.retain;
  _stream:= NSOutputStream.alloc.initToFileAtPath_append( _localPath, False );
  _stream.open;
  _callback:= callback;
end;

destructor TMiniHttpDownloadProcessor.Destroy;
begin
  inherited Destroy;
  _localPath.release;
  _stream.release;
end;

function TMiniHttpDownloadProcessor.receive( const newData: NSData; const buffer: NSMutableData; const connection: NSURLConnection ): Boolean;
var
  offset: NSUInteger;
  maxLength: NSUInteger;
  writenLength: NSInteger;
begin
  offset:= 0;
  maxLength:= newData.length;
  inc( _accumulatedBytes, maxLength );

  repeat
    writenLength:= _stream.write_maxLength(
      newData.bytes + offset,
      maxLength - offset );
    offset:= offset + writenLength;
  until offset >= maxLength;

  Result:= _callback.progress( _accumulatedBytes );
end;

function TMiniHttpDownloadProcessor.send(const accumulatedBytes: Integer;
  const connection: NSURLConnection): Boolean;
begin
  Result:= True;
end;

procedure TMiniHttpDownloadProcessor.complete( const response: NSURLResponse; const error: NSError );
begin
  _stream.close;
end;

type

  { TMiniHttpUploadProcessor }

  TMiniHttpUploadProcessor = class( TDefaultHttpDataProcessor )
  private
    _localPath: NSString;
    _stream: NSInputStream;
  private
    _callback: IMiniHttpDataCallback;
  public
    constructor Create( const localPath: String; const range: NSRangePtr; const callback: IMiniHttpDataCallback );
    destructor Destroy; override;
  public
    function send( const accumulatedBytes: Integer; const connection: NSURLConnection ): Boolean; override;
    function getHttpBodyStream: NSInputStream; override;
    procedure complete( const response: NSURLResponse; const error: NSError ); override;
  end;

{ TMiniHttpUploadProcessor }

constructor TMiniHttpUploadProcessor.Create(
  const localPath: String;
  const range: NSRangePtr;
  const callback: IMiniHttpDataCallback );
begin
  _localPath:= StringToNSString( localPath );
  _localPath.retain;
  if Assigned(range) then
    _stream:= NSFileRangeInputStream.alloc.initWithFileAtPath_Range( _localPath, range^ )
  else
    _stream:= NSInputStream.alloc.initWithFileAtPath( _localPath );
  _callback:= callback;
end;

destructor TMiniHttpUploadProcessor.Destroy;
begin
  inherited Destroy;
  _localPath.release;
  _stream.release;
end;

function TMiniHttpUploadProcessor.getHttpBodyStream: NSInputStream;
begin
  Result:= _stream;
end;

function TMiniHttpUploadProcessor.send(const accumulatedBytes: Integer;
  const connection: NSURLConnection): Boolean;
begin
  Result:= _callback.progress( accumulatedBytes );
end;

procedure TMiniHttpUploadProcessor.complete(const response: NSURLResponse;
  const error: NSError);
begin
  _stream.close;
end;

{ TMiniHttpResult }

destructor TMiniHttpResult.Destroy;
begin
  if Assigned(self.response) then
    self.response.release;
  if Assigned(self.error) then
    self.error.release;
end;

function TMiniHttpResult.getHeader(const name: String): String;
begin
  if Assigned(self.response) then
    Result:= NSString( self.response.allHeaderFields.objectForKey( NSSTR(name) ) ).UTF8String
  else
    Result:= EmptyStr;
end;

{ TMiniHttpConnectionDataDelegate }

procedure TMiniHttpConnectionDataDelegate.cancelConnection(
  const connection: NSURLConnection; const name: NSString );
begin
  _state:= TMiniHttpConnectionState.aborted;
  _result.resultCode:= -1;
  TLogUtil.logError( 'HttpClient: Connection Canceled in ' + name.UTF8String );
  connection.cancel;
  CFRunLoopStop( _runloop );
end;

function TMiniHttpConnectionDataDelegate.init: id;
begin
  Result:= Inherited init;
  _data:= NSMutableData.new;
  _result:= TMiniHttpResult.Create;
  _runloop:= CFRunLoopGetCurrent();
  _state:= TMiniHttpConnectionState.running;
end;

procedure TMiniHttpConnectionDataDelegate.dealloc;
begin
  if Assigned(_processor) then
    _processor.Free;
  _data.release;
  _result.Free;
end;

procedure TMiniHttpConnectionDataDelegate.connection_didReceiveResponse(
  connection: NSURLConnection; response: NSURLResponse);
begin
  _result.response:= NSHTTPURLResponse( response );
  response.retain;
end;

procedure TMiniHttpConnectionDataDelegate.setProcessor( const processor: TMiniHttpDataProcessor );
begin
  _processor:= processor;
end;

procedure TMiniHttpConnectionDataDelegate.connection_didReceiveData
  (connection: NSURLConnection; data: NSData);
var
  ret: Boolean;
begin
  if NOT isRunning then
    Exit;

  ret:= _processor.receive( data, _data, connection );
  if NOT ret then begin
    self.cancelConnection( connection , NSSTR('connection_didReceiveData()') );
  end;
end;

procedure TMiniHttpConnectionDataDelegate.connection_didSendBodyData_totalBytesWritten_totalBytesExpectedToWrite
  (connection: NSURLConnection; bytesWritten: NSInteger;
  totalBytesWritten: NSInteger; totalBytesExpectedToWrite: NSInteger);
var
  ret: Boolean;
begin
  if NOT isRunning then
    Exit;

  ret:= _processor.send( totalBytesWritten, connection );
  if NOT ret then begin
    self.cancelConnection( connection , NSSTR('connection_didSendBodyData_totalBytesWritten_totalBytesExpectedToWrite()') );
  end;
end;

procedure TMiniHttpConnectionDataDelegate.connectionDidFinishLoading
  (connection: NSURLConnection);
var
  dataString: NSString;
begin
  _state:= TMiniHttpConnectionState.successful;
  _processor.complete( _result.response, nil );
  _result.resultCode:= _result.response.statusCode;
  if (_data.length>0) then begin
    dataString:= NSString.alloc.initWithData_encoding( _data, NSUTF8StringEncoding );
    _result.body:= dataString.UTF8String;
    dataString.release;
  end;
  TLogUtil.logInformation( 'HttpClient connectionDidFinishLoading' );
  TLogUtil.logInformation( 'resultCode=' + IntToStr(_result.response.statusCode) );
  TLogUtil.logInformation( 'body=' + _result.body );
  CFRunLoopStop( _runloop );
end;

procedure TMiniHttpConnectionDataDelegate.connection_didFailWithError(
  connection: NSURLConnection; error: NSError);
begin
  _state:= TMiniHttpConnectionState.failed;
  _processor.complete( _result.response, error );

  TLogUtil.logError( 'HttpClient connection_didFailWithError !!!' );
  if Assigned(error) then begin
    TLogUtil.logError( 'error.code=' + IntToStr(error.code) );
    TLogUtil.logError( 'error.domain=' + error.domain.UTF8String );
    TLogUtil.logError( 'error.description=' + error.description.UTF8String );
  end;

  _result.resultCode:= -1;
  _result.error:= error;
  error.retain;
end;

function TMiniHttpConnectionDataDelegate.state: TMiniHttpConnectionState;
begin
  Result:= _state;
end;

function TMiniHttpConnectionDataDelegate.isRunning: Boolean;
begin
  Result:= (_state = TMiniHttpConnectionState.running);
end;

function TMiniHttpConnectionDataDelegate.getResult: TMiniHttpResult;
begin
  Result:= _result;
end;

{ TMiniHttpClient }

constructor TMiniHttpClient.Create;
begin
  _request:= NSMutableURLRequest.new;
end;

destructor TMiniHttpClient.Destroy;
begin
  _request.release;
end;

procedure TMiniHttpClient.addHeader(const name: NSString; const value: NSString);
begin
  _request.addValue_forHTTPHeaderField( value, name );
end;

procedure TMiniHttpClient.addHeader(const name: String; const value: String);
begin
  self.addHeader( NSSTR(name), StringToNSString(value) );
end;

procedure TMiniHttpClient.setBody(const body: String);
var
  bodyData: NSData;
begin
  bodyData:= StringToNSString(body).dataUsingEncoding( NSUTF8StringEncoding );
  _request.setHTTPBody( bodyData );
  _request.setHTTPMethod( HttpConst.Method.POST );
  self.setContentLength( bodyData.length );
end;

procedure TMiniHttpClient.setContentType(const contentType: NSString);
begin
  self.addHeader( HttpConst.Header.ContentType, contentType );
end;

procedure TMiniHttpClient.setContentLength(const length: Integer);
begin
  self.addHeader( HttpConst.Header.ContentLength, StringToNSString(IntToStr(length)) );
end;

procedure TMiniHttpClient.doRunloop(
  const delegate: TMiniHttpConnectionDataDelegate);
var
  connection: NSURLConnection;
begin
  try
    TLogUtil.logInformation( 'HttpClient start:' + _request.description.UTF8String );
    connection:= NSURLConnection.alloc.initWithRequest_delegate(
      _request, delegate );
    connection.start;
    repeat
      CFRunLoopRun;
    until NOT delegate.isRunning;
  finally
    connection.release;
  end;

  if delegate.state = TMiniHttpConnectionState.aborted then
    raise EAbort.Create( 'Raised by HttpClient' );
end;

function TMiniHttpClient.doPost(
  const urlPart: String;
  lclItems: TQueryItemsDictonary;
  const processor: TMiniHttpDataProcessor): TMiniHttpResult;
var
  url: NSURL;
  delegate: TMiniHttpConnectionDataDelegate;

  function getBody: String;
  var
    components: NSURLComponents;
    cocoaItems: NSArray;
  begin
    components:= NSURLComponents.new;
    cocoaItems:= THttpClientUtil.toQueryItems( lclItems );
    components.setQueryItems( cocoaItems );
    Result:= components.query.UTF8String;
    components.release;
  end;

begin
  try
    url:= NSURL.URLWithString( StringToNSString(urlPart) );
    _request.setURL( url );
    _request.setHTTPMethod( HttpConst.Method.POST );
    if lclItems <> nil then begin
      self.setBody( getBody );
      self.setContentType( HttpConst.ContentType.UrlEncoded );
    end;

    delegate:= TMiniHttpConnectionDataDelegate.new;
    delegate.setProcessor( processor );
    doRunloop( delegate );
  finally
    Result:= delegate.getResult;
    FreeAndNil( lclItems );
    delegate.autorelease;
  end;
end;

function TMiniHttpClient.post( const urlPart: String; lclItems: TQueryItemsDictonary ): TMiniHttpResult;
var
  processor: TMiniHttpDataProcessor;
begin
  processor:= TDefaultHttpDataProcessor.Create;
  Result:= self.doPost( urlPart, lclItems, processor );
end;

function TMiniHttpClient.download(
  const urlPart: String;
  const localPath: String;
  const callback: IMiniHttpDataCallback ): TMiniHttpResult;
var
  processor: TMiniHttpDataProcessor;
begin
  TLogUtil.logInformation( '>> HttpClient: Download file start' );
  processor:= TMiniHttpDownloadProcessor.Create( localPath, callback );
  Result:= self.doPost( urlPart, nil, processor );
  TLogUtil.logInformation( '<< HttpClient: Download file end' );
end;

function TMiniHttpClient.doUpload(
  const urlPart: String;
  const localPath: String;
  const range: NSRangePtr;
  const callback: IMiniHttpDataCallback): TMiniHttpResult;
var
  processor: TMiniHttpUploadProcessor;
begin
  TLogUtil.logInformation( '>> HttpClient: Upload file start' );
  processor:= TMiniHttpUploadProcessor.Create( localPath, range, callback );
  _request.setHTTPBodyStream( processor.getHttpBodyStream );
  self.setContentType( HttpConst.ContentType.OctetStream );
  Result:= self.doPost( urlPart, nil, processor );
  TLogUtil.logInformation( '<< HttpClient: Upload file end' );
end;

function TMiniHttpClient.upload(
  const urlPart: String;
  const localPath: String;
  const callback: IMiniHttpDataCallback): TMiniHttpResult;
begin
  Result:= self.doUpload( urlPart, localPath, nil, callback );
end;

function TMiniHttpClient.uploadRange(
  const urlPart: String;
  const localPath: String;
  const range: NSRange;
  const callback: IMiniHttpDataCallback): TMiniHttpResult;
begin
  Result:= self.doUpload( urlPart, localPath, @range, callback );
end;

initialization
  HttpConst.Method.GET:= NSSTR( 'GET' );
  HttpConst.Method.POST:= NSSTR( 'POST' );

  HttpConst.Header.ContentType:= NSSTR('content-type');
  HttpConst.Header.ContentLength:= NSSTR('content-length');

  HttpConst.ContentType.UrlEncoded:= NSSTR( 'application/x-www-form-urlencoded' );
  HttpConst.ContentType.JSON:= NSSTR( 'application/json' );
  HttpConst.ContentType.OctetStream:= NSSTR( 'application/octet-stream' );

end.

