{
  SChannel to OpenSSL wrapper

  Copyright (c) 2008 Boris Krasnovskiy
  Copyright (c) 2013-2015 Alexander Koblov (pascal port)

  This program is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation; either version 2
  of the License.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
}

unit ssl_winssl_lib;

{$mode delphi}

interface

uses
  Windows, SynSock, JwaSspi, CTypes;

type
  PSSL_CTX = ^SSL_CTX;
  SSL_CTX = record
    dwProtocol: DWORD;
    bVerify: BOOL;
  end;

  PSSL_METHOD = ^SSL_METHOD;
  SSL_METHOD = record
    dummy: DWORD;
  end;

  PSSL = ^SSL;
  SSL = record
    s: TSocket;
    ctx: PSSL_CTX;
    hContext: CtxtHandle;
    hCreds: CredHandle;
    pbRecDataBuf: PByte;
    cbRecDataBuf: LONG;
    sbRecDataBuf: LONG;
    pbIoBuffer: PByte;
    cbIoBuffer: LONG;
    sbIoBuffer: LONG;
    exIoBuffer: BOOL;
    rmshtdn: BOOL;
  end;

function SSL_library_init(): cint; cdecl;
function SSL_set_fd(ssl: PSSL; fd: cint): cint; cdecl;
function SSL_CTX_new(method: PSSL_METHOD): PSSL_CTX; cdecl;
procedure SSL_CTX_free(ctx: PSSL_CTX); cdecl;
function SSL_new(ctx: PSSL_CTX): PSSL; cdecl;
procedure SSL_free(ssl: PSSL); cdecl;
function SSL_connect(ssl: PSSL): cint; cdecl;
function SSL_shutdown(ssl: PSSL): cint; cdecl;
function SSL_read(ssl: PSSL; buf: PByte; num: cint): cint; cdecl;
function SSL_write(ssl: PSSL; const buf: PByte; num: cint): cint; cdecl;
function SSL_pending(ssl: PSSL): cint; cdecl;
function SSLv23_method(): PSSL_METHOD; cdecl;
function SSLv2_method(): PSSL_METHOD; cdecl;
function SSLv3_method(): PSSL_METHOD; cdecl;
function TLSv1_method(): PSSL_METHOD; cdecl;
function TLSv1_1_method(): PSSL_METHOD; cdecl;
function TLSv1_2_method(): PSSL_METHOD; cdecl;
procedure SSL_CTX_set_verify(ctx: PSSL_CTX; mode: cint; func: Pointer); cdecl;
function SSL_get_error (ssl: PSSL; ret: cint): cint; cdecl;

implementation

uses
  JwaWinError,
  ssl_openssl_lib, blcksock, ssl_openssl;

const
  SCHANNEL_CRED_VERSION = $00000004;

const
  SCH_CRED_MANUAL_CRED_VALIDATION = $00000008;
  SCH_CRED_NO_DEFAULT_CREDS       = $00000010;

const
  SCHANNEL_SHUTDOWN               = 1;   // gracefully close down a connection

const
  SP_PROT_SSL2_SERVER       =      $00000004;
  SP_PROT_SSL2_CLIENT       =      $00000008;
  SP_PROT_SSL2              =      (SP_PROT_SSL2_SERVER or SP_PROT_SSL2_CLIENT);

  SP_PROT_SSL3_SERVER       =      $00000010;
  SP_PROT_SSL3_CLIENT       =      $00000020;
  SP_PROT_SSL3              =      (SP_PROT_SSL3_SERVER or SP_PROT_SSL3_CLIENT);

  SP_PROT_TLS1_SERVER       =      $00000040;
  SP_PROT_TLS1_CLIENT       =      $00000080;
  SP_PROT_TLS1              =      (SP_PROT_TLS1_SERVER or SP_PROT_TLS1_CLIENT);

  SP_PROT_TLS1_1_SERVER     =      $00000100;
  SP_PROT_TLS1_1_CLIENT     =      $00000200;
  SP_PROT_TLS1_1            =      (SP_PROT_TLS1_1_SERVER or SP_PROT_TLS1_1_CLIENT);

  SP_PROT_TLS1_2_SERVER     =      $00000400;
  SP_PROT_TLS1_2_CLIENT     =      $00000800;
  SP_PROT_TLS1_2            =      (SP_PROT_TLS1_2_SERVER or SP_PROT_TLS1_2_CLIENT);

const
  UNISP_NAME_A = AnsiString('Microsoft Unified Security Protocol Provider');
  UNISP_NAME_W = WideString('Microsoft Unified Security Protocol Provider');



type
  ALG_ID = type cuint;
  HCERTSTORE = type HANDLE;
  PCCERT_CONTEXT = type Pointer;

type
  SCHANNEL_CRED = record
    dwVersion: DWORD;
    cCreds: DWORD;
    paCred: PCCERT_CONTEXT;
    hRootStore: HCERTSTORE;
    cMappers: DWORD;
    aphMappers: Pointer;
    cSupportedAlgs: DWORD;
    palgSupportedAlgs: ^ALG_ID;
    grbitEnabledProtocols: DWORD;
    dwMinimumCipherStrength: DWORD;
    dwMaximumCipherStrength: DWORD;
    dwSessionLifespan: DWORD;
    dwFlags: DWORD;
    dwCredFormat: DWORD;
  end;

var
  g_hSecurity: HMODULE;
  g_pSSPI: PSecurityFunctionTableA;

function SSL_library_init(): cint; cdecl;
var
  pInitSecurityInterface: INIT_SECURITY_INTERFACE_A;
begin
  if (g_hSecurity <> 0) then Exit(1);

  g_hSecurity:= LoadLibraryA('schannel.dll');
  if (g_hSecurity = 0) then Exit(0);

  pInitSecurityInterface := INIT_SECURITY_INTERFACE_A(GetProcAddress(g_hSecurity, SECURITY_ENTRYPOINT_ANSIA));
  if (pInitSecurityInterface <> nil) then
    g_pSSPI := pInitSecurityInterface();

  if (g_pSSPI = nil) then
  begin
    FreeLibrary(g_hSecurity);
    g_hSecurity := 0;
    Exit(0);
  end;
  
  Result := 1;
end;

function SSL_set_fd(ssl: PSSL; fd: cint): cint; cdecl;
begin
  if (ssl = nil) then Exit(0);

  ssl^.s := TSocket(fd);
  Result := 1;
end;

function SSL_CTX_new(method: PSSL_METHOD): PSSL_CTX; cdecl;
begin
  if (g_hSecurity = 0) then Exit(nil);

  Result := GetMem(SizeOf(SSL_CTX));

  Result^.dwProtocol := DWORD(method);
end;

procedure SSL_CTX_free(ctx: PSSL_CTX); cdecl;
begin
  FreeMem(ctx);
end;

function SSL_new(ctx: PSSL_CTX): PSSL; cdecl;
var
  SchannelCred: SCHANNEL_CRED;
  tsExpiry: TimeStamp;
  scRet: SECURITY_STATUS;
begin
  if (ctx = nil) then Exit(nil);

  Result := GetMem(SizeOf(SSL));
  ZeroMemory(Result, SizeOf(SSL));
  Result^.ctx := ctx;

  ZeroMemory(@SchannelCred, SizeOf(SchannelCred));

  SchannelCred.dwVersion := SCHANNEL_CRED_VERSION;
  SchannelCred.grbitEnabledProtocols := ctx^.dwProtocol;

  SchannelCred.dwFlags := SchannelCred.dwFlags or SCH_CRED_NO_DEFAULT_CREDS;

  if (not ctx^.bVerify) then
    SchannelCred.dwFlags := SchannelCred.dwFlags or SCH_CRED_MANUAL_CRED_VALIDATION;

  // Create an SSPI credential.
  scRet := g_pSSPI^.AcquireCredentialsHandleA(
                      nil,                   // Name of principal
                      UNISP_NAME_A,          // Name of package
                      SECPKG_CRED_OUTBOUND,  // Flags indicating use
                      nil,                   // Pointer to logon ID
                      @SchannelCred,         // Package specific data
                      nil,                   // Pointer to GetKey() func
                      nil,                   // Value to pass to GetKey()
                      @Result^.hCreds,       // (out) Cred Handle
                      @tsExpiry);            // (out) Lifetime (optional)

  if (scRet <> SEC_E_OK) then
  begin
    FreeMem(Result);
    Result := nil;
  end;
end;

procedure SSL_free(ssl: PSSL); cdecl;
begin
  if (ssl = nil) then Exit;

  g_pSSPI^.FreeCredentialHandle(@ssl^.hCreds);
  g_pSSPI^.DeleteSecurityContext(@ssl^.hContext);

  FreeMem(ssl^.pbRecDataBuf);
  FreeMem(ssl^.pbIoBuffer);
  FreeMem(ssl);
end;

function ClientHandshakeLoop(ssl: PSSL; fDoInitialRead: BOOL): SECURITY_STATUS;
var
  InBuffer: SecBufferDesc;
  InBuffers: array [0..1] of SecBuffer;
  OutBuffer: SecBufferDesc;
  OutBuffers: array [0..0] of SecBuffer;
  dwSSPIFlags: DWORD;
  dwSSPIOutFlags: DWORD = 0;
  tsExpiry: TimeStamp;
  scRet: SECURITY_STATUS;
  cbData: LONG;
  fDoRead: BOOL;
  tv: TTimeVal = (tv_sec: 10; tv_usec: 0);
  fd: TFDSet;
begin
  dwSSPIFlags := ISC_REQ_SEQUENCE_DETECT   or
                 ISC_REQ_REPLAY_DETECT     or
                 ISC_REQ_CONFIDENTIALITY   or
                 ISC_RET_EXTENDED_ERROR    or
                 ISC_REQ_ALLOCATE_MEMORY   or
                 ISC_REQ_STREAM;

  ssl^.cbIoBuffer := 0;

  fDoRead := fDoInitialRead;

  scRet := SEC_I_CONTINUE_NEEDED;

  // Loop until the handshake is finished or an error occurs.
  while (scRet = SEC_I_CONTINUE_NEEDED)        or
        (scRet = SEC_E_INCOMPLETE_MESSAGE)     or
        (scRet = SEC_I_INCOMPLETE_CREDENTIALS) do
  begin
    // Read server data
    if (0 = ssl^.cbIoBuffer) or (scRet = SEC_E_INCOMPLETE_MESSAGE) then
    begin
      if (fDoRead) then
      begin
        // If buffer not large enough reallocate buffer
        if (ssl^.sbIoBuffer <= ssl^.cbIoBuffer) then
        begin
          ssl^.sbIoBuffer += 2048;
          ssl^.pbIoBuffer := PUCHAR(ReAllocMem(ssl^.pbIoBuffer, ssl^.sbIoBuffer));
        end;

        FD_ZERO(fd);
        FD_SET(ssl^.s, fd);
        if (select(1, @fd, nil, nil, @tv) <> 1) then
        begin
          scRet := SEC_E_INTERNAL_ERROR;
          break;
        end;

        cbData := recv(ssl^.s,
                       ssl^.pbIoBuffer + ssl^.cbIoBuffer,
                       ssl^.sbIoBuffer - ssl^.cbIoBuffer,
                       0);
        if (cbData = SOCKET_ERROR) then
        begin
          scRet := SEC_E_INTERNAL_ERROR;
          break;
        end
        else if (cbData = 0) then
        begin
          scRet := SEC_E_INTERNAL_ERROR;
          break;
        end;

        ssl^.cbIoBuffer += cbData;
      end
      else begin
        fDoRead := TRUE;
      end;
    end;


    // Set up the input buffers. Buffer 0 is used to pass in data
    // received from the server. Schannel will consume some or all
    // of this. Leftover data (if any) will be placed in buffer 1 and
    // given a buffer type of SECBUFFER_EXTRA.

    InBuffers[0].pvBuffer   := ssl^.pbIoBuffer;
    InBuffers[0].cbBuffer   := ssl^.cbIoBuffer;
    InBuffers[0].BufferType := SECBUFFER_TOKEN;

    InBuffers[1].pvBuffer   := nil;
    InBuffers[1].cbBuffer   := 0;
    InBuffers[1].BufferType := SECBUFFER_EMPTY;

    InBuffer.cBuffers       := 2;
    InBuffer.pBuffers       := InBuffers;
    InBuffer.ulVersion      := SECBUFFER_VERSION;

    // Set up the output buffers. These are initialized to NULL
    // so as to make it less likely we'll attempt to free random
    // garbage later.

    OutBuffers[0].pvBuffer  := nil;
    OutBuffers[0].BufferType:= SECBUFFER_TOKEN;
    OutBuffers[0].cbBuffer  := 0;

    OutBuffer.cBuffers      := 1;
    OutBuffer.pBuffers      := OutBuffers;
    OutBuffer.ulVersion     := SECBUFFER_VERSION;

    scRet := g_pSSPI^.InitializeSecurityContextA(@ssl^.hCreds,
                                                 @ssl^.hContext,
                                                 nil,
                                                 dwSSPIFlags,
                                                 0,
                                                 SECURITY_NATIVE_DREP,
                                                 @InBuffer,
                                                 0,
                                                 nil,
                                                 @OutBuffer,
                                                 dwSSPIOutFlags,
                                                 @tsExpiry);

    // If success (or if the error was one of the special extended ones),
    // send the contents of the output buffer to the server.
    if (scRet = SEC_E_OK)                or
       (scRet = SEC_I_CONTINUE_NEEDED)   or
       (FAILED(scRet) and (dwSSPIOutFlags and ISC_RET_EXTENDED_ERROR <> 0)) then
    begin
      if (OutBuffers[0].cbBuffer <> 0) and (OutBuffers[0].pvBuffer <> nil) then
      begin
        cbData := send(ssl^.s,
                       OutBuffers[0].pvBuffer,
                       OutBuffers[0].cbBuffer,
                       0);
        if (cbData = SOCKET_ERROR) or (cbData = 0) then
        begin
            g_pSSPI^.FreeContextBuffer(OutBuffers[0].pvBuffer);
            g_pSSPI^.DeleteSecurityContext(@ssl^.hContext);
            Exit(SEC_E_INTERNAL_ERROR);
        end;

        // Free output buffer.
        g_pSSPI^.FreeContextBuffer(OutBuffers[0].pvBuffer);
        OutBuffers[0].pvBuffer := nil;
      end;
    end;

    // we need to read more data from the server and try again.
    if (scRet = SEC_E_INCOMPLETE_MESSAGE) then continue;

    // handshake completed successfully.
    if (scRet = SEC_E_OK) then
    begin
      // Store remaining data for further use
      if (InBuffers[1].BufferType = SECBUFFER_EXTRA) then
        begin
          ssl^.exIoBuffer := True;
          MoveMemory(ssl^.pbIoBuffer,
                     ssl^.pbIoBuffer + (ssl^.cbIoBuffer - InBuffers[1].cbBuffer),
                     InBuffers[1].cbBuffer);
          ssl^.cbIoBuffer := InBuffers[1].cbBuffer;
        end
      else
        ssl^.cbIoBuffer := 0;
      break;
    end;

    // Check for fatal error.
    if (FAILED(scRet)) then break;

    // server just requested client authentication.
    if (scRet = SEC_I_INCOMPLETE_CREDENTIALS) then
    begin
      // Server has requested client authentication and
      // GetNewClientCredentials(ssl);

      // Go around again.
      fDoRead := FALSE;
      scRet := SEC_I_CONTINUE_NEEDED;
      continue;
    end;


    // Copy any leftover data from the buffer, and go around again.
    if ( InBuffers[1].BufferType = SECBUFFER_EXTRA ) then
      begin
        ssl^.exIoBuffer := True;
        MoveMemory(ssl^.pbIoBuffer,
                   ssl^.pbIoBuffer + (ssl^.cbIoBuffer - InBuffers[1].cbBuffer),
                   InBuffers[1].cbBuffer);

        ssl^.cbIoBuffer := InBuffers[1].cbBuffer;
      end
    else
      ssl^.cbIoBuffer := 0;
  end;

  // Delete the security context in the case of a fatal error.
  if (FAILED(scRet)) then
  begin
    g_pSSPI^.DeleteSecurityContext(@ssl^.hContext);
  end;

  if (ssl^.cbIoBuffer = 0) then
  begin
    FreeMem(ssl^.pbIoBuffer);
    ssl^.pbIoBuffer := nil;
    ssl^.sbIoBuffer := 0;
  end;

  Result := scRet;
end;


function SSL_connect(ssl: PSSL): cint; cdecl;
var
  OutBuffer: SecBufferDesc;
  OutBuffers: array[0..0] of SecBuffer;
  dwSSPIFlags: DWORD;
  dwSSPIOutFlags: DWORD = 0;
  tsExpiry: TimeStamp;
  scRet: SECURITY_STATUS;
  cbData: LONG;
  sock: TVarSin;
begin
  if (ssl = nil) then Exit(0);

  dwSSPIFlags := ISC_REQ_SEQUENCE_DETECT   or
                 ISC_REQ_REPLAY_DETECT     or
                 ISC_REQ_CONFIDENTIALITY   or
                 ISC_RET_EXTENDED_ERROR    or
                 ISC_REQ_ALLOCATE_MEMORY   or
                 ISC_REQ_STREAM;

  //  Initiate a ClientHello message and generate a token.

  OutBuffers[0].pvBuffer   := nil;
  OutBuffers[0].BufferType := SECBUFFER_TOKEN;
  OutBuffers[0].cbBuffer   := 0;

  OutBuffer.cBuffers  := 1;
  OutBuffer.pBuffers  := OutBuffers;
  OutBuffer.ulVersion := SECBUFFER_VERSION;

  GetPeerName(ssl^.s, sock);

  scRet := g_pSSPI^.InitializeSecurityContextA(
                  @ssl^.hCreds,
                  nil,
                  inet_ntoa(sock.sin_addr),
                  dwSSPIFlags,
                  0,
                  SECURITY_NATIVE_DREP,
                  nil,
                  0,
                  @ssl^.hContext,
                  @OutBuffer,
                  dwSSPIOutFlags,
                  @tsExpiry);

  if (scRet <> SEC_I_CONTINUE_NEEDED) then
  begin
    Exit(0);
  end;

  // Send response to server if there is one.
  if (OutBuffers[0].cbBuffer <> 0) and (OutBuffers[0].pvBuffer <> nil) then
  begin
    cbData := send(ssl^.s,
                   OutBuffers[0].pvBuffer,
                   OutBuffers[0].cbBuffer,
                   0);
    if (cbData = SOCKET_ERROR) or (cbData = 0) then
    begin
      g_pSSPI^.FreeContextBuffer(OutBuffers[0].pvBuffer);
      g_pSSPI^.DeleteSecurityContext(@ssl^.hContext);
      Exit(0);
    end;

    // Free output buffer.
    g_pSSPI^.FreeContextBuffer(OutBuffers[0].pvBuffer);
    OutBuffers[0].pvBuffer := nil;
  end;

  Result := cint(ClientHandshakeLoop(ssl, TRUE) = SEC_E_OK);
end;

function SSL_shutdown(ssl: PSSL): cint; cdecl;
var
  dwType: DWORD;
  OutBuffer: SecBufferDesc;
  OutBuffers: array[0..0] of SecBuffer;
  dwSSPIFlags: DWORD;
  dwSSPIOutFlags: DWORD = 0;
  tsExpiry: TimeStamp;
  Status: DWORD;
begin
  if (ssl = nil) then Exit(SOCKET_ERROR);

  dwType := SCHANNEL_SHUTDOWN;

  OutBuffers[0].pvBuffer   := @dwType;
  OutBuffers[0].BufferType := SECBUFFER_TOKEN;
  OutBuffers[0].cbBuffer   := SizeOf(dwType);

  OutBuffer.cBuffers  := 1;
  OutBuffer.pBuffers  := OutBuffers;
  OutBuffer.ulVersion := SECBUFFER_VERSION;

  Status := g_pSSPI^.ApplyControlToken(@ssl^.hContext, @OutBuffer);
  if (FAILED(Status)) then Exit(cint(ssl^.rmshtdn));

  //
  // Build an SSL close notify message.
  //

  dwSSPIFlags := ISC_REQ_SEQUENCE_DETECT   or
                 ISC_REQ_REPLAY_DETECT     or
                 ISC_REQ_CONFIDENTIALITY   or
                 ISC_RET_EXTENDED_ERROR    or
                 ISC_REQ_ALLOCATE_MEMORY   or
                 ISC_REQ_STREAM;

  OutBuffers[0].pvBuffer   := nil;
  OutBuffers[0].BufferType := SECBUFFER_TOKEN;
  OutBuffers[0].cbBuffer   := 0;

  OutBuffer.cBuffers  := 1;
  OutBuffer.pBuffers  := OutBuffers;
  OutBuffer.ulVersion := SECBUFFER_VERSION;

  Status := g_pSSPI^.InitializeSecurityContextA(
                     @ssl^.hCreds,
                     @ssl^.hContext,
                     nil,
                     dwSSPIFlags,
                     0,
                     SECURITY_NATIVE_DREP,
                     nil,
                     0,
                     @ssl^.hContext,
                     @OutBuffer,
                     dwSSPIOutFlags,
                     @tsExpiry);

  if (FAILED(Status)) then Exit(cint(ssl^.rmshtdn));

  // Send the close notify message to the server.
  if (OutBuffers[0].pvBuffer <> nil) and (OutBuffers[0].cbBuffer <> 0) then
  begin
    send(ssl^.s, OutBuffers[0].pvBuffer, OutBuffers[0].cbBuffer, 0);
    g_pSSPI^.FreeContextBuffer(OutBuffers[0].pvBuffer);
  end;
    
  // Free the security context.
  g_pSSPI^.DeleteSecurityContext(@ssl^.hContext);

  Result := cint(ssl^.rmshtdn);
end;

function SSL_read(ssl: PSSL; buf: PByte; num: cint): cint; cdecl;
var
  scRet: SECURITY_STATUS;
  cbData: LONG;
  i: cint;
  Message: SecBufferDesc;
  Buffers: array [0..3] of SecBuffer;
  pDataBuffer: PSecBuffer;
  pExtraBuffer: PSecBuffer;
  bytes, rbytes: LONG;
  fQOP: ULONG = 0;
begin
  if (ssl = nil) then Exit(SOCKET_ERROR);

  if (num = 0) then Exit(0);

  if (ssl^.cbRecDataBuf <> 0) then
  begin
    bytes := Min(num, ssl^.cbRecDataBuf);
    CopyMemory(buf, ssl^.pbRecDataBuf, bytes);

    rbytes := ssl^.cbRecDataBuf - bytes;
    MoveMemory(ssl^.pbRecDataBuf, ssl^.pbRecDataBuf + bytes, rbytes);
    ssl^.cbRecDataBuf := rbytes;

    Exit(bytes);
  end;

  scRet := SEC_E_OK;

  while (True) do
  begin
    if (0 = ssl^.cbIoBuffer) or (scRet = SEC_E_INCOMPLETE_MESSAGE) then
    begin
      if (ssl^.sbIoBuffer <= ssl^.cbIoBuffer) then
      begin
        ssl^.sbIoBuffer += 2048;
        ssl^.pbIoBuffer := PUCHAR(ReAllocMem(ssl^.pbIoBuffer, ssl^.sbIoBuffer));
      end;

      cbData := recv(ssl^.s, ssl^.pbIoBuffer + ssl^.cbIoBuffer, ssl^.sbIoBuffer - ssl^.cbIoBuffer, 0);
      if (cbData = SOCKET_ERROR) then
      begin
        Exit(SOCKET_ERROR);
      end
      else if (cbData = 0) then
      begin
        // Server disconnected.
        if (ssl^.cbIoBuffer <> 0) then
        begin
          scRet := SEC_E_INTERNAL_ERROR;
          Exit(SOCKET_ERROR);
        end
        else
          Exit(0);
      end
      else
        ssl^.cbIoBuffer += cbData;
    end;

    // Attempt to decrypt the received data.
    Buffers[0].pvBuffer     := ssl^.pbIoBuffer;
    Buffers[0].cbBuffer     := ssl^.cbIoBuffer;
    Buffers[0].BufferType   := SECBUFFER_DATA;

    Buffers[1].BufferType   := SECBUFFER_EMPTY;
    Buffers[2].BufferType   := SECBUFFER_EMPTY;
    Buffers[3].BufferType   := SECBUFFER_EMPTY;

    Message.ulVersion       := SECBUFFER_VERSION;
    Message.cBuffers        := 4;
    Message.pBuffers        := Buffers;

    if (@g_pSSPI^.DecryptMessage <> nil) then
      scRet := g_pSSPI^.DecryptMessage(@ssl^.hContext, @Message, 0, fQOP)
    else
      scRet := DECRYPT_MESSAGE_FN(g_pSSPI^.Reserved4)(@ssl^.hContext, @Message, 0, fQOP);

    if (scRet = SEC_E_INCOMPLETE_MESSAGE) then
    begin
      // The input buffer contains only a fragment of an
      // encrypted record. Loop around and read some more
      // data.
      continue;
    end;

    // Server signaled end of session
    if (scRet = SEC_I_CONTEXT_EXPIRED) then
    begin
      ssl^.rmshtdn := TRUE;
      SSL_shutdown(ssl);
      Exit(0);
    end;

    if (scRet <> SEC_E_OK) and
       (scRet <> SEC_I_RENEGOTIATE) and
       (scRet <> SEC_I_CONTEXT_EXPIRED) then
    begin
      Exit(SOCKET_ERROR);
    end;

    // Locate data and (optional) extra buffers.
    pDataBuffer  := nil;
    pExtraBuffer := nil;
    for i := 1 to 3 do
    begin
      if (pDataBuffer = nil) and (Buffers[i].BufferType = SECBUFFER_DATA) then
      begin
        pDataBuffer := @Buffers[i];
      end;

      if (pExtraBuffer = nil) and (Buffers[i].BufferType = SECBUFFER_EXTRA) then
      begin
        pExtraBuffer := @Buffers[i];
      end;
    end;

    // Return decrypted data.
    if Assigned(pDataBuffer) then
    begin
      bytes := Min(num, pDataBuffer^.cbBuffer);
      CopyMemory(buf, pDataBuffer^.pvBuffer, bytes);

      rbytes := pDataBuffer^.cbBuffer - bytes;
      if (rbytes > 0) then
      begin
        if (ssl^.sbRecDataBuf < rbytes) then
        begin
          ssl^.sbRecDataBuf := rbytes;
          ssl^.pbRecDataBuf := PUCHAR(ReAllocMem(ssl^.pbRecDataBuf, rbytes));
        end;
        CopyMemory(ssl^.pbRecDataBuf, pDataBuffer^.pvBuffer + bytes, rbytes);
        ssl^.cbRecDataBuf := rbytes;
      end;
    end;

    // Move any "extra" data to the input buffer.
    if Assigned(pExtraBuffer) then
    begin
      MoveMemory(ssl^.pbIoBuffer, pExtraBuffer^.pvBuffer, pExtraBuffer^.cbBuffer);
      ssl^.cbIoBuffer := pExtraBuffer^.cbBuffer;
    end
    else
      ssl^.cbIoBuffer := 0;

    if (pDataBuffer <> nil) and (bytes <> 0) then Exit(bytes);

    if (scRet = SEC_I_RENEGOTIATE) then
    begin
      // The server wants to perform another handshake
      // sequence.

      scRet := ClientHandshakeLoop(ssl, FALSE);
      if (scRet <> SEC_E_OK) then Exit(SOCKET_ERROR);
    end;
  end;
end;


function SSL_write(ssl: PSSL; const buf: PByte; num: cint): cint; cdecl;
var
  Sizes: SecPkgContext_StreamSizes;
  scRet: SECURITY_STATUS;
  cbData: LONG;
  Message: SecBufferDesc;
  Buffers: array[0..3] of SecBuffer;
  pbDataBuffer: PUCHAR;
  pbMessage: PUCHAR;
  cbMessage: DWORD;
  sendOff: DWORD = 0;
begin
  if (ssl = nil) then Exit(SOCKET_ERROR);

  FillChar(Buffers, SizeOf(Buffers), 0);

  scRet := g_pSSPI^.QueryContextAttributesA(@ssl^.hContext, SECPKG_ATTR_STREAM_SIZES, @Sizes);
  if (scRet <> SEC_E_OK) then Exit(scRet);

  pbDataBuffer := PUCHAR(GetMem(Sizes.cbMaximumMessage + Sizes.cbHeader + Sizes.cbTrailer));

  pbMessage := pbDataBuffer + Sizes.cbHeader;

  while (sendOff < DWORD(num)) do
  begin
    cbMessage := Min(Sizes.cbMaximumMessage, DWORD(num) - sendOff);
    CopyMemory(pbMessage, buf + sendOff, cbMessage);

    Buffers[0].pvBuffer     := pbDataBuffer;
    Buffers[0].cbBuffer     := Sizes.cbHeader;
    Buffers[0].BufferType   := SECBUFFER_STREAM_HEADER;

    Buffers[1].pvBuffer     := pbMessage;
    Buffers[1].cbBuffer     := cbMessage;
    Buffers[1].BufferType   := SECBUFFER_DATA;

    Buffers[2].pvBuffer     := pbMessage + cbMessage;
    Buffers[2].cbBuffer     := Sizes.cbTrailer;
    Buffers[2].BufferType   := SECBUFFER_STREAM_TRAILER;

    Buffers[3].BufferType   := SECBUFFER_EMPTY;

    Message.ulVersion       := SECBUFFER_VERSION;
    Message.cBuffers        := 4;
    Message.pBuffers        := Buffers;

    if (@g_pSSPI^.EncryptMessage <> nil) then
      scRet := g_pSSPI^.EncryptMessage(@ssl^.hContext, 0, @Message, 0)
    else
      scRet := ENCRYPT_MESSAGE_FN(g_pSSPI^.Reserved3)(@ssl^.hContext, 0, @Message, 0);

    if (FAILED(scRet)) then break;

    // Calculate encrypted packet size 
    cbData := Buffers[0].cbBuffer + Buffers[1].cbBuffer + Buffers[2].cbBuffer;

    // Send the encrypted data to the server.
    cbData := send(ssl^.s, pbDataBuffer, cbData, 0);
    if (cbData = SOCKET_ERROR) or (cbData = 0) then
    begin
      g_pSSPI^.DeleteSecurityContext(@ssl^.hContext);
      scRet := SEC_E_INTERNAL_ERROR;
      break;
    end;

    sendOff += cbMessage;
  end;

  FreeMem(pbDataBuffer);

  if scRet = SEC_E_OK then
    Result := num
  else
    Result := SOCKET_ERROR;
end;

function SSL_pending(ssl: PSSL): cint; cdecl;
begin
  if (ssl = nil) then Exit(0);

  if ssl^.cbRecDataBuf > 0 then
    Result := ssl^.cbRecDataBuf
  else if ssl^.exIoBuffer then
    begin
      ssl^.exIoBuffer := False;
      Result := ssl^.cbIoBuffer
    end
  else
    Result := 0;
end;

function SSLv23_method(): PSSL_METHOD; cdecl;
begin
  Result:= PSSL_METHOD(SP_PROT_SSL3 or SP_PROT_TLS1 or SP_PROT_TLS1_1);
end;

function SSLv2_method(): PSSL_METHOD; cdecl;
begin
  Result := PSSL_METHOD(SP_PROT_SSL2);
end;

function SSLv3_method(): PSSL_METHOD; cdecl;
begin
  Result := PSSL_METHOD(SP_PROT_SSL3);
end;

function TLSv1_method(): PSSL_METHOD; cdecl;
begin
  Result := PSSL_METHOD(SP_PROT_TLS1);
end;

function TLSv1_1_method(): PSSL_METHOD; cdecl;
begin
  Result := PSSL_METHOD(SP_PROT_TLS1_1);
end;

function TLSv1_2_method(): PSSL_METHOD; cdecl;
begin
  Result := PSSL_METHOD(SP_PROT_TLS1_2);
end;

procedure SSL_CTX_set_verify(ctx: PSSL_CTX; mode: cint; func: Pointer); cdecl;
begin
  if (ctx <> nil) then ctx^.bVerify := mode <> 0;
end;

function SSL_get_error (ssl: PSSL; ret: cint): cint; cdecl;
begin
  if (ret > 0) then
    Result := SSL_ERROR_NONE
  else
    Result := SSL_ERROR_ZERO_RETURN;
end;

var
  lpBuffer: TMemoryBasicInformation;
begin
  if (IsSSLloaded = False) then
  begin
    if VirtualQuery(@lpBuffer, @lpBuffer, SizeOf(lpBuffer)) = SizeOf(lpBuffer) then
    begin
      SetLength(DLLSSLName, MAX_PATH);
      SetLength(DLLSSLName, GetModuleFileName(THandle(lpBuffer.AllocationBase),
                                              PAnsiChar(DLLSSLName), MAX_PATH));
      DLLUtilName := DLLSSLName;

      if InitSSLInterface then
        SSLImplementation := TSSLOpenSSL;
    end;
  end;
end.
