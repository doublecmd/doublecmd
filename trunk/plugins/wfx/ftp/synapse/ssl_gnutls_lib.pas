{
  GnuTLS to OpenSSL wrapper (based on GNUTLS-EXTRA)

  Copyright (c) 2013-2015 Alexander Koblov <alexx2000@mail.ru>
  Copyright (c) 2004, 2005, 2006 Free Software Foundation
  Copyright (c) 2002 Andrew McDonald <andrew@mcdonald.org.uk>

  GNUTLS-EXTRA is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License as
  published by the Free Software Foundation; either version 2 of the
  License, or (at your option) any later version.

  GNUTLS-EXTRA is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with GNUTLS-EXTRA; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
  02110-1301, USA.
}

unit ssl_gnutls_lib;

{$mode delphi}

interface

uses
  CTypes;

type
  gnutls_protocol_t =
  (
    GNUTLS_SSL3 = 1,
    GNUTLS_TLS1_0,
    GNUTLS_TLS1_1,
    GNUTLS_TLS1_2,
    GNUTLS_VERSION_UNKNOWN = $ff
  ) ;

  gnutls_cipher_algorithm_t =
  (
    GNUTLS_CIPHER_NULL = 1,
    GNUTLS_CIPHER_ARCFOUR_128,
    GNUTLS_CIPHER_3DES_CBC,
    GNUTLS_CIPHER_AES_128_CBC,
    GNUTLS_CIPHER_AES_256_CBC,
    GNUTLS_CIPHER_ARCFOUR_40,
    GNUTLS_CIPHER_CAMELLIA_128_CBC,
    GNUTLS_CIPHER_CAMELLIA_256_CBC,
    GNUTLS_CIPHER_RC2_40_CBC = 90,
    GNUTLS_CIPHER_DES_CBC
  );

  gnutls_kx_algorithm_t =
  (
    GNUTLS_KX_RSA = 1,
    GNUTLS_KX_DHE_DSS,
    GNUTLS_KX_DHE_RSA,
    GNUTLS_KX_ANON_DH,
    GNUTLS_KX_SRP,
    GNUTLS_KX_RSA_EXPORT,
    GNUTLS_KX_SRP_RSA,
    GNUTLS_KX_SRP_DSS,
    GNUTLS_KX_PSK,
    GNUTLS_KX_DHE_PSK
  );

  gnutls_mac_algorithm_t =
  (
    GNUTLS_MAC_UNKNOWN = 0,
    GNUTLS_MAC_NULL = 1,
    GNUTLS_MAC_MD5,
    GNUTLS_MAC_SHA1,
    GNUTLS_MAC_RMD160,
    GNUTLS_MAC_MD2,
    GNUTLS_MAC_SHA256,
    GNUTLS_MAC_SHA384,
    GNUTLS_MAC_SHA512
  );

  gnutls_compression_method_t =
  (
    GNUTLS_COMP_NULL = 1,
    GNUTLS_COMP_DEFLATE,
    GNUTLS_COMP_LZO
  );

  gnutls_certificate_type_t =
  (
    GNUTLS_CRT_X509 = 1,
    GNUTLS_CRT_OPENPGP
  );

  gnutls_connection_end_t =
  (
    GNUTLS_SERVER = 1,
    GNUTLS_CLIENT
  );

  gnutls_credentials_type_t =
  (
    GNUTLS_CRD_CERTIFICATE = 1,
    GNUTLS_CRD_ANON,
    GNUTLS_CRD_SRP,
    GNUTLS_CRD_PSK,
    GNUTLS_CRD_IA
  );

  gnutls_x509_crt_fmt_t =
  (
    GNUTLS_X509_FMT_DER,
    GNUTLS_X509_FMT_PEM
  );

  gnutls_close_request_t =
  (
    GNUTLS_SHUT_RDWR = 0,
    GNUTLS_SHUT_WR = 1
  );

type
  gnutls_session_t = type Pointer;
  gnutls_transport_ptr_t = type Pointer;
  gnutls_certificate_credentials_t = type Pointer;

  PSSL_METHOD = ^SSL_METHOD;
  SSL_METHOD = record
    connend: gnutls_connection_end_t;
    priorities: array[0..High(Byte)] of AnsiChar;
  end;

  PSSL_CIPHER = ^SSL_CIPHER;
  SSL_CIPHER = record
    version: gnutls_protocol_t;
    cipher: gnutls_cipher_algorithm_t;
    kx: gnutls_kx_algorithm_t;
    mac: gnutls_mac_algorithm_t;
    compression: gnutls_compression_method_t;
    cert: gnutls_certificate_type_t;
  end;

  PSSL_CTX = ^SSL_CTX;
  SSL_CTX = record
    method: PSSL_METHOD;
    certfile: AnsiString;
    certfile_type: gnutls_x509_crt_fmt_t;
    keyfile: AnsiString;
    keyfile_type: gnutls_x509_crt_fmt_t;
    verify_callback: Pointer;
    verify_mode: cint;
  end;

  PSSL = ^SSL;
  SSL = record
    gnutls_state: gnutls_session_t;
    gnutls_cred: gnutls_certificate_credentials_t;
    ctx: PSSL_CTX;
    ciphersuite: SSL_CIPHER;
    last_error: cint;
    shutdown: cint;
    state: cint;
    verify_callback: Pointer;
    verify_mode: cint;
  end;

function SSL_library_init (): cint; cdecl;
function SSL_CTX_new (method: PSSL_METHOD): PSSL_CTX; cdecl;
procedure SSL_CTX_free (ctx: PSSL_CTX); cdecl;
function SSL_CTX_use_certificate_file (ctx: PSSL_CTX; const certfile: PAnsiChar; certtype: gnutls_x509_crt_fmt_t): cint; cdecl;
function SSL_CTX_use_PrivateKey_file (ctx: PSSL_CTX; const keyfile: PAnsiChar; keytype: gnutls_x509_crt_fmt_t): cint; cdecl;
procedure SSL_CTX_set_verify (ctx: PSSL_CTX; verify_mode: cint;
  		            verify_callback: pointer); cdecl;
function SSL_new (ctx: PSSL_CTX): PSSL; cdecl;
procedure SSL_free (ssl: PSSL); cdecl;
function SSL_get_error (ssl: PSSL; ret: cint): cint; cdecl;
function SSL_set_fd (ssl: PSSL; fd: cint): cint; cdecl;
function SSL_pending (ssl: PSSL): cint; cdecl;
function SSL_connect (ssl: PSSL): cint; cdecl;
function SSL_shutdown (ssl: PSSL): cint; cdecl;
function SSL_read (ssl: PSSL; buf: PByte; len: cint): cint; cdecl;
function SSL_write (ssl: PSSL; const buf: PByte; len: cint): cint; cdecl;
function SSLv23_method(): PSSL_METHOD; cdecl;
function SSLv2_method(): PSSL_METHOD; cdecl;
function SSLv3_method(): PSSL_METHOD; cdecl;
function TLSv1_method(): PSSL_METHOD; cdecl;
function TLSv1_1_method(): PSSL_METHOD; cdecl;
function TLSv1_2_method(): PSSL_METHOD; cdecl;
function SSL_get_current_cipher (ssl: PSSL): PSSL_CIPHER; cdecl;
function SSL_CIPHER_get_name (cipher: PSSL_CIPHER): PAnsiChar; cdecl;
function SSL_CIPHER_get_bits (cipher: PSSL_CIPHER; bits: pcint): cint; cdecl;
function ERR_get_error (): culong; cdecl;
function ERR_error_string (e: culong; buf: PAnsiChar): PAnsiChar; cdecl;

implementation

uses
  SysUtils, DynLibs,
  ssl_openssl_lib, ssl_openssl, blcksock, dl;

threadvar
  last_error: cint;

var
  gnutls_global_init: function(): cint; cdecl;

  gnutls_init: function(out session: gnutls_session_t; con_end: gnutls_connection_end_t): cint; cdecl;
  gnutls_deinit: procedure(session: gnutls_session_t); cdecl;

  gnutls_priority_set_direct: function(session: gnutls_session_t; const priorities: PAnsiChar; const err_pos: PPAnsiChar): cint; cdecl;
  gnutls_credentials_set: function(session: gnutls_session_t; cred_type: gnutls_credentials_type_t; cred: Pointer): cint; cdecl;
  gnutls_certificate_set_x509_trust_file: function(res: gnutls_certificate_credentials_t; const CAFILE: PAnsiChar; crt_type: gnutls_x509_crt_fmt_t): cint; cdecl;
  gnutls_certificate_set_x509_key_file: function(res: gnutls_certificate_credentials_t; const CERTFILE: PAnsiChar; const KEYFILE: PAnsiChar; crt_type: gnutls_x509_crt_fmt_t): cint; cdecl;

  gnutls_certificate_allocate_credentials: function(out res: gnutls_certificate_credentials_t): cint; cdecl;
  gnutls_certificate_free_credentials: procedure(sc: gnutls_certificate_credentials_t); cdecl;

  gnutls_transport_set_ptr: procedure(session: gnutls_session_t; ptr: gnutls_transport_ptr_t); cdecl;
  gnutls_record_check_pending: function(session: gnutls_session_t): csize_t; cdecl;

  gnutls_handshake: function(session: gnutls_session_t): cint; cdecl;
  gnutls_bye: function(session: gnutls_session_t; how: gnutls_close_request_t): cint; cdecl;

  gnutls_record_send: function(session: gnutls_session_t; const data: Pointer; sizeofdata: csize_t): PtrInt; cdecl;
  gnutls_record_recv: function(session: gnutls_session_t; data: Pointer; sizeofdata: csize_t): PtrInt; cdecl;

  gnutls_protocol_get_version: function(session: gnutls_session_t): gnutls_protocol_t; cdecl;
  gnutls_cipher_get: function(session: gnutls_session_t): gnutls_cipher_algorithm_t; cdecl;
  gnutls_kx_get: function(session: gnutls_session_t): gnutls_kx_algorithm_t; cdecl;
  gnutls_mac_get: function(session: gnutls_session_t): gnutls_mac_algorithm_t; cdecl;
  gnutls_compression_get: function(session: gnutls_session_t): gnutls_compression_method_t; cdecl;
  gnutls_certificate_type_get: function(session: gnutls_session_t): gnutls_certificate_type_t; cdecl;
  gnutls_cipher_suite_get_name: function(kx_algorithm: gnutls_kx_algorithm_t; cipher_algorithm: gnutls_cipher_algorithm_t;
                                         mac_algorithm: gnutls_mac_algorithm_t): PAnsiChar; cdecl;
  gnutls_cipher_get_key_size: function(algorithm: gnutls_cipher_algorithm_t): csize_t; cdecl;

  gnutls_strerror: function(error: cint): PAnsiChar; cdecl;

  gnutls_check_version: function(const req_version: PAnsiChar): PAnsiChar; cdecl;

(* Library initialisation functions *)

function SSL_library_init (): cint; cdecl;
begin
  gnutls_global_init ();
  (* NB: we haven't got anywhere to call gnutls_global_deinit() *)
  Result := 1;
end;

(* SSL_CTX structure handling *)

function SSL_CTX_new (method: PSSL_METHOD): PSSL_CTX; cdecl;
begin
  Result := GetMem(SizeOf(SSL_CTX));
  if Assigned(Result) then
  begin
    FillChar(Result^, SizeOf(SSL_CTX), 0);
    Result^.method := method;
  end;
end;

procedure SSL_CTX_free (ctx: PSSL_CTX); cdecl;
begin
  FreeMem(ctx^.method);
  FreeMem(ctx);
end;

function SSL_CTX_use_certificate_file (ctx: PSSL_CTX; const certfile: PAnsiChar; certtype: gnutls_x509_crt_fmt_t): cint; cdecl;
begin
  ctx^.certfile := StrPas(certfile);

  ctx^.certfile_type := certtype;

  Result := 1;
end;

function SSL_CTX_use_PrivateKey_file (ctx: PSSL_CTX; const keyfile: PAnsiChar; keytype: gnutls_x509_crt_fmt_t): cint; cdecl;
begin
  ctx^.keyfile := StrPas(keyfile);

  ctx^.keyfile_type := keytype;

  Result := 1;
end;

procedure SSL_CTX_set_verify (ctx: PSSL_CTX; verify_mode: cint;
		              verify_callback: pointer); cdecl;
begin
  ctx^.verify_mode := verify_mode;
  ctx^.verify_callback := verify_callback;
end;

(* SSL structure handling *)

function SSL_new (ctx: PSSL_CTX): PSSL; cdecl;
var
  err: cint;
begin
  Result := GetMem(SizeOf(SSL));
  if not Assigned(Result) then Exit;
  FillChar(Result^, SizeOf(SSL), 0);

  err := gnutls_certificate_allocate_credentials (Result^.gnutls_cred);
  if (err < 0) then
  begin
    last_error := err;
    FreeMem (Result);
    Exit(nil);
  end;

  gnutls_init (Result^.gnutls_state, ctx^.method^.connend);

  gnutls_priority_set_direct (Result^.gnutls_state, ctx^.method^.priorities, nil);

  gnutls_credentials_set (Result^.gnutls_state, GNUTLS_CRD_CERTIFICATE,
			  Result^.gnutls_cred);

  if Length(ctx^.certfile) > 0 then
    gnutls_certificate_set_x509_trust_file (Result^.gnutls_cred,
					    PAnsiChar(ctx^.certfile),
					    ctx^.certfile_type);
  if Length(ctx^.keyfile) > 0  then
    gnutls_certificate_set_x509_key_file (Result^.gnutls_cred,
					  PAnsiChar(ctx^.certfile),
                                          PAnsiChar(ctx^.keyfile),
					  ctx^.keyfile_type);

  Result^.ctx := ctx;
  Result^.verify_mode := ctx^.verify_mode;
  Result^.verify_callback := ctx^.verify_callback;
end;

procedure SSL_free (ssl: PSSL); cdecl;
begin
  gnutls_certificate_free_credentials (ssl^.gnutls_cred);
  gnutls_deinit (ssl^.gnutls_state);
  FreeMem(ssl);
end;

function SSL_get_error (ssl: PSSL; ret: cint): cint; cdecl;
begin
  if (ret > 0) then
    Result := SSL_ERROR_NONE
  else
    Result := SSL_ERROR_ZERO_RETURN;
end;

function SSL_set_fd (ssl: PSSL; fd: cint): cint; cdecl;
begin
  {$PUSH}{$HINTS OFF}{$WARNINGS OFF}
  gnutls_transport_set_ptr (ssl^.gnutls_state, Pointer(fd));
  {$POP}
  Result := 1;
end;

function SSL_pending (ssl: PSSL): cint; cdecl;
begin
  Result := gnutls_record_check_pending (ssl^.gnutls_state);
end;

(* SSL connection open/close/read/write functions *)

function SSL_connect (ssl: PSSL): cint; cdecl;
var
  err: cint;
begin
  err := gnutls_handshake (ssl^.gnutls_state);
  ssl^.last_error := err;

  if (err < 0) then
  begin
    last_error := err;
    Exit(0);
  end;

  Result := 1;
end;

function SSL_shutdown (ssl: PSSL): cint; cdecl;
begin
  if (ssl^.shutdown <> 0) then
    begin
      gnutls_bye (ssl^.gnutls_state, GNUTLS_SHUT_WR);
      Inc(ssl^.shutdown);
    end
  else
    begin
      gnutls_bye (ssl^.gnutls_state, GNUTLS_SHUT_RDWR);
      Inc(ssl^.shutdown);
    end;

  (* FIXME *)
  Result := 1;
end;

function SSL_read (ssl: PSSL; buf: PByte; len: cint): cint; cdecl;
begin
  Result := gnutls_record_recv (ssl^.gnutls_state, buf, len);
  ssl^.last_error := Result;

  if (Result < 0) then
  begin
    last_error := Result;
    Result := 0;
  end;
end;

function SSL_write (ssl: PSSL; const buf: PByte; len: cint): cint; cdecl;
begin
  Result := gnutls_record_send (ssl^.gnutls_state, buf, len);
  ssl^.last_error := Result;

  if (Result < 0) then
  begin
    last_error := Result;
    Result := 0;
  end;
end;

(* SSL_METHOD functions *)

function SSLv23_method(): PSSL_METHOD; cdecl;
begin
  Result := GetMem(SizeOf(SSL_METHOD));
  if Assigned(Result) then
  begin
    Result^.connend := GNUTLS_CLIENT;
    Result^.priorities := 'NONE:+VERS-TLS-ALL:+VERS-SSL3.0:+CIPHER-ALL:+COMP-ALL:+RSA:+DHE-RSA:+DHE-DSS:+MAC-ALL';
  end;
end;

function SSLv2_method(): PSSL_METHOD; cdecl;
begin
  Result := nil;
end;

function SSLv3_method(): PSSL_METHOD; cdecl;
begin
  Result := GetMem(SizeOf(SSL_METHOD));
  if Assigned(Result) then
  begin
    Result^.connend := GNUTLS_CLIENT;
    Result^.priorities := 'NONE:+VERS-SSL3.0:+CIPHER-ALL:+COMP-ALL:+RSA:+DHE-RSA:+DHE-DSS:+MAC-ALL';
  end;
end;

function TLSv1_method(): PSSL_METHOD; cdecl;
begin
  Result := GetMem(SizeOf(SSL_METHOD));
  if Assigned(Result) then
  begin
    Result^.connend := GNUTLS_CLIENT;
    Result^.priorities := 'NONE:+VERS-TLS1.0:+CIPHER-ALL:+COMP-ALL:+RSA:+DHE-RSA:+DHE-DSS:+MAC-ALL';
  end;
end;

function TLSv1_1_method: PSSL_METHOD; cdecl;
begin
  Result := GetMem(SizeOf(SSL_METHOD));
  if Assigned(Result) then
  begin
    Result^.connend := GNUTLS_CLIENT;
    Result^.priorities := 'NONE:+VERS-TLS1.1:+CIPHER-ALL:+COMP-ALL:+RSA:+DHE-RSA:+DHE-DSS:+MAC-ALL';
  end;
end;

function TLSv1_2_method: PSSL_METHOD; cdecl;
begin
  Result := GetMem(SizeOf(SSL_METHOD));
  if Assigned(Result) then
  begin
    Result^.connend := GNUTLS_CLIENT;
    Result^.priorities := 'NONE:+VERS-TLS1.2:+CIPHER-ALL:+COMP-ALL:+RSA:+DHE-RSA:+DHE-DSS:+MAC-ALL';
  end;
end;

(* SSL_CIPHER functions *)

function SSL_get_current_cipher (ssl: PSSL): PSSL_CIPHER; cdecl;
begin
  if not Assigned(ssl) then Exit(nil);

  ssl^.ciphersuite.version := gnutls_protocol_get_version (ssl^.gnutls_state);
  ssl^.ciphersuite.cipher := gnutls_cipher_get (ssl^.gnutls_state);
  ssl^.ciphersuite.kx := gnutls_kx_get (ssl^.gnutls_state);
  ssl^.ciphersuite.mac := gnutls_mac_get (ssl^.gnutls_state);
  ssl^.ciphersuite.compression := gnutls_compression_get (ssl^.gnutls_state);
  ssl^.ciphersuite.cert := gnutls_certificate_type_get (ssl^.gnutls_state);

  Result := @ssl^.ciphersuite;
end;

function SSL_CIPHER_get_name (cipher: PSSL_CIPHER): PAnsiChar; cdecl;
begin
  if not Assigned(cipher) then Exit('NONE');

  Result := gnutls_cipher_suite_get_name (cipher^.kx,
				          cipher^.cipher, cipher^.mac);
end;

function SSL_CIPHER_get_bits (cipher: PSSL_CIPHER; bits: pcint): cint; cdecl;
begin
  if not Assigned(cipher) then Exit(0);

  Result := (8 * gnutls_cipher_get_key_size (cipher^.cipher));

  if Assigned(bits) then bits^ := Result;
end;

(* error handling *)

function ERR_get_error (): culong; cdecl;
begin
  Result := -1 * last_error;
  last_error := 0;
end;

function ERR_error_string (e: culong; buf: PAnsiChar): PAnsiChar; cdecl;
begin
  Result := gnutls_strerror (-1 * cint(e));
end;

function SafeGetProcAddress(Lib : TlibHandle; const ProcName : AnsiString) : Pointer;
begin
  Result:= GetProcedureAddress(Lib, ProcName);
  if (Result = nil) then raise Exception.Create(EmptyStr);
end;

const
  libgnutls: array[0..2] of String = ('30', '28', '26');
var
  index: Integer;
  dlinfo: dl_info;
  gnutls: TLibHandle = NilHandle;
begin
  if (IsSSLloaded = False) then
  begin
    for index:= Low(libgnutls) to High(libgnutls) do
    begin
      gnutls:= LoadLibrary('libgnutls.so.' + libgnutls[index]);
      if gnutls <> NilHandle then Break;
    end;

    if (gnutls <> NilHandle) then
    try
      @gnutls_check_version:= SafeGetProcAddress(gnutls, 'gnutls_check_version');

      if (gnutls_check_version('3.0.0') = nil) then raise Exception.Create(EmptyStr);

      @gnutls_global_init:= SafeGetProcAddress(gnutls, 'gnutls_global_init');

      @gnutls_init:= SafeGetProcAddress(gnutls, 'gnutls_init');
      @gnutls_deinit:= SafeGetProcAddress(gnutls, 'gnutls_deinit');

      @gnutls_priority_set_direct:= SafeGetProcAddress(gnutls, 'gnutls_priority_set_direct');
      @gnutls_credentials_set:= SafeGetProcAddress(gnutls, 'gnutls_credentials_set');
      @gnutls_certificate_set_x509_trust_file:= SafeGetProcAddress(gnutls, 'gnutls_certificate_set_x509_trust_file');
      @gnutls_certificate_set_x509_key_file:= SafeGetProcAddress(gnutls, 'gnutls_certificate_set_x509_key_file');

      @gnutls_certificate_allocate_credentials:= SafeGetProcAddress(gnutls, 'gnutls_certificate_allocate_credentials');
      @gnutls_certificate_free_credentials:= SafeGetProcAddress(gnutls, 'gnutls_certificate_free_credentials');

      @gnutls_transport_set_ptr:= SafeGetProcAddress(gnutls, 'gnutls_transport_set_ptr');
      @gnutls_record_check_pending:= SafeGetProcAddress(gnutls, 'gnutls_record_check_pending');

      @gnutls_handshake:= SafeGetProcAddress(gnutls, 'gnutls_handshake');
      @gnutls_bye:= SafeGetProcAddress(gnutls, 'gnutls_bye');

      @gnutls_record_send:= SafeGetProcAddress(gnutls, 'gnutls_record_send');
      @gnutls_record_recv:= SafeGetProcAddress(gnutls, 'gnutls_record_recv');

      @gnutls_protocol_get_version:= SafeGetProcAddress(gnutls, 'gnutls_protocol_get_version');
      @gnutls_cipher_get:= SafeGetProcAddress(gnutls, 'gnutls_cipher_get');
      @gnutls_kx_get:= SafeGetProcAddress(gnutls, 'gnutls_kx_get');
      @gnutls_mac_get:= SafeGetProcAddress(gnutls, 'gnutls_mac_get');
      @gnutls_compression_get:= SafeGetProcAddress(gnutls, 'gnutls_compression_get');
      @gnutls_certificate_type_get:= SafeGetProcAddress(gnutls, 'gnutls_certificate_type_get');
      @gnutls_cipher_suite_get_name:= SafeGetProcAddress(gnutls, 'gnutls_cipher_suite_get_name');
      @gnutls_cipher_get_key_size:= SafeGetProcAddress(gnutls, 'gnutls_cipher_get_key_size');

      @gnutls_strerror:= SafeGetProcAddress(gnutls, 'gnutls_strerror');


      FillChar(dlinfo, SizeOf(dlinfo), 0);
      if dladdr(@dlinfo, @dlinfo) <> 0 then
      begin
        DLLSSLName:= dlinfo.dli_fname;
        DLLUtilName:= DLLSSLName;

        if InitSSLInterface then
          SSLImplementation:= TSSLOpenSSL;
      end;
    except
      FreeLibrary(gnutls);
    end;
  end;

end.
