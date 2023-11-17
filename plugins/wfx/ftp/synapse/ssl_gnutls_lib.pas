{==============================================================================|
| Project : Ararat Synapse                                       | 001.000.000 |
|==============================================================================|
| Content: SSL support by GnuTLS                                               |
|==============================================================================|
| Copyright (C) 2013-2023 Alexander Koblov <alexx2000@mail.ru>                 |
|                                                                              |
| The GnuTLS is free software; you can redistribute it and/or                  |
| modify it under the terms of the GNU Lesser General Public License           |
| as published by the Free Software Foundation; either version 2.1 of          |
| the License, or (at your option) any later version.                          |
|                                                                              |
| This library is distributed in the hope that it will be useful, but          |
| WITHOUT ANY WARRANTY; without even the implied warranty of                   |
| MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            |
| Lesser General Public License for more details.                              |
|                                                                              |
| You should have received a copy of the GNU Lesser General Public License     |
| along with this program. If not, see <https://www.gnu.org/licenses/>         |
|==============================================================================}

unit ssl_gnutls_lib;

{$mode delphi}
{$packrecords c}

interface

uses
  CTypes;

const
  GNUTLS_E_SUCCESS = 0;
  GNUTLS_E_AGAIN = -28;
  GNUTLS_E_INTERRUPTED = -52;

type
  gnutls_protocol_t =
  (
    GNUTLS_SSL3 = 1,
    GNUTLS_TLS1_0,
    GNUTLS_TLS1_1,
    GNUTLS_TLS1_2,
    GNUTLS_TLS1_3,
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

  gnutls_init_flags_t =
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
    GNUTLS_X509_FMT_DER = 0,
    GNUTLS_X509_FMT_PEM = 1
  );

  gnutls_close_request_t =
  (
    GNUTLS_SHUT_RDWR = 0,
    GNUTLS_SHUT_WR = 1
  );

type
  gnutls_datum_t = record
    data: pcuchar;
    size: cuint;
  end;
  gnutls_datum_ptr_t = ^gnutls_datum_t;

type
  gnutls_session_st = record end;
  gnutls_session_t = ^gnutls_session_st;
  gnutls_transport_ptr_t = type UIntPtr;
  gnutls_session_ptr_t = ^gnutls_session_t;
  gnutls_certificate_credentials_st = record end;
  gnutls_certificate_credentials_t = ^gnutls_certificate_credentials_st;

var
  gnutls_global_init: function(): cint; cdecl;

  gnutls_init: function(session: gnutls_session_ptr_t; flags: gnutls_init_flags_t): cint; cdecl;
  gnutls_deinit: procedure(session: gnutls_session_t); cdecl;

  gnutls_priority_set_direct: function(session: gnutls_session_t; const priorities: PAnsiChar; const err_pos: PPAnsiChar): cint; cdecl;
  gnutls_credentials_set: function(session: gnutls_session_t; cred_type: gnutls_credentials_type_t; cred: Pointer): cint; cdecl;
  gnutls_certificate_set_x509_trust_file: function(res: gnutls_certificate_credentials_t; const CAFILE: PAnsiChar; crt_type: gnutls_x509_crt_fmt_t): cint; cdecl;
  gnutls_certificate_set_x509_key_file: function(res: gnutls_certificate_credentials_t; const CERTFILE: PAnsiChar; const KEYFILE: PAnsiChar; crt_type: gnutls_x509_crt_fmt_t): cint; cdecl;

  gnutls_certificate_allocate_credentials: function(out res: gnutls_certificate_credentials_t): cint; cdecl;
  gnutls_certificate_free_credentials: procedure(sc: gnutls_certificate_credentials_t); cdecl;

  gnutls_free: procedure(ptr: Pointer); cdecl;
  gnutls_session_get_data2: function(session: gnutls_session_t; data: gnutls_datum_ptr_t): cint; cdecl;
  gnutls_session_set_data: function(session: gnutls_session_t; session_data: Pointer; session_data_size: csize_t): cint; cdecl;

  gnutls_transport_set_ptr: procedure(session: gnutls_session_t; ptr: gnutls_transport_ptr_t); cdecl;
  gnutls_record_check_pending: function(session: gnutls_session_t): csize_t; cdecl;

  gnutls_handshake: function(session: gnutls_session_t): cint; cdecl;
  gnutls_bye: function(session: gnutls_session_t; how: gnutls_close_request_t): cint; cdecl;

  gnutls_record_send: function(session: gnutls_session_t; const data: Pointer; sizeofdata: csize_t): PtrInt; cdecl;
  gnutls_record_recv: function(session: gnutls_session_t; data: Pointer; sizeofdata: csize_t): PtrInt; cdecl;

  gnutls_protocol_get_name: function(version: gnutls_protocol_t): PAnsiChar; cdecl;
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

function InitSSLInterface: Boolean;

implementation

uses
  SysUtils, DynLibs;

function SafeGetProcAddress(Lib : TlibHandle; const ProcName : AnsiString) : Pointer;
begin
  Result:= GetProcedureAddress(Lib, ProcName);
  if (Result = nil) then raise Exception.Create(EmptyStr);
end;

function InitSSLInterface: Boolean;
const
  libgnutls: array[0..2] of String = ('30', '28', '26');
var
  index: Integer;
  gnutls: TLibHandle;
begin
  for index:= Low(libgnutls) to High(libgnutls) do
  begin
    gnutls:= LoadLibrary('libgnutls.so.' + libgnutls[index]);
    if gnutls <> NilHandle then Break;
  end;
  Result:= (gnutls <> NilHandle);
  if Result then
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

    @gnutls_free:= SafeGetProcAddress(gnutls, 'gnutls_free');
    @gnutls_session_get_data2:= SafeGetProcAddress(gnutls, 'gnutls_session_get_data2');
    @gnutls_session_set_data:= SafeGetProcAddress(gnutls, 'gnutls_session_set_data');

    @gnutls_transport_set_ptr:= SafeGetProcAddress(gnutls, 'gnutls_transport_set_ptr');
    @gnutls_record_check_pending:= SafeGetProcAddress(gnutls, 'gnutls_record_check_pending');

    @gnutls_handshake:= SafeGetProcAddress(gnutls, 'gnutls_handshake');
    @gnutls_bye:= SafeGetProcAddress(gnutls, 'gnutls_bye');

    @gnutls_record_send:= SafeGetProcAddress(gnutls, 'gnutls_record_send');
    @gnutls_record_recv:= SafeGetProcAddress(gnutls, 'gnutls_record_recv');

    @gnutls_protocol_get_name:= SafeGetProcAddress(gnutls, 'gnutls_protocol_get_name');
    @gnutls_protocol_get_version:= SafeGetProcAddress(gnutls, 'gnutls_protocol_get_version');
    @gnutls_cipher_get:= SafeGetProcAddress(gnutls, 'gnutls_cipher_get');
    @gnutls_kx_get:= SafeGetProcAddress(gnutls, 'gnutls_kx_get');
    @gnutls_mac_get:= SafeGetProcAddress(gnutls, 'gnutls_mac_get');
    @gnutls_compression_get:= SafeGetProcAddress(gnutls, 'gnutls_compression_get');
    @gnutls_certificate_type_get:= SafeGetProcAddress(gnutls, 'gnutls_certificate_type_get');
    @gnutls_cipher_suite_get_name:= SafeGetProcAddress(gnutls, 'gnutls_cipher_suite_get_name');
    @gnutls_cipher_get_key_size:= SafeGetProcAddress(gnutls, 'gnutls_cipher_get_key_size');

    @gnutls_strerror:= SafeGetProcAddress(gnutls, 'gnutls_strerror');

    if (gnutls_global_init() <> GNUTLS_E_SUCCESS) then
      raise Exception.Create(EmptyStr);
  except
    Result:= False;
    FreeLibrary(gnutls);
  end;
end;

end.
