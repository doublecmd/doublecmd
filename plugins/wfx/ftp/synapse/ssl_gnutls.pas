{==============================================================================|
| Project : Ararat Synapse                                       | 001.000.000 |
|==============================================================================|
| Content: SSL support by GnuTLS                                               |
|==============================================================================|
| Copyright (C) 2013-2023 Alexander Koblov <alexx2000@mail.ru>                 |
| All rights reserved.                                                         |
|                                                                              |
| Redistribution and use in source and binary forms, with or without           |
| modification, are permitted provided that the following conditions are met:  |
|                                                                              |
| Redistributions of source code must retain the above copyright notice, this  |
| list of conditions and the following disclaimer.                             |
|                                                                              |
| Redistributions in binary form must reproduce the above copyright notice,    |
| this list of conditions and the following disclaimer in the documentation    |
| and/or other materials provided with the distribution.                       |
|                                                                              |
| Neither the name of Lukas Gebauer nor the names of its contributors may      |
| be used to endorse or promote products derived from this software without    |
| specific prior written permission.                                           |
|                                                                              |
| THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"  |
| AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE    |
| IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE   |
| ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR  |
| ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL       |
| DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR   |
| SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER   |
| CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT           |
| LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY    |
| OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  |
| DAMAGE.                                                                      |
|==============================================================================|
| The Initial Developer of the Original Code is Lukas Gebauer (Czech Republic).|
| Portions created by Lukas Gebauer are Copyright (C) 2005-2023.               |
| Portions created by Petr Fejfar are Copyright (C) 2011-2012.                 |
| All Rights Reserved.                                                         |
|==============================================================================}

{:@abstract(SSL plugin for GnuTLS)

Compatibility with GnuTLS versions:
3.0.0+

GnuTLS libraries are loaded dynamicly - you not need GnuTLS librares even you
compile your application with this unit. SSL just not working when you not have
GnuTLS libraries.
}

{$MODE DELPHI}

{$IFDEF UNICODE}
  {$WARN IMPLICIT_STRING_CAST OFF}
  {$WARN IMPLICIT_STRING_CAST_LOSS OFF}
{$ENDIF}

unit ssl_gnutls;

interface

uses
  SysUtils, Classes,
  blcksock, synsock, synautil,
  ssl_gnutls_lib;

type
  {:@abstract(class implementing GnuTLS SSL plugin.)
   Instance of this class will be created for each @link(TTCPBlockSocket).
   You not need to create instance of this class, all is done by Synapse itself!}

  { TSSLGnuTLS }

  TSSLGnuTLS = class(TCustomSSL)
  private
    FShutdown: Integer;
    FDatum: gnutls_datum_t;
    FSession: gnutls_session_t;
    FPriorities: array[Byte] of AnsiChar;
    FCredentials: gnutls_certificate_credentials_t;
  protected
    function Init: Boolean;
    function DeInit: Boolean;
    function Prepare: Boolean;
    function SSLCheck: Boolean;
  public
    {:See @inherited}
    constructor Create(const Value: TTCPBlockSocket); override;
    destructor Destroy; override;
    {:See @inherited}
    function LibVersion: String; override;
    {:See @inherited}
    function LibName: String; override;
    {:See @inherited}
    function Connect: boolean; override;
    {:See @inherited}
    function Shutdown: boolean; override;
    {:See @inherited}
    function BiShutdown: boolean; override;
    {:See @inherited}
    function SendBuffer(Buffer: TMemory; Len: Integer): Integer; override;
    {:See @inherited}
    function RecvBuffer(Buffer: TMemory; Len: Integer): Integer; override;
    {:See @inherited}
    function WaitingData: Integer; override;
    {:See @inherited}
    function GetSSLVersion: string; override;
    {:See @inherited}
    function GetCipherName: string; override;
    {:See @inherited}
    function GetCipherBits: integer; override;
    {:See @inherited}
    function GetCipherAlgBits: integer; override;
  end;

implementation

{==============================================================================}

constructor TSSLGnuTLS.Create(const Value: TTCPBlockSocket);
begin
  inherited Create(Value);
end;

destructor TSSLGnuTLS.Destroy;
begin
  DeInit;
  inherited Destroy;
end;

function TSSLGnuTLS.LibVersion: String;
begin
  Result := 'GnuTLS ' + gnutls_check_version('3.0.0');
end;

function TSSLGnuTLS.LibName: String;
begin
  Result := 'ssl_gnutls';
end;

function TSSLGnuTLS.Init: Boolean;
begin
  Result := False;
  FLastError := 0;
  FLastErrorDesc := EmptyStr;

  case FSSLType of
    LT_SSLv3:
      FPriorities := 'NONE:+VERS-SSL3.0:+CIPHER-ALL:+COMP-ALL:+RSA:+DHE-RSA:+DHE-DSS:+MAC-ALL';
    LT_TLSv1:
      FPriorities := 'NONE:+VERS-TLS1.0:+CIPHER-ALL:+COMP-ALL:+RSA:+DHE-RSA:+DHE-DSS:+MAC-ALL';
    LT_TLSv1_1:
      FPriorities := 'NONE:+VERS-TLS1.1:+CIPHER-ALL:+COMP-ALL:+RSA:+DHE-RSA:+DHE-DSS:+MAC-ALL';
    LT_TLSv1_2:
      FPriorities := 'NONE:+VERS-TLS1.2:+CIPHER-ALL:+COMP-ALL:+RSA:+DHE-RSA:+DHE-DSS:+MAC-ALL';
    LT_TLSv1_3:
      FPriorities := 'NONE:+VERS-TLS1.3:+CIPHER-ALL:+COMP-ALL:+RSA:+DHE-RSA:+DHE-DSS:+MAC-ALL';
    LT_all:
      FPriorities := 'NONE:+VERS-TLS-ALL:+CIPHER-ALL:+COMP-ALL:+RSA:+DHE-RSA:+DHE-DSS:+MAC-ALL';
  else
    Exit;
  end;

  FLastError := gnutls_certificate_allocate_credentials(FCredentials);
  if not SSLCheck then Exit;

  FLastError := gnutls_init(@FSession, GNUTLS_CLIENT);
  if not SSLCheck then Exit;

  FLastError := gnutls_priority_set_direct(FSession, FPriorities, nil);
  if not SSLCheck then Exit;

  FLastError := gnutls_credentials_set(FSession, GNUTLS_CRD_CERTIFICATE, FCredentials);
  if not SSLCheck then Exit;

  if Length(FCertificateFile) > 0 then
  begin
    gnutls_certificate_set_x509_trust_file(FCredentials,
                                           PAnsiChar(FCertificateFile),
                                           GNUTLS_X509_FMT_PEM);
  end;
  if Length(FPrivateKeyFile) > 0  then
  begin
    gnutls_certificate_set_x509_key_file(FCredentials,
                                         PAnsiChar(FCertificateFile),
                                         PAnsiChar(FPrivateKeyFile),
                                         GNUTLS_X509_FMT_PEM);
  end;
  Result := True;
end;

function TSSLGnuTLS.DeInit: Boolean;
begin
  Result := True;

  if Assigned(FSessionNew) then
  begin
    gnutls_free(FDatum.data);
    FSessionNew := nil;
    FDatum.data := nil;
    FDatum.size := 0
  end;

  if Assigned(FCredentials) then
  begin
    gnutls_certificate_free_credentials(FCredentials);
    FCredentials := nil;
  end;

  if Assigned(FSession) then
  begin
    gnutls_deinit(FSession);
    FSession := nil
  end;

  FSSLEnabled := False;
end;

function TSSLGnuTLS.Prepare: Boolean;
begin
  DeInit;
  if Init then
    Result := True
  else begin
    DeInit;
    Result := False;
  end;
end;

function TSSLGnuTLS.SSLCheck: Boolean;
var
  P : PAnsiChar;
begin
  if FLastError = GNUTLS_E_SUCCESS then
  begin
    Result := True;
    FLastErrorDesc := EmptyStr;
  end
  else begin
    Result := False;
    P := gnutls_strerror(FLastError);
    FLastErrorDesc := StrPas(P);
  end;
end;

function TSSLGnuTLS.Connect: boolean;
var
  B: Boolean;
begin
  Result := False;
  if FSocket.Socket = INVALID_SOCKET then
    Exit;
  if Prepare then
  begin
    gnutls_transport_set_ptr(FSession, gnutls_transport_ptr_t(FSocket.Socket));
    // Reuse session
    if Assigned(FSessionOld) then begin
      gnutls_session_set_data(FSession, gnutls_datum_ptr_t(FSessionOld)^.data, gnutls_datum_ptr_t(FSessionOld)^.size);
    end;
    // do blocking call of SSL_Connect
    if FSocket.ConnectionTimeout <= 0 then
    begin
      repeat
        FLastError := gnutls_handshake(FSession);
      until (FLastError <> GNUTLS_E_AGAIN) and (FLastError <> GNUTLS_E_INTERRUPTED);
    end
    // do non-blocking call of SSL_Connect
    else begin
      B := FSocket.NonBlockMode;
      FSocket.NonBlockMode := True;
      repeat
        FLastError := gnutls_handshake(FSession);
      until (FLastError <> GNUTLS_E_AGAIN) and (FLastError <> GNUTLS_E_INTERRUPTED);
      FSocket.NonBlockMode := B;
    end;
    if SSLCheck then
    begin
      if (FSessionOld = nil) then
      begin
        if (gnutls_session_get_data2(FSession, @FDatum) = GNUTLS_E_SUCCESS) then
        begin
          FSessionNew := @FDatum;
        end;
      end;
      FSSLEnabled := True;
      FShutdown := 0;
      Result := True;
    end;
  end;
end;

function TSSLGnuTLS.Shutdown: boolean;
begin
  Result := BiShutdown;
end;

function TSSLGnuTLS.BiShutdown: boolean;
begin
  if (FShutdown > 0) then
    gnutls_bye(FSession, GNUTLS_SHUT_WR)
  else begin
    gnutls_bye(FSession, GNUTLS_SHUT_RDWR);
  end;
  Inc(FShutdown);

  DeInit;
  Result := True;
end;

function TSSLGnuTLS.SendBuffer(Buffer: TMemory; Len: Integer): Integer;
begin
  FLastError := 0;
  FLastErrorDesc := EmptyStr;

  repeat
    Result := gnutls_record_send(FSession, Buffer , Len);
  until (Result <> GNUTLS_E_AGAIN) and (Result <> GNUTLS_E_INTERRUPTED);

  if (Result < 0) then
  begin
    FLastError := Result;
    Result := 0;
  end;
end;

function TSSLGnuTLS.RecvBuffer(Buffer: TMemory; Len: Integer): Integer;
begin
  FLastError := 0;
  FLastErrorDesc := EmptyStr;

  repeat
    Result := gnutls_record_recv(FSession, Buffer , Len);
  until (Result <> GNUTLS_E_AGAIN) and (Result <> GNUTLS_E_INTERRUPTED);

  if (Result < 0) then
  begin
    FLastError := Result;
    Result := 0;
  end;
end;

function TSSLGnuTLS.WaitingData: Integer;
begin
  Result := gnutls_record_check_pending(FSession);
end;

function TSSLGnuTLS.GetSSLVersion: string;
begin
  if (FSession = nil) then
    Result := EmptyStr
  else
    Result := gnutls_protocol_get_name(gnutls_protocol_get_version(FSession));
end;

function TSSLGnuTLS.GetCipherName: string;
var
  kx: gnutls_kx_algorithm_t;
  mac: gnutls_mac_algorithm_t;
  cipher: gnutls_cipher_algorithm_t;
begin
  if (FSession = nil) then
    Result := EmptyStr
  else begin
    kx := gnutls_kx_get(FSession);
    mac := gnutls_mac_get(FSession);
    cipher := gnutls_cipher_get(FSession);
    Result := gnutls_cipher_suite_get_name(kx, cipher, mac);
  end;
end;

function TSSLGnuTLS.GetCipherBits: integer;
begin
  Result := GetCipherAlgBits;
end;

function TSSLGnuTLS.GetCipherAlgBits: integer;
begin
  if (FSession = nil) then
    Result := 0
  else
    Result := (gnutls_cipher_get_key_size(gnutls_cipher_get(FSession)) * 8);
end;

{==============================================================================}

initialization
  if (SSLImplementation = TSSLNone) and InitSSLInterface then
    SSLImplementation := TSSLGnuTLS;

end.
