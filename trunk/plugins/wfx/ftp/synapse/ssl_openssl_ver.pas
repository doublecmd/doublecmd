{==============================================================================|
| Project : Ararat Synapse                                       | 003.004.001 |
|==============================================================================|
| Content: SSL support by OpenSSL                                              |
|==============================================================================|
| Copyright (c)1999-2005, Lukas Gebauer                                        |
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
| Portions created by Lukas Gebauer are Copyright (c)2002-2005.                |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|   Alexander Koblov                                                           |
|   Ales Katona (Try to load all library versions until you find or run out)   |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

unit ssl_openssl_ver;

{$mode delphi}

interface

implementation

uses
  blcksock, ssl_openssl, ssl_openssl_lib;

const
  LibSSLName = 'libssl';
  LibUtilName = 'libcrypto';

  { ADD NEW ONES WHEN THEY APPEAR!
    Always make .so/dylib first, then versions, in descending order!
    Add "." .before the version, first is always just "" }
  LibVersions: array[1..13] of String = ('', '.1.1', '.1.0.2', '.1.0.1','.1.0.0',
                                         '.0.9.8', '.0.9.7', '.0.9.6', '.0.9.5',
                                         '.0.9.4', '.0.9.3', '.0.9.2', '.0.9.1');

function GetLibraryName(const Value: String; Index: Integer): String;
begin
{$IFDEF DARWIN}
  Result := Value + LibVersions[Index] + '.dylib';
{$ELSE}
  Result := Value + '.so' + LibVersions[Index];
{$ENDIF}
end;

var
  Index: Integer;
begin
  if not IsSSLloaded then
  begin
    for Index := Low(LibVersions) to High(LibVersions) do
    begin
      DLLSSLName := GetLibraryName(LibSSLName, Index);
      DLLUtilName := GetLibraryName(LibUtilName, Index);

      if InitSSLInterface then
      begin
        SSLImplementation:= TSSLOpenSSL;
        Break;
      end;
    end;
  end;
end.
