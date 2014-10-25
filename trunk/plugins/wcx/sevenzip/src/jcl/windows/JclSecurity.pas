{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JclSecurity.pas.                                                            }
{                                                                                                  }
{ The Initial Developer of the Original Code is Marcel van Brakel.                                 }
{ Portions created by Marcel van Brakel are Copyright (C) Marcel van Brakel. All Rights Reserved.  }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Marcel van Brakel                                                                              }
{   Peter Friese                                                                                   }
{   Robert Marquardt (marquardt)                                                                   }
{   John C Molyneux                                                                                }
{   Robert Rossmair (rrossmair)                                                                    }
{   Matthias Thoma (mthoma)                                                                        }
{   Petr Vones (pvones)                                                                            }
{   Christoph Lindeman                                                                             }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Various NT security related routines to perform commen asks such as enabling and disabling       }
{ privileges.                                                                                      }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclSecurity;

{$I jcl.inc}
{$I windowsonly.inc}

{$HPPEMIT '#define TTokenInformationClass TOKEN_INFORMATION_CLASS'}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNITSCOPE}
  Winapi.Windows, System.SysUtils,
  {$ELSE ~HAS_UNITSCOPE}
  Windows, SysUtils,
  {$ENDIF ~HAS_UNITSCOPE}
  JclBase;

type
  EJclSecurityError = class(EJclError);

// Access Control
function CreateNullDacl(out Sa: TSecurityAttributes; const Inheritable: Boolean): PSecurityAttributes;
function CreateInheritable(out Sa: TSecurityAttributes): PSecurityAttributes;

// Privileges
function IsGroupMember(RelativeGroupID: DWORD): Boolean;
function IsAdministrator: Boolean;
function IsUser: Boolean;
function IsGuest: Boolean;
function IsPowerUser: Boolean;
function IsAccountOperator: Boolean;
function IsSystemOperator: Boolean;
function IsPrintOperator: Boolean;
function IsBackupOperator: Boolean;
function IsReplicator: Boolean;
function IsRASServer: Boolean;
function IsPreWin2000CompAccess: Boolean;
function IsRemoteDesktopUser: Boolean;
function IsNetworkConfigurationOperator: Boolean;
function IsIncomingForestTrustBuilder: Boolean;
function IsMonitoringUser: Boolean;
function IsLoggingUser: Boolean;
function IsAuthorizationAccess: Boolean;
function IsTSLicenseServer: Boolean;

function EnableProcessPrivilege(const Enable: Boolean; const Privilege: string): Boolean;
function EnableThreadPrivilege(const Enable: Boolean; const Privilege: string): Boolean;
function IsPrivilegeEnabled(const Privilege: string): Boolean;

function GetPrivilegeDisplayName(const PrivilegeName: string): string;
function SetUserObjectFullAccess(hUserObject: THandle): Boolean;
function GetUserObjectName(hUserObject: THandle): string;

// Account Information
procedure LookupAccountBySid(Sid: PSID; out Name, Domain: AnsiString; Silent: Boolean = False); overload;
procedure LookupAccountBySid(Sid: PSID; out Name, Domain: WideString; Silent: Boolean = False); overload;
procedure QueryTokenInformation(Token: THandle; InformationClass: TTokenInformationClass; var Buffer: Pointer);
procedure FreeTokenInformation(var Buffer: Pointer);
function GetInteractiveUserName: string;

// SID utilities
function SIDToString(ASID: PSID): string;
procedure StringToSID(const SIDString: String; SID: PSID; cbSID: DWORD);

// Computer Information
function GetComputerSID(SID: PSID; cbSID: DWORD): Boolean;

// Windows Vista/Server 2008 UAC (User Account Control)
function IsUACEnabled: Boolean;
function IsElevated: Boolean;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\windows';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF HAS_UNITSCOPE}
  System.Classes,
  {$ELSE ~HAS_UNITSCOPE}
  Classes,
  {$ENDIF ~HAS_UNITSCOPE}
  {$IFDEF BORLAND}
  AccCtrl,
  {$ENDIF BORLAND}
  JclRegistry, JclResources, JclStrings, JclSysInfo, JclWin32;

//=== Access Control =========================================================

function CreateNullDacl(out Sa: TSecurityAttributes; const Inheritable: Boolean): PSecurityAttributes;
begin
  if IsWinNT then
  begin
    Sa.lpSecurityDescriptor := AllocMem(SizeOf(TSecurityDescriptor));
    try
      Sa.nLength := SizeOf(Sa);
      Sa.bInheritHandle := Inheritable;
      Win32Check(InitializeSecurityDescriptor(Sa.lpSecurityDescriptor, SECURITY_DESCRIPTOR_REVISION));
      Win32Check(SetSecurityDescriptorDacl(Sa.lpSecurityDescriptor, True, nil, False));
      Result := @Sa;
    except
      FreeMem(Sa.lpSecurityDescriptor);
      Sa.lpSecurityDescriptor := nil;
      raise;
    end;
  end
  else
  begin
    Sa.lpSecurityDescriptor := nil;
    Result := nil;
  end;
end;

function CreateInheritable(out Sa: TSecurityAttributes): PSecurityAttributes;
begin
  Sa.nLength := SizeOf(Sa);
  Sa.lpSecurityDescriptor := nil;
  Sa.bInheritHandle := True;
  if IsWinNT then
    Result := @Sa
  else
    Result := nil;
end;

//=== Privileges =============================================================

function IsGroupMember(RelativeGroupID: DWORD): Boolean;
var
  psidAdmin: Pointer;
  Token: THandle;
  Count: DWORD;
  TokenInfo: PTokenGroups;
  HaveToken: Boolean;
  I: Integer;
const
  SE_GROUP_USE_FOR_DENY_ONLY = $00000010;
begin
  Result := not IsWinNT;
  if Result then // Win9x and ME don't have user groups
    Exit;
  psidAdmin := nil;
  TokenInfo := nil;
  HaveToken := False;
  try
    Token := 0;
    HaveToken := OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True, Token);
    if (not HaveToken) and (GetLastError = ERROR_NO_TOKEN) then
      HaveToken := OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, Token);
    if HaveToken then
    begin
      {$IFDEF FPC}
      Win32Check(AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2,
        SECURITY_BUILTIN_DOMAIN_RID, RelativeGroupID, 0, 0, 0, 0, 0, 0,
        psidAdmin));
      if GetTokenInformation(Token, TokenGroups, nil, 0, @Count) or
       (GetLastError <> ERROR_INSUFFICIENT_BUFFER) then
         RaiseLastOSError;
      TokenInfo := PTokenGroups(AllocMem(Count));
      Win32Check(GetTokenInformation(Token, TokenGroups, TokenInfo, Count, @Count));
      {$ELSE FPC}
      Win32Check(AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2,
        SECURITY_BUILTIN_DOMAIN_RID, RelativeGroupID, 0, 0, 0, 0, 0, 0,
        psidAdmin));
      if GetTokenInformation(Token, TokenGroups, nil, 0, Count) or
       (GetLastError <> ERROR_INSUFFICIENT_BUFFER) then
         RaiseLastOSError;
      TokenInfo := PTokenGroups(AllocMem(Count));
      Win32Check(GetTokenInformation(Token, TokenGroups, TokenInfo, Count, Count));
      {$ENDIF FPC}
      for I := 0 to TokenInfo^.GroupCount - 1 do
      begin
        {$RANGECHECKS OFF} // Groups is an array [0..0] of TSIDAndAttributes, ignore ERangeError
        Result := EqualSid(psidAdmin, TokenInfo^.Groups[I].Sid);
        if Result then
        begin
          //consider denied ACE with Administrator SID
          Result := TokenInfo^.Groups[I].Attributes and SE_GROUP_USE_FOR_DENY_ONLY
              <> SE_GROUP_USE_FOR_DENY_ONLY;
          Break;
        end;
        {$IFDEF RANGECHECKS_ON}
        {$RANGECHECKS ON}
        {$ENDIF RANGECHECKS_ON}
      end;
    end;
  finally
    if TokenInfo <> nil then
      FreeMem(TokenInfo);
    if HaveToken then
      CloseHandle(Token);
    if psidAdmin <> nil then
      FreeSid(psidAdmin);
  end;
end;

function IsAdministrator: Boolean;
begin
  Result := IsGroupMember(DOMAIN_ALIAS_RID_ADMINS);
end;

function IsUser: Boolean;
begin
  Result := IsGroupMember(DOMAIN_ALIAS_RID_USERS);
end;

function IsGuest: Boolean;
begin
  Result := IsGroupMember(DOMAIN_ALIAS_RID_GUESTS);
end;

function IsPowerUser: Boolean;
begin
  Result := IsGroupMember(DOMAIN_ALIAS_RID_POWER_USERS);
end;

function IsAccountOperator: Boolean;
begin
  Result := IsGroupMember(DOMAIN_ALIAS_RID_ACCOUNT_OPS);
end;

function IsSystemOperator: Boolean;
begin
  Result := IsGroupMember(DOMAIN_ALIAS_RID_SYSTEM_OPS);
end;

function IsPrintOperator: Boolean;
begin
  Result := IsGroupMember(DOMAIN_ALIAS_RID_PRINT_OPS);
end;

function IsBackupOperator: Boolean;
begin
  Result := IsGroupMember(DOMAIN_ALIAS_RID_BACKUP_OPS);
end;

function IsReplicator: Boolean;
begin
  Result := IsGroupMember(DOMAIN_ALIAS_RID_REPLICATOR);
end;

function IsRASServer: Boolean;
begin
  Result := IsGroupMember(DOMAIN_ALIAS_RID_RAS_SERVERS);
end;

function IsPreWin2000CompAccess: Boolean;
begin
  Result := IsGroupMember(DOMAIN_ALIAS_RID_PREW2KCOMPACCESS);
end;

function IsRemoteDesktopUser: Boolean;
begin
  Result := IsGroupMember(DOMAIN_ALIAS_RID_REMOTE_DESKTOP_USERS);
end;

function IsNetworkConfigurationOperator: Boolean;
begin
  Result := IsGroupMember(DOMAIN_ALIAS_RID_NETWORK_CONFIGURATION_OPS);
end;

function IsIncomingForestTrustBuilder: Boolean;
begin
  Result := IsGroupMember(DOMAIN_ALIAS_RID_INCOMING_FOREST_TRUST_BUILDERS);
end;

function IsMonitoringUser: Boolean;
begin
  Result := IsGroupMember(DOMAIN_ALIAS_RID_MONITORING_USERS);
end;

function IsLoggingUser: Boolean;
begin
  Result := IsGroupMember(DOMAIN_ALIAS_RID_LOGGING_USERS);
end;

function IsAuthorizationAccess: Boolean;
begin
  Result := IsGroupMember(DOMAIN_ALIAS_RID_AUTHORIZATIONACCESS);
end;

function IsTSLicenseServer: Boolean;
begin
  Result := IsGroupMember(DOMAIN_ALIAS_RID_TS_LICENSE_SERVERS);
end;

function EnableProcessPrivilege(const Enable: Boolean; const Privilege: string): Boolean;
const
  PrivAttrs: array [Boolean] of DWORD = (0, SE_PRIVILEGE_ENABLED);
var
  Token: THandle;
  TokenPriv: TTokenPrivileges;
begin
  Result := not IsWinNT;
  if Result then  // if Win9x, then function return True
    Exit;
  Token := 0;
  if OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES, Token) then
  begin
    TokenPriv.PrivilegeCount := 1;
    LookupPrivilegeValue(nil, PChar(Privilege), TokenPriv.Privileges[0].Luid);
    TokenPriv.Privileges[0].Attributes := PrivAttrs[Enable];
    JclWin32.AdjustTokenPrivileges(Token, False, TokenPriv, SizeOf(TokenPriv), nil, nil);
    Result := GetLastError = ERROR_SUCCESS;
    CloseHandle(Token);
  end;
end;

function EnableThreadPrivilege(const Enable: Boolean; const Privilege: string): Boolean;
const
  PrivAttrs: array [Boolean] of DWORD = (0, SE_PRIVILEGE_ENABLED);
var
  Token: THandle;
  TokenPriv: TTokenPrivileges;
  HaveToken: Boolean;
begin
  Result := not IsWinNT;
  if Result then  // Win9x/ME
    Exit;
  Token := 0;
  HaveToken := OpenThreadToken(GetCurrentThread, TOKEN_ADJUST_PRIVILEGES,
    False, Token);
  if (not HaveToken) and (GetLastError = ERROR_NO_TOKEN) then
    HaveToken := OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES, Token);
  if HaveToken then
  begin
    TokenPriv.PrivilegeCount := 1;
    LookupPrivilegeValue(nil, PChar(Privilege), TokenPriv.Privileges[0].Luid);
    TokenPriv.Privileges[0].Attributes := PrivAttrs[Enable];
    JclWin32.AdjustTokenPrivileges(Token, False, TokenPriv, SizeOf(TokenPriv), nil, nil);
    Result := GetLastError = ERROR_SUCCESS;
    CloseHandle(Token);
  end;
end;

function IsPrivilegeEnabled(const Privilege: string): Boolean;
var
  Token: THandle;
  TokenPriv: TPrivilegeSet;
  Res: LongBool;
  HaveToken: Boolean;
begin
  Result := not IsWinNT;
  if Result then  // Win9x/ME
    Exit;
  Token := 0;
  HaveToken := OpenThreadToken(GetCurrentThread, TOKEN_QUERY, False, Token);
  if (not HaveToken) and (GetLastError = ERROR_NO_TOKEN) then
    HaveToken := OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, Token);
  if HaveToken then
  begin
    TokenPriv.PrivilegeCount := 1;
    TokenPriv.Control := 0;
    LookupPrivilegeValue(nil, PChar(Privilege), TokenPriv.Privilege[0].Luid);
    Res := False;
    Result := PrivilegeCheck(Token, TokenPriv, Res) and Res;
    CloseHandle(Token);
  end;
end;

function GetPrivilegeDisplayName(const PrivilegeName: string): string;
var
  Count: DWORD;
  LangID: DWORD;
begin
  if IsWinNT then
  begin
    Count  := 0;
    LangID := LANG_USER_DEFAULT;

    // have the the API function determine the required string length
    Result := '';
    if not LookupPrivilegeDisplayName(nil, PChar(PrivilegeName), PChar(Result), Count, LangID) then
      Count := 256;
    SetLength(Result, Count + 1);

    if LookupPrivilegeDisplayName(nil, PChar(PrivilegeName), PChar(Result), Count, LangID) then
      StrResetLength(Result)
    else
      Result := '';
  end
  else
    Result := '';  // Win9x/ME
end;

function SetUserObjectFullAccess(hUserObject: THandle): Boolean;
var
  Sd: PSecurity_Descriptor;
  Si: Security_Information;
begin
  Result := not IsWinNT;
  if Result then  // Win9x/ME
    Exit;
  { TODO : Check the success of called functions }
  Sd := PSecurity_Descriptor(LocalAlloc(LPTR, SECURITY_DESCRIPTOR_MIN_LENGTH));
  InitializeSecurityDescriptor(Sd, SECURITY_DESCRIPTOR_REVISION);
  SetSecurityDescriptorDacl(Sd, True, nil, False);

  Si := DACL_SECURITY_INFORMATION;
  Result := SetUserObjectSecurity(hUserObject, Si, Sd);

  LocalFree(HLOCAL(Sd));
end;

function GetUserObjectName(hUserObject: THandle): string;
var
  Count: DWORD;
begin
  if IsWinNT then
  begin
    // have the API function determine the required string length
    Count := 0;
    Result := '';
    GetUserObjectInformation(hUserObject, UOI_NAME, PChar(Result), 0, Count);
    SetLength(Result, Count + 1);

    if GetUserObjectInformation(hUserObject, UOI_NAME, PChar(Result), Count, Count) then
      StrResetLength(Result)
    else
      Result := '';
  end
  else
    Result := '';
end;

//=== Account Information ====================================================

procedure LookupAccountBySid(Sid: PSID; out Name, Domain: AnsiString; Silent: Boolean);
var
  NameSize, DomainSize: DWORD;
  Use: SID_NAME_USE;
  Success: Boolean;
begin
  if IsWinNT then
  begin
    NameSize := 0;
    DomainSize := 0;
    Use := SidTypeUnknown;
    LookupAccountSidA(nil, Sid, nil, NameSize, nil, DomainSize, Use);
    if NameSize > 0 then
      SetLength(Name, NameSize - 1);
    if DomainSize > 0 then
      SetLength(Domain, DomainSize - 1);
    Success := LookupAccountSidA(nil, Sid, PAnsiChar(Name), NameSize, PAnsiChar(Domain), DomainSize, Use);
    if Silent and not Success then
    begin
      Name := AnsiString(SIDToString(Sid));
      Domain := '';
    end
    else
      Win32Check(Success);
  end
  else
  begin             // if Win9x, then function return ''
    Name := '';
    Domain := '';
  end;
end;

procedure LookupAccountBySid(Sid: PSID; out Name, Domain: WideString; Silent: Boolean);
var
  NameSize, DomainSize: DWORD;
  Use: SID_NAME_USE;
  Success: Boolean;
begin
  if IsWinNT then
  begin
    NameSize := 0;
    DomainSize := 0;
    Use := SidTypeUnknown;
    LookupAccountSidW(nil, Sid, nil, NameSize, nil, DomainSize, Use);
    if NameSize > 0 then
      SetLength(Name, NameSize - 1);
    if DomainSize > 0 then
      SetLength(Domain, DomainSize - 1);
    Success := LookupAccountSidW(nil, Sid, PWideChar(Name), NameSize, PWideChar(Domain), DomainSize, Use);
    if Silent and not Success then
    begin
      Name := WideString(SIDToString(Sid));
      Domain := '';
    end
    else
      Win32Check(Success);
  end
  else
  begin
    Name := '';
    Domain := '';
  end;
end;

procedure QueryTokenInformation(Token: THandle;
  InformationClass: TTokenInformationClass; var Buffer: Pointer);
var
  Ret: BOOL;
  Length, LastError: DWORD;
begin
  Buffer := nil;
  if not IsWinNT then  // Win9x/ME
    Exit;
  Length := 0;
  {$IFDEF FPC}
  Ret := GetTokenInformation(Token, InformationClass, Buffer, Length, @Length);
  {$ELSE ~FPC}
  Ret := GetTokenInformation(Token, InformationClass, Buffer, Length, Length);
  {$ENDIF ~FPC}
  if (not Ret) and (GetLastError = ERROR_INSUFFICIENT_BUFFER) then
  begin
    GetMem(Buffer, Length);
    {$IFDEF FPC}
    Ret := GetTokenInformation(Token, InformationClass, Buffer, Length, @Length);
    {$ELSE ~FPC}
    Ret := GetTokenInformation(Token, InformationClass, Buffer, Length, Length);
    {$ENDIF ~FPC}
    if not Ret then
    begin
      LastError := GetLastError;
      FreeTokenInformation(Buffer);
      SetLastError(LastError);
    end;
  end;
end;

procedure FreeTokenInformation(var Buffer: Pointer);
begin
  if Buffer <> nil then
    FreeMem(Buffer);
  Buffer := nil;
end;

function GetInteractiveUserName: string;
var
  Handle: THandle;
  Token: THandle;
  User: PTokenUser;
  {$IFDEF SUPPORTS_UNICODE}
  Name, Domain: WideString;
  {$ELSE ~SUPPORTS_UNICODE}
  Name, Domain: AnsiString;
  {$ENDIF ~SUPPORTS_UNICODE}
begin
  Result := '';
  if not IsWinNT then  // if Win9x, then function return ''
    Exit;
  Handle := GetShellProcessHandle;
  try
    Token := 0;
    Win32Check(OpenProcessToken(Handle, TOKEN_QUERY, Token));
    try
      User := nil;
      QueryTokenInformation(Token, TokenUser, Pointer(User));
      try
        LookupAccountBySid(User.User.Sid, Name, Domain);
        Result := Domain + '\' + Name;
      finally
        FreeMem(User);
      end;
    finally
      CloseHandle(Token);
    end;
  finally
    CloseHandle(Handle);
  end;
end;

//=== SID utilities ==========================================================

function SIDToString(ASID: PSID): string;
var
  SidIdAuthority: PSIDIdentifierAuthority;
  SubAuthorities, SidRev, SidSize: DWORD;
  Counter: Integer;
begin
  SidRev := SID_REVISION;

  // Validate the binary SID.
  if not IsValidSid(ASid) then
    Raise EJclSecurityError.CreateRes(@RsInvalidSID);

  // Get the identifier authority value from the SID.
  SidIdAuthority := GetSidIdentifierAuthority(ASid);

  // Get the number of subauthorities in the SID.
  SubAuthorities := GetSidSubAuthorityCount(ASid)^;

  //Compute the buffer length.
  // S-SID_REVISION- + IdentifierAuthority- + subauthorities- + NULL
  SidSize := (15 + 12 + (12 * SubAuthorities) + 1) * SizeOf(CHAR);

  SetLength(Result, SidSize+1);

  // Add 'S' prefix and revision number to the string.
  Result := Format('S-%u-',[SidRev]);

  // Add SID identifier authority to the string.
  if (SidIdAuthority^.Value[0] <> 0) or (SidIdAuthority^.Value[1] <> 0) then
    Result := Result + AnsiLowerCase(Format('0x%2.2x%2.2x%2.2x%2.2x%2.2x%2.2x',
        [USHORT(SidIdAuthority^.Value[0]),
         USHORT(SidIdAuthority^.Value[1]),
         USHORT(SidIdAuthority^.Value[2]),
         USHORT(SidIdAuthority^.Value[3]),
         USHORT(SidIdAuthority^.Value[4]),
         USHORT(SidIdAuthority^.Value[5])]))
  else
    Result := Result + Format('%u',
        [ULONG(SidIdAuthority^.Value[5])+
         ULONG(SidIdAuthority^.Value[4] shl 8)+
         ULONG(SidIdAuthority^.Value[3] shl 16)+
         ULONG(SidIdAuthority^.Value[2] shl 24)]);

  // Add SID subauthorities to the string.
  for Counter := 0 to SubAuthorities-1 do
    Result := Result + Format('-%u',[GetSidSubAuthority(ASid, Counter)^]);
end;

procedure StringToSID(const SIDString: String; SID: PSID; cbSID: DWORD);
var
  {$IFDEF FPC} ASID: PSID; {$ELSE} ASID : ^_SID; {$ENDIF}
  CurrentPos, TempPos: Integer;
  AuthorityValue, RequiredSize: DWORD;
  Authority: string;
begin
  if (Length (SIDString) <= 3) or (SIDString [1] <> 'S') or (SIDString [2] <> '-') then
    raise EJclSecurityError.CreateRes(@RsInvalidSID);

  RequiredSize := SizeOf(_SID) - SizeOf(DWORD); // _SID.Revision + _SID.SubAuthorityCount + _SID.IdentifierAuthority
  if cbSID < RequiredSize then
    raise EJclSecurityError.CreateRes(@RsSIDBufferTooSmall);

  ASID := SID; // typecast from opaque structure

  CurrentPos := StrFind('-', SIDString, 3);
  if CurrentPos <= 0 then
    raise EJclSecurityError.CreateRes(@RsInvalidSID);
  ASID^.Revision := StrToInt(Copy(SIDString, 3, CurrentPos - 3));

  Inc(CurrentPos);
  TempPos := StrFind('-', SIDString, CurrentPos);
  if TempPos = 0 then
    Authority := Copy(SIDString, CurrentPos, Length(SIDString) - CurrentPos + 1)
  else
    Authority := Copy(SIDString, CurrentPos, TempPos - CurrentPos);

  if Length(Authority) < 1 then
    raise EJclSecurityError.CreateRes(@RsInvalidSID);
  if (Length(Authority) = 14) and (Authority[1] = '0') and (Authority[2] = 'x') then
  begin
    ASID^.IdentifierAuthority.Value[0] := StrToInt(HexPrefix + Authority[3] + Authority[4]);
    ASID^.IdentifierAuthority.Value[1] := StrToInt(HexPrefix + Authority[5] + Authority[6]);
    ASID^.IdentifierAuthority.Value[2] := StrToInt(HexPrefix + Authority[7] + Authority[8]);
    ASID^.IdentifierAuthority.Value[3] := StrToInt(HexPrefix + Authority[9] + Authority[10]);
    ASID^.IdentifierAuthority.Value[4] := StrToInt(HexPrefix + Authority[11] + Authority[12]);
    ASID^.IdentifierAuthority.Value[5] := StrToInt(HexPrefix + Authority[13] + Authority[14]);
  end
  else
  begin
    ASID^.IdentifierAuthority.Value[0] := 0;
    ASID^.IdentifierAuthority.Value[1] := 0;
    AuthorityValue := StrToInt(Authority);
    ASID^.IdentifierAuthority.Value[2] := (AuthorityValue and $FF000000) shr 24;
    ASID^.IdentifierAuthority.Value[3] := (AuthorityValue and $00FF0000) shr 16;
    ASID^.IdentifierAuthority.Value[4] := (AuthorityValue and $0000FF00) shr 8;
    ASID^.IdentifierAuthority.Value[5] :=  AuthorityValue and $000000FF;
  end;

  CurrentPos := TempPos + 1;
  ASID^.SubAuthorityCount := 0;

  while CurrentPos > 1 do
  begin
    TempPos := StrFind('-', SIDString, CurrentPos);

    Inc(RequiredSize, SizeOf(DWORD)); // _SID.SubAuthority[x]
    if cbSID < RequiredSize then
      raise EJclSecurityError.CreateRes(@RsSIDBufferTooSmall);

    if TempPos = 0 then
      Authority := Copy(SIDString, CurrentPos, Length(SIDString) - CurrentPos + 1)
    else
      Authority := Copy(SIDString, CurrentPos, TempPos - CurrentPos);

    {$RANGECHECKS OFF}
    ASID^.SubAuthority[ASID^.SubAuthorityCount] := StrToInt64(Authority);
    {$IFDEF RANGECHECKS_ON}
    {$RANGECHECKS ON}
    {$ENDIF RANGECHECKS_ON}
    Inc(ASID^.SubAuthorityCount);

    CurrentPos := TempPos + 1;
  end;
end;

//=== Computer Information ===================================================

function LsaNTCheck(NTResult: Cardinal) : Cardinal;
var
  WinError: Cardinal;
begin
  Result := NTResult;
  if ($C0000000 and Cardinal(NTResult)) = $C0000000 then
  begin
    WinError := LsaNtStatusToWinError(NTResult);
    if WinError <> ERROR_SUCCESS then
      raise EJclSecurityError.CreateResFmt(@RsLsaError, [NTResult, SysErrorMessage(WinError)]);
  end;
end;

function GetComputerSID(SID: PSID; cbSID: DWORD): Boolean;
var
  ObjectAttributes: TLsaObjectAttributes;
  PolicyHandle: TLsaHandle;
  Info: PPolicyAccountDomainInfo;
begin
  if IsWinNT then
  begin
    ZeroMemory(@ObjectAttributes,SizeOf(ObjectAttributes));

    {$IFDEF FPC}
    PolicyHandle := 0;
    {$ENDIF FPC}
    LsaNTCheck(LsaOpenPolicy(nil, // Use local system
      ObjectAttributes, //Object attributes.
      POLICY_VIEW_LOCAL_INFORMATION, // We're just looking
      PolicyHandle)); //Receives the policy handle.
    try
      Info := nil;
      LsaNTCheck(LsaQueryInformationPolicy(PolicyHandle, PolicyAccountDomainInformation,
        Pointer(Info)));
      try
        Result := CopySid(cbSID,SID,Info^.DomainSid);
      finally
        LsaFreeMemory(Info);
      end;
    finally
      LsaClose(PolicyHandle);
    end;
  end
  else
    Result := False; // Win9x
end;

//=== Windows Vista/Server 2008 UAC (User Account Control) ===================

function IsUACEnabled: Boolean;
begin
  Result := JclCheckWinVersion(6, 0) and
    RegReadBoolDef(HKLM, '\Software\Microsoft\Windows\CurrentVersion\Policies\System', 'EnableLUA', False);
end;

// source: Vista elevator from the Code Project
function IsElevated: Boolean;
const
  TokenElevation = TTokenInformationClass(20);
type
  TOKEN_ELEVATION = record
    TokenIsElevated: DWORD;
  end;
var
  TokenHandle: THandle;
  ResultLength: Cardinal;
  ATokenElevation: TOKEN_ELEVATION;
begin
  if JclCheckWinVersion(6, 0) then
  begin
    TokenHandle := 0;
    if OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, TokenHandle) then
    begin
      try
        ResultLength := 0;
        if GetTokenInformation(TokenHandle, TokenElevation, @ATokenElevation, SizeOf(ATokenElevation), ResultLength) then
          Result := ATokenElevation.TokenIsElevated <> 0
        else
          Result := False;
      finally
        CloseHandle(TokenHandle);
      end;
    end
    else
      Result := False;
  end
  else
    Result := IsAdministrator;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
