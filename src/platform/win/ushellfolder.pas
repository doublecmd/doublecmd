unit uShellFolder;

{$mode delphi}

interface

uses
  Classes, SysUtils, Windows, ShlObj, ActiveX, ComObj, uShlObjAdditional;

const
  FOLDERID_AccountPictures: TGUID = '{008ca0b1-55b4-4c56-b8a8-4de4b299d3be}';
  FOLDERID_ApplicationShortcuts: TGUID = '{A3918781-E5F2-4890-B3D9-A7E54332328C}';
  FOLDERID_CameraRoll: TGUID = '{AB5FB87B-7CE2-4F83-915D-550846C9537B}';
  FOLDERID_Contacts: TGUID = '{56784854-C6CB-462b-8169-88E350ACB882}';
  FOLDERID_DeviceMetadataStore: TGUID = '{5CE4A5E9-E4EB-479D-B89F-130C02886155}';
  FOLDERID_Downloads: TGUID = '{374DE290-123F-4565-9164-39C4925E467B}';
  FOLDERID_GameTasks: TGUID = '{054FAE61-4DD8-4787-80B6-090220C4B700}';
  FOLDERID_ImplicitAppShortcuts: TGUID = '{BCB5256F-79F6-4CEE-B725-DC34E402FD46}';
  FOLDERID_Libraries: TGUID = '{1B3EA5DC-B587-4786-B4EF-BD1DC332AEAE}';
  FOLDERID_Links: TGUID = '{bfb9d5e0-c6a9-404c-b2b2-ae6db6af4968}';
  FOLDERID_LocalAppDataLow: TGUID = '{A520A1A4-1780-4FF6-BD18-167343C5AF16}';
  FOLDERID_OriginalImages: TGUID = '{2C36C0AA-5812-4b87-BFD0-4CD0DFB19B39}';
  FOLDERID_PhotoAlbums: TGUID = '{69D2CF90-FC33-4FB7-9A0C-EBB0F0FCB43C}';
  FOLDERID_Playlists: TGUID = '{DE92C1C7-837F-4F69-A3BB-86E631204A23}';
  FOLDERID_ProgramFilesX64: TGUID = '{6D809377-6AF0-444b-8957-A3773F02200E}';
  FOLDERID_ProgramFilesCommonX64: TGUID = '{6365D5A7-0F0D-45E5-87F6-0DA56B6A4F7D}';
  FOLDERID_Public: TGUID = '{DFDF76A2-C82A-4D63-906A-5644AC457385}';
  FOLDERID_PublicDownloads: TGUID = '{3D644C9B-1FB8-4f30-9B45-F670235F79C0}';
  FOLDERID_PublicGameTasks: TGUID = '{DEBF2536-E1A8-4c59-B6A2-414586476AEA}';
  FOLDERID_PublicLibraries: TGUID = '{48DAF80B-E6CF-4F4E-B800-0E69D84EE384}';
  FOLDERID_PublicRingtones: TGUID = '{E555AB60-153B-4D17-9F04-A5FE99FC15EC}';
  FOLDERID_PublicUserTiles: TGUID = '{0482af6c-08f1-4c34-8c90-e17ec98b1e17}';
  FOLDERID_QuickLaunch: TGUID = '{52a4f021-7b75-48a9-9f6b-4b87a210bc8f}';
  FOLDERID_Ringtones: TGUID = '{C870044B-F49E-4126-A9C3-B52A1FF411E8}';
  FOLDERID_RoamedTileImages: TGUID = '{AAA8D5A5-F1D6-4259-BAA8-78E7EF60835E}';
  FOLDERID_RoamingTiles: TGUID = '{00BCFC5A-ED94-4e48-96A1-3F6217F21990}';
  FOLDERID_SampleMusic: TGUID = '{B250C668-F57D-4EE1-A63C-290EE7D1AA1F}';
  FOLDERID_SamplePictures: TGUID = '{C4900540-2379-4C75-844B-64E6FAF8716B}';
  FOLDERID_SamplePlaylists: TGUID = '{15CA69B3-30EE-49C1-ACE1-6B5EC372AFB5}';
  FOLDERID_SampleVideos: TGUID = '{859EAD94-2E85-48AD-A71A-0969CB56A6CD}';
  FOLDERID_SavedGames: TGUID = '{4C5C32FF-BB9D-43b0-B5B4-2D72E54EAAA4}';
  FOLDERID_SavedPictures: TGUID = '{3B193882-D3AD-4eab-965A-69829D1FB59F}';
  FOLDERID_SavedSearches: TGUID = '{7d1d3a04-debb-4115-95cf-2f29da2920da}';
  FOLDERID_Screenshots: TGUID = '{b7bede81-df94-4682-a7d8-57a52620b86f}';
  FOLDERID_SearchHistory: TGUID = '{0D4C3DB6-03A3-462F-A0E6-08924C41B5D4}';
  FOLDERID_SearchTemplates: TGUID = '{7E636BFE-DFA9-4D5E-B456-D7B39851D8A9}';
  FOLDERID_SidebarDefaultParts: TGUID = '{7B396E54-9EC5-4300-BE0A-2482EBAE1A26}';
  FOLDERID_SidebarParts: TGUID = '{A75D362E-50FC-4fb7-AC2C-A8BEAA314493}';
  FOLDERID_SkyDrive: TGUID = '{A52BBA46-E9E1-435f-B3D9-28DAA648C0F6}';
  FOLDERID_SkyDriveCameraRoll: TGUID = '{767E6811-49CB-4273-87C2-20F355E1085B}';
  FOLDERID_SkyDriveDocuments: TGUID = '{24D89E24-2F19-4534-9DDE-6A6671FBB8FE}';
  FOLDERID_SkyDrivePictures: TGUID = '{339719B5-8C47-4894-94C2-D8F77ADD44A6}';
  FOLDERID_UserPinned: TGUID = '{9E3995AB-1F9C-4F13-B827-48B24B6C7174}';
  FOLDERID_UserProfiles: TGUID = '{0762D272-C50A-4BB0-A382-697DCD729B80}';
  FOLDERID_UserProgramFiles: TGUID = '{5CD7AEE2-2219-4A67-B85D-6C9CE15660CB}';
  FOLDERID_UserProgramFilesCommon: TGUID = '{BCBD3057-CA5C-4622-B42D-BC56DB0AE516}';

function GetKnownFolderPath(const rfid: TGUID; out APath: String): Boolean;

function GetDisplayName(AFolder: IShellFolder; PIDL: PItemIDList; Flags: DWORD): String;
function GetDetails(AFolder: IShellFolder2; PIDL: PItemIDList; const pscid: SHCOLUMNID): OleVariant;

implementation

uses
  ShellApi, LazUTF8, DCConvertEncoding;

function StrRetToString(PIDL: PItemIDList; StrRet: TStrRet): String;
var
  S: array[0..MAX_PATH] of WideChar;
begin
  if StrRetToBufW(@StrRet, PIDL, S, MAX_PATH) <> S_OK then
    Result:= EmptyStr
  else
    Result:= CeUtf16ToUtf8(UnicodeString(S));
end;

function GetDisplayName(AFolder: IShellFolder; PIDL: PItemIDList;
                        Flags: DWORD): String;
var
  StrRet: TStrRet;
begin
  Result:= EmptyStr;
  StrRet:= Default(TStrRet);
  if Succeeded(AFolder.GetDisplayNameOf(PIDL, Flags, StrRet)) then
    Result := StrRetToString(PIDL, StrRet);
  if (Length(Result) = 0) and (Flags <> SHGDN_NORMAL) then
    Result := GetDisplayName(AFolder, PIDL, SHGDN_NORMAL);
end;

function GetDetails(AFolder: IShellFolder2; PIDL: PItemIDList; const pscid: SHCOLUMNID): OleVariant;
var
  AValue: OleVariant;
begin
 if Succeeded(AFolder.GetDetailsEx(pidl, @pscid, @AValue)) then
   Result:= AValue
 else
   Result:= Unassigned;
end;

const
  KF_FLAG_DEFAULT = $00000000;

var
  SHGetKnownFolderPath: function(const rfid: TGUID; dwFlags: DWORD; hToken: HANDLE; out ppszPath: LPCWSTR): HRESULT; stdcall;

function GetKnownFolderPath(const rfid: TGUID; out APath: String): Boolean;
var
  ppszPath: LPCWSTR;
begin
  Result:= Succeeded(SHGetKnownFolderPath(rfid, KF_FLAG_DEFAULT, 0, ppszPath));
  if Result then APath:= UTF16ToUTF8(ppszPath);
  CoTaskMemFree(ppszPath);
end;

initialization
  if Win32MajorVersion > 5 then @SHGetKnownFolderPath:= GetProcAddress(GetModuleHandleW(shell32), 'SHGetKnownFolderPath');
end.

