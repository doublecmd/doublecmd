Unit WMPLib_1_0_TLB;

//  Imported WMPLib on 29.08.2021 15:27:47 from C:\WINDOWS\system32\wmp.dll

{$mode delphi}{$H+}

interface

Uses
  Windows,ActiveX,Classes,Variants,stdole2,EventSink;
Const
  WMPLibMajorVersion = 1;
  WMPLibMinorVersion = 0;
  WMPLibLCID = 0;
  LIBID_WMPLib : TGUID = '{6BF52A50-394A-11D3-B153-00C04F79FAA6}';

  IID_IWMPEvents : TGUID = '{19A6627B-DA9E-47C1-BB23-00B5E668236A}';
  IID_IWMPEvents2 : TGUID = '{1E7601FA-47EA-4107-9EA9-9004ED9684FF}';
  IID_IWMPSyncDevice : TGUID = '{82A2986C-0293-4FD0-B279-B21B86C058BE}';
  IID_IWMPEvents3 : TGUID = '{1F504270-A66B-4223-8E96-26A06C63D69F}';
  IID_IWMPCdromRip : TGUID = '{56E2294F-69ED-4629-A869-AEA72C0DCC2C}';
  IID_IWMPCdromBurn : TGUID = '{BD94DBEB-417F-4928-AA06-087D56ED9B59}';
  IID_IWMPPlaylist : TGUID = '{D5F0F4F1-130C-11D3-B14E-00C04F79FAA6}';
  IID_IWMPMedia : TGUID = '{94D55E95-3FAC-11D3-B155-00C04F79FAA6}';
  IID_IWMPLibrary : TGUID = '{3DF47861-7DF1-4C1F-A81B-4C26F0F7A7C6}';
  IID_IWMPMediaCollection : TGUID = '{8363BC22-B4B4-4B19-989D-1CD765749DD1}';
  IID_IWMPStringCollection : TGUID = '{4A976298-8C0D-11D3-B389-00C04F68574B}';
  IID_IWMPEvents4 : TGUID = '{26DABCFA-306B-404D-9A6F-630A8405048D}';
  IID__WMPOCXEvents : TGUID = '{6BF52A51-394A-11D3-B153-00C04F79FAA6}';
  CLASS_WindowsMediaPlayer : TGUID = '{6BF52A52-394A-11D3-B153-00C04F79FAA6}';
  IID_IWMPPlayer4 : TGUID = '{6C497D62-8919-413C-82DB-E935FB3EC584}';
  IID_IWMPCore3 : TGUID = '{7587C667-628F-499F-88E7-6A6F4E888464}';
  IID_IWMPCore2 : TGUID = '{BC17E5B7-7561-4C18-BB90-17D485775659}';
  IID_IWMPCore : TGUID = '{D84CCA99-CCE2-11D2-9ECC-0000F8085981}';
  IID_IWMPControls : TGUID = '{74C09E02-F828-11D2-A74B-00A0C905F36E}';
  IID_IWMPSettings : TGUID = '{9104D1AB-80C9-4FED-ABF0-2E6417A6DF14}';
  IID_IWMPPlaylistCollection : TGUID = '{10A13217-23A7-439B-B1C0-D847C79B7774}';
  IID_IWMPPlaylistArray : TGUID = '{679409C0-99F7-11D3-9FB7-00105AA620BB}';
  IID_IWMPNetwork : TGUID = '{EC21B779-EDEF-462D-BBA4-AD9DDE2B29A7}';
  IID_IWMPCdromCollection : TGUID = '{EE4C8FE2-34B2-11D3-A3BF-006097C9B344}';
  IID_IWMPCdrom : TGUID = '{CFAB6E98-8730-11D3-B388-00C04F68574B}';
  IID_IWMPClosedCaption : TGUID = '{4F2DF574-C588-11D3-9ED0-00C04FB6E937}';
  IID_IWMPError : TGUID = '{A12DCF7D-14AB-4C1B-A8CD-63909F06025B}';
  IID_IWMPErrorItem : TGUID = '{3614C646-3B3B-4DE7-A81E-930E3F2127B3}';
  IID_IWMPDVD : TGUID = '{8DA61686-4668-4A5C-AE5D-803193293DBE}';
  IID_IWMPPlayerApplication : TGUID = '{40897764-CEAB-47BE-AD4A-8E28537F9BBF}';
  IID_IWMPPlayer3 : TGUID = '{54062B68-052A-4C25-A39F-8B63346511D4}';
  IID_IWMPPlayer2 : TGUID = '{0E6B01D1-D407-4C85-BF5F-1C01F6150280}';
  IID_IWMPPlayer : TGUID = '{6BF52A4F-394A-11D3-B153-00C04F79FAA6}';
  IID_IWMPErrorItem2 : TGUID = '{F75CCEC0-C67C-475C-931E-8719870BEE7D}';
  IID_IWMPControls2 : TGUID = '{6F030D25-0890-480F-9775-1F7E40AB5B8E}';
  IID_IWMPMedia2 : TGUID = '{AB7C88BB-143E-4EA4-ACC3-E4350B2106C3}';
  IID_IWMPMedia3 : TGUID = '{F118EFC7-F03A-4FB4-99C9-1C02A5C1065B}';
  IID_IWMPMetadataPicture : TGUID = '{5C29BBE0-F87D-4C45-AA28-A70F0230FFA9}';
  IID_IWMPMetadataText : TGUID = '{769A72DB-13D2-45E2-9C48-53CA9D5B7450}';
  IID_IWMPSettings2 : TGUID = '{FDA937A4-EECE-4DA5-A0B6-39BF89ADE2C2}';
  IID_IWMPControls3 : TGUID = '{A1D1110E-D545-476A-9A78-AC3E4CB1E6BD}';
  IID_IWMPClosedCaption2 : TGUID = '{350BA78B-6BC8-4113-A5F5-312056934EB6}';
  IID_IWMPMediaCollection2 : TGUID = '{8BA957F5-FD8C-4791-B82D-F840401EE474}';
  IID_IWMPQuery : TGUID = '{A00918F3-A6B0-4BFB-9189-FD834C7BC5A5}';
  IID_IWMPStringCollection2 : TGUID = '{46AD648D-53F1-4A74-92E2-2A1B68D63FD4}';
  IID_IWMPPlayerServices : TGUID = '{1D01FBDB-ADE2-4C8D-9842-C190B95C3306}';
  IID_IWMPPlayerServices2 : TGUID = '{1BB1592F-F040-418A-9F71-17C7512B4D70}';
  IID_IWMPRemoteMediaServices : TGUID = '{CBB92747-741F-44FE-AB5B-F1A48F3B2A59}';
  IID_IWMPSyncServices : TGUID = '{8B5050FF-E0A4-4808-B3A8-893A9E1ED894}';
  IID_IWMPLibraryServices : TGUID = '{39C2F8D5-1CF2-4D5E-AE09-D73492CF9EAA}';
  IID_IWMPLibrarySharingServices : TGUID = '{82CBA86B-9F04-474B-A365-D6DD1466E541}';
  IID_IWMPLibrary2 : TGUID = '{DD578A4E-79B1-426C-BF8F-3ADD9072500B}';
  IID_IWMPFolderMonitorServices : TGUID = '{788C8743-E57F-439D-A468-5BC77F2E59C6}';
  IID_IWMPSyncDevice2 : TGUID = '{88AFB4B2-140A-44D2-91E6-4543DA467CD1}';
  IID_IWMPSyncDevice3 : TGUID = '{B22C85F9-263C-4372-A0DA-B518DB9B4098}';
  IID_IWMPPlaylistCtrl : TGUID = '{5F9CFD92-8CAD-11D3-9A7E-00C04F8EFB70}';
  IID_IAppDispatch : TGUID = '{E41C88DD-2364-4FF7-A0F5-CA9859AF783F}';
  IID_IWMPSafeBrowser : TGUID = '{EF870383-83AB-4EA9-BE48-56FA4251AF10}';
  IID_IWMPObjectExtendedProps : TGUID = '{21D077C1-4BAA-11D3-BD45-00C04F6EA5AE}';
  IID_IWMPLayoutSubView : TGUID = '{72F486B1-0D43-11D3-BD3F-00C04F6EA5AE}';
  IID_IWMPLayoutView : TGUID = '{172E905D-80D9-4C2F-B7CE-2CCB771787A2}';
  IID_IWMPEventObject : TGUID = '{5AF0BEC1-46AA-11D3-BD45-00C04F6EA5AE}';
  IID_IWMPTheme : TGUID = '{6FCAE13D-E492-4584-9C21-D2C052A2A33A}';
  IID_IWMPLayoutSettingsDispatch : TGUID = '{B2C2D18E-97AF-4B6A-A56B-2FFFF470FB81}';
  IID_IWMPWindow : TGUID = '{43D5AE92-4332-477C-8883-E0B3B063C5D2}';
  IID_IWMPBrandDispatch : TGUID = '{98BB02D4-ED74-43CC-AD6A-45888F2E0DCC}';
  IID_IWMPNowPlayingHelperDispatch : TGUID = '{504F112E-77CC-4E3C-A073-5371B31D9B36}';
  IID_IWMPNowDoingDispatch : TGUID = '{2A2E0DA3-19FA-4F82-BE18-CD7D7A3B977F}';
  IID_IWMPHoverPreviewDispatch : TGUID = '{946B023E-044C-4473-8018-74954F09DC7E}';
  IID_IWMPButtonCtrlEvents : TGUID = '{BB17FFF7-1692-4555-918A-6AF7BFACEDD2}';
  CLASS_WMPButtonCtrl : TGUID = '{87291B51-0C8E-11D3-BB2A-00A0C93CA73A}';
  IID_IWMPButtonCtrl : TGUID = '{87291B50-0C8E-11D3-BB2A-00A0C93CA73A}';
  CLASS_WMPListBoxCtrl : TGUID = '{FC1880CF-83B9-43A7-A066-C44CE8C82583}';
  IID_IWMPListBoxCtrl : TGUID = '{FC1880CE-83B9-43A7-A066-C44CE8C82583}';
  IID_IWMPListBoxItem : TGUID = '{D255DFB8-C22A-42CF-B8B7-F15D7BCF65D6}';
  IID_IWMPPlaylistCtrlColumn : TGUID = '{63D9D30F-AE4C-4678-8CA8-5720F4FE4419}';
  IID_IWMPSliderCtrlEvents : TGUID = '{CDAC14D2-8BE4-11D3-BB48-00A0C93CA73A}';
  CLASS_WMPSliderCtrl : TGUID = '{F2BF2C90-405F-11D3-BB39-00A0C93CA73A}';
  IID_IWMPSliderCtrl : TGUID = '{F2BF2C8F-405F-11D3-BB39-00A0C93CA73A}';
  IID_IWMPVideoCtrlEvents : TGUID = '{A85C0477-714C-4A06-B9F6-7C8CA38B45DC}';
  CLASS_WMPVideoCtrl : TGUID = '{61CECF11-FC3A-11D2-A1CD-005004602752}';
  IID_IWMPVideoCtrl : TGUID = '{61CECF10-FC3A-11D2-A1CD-005004602752}';
  CLASS_WMPEffects : TGUID = '{47DEA830-D619-4154-B8D8-6B74845D6A2D}';
  IID_IWMPEffectsCtrl : TGUID = '{A9EFAB80-0A60-4C3F-BBD1-4558DD2A9769}';
  CLASS_WMPEqualizerSettingsCtrl : TGUID = '{93EB32F5-87B1-45AD-ACC6-0F2483DB83BB}';
  IID_IWMPEqualizerSettingsCtrl : TGUID = '{2BD3716F-A914-49FB-8655-996D5F495498}';
  CLASS_WMPVideoSettingsCtrl : TGUID = '{AE7BFAFE-DCC8-4A73-92C8-CC300CA88859}';
  IID_IWMPVideoSettingsCtrl : TGUID = '{07EC23DA-EF73-4BDE-A40F-F269E0B7AFD6}';
  CLASS_WMPLibraryTreeCtrl : TGUID = '{D9DE732A-AEE9-4503-9D11-5605589977A8}';
  IID_IWMPLibraryTreeCtrl : TGUID = '{B738FCAE-F089-45DF-AED6-034B9E7DB632}';
  CLASS_WMPEditCtrl : TGUID = '{6342FCED-25EA-4033-BDDB-D049A14382D3}';
  IID_IWMPEditCtrl : TGUID = '{70E1217C-C617-4CFD-BD8A-69CA2043E70B}';
  CLASS_WMPSkinList : TGUID = '{A8A55FAC-82EA-4BD7-BD7B-11586A4D99E4}';
  IID_IWMPSkinList : TGUID = '{8CEA03A2-D0C5-4E97-9C38-A676A639A51D}';
  IID_IWMPPluginUIHost : TGUID = '{5D0AD945-289E-45C5-A9C6-F301F0152108}';
  CLASS_WMPMenuCtrl : TGUID = '{BAB3768B-8883-4AEC-9F9B-E14C947913EF}';
  IID_IWMPMenuCtrl : TGUID = '{158A7ADC-33DA-4039-A553-BDDBBE389F5C}';
  CLASS_WMPAutoMenuCtrl : TGUID = '{6B28F900-8D64-4B80-9963-CC52DDD1FBB4}';
  IID_IWMPAutoMenuCtrl : TGUID = '{1AD13E0B-4F3A-41DF-9BE2-F9E6FE0A7875}';
  CLASS_WMPRegionalButtonCtrl : TGUID = '{AE3B6831-25A9-11D3-BD41-00C04F6EA5AE}';
  IID_IWMPRegionalButtonCtrl : TGUID = '{58D507B1-2354-11D3-BD41-00C04F6EA5AE}';
  IID_IWMPRegionalButtonEvents : TGUID = '{50FC8D31-67AC-11D3-BD4C-00C04F6EA5AE}';
  CLASS_WMPRegionalButton : TGUID = '{09AEFF11-69EF-11D3-BD4D-00C04F6EA5AE}';
  IID_IWMPRegionalButton : TGUID = '{58D507B2-2354-11D3-BD41-00C04F6EA5AE}';
  IID_IWMPCustomSliderCtrlEvents : TGUID = '{95F45AA4-ED0A-11D2-BA67-0000F80855E6}';
  CLASS_WMPCustomSliderCtrl : TGUID = '{95F45AA3-ED0A-11D2-BA67-0000F80855E6}';
  IID_IWMPCustomSlider : TGUID = '{95F45AA2-ED0A-11D2-BA67-0000F80855E6}';
  CLASS_WMPTextCtrl : TGUID = '{DDDA102E-0E17-11D3-A2E2-00C04F79F88E}';
  IID_IWMPTextCtrl : TGUID = '{237DAC8E-0E32-11D3-A2E2-00C04F79F88E}';
  CLASS_WMPPlaylistCtrl : TGUID = '{5F9CFD93-8CAD-11D3-9A7E-00C04F8EFB70}';
  IID_ITaskCntrCtrl : TGUID = '{891EADB1-1C45-48B0-B704-49A888DA98C4}';
  IID__WMPCoreEvents : TGUID = '{D84CCA96-CCE2-11D2-9ECC-0000F8085981}';
  CLASS_WMPCore : TGUID = '{09428D37-E0B9-11D2-B147-00C04F79FAA6}';
  IID_IWMPGraphEventHandler : TGUID = '{6B550945-018F-11D3-B14A-00C04F79FAA6}';
  IID_IBattery : TGUID = '{F8578BFA-CD8F-4CE1-A684-5B7E85FCA7DC}';
  IID_IBatteryPreset : TGUID = '{40C6BDE7-9C90-49D4-AD20-BEF81A6C5F22}';
  IID_IBatteryRandomPreset : TGUID = '{F85E2D65-207D-48DB-84B1-915E1735DB17}';
  IID_IBatterySavedPreset : TGUID = '{876E7208-0172-4EBB-B08B-2E1D30DFE44C}';
  IID_IBarsEffect : TGUID = '{33E9291A-F6A9-11D2-9435-00A0C92A2F2D}';
  IID_IWMPExternal : TGUID = '{E2CC638C-FD2C-409B-A1EA-5DDB72DC8E84}';
  IID_IWMPExternalColors : TGUID = '{D10CCDFF-472D-498C-B5FE-3630E5405E0A}';
  IID_IWMPSubscriptionServiceLimited : TGUID = '{54DF358E-CF38-4010-99F1-F44B0E9000E5}';
  IID_IWMPSubscriptionServiceExternal : TGUID = '{2E922378-EE70-4CEB-BBAB-CE7CE4A04816}';
  IID_IWMPDownloadManager : TGUID = '{E15E9AD1-8F20-4CC4-9EC7-1A328CA86A0D}';
  IID_IWMPDownloadCollection : TGUID = '{0A319C7F-85F9-436C-B88E-82FD88000E1C}';
  IID_IWMPDownloadItem2 : TGUID = '{9FBB3336-6DA3-479D-B8FF-67D46E20A987}';
  IID_IWMPDownloadItem : TGUID = '{C9470E8E-3F6B-46A9-A0A9-452815C34297}';
  IID_IWMPSubscriptionServicePlayMedia : TGUID = '{5F0248C1-62B3-42D7-B927-029119E6AD14}';
  IID_IWMPDiscoExternal : TGUID = '{A915CEA2-72DF-41E1-A576-EF0BAE5E5169}';
  IID_IWMPCDDVDWizardExternal : TGUID = '{2D7EF888-1D3C-484A-A906-9F49D99BB344}';
  IID_IWMPBaseExternal : TGUID = '{F81B2A59-02BC-4003-8B2F-C124AF66FC66}';
  IID_IWMPOfflineExternal : TGUID = '{3148E685-B243-423D-8341-8480D6EFF674}';
  IID_IWMPDMRAVTransportService : TGUID = '{4E195DB1-9E29-47FC-9CE1-DE9937D32925}';
  IID_IWMPDMRConnectionManagerService : TGUID = '{FB61CD38-8DE7-4479-8B76-A8D097C20C70}';
  IID_IWMPDMRRenderingControlService : TGUID = '{FF4B1BDA-19F0-42CF-8DDA-19162950C543}';

//Enums

Type
  WMPPlaylistChangeEventType =LongWord;
Const
  wmplcUnknown = $0000000000000000;
  wmplcClear = $0000000000000001;
  wmplcInfoChange = $0000000000000002;
  wmplcMove = $0000000000000003;
  wmplcDelete = $0000000000000004;
  wmplcInsert = $0000000000000005;
  wmplcAppend = $0000000000000006;
  wmplcPrivate = $0000000000000007;
  wmplcNameChange = $0000000000000008;
  wmplcMorph = $0000000000000009;
  wmplcSort = $000000000000000A;
  wmplcLast = $000000000000000B;
Type
  WMPDeviceStatus =LongWord;
Const
  wmpdsUnknown = $0000000000000000;
  wmpdsPartnershipExists = $0000000000000001;
  wmpdsPartnershipDeclined = $0000000000000002;
  wmpdsPartnershipAnother = $0000000000000003;
  wmpdsManualDevice = $0000000000000004;
  wmpdsNewDevice = $0000000000000005;
  wmpdsLast = $0000000000000006;
Type
  WMPSyncState =LongWord;
Const
  wmpssUnknown = $0000000000000000;
  wmpssSynchronizing = $0000000000000001;
  wmpssStopped = $0000000000000002;
  wmpssEstimating = $0000000000000003;
  wmpssLast = $0000000000000004;
Type
  WMPRipState =LongWord;
Const
  wmprsUnknown = $0000000000000000;
  wmprsRipping = $0000000000000001;
  wmprsStopped = $0000000000000002;
Type
  WMPBurnFormat =LongWord;
Const
  wmpbfAudioCD = $0000000000000000;
  wmpbfDataCD = $0000000000000001;
Type
  WMPBurnState =LongWord;
Const
  wmpbsUnknown = $0000000000000000;
  wmpbsBusy = $0000000000000001;
  wmpbsReady = $0000000000000002;
  wmpbsWaitingForDisc = $0000000000000003;
  wmpbsRefreshStatusPending = $0000000000000004;
  wmpbsPreparingToBurn = $0000000000000005;
  wmpbsBurning = $0000000000000006;
  wmpbsStopped = $0000000000000007;
  wmpbsErasing = $0000000000000008;
  wmpbsDownloading = $0000000000000009;
Type
  WMPLibraryType =LongWord;
Const
  wmpltUnknown = $0000000000000000;
  wmpltAll = $0000000000000001;
  wmpltLocal = $0000000000000002;
  wmpltRemote = $0000000000000003;
  wmpltDisc = $0000000000000004;
  wmpltPortableDevice = $0000000000000005;
Type
  WMPFolderScanState =LongWord;
Const
  wmpfssUnknown = $0000000000000000;
  wmpfssScanning = $0000000000000001;
  wmpfssUpdating = $0000000000000002;
  wmpfssStopped = $0000000000000003;
Type
  WMPStringCollectionChangeEventType =LongWord;
Const
  wmpsccetUnknown = $0000000000000000;
  wmpsccetInsert = $0000000000000001;
  wmpsccetChange = $0000000000000002;
  wmpsccetDelete = $0000000000000003;
  wmpsccetClear = $0000000000000004;
  wmpsccetBeginUpdates = $0000000000000005;
  wmpsccetEndUpdates = $0000000000000006;
Type
  WMPOpenState =LongWord;
Const
  wmposUndefined = $0000000000000000;
  wmposPlaylistChanging = $0000000000000001;
  wmposPlaylistLocating = $0000000000000002;
  wmposPlaylistConnecting = $0000000000000003;
  wmposPlaylistLoading = $0000000000000004;
  wmposPlaylistOpening = $0000000000000005;
  wmposPlaylistOpenNoMedia = $0000000000000006;
  wmposPlaylistChanged = $0000000000000007;
  wmposMediaChanging = $0000000000000008;
  wmposMediaLocating = $0000000000000009;
  wmposMediaConnecting = $000000000000000A;
  wmposMediaLoading = $000000000000000B;
  wmposMediaOpening = $000000000000000C;
  wmposMediaOpen = $000000000000000D;
  wmposBeginCodecAcquisition = $000000000000000E;
  wmposEndCodecAcquisition = $000000000000000F;
  wmposBeginLicenseAcquisition = $0000000000000010;
  wmposEndLicenseAcquisition = $0000000000000011;
  wmposBeginIndividualization = $0000000000000012;
  wmposEndIndividualization = $0000000000000013;
  wmposMediaWaiting = $0000000000000014;
  wmposOpeningUnknownURL = $0000000000000015;
Type
  WMPPlayState =LongWord;
Const
  wmppsUndefined = $0000000000000000;
  wmppsStopped = $0000000000000001;
  wmppsPaused = $0000000000000002;
  wmppsPlaying = $0000000000000003;
  wmppsScanForward = $0000000000000004;
  wmppsScanReverse = $0000000000000005;
  wmppsBuffering = $0000000000000006;
  wmppsWaiting = $0000000000000007;
  wmppsMediaEnded = $0000000000000008;
  wmppsTransitioning = $0000000000000009;
  wmppsReady = $000000000000000A;
  wmppsReconnecting = $000000000000000B;
  wmppsLast = $000000000000000C;
Type
  WMPSubscriptionDownloadState =LongWord;
Const
  wmpsdlsDownloading = $0000000000000000;
  wmpsdlsPaused = $0000000000000001;
  wmpsdlsProcessing = $0000000000000002;
  wmpsdlsCompleted = $0000000000000003;
  wmpsdlsCancelled = $0000000000000004;
Type
  WMP_WRITENAMESEX_TYPE =LongWord;
Const
  WMP_WRITENAMES_TYPE_CD_BY_TOC = $0000000000000000;
  WMP_WRITENAMES_TYPE_CD_BY_CONTENT_ID = $0000000000000001;
  WMP_WRITENAMES_TYPE_CD_BY_MDQCD = $0000000000000002;
  WMP_WRITENAMES_TYPE_DVD_BY_DVDID = $0000000000000003;
//Forward declarations

Type
 IWMPEvents = interface;
 IWMPEvents2 = interface;
 IWMPSyncDevice = interface;
 IWMPEvents3 = interface;
 IWMPCdromRip = interface;
 IWMPCdromBurn = interface;
 IWMPPlaylist = interface;
 IWMPPlaylistDisp = dispinterface;
 IWMPMedia = interface;
 IWMPMediaDisp = dispinterface;
 IWMPLibrary = interface;
 IWMPMediaCollection = interface;
 IWMPMediaCollectionDisp = dispinterface;
 IWMPStringCollection = interface;
 IWMPStringCollectionDisp = dispinterface;
 IWMPEvents4 = interface;
 _WMPOCXEvents = dispinterface;
 IWMPPlayer4 = interface;
 IWMPPlayer4Disp = dispinterface;
 IWMPCore3 = interface;
 IWMPCore3Disp = dispinterface;
 IWMPCore2 = interface;
 IWMPCore2Disp = dispinterface;
 IWMPCore = interface;
 IWMPCoreDisp = dispinterface;
 IWMPControls = interface;
 IWMPControlsDisp = dispinterface;
 IWMPSettings = interface;
 IWMPSettingsDisp = dispinterface;
 IWMPPlaylistCollection = interface;
 IWMPPlaylistCollectionDisp = dispinterface;
 IWMPPlaylistArray = interface;
 IWMPPlaylistArrayDisp = dispinterface;
 IWMPNetwork = interface;
 IWMPNetworkDisp = dispinterface;
 IWMPCdromCollection = interface;
 IWMPCdromCollectionDisp = dispinterface;
 IWMPCdrom = interface;
 IWMPCdromDisp = dispinterface;
 IWMPClosedCaption = interface;
 IWMPClosedCaptionDisp = dispinterface;
 IWMPError = interface;
 IWMPErrorDisp = dispinterface;
 IWMPErrorItem = interface;
 IWMPErrorItemDisp = dispinterface;
 IWMPDVD = interface;
 IWMPDVDDisp = dispinterface;
 IWMPPlayerApplication = interface;
 IWMPPlayerApplicationDisp = dispinterface;
 IWMPPlayer3 = interface;
 IWMPPlayer3Disp = dispinterface;
 IWMPPlayer2 = interface;
 IWMPPlayer2Disp = dispinterface;
 IWMPPlayer = interface;
 IWMPPlayerDisp = dispinterface;
 IWMPErrorItem2 = interface;
 IWMPErrorItem2Disp = dispinterface;
 IWMPControls2 = interface;
 IWMPControls2Disp = dispinterface;
 IWMPMedia2 = interface;
 IWMPMedia2Disp = dispinterface;
 IWMPMedia3 = interface;
 IWMPMedia3Disp = dispinterface;
 IWMPMetadataPicture = interface;
 IWMPMetadataPictureDisp = dispinterface;
 IWMPMetadataText = interface;
 IWMPMetadataTextDisp = dispinterface;
 IWMPSettings2 = interface;
 IWMPSettings2Disp = dispinterface;
 IWMPControls3 = interface;
 IWMPControls3Disp = dispinterface;
 IWMPClosedCaption2 = interface;
 IWMPClosedCaption2Disp = dispinterface;
 IWMPMediaCollection2 = interface;
 IWMPMediaCollection2Disp = dispinterface;
 IWMPQuery = interface;
 IWMPQueryDisp = dispinterface;
 IWMPStringCollection2 = interface;
 IWMPStringCollection2Disp = dispinterface;
 IWMPPlayerServices = interface;
 IWMPPlayerServices2 = interface;
 IWMPRemoteMediaServices = interface;
 IWMPSyncServices = interface;
 IWMPLibraryServices = interface;
 IWMPLibrarySharingServices = interface;
 IWMPLibrary2 = interface;
 IWMPFolderMonitorServices = interface;
 IWMPSyncDevice2 = interface;
 IWMPSyncDevice3 = interface;
 IWMPPlaylistCtrl = interface;
 IWMPPlaylistCtrlDisp = dispinterface;
 IAppDispatch = interface;
 IAppDispatchDisp = dispinterface;
 IWMPSafeBrowser = interface;
 IWMPSafeBrowserDisp = dispinterface;
 IWMPObjectExtendedProps = interface;
 IWMPObjectExtendedPropsDisp = dispinterface;
 IWMPLayoutSubView = interface;
 IWMPLayoutSubViewDisp = dispinterface;
 IWMPLayoutView = interface;
 IWMPLayoutViewDisp = dispinterface;
 IWMPEventObject = interface;
 IWMPEventObjectDisp = dispinterface;
 IWMPTheme = interface;
 IWMPThemeDisp = dispinterface;
 IWMPLayoutSettingsDispatch = interface;
 IWMPLayoutSettingsDispatchDisp = dispinterface;
 IWMPWindow = interface;
 IWMPWindowDisp = dispinterface;
 IWMPBrandDispatch = interface;
 IWMPBrandDispatchDisp = dispinterface;
 IWMPNowPlayingHelperDispatch = interface;
 IWMPNowPlayingHelperDispatchDisp = dispinterface;
 IWMPNowDoingDispatch = interface;
 IWMPNowDoingDispatchDisp = dispinterface;
 IWMPHoverPreviewDispatch = interface;
 IWMPHoverPreviewDispatchDisp = dispinterface;
 IWMPButtonCtrlEvents = dispinterface;
 IWMPButtonCtrl = interface;
 IWMPButtonCtrlDisp = dispinterface;
 IWMPListBoxCtrl = interface;
 IWMPListBoxCtrlDisp = dispinterface;
 IWMPListBoxItem = interface;
 IWMPListBoxItemDisp = dispinterface;
 IWMPPlaylistCtrlColumn = interface;
 IWMPPlaylistCtrlColumnDisp = dispinterface;
 IWMPSliderCtrlEvents = dispinterface;
 IWMPSliderCtrl = interface;
 IWMPSliderCtrlDisp = dispinterface;
 IWMPVideoCtrlEvents = dispinterface;
 IWMPVideoCtrl = interface;
 IWMPVideoCtrlDisp = dispinterface;
 IWMPEffectsCtrl = interface;
 IWMPEffectsCtrlDisp = dispinterface;
 IWMPEqualizerSettingsCtrl = interface;
 IWMPEqualizerSettingsCtrlDisp = dispinterface;
 IWMPVideoSettingsCtrl = interface;
 IWMPVideoSettingsCtrlDisp = dispinterface;
 IWMPLibraryTreeCtrl = interface;
 IWMPLibraryTreeCtrlDisp = dispinterface;
 IWMPEditCtrl = interface;
 IWMPEditCtrlDisp = dispinterface;
 IWMPSkinList = interface;
 IWMPSkinListDisp = dispinterface;
 IWMPPluginUIHost = interface;
 IWMPPluginUIHostDisp = dispinterface;
 IWMPMenuCtrl = interface;
 IWMPMenuCtrlDisp = dispinterface;
 IWMPAutoMenuCtrl = interface;
 IWMPAutoMenuCtrlDisp = dispinterface;
 IWMPRegionalButtonCtrl = interface;
 IWMPRegionalButtonCtrlDisp = dispinterface;
 IWMPRegionalButtonEvents = dispinterface;
 IWMPRegionalButton = interface;
 IWMPRegionalButtonDisp = dispinterface;
 IWMPCustomSliderCtrlEvents = dispinterface;
 IWMPCustomSlider = interface;
 IWMPCustomSliderDisp = dispinterface;
 IWMPTextCtrl = interface;
 IWMPTextCtrlDisp = dispinterface;
 ITaskCntrCtrl = interface;
 ITaskCntrCtrlDisp = dispinterface;
 _WMPCoreEvents = dispinterface;
 IWMPGraphEventHandler = interface;
 IWMPGraphEventHandlerDisp = dispinterface;
 IBattery = interface;
 IBatteryDisp = dispinterface;
 IBatteryPreset = interface;
 IBatteryPresetDisp = dispinterface;
 IBatteryRandomPreset = interface;
 IBatteryRandomPresetDisp = dispinterface;
 IBatterySavedPreset = interface;
 IBatterySavedPresetDisp = dispinterface;
 IBarsEffect = interface;
 IBarsEffectDisp = dispinterface;
 IWMPExternal = interface;
 IWMPExternalDisp = dispinterface;
 IWMPExternalColors = interface;
 IWMPExternalColorsDisp = dispinterface;
 IWMPSubscriptionServiceLimited = interface;
 IWMPSubscriptionServiceLimitedDisp = dispinterface;
 IWMPSubscriptionServiceExternal = interface;
 IWMPSubscriptionServiceExternalDisp = dispinterface;
 IWMPDownloadManager = interface;
 IWMPDownloadManagerDisp = dispinterface;
 IWMPDownloadCollection = interface;
 IWMPDownloadCollectionDisp = dispinterface;
 IWMPDownloadItem2 = interface;
 IWMPDownloadItem2Disp = dispinterface;
 IWMPDownloadItem = interface;
 IWMPDownloadItemDisp = dispinterface;
 IWMPSubscriptionServicePlayMedia = interface;
 IWMPSubscriptionServicePlayMediaDisp = dispinterface;
 IWMPDiscoExternal = interface;
 IWMPDiscoExternalDisp = dispinterface;
 IWMPCDDVDWizardExternal = interface;
 IWMPCDDVDWizardExternalDisp = dispinterface;
 IWMPBaseExternal = interface;
 IWMPBaseExternalDisp = dispinterface;
 IWMPOfflineExternal = interface;
 IWMPOfflineExternalDisp = dispinterface;
 IWMPDMRAVTransportService = interface;
 IWMPDMRAVTransportServiceDisp = dispinterface;
 IWMPDMRConnectionManagerService = interface;
 IWMPDMRConnectionManagerServiceDisp = dispinterface;
 IWMPDMRRenderingControlService = interface;
 IWMPDMRRenderingControlServiceDisp = dispinterface;

//Map CoClass to its default interface

 WindowsMediaPlayer = IWMPPlayer4;
 WMPButtonCtrl = IWMPButtonCtrl;
 WMPListBoxCtrl = IWMPListBoxCtrl;
 WMPSliderCtrl = IWMPSliderCtrl;
 WMPVideoCtrl = IWMPVideoCtrl;
 WMPEffects = IWMPEffectsCtrl;
 WMPEqualizerSettingsCtrl = IWMPEqualizerSettingsCtrl;
 WMPVideoSettingsCtrl = IWMPVideoSettingsCtrl;
 WMPLibraryTreeCtrl = IWMPLibraryTreeCtrl;
 WMPEditCtrl = IWMPEditCtrl;
 WMPSkinList = IWMPSkinList;
 WMPMenuCtrl = IWMPMenuCtrl;
 WMPAutoMenuCtrl = IWMPAutoMenuCtrl;
 WMPRegionalButtonCtrl = IWMPRegionalButtonCtrl;
 WMPRegionalButton = IWMPRegionalButton;
 WMPCustomSliderCtrl = IWMPCustomSlider;
 WMPTextCtrl = IWMPTextCtrl;
 WMPPlaylistCtrl = IWMPPlaylistCtrl;
 WMPCore = IWMPCore3;

//records, unions, aliases

 ULONG_PTR = LongWord;

//interface declarations

// IWMPEvents : IWMPEvents: Public interface.

 IWMPEvents = interface(IUnknown)
   ['{19A6627B-DA9E-47C1-BB23-00B5E668236A}']
    // OpenStateChange : Sent when the control changes OpenState 
   function OpenStateChange(NewState:Integer):HRESULT;stdcall;
    // PlayStateChange : Sent when the control changes PlayState 
   function PlayStateChange(NewState:Integer):HRESULT;stdcall;
    // AudioLanguageChange : Sent when the current audio language has changed 
   function AudioLanguageChange(LangID:Integer):HRESULT;stdcall;
    // StatusChange : Sent when the status string changes 
   function StatusChange:HRESULT;stdcall;
    // ScriptCommand : Sent when a synchronized command or URL is received 
   function ScriptCommand(scType:WideString;Param:WideString):HRESULT;stdcall;
    // NewStream : Sent when a new stream is started in a channel 
   function NewStream:HRESULT;stdcall;
    // Disconnect : Sent when the control is disconnected from the server 
   function Disconnect(Result:Integer):HRESULT;stdcall;
    // Buffering : Sent when the control begins or ends buffering 
   function Buffering(Start:WordBool):HRESULT;stdcall;
    // Error : Sent when the control has an error condition 
   function Error:HRESULT;stdcall;
    // Warning : Sent when the control encounters a problem 
   function Warning(WarningType:Integer;Param:Integer;Description:WideString):HRESULT;stdcall;
    // EndOfStream : Sent when the end of file is reached 
   function EndOfStream(Result:Integer):HRESULT;stdcall;
    // PositionChange : Indicates that the current position of the movie has changed 
   function PositionChange(oldPosition:Double;newPosition:Double):HRESULT;stdcall;
    // MarkerHit : Sent when a marker is reached 
   function MarkerHit(MarkerNum:Integer):HRESULT;stdcall;
    // DurationUnitChange : Indicates that the unit used to express duration and position has changed 
   function DurationUnitChange(NewDurationUnit:Integer):HRESULT;stdcall;
    // CdromMediaChange : Indicates that the CD ROM media has changed 
   function CdromMediaChange(CdromNum:Integer):HRESULT;stdcall;
    // PlaylistChange : Sent when a playlist changes 
   function PlaylistChange(Playlist:IDispatch;change:WMPPlaylistChangeEventType):HRESULT;stdcall;
    // CurrentPlaylistChange : Sent when the current playlist changes 
   function CurrentPlaylistChange(change:WMPPlaylistChangeEventType):HRESULT;stdcall;
    // CurrentPlaylistItemAvailable : Sent when a current playlist item becomes available 
   function CurrentPlaylistItemAvailable(bstrItemName:WideString):HRESULT;stdcall;
    // MediaChange : Sent when a media object changes 
   function MediaChange(Item:IDispatch):HRESULT;stdcall;
    // CurrentMediaItemAvailable : Sent when a current media item becomes available 
   function CurrentMediaItemAvailable(bstrItemName:WideString):HRESULT;stdcall;
    // CurrentItemChange : Sent when the item selection on the current playlist changes 
   function CurrentItemChange(pdispMedia:IDispatch):HRESULT;stdcall;
    // MediaCollectionChange : Sent when the media collection needs to be requeried 
   function MediaCollectionChange:HRESULT;stdcall;
    // MediaCollectionAttributeStringAdded : Sent when an attribute string is added in the media collection 
   function MediaCollectionAttributeStringAdded(bstrAttribName:WideString;bstrAttribVal:WideString):HRESULT;stdcall;
    // MediaCollectionAttributeStringRemoved : Sent when an attribute string is removed from the media collection 
   function MediaCollectionAttributeStringRemoved(bstrAttribName:WideString;bstrAttribVal:WideString):HRESULT;stdcall;
    // MediaCollectionAttributeStringChanged : Sent when an attribute string is changed in the media collection 
   function MediaCollectionAttributeStringChanged(bstrAttribName:WideString;bstrOldAttribVal:WideString;bstrNewAttribVal:WideString):HRESULT;stdcall;
    // PlaylistCollectionChange : Sent when playlist collection needs to be requeried 
   function PlaylistCollectionChange:HRESULT;stdcall;
    // PlaylistCollectionPlaylistAdded : Sent when a playlist is added to the playlist collection 
   function PlaylistCollectionPlaylistAdded(bstrPlaylistName:WideString):HRESULT;stdcall;
    // PlaylistCollectionPlaylistRemoved : Sent when a playlist is removed from the playlist collection 
   function PlaylistCollectionPlaylistRemoved(bstrPlaylistName:WideString):HRESULT;stdcall;
    // PlaylistCollectionPlaylistSetAsDeleted : Sent when a playlist has been set or reset as deleted 
   function PlaylistCollectionPlaylistSetAsDeleted(bstrPlaylistName:WideString;varfIsDeleted:WordBool):HRESULT;stdcall;
    // ModeChange : Playlist playback mode has changed 
   function ModeChange(ModeName:WideString;NewValue:WordBool):HRESULT;stdcall;
    // MediaError : Sent when the media object has an error condition 
   function MediaError(pMediaObject:IDispatch):HRESULT;stdcall;
    // OpenPlaylistSwitch : Current playlist switch with no open state change 
   function OpenPlaylistSwitch(pItem:IDispatch):HRESULT;stdcall;
    // DomainChange : Send a current domain 
   function DomainChange(strDomain:WideString):HRESULT;stdcall;
    // SwitchedToPlayerApplication : Sent when display switches to player application 
   function SwitchedToPlayerApplication:HRESULT;stdcall;
    // SwitchedToControl : Sent when display switches to control 
   function SwitchedToControl:HRESULT;stdcall;
    // PlayerDockedStateChange : Sent when the player docks or undocks 
   function PlayerDockedStateChange:HRESULT;stdcall;
    // PlayerReconnect : Sent when the OCX reconnects to the player 
   function PlayerReconnect:HRESULT;stdcall;
    // Click : Occurs when a user clicks the mouse 
   function Click(nButton:Smallint;nShiftState:Smallint;fX:Integer;fY:Integer):HRESULT;stdcall;
    // DoubleClick : Occurs when a user double-clicks the mouse 
   function DoubleClick(nButton:Smallint;nShiftState:Smallint;fX:Integer;fY:Integer):HRESULT;stdcall;
    // KeyDown : Occurs when a key is pressed 
   function KeyDown(nKeyCode:Smallint;nShiftState:Smallint):HRESULT;stdcall;
    // KeyPress : Occurs when a key is pressed and released 
   function KeyPress(nKeyAscii:Smallint):HRESULT;stdcall;
    // KeyUp : Occurs when a key is released 
   function KeyUp(nKeyCode:Smallint;nShiftState:Smallint):HRESULT;stdcall;
    // MouseDown : Occurs when a mouse button is pressed 
   function MouseDown(nButton:Smallint;nShiftState:Smallint;fX:Integer;fY:Integer):HRESULT;stdcall;
    // MouseMove : Occurs when a mouse pointer is moved 
   function MouseMove(nButton:Smallint;nShiftState:Smallint;fX:Integer;fY:Integer):HRESULT;stdcall;
    // MouseUp : Occurs when a mouse button is released 
   function MouseUp(nButton:Smallint;nShiftState:Smallint;fX:Integer;fY:Integer):HRESULT;stdcall;
  end;


// IWMPEvents2 : IWMPEvents2: Public interface.

 IWMPEvents2 = interface(IWMPEvents)
   ['{1E7601FA-47EA-4107-9EA9-9004ED9684FF}']
    // DeviceConnect : Occurs when a device is connected 
   function DeviceConnect(pDevice:IWMPSyncDevice):HRESULT;stdcall;
    // DeviceDisconnect : Occurs when a device is disconnected 
   function DeviceDisconnect(pDevice:IWMPSyncDevice):HRESULT;stdcall;
    // DeviceStatusChange : Occurs when a device status changes 
   function DeviceStatusChange(pDevice:IWMPSyncDevice;NewStatus:WMPDeviceStatus):HRESULT;stdcall;
    // DeviceSyncStateChange : Occurs when a device sync state changes 
   function DeviceSyncStateChange(pDevice:IWMPSyncDevice;NewState:WMPSyncState):HRESULT;stdcall;
    // DeviceSyncError : Occurs when a device's media has an error 
   function DeviceSyncError(pDevice:IWMPSyncDevice;pMedia:IDispatch):HRESULT;stdcall;
    // CreatePartnershipComplete : Occurs when createPartnership call completes 
   function CreatePartnershipComplete(pDevice:IWMPSyncDevice;hrResult:HResult):HRESULT;stdcall;
  end;


// IWMPSyncDevice : IWMPSyncDevice: Public interface for Windows Media Player SDK.

 IWMPSyncDevice = interface(IUnknown)
   ['{82A2986C-0293-4FD0-B279-B21B86C058BE}']
   function Get_friendlyName : WideString; stdcall;
   procedure Set_friendlyName(const pbstrName:WideString); stdcall;
   function Get_deviceName : WideString; stdcall;
   function Get_deviceId : WideString; stdcall;
   function Get_partnershipIndex : Integer; stdcall;
   function Get_connected : WordBool; stdcall;
   function Get_status : WMPDeviceStatus; stdcall;
   function Get_syncState : WMPSyncState; stdcall;
   function Get_progress : Integer; stdcall;
    // getItemInfo :  
   function getItemInfo(bstrItemName:WideString):HRESULT;stdcall;
    // createPartnership :  
   function createPartnership(vbShowUI:WordBool):HRESULT;stdcall;
    // deletePartnership :  
   function deletePartnership:HRESULT;stdcall;
    // Start :  
   function Start:HRESULT;stdcall;
    // stop :  
   function stop:HRESULT;stdcall;
    // showSettings :  
   function showSettings:HRESULT;stdcall;
    // isIdentical :  
   function isIdentical(pDevice:IWMPSyncDevice):HRESULT;stdcall;
    // friendlyName :  
   property friendlyName:WideString read Get_friendlyName write Set_friendlyName;
    // deviceName :  
   property deviceName:WideString read Get_deviceName;
    // deviceId :  
   property deviceId:WideString read Get_deviceId;
    // partnershipIndex :  
   property partnershipIndex:Integer read Get_partnershipIndex;
    // connected :  
   property connected:WordBool read Get_connected;
    // status :  
   property status:WMPDeviceStatus read Get_status;
    // syncState :  
   property syncState:WMPSyncState read Get_syncState;
    // progress :  
   property progress:Integer read Get_progress;
  end;


// IWMPEvents3 : IWMPEvents3: Public interface.

 IWMPEvents3 = interface(IWMPEvents2)
   ['{1F504270-A66B-4223-8E96-26A06C63D69F}']
    // CdromRipStateChange : Occurs when ripping state changes 
   function CdromRipStateChange(pCdromRip:IWMPCdromRip;wmprs:WMPRipState):HRESULT;stdcall;
    // CdromRipMediaError : Occurs when an error happens while ripping a media 
   function CdromRipMediaError(pCdromRip:IWMPCdromRip;pMedia:IDispatch):HRESULT;stdcall;
    // CdromBurnStateChange : Occurs when burning state changes 
   function CdromBurnStateChange(pCdromBurn:IWMPCdromBurn;wmpbs:WMPBurnState):HRESULT;stdcall;
    // CdromBurnMediaError : Occurs when an error happens while burning a media 
   function CdromBurnMediaError(pCdromBurn:IWMPCdromBurn;pMedia:IDispatch):HRESULT;stdcall;
    // CdromBurnError : Occurs when a generic error happens while burning 
   function CdromBurnError(pCdromBurn:IWMPCdromBurn;hrError:HResult):HRESULT;stdcall;
    // LibraryConnect : Occurs when a library is connected 
   function LibraryConnect(pLibrary:IWMPLibrary):HRESULT;stdcall;
    // LibraryDisconnect : Occurs when a library is disconnected 
   function LibraryDisconnect(pLibrary:IWMPLibrary):HRESULT;stdcall;
    // FolderScanStateChange : Occurs when a folder scan state changes 
   function FolderScanStateChange(wmpfss:WMPFolderScanState):HRESULT;stdcall;
    // StringCollectionChange : Sent when a string collection changes 
   function StringCollectionChange(pdispStringCollection:IDispatch;change:WMPStringCollectionChangeEventType;lCollectionIndex:Integer):HRESULT;stdcall;
    // MediaCollectionMediaAdded : Sent when a media is added to the local library 
   function MediaCollectionMediaAdded(pdispMedia:IDispatch):HRESULT;stdcall;
    // MediaCollectionMediaRemoved : Sent when a media is removed from the local library 
   function MediaCollectionMediaRemoved(pdispMedia:IDispatch):HRESULT;stdcall;
  end;


// IWMPCdromRip : IWMPCdromRip: Public interface for Windows Media Player SDK.

 IWMPCdromRip = interface(IUnknown)
   ['{56E2294F-69ED-4629-A869-AEA72C0DCC2C}']
   function Get_ripState : WMPRipState; stdcall;
   function Get_ripProgress : Integer; stdcall;
    // startRip :  
   function startRip:HRESULT;stdcall;
    // stopRip :  
   function stopRip:HRESULT;stdcall;
    // ripState :  
   property ripState:WMPRipState read Get_ripState;
    // ripProgress :  
   property ripProgress:Integer read Get_ripProgress;
  end;


// IWMPCdromBurn : IWMPCdromBurn: Public interface for Windows Media Player SDK.

 IWMPCdromBurn = interface(IUnknown)
   ['{BD94DBEB-417F-4928-AA06-087D56ED9B59}']
    // isAvailable :  
   function isAvailable(bstrItem:WideString):HRESULT;stdcall;
    // getItemInfo :  
   function getItemInfo(bstrItem:WideString):HRESULT;stdcall;
   function Get_label_ : WideString; stdcall;
   procedure Set_label_(const pbstrLabel:WideString); stdcall;
   function Get_burnFormat : WMPBurnFormat; stdcall;
   procedure Set_burnFormat(const pwmpbf:WMPBurnFormat); stdcall;
   function Get_burnPlaylist : IWMPPlaylist; stdcall;
   procedure Set_burnPlaylist(const ppPlaylist:IWMPPlaylist); stdcall;
    // refreshStatus :  
   function refreshStatus:HRESULT;stdcall;
   function Get_burnState : WMPBurnState; stdcall;
   function Get_burnProgress : Integer; stdcall;
    // startBurn :  
   function startBurn:HRESULT;stdcall;
    // stopBurn :  
   function stopBurn:HRESULT;stdcall;
    // erase :  
   function erase:HRESULT;stdcall;
    // label :  
   property label_:WideString read Get_label_ write Set_label_;
    // burnFormat :  
   property burnFormat:WMPBurnFormat read Get_burnFormat write Set_burnFormat;
    // burnPlaylist :  
   property burnPlaylist:IWMPPlaylist read Get_burnPlaylist write Set_burnPlaylist;
    // burnState :  
   property burnState:WMPBurnState read Get_burnState;
    // burnProgress :  
   property burnProgress:Integer read Get_burnProgress;
  end;


// IWMPPlaylist : IWMPPlaylist: Public interface.

 IWMPPlaylist = interface(IDispatch)
   ['{D5F0F4F1-130C-11D3-B14E-00C04F79FAA6}']
   function Get_count : Integer; safecall;
   function Get_name : WideString; safecall;
   procedure Set_name(const pbstrName:WideString); safecall;
   function Get_attributeCount : Integer; safecall;
   function Get_attributeName(lIndex:Integer) : WideString; safecall;
   function Get_Item : IWMPMedia; safecall;
    // getItemInfo : Returns the value of a playlist attribute 
   function getItemInfo(bstrName:WideString):WideString;safecall;
    // setItemInfo : Sets the value of a playlist attribute 
   procedure setItemInfo(bstrName:WideString;bstrValue:WideString);safecall;
   function Get_isIdentical(pIWMPPlaylist:IWMPPlaylist) : WordBool; safecall;
    // clear : Removes all items from the playlist 
   procedure clear;safecall;
    // insertItem : Inserts an item into the playlist at the specified location 
   procedure insertItem(lIndex:Integer;pIWMPMedia:IWMPMedia);safecall;
    // appendItem : Adds an item to the end of the playlist 
   procedure appendItem(pIWMPMedia:IWMPMedia);safecall;
    // removeItem : Removes the specified item from the playlist 
   procedure removeItem(pIWMPMedia:IWMPMedia);safecall;
    // moveItem : Changes the location of an item in the playlist 
   procedure moveItem(lIndexOld:Integer;lIndexNew:Integer);safecall;
    // count : Returns the number of items in the playlist 
   property count:Integer read Get_count;
    // name : Returns the name of the playlist 
   property name:WideString read Get_name write Set_name;
    // attributeCount : Returns the number of attributes associated with the playlist 
   property attributeCount:Integer read Get_attributeCount;
    // attributeName : Returns the name of an attribute specified by an index 
   property attributeName[lIndex:Integer]:WideString read Get_attributeName;
    // Item : Returns the item at the specified index 
   property Item:IWMPMedia read Get_Item;
    // isIdentical : Determines if the supplied object is the same as the this one 
   property isIdentical[pIWMPPlaylist:IWMPPlaylist]:WordBool read Get_isIdentical;
  end;


// IWMPPlaylist : IWMPPlaylist: Public interface.

 IWMPPlaylistDisp = dispinterface
   ['{D5F0F4F1-130C-11D3-B14E-00C04F79FAA6}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // getItemInfo : Returns the value of a playlist attribute 
   function getItemInfo(bstrName:WideString):WideString;dispid 203;
    // setItemInfo : Sets the value of a playlist attribute 
   procedure setItemInfo(bstrName:WideString;bstrValue:WideString);dispid 204;
    // clear : Removes all items from the playlist 
   procedure clear;dispid 205;
    // insertItem : Inserts an item into the playlist at the specified location 
   procedure insertItem(lIndex:Integer;pIWMPMedia:IWMPMedia);dispid 206;
    // appendItem : Adds an item to the end of the playlist 
   procedure appendItem(pIWMPMedia:IWMPMedia);dispid 207;
    // removeItem : Removes the specified item from the playlist 
   procedure removeItem(pIWMPMedia:IWMPMedia);dispid 208;
    // moveItem : Changes the location of an item in the playlist 
   procedure moveItem(lIndexOld:Integer;lIndexNew:Integer);dispid 209;
    // count : Returns the number of items in the playlist 
   property count:Integer  readonly dispid 201;
    // name : Returns the name of the playlist 
   property name:WideString dispid 202;
    // attributeCount : Returns the number of attributes associated with the playlist 
   property attributeCount:Integer  readonly dispid 210;
    // attributeName : Returns the name of an attribute specified by an index 
   property attributeName[lIndex:Integer]:WideString  readonly dispid 211;
    // Item : Returns the item at the specified index 
   property Item:IWMPMedia  readonly dispid 212;
    // isIdentical : Determines if the supplied object is the same as the this one 
   property isIdentical[pIWMPPlaylist:IWMPPlaylist]:WordBool  readonly dispid 213;
  end;


// IWMPMedia : IWMPMedia: Public interface.

 IWMPMedia = interface(IDispatch)
   ['{94D55E95-3FAC-11D3-B155-00C04F79FAA6}']
   function Get_isIdentical(pIWMPMedia:IWMPMedia) : WordBool; safecall;
   function Get_sourceURL : WideString; safecall;
   function Get_name : WideString; safecall;
   procedure Set_name(const pbstrName:WideString); safecall;
   function Get_imageSourceWidth : Integer; safecall;
   function Get_imageSourceHeight : Integer; safecall;
   function Get_markerCount : Integer; safecall;
    // getMarkerTime : Returns the time of a marker 
   function getMarkerTime(MarkerNum:Integer):Double;safecall;
    // getMarkerName : Returns the name of a marker 
   function getMarkerName(MarkerNum:Integer):WideString;safecall;
   function Get_duration : Double; safecall;
   function Get_durationString : WideString; safecall;
   function Get_attributeCount : Integer; safecall;
    // getAttributeName : Returns the name of the attribute whose index has been specified 
   function getAttributeName(lIndex:Integer):WideString;safecall;
    // getItemInfo : Returns the value of specified attribute for this media 
   function getItemInfo(bstrItemName:WideString):WideString;safecall;
    // setItemInfo : Sets the value of specified attribute for this media 
   procedure setItemInfo(bstrItemName:WideString;bstrVal:WideString);safecall;
    // getItemInfoByAtom : Gets an item info by atom 
   function getItemInfoByAtom(lAtom:Integer):WideString;safecall;
    // isMemberOf : Is the media a member of the given playlist 
   function isMemberOf(pPlaylist:IWMPPlaylist):WordBool;safecall;
    // isReadOnlyItem : Is the attribute read only 
   function isReadOnlyItem(bstrItemName:WideString):WordBool;safecall;
    // isIdentical : Determines if the supplied object is the same as the this one 
   property isIdentical[pIWMPMedia:IWMPMedia]:WordBool read Get_isIdentical;
    // sourceURL : Returns the media URL 
   property sourceURL:WideString read Get_sourceURL;
    // name : Returns the name of the media 
   property name:WideString read Get_name write Set_name;
    // imageSourceWidth : Returns the original width of the source images 
   property imageSourceWidth:Integer read Get_imageSourceWidth;
    // imageSourceHeight : Returns the original height of the source images 
   property imageSourceHeight:Integer read Get_imageSourceHeight;
    // markerCount : Returns the number of markers in the file 
   property markerCount:Integer read Get_markerCount;
    // duration : Returns duration of current media 
   property duration:Double read Get_duration;
    // durationString : Returns duration of current media as a string 
   property durationString:WideString read Get_durationString;
    // attributeCount : Returns the count of the attributes associated with this media 
   property attributeCount:Integer read Get_attributeCount;
  end;


// IWMPMedia : IWMPMedia: Public interface.

 IWMPMediaDisp = dispinterface
   ['{94D55E95-3FAC-11D3-B155-00C04F79FAA6}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // getMarkerTime : Returns the time of a marker 
   function getMarkerTime(MarkerNum:Integer):Double;dispid 755;
    // getMarkerName : Returns the name of a marker 
   function getMarkerName(MarkerNum:Integer):WideString;dispid 756;
    // getAttributeName : Returns the name of the attribute whose index has been specified 
   function getAttributeName(lIndex:Integer):WideString;dispid 760;
    // getItemInfo : Returns the value of specified attribute for this media 
   function getItemInfo(bstrItemName:WideString):WideString;dispid 761;
    // setItemInfo : Sets the value of specified attribute for this media 
   procedure setItemInfo(bstrItemName:WideString;bstrVal:WideString);dispid 762;
    // getItemInfoByAtom : Gets an item info by atom 
   function getItemInfoByAtom(lAtom:Integer):WideString;dispid 765;
    // isMemberOf : Is the media a member of the given playlist 
   function isMemberOf(pPlaylist:IWMPPlaylist):WordBool;dispid 766;
    // isReadOnlyItem : Is the attribute read only 
   function isReadOnlyItem(bstrItemName:WideString):WordBool;dispid 767;
    // isIdentical : Determines if the supplied object is the same as the this one 
   property isIdentical[pIWMPMedia:IWMPMedia]:WordBool  readonly dispid 763;
    // sourceURL : Returns the media URL 
   property sourceURL:WideString  readonly dispid 751;
    // name : Returns the name of the media 
   property name:WideString dispid 764;
    // imageSourceWidth : Returns the original width of the source images 
   property imageSourceWidth:Integer  readonly dispid 752;
    // imageSourceHeight : Returns the original height of the source images 
   property imageSourceHeight:Integer  readonly dispid 753;
    // markerCount : Returns the number of markers in the file 
   property markerCount:Integer  readonly dispid 754;
    // duration : Returns duration of current media 
   property duration:Double  readonly dispid 757;
    // durationString : Returns duration of current media as a string 
   property durationString:WideString  readonly dispid 758;
    // attributeCount : Returns the count of the attributes associated with this media 
   property attributeCount:Integer  readonly dispid 759;
  end;


// IWMPLibrary : IWMPLibrary: Public interface for Windows Media Player SDK.

 IWMPLibrary = interface(IUnknown)
   ['{3DF47861-7DF1-4C1F-A81B-4C26F0F7A7C6}']
   function Get_name : WideString; stdcall;
   function Get_type_ : WMPLibraryType; stdcall;
   function Get_mediaCollection : IWMPMediaCollection; stdcall;
    // isIdentical :  
   function isIdentical(pIWMPLibrary:IWMPLibrary):HRESULT;stdcall;
    // name :  
   property name:WideString read Get_name;
    // type :  
   property type_:WMPLibraryType read Get_type_;
    // mediaCollection :  
   property mediaCollection:IWMPMediaCollection read Get_mediaCollection;
  end;


// IWMPMediaCollection : IWMPMediaCollection: Public interface.

 IWMPMediaCollection = interface(IDispatch)
   ['{8363BC22-B4B4-4B19-989D-1CD765749DD1}']
    // add : Creates a new media object 
   function add(bstrURL:WideString):IWMPMedia;safecall;
    // getAll : Returns a collection of all the items 
   function getAll:IWMPPlaylist;safecall;
    // getByName : Returns a collection of items with the given name 
   function getByName(bstrName:WideString):IWMPPlaylist;safecall;
    // getByGenre : Returns a collection of items with the given genre 
   function getByGenre(bstrGenre:WideString):IWMPPlaylist;safecall;
    // getByAuthor : Returns a collection of items by a given author 
   function getByAuthor(bstrAuthor:WideString):IWMPPlaylist;safecall;
    // getByAlbum : Returns a collection of items from the given album 
   function getByAlbum(bstrAlbum:WideString):IWMPPlaylist;safecall;
    // getByAttribute : Returns a collection of items with the given attribute 
   function getByAttribute(bstrAttribute:WideString;bstrValue:WideString):IWMPPlaylist;safecall;
    // remove : Removes an item from the media collection 
   procedure remove(pItem:IWMPMedia;varfDeleteFile:WordBool);safecall;
    // getAttributeStringCollection : Returns the string collection associated with an attribute 
   function getAttributeStringCollection(bstrAttribute:WideString;bstrMediaType:WideString):IWMPStringCollection;safecall;
    // getMediaAtom : Gets an atom associated with an item name which can be requested from an IWMPMedia out of this collection via getItemInfoByAtom 
   function getMediaAtom(bstrItemName:WideString):Integer;safecall;
    // setDeleted : Sets the deleted flag on a media object 
   procedure setDeleted(pItem:IWMPMedia;varfIsDeleted:WordBool);safecall;
    // isDeleted : Gets the deleted flag on a media object 
   function isDeleted(pItem:IWMPMedia):WordBool;safecall;
  end;


// IWMPMediaCollection : IWMPMediaCollection: Public interface.

 IWMPMediaCollectionDisp = dispinterface
   ['{8363BC22-B4B4-4B19-989D-1CD765749DD1}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // add : Creates a new media object 
   function add(bstrURL:WideString):IWMPMedia;dispid 452;
    // getAll : Returns a collection of all the items 
   function getAll:IWMPPlaylist;dispid 453;
    // getByName : Returns a collection of items with the given name 
   function getByName(bstrName:WideString):IWMPPlaylist;dispid 454;
    // getByGenre : Returns a collection of items with the given genre 
   function getByGenre(bstrGenre:WideString):IWMPPlaylist;dispid 455;
    // getByAuthor : Returns a collection of items by a given author 
   function getByAuthor(bstrAuthor:WideString):IWMPPlaylist;dispid 456;
    // getByAlbum : Returns a collection of items from the given album 
   function getByAlbum(bstrAlbum:WideString):IWMPPlaylist;dispid 457;
    // getByAttribute : Returns a collection of items with the given attribute 
   function getByAttribute(bstrAttribute:WideString;bstrValue:WideString):IWMPPlaylist;dispid 458;
    // remove : Removes an item from the media collection 
   procedure remove(pItem:IWMPMedia;varfDeleteFile:WordBool);dispid 459;
    // getAttributeStringCollection : Returns the string collection associated with an attribute 
   function getAttributeStringCollection(bstrAttribute:WideString;bstrMediaType:WideString):IWMPStringCollection;dispid 461;
    // getMediaAtom : Gets an atom associated with an item name which can be requested from an IWMPMedia out of this collection via getItemInfoByAtom 
   function getMediaAtom(bstrItemName:WideString):Integer;dispid 470;
    // setDeleted : Sets the deleted flag on a media object 
   procedure setDeleted(pItem:IWMPMedia;varfIsDeleted:WordBool);dispid 471;
    // isDeleted : Gets the deleted flag on a media object 
   function isDeleted(pItem:IWMPMedia):WordBool;dispid 472;
  end;


// IWMPStringCollection : IWMPStringCollection: Public interface.

 IWMPStringCollection = interface(IDispatch)
   ['{4A976298-8C0D-11D3-B389-00C04F68574B}']
   function Get_count : Integer; safecall;
    // Item : Returns the string at the given index 
   function Item(lIndex:Integer):WideString;safecall;
    // count : Returns the number of items in the string collection 
   property count:Integer read Get_count;
  end;


// IWMPStringCollection : IWMPStringCollection: Public interface.

 IWMPStringCollectionDisp = dispinterface
   ['{4A976298-8C0D-11D3-B389-00C04F68574B}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // Item : Returns the string at the given index 
   function Item(lIndex:Integer):WideString;dispid 402;
    // count : Returns the number of items in the string collection 
   property count:Integer  readonly dispid 401;
  end;


// IWMPEvents4 : IWMPEvents4: Public interface.

 IWMPEvents4 = interface(IWMPEvents3)
   ['{26DABCFA-306B-404D-9A6F-630A8405048D}']
    // DeviceEstimation : Occurs when the sync estimation completed 
   function DeviceEstimation(pDevice:IWMPSyncDevice;hrResult:HResult;qwEstimatedUsedSpace:Int64;qwEstimatedSpace:Int64):HRESULT;stdcall;
  end;


// _WMPOCXEvents : _WMPOCXEvents: Public interface.

 _WMPOCXEvents = dispinterface
   ['{6BF52A51-394A-11D3-B153-00C04F79FAA6}']
    // OpenStateChange : Sent when the control changes OpenState 
   procedure OpenStateChange(NewState:Integer);dispid 5001;
    // PlayStateChange : Sent when the control changes PlayState 
   procedure PlayStateChange(NewState:Integer);dispid 5101;
    // AudioLanguageChange : Sent when the current audio language has changed 
   procedure AudioLanguageChange(LangID:Integer);dispid 5102;
    // StatusChange : Sent when the status string changes 
   procedure StatusChange;dispid 5002;
    // ScriptCommand : Sent when a synchronized command or URL is received 
   procedure ScriptCommand(scType:WideString;Param:WideString);dispid 5301;
    // NewStream : Sent when a new stream is started in a channel 
   procedure NewStream;dispid 5403;
    // Disconnect : Sent when the control is disconnected from the server 
   procedure Disconnect(Result:Integer);dispid 5401;
    // Buffering : Sent when the control begins or ends buffering 
   procedure Buffering(Start:WordBool);dispid 5402;
    // Error : Sent when the control has an error condition 
   procedure Error;dispid 5501;
    // Warning : Sent when the control encounters a problem 
   procedure Warning(WarningType:Integer;Param:Integer;Description:WideString);dispid 5601;
    // EndOfStream : Sent when the end of file is reached 
   procedure EndOfStream(Result:Integer);dispid 5201;
    // PositionChange : Indicates that the current position of the movie has changed 
   procedure PositionChange(oldPosition:Double;newPosition:Double);dispid 5202;
    // MarkerHit : Sent when a marker is reached 
   procedure MarkerHit(MarkerNum:Integer);dispid 5203;
    // DurationUnitChange : Indicates that the unit used to express duration and position has changed 
   procedure DurationUnitChange(NewDurationUnit:Integer);dispid 5204;
    // CdromMediaChange : Indicates that the CD ROM media has changed 
   procedure CdromMediaChange(CdromNum:Integer);dispid 5701;
    // PlaylistChange : Sent when a playlist changes 
   procedure PlaylistChange(Playlist:IDispatch;change:WMPPlaylistChangeEventType);dispid 5801;
    // CurrentPlaylistChange : Sent when the current playlist changes 
   procedure CurrentPlaylistChange(change:WMPPlaylistChangeEventType);dispid 5804;
    // CurrentPlaylistItemAvailable : Sent when a current playlist item becomes available 
   procedure CurrentPlaylistItemAvailable(bstrItemName:WideString);dispid 5805;
    // MediaChange : Sent when a media object changes 
   procedure MediaChange(Item:IDispatch);dispid 5802;
    // CurrentMediaItemAvailable : Sent when a current media item becomes available 
   procedure CurrentMediaItemAvailable(bstrItemName:WideString);dispid 5803;
    // CurrentItemChange : Sent when the item selection on the current playlist changes 
   procedure CurrentItemChange(pdispMedia:IDispatch);dispid 5806;
    // MediaCollectionChange : Sent when the media collection needs to be requeried 
   procedure MediaCollectionChange;dispid 5807;
    // MediaCollectionAttributeStringAdded : Sent when an attribute string is added in the media collection 
   procedure MediaCollectionAttributeStringAdded(bstrAttribName:WideString;bstrAttribVal:WideString);dispid 5808;
    // MediaCollectionAttributeStringRemoved : Sent when an attribute string is removed from the media collection 
   procedure MediaCollectionAttributeStringRemoved(bstrAttribName:WideString;bstrAttribVal:WideString);dispid 5809;
    // MediaCollectionAttributeStringChanged : Sent when an attribute string is changed in the media collection 
   procedure MediaCollectionAttributeStringChanged(bstrAttribName:WideString;bstrOldAttribVal:WideString;bstrNewAttribVal:WideString);dispid 5820;
    // PlaylistCollectionChange : Sent when playlist collection needs to be requeried 
   procedure PlaylistCollectionChange;dispid 5810;
    // PlaylistCollectionPlaylistAdded : Sent when a playlist is added to the playlist collection 
   procedure PlaylistCollectionPlaylistAdded(bstrPlaylistName:WideString);dispid 5811;
    // PlaylistCollectionPlaylistRemoved : Sent when a playlist is removed from the playlist collection 
   procedure PlaylistCollectionPlaylistRemoved(bstrPlaylistName:WideString);dispid 5812;
    // PlaylistCollectionPlaylistSetAsDeleted : Sent when a playlist has been set or reset as deleted 
   procedure PlaylistCollectionPlaylistSetAsDeleted(bstrPlaylistName:WideString;varfIsDeleted:WordBool);dispid 5818;
    // ModeChange : Playlist playback mode has changed 
   procedure ModeChange(ModeName:WideString;NewValue:WordBool);dispid 5819;
    // MediaError : Sent when the media object has an error condition 
   procedure MediaError(pMediaObject:IDispatch);dispid 5821;
    // OpenPlaylistSwitch : Current playlist switch with no open state change 
   procedure OpenPlaylistSwitch(pItem:IDispatch);dispid 5823;
    // DomainChange : Send a current domain 
   procedure DomainChange(strDomain:WideString);dispid 5822;
    // SwitchedToPlayerApplication : Sent when display switches to player application 
   procedure SwitchedToPlayerApplication;dispid 6501;
    // SwitchedToControl : Sent when display switches to control 
   procedure SwitchedToControl;dispid 6502;
    // PlayerDockedStateChange : Sent when the player docks or undocks 
   procedure PlayerDockedStateChange;dispid 6503;
    // PlayerReconnect : Sent when the OCX reconnects to the player 
   procedure PlayerReconnect;dispid 6504;
    // Click : Occurs when a user clicks the mouse 
   procedure Click(nButton:Smallint;nShiftState:Smallint;fX:Integer;fY:Integer);dispid 6505;
    // DoubleClick : Occurs when a user double-clicks the mouse 
   procedure DoubleClick(nButton:Smallint;nShiftState:Smallint;fX:Integer;fY:Integer);dispid 6506;
    // KeyDown : Occurs when a key is pressed 
   procedure KeyDown(nKeyCode:Smallint;nShiftState:Smallint);dispid 6507;
    // KeyPress : Occurs when a key is pressed and released 
   procedure KeyPress(nKeyAscii:Smallint);dispid 6508;
    // KeyUp : Occurs when a key is released 
   procedure KeyUp(nKeyCode:Smallint;nShiftState:Smallint);dispid 6509;
    // MouseDown : Occurs when a mouse button is pressed 
   procedure MouseDown(nButton:Smallint;nShiftState:Smallint;fX:Integer;fY:Integer);dispid 6510;
    // MouseMove : Occurs when a mouse pointer is moved 
   procedure MouseMove(nButton:Smallint;nShiftState:Smallint;fX:Integer;fY:Integer);dispid 6511;
    // MouseUp : Occurs when a mouse button is released 
   procedure MouseUp(nButton:Smallint;nShiftState:Smallint;fX:Integer;fY:Integer);dispid 6512;
    // DeviceConnect : Occurs when a device is connected 
   procedure DeviceConnect(pDevice:IWMPSyncDevice);dispid 6513;
    // DeviceDisconnect : Occurs when a device is disconnected 
   procedure DeviceDisconnect(pDevice:IWMPSyncDevice);dispid 6514;
    // DeviceStatusChange : Occurs when a device status changes 
   procedure DeviceStatusChange(pDevice:IWMPSyncDevice;NewStatus:WMPDeviceStatus);dispid 6515;
    // DeviceSyncStateChange : Occurs when a device sync state changes 
   procedure DeviceSyncStateChange(pDevice:IWMPSyncDevice;NewState:WMPSyncState);dispid 6516;
    // DeviceSyncError : Occurs when a device's media has an error 
   procedure DeviceSyncError(pDevice:IWMPSyncDevice;pMedia:IDispatch);dispid 6517;
    // CreatePartnershipComplete : Occurs when createPartnership call completes 
   procedure CreatePartnershipComplete(pDevice:IWMPSyncDevice;hrResult:HResult);dispid 6518;
    // DeviceEstimation : Occurs when the sync estimation completed 
   procedure DeviceEstimation(pDevice:IWMPSyncDevice;hrResult:HResult;qwEstimatedUsedSpace:Int64;qwEstimatedSpace:Int64);dispid 6527;
    // CdromRipStateChange : Occurs when ripping state changes 
   procedure CdromRipStateChange(pCdromRip:IWMPCdromRip;wmprs:WMPRipState);dispid 6519;
    // CdromRipMediaError : Occurs when an error happens while ripping a media 
   procedure CdromRipMediaError(pCdromRip:IWMPCdromRip;pMedia:IDispatch);dispid 6520;
    // CdromBurnStateChange : Occurs when burning state changes 
   procedure CdromBurnStateChange(pCdromBurn:IWMPCdromBurn;wmpbs:WMPBurnState);dispid 6521;
    // CdromBurnMediaError : Occurs when an error happens while burning a media 
   procedure CdromBurnMediaError(pCdromBurn:IWMPCdromBurn;pMedia:IDispatch);dispid 6522;
    // CdromBurnError : Occurs when a generic error happens while burning 
   procedure CdromBurnError(pCdromBurn:IWMPCdromBurn;hrError:HResult);dispid 6523;
    // LibraryConnect : Occurs when a library is connected 
   procedure LibraryConnect(pLibrary:IWMPLibrary);dispid 6524;
    // LibraryDisconnect : Occurs when a library is disconnected 
   procedure LibraryDisconnect(pLibrary:IWMPLibrary);dispid 6525;
    // FolderScanStateChange : Occurs when a folder scan state changes 
   procedure FolderScanStateChange(wmpfss:WMPFolderScanState);dispid 6526;
    // StringCollectionChange : Sent when a string collection changes 
   procedure StringCollectionChange(pdispStringCollection:IDispatch;change:WMPStringCollectionChangeEventType;lCollectionIndex:Integer);dispid 5824;
    // MediaCollectionMediaAdded : Sent when a media is added to the local library 
   procedure MediaCollectionMediaAdded(pdispMedia:IDispatch);dispid 5825;
    // MediaCollectionMediaRemoved : Sent when a media is removed from the local library 
   procedure MediaCollectionMediaRemoved(pdispMedia:IDispatch);dispid 5826;
  end;


// IWMPCore : IWMPCore: Public interface.

 IWMPCore = interface(IDispatch)
   ['{D84CCA99-CCE2-11D2-9ECC-0000F8085981}']
    // close : Closes the media 
   procedure close;safecall;
   function Get_URL : WideString; safecall;
   procedure Set_URL(const pbstrURL:WideString); safecall;
   function Get_openState : WMPOpenState; safecall;
   function Get_playState : WMPPlayState; safecall;
   function Get_controls : IWMPControls; safecall;
   function Get_settings : IWMPSettings; safecall;
   function Get_currentMedia : IWMPMedia; safecall;
   procedure Set_currentMedia(const ppMedia:IWMPMedia); safecall;
   function Get_mediaCollection : IWMPMediaCollection; safecall;
   function Get_playlistCollection : IWMPPlaylistCollection; safecall;
   function Get_versionInfo : WideString; safecall;
    // launchURL :  
   procedure launchURL(bstrURL:WideString);safecall;
   function Get_network : IWMPNetwork; safecall;
   function Get_currentPlaylist : IWMPPlaylist; safecall;
   procedure Set_currentPlaylist(const ppPL:IWMPPlaylist); safecall;
   function Get_cdromCollection : IWMPCdromCollection; safecall;
   function Get_closedCaption : IWMPClosedCaption; safecall;
   function Get_isOnline : WordBool; safecall;
   function Get_Error : IWMPError; safecall;
   function Get_status : WideString; safecall;
    // URL : Returns or sets the URL 
   property URL:WideString read Get_URL write Set_URL;
    // openState : Returns the open state of the player 
   property openState:WMPOpenState read Get_openState;
    // playState : Returns the play state of the player 
   property playState:WMPPlayState read Get_playState;
    // controls : Returns the control handler 
   property controls:IWMPControls read Get_controls;
    // settings : Returns the settings handler 
   property settings:IWMPSettings read Get_settings;
    // currentMedia : Returns or sets the current media object 
   property currentMedia:IWMPMedia read Get_currentMedia write Set_currentMedia;
    // mediaCollection : Returns the media collection handler 
   property mediaCollection:IWMPMediaCollection read Get_mediaCollection;
    // playlistCollection : Returns the playlist collection handler 
   property playlistCollection:IWMPPlaylistCollection read Get_playlistCollection;
    // versionInfo : Returns the version information for the player 
   property versionInfo:WideString read Get_versionInfo;
    // network : Returns the network information handler 
   property network:IWMPNetwork read Get_network;
    // currentPlaylist : Returns/sets the current playlist 
   property currentPlaylist:IWMPPlaylist read Get_currentPlaylist write Set_currentPlaylist;
    // cdromCollection : Get the CDROM drive collection 
   property cdromCollection:IWMPCdromCollection read Get_cdromCollection;
    // closedCaption : Returns the closed caption handler 
   property closedCaption:IWMPClosedCaption read Get_closedCaption;
    // isOnline : Returns whether the machine is online. 
   property isOnline:WordBool read Get_isOnline;
    // Error : Returns the error object 
   property Error:IWMPError read Get_Error;
    // status : Returns status string 
   property status:WideString read Get_status;
  end;


// IWMPCore : IWMPCore: Public interface.

 IWMPCoreDisp = dispinterface
   ['{D84CCA99-CCE2-11D2-9ECC-0000F8085981}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // close : Closes the media 
   procedure close;dispid 3;
    // launchURL :  
   procedure launchURL(bstrURL:WideString);dispid 12;
    // URL : Returns or sets the URL 
   property URL:WideString dispid 1;
    // openState : Returns the open state of the player 
   property openState:WMPOpenState  readonly dispid 2;
    // playState : Returns the play state of the player 
   property playState:WMPPlayState  readonly dispid 10;
    // controls : Returns the control handler 
   property controls:IWMPControls  readonly dispid 4;
    // settings : Returns the settings handler 
   property settings:IWMPSettings  readonly dispid 5;
    // currentMedia : Returns or sets the current media object 
   property currentMedia:IWMPMedia dispid 6;
    // mediaCollection : Returns the media collection handler 
   property mediaCollection:IWMPMediaCollection  readonly dispid 8;
    // playlistCollection : Returns the playlist collection handler 
   property playlistCollection:IWMPPlaylistCollection  readonly dispid 9;
    // versionInfo : Returns the version information for the player 
   property versionInfo:WideString  readonly dispid 11;
    // network : Returns the network information handler 
   property network:IWMPNetwork  readonly dispid 7;
    // currentPlaylist : Returns/sets the current playlist 
   property currentPlaylist:IWMPPlaylist dispid 13;
    // cdromCollection : Get the CDROM drive collection 
   property cdromCollection:IWMPCdromCollection  readonly dispid 14;
    // closedCaption : Returns the closed caption handler 
   property closedCaption:IWMPClosedCaption  readonly dispid 15;
    // isOnline : Returns whether the machine is online. 
   property isOnline:WordBool  readonly dispid 16;
    // Error : Returns the error object 
   property Error:IWMPError  readonly dispid 17;
    // status : Returns status string 
   property status:WideString  readonly dispid 18;
  end;


// IWMPCore2 : IWMPCore2: Public interface.

 IWMPCore2 = interface(IWMPCore)
   ['{BC17E5B7-7561-4C18-BB90-17D485775659}']
   function Get_dvd : IWMPDVD; safecall;
    // dvd : Returns the DVD handler 
   property dvd:IWMPDVD read Get_dvd;
  end;

// IWMPCore2 : IWMPCore2: Public interface.

 IWMPCore2Disp = dispinterface
   ['{BC17E5B7-7561-4C18-BB90-17D485775659}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // close : Closes the media 
   procedure close;dispid 3;
    // launchURL :  
   procedure launchURL(bstrURL:WideString);dispid 12;
    // URL : Returns or sets the URL 
   property URL:WideString dispid 1;
    // openState : Returns the open state of the player 
   property openState:WMPOpenState  readonly dispid 2;
    // playState : Returns the play state of the player 
   property playState:WMPPlayState  readonly dispid 10;
    // controls : Returns the control handler 
   property controls:IWMPControls  readonly dispid 4;
    // settings : Returns the settings handler 
   property settings:IWMPSettings  readonly dispid 5;
    // currentMedia : Returns or sets the current media object 
   property currentMedia:IWMPMedia dispid 6;
    // mediaCollection : Returns the media collection handler 
   property mediaCollection:IWMPMediaCollection  readonly dispid 8;
    // playlistCollection : Returns the playlist collection handler 
   property playlistCollection:IWMPPlaylistCollection  readonly dispid 9;
    // versionInfo : Returns the version information for the player 
   property versionInfo:WideString  readonly dispid 11;
    // network : Returns the network information handler 
   property network:IWMPNetwork  readonly dispid 7;
    // currentPlaylist : Returns/sets the current playlist 
   property currentPlaylist:IWMPPlaylist dispid 13;
    // cdromCollection : Get the CDROM drive collection 
   property cdromCollection:IWMPCdromCollection  readonly dispid 14;
    // closedCaption : Returns the closed caption handler 
   property closedCaption:IWMPClosedCaption  readonly dispid 15;
    // isOnline : Returns whether the machine is online. 
   property isOnline:WordBool  readonly dispid 16;
    // Error : Returns the error object 
   property Error:IWMPError  readonly dispid 17;
    // status : Returns status string 
   property status:WideString  readonly dispid 18;
    // dvd : Returns the DVD handler 
   property dvd:IWMPDVD  readonly dispid 40;
  end;


// IWMPCore3 : IWMPCore3: Public interface.

 IWMPCore3 = interface(IWMPCore2)
   ['{7587C667-628F-499F-88E7-6A6F4E888464}']
    // newPlaylist : Creates a new playlist object 
   function newPlaylist(bstrName:WideString;bstrURL:WideString):IWMPPlaylist;safecall;
    // newMedia : Creates a new media object 
   function newMedia(bstrURL:WideString):IWMPMedia;safecall;
  end;

// IWMPCore3 : IWMPCore3: Public interface.

 IWMPCore3Disp = dispinterface
   ['{7587C667-628F-499F-88E7-6A6F4E888464}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // close : Closes the media 
   procedure close;dispid 3;
    // launchURL :  
   procedure launchURL(bstrURL:WideString);dispid 12;
    // newPlaylist : Creates a new playlist object 
   function newPlaylist(bstrName:WideString;bstrURL:WideString):IWMPPlaylist;dispid 41;
    // newMedia : Creates a new media object 
   function newMedia(bstrURL:WideString):IWMPMedia;dispid 42;
    // URL : Returns or sets the URL 
   property URL:WideString dispid 1;
    // openState : Returns the open state of the player 
   property openState:WMPOpenState  readonly dispid 2;
    // playState : Returns the play state of the player 
   property playState:WMPPlayState  readonly dispid 10;
    // controls : Returns the control handler 
   property controls:IWMPControls  readonly dispid 4;
    // settings : Returns the settings handler 
   property settings:IWMPSettings  readonly dispid 5;
    // currentMedia : Returns or sets the current media object 
   property currentMedia:IWMPMedia dispid 6;
    // mediaCollection : Returns the media collection handler 
   property mediaCollection:IWMPMediaCollection  readonly dispid 8;
    // playlistCollection : Returns the playlist collection handler 
   property playlistCollection:IWMPPlaylistCollection  readonly dispid 9;
    // versionInfo : Returns the version information for the player 
   property versionInfo:WideString  readonly dispid 11;
    // network : Returns the network information handler 
   property network:IWMPNetwork  readonly dispid 7;
    // currentPlaylist : Returns/sets the current playlist 
   property currentPlaylist:IWMPPlaylist dispid 13;
    // cdromCollection : Get the CDROM drive collection 
   property cdromCollection:IWMPCdromCollection  readonly dispid 14;
    // closedCaption : Returns the closed caption handler 
   property closedCaption:IWMPClosedCaption  readonly dispid 15;
    // isOnline : Returns whether the machine is online. 
   property isOnline:WordBool  readonly dispid 16;
    // Error : Returns the error object 
   property Error:IWMPError  readonly dispid 17;
    // status : Returns status string 
   property status:WideString  readonly dispid 18;
    // dvd : Returns the DVD handler 
   property dvd:IWMPDVD  readonly dispid 40;
  end;


// IWMPPlayer4 : IWMPPlayer4: Public interface.

 IWMPPlayer4 = interface(IWMPCore3)
   ['{6C497D62-8919-413C-82DB-E935FB3EC584}']
   function Get_enabled : WordBool; safecall;
   procedure Set_enabled(const pbEnabled:WordBool); safecall;
   function Get_fullScreen : WordBool; safecall;
   procedure Set_fullScreen(const pbFullScreen:WordBool); safecall;
   function Get_enableContextMenu : WordBool; safecall;
   procedure Set_enableContextMenu(const pbEnableContextMenu:WordBool); safecall;
   procedure Set_uiMode(const pbstrMode:WideString); safecall;
   function Get_uiMode : WideString; safecall;
   function Get_stretchToFit : WordBool; safecall;
   procedure Set_stretchToFit(const pbEnabled:WordBool); safecall;
   function Get_windowlessVideo : WordBool; safecall;
   procedure Set_windowlessVideo(const pbEnabled:WordBool); safecall;
   function Get_isRemote : WordBool; safecall;
   function Get_playerApplication : IWMPPlayerApplication; safecall;
    // openPlayer : Opens the player with the specified URL 
   procedure openPlayer(bstrURL:WideString);safecall;
    // enabled : Returns a boolean value specifying whether or not the control is enabled 
   property enabled:WordBool read Get_enabled write Set_enabled;
    // fullScreen : Returns a boolean value specifying whether or not the control is in full screen mode 
   property fullScreen:WordBool read Get_fullScreen write Set_fullScreen;
    // enableContextMenu : Returns a boolean value specifying whether or not the context menu is enabled on the control 
   property enableContextMenu:WordBool read Get_enableContextMenu write Set_enableContextMenu;
    // uiMode : Specifies the ui mode to select 
   property uiMode:WideString read Get_uiMode write Set_uiMode;
    // stretchToFit : Returns a boolean value specifying whether or not video is stretched 
   property stretchToFit:WordBool read Get_stretchToFit write Set_stretchToFit;
    // windowlessVideo : Returns a boolean value specifying whether or not video is windowless 
   property windowlessVideo:WordBool read Get_windowlessVideo write Set_windowlessVideo;
    // isRemote : Indicates whether the player is running remotely 
   property isRemote:WordBool read Get_isRemote;
    // playerApplication : Returns the player application handler 
   property playerApplication:IWMPPlayerApplication read Get_playerApplication;
  end;

// IWMPPlayer4 : IWMPPlayer4: Public interface.

 IWMPPlayer4Disp = dispinterface
   ['{6C497D62-8919-413C-82DB-E935FB3EC584}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // close : Closes the media 
   procedure close;dispid 3;
    // launchURL :  
   procedure launchURL(bstrURL:WideString);dispid 12;
    // newPlaylist : Creates a new playlist object 
   function newPlaylist(bstrName:WideString;bstrURL:WideString):IWMPPlaylist;dispid 41;
    // newMedia : Creates a new media object 
   function newMedia(bstrURL:WideString):IWMPMedia;dispid 42;
    // openPlayer : Opens the player with the specified URL 
   procedure openPlayer(bstrURL:WideString);dispid 28;
    // URL : Returns or sets the URL 
   property URL:WideString dispid 1;
    // openState : Returns the open state of the player 
   property openState:WMPOpenState  readonly dispid 2;
    // playState : Returns the play state of the player 
   property playState:WMPPlayState  readonly dispid 10;
    // controls : Returns the control handler 
   property controls:IWMPControls  readonly dispid 4;
    // settings : Returns the settings handler 
   property settings:IWMPSettings  readonly dispid 5;
    // currentMedia : Returns or sets the current media object 
   property currentMedia:IWMPMedia dispid 6;
    // mediaCollection : Returns the media collection handler 
   property mediaCollection:IWMPMediaCollection  readonly dispid 8;
    // playlistCollection : Returns the playlist collection handler 
   property playlistCollection:IWMPPlaylistCollection  readonly dispid 9;
    // versionInfo : Returns the version information for the player 
   property versionInfo:WideString  readonly dispid 11;
    // network : Returns the network information handler 
   property network:IWMPNetwork  readonly dispid 7;
    // currentPlaylist : Returns/sets the current playlist 
   property currentPlaylist:IWMPPlaylist dispid 13;
    // cdromCollection : Get the CDROM drive collection 
   property cdromCollection:IWMPCdromCollection  readonly dispid 14;
    // closedCaption : Returns the closed caption handler 
   property closedCaption:IWMPClosedCaption  readonly dispid 15;
    // isOnline : Returns whether the machine is online. 
   property isOnline:WordBool  readonly dispid 16;
    // Error : Returns the error object 
   property Error:IWMPError  readonly dispid 17;
    // status : Returns status string 
   property status:WideString  readonly dispid 18;
    // dvd : Returns the DVD handler 
   property dvd:IWMPDVD  readonly dispid 40;
    // enabled : Returns a boolean value specifying whether or not the control is enabled 
   property enabled:WordBool dispid 19;
    // fullScreen : Returns a boolean value specifying whether or not the control is in full screen mode 
   property fullScreen:WordBool dispid 21;
    // enableContextMenu : Returns a boolean value specifying whether or not the context menu is enabled on the control 
   property enableContextMenu:WordBool dispid 22;
    // uiMode : Specifies the ui mode to select 
   property uiMode:WideString dispid 23;
    // stretchToFit : Returns a boolean value specifying whether or not video is stretched 
   property stretchToFit:WordBool dispid 24;
    // windowlessVideo : Returns a boolean value specifying whether or not video is windowless 
   property windowlessVideo:WordBool dispid 25;
    // isRemote : Indicates whether the player is running remotely 
   property isRemote:WordBool  readonly dispid 26;
    // playerApplication : Returns the player application handler 
   property playerApplication:IWMPPlayerApplication  readonly dispid 27;
  end;


// IWMPControls : IWMPControls: Public interface.

 IWMPControls = interface(IDispatch)
   ['{74C09E02-F828-11D2-A74B-00A0C905F36E}']
   function Get_isAvailable(bstrItem:WideString) : WordBool; safecall;
    // play : Begins playing media 
   procedure play;safecall;
    // stop : Stops play of media 
   procedure stop;safecall;
    // pause : Pauses play of media 
   procedure pause;safecall;
    // fastForward : Fast play of media in forward direction 
   procedure fastForward;safecall;
    // fastReverse : Fast play of media in reverse direction 
   procedure fastReverse;safecall;
   function Get_currentPosition : Double; safecall;
   procedure Set_currentPosition(const pdCurrentPosition:Double); safecall;
   function Get_currentPositionString : WideString; safecall;
    // next : Sets the current item to the next item in the playlist 
   procedure next;safecall;
    // previous : Sets the current item to the previous item in the playlist 
   procedure previous;safecall;
   function Get_currentItem : IWMPMedia; safecall;
   procedure Set_currentItem(const ppIWMPMedia:IWMPMedia); safecall;
   function Get_currentMarker : Integer; safecall;
   procedure Set_currentMarker(const plMarker:Integer); safecall;
    // playItem : Sets the current item and plays it 
   procedure playItem(pIWMPMedia:IWMPMedia);safecall;
    // isAvailable : Returns whether or not the specified media functionality is available 
   property isAvailable[bstrItem:WideString]:WordBool read Get_isAvailable;
    // currentPosition : Returns the current position in media 
   property currentPosition:Double read Get_currentPosition write Set_currentPosition;
    // currentPositionString : Returns the current position in media as a string 
   property currentPositionString:WideString read Get_currentPositionString;
    // currentItem : Returns/Sets the play item 
   property currentItem:IWMPMedia read Get_currentItem write Set_currentItem;
    // currentMarker : Returns the current marker 
   property currentMarker:Integer read Get_currentMarker write Set_currentMarker;
  end;


// IWMPControls : IWMPControls: Public interface.

 IWMPControlsDisp = dispinterface
   ['{74C09E02-F828-11D2-A74B-00A0C905F36E}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // play : Begins playing media 
   procedure play;dispid 51;
    // stop : Stops play of media 
   procedure stop;dispid 52;
    // pause : Pauses play of media 
   procedure pause;dispid 53;
    // fastForward : Fast play of media in forward direction 
   procedure fastForward;dispid 54;
    // fastReverse : Fast play of media in reverse direction 
   procedure fastReverse;dispid 55;
    // next : Sets the current item to the next item in the playlist 
   procedure next;dispid 58;
    // previous : Sets the current item to the previous item in the playlist 
   procedure previous;dispid 59;
    // playItem : Sets the current item and plays it 
   procedure playItem(pIWMPMedia:IWMPMedia);dispid 63;
    // isAvailable : Returns whether or not the specified media functionality is available 
   property isAvailable[bstrItem:WideString]:WordBool  readonly dispid 62;
    // currentPosition : Returns the current position in media 
   property currentPosition:Double dispid 56;
    // currentPositionString : Returns the current position in media as a string 
   property currentPositionString:WideString  readonly dispid 57;
    // currentItem : Returns/Sets the play item 
   property currentItem:IWMPMedia dispid 60;
    // currentMarker : Returns the current marker 
   property currentMarker:Integer dispid 61;
  end;


// IWMPSettings : IWMPSettings: Public interface.

 IWMPSettings = interface(IDispatch)
   ['{9104D1AB-80C9-4FED-ABF0-2E6417A6DF14}']
   function Get_isAvailable(bstrItem:WideString) : WordBool; safecall;
   function Get_autoStart : WordBool; safecall;
   procedure Set_autoStart(const pfAutoStart:WordBool); safecall;
   function Get_baseURL : WideString; safecall;
   procedure Set_baseURL(const pbstrBaseURL:WideString); safecall;
   function Get_defaultFrame : WideString; safecall;
   procedure Set_defaultFrame(const pbstrDefaultFrame:WideString); safecall;
   function Get_invokeURLs : WordBool; safecall;
   procedure Set_invokeURLs(const pfInvokeURLs:WordBool); safecall;
   function Get_mute : WordBool; safecall;
   procedure Set_mute(const pfMute:WordBool); safecall;
   function Get_playCount : Integer; safecall;
   procedure Set_playCount(const plCount:Integer); safecall;
   function Get_rate : Double; safecall;
   procedure Set_rate(const pdRate:Double); safecall;
   function Get_balance : Integer; safecall;
   procedure Set_balance(const plBalance:Integer); safecall;
   function Get_volume : Integer; safecall;
   procedure Set_volume(const plVolume:Integer); safecall;
    // getMode : Returns the mode of the playlist 
   function getMode(bstrMode:WideString):WordBool;safecall;
    // setMode : Sets the mode of the playlist 
   procedure setMode(bstrMode:WideString;varfMode:WordBool);safecall;
   function Get_enableErrorDialogs : WordBool; safecall;
   procedure Set_enableErrorDialogs(const pfEnableErrorDialogs:WordBool); safecall;
    // isAvailable : Returns whether or not the specified media functionality is available 
   property isAvailable[bstrItem:WideString]:WordBool read Get_isAvailable;
    // autoStart : Returns whether media should automatically begin playing 
   property autoStart:WordBool read Get_autoStart write Set_autoStart;
    // baseURL : Returns the base URL used for relative path resolution 
   property baseURL:WideString read Get_baseURL write Set_baseURL;
    // defaultFrame : Returns the frame location that changes when a URL flip occurs 
   property defaultFrame:WideString read Get_defaultFrame write Set_defaultFrame;
    // invokeURLs : Returns whether URL events should spawn a browser. 
   property invokeURLs:WordBool read Get_invokeURLs write Set_invokeURLs;
    // mute : Returns whether audio should be muted. 
   property mute:WordBool read Get_mute write Set_mute;
    // playCount : Returns how many times media should play 
   property playCount:Integer read Get_playCount write Set_playCount;
    // rate : Returns current playback rate 
   property rate:Double read Get_rate write Set_rate;
    // balance : Returns current audio Balance 
   property balance:Integer read Get_balance write Set_balance;
    // volume : Returns current audio volume 
   property volume:Integer read Get_volume write Set_volume;
    // enableErrorDialogs : Returns whether error dialogs are shown by default when embedded 
   property enableErrorDialogs:WordBool read Get_enableErrorDialogs write Set_enableErrorDialogs;
  end;


// IWMPSettings : IWMPSettings: Public interface.

 IWMPSettingsDisp = dispinterface
   ['{9104D1AB-80C9-4FED-ABF0-2E6417A6DF14}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // getMode : Returns the mode of the playlist 
   function getMode(bstrMode:WideString):WordBool;dispid 110;
    // setMode : Sets the mode of the playlist 
   procedure setMode(bstrMode:WideString;varfMode:WordBool);dispid 111;
    // isAvailable : Returns whether or not the specified media functionality is available 
   property isAvailable[bstrItem:WideString]:WordBool  readonly dispid 113;
    // autoStart : Returns whether media should automatically begin playing 
   property autoStart:WordBool dispid 101;
    // baseURL : Returns the base URL used for relative path resolution 
   property baseURL:WideString dispid 108;
    // defaultFrame : Returns the frame location that changes when a URL flip occurs 
   property defaultFrame:WideString dispid 109;
    // invokeURLs : Returns whether URL events should spawn a browser. 
   property invokeURLs:WordBool dispid 103;
    // mute : Returns whether audio should be muted. 
   property mute:WordBool dispid 104;
    // playCount : Returns how many times media should play 
   property playCount:Integer dispid 105;
    // rate : Returns current playback rate 
   property rate:Double dispid 106;
    // balance : Returns current audio Balance 
   property balance:Integer dispid 102;
    // volume : Returns current audio volume 
   property volume:Integer dispid 107;
    // enableErrorDialogs : Returns whether error dialogs are shown by default when embedded 
   property enableErrorDialogs:WordBool dispid 112;
  end;


// IWMPPlaylistCollection : IWMPPlaylistCollection: Public interface.

 IWMPPlaylistCollection = interface(IDispatch)
   ['{10A13217-23A7-439B-B1C0-D847C79B7774}']
    // newPlaylist : Creates a new playlist object 
   function newPlaylist(bstrName:WideString):IWMPPlaylist;safecall;
    // getAll : Returns a playlist array with all the playlists 
   function getAll:IWMPPlaylistArray;safecall;
    // getByName : Returns a playlist array with playlists matching the given name 
   function getByName(bstrName:WideString):IWMPPlaylistArray;safecall;
    // remove : Removes an item from the playlist collection 
   procedure remove(pItem:IWMPPlaylist);safecall;
    // setDeleted : Sets the deleted flag on a playlist object 
   procedure setDeleted(pItem:IWMPPlaylist;varfIsDeleted:WordBool);safecall;
    // isDeleted : Gets the deleted flag on a playlist object 
   function isDeleted(pItem:IWMPPlaylist):WordBool;safecall;
    // importPlaylist : Imports a playlist object into the library 
   function importPlaylist(pItem:IWMPPlaylist):IWMPPlaylist;safecall;
  end;


// IWMPPlaylistCollection : IWMPPlaylistCollection: Public interface.

 IWMPPlaylistCollectionDisp = dispinterface
   ['{10A13217-23A7-439B-B1C0-D847C79B7774}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // newPlaylist : Creates a new playlist object 
   function newPlaylist(bstrName:WideString):IWMPPlaylist;dispid 552;
    // getAll : Returns a playlist array with all the playlists 
   function getAll:IWMPPlaylistArray;dispid 553;
    // getByName : Returns a playlist array with playlists matching the given name 
   function getByName(bstrName:WideString):IWMPPlaylistArray;dispid 554;
    // remove : Removes an item from the playlist collection 
   procedure remove(pItem:IWMPPlaylist);dispid 556;
    // setDeleted : Sets the deleted flag on a playlist object 
   procedure setDeleted(pItem:IWMPPlaylist;varfIsDeleted:WordBool);dispid 560;
    // isDeleted : Gets the deleted flag on a playlist object 
   function isDeleted(pItem:IWMPPlaylist):WordBool;dispid 561;
    // importPlaylist : Imports a playlist object into the library 
   function importPlaylist(pItem:IWMPPlaylist):IWMPPlaylist;dispid 562;
  end;


// IWMPPlaylistArray : IWMPPlaylistArray: Public interface.

 IWMPPlaylistArray = interface(IDispatch)
   ['{679409C0-99F7-11D3-9FB7-00105AA620BB}']
   function Get_count : Integer; safecall;
    // Item : Returns the playlist object at the given index 
   function Item(lIndex:Integer):IWMPPlaylist;safecall;
    // count : Returns the number of items in the playlist array 
   property count:Integer read Get_count;
  end;


// IWMPPlaylistArray : IWMPPlaylistArray: Public interface.

 IWMPPlaylistArrayDisp = dispinterface
   ['{679409C0-99F7-11D3-9FB7-00105AA620BB}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // Item : Returns the playlist object at the given index 
   function Item(lIndex:Integer):IWMPPlaylist;dispid 502;
    // count : Returns the number of items in the playlist array 
   property count:Integer  readonly dispid 501;
  end;


// IWMPNetwork : IWMPNetwork: Public interface.

 IWMPNetwork = interface(IDispatch)
   ['{EC21B779-EDEF-462D-BBA4-AD9DDE2B29A7}']
   function Get_bandWidth : Integer; safecall;
   function Get_recoveredPackets : Integer; safecall;
   function Get_sourceProtocol : WideString; safecall;
   function Get_receivedPackets : Integer; safecall;
   function Get_lostPackets : Integer; safecall;
   function Get_receptionQuality : Integer; safecall;
   function Get_bufferingCount : Integer; safecall;
   function Get_bufferingProgress : Integer; safecall;
   function Get_bufferingTime : Integer; safecall;
   procedure Set_bufferingTime(const plBufferingTime:Integer); safecall;
   function Get_frameRate : Integer; safecall;
   function Get_maxBitRate : Integer; safecall;
   function Get_bitRate : Integer; safecall;
    // getProxySettings : Returns the proxy settings for the specified protocol 
   function getProxySettings(bstrProtocol:WideString):Integer;safecall;
    // setProxySettings : Sets the proxy settings for the specified protocol 
   procedure setProxySettings(bstrProtocol:WideString;lProxySetting:Integer);safecall;
    // getProxyName : Returns the proxy name for the specified protocol 
   function getProxyName(bstrProtocol:WideString):WideString;safecall;
    // setProxyName : Sets the proxy name for the specified protocol 
   procedure setProxyName(bstrProtocol:WideString;bstrProxyName:WideString);safecall;
    // getProxyPort : Returns the proxy port for the specified protocol 
   function getProxyPort(bstrProtocol:WideString):Integer;safecall;
    // setProxyPort : Sets the proxy port for the specified protocol 
   procedure setProxyPort(bstrProtocol:WideString;lProxyPort:Integer);safecall;
    // getProxyExceptionList : Returns the proxy exception list for the specified protocol 
   function getProxyExceptionList(bstrProtocol:WideString):WideString;safecall;
    // setProxyExceptionList : Sets the proxy exception list for the specified protocol 
   procedure setProxyExceptionList(bstrProtocol:WideString;pbstrExceptionList:WideString);safecall;
    // getProxyBypassForLocal : Returns whether or not to bypass the proxy for local addresses 
   function getProxyBypassForLocal(bstrProtocol:WideString):WordBool;safecall;
    // setProxyBypassForLocal : Sets whether or not to by pass the proxy for local addresses 
   procedure setProxyBypassForLocal(bstrProtocol:WideString;fBypassForLocal:WordBool);safecall;
   function Get_maxBandwidth : Integer; safecall;
   procedure Set_maxBandwidth(const lMaxBandwidth:Integer); safecall;
   function Get_downloadProgress : Integer; safecall;
   function Get_encodedFrameRate : Integer; safecall;
   function Get_framesSkipped : Integer; safecall;
    // bandWidth : Returns the current bandwidth of the clip. 
   property bandWidth:Integer read Get_bandWidth;
    // recoveredPackets : Returns the number of recovered packets 
   property recoveredPackets:Integer read Get_recoveredPackets;
    // sourceProtocol : Returns the source protocol used to receive data. 
   property sourceProtocol:WideString read Get_sourceProtocol;
    // receivedPackets : Returns the number of packets received. 
   property receivedPackets:Integer read Get_receivedPackets;
    // lostPackets : Returns the number of packets lost. 
   property lostPackets:Integer read Get_lostPackets;
    // receptionQuality : Returns the percentage of packets received in the last 15 seconds. 
   property receptionQuality:Integer read Get_receptionQuality;
    // bufferingCount : Returns the number of times buffering occurred during clip playback. 
   property bufferingCount:Integer read Get_bufferingCount;
    // bufferingProgress : Returns the percentage of buffering completed. 
   property bufferingProgress:Integer read Get_bufferingProgress;
    // bufferingTime : Returns the number of seconds allocated for buffering for this media type. 
   property bufferingTime:Integer read Get_bufferingTime write Set_bufferingTime;
    // frameRate : Current video frame rate in frames/second 
   property frameRate:Integer read Get_frameRate;
    // maxBitRate : Maximum possible video bit rate 
   property maxBitRate:Integer read Get_maxBitRate;
    // bitRate : Current video bit rate 
   property bitRate:Integer read Get_bitRate;
    // maxBandwidth : Returns or sets maximum allowed bandwidth 
   property maxBandwidth:Integer read Get_maxBandwidth write Set_maxBandwidth;
    // downloadProgress : Returns the percentage of download completed. 
   property downloadProgress:Integer read Get_downloadProgress;
    // encodedFrameRate : Returns the video frame rate, in frames/second, that the file was encoded in 
   property encodedFrameRate:Integer read Get_encodedFrameRate;
    // framesSkipped : Returns the number of skipped frames 
   property framesSkipped:Integer read Get_framesSkipped;
  end;


// IWMPNetwork : IWMPNetwork: Public interface.

 IWMPNetworkDisp = dispinterface
   ['{EC21B779-EDEF-462D-BBA4-AD9DDE2B29A7}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // getProxySettings : Returns the proxy settings for the specified protocol 
   function getProxySettings(bstrProtocol:WideString):Integer;dispid 813;
    // setProxySettings : Sets the proxy settings for the specified protocol 
   procedure setProxySettings(bstrProtocol:WideString;lProxySetting:Integer);dispid 814;
    // getProxyName : Returns the proxy name for the specified protocol 
   function getProxyName(bstrProtocol:WideString):WideString;dispid 815;
    // setProxyName : Sets the proxy name for the specified protocol 
   procedure setProxyName(bstrProtocol:WideString;bstrProxyName:WideString);dispid 816;
    // getProxyPort : Returns the proxy port for the specified protocol 
   function getProxyPort(bstrProtocol:WideString):Integer;dispid 817;
    // setProxyPort : Sets the proxy port for the specified protocol 
   procedure setProxyPort(bstrProtocol:WideString;lProxyPort:Integer);dispid 818;
    // getProxyExceptionList : Returns the proxy exception list for the specified protocol 
   function getProxyExceptionList(bstrProtocol:WideString):WideString;dispid 819;
    // setProxyExceptionList : Sets the proxy exception list for the specified protocol 
   procedure setProxyExceptionList(bstrProtocol:WideString;pbstrExceptionList:WideString);dispid 820;
    // getProxyBypassForLocal : Returns whether or not to bypass the proxy for local addresses 
   function getProxyBypassForLocal(bstrProtocol:WideString):WordBool;dispid 821;
    // setProxyBypassForLocal : Sets whether or not to by pass the proxy for local addresses 
   procedure setProxyBypassForLocal(bstrProtocol:WideString;fBypassForLocal:WordBool);dispid 822;
    // bandWidth : Returns the current bandwidth of the clip. 
   property bandWidth:Integer  readonly dispid 801;
    // recoveredPackets : Returns the number of recovered packets 
   property recoveredPackets:Integer  readonly dispid 802;
    // sourceProtocol : Returns the source protocol used to receive data. 
   property sourceProtocol:WideString  readonly dispid 803;
    // receivedPackets : Returns the number of packets received. 
   property receivedPackets:Integer  readonly dispid 804;
    // lostPackets : Returns the number of packets lost. 
   property lostPackets:Integer  readonly dispid 805;
    // receptionQuality : Returns the percentage of packets received in the last 15 seconds. 
   property receptionQuality:Integer  readonly dispid 806;
    // bufferingCount : Returns the number of times buffering occurred during clip playback. 
   property bufferingCount:Integer  readonly dispid 807;
    // bufferingProgress : Returns the percentage of buffering completed. 
   property bufferingProgress:Integer  readonly dispid 808;
    // bufferingTime : Returns the number of seconds allocated for buffering for this media type. 
   property bufferingTime:Integer dispid 809;
    // frameRate : Current video frame rate in frames/second 
   property frameRate:Integer  readonly dispid 810;
    // maxBitRate : Maximum possible video bit rate 
   property maxBitRate:Integer  readonly dispid 811;
    // bitRate : Current video bit rate 
   property bitRate:Integer  readonly dispid 812;
    // maxBandwidth : Returns or sets maximum allowed bandwidth 
   property maxBandwidth:Integer dispid 823;
    // downloadProgress : Returns the percentage of download completed. 
   property downloadProgress:Integer  readonly dispid 824;
    // encodedFrameRate : Returns the video frame rate, in frames/second, that the file was encoded in 
   property encodedFrameRate:Integer  readonly dispid 825;
    // framesSkipped : Returns the number of skipped frames 
   property framesSkipped:Integer  readonly dispid 826;
  end;


// IWMPCdromCollection : IWMPCdromCollection: Public interface.

 IWMPCdromCollection = interface(IDispatch)
   ['{EE4C8FE2-34B2-11D3-A3BF-006097C9B344}']
   function Get_count : Integer; safecall;
    // Item : Returns the CDROM object at the given index 
   function Item(lIndex:Integer):IWMPCdrom;safecall;
    // getByDriveSpecifier : Returns the CDROM object associated with a particular drive specifier, e.g. F: 
   function getByDriveSpecifier(bstrDriveSpecifier:WideString):IWMPCdrom;safecall;
    // count : Returns the number of items in the cdrom collection 
   property count:Integer read Get_count;
  end;


// IWMPCdromCollection : IWMPCdromCollection: Public interface.

 IWMPCdromCollectionDisp = dispinterface
   ['{EE4C8FE2-34B2-11D3-A3BF-006097C9B344}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // Item : Returns the CDROM object at the given index 
   function Item(lIndex:Integer):IWMPCdrom;dispid 302;
    // getByDriveSpecifier : Returns the CDROM object associated with a particular drive specifier, e.g. F: 
   function getByDriveSpecifier(bstrDriveSpecifier:WideString):IWMPCdrom;dispid 303;
    // count : Returns the number of items in the cdrom collection 
   property count:Integer  readonly dispid 301;
  end;


// IWMPCdrom : IWMPCdrom: Public interface.

 IWMPCdrom = interface(IDispatch)
   ['{CFAB6E98-8730-11D3-B388-00C04F68574B}']
   function Get_driveSpecifier : WideString; safecall;
   function Get_Playlist : IWMPPlaylist; safecall;
    // eject : Eject the CD in the CDROM drive 
   procedure eject;safecall;
    // driveSpecifier : Returns the CDROM drive specifier 
   property driveSpecifier:WideString read Get_driveSpecifier;
    // Playlist : Returns the playlist of tracks currently in the CDROM drive 
   property Playlist:IWMPPlaylist read Get_Playlist;
  end;


// IWMPCdrom : IWMPCdrom: Public interface.

 IWMPCdromDisp = dispinterface
   ['{CFAB6E98-8730-11D3-B388-00C04F68574B}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // eject : Eject the CD in the CDROM drive 
   procedure eject;dispid 253;
    // driveSpecifier : Returns the CDROM drive specifier 
   property driveSpecifier:WideString  readonly dispid 251;
    // Playlist : Returns the playlist of tracks currently in the CDROM drive 
   property Playlist:IWMPPlaylist  readonly dispid 252;
  end;


// IWMPClosedCaption : IWMPClosedCaption: Public interface.

 IWMPClosedCaption = interface(IDispatch)
   ['{4F2DF574-C588-11D3-9ED0-00C04FB6E937}']
   function Get_SAMIStyle : WideString; safecall;
   procedure Set_SAMIStyle(const pbstrSAMIStyle:WideString); safecall;
   function Get_SAMILang : WideString; safecall;
   procedure Set_SAMILang(const pbstrSAMILang:WideString); safecall;
   function Get_SAMIFileName : WideString; safecall;
   procedure Set_SAMIFileName(const pbstrSAMIFileName:WideString); safecall;
   function Get_captioningId : WideString; safecall;
   procedure Set_captioningId(const pbstrCaptioningID:WideString); safecall;
    // SAMIStyle : Returns the previously set SAMI style 
   property SAMIStyle:WideString read Get_SAMIStyle write Set_SAMIStyle;
    // SAMILang : Returns the previously set SAMI language 
   property SAMILang:WideString read Get_SAMILang write Set_SAMILang;
    // SAMIFileName : Returns the previously set SAMI file name 
   property SAMIFileName:WideString read Get_SAMIFileName write Set_SAMIFileName;
    // captioningId : Returns the previously set Captioning ID 
   property captioningId:WideString read Get_captioningId write Set_captioningId;
  end;


// IWMPClosedCaption : IWMPClosedCaption: Public interface.

 IWMPClosedCaptionDisp = dispinterface
   ['{4F2DF574-C588-11D3-9ED0-00C04FB6E937}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // SAMIStyle : Returns the previously set SAMI style 
   property SAMIStyle:WideString dispid 951;
    // SAMILang : Returns the previously set SAMI language 
   property SAMILang:WideString dispid 952;
    // SAMIFileName : Returns the previously set SAMI file name 
   property SAMIFileName:WideString dispid 953;
    // captioningId : Returns the previously set Captioning ID 
   property captioningId:WideString dispid 954;
  end;


// IWMPError : IWMPError: Public interface.

 IWMPError = interface(IDispatch)
   ['{A12DCF7D-14AB-4C1B-A8CD-63909F06025B}']
    // clearErrorQueue : Clears the error queue 
   procedure clearErrorQueue;safecall;
   function Get_errorCount : Integer; safecall;
   function Get_Item(dwIndex:Integer) : IWMPErrorItem; safecall;
    // webHelp : Launches WebHelp 
   procedure webHelp;safecall;
    // errorCount : Returns the number of error items 
   property errorCount:Integer read Get_errorCount;
    // Item : Returns an error item object 
   property Item[dwIndex:Integer]:IWMPErrorItem read Get_Item;
  end;


// IWMPError : IWMPError: Public interface.

 IWMPErrorDisp = dispinterface
   ['{A12DCF7D-14AB-4C1B-A8CD-63909F06025B}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // clearErrorQueue : Clears the error queue 
   procedure clearErrorQueue;dispid 851;
    // webHelp : Launches WebHelp 
   procedure webHelp;dispid 854;
    // errorCount : Returns the number of error items 
   property errorCount:Integer  readonly dispid 852;
    // Item : Returns an error item object 
   property Item[dwIndex:Integer]:IWMPErrorItem  readonly dispid 853;
  end;


// IWMPErrorItem : IWMPErrorItem: Public interface.

 IWMPErrorItem = interface(IDispatch)
   ['{3614C646-3B3B-4DE7-A81E-930E3F2127B3}']
   function Get_errorCode : Integer; safecall;
   function Get_errorDescription : WideString; safecall;
   function Get_errorContext : OleVariant; safecall;
   function Get_remedy : Integer; safecall;
   function Get_customUrl : WideString; safecall;
    // errorCode : Returns the error code 
   property errorCode:Integer read Get_errorCode;
    // errorDescription : Returns a description of the error 
   property errorDescription:WideString read Get_errorDescription;
    // errorContext : Returns context information for the error 
   property errorContext:OleVariant read Get_errorContext;
    // remedy : Returns remedy code for the error 
   property remedy:Integer read Get_remedy;
    // customUrl : Returns a custom url for this error (if avail) 
   property customUrl:WideString read Get_customUrl;
  end;


// IWMPErrorItem : IWMPErrorItem: Public interface.

 IWMPErrorItemDisp = dispinterface
   ['{3614C646-3B3B-4DE7-A81E-930E3F2127B3}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // errorCode : Returns the error code 
   property errorCode:Integer  readonly dispid 901;
    // errorDescription : Returns a description of the error 
   property errorDescription:WideString  readonly dispid 902;
    // errorContext : Returns context information for the error 
   property errorContext:OleVariant  readonly dispid 903;
    // remedy : Returns remedy code for the error 
   property remedy:Integer  readonly dispid 904;
    // customUrl : Returns a custom url for this error (if avail) 
   property customUrl:WideString  readonly dispid 905;
  end;


// IWMPDVD : IWMPDVD: Public interface.

 IWMPDVD = interface(IDispatch)
   ['{8DA61686-4668-4A5C-AE5D-803193293DBE}']
   function Get_isAvailable(bstrItem:WideString) : WordBool; safecall;
   function Get_domain : WideString; safecall;
    // topMenu : Displays the top menu of the DVD 
   procedure topMenu;safecall;
    // titleMenu : Displays the title menu of the current DVD title 
   procedure titleMenu;safecall;
    // back : Navigates back one menu 
   procedure back;safecall;
    // resume : Removes the menu from the screen and returns to playing the DVD 
   procedure resume;safecall;
    // isAvailable : Returns whether or not the specified DVD functionality is available 
   property isAvailable[bstrItem:WideString]:WordBool read Get_isAvailable;
    // domain : Returns the current DVD domain 
   property domain:WideString read Get_domain;
  end;


// IWMPDVD : IWMPDVD: Public interface.

 IWMPDVDDisp = dispinterface
   ['{8DA61686-4668-4A5C-AE5D-803193293DBE}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // topMenu : Displays the top menu of the DVD 
   procedure topMenu;dispid 1003;
    // titleMenu : Displays the title menu of the current DVD title 
   procedure titleMenu;dispid 1004;
    // back : Navigates back one menu 
   procedure back;dispid 1005;
    // resume : Removes the menu from the screen and returns to playing the DVD 
   procedure resume;dispid 1006;
    // isAvailable : Returns whether or not the specified DVD functionality is available 
   property isAvailable[bstrItem:WideString]:WordBool  readonly dispid 1001;
    // domain : Returns the current DVD domain 
   property domain:WideString  readonly dispid 1002;
  end;


// IWMPPlayerApplication : IWMPPlayerApplication: Public interface.

 IWMPPlayerApplication = interface(IDispatch)
   ['{40897764-CEAB-47BE-AD4A-8E28537F9BBF}']
    // switchToPlayerApplication : Switches the display to player application 
   procedure switchToPlayerApplication;safecall;
    // switchToControl : Switches the display to control 
   procedure switchToControl;safecall;
   function Get_playerDocked : WordBool; safecall;
   function Get_hasDisplay : WordBool; safecall;
    // playerDocked : Returns a boolean value specifying whether or not the player is docked 
   property playerDocked:WordBool read Get_playerDocked;
    // hasDisplay : Returns a boolean value specifying whether or not the control has display 
   property hasDisplay:WordBool read Get_hasDisplay;
  end;


// IWMPPlayerApplication : IWMPPlayerApplication: Public interface.

 IWMPPlayerApplicationDisp = dispinterface
   ['{40897764-CEAB-47BE-AD4A-8E28537F9BBF}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // switchToPlayerApplication : Switches the display to player application 
   procedure switchToPlayerApplication;dispid 1101;
    // switchToControl : Switches the display to control 
   procedure switchToControl;dispid 1102;
    // playerDocked : Returns a boolean value specifying whether or not the player is docked 
   property playerDocked:WordBool  readonly dispid 1103;
    // hasDisplay : Returns a boolean value specifying whether or not the control has display 
   property hasDisplay:WordBool  readonly dispid 1104;
  end;


// IWMPPlayer3 : IWMPPlayer3: Public interface.

 IWMPPlayer3 = interface(IWMPCore2)
   ['{54062B68-052A-4C25-A39F-8B63346511D4}']
   function Get_enabled : WordBool; safecall;
   procedure Set_enabled(const pbEnabled:WordBool); safecall;
   function Get_fullScreen : WordBool; safecall;
   procedure Set_fullScreen(const pbFullScreen:WordBool); safecall;
   function Get_enableContextMenu : WordBool; safecall;
   procedure Set_enableContextMenu(const pbEnableContextMenu:WordBool); safecall;
   procedure Set_uiMode(const pbstrMode:WideString); safecall;
   function Get_uiMode : WideString; safecall;
   function Get_stretchToFit : WordBool; safecall;
   procedure Set_stretchToFit(const pbEnabled:WordBool); safecall;
   function Get_windowlessVideo : WordBool; safecall;
   procedure Set_windowlessVideo(const pbEnabled:WordBool); safecall;
    // enabled : Returns a boolen value specifying whether or not the control is enabled 
   property enabled:WordBool read Get_enabled write Set_enabled;
    // fullScreen : Returns a boolean value specifying whether or not the control is in full screen mode 
   property fullScreen:WordBool read Get_fullScreen write Set_fullScreen;
    // enableContextMenu : Returns a boolean value specifying whether or not the context menu is enabled on the control 
   property enableContextMenu:WordBool read Get_enableContextMenu write Set_enableContextMenu;
    // uiMode : Specifies the ui mode to select 
   property uiMode:WideString read Get_uiMode write Set_uiMode;
    // stretchToFit : Returns a boolen value specifying whether or not video is stretched 
   property stretchToFit:WordBool read Get_stretchToFit write Set_stretchToFit;
    // windowlessVideo : Returns a boolen value specifying whether or not video is windowless 
   property windowlessVideo:WordBool read Get_windowlessVideo write Set_windowlessVideo;
  end;


// IWMPPlayer3 : IWMPPlayer3: Public interface.

 IWMPPlayer3Disp = dispinterface
   ['{54062B68-052A-4C25-A39F-8B63346511D4}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // close : Closes the media 
   procedure close;dispid 3;
    // launchURL :  
   procedure launchURL(bstrURL:WideString);dispid 12;
    // URL : Returns or sets the URL 
   property URL:WideString dispid 1;
    // openState : Returns the open state of the player 
   property openState:WMPOpenState  readonly dispid 2;
    // playState : Returns the play state of the player 
   property playState:WMPPlayState  readonly dispid 10;
    // controls : Returns the control handler 
   property controls:IWMPControls  readonly dispid 4;
    // settings : Returns the settings handler 
   property settings:IWMPSettings  readonly dispid 5;
    // currentMedia : Returns or sets the current media object 
   property currentMedia:IWMPMedia dispid 6;
    // mediaCollection : Returns the media collection handler 
   property mediaCollection:IWMPMediaCollection  readonly dispid 8;
    // playlistCollection : Returns the playlist collection handler 
   property playlistCollection:IWMPPlaylistCollection  readonly dispid 9;
    // versionInfo : Returns the version information for the player 
   property versionInfo:WideString  readonly dispid 11;
    // network : Returns the network information handler 
   property network:IWMPNetwork  readonly dispid 7;
    // currentPlaylist : Returns/sets the current playlist 
   property currentPlaylist:IWMPPlaylist dispid 13;
    // cdromCollection : Get the CDROM drive collection 
   property cdromCollection:IWMPCdromCollection  readonly dispid 14;
    // closedCaption : Returns the closed caption handler 
   property closedCaption:IWMPClosedCaption  readonly dispid 15;
    // isOnline : Returns whether the machine is online. 
   property isOnline:WordBool  readonly dispid 16;
    // Error : Returns the error object 
   property Error:IWMPError  readonly dispid 17;
    // status : Returns status string 
   property status:WideString  readonly dispid 18;
    // dvd : Returns the DVD handler 
   property dvd:IWMPDVD  readonly dispid 40;
    // enabled : Returns a boolen value specifying whether or not the control is enabled 
   property enabled:WordBool dispid 19;
    // fullScreen : Returns a boolean value specifying whether or not the control is in full screen mode 
   property fullScreen:WordBool dispid 21;
    // enableContextMenu : Returns a boolean value specifying whether or not the context menu is enabled on the control 
   property enableContextMenu:WordBool dispid 22;
    // uiMode : Specifies the ui mode to select 
   property uiMode:WideString dispid 23;
    // stretchToFit : Returns a boolen value specifying whether or not video is stretched 
   property stretchToFit:WordBool dispid 24;
    // windowlessVideo : Returns a boolen value specifying whether or not video is windowless 
   property windowlessVideo:WordBool dispid 25;
  end;


// IWMPPlayer2 : IWMPPlayer2: Public interface.

 IWMPPlayer2 = interface(IWMPCore)
   ['{0E6B01D1-D407-4C85-BF5F-1C01F6150280}']
   function Get_enabled : WordBool; safecall;
   procedure Set_enabled(const pbEnabled:WordBool); safecall;
   function Get_fullScreen : WordBool; safecall;
   procedure Set_fullScreen(const pbFullScreen:WordBool); safecall;
   function Get_enableContextMenu : WordBool; safecall;
   procedure Set_enableContextMenu(const pbEnableContextMenu:WordBool); safecall;
   procedure Set_uiMode(const pbstrMode:WideString); safecall;
   function Get_uiMode : WideString; safecall;
   function Get_stretchToFit : WordBool; safecall;
   procedure Set_stretchToFit(const pbEnabled:WordBool); safecall;
   function Get_windowlessVideo : WordBool; safecall;
   procedure Set_windowlessVideo(const pbEnabled:WordBool); safecall;
    // enabled : Returns a boolen value specifying whether or not the control is enabled 
   property enabled:WordBool read Get_enabled write Set_enabled;
    // fullScreen : Returns a boolean value specifying whether or not the control is in full screen mode 
   property fullScreen:WordBool read Get_fullScreen write Set_fullScreen;
    // enableContextMenu : Returns a boolean value specifying whether or not the context menu is enabled on the control 
   property enableContextMenu:WordBool read Get_enableContextMenu write Set_enableContextMenu;
    // uiMode : Specifies the ui mode to select 
   property uiMode:WideString read Get_uiMode write Set_uiMode;
    // stretchToFit : Returns a boolen value specifying whether or not video is stretched 
   property stretchToFit:WordBool read Get_stretchToFit write Set_stretchToFit;
    // windowlessVideo : Returns a boolen value specifying whether or not video is windowless 
   property windowlessVideo:WordBool read Get_windowlessVideo write Set_windowlessVideo;
  end;


// IWMPPlayer2 : IWMPPlayer2: Public interface.

 IWMPPlayer2Disp = dispinterface
   ['{0E6B01D1-D407-4C85-BF5F-1C01F6150280}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // close : Closes the media 
   procedure close;dispid 3;
    // launchURL :  
   procedure launchURL(bstrURL:WideString);dispid 12;
    // URL : Returns or sets the URL 
   property URL:WideString dispid 1;
    // openState : Returns the open state of the player 
   property openState:WMPOpenState  readonly dispid 2;
    // playState : Returns the play state of the player 
   property playState:WMPPlayState  readonly dispid 10;
    // controls : Returns the control handler 
   property controls:IWMPControls  readonly dispid 4;
    // settings : Returns the settings handler 
   property settings:IWMPSettings  readonly dispid 5;
    // currentMedia : Returns or sets the current media object 
   property currentMedia:IWMPMedia dispid 6;
    // mediaCollection : Returns the media collection handler 
   property mediaCollection:IWMPMediaCollection  readonly dispid 8;
    // playlistCollection : Returns the playlist collection handler 
   property playlistCollection:IWMPPlaylistCollection  readonly dispid 9;
    // versionInfo : Returns the version information for the player 
   property versionInfo:WideString  readonly dispid 11;
    // network : Returns the network information handler 
   property network:IWMPNetwork  readonly dispid 7;
    // currentPlaylist : Returns/sets the current playlist 
   property currentPlaylist:IWMPPlaylist dispid 13;
    // cdromCollection : Get the CDROM drive collection 
   property cdromCollection:IWMPCdromCollection  readonly dispid 14;
    // closedCaption : Returns the closed caption handler 
   property closedCaption:IWMPClosedCaption  readonly dispid 15;
    // isOnline : Returns whether the machine is online. 
   property isOnline:WordBool  readonly dispid 16;
    // Error : Returns the error object 
   property Error:IWMPError  readonly dispid 17;
    // status : Returns status string 
   property status:WideString  readonly dispid 18;
    // enabled : Returns a boolen value specifying whether or not the control is enabled 
   property enabled:WordBool dispid 19;
    // fullScreen : Returns a boolean value specifying whether or not the control is in full screen mode 
   property fullScreen:WordBool dispid 21;
    // enableContextMenu : Returns a boolean value specifying whether or not the context menu is enabled on the control 
   property enableContextMenu:WordBool dispid 22;
    // uiMode : Specifies the ui mode to select 
   property uiMode:WideString dispid 23;
    // stretchToFit : Returns a boolen value specifying whether or not video is stretched 
   property stretchToFit:WordBool dispid 24;
    // windowlessVideo : Returns a boolen value specifying whether or not video is windowless 
   property windowlessVideo:WordBool dispid 25;
  end;


// IWMPPlayer : IWMPPlayer: Public interface.

 IWMPPlayer = interface(IWMPCore)
   ['{6BF52A4F-394A-11D3-B153-00C04F79FAA6}']
   function Get_enabled : WordBool; safecall;
   procedure Set_enabled(const pbEnabled:WordBool); safecall;
   function Get_fullScreen : WordBool; safecall;
   procedure Set_fullScreen(const pbFullScreen:WordBool); safecall;
   function Get_enableContextMenu : WordBool; safecall;
   procedure Set_enableContextMenu(const pbEnableContextMenu:WordBool); safecall;
   procedure Set_uiMode(const pbstrMode:WideString); safecall;
   function Get_uiMode : WideString; safecall;
    // enabled : Returns a boolen value specifying whether or not the control is enabled 
   property enabled:WordBool read Get_enabled write Set_enabled;
    // fullScreen : Returns a boolean value specifying whether or not the control is in full screen mode 
   property fullScreen:WordBool read Get_fullScreen write Set_fullScreen;
    // enableContextMenu : Returns a boolean value specifying whether or not the context menu is enabled on the control 
   property enableContextMenu:WordBool read Get_enableContextMenu write Set_enableContextMenu;
    // uiMode : Specifies the ui mode to select 
   property uiMode:WideString read Get_uiMode write Set_uiMode;
  end;


// IWMPPlayer : IWMPPlayer: Public interface.

 IWMPPlayerDisp = dispinterface
   ['{6BF52A4F-394A-11D3-B153-00C04F79FAA6}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // close : Closes the media 
   procedure close;dispid 3;
    // launchURL :  
   procedure launchURL(bstrURL:WideString);dispid 12;
    // URL : Returns or sets the URL 
   property URL:WideString dispid 1;
    // openState : Returns the open state of the player 
   property openState:WMPOpenState  readonly dispid 2;
    // playState : Returns the play state of the player 
   property playState:WMPPlayState  readonly dispid 10;
    // controls : Returns the control handler 
   property controls:IWMPControls  readonly dispid 4;
    // settings : Returns the settings handler 
   property settings:IWMPSettings  readonly dispid 5;
    // currentMedia : Returns or sets the current media object 
   property currentMedia:IWMPMedia dispid 6;
    // mediaCollection : Returns the media collection handler 
   property mediaCollection:IWMPMediaCollection  readonly dispid 8;
    // playlistCollection : Returns the playlist collection handler 
   property playlistCollection:IWMPPlaylistCollection  readonly dispid 9;
    // versionInfo : Returns the version information for the player 
   property versionInfo:WideString  readonly dispid 11;
    // network : Returns the network information handler 
   property network:IWMPNetwork  readonly dispid 7;
    // currentPlaylist : Returns/sets the current playlist 
   property currentPlaylist:IWMPPlaylist dispid 13;
    // cdromCollection : Get the CDROM drive collection 
   property cdromCollection:IWMPCdromCollection  readonly dispid 14;
    // closedCaption : Returns the closed caption handler 
   property closedCaption:IWMPClosedCaption  readonly dispid 15;
    // isOnline : Returns whether the machine is online. 
   property isOnline:WordBool  readonly dispid 16;
    // Error : Returns the error object 
   property Error:IWMPError  readonly dispid 17;
    // status : Returns status string 
   property status:WideString  readonly dispid 18;
    // enabled : Returns a boolen value specifying whether or not the control is enabled 
   property enabled:WordBool dispid 19;
    // fullScreen : Returns a boolean value specifying whether or not the control is in full screen mode 
   property fullScreen:WordBool dispid 21;
    // enableContextMenu : Returns a boolean value specifying whether or not the context menu is enabled on the control 
   property enableContextMenu:WordBool dispid 22;
    // uiMode : Specifies the ui mode to select 
   property uiMode:WideString dispid 23;
  end;


// IWMPErrorItem2 : IWMPErrorItem2: Public interface.

 IWMPErrorItem2 = interface(IWMPErrorItem)
   ['{F75CCEC0-C67C-475C-931E-8719870BEE7D}']
   function Get_condition : Integer; safecall;
    // condition : Returns condition for the error 
   property condition:Integer read Get_condition;
  end;


// IWMPErrorItem2 : IWMPErrorItem2: Public interface.

 IWMPErrorItem2Disp = dispinterface
   ['{F75CCEC0-C67C-475C-931E-8719870BEE7D}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // errorCode : Returns the error code 
   property errorCode:Integer  readonly dispid 901;
    // errorDescription : Returns a description of the error 
   property errorDescription:WideString  readonly dispid 902;
    // errorContext : Returns context information for the error 
   property errorContext:OleVariant  readonly dispid 903;
    // remedy : Returns remedy code for the error 
   property remedy:Integer  readonly dispid 904;
    // customUrl : Returns a custom url for this error (if avail) 
   property customUrl:WideString  readonly dispid 905;
    // condition : Returns condition for the error 
   property condition:Integer  readonly dispid 906;
  end;


// IWMPControls2 : IWMPControls2: Public interface.

 IWMPControls2 = interface(IWMPControls)
   ['{6F030D25-0890-480F-9775-1F7E40AB5B8E}']
    // step : Advances the video one frame 
   procedure step(lStep:Integer);safecall;
  end;


// IWMPControls2 : IWMPControls2: Public interface.

 IWMPControls2Disp = dispinterface
   ['{6F030D25-0890-480F-9775-1F7E40AB5B8E}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // play : Begins playing media 
   procedure play;dispid 51;
    // stop : Stops play of media 
   procedure stop;dispid 52;
    // pause : Pauses play of media 
   procedure pause;dispid 53;
    // fastForward : Fast play of media in forward direction 
   procedure fastForward;dispid 54;
    // fastReverse : Fast play of media in reverse direction 
   procedure fastReverse;dispid 55;
    // next : Sets the current item to the next item in the playlist 
   procedure next;dispid 58;
    // previous : Sets the current item to the previous item in the playlist 
   procedure previous;dispid 59;
    // playItem : Sets the current item and plays it 
   procedure playItem(pIWMPMedia:IWMPMedia);dispid 63;
    // step : Advances the video one frame 
   procedure step(lStep:Integer);dispid 64;
    // isAvailable : Returns whether or not the specified media functionality is available 
   property isAvailable[bstrItem:WideString]:WordBool  readonly dispid 62;
    // currentPosition : Returns the current position in media 
   property currentPosition:Double dispid 56;
    // currentPositionString : Returns the current position in media as a string 
   property currentPositionString:WideString  readonly dispid 57;
    // currentItem : Returns/Sets the play item 
   property currentItem:IWMPMedia dispid 60;
    // currentMarker : Returns the current marker 
   property currentMarker:Integer dispid 61;
  end;


// IWMPMedia2 : IWMPMedia2: Public interface.

 IWMPMedia2 = interface(IWMPMedia)
   ['{AB7C88BB-143E-4EA4-ACC3-E4350B2106C3}']
   function Get_Error : IWMPErrorItem; safecall;
    // Error : Returns an error item pointer for a media specific error 
   property Error:IWMPErrorItem read Get_Error;
  end;


// IWMPMedia2 : IWMPMedia2: Public interface.

 IWMPMedia2Disp = dispinterface
   ['{AB7C88BB-143E-4EA4-ACC3-E4350B2106C3}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // getMarkerTime : Returns the time of a marker 
   function getMarkerTime(MarkerNum:Integer):Double;dispid 755;
    // getMarkerName : Returns the name of a marker 
   function getMarkerName(MarkerNum:Integer):WideString;dispid 756;
    // getAttributeName : Returns the name of the attribute whose index has been specified 
   function getAttributeName(lIndex:Integer):WideString;dispid 760;
    // getItemInfo : Returns the value of specified attribute for this media 
   function getItemInfo(bstrItemName:WideString):WideString;dispid 761;
    // setItemInfo : Sets the value of specified attribute for this media 
   procedure setItemInfo(bstrItemName:WideString;bstrVal:WideString);dispid 762;
    // getItemInfoByAtom : Gets an item info by atom 
   function getItemInfoByAtom(lAtom:Integer):WideString;dispid 765;
    // isMemberOf : Is the media a member of the given playlist 
   function isMemberOf(pPlaylist:IWMPPlaylist):WordBool;dispid 766;
    // isReadOnlyItem : Is the attribute read only 
   function isReadOnlyItem(bstrItemName:WideString):WordBool;dispid 767;
    // isIdentical : Determines if the supplied object is the same as the this one 
   property isIdentical[pIWMPMedia:IWMPMedia]:WordBool  readonly dispid 763;
    // sourceURL : Returns the media URL 
   property sourceURL:WideString  readonly dispid 751;
    // name : Returns the name of the media 
   property name:WideString dispid 764;
    // imageSourceWidth : Returns the original width of the source images 
   property imageSourceWidth:Integer  readonly dispid 752;
    // imageSourceHeight : Returns the original height of the source images 
   property imageSourceHeight:Integer  readonly dispid 753;
    // markerCount : Returns the number of markers in the file 
   property markerCount:Integer  readonly dispid 754;
    // duration : Returns duration of current media 
   property duration:Double  readonly dispid 757;
    // durationString : Returns duration of current media as a string 
   property durationString:WideString  readonly dispid 758;
    // attributeCount : Returns the count of the attributes associated with this media 
   property attributeCount:Integer  readonly dispid 759;
    // Error : Returns an error item pointer for a media specific error 
   property Error:IWMPErrorItem  readonly dispid 768;
  end;


// IWMPMedia3 : IWMPMedia3: Public interface.

 IWMPMedia3 = interface(IWMPMedia2)
   ['{F118EFC7-F03A-4FB4-99C9-1C02A5C1065B}']
    // getAttributeCountByType :  
   function getAttributeCountByType(bstrType:WideString;bstrLanguage:WideString):Integer;safecall;
    // getItemInfoByType :  
   function getItemInfoByType(bstrType:WideString;bstrLanguage:WideString;lIndex:Integer):OleVariant;safecall;
  end;


// IWMPMedia3 : IWMPMedia3: Public interface.

 IWMPMedia3Disp = dispinterface
   ['{F118EFC7-F03A-4FB4-99C9-1C02A5C1065B}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // getMarkerTime : Returns the time of a marker 
   function getMarkerTime(MarkerNum:Integer):Double;dispid 755;
    // getMarkerName : Returns the name of a marker 
   function getMarkerName(MarkerNum:Integer):WideString;dispid 756;
    // getAttributeName : Returns the name of the attribute whose index has been specified 
   function getAttributeName(lIndex:Integer):WideString;dispid 760;
    // getItemInfo : Returns the value of specified attribute for this media 
   function getItemInfo(bstrItemName:WideString):WideString;dispid 761;
    // setItemInfo : Sets the value of specified attribute for this media 
   procedure setItemInfo(bstrItemName:WideString;bstrVal:WideString);dispid 762;
    // getItemInfoByAtom : Gets an item info by atom 
   function getItemInfoByAtom(lAtom:Integer):WideString;dispid 765;
    // isMemberOf : Is the media a member of the given playlist 
   function isMemberOf(pPlaylist:IWMPPlaylist):WordBool;dispid 766;
    // isReadOnlyItem : Is the attribute read only 
   function isReadOnlyItem(bstrItemName:WideString):WordBool;dispid 767;
    // getAttributeCountByType :  
   function getAttributeCountByType(bstrType:WideString;bstrLanguage:WideString):Integer;dispid 769;
    // getItemInfoByType :  
   function getItemInfoByType(bstrType:WideString;bstrLanguage:WideString;lIndex:Integer):OleVariant;dispid 770;
    // isIdentical : Determines if the supplied object is the same as the this one 
   property isIdentical[pIWMPMedia:IWMPMedia]:WordBool  readonly dispid 763;
    // sourceURL : Returns the media URL 
   property sourceURL:WideString  readonly dispid 751;
    // name : Returns the name of the media 
   property name:WideString dispid 764;
    // imageSourceWidth : Returns the original width of the source images 
   property imageSourceWidth:Integer  readonly dispid 752;
    // imageSourceHeight : Returns the original height of the source images 
   property imageSourceHeight:Integer  readonly dispid 753;
    // markerCount : Returns the number of markers in the file 
   property markerCount:Integer  readonly dispid 754;
    // duration : Returns duration of current media 
   property duration:Double  readonly dispid 757;
    // durationString : Returns duration of current media as a string 
   property durationString:WideString  readonly dispid 758;
    // attributeCount : Returns the count of the attributes associated with this media 
   property attributeCount:Integer  readonly dispid 759;
    // Error : Returns an error item pointer for a media specific error 
   property Error:IWMPErrorItem  readonly dispid 768;
  end;


// IWMPMetadataPicture : IWMPMetadataPicture: Not Public.  Internal interface used by Windows Media Player.

 IWMPMetadataPicture = interface(IDispatch)
   ['{5C29BBE0-F87D-4C45-AA28-A70F0230FFA9}']
   function Get_mimeType : WideString; safecall;
   function Get_pictureType : WideString; safecall;
   function Get_Description : WideString; safecall;
   function Get_URL : WideString; safecall;
    // mimeType :  
   property mimeType:WideString read Get_mimeType;
    // pictureType :  
   property pictureType:WideString read Get_pictureType;
    // Description :  
   property Description:WideString read Get_Description;
    // URL :  
   property URL:WideString read Get_URL;
  end;


// IWMPMetadataPicture : IWMPMetadataPicture: Not Public.  Internal interface used by Windows Media Player.

 IWMPMetadataPictureDisp = dispinterface
   ['{5C29BBE0-F87D-4C45-AA28-A70F0230FFA9}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // mimeType :  
   property mimeType:WideString  readonly dispid 1051;
    // pictureType :  
   property pictureType:WideString  readonly dispid 1052;
    // Description :  
   property Description:WideString  readonly dispid 1053;
    // URL :  
   property URL:WideString  readonly dispid 1054;
  end;


// IWMPMetadataText : IWMPMetadataText: Not Public.  Internal interface used by Windows Media Player.

 IWMPMetadataText = interface(IDispatch)
   ['{769A72DB-13D2-45E2-9C48-53CA9D5B7450}']
   function Get_Description : WideString; safecall;
   function Get_text_ : WideString; safecall;
    // Description :  
   property Description:WideString read Get_Description;
    // text :  
   property text_:WideString read Get_text_;
  end;


// IWMPMetadataText : IWMPMetadataText: Not Public.  Internal interface used by Windows Media Player.

 IWMPMetadataTextDisp = dispinterface
   ['{769A72DB-13D2-45E2-9C48-53CA9D5B7450}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // Description :  
   property Description:WideString  readonly dispid 1056;
    // text :  
   property text_:WideString  readonly dispid 1055;
  end;


// IWMPSettings2 : IWMPSettings2: Public interface.

 IWMPSettings2 = interface(IWMPSettings)
   ['{FDA937A4-EECE-4DA5-A0B6-39BF89ADE2C2}']
   function Get_defaultAudioLanguage : Integer; safecall;
   function Get_mediaAccessRights : WideString; safecall;
    // requestMediaAccessRights :  
   function requestMediaAccessRights(bstrDesiredAccess:WideString):WordBool;safecall;
    // defaultAudioLanguage : Returns the LCID of default audio language 
   property defaultAudioLanguage:Integer read Get_defaultAudioLanguage;
    // mediaAccessRights :  
   property mediaAccessRights:WideString read Get_mediaAccessRights;
  end;


// IWMPSettings2 : IWMPSettings2: Public interface.

 IWMPSettings2Disp = dispinterface
   ['{FDA937A4-EECE-4DA5-A0B6-39BF89ADE2C2}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // getMode : Returns the mode of the playlist 
   function getMode(bstrMode:WideString):WordBool;dispid 110;
    // setMode : Sets the mode of the playlist 
   procedure setMode(bstrMode:WideString;varfMode:WordBool);dispid 111;
    // requestMediaAccessRights :  
   function requestMediaAccessRights(bstrDesiredAccess:WideString):WordBool;dispid 116;
    // isAvailable : Returns whether or not the specified media functionality is available 
   property isAvailable[bstrItem:WideString]:WordBool  readonly dispid 113;
    // autoStart : Returns whether media should automatically begin playing 
   property autoStart:WordBool dispid 101;
    // baseURL : Returns the base URL used for relative path resolution 
   property baseURL:WideString dispid 108;
    // defaultFrame : Returns the frame location that changes when a URL flip occurs 
   property defaultFrame:WideString dispid 109;
    // invokeURLs : Returns whether URL events should spawn a browser. 
   property invokeURLs:WordBool dispid 103;
    // mute : Returns whether audio should be muted. 
   property mute:WordBool dispid 104;
    // playCount : Returns how many times media should play 
   property playCount:Integer dispid 105;
    // rate : Returns current playback rate 
   property rate:Double dispid 106;
    // balance : Returns current audio Balance 
   property balance:Integer dispid 102;
    // volume : Returns current audio volume 
   property volume:Integer dispid 107;
    // enableErrorDialogs : Returns whether error dialogs are shown by default when embedded 
   property enableErrorDialogs:WordBool dispid 112;
    // defaultAudioLanguage : Returns the LCID of default audio language 
   property defaultAudioLanguage:Integer  readonly dispid 114;
    // mediaAccessRights :  
   property mediaAccessRights:WideString  readonly dispid 115;
  end;


// IWMPControls3 : IWMPControls3: Public interface.

 IWMPControls3 = interface(IWMPControls2)
   ['{A1D1110E-D545-476A-9A78-AC3E4CB1E6BD}']
   function Get_audioLanguageCount : Integer; safecall;
    // getAudioLanguageID : Returns the LCID corresponding to the index 
   function getAudioLanguageID(lIndex:Integer):Integer;safecall;
    // getAudioLanguageDescription : Returns the desription corresponding to the index 
   function getAudioLanguageDescription(lIndex:Integer):WideString;safecall;
   function Get_currentAudioLanguage : Integer; safecall;
   procedure Set_currentAudioLanguage(const plLangID:Integer); safecall;
   function Get_currentAudioLanguageIndex : Integer; safecall;
   procedure Set_currentAudioLanguageIndex(const plIndex:Integer); safecall;
    // getLanguageName : Returns the human-readable name of language specified by LCID 
   function getLanguageName(lLangID:Integer):WideString;safecall;
   function Get_currentPositionTimecode : WideString; safecall;
   procedure Set_currentPositionTimecode(const bstrTimecode:WideString); safecall;
    // audioLanguageCount : Returns the count of supported audio languages 
   property audioLanguageCount:Integer read Get_audioLanguageCount;
    // currentAudioLanguage : Gets the current audio language setting for playback 
   property currentAudioLanguage:Integer read Get_currentAudioLanguage write Set_currentAudioLanguage;
    // currentAudioLanguageIndex : Gets the current audio language index setting for playback 
   property currentAudioLanguageIndex:Integer read Get_currentAudioLanguageIndex write Set_currentAudioLanguageIndex;
    // currentPositionTimecode : Returns the current timecode position in media 
   property currentPositionTimecode:WideString read Get_currentPositionTimecode write Set_currentPositionTimecode;
  end;


// IWMPControls3 : IWMPControls3: Public interface.

 IWMPControls3Disp = dispinterface
   ['{A1D1110E-D545-476A-9A78-AC3E4CB1E6BD}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // play : Begins playing media 
   procedure play;dispid 51;
    // stop : Stops play of media 
   procedure stop;dispid 52;
    // pause : Pauses play of media 
   procedure pause;dispid 53;
    // fastForward : Fast play of media in forward direction 
   procedure fastForward;dispid 54;
    // fastReverse : Fast play of media in reverse direction 
   procedure fastReverse;dispid 55;
    // next : Sets the current item to the next item in the playlist 
   procedure next;dispid 58;
    // previous : Sets the current item to the previous item in the playlist 
   procedure previous;dispid 59;
    // playItem : Sets the current item and plays it 
   procedure playItem(pIWMPMedia:IWMPMedia);dispid 63;
    // step : Advances the video one frame 
   procedure step(lStep:Integer);dispid 64;
    // getAudioLanguageID : Returns the LCID corresponding to the index 
   function getAudioLanguageID(lIndex:Integer):Integer;dispid 66;
    // getAudioLanguageDescription : Returns the desription corresponding to the index 
   function getAudioLanguageDescription(lIndex:Integer):WideString;dispid 67;
    // getLanguageName : Returns the human-readable name of language specified by LCID 
   function getLanguageName(lLangID:Integer):WideString;dispid 70;
    // isAvailable : Returns whether or not the specified media functionality is available 
   property isAvailable[bstrItem:WideString]:WordBool  readonly dispid 62;
    // currentPosition : Returns the current position in media 
   property currentPosition:Double dispid 56;
    // currentPositionString : Returns the current position in media as a string 
   property currentPositionString:WideString  readonly dispid 57;
    // currentItem : Returns/Sets the play item 
   property currentItem:IWMPMedia dispid 60;
    // currentMarker : Returns the current marker 
   property currentMarker:Integer dispid 61;
    // audioLanguageCount : Returns the count of supported audio languages 
   property audioLanguageCount:Integer  readonly dispid 65;
    // currentAudioLanguage : Gets the current audio language setting for playback 
   property currentAudioLanguage:Integer dispid 68;
    // currentAudioLanguageIndex : Gets the current audio language index setting for playback 
   property currentAudioLanguageIndex:Integer dispid 69;
    // currentPositionTimecode : Returns the current timecode position in media 
   property currentPositionTimecode:WideString dispid 71;
  end;


// IWMPClosedCaption2 : IWMPClosedCaption2: Public interface.

 IWMPClosedCaption2 = interface(IWMPClosedCaption)
   ['{350BA78B-6BC8-4113-A5F5-312056934EB6}']
   function Get_SAMILangCount : Integer; safecall;
    // getSAMILangName : Returns the name of a SAMI language by index 
   function getSAMILangName(nIndex:Integer):WideString;safecall;
    // getSAMILangID : Returns the ID of a SAMI language by index 
   function getSAMILangID(nIndex:Integer):Integer;safecall;
   function Get_SAMIStyleCount : Integer; safecall;
    // getSAMIStyleName : Returns the name of a SAMI style by index 
   function getSAMIStyleName(nIndex:Integer):WideString;safecall;
    // SAMILangCount : Returns the count of SAMI languages 
   property SAMILangCount:Integer read Get_SAMILangCount;
    // SAMIStyleCount : Returns the count of SAMI styles 
   property SAMIStyleCount:Integer read Get_SAMIStyleCount;
  end;


// IWMPClosedCaption2 : IWMPClosedCaption2: Public interface.

 IWMPClosedCaption2Disp = dispinterface
   ['{350BA78B-6BC8-4113-A5F5-312056934EB6}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // getSAMILangName : Returns the name of a SAMI language by index 
   function getSAMILangName(nIndex:Integer):WideString;dispid 956;
    // getSAMILangID : Returns the ID of a SAMI language by index 
   function getSAMILangID(nIndex:Integer):Integer;dispid 957;
    // getSAMIStyleName : Returns the name of a SAMI style by index 
   function getSAMIStyleName(nIndex:Integer):WideString;dispid 959;
    // SAMIStyle : Returns the previously set SAMI style 
   property SAMIStyle:WideString dispid 951;
    // SAMILang : Returns the previously set SAMI language 
   property SAMILang:WideString dispid 952;
    // SAMIFileName : Returns the previously set SAMI file name 
   property SAMIFileName:WideString dispid 953;
    // captioningId : Returns the previously set Captioning ID 
   property captioningId:WideString dispid 954;
    // SAMILangCount : Returns the count of SAMI languages 
   property SAMILangCount:Integer  readonly dispid 955;
    // SAMIStyleCount : Returns the count of SAMI styles 
   property SAMIStyleCount:Integer  readonly dispid 958;
  end;


// IWMPMediaCollection2 : IWMPMediaCollection2: Public interface for Windows Media Player SDK.

 IWMPMediaCollection2 = interface(IWMPMediaCollection)
   ['{8BA957F5-FD8C-4791-B82D-F840401EE474}']
    // createQuery : Creates an empty query object 
   function createQuery:IWMPQuery;safecall;
    // getPlaylistByQuery : Creates a playlist from a query 
   function getPlaylistByQuery(pQuery:IWMPQuery;bstrMediaType:WideString;bstrSortAttribute:WideString;fSortAscending:WordBool):IWMPPlaylist;safecall;
    // getStringCollectionByQuery : Creates a string collection from a query 
   function getStringCollectionByQuery(bstrAttribute:WideString;pQuery:IWMPQuery;bstrMediaType:WideString;bstrSortAttribute:WideString;fSortAscending:WordBool):IWMPStringCollection;safecall;
    // getByAttributeAndMediaType : Returns a collection of items with the given attribute and media type 
   function getByAttributeAndMediaType(bstrAttribute:WideString;bstrValue:WideString;bstrMediaType:WideString):IWMPPlaylist;safecall;
  end;


// IWMPMediaCollection2 : IWMPMediaCollection2: Public interface for Windows Media Player SDK.

 IWMPMediaCollection2Disp = dispinterface
   ['{8BA957F5-FD8C-4791-B82D-F840401EE474}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // add : Creates a new media object 
   function add(bstrURL:WideString):IWMPMedia;dispid 452;
    // getAll : Returns a collection of all the items 
   function getAll:IWMPPlaylist;dispid 453;
    // getByName : Returns a collection of items with the given name 
   function getByName(bstrName:WideString):IWMPPlaylist;dispid 454;
    // getByGenre : Returns a collection of items with the given genre 
   function getByGenre(bstrGenre:WideString):IWMPPlaylist;dispid 455;
    // getByAuthor : Returns a collection of items by a given author 
   function getByAuthor(bstrAuthor:WideString):IWMPPlaylist;dispid 456;
    // getByAlbum : Returns a collection of items from the given album 
   function getByAlbum(bstrAlbum:WideString):IWMPPlaylist;dispid 457;
    // getByAttribute : Returns a collection of items with the given attribute 
   function getByAttribute(bstrAttribute:WideString;bstrValue:WideString):IWMPPlaylist;dispid 458;
    // remove : Removes an item from the media collection 
   procedure remove(pItem:IWMPMedia;varfDeleteFile:WordBool);dispid 459;
    // getAttributeStringCollection : Returns the string collection associated with an attribute 
   function getAttributeStringCollection(bstrAttribute:WideString;bstrMediaType:WideString):IWMPStringCollection;dispid 461;
    // getMediaAtom : Gets an atom associated with an item name which can be requested from an IWMPMedia out of this collection via getItemInfoByAtom 
   function getMediaAtom(bstrItemName:WideString):Integer;dispid 470;
    // setDeleted : Sets the deleted flag on a media object 
   procedure setDeleted(pItem:IWMPMedia;varfIsDeleted:WordBool);dispid 471;
    // isDeleted : Gets the deleted flag on a media object 
   function isDeleted(pItem:IWMPMedia):WordBool;dispid 472;
    // createQuery : Creates an empty query object 
   function createQuery:IWMPQuery;dispid 1401;
    // getPlaylistByQuery : Creates a playlist from a query 
   function getPlaylistByQuery(pQuery:IWMPQuery;bstrMediaType:WideString;bstrSortAttribute:WideString;fSortAscending:WordBool):IWMPPlaylist;dispid 1402;
    // getStringCollectionByQuery : Creates a string collection from a query 
   function getStringCollectionByQuery(bstrAttribute:WideString;pQuery:IWMPQuery;bstrMediaType:WideString;bstrSortAttribute:WideString;fSortAscending:WordBool):IWMPStringCollection;dispid 1403;
    // getByAttributeAndMediaType : Returns a collection of items with the given attribute and media type 
   function getByAttributeAndMediaType(bstrAttribute:WideString;bstrValue:WideString;bstrMediaType:WideString):IWMPPlaylist;dispid 1404;
  end;


// IWMPQuery : IWMPQuery: Public interface for Windows Media Player SDK.

 IWMPQuery = interface(IDispatch)
   ['{A00918F3-A6B0-4BFB-9189-FD834C7BC5A5}']
    // addCondition : Adds a single AND query parameter to existing group 
   procedure addCondition(bstrAttribute:WideString;bstrOperator:WideString;bstrValue:WideString);safecall;
    // beginNextGroup : Starts a new OR query group 
   procedure beginNextGroup;safecall;
  end;


// IWMPQuery : IWMPQuery: Public interface for Windows Media Player SDK.

 IWMPQueryDisp = dispinterface
   ['{A00918F3-A6B0-4BFB-9189-FD834C7BC5A5}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // addCondition : Adds a single AND query parameter to existing group 
   procedure addCondition(bstrAttribute:WideString;bstrOperator:WideString;bstrValue:WideString);dispid 1351;
    // beginNextGroup : Starts a new OR query group 
   procedure beginNextGroup;dispid 1352;
  end;


// IWMPStringCollection2 : IWMPStringCollection2: Public interface for Windows Media Player SDK.

 IWMPStringCollection2 = interface(IWMPStringCollection)
   ['{46AD648D-53F1-4A74-92E2-2A1B68D63FD4}']
    // isIdentical : Determines if the supplied object is the same as this one 
   function isIdentical(pIWMPStringCollection2:IWMPStringCollection2):WordBool;safecall;
    // getItemInfo : Gets an attribute from a string collection backing object 
   function getItemInfo(lCollectionIndex:Integer;bstrItemName:WideString):WideString;safecall;
    // getAttributeCountByType : Gets count of values for a particular attribute 
   function getAttributeCountByType(lCollectionIndex:Integer;bstrType:WideString;bstrLanguage:WideString):Integer;safecall;
    // getItemInfoByType : Gets one value of an attribute from a string collection backing object 
   function getItemInfoByType(lCollectionIndex:Integer;bstrType:WideString;bstrLanguage:WideString;lAttributeIndex:Integer):OleVariant;safecall;
  end;


// IWMPStringCollection2 : IWMPStringCollection2: Public interface for Windows Media Player SDK.

 IWMPStringCollection2Disp = dispinterface
   ['{46AD648D-53F1-4A74-92E2-2A1B68D63FD4}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // Item : Returns the string at the given index 
   function Item(lIndex:Integer):WideString;dispid 402;
    // isIdentical : Determines if the supplied object is the same as this one 
   function isIdentical(pIWMPStringCollection2:IWMPStringCollection2):WordBool;dispid 1451;
    // getItemInfo : Gets an attribute from a string collection backing object 
   function getItemInfo(lCollectionIndex:Integer;bstrItemName:WideString):WideString;dispid 1452;
    // getAttributeCountByType : Gets count of values for a particular attribute 
   function getAttributeCountByType(lCollectionIndex:Integer;bstrType:WideString;bstrLanguage:WideString):Integer;dispid 1453;
    // getItemInfoByType : Gets one value of an attribute from a string collection backing object 
   function getItemInfoByType(lCollectionIndex:Integer;bstrType:WideString;bstrLanguage:WideString;lAttributeIndex:Integer):OleVariant;dispid 1454;
    // count : Returns the number of items in the string collection 
   property count:Integer  readonly dispid 401;
  end;


// IWMPPlayerServices : IWMPPlayerServices: Public interface for Windows Media Player SDK.

 IWMPPlayerServices = interface(IUnknown)
   ['{1D01FBDB-ADE2-4C8D-9842-C190B95C3306}']
    // activateUIPlugin :  
   function activateUIPlugin(bstrPlugin:WideString):HRESULT;stdcall;
    // setTaskPane :  
   function setTaskPane(bstrTaskPane:WideString):HRESULT;stdcall;
    // setTaskPaneURL :  
   function setTaskPaneURL(bstrTaskPane:WideString;bstrURL:WideString;bstrFriendlyName:WideString):HRESULT;stdcall;
  end;


// IWMPPlayerServices2 : IWMPPlayerServices2: Public interface for Windows Media Player SDK.

 IWMPPlayerServices2 = interface(IWMPPlayerServices)
   ['{1BB1592F-F040-418A-9F71-17C7512B4D70}']
    // setBackgroundProcessingPriority :  
   function setBackgroundProcessingPriority(bstrPriority:WideString):HRESULT;stdcall;
  end;


// IWMPRemoteMediaServices : IWMPRemoteMediaServices: Public interface for Windows Media Player SDK.

 IWMPRemoteMediaServices = interface(IUnknown)
   ['{CBB92747-741F-44FE-AB5B-F1A48F3B2A59}']
    // GetServiceType :  
   function GetServiceType(out pbstrType:WideString):HRESULT;stdcall;
    // GetApplicationName :  
   function GetApplicationName(out pbstrName:WideString):HRESULT;stdcall;
    // GetScriptableObject :  
   function GetScriptableObject(out pbstrName:WideString;out ppDispatch:IDispatch):HRESULT;stdcall;
    // GetCustomUIMode :  
   function GetCustomUIMode(out pbstrFile:WideString):HRESULT;stdcall;
  end;


// IWMPSyncServices : IWMPSyncServices: Public interface for Windows Media Player SDK.

 IWMPSyncServices = interface(IUnknown)
   ['{8B5050FF-E0A4-4808-B3A8-893A9E1ED894}']
   function Get_deviceCount : Integer; stdcall;
    // getDevice :  
   function getDevice(lIndex:Integer):HRESULT;stdcall;
    // deviceCount :  
   property deviceCount:Integer read Get_deviceCount;
  end;


// IWMPLibraryServices : IWMPLibraryServices: Public interface for Windows Media Player SDK.

 IWMPLibraryServices = interface(IUnknown)
   ['{39C2F8D5-1CF2-4D5E-AE09-D73492CF9EAA}']
    // getCountByType :  
   function getCountByType(wmplt:WMPLibraryType):HRESULT;stdcall;
    // getLibraryByType :  
   function getLibraryByType(wmplt:WMPLibraryType;lIndex:Integer):HRESULT;stdcall;
  end;


// IWMPLibrarySharingServices : IWMPLibrarySharingServices: Public interface for Windows Media Player SDK.

 IWMPLibrarySharingServices = interface(IUnknown)
   ['{82CBA86B-9F04-474B-A365-D6DD1466E541}']
    // isLibraryShared :  
   function isLibraryShared:HRESULT;stdcall;
    // isLibrarySharingEnabled :  
   function isLibrarySharingEnabled:HRESULT;stdcall;
    // showLibrarySharing :  
   function showLibrarySharing:HRESULT;stdcall;
  end;


// IWMPLibrary2 : IWMPLibrary2: Public interface for Windows Media Player SDK.

 IWMPLibrary2 = interface(IWMPLibrary)
   ['{DD578A4E-79B1-426C-BF8F-3ADD9072500B}']
    // getItemInfo :  
   function getItemInfo(bstrItemName:WideString):HRESULT;stdcall;
  end;


// IWMPFolderMonitorServices : IWMPFolderMonitorServices: Public interface for Windows Media Player SDK.

 IWMPFolderMonitorServices = interface(IUnknown)
   ['{788C8743-E57F-439D-A468-5BC77F2E59C6}']
   function Get_count : Integer; stdcall;
    // Item :  
   function Item(lIndex:Integer):HRESULT;stdcall;
    // add :  
   function add(bstrFolder:WideString):HRESULT;stdcall;
    // remove :  
   function remove(lIndex:Integer):HRESULT;stdcall;
   function Get_scanState : WMPFolderScanState; stdcall;
   function Get_currentFolder : WideString; stdcall;
   function Get_scannedFilesCount : Integer; stdcall;
   function Get_addedFilesCount : Integer; stdcall;
   function Get_updateProgress : Integer; stdcall;
    // startScan :  
   function startScan:HRESULT;stdcall;
    // stopScan :  
   function stopScan:HRESULT;stdcall;
    // count :  
   property count:Integer read Get_count;
    // scanState :  
   property scanState:WMPFolderScanState read Get_scanState;
    // currentFolder :  
   property currentFolder:WideString read Get_currentFolder;
    // scannedFilesCount :  
   property scannedFilesCount:Integer read Get_scannedFilesCount;
    // addedFilesCount :  
   property addedFilesCount:Integer read Get_addedFilesCount;
    // updateProgress :  
   property updateProgress:Integer read Get_updateProgress;
  end;


// IWMPSyncDevice2 : IWMPSyncDevice2: Public interface for Windows Media Player SDK.

 IWMPSyncDevice2 = interface(IWMPSyncDevice)
   ['{88AFB4B2-140A-44D2-91E6-4543DA467CD1}']
    // setItemInfo :  
   function setItemInfo(bstrItemName:WideString;bstrVal:WideString):HRESULT;stdcall;
  end;


// IWMPSyncDevice3 : IWMPSyncDevice3: Public interface for Windows Media Player SDK.

 IWMPSyncDevice3 = interface(IWMPSyncDevice2)
   ['{B22C85F9-263C-4372-A0DA-B518DB9B4098}']
    // estimateSyncSize :  
   function estimateSyncSize(pNonRulePlaylist:IWMPPlaylist;pRulesPlaylist:IWMPPlaylist):HRESULT;stdcall;
    // cancelEstimation :  
   function cancelEstimation:HRESULT;stdcall;
  end;


// IWMPPlaylistCtrl : IWMPPlaylistCtrl: Public interface for skin object model.

 IWMPPlaylistCtrl = interface(IDispatch)
   ['{5F9CFD92-8CAD-11D3-9A7E-00C04F8EFB70}']
   function Get_Playlist : IWMPPlaylist; safecall;
   procedure Set_Playlist(const ppdispPlaylist:IWMPPlaylist); safecall;
   function Get_columns : WideString; safecall;
   procedure Set_columns(const pbstrColumns:WideString); safecall;
   function Get_columnCount : Integer; safecall;
   function Get_columnOrder : WideString; safecall;
   procedure Set_columnOrder(const pbstrColumnOrder:WideString); safecall;
   function Get_columnsVisible : WordBool; safecall;
   procedure Set_columnsVisible(const pVal:WordBool); safecall;
   function Get_dropDownVisible : WordBool; safecall;
   procedure Set_dropDownVisible(const pVal:WordBool); safecall;
   function Get_playlistItemsVisible : WordBool; safecall;
   procedure Set_playlistItemsVisible(const pVal:WordBool); safecall;
   function Get_checkboxesVisible : WordBool; safecall;
   procedure Set_checkboxesVisible(const pVal:WordBool); safecall;
   function Get_backgroundColor : WideString; safecall;
   procedure Set_backgroundColor(const pbstrColor:WideString); safecall;
   function Get_foregroundColor : WideString; safecall;
   procedure Set_foregroundColor(const pbstrColor:WideString); safecall;
   function Get_disabledItemColor : WideString; safecall;
   procedure Set_disabledItemColor(const pbstrColor:WideString); safecall;
   function Get_itemPlayingColor : WideString; safecall;
   procedure Set_itemPlayingColor(const pbstrColor:WideString); safecall;
   function Get_itemPlayingBackgroundColor : WideString; safecall;
   procedure Set_itemPlayingBackgroundColor(const pbstrBackgroundColor:WideString); safecall;
   function Get_backgroundImage : WideString; safecall;
   procedure Set_backgroundImage(const pbstrImage:WideString); safecall;
   function Get_allowItemEditing : WordBool; safecall;
   procedure Set_allowItemEditing(const pVal:WordBool); safecall;
   function Get_allowColumnSorting : WordBool; safecall;
   procedure Set_allowColumnSorting(const pVal:WordBool); safecall;
   function Get_dropDownList : WideString; safecall;
   procedure Set_dropDownList(const pbstrList:WideString); safecall;
   function Get_dropDownToolTip : WideString; safecall;
   procedure Set_dropDownToolTip(const pbstrToolTip:WideString); safecall;
   function Get_copying : WordBool; safecall;
   procedure Set_copying(const pVal:WordBool); safecall;
    // copy : method copy 
   procedure copy;safecall;
    // abortCopy : method abortCopy 
   procedure abortCopy;safecall;
    // deleteSelected : method deleteSelected 
   procedure deleteSelected;safecall;
    // deleteSelectedFromLibrary : method deleteSelectedFromLibrary 
   procedure deleteSelectedFromLibrary;safecall;
    // moveSelectedUp : method moveSelectedUp 
   procedure moveSelectedUp;safecall;
    // moveSelectedDown : method moveSelectedDown 
   procedure moveSelectedDown;safecall;
    // addSelectedToPlaylist : method addSelectedToPlaylist 
   procedure addSelectedToPlaylist(pdispPlaylist:IWMPPlaylist);safecall;
    // getNextSelectedItem : method getNextSelectedItem 
   function getNextSelectedItem(nStartIndex:Integer):Integer;safecall;
    // getNextCheckedItem : method getNextCheckedItem 
   function getNextCheckedItem(nStartIndex:Integer):Integer;safecall;
    // setSelectedState : method setSelectedState 
   procedure setSelectedState(nIndex:Integer;vbSelected:WordBool);safecall;
    // setCheckedState : method setCheckedState 
   procedure setCheckedState(nIndex:Integer;vbChecked:WordBool);safecall;
    // sortColumn : method sortColumn 
   procedure sortColumn(nIndex:Integer);safecall;
    // setColumnResizeMode : method setColumnResizeMode 
   procedure setColumnResizeMode(nIndex:Integer;newMode:WideString);safecall;
    // setColumnWidth : method setColumnWidth 
   procedure setColumnWidth(nIndex:Integer;nWidth:Integer);safecall;
   function Get_itemErrorColor : WideString; safecall;
   procedure Set_itemErrorColor(const pbstrColor:WideString); safecall;
   function Get_itemCount : Integer; safecall;
   function Get_itemMedia(nIndex:Integer) : IWMPMedia; safecall;
   function Get_itemPlaylist(nIndex:Integer) : IWMPPlaylist; safecall;
    // getNextSelectedItem2 : method getNextSelectedItem2 
   function getNextSelectedItem2(nStartIndex:Integer):Integer;safecall;
    // getNextCheckedItem2 : method getNextCheckedItem2 
   function getNextCheckedItem2(nStartIndex:Integer):Integer;safecall;
    // setSelectedState2 : method setSelectedState2 
   procedure setSelectedState2(nIndex:Integer;vbSelected:WordBool);safecall;
    // setCheckedState2 : method setCheckedState2 
   procedure setCheckedState2(nIndex:Integer;vbChecked:WordBool);safecall;
   function Get_leftStatus : WideString; safecall;
   procedure Set_leftStatus(const pbstrStatus:WideString); safecall;
   function Get_rightStatus : WideString; safecall;
   procedure Set_rightStatus(const pbstrStatus:WideString); safecall;
   function Get_editButtonVisible : WordBool; safecall;
   procedure Set_editButtonVisible(const pVal:WordBool); safecall;
   function Get_dropDownImage : WideString; safecall;
   procedure Set_dropDownImage(const pbstrImage:WideString); safecall;
   function Get_dropDownBackgroundImage : WideString; safecall;
   procedure Set_dropDownBackgroundImage(const pbstrImage:WideString); safecall;
   function Get_hueShift : Single; safecall;
   procedure Set_hueShift(const pVal:Single); safecall;
   function Get_saturation : Single; safecall;
   procedure Set_saturation(const pVal:Single); safecall;
   function Get_statusColor : WideString; safecall;
   procedure Set_statusColor(const pbstrColor:WideString); safecall;
   function Get_toolbarVisible : WordBool; safecall;
   procedure Set_toolbarVisible(const pVal:WordBool); safecall;
   function Get_itemSelectedColor : WideString; safecall;
   procedure Set_itemSelectedColor(const pbstrColor:WideString); safecall;
   function Get_itemSelectedFocusLostColor : WideString; safecall;
   procedure Set_itemSelectedFocusLostColor(const pbstrFocusLostColor:WideString); safecall;
   function Get_itemSelectedBackgroundColor : WideString; safecall;
   procedure Set_itemSelectedBackgroundColor(const pbstrColor:WideString); safecall;
   function Get_itemSelectedBackgroundFocusLostColor : WideString; safecall;
   procedure Set_itemSelectedBackgroundFocusLostColor(const pbstrFocusLostColor:WideString); safecall;
   function Get_backgroundSplitColor : WideString; safecall;
   procedure Set_backgroundSplitColor(const pbstrColor:WideString); safecall;
   function Get_statusTextColor : WideString; safecall;
   procedure Set_statusTextColor(const pbstrColor:WideString); safecall;
    // Playlist : property playlist 
   property Playlist:IWMPPlaylist read Get_Playlist write Set_Playlist;
    // columns : property columns 
   property columns:WideString read Get_columns write Set_columns;
    // columnCount : property columnCount 
   property columnCount:Integer read Get_columnCount;
    // columnOrder : property columnOrder 
   property columnOrder:WideString read Get_columnOrder write Set_columnOrder;
    // columnsVisible : property columnsVisible 
   property columnsVisible:WordBool read Get_columnsVisible write Set_columnsVisible;
    // dropDownVisible : property dropDownVisible 
   property dropDownVisible:WordBool read Get_dropDownVisible write Set_dropDownVisible;
    // playlistItemsVisible : property playlistItemsVisible 
   property playlistItemsVisible:WordBool read Get_playlistItemsVisible write Set_playlistItemsVisible;
    // checkboxesVisible : property checkboxesVisible 
   property checkboxesVisible:WordBool read Get_checkboxesVisible write Set_checkboxesVisible;
    // backgroundColor : property backgroundColor 
   property backgroundColor:WideString read Get_backgroundColor write Set_backgroundColor;
    // foregroundColor : property foregroundColor 
   property foregroundColor:WideString read Get_foregroundColor write Set_foregroundColor;
    // disabledItemColor : property disabledItemColor 
   property disabledItemColor:WideString read Get_disabledItemColor write Set_disabledItemColor;
    // itemPlayingColor : property itemPlayingColor 
   property itemPlayingColor:WideString read Get_itemPlayingColor write Set_itemPlayingColor;
    // itemPlayingBackgroundColor : property itemPlayingBackgroundColor 
   property itemPlayingBackgroundColor:WideString read Get_itemPlayingBackgroundColor write Set_itemPlayingBackgroundColor;
    // backgroundImage : property backgroundImage 
   property backgroundImage:WideString read Get_backgroundImage write Set_backgroundImage;
    // allowItemEditing : property allowItemEditing 
   property allowItemEditing:WordBool read Get_allowItemEditing write Set_allowItemEditing;
    // allowColumnSorting : property allowColumnSorting 
   property allowColumnSorting:WordBool read Get_allowColumnSorting write Set_allowColumnSorting;
    // dropDownList : property dropDownList 
   property dropDownList:WideString read Get_dropDownList write Set_dropDownList;
    // dropDownToolTip : property dropDownToolTip 
   property dropDownToolTip:WideString read Get_dropDownToolTip write Set_dropDownToolTip;
    // copying : property copying 
   property copying:WordBool read Get_copying write Set_copying;
    // itemErrorColor : property itemErrorColor 
   property itemErrorColor:WideString read Get_itemErrorColor write Set_itemErrorColor;
    // itemCount : property itemCount 
   property itemCount:Integer read Get_itemCount;
    // itemMedia : property itemMedia 
   property itemMedia[nIndex:Integer]:IWMPMedia read Get_itemMedia;
    // itemPlaylist : property itemPlaylist 
   property itemPlaylist[nIndex:Integer]:IWMPPlaylist read Get_itemPlaylist;
    // leftStatus : property leftStatus 
   property leftStatus:WideString read Get_leftStatus write Set_leftStatus;
    // rightStatus : property rightStatus 
   property rightStatus:WideString read Get_rightStatus write Set_rightStatus;
    // editButtonVisible : property editButtonVisible 
   property editButtonVisible:WordBool read Get_editButtonVisible write Set_editButtonVisible;
    // dropDownImage : property dropDownImage 
   property dropDownImage:WideString read Get_dropDownImage write Set_dropDownImage;
    // dropDownBackgroundImage : property dropDownBackgroundImage 
   property dropDownBackgroundImage:WideString read Get_dropDownBackgroundImage write Set_dropDownBackgroundImage;
    // hueShift : property hueShift 
   property hueShift:Single read Get_hueShift write Set_hueShift;
    // saturation : property saturation 
   property saturation:Single read Get_saturation write Set_saturation;
    // statusColor : property statusColor 
   property statusColor:WideString read Get_statusColor write Set_statusColor;
    // toolbarVisible : property toolbarVisible 
   property toolbarVisible:WordBool read Get_toolbarVisible write Set_toolbarVisible;
    // itemSelectedColor : property itemSelectedColor 
   property itemSelectedColor:WideString read Get_itemSelectedColor write Set_itemSelectedColor;
    // itemSelectedFocusLostColor : property itemSelectedFocusLostColor 
   property itemSelectedFocusLostColor:WideString read Get_itemSelectedFocusLostColor write Set_itemSelectedFocusLostColor;
    // itemSelectedBackgroundColor : property itemSelectedBackgroundColor 
   property itemSelectedBackgroundColor:WideString read Get_itemSelectedBackgroundColor write Set_itemSelectedBackgroundColor;
    // itemSelectedBackgroundFocusLostColor : property itemSelectedBackgroundFocusLostColor 
   property itemSelectedBackgroundFocusLostColor:WideString read Get_itemSelectedBackgroundFocusLostColor write Set_itemSelectedBackgroundFocusLostColor;
    // backgroundSplitColor : property backgroundSplitColor 
   property backgroundSplitColor:WideString read Get_backgroundSplitColor write Set_backgroundSplitColor;
    // statusTextColor : property statusTextColor 
   property statusTextColor:WideString read Get_statusTextColor write Set_statusTextColor;
  end;


// IWMPPlaylistCtrl : IWMPPlaylistCtrl: Public interface for skin object model.

 IWMPPlaylistCtrlDisp = dispinterface
   ['{5F9CFD92-8CAD-11D3-9A7E-00C04F8EFB70}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // copy : method copy 
   procedure copy;dispid 5623;
    // abortCopy : method abortCopy 
   procedure abortCopy;dispid 5624;
    // deleteSelected : method deleteSelected 
   procedure deleteSelected;dispid 5625;
    // deleteSelectedFromLibrary : method deleteSelectedFromLibrary 
   procedure deleteSelectedFromLibrary;dispid 5626;
    // moveSelectedUp : method moveSelectedUp 
   procedure moveSelectedUp;dispid 5628;
    // moveSelectedDown : method moveSelectedDown 
   procedure moveSelectedDown;dispid 5629;
    // addSelectedToPlaylist : method addSelectedToPlaylist 
   procedure addSelectedToPlaylist(pdispPlaylist:IWMPPlaylist);dispid 5630;
    // getNextSelectedItem : method getNextSelectedItem 
   function getNextSelectedItem(nStartIndex:Integer):Integer;dispid 5631;
    // getNextCheckedItem : method getNextCheckedItem 
   function getNextCheckedItem(nStartIndex:Integer):Integer;dispid 5632;
    // setSelectedState : method setSelectedState 
   procedure setSelectedState(nIndex:Integer;vbSelected:WordBool);dispid 5633;
    // setCheckedState : method setCheckedState 
   procedure setCheckedState(nIndex:Integer;vbChecked:WordBool);dispid 5634;
    // sortColumn : method sortColumn 
   procedure sortColumn(nIndex:Integer);dispid 5635;
    // setColumnResizeMode : method setColumnResizeMode 
   procedure setColumnResizeMode(nIndex:Integer;newMode:WideString);dispid 5636;
    // setColumnWidth : method setColumnWidth 
   procedure setColumnWidth(nIndex:Integer;nWidth:Integer);dispid 5637;
    // getNextSelectedItem2 : method getNextSelectedItem2 
   function getNextSelectedItem2(nStartIndex:Integer):Integer;dispid 5646;
    // getNextCheckedItem2 : method getNextCheckedItem2 
   function getNextCheckedItem2(nStartIndex:Integer):Integer;dispid 5647;
    // setSelectedState2 : method setSelectedState2 
   procedure setSelectedState2(nIndex:Integer;vbSelected:WordBool);dispid 5648;
    // setCheckedState2 : method setCheckedState2 
   procedure setCheckedState2(nIndex:Integer;vbChecked:WordBool);dispid 5649;
    // Playlist : property playlist 
   property Playlist:IWMPPlaylist dispid 5601;
    // columns : property columns 
   property columns:WideString dispid 5602;
    // columnCount : property columnCount 
   property columnCount:Integer  readonly dispid 5603;
    // columnOrder : property columnOrder 
   property columnOrder:WideString dispid 5604;
    // columnsVisible : property columnsVisible 
   property columnsVisible:WordBool dispid 5605;
    // dropDownVisible : property dropDownVisible 
   property dropDownVisible:WordBool dispid 5607;
    // playlistItemsVisible : property playlistItemsVisible 
   property playlistItemsVisible:WordBool dispid 5608;
    // checkboxesVisible : property checkboxesVisible 
   property checkboxesVisible:WordBool dispid 5609;
    // backgroundColor : property backgroundColor 
   property backgroundColor:WideString dispid 5612;
    // foregroundColor : property foregroundColor 
   property foregroundColor:WideString dispid 5613;
    // disabledItemColor : property disabledItemColor 
   property disabledItemColor:WideString dispid 5614;
    // itemPlayingColor : property itemPlayingColor 
   property itemPlayingColor:WideString dispid 5615;
    // itemPlayingBackgroundColor : property itemPlayingBackgroundColor 
   property itemPlayingBackgroundColor:WideString dispid 5616;
    // backgroundImage : property backgroundImage 
   property backgroundImage:WideString dispid 5617;
    // allowItemEditing : property allowItemEditing 
   property allowItemEditing:WordBool dispid 5618;
    // allowColumnSorting : property allowColumnSorting 
   property allowColumnSorting:WordBool dispid 5619;
    // dropDownList : property dropDownList 
   property dropDownList:WideString dispid 5620;
    // dropDownToolTip : property dropDownToolTip 
   property dropDownToolTip:WideString dispid 5621;
    // copying : property copying 
   property copying:WordBool dispid 5622;
    // itemErrorColor : property itemErrorColor 
   property itemErrorColor:WideString dispid 5642;
    // itemCount : property itemCount 
   property itemCount:Integer  readonly dispid 5643;
    // itemMedia : property itemMedia 
   property itemMedia[nIndex:Integer]:IWMPMedia  readonly dispid 5644;
    // itemPlaylist : property itemPlaylist 
   property itemPlaylist[nIndex:Integer]:IWMPPlaylist  readonly dispid 5645;
    // leftStatus : property leftStatus 
   property leftStatus:WideString dispid 5650;
    // rightStatus : property rightStatus 
   property rightStatus:WideString dispid 5651;
    // editButtonVisible : property editButtonVisible 
   property editButtonVisible:WordBool dispid 5652;
    // dropDownImage : property dropDownImage 
   property dropDownImage:WideString dispid 5653;
    // dropDownBackgroundImage : property dropDownBackgroundImage 
   property dropDownBackgroundImage:WideString dispid 5654;
    // hueShift : property hueShift 
   property hueShift:Single dispid 5655;
    // saturation : property saturation 
   property saturation:Single dispid 5656;
    // statusColor : property statusColor 
   property statusColor:WideString dispid 5658;
    // toolbarVisible : property toolbarVisible 
   property toolbarVisible:WordBool dispid 5660;
    // itemSelectedColor : property itemSelectedColor 
   property itemSelectedColor:WideString dispid 5662;
    // itemSelectedFocusLostColor : property itemSelectedFocusLostColor 
   property itemSelectedFocusLostColor:WideString dispid 5663;
    // itemSelectedBackgroundColor : property itemSelectedBackgroundColor 
   property itemSelectedBackgroundColor:WideString dispid 5664;
    // itemSelectedBackgroundFocusLostColor : property itemSelectedBackgroundFocusLostColor 
   property itemSelectedBackgroundFocusLostColor:WideString dispid 5665;
    // backgroundSplitColor : property backgroundSplitColor 
   property backgroundSplitColor:WideString dispid 5666;
    // statusTextColor : property statusTextColor 
   property statusTextColor:WideString dispid 5667;
  end;


// IAppDispatch : IAppDispatch: Not Public.  Internal interface used by Windows Media Player.

 IAppDispatch = interface(IDispatch)
   ['{E41C88DD-2364-4FF7-A0F5-CA9859AF783F}']
   function Get_titlebarVisible : WordBool; safecall;
   procedure Set_titlebarVisible(const pVal:WordBool); safecall;
   function Get_titlebarAutoHide : WordBool; safecall;
   procedure Set_titlebarAutoHide(const pVal:WordBool); safecall;
   function Get_currentTask : WideString; safecall;
   procedure Set_currentTask(const pVal:WideString); safecall;
   function Get_libraryBasketMode : Integer; safecall;
   procedure Set_libraryBasketMode(const pVal:Integer); safecall;
   function Get_libraryBasketWidth : Integer; safecall;
   function Get_breadcrumbItemCount : Integer; safecall;
   function Get_breadcrumbItemName(lIndex:Integer) : WideString; safecall;
   function Get_breadcrumbItemHasMenu(lIndex:Integer) : WordBool; safecall;
    // breadcrumbItemClick :  
   procedure breadcrumbItemClick(lIndex:Integer);safecall;
   function Get_settingsVisible : WordBool; safecall;
   procedure Set_settingsVisible(const pVal:WordBool); safecall;
   function Get_playlistVisible : WordBool; safecall;
   procedure Set_playlistVisible(const pVal:WordBool); safecall;
    // gotoSkinMode :  
   procedure gotoSkinMode;safecall;
    // gotoPlayerMode :  
   procedure gotoPlayerMode;safecall;
    // gotoLibraryMode :  
   procedure gotoLibraryMode(lButton:Integer);safecall;
    // navigatePrevious :  
   procedure navigatePrevious;safecall;
    // navigateNext :  
   procedure navigateNext;safecall;
    // goFullScreen :  
   procedure goFullScreen;safecall;
   function Get_fullScreenEnabled : WordBool; safecall;
   function Get_serviceLoginVisible : WordBool; safecall;
   function Get_serviceLoginSignedIn : WordBool; safecall;
    // serviceLogin :  
   procedure serviceLogin;safecall;
    // serviceLogout :  
   procedure serviceLogout;safecall;
   function Get_serviceGetInfo(bstrItem:WideString) : OleVariant; safecall;
   function Get_navigatePreviousEnabled : WordBool; safecall;
   function Get_navigateNextEnabled : WordBool; safecall;
    // navigateToAddress :  
   procedure navigateToAddress(address:WideString);safecall;
   function Get_glassEnabled : WordBool; safecall;
   function Get_inVistaPlus : WordBool; safecall;
    // adjustLeft :  
   procedure adjustLeft(nDistance:Integer);safecall;
   function Get_taskbarVisible : WordBool; safecall;
   procedure Set_taskbarVisible(const pVal:WordBool); safecall;
   function Get_DPI : Integer; safecall;
   function Get_previousEnabled : WordBool; safecall;
   function Get_playLibraryItemEnabled : WordBool; safecall;
    // previous :  
   procedure previous;safecall;
   function Get_titlebarCurrentlyVisible : WordBool; safecall;
   function Get_menubarCurrentlyVisible : WordBool; safecall;
   function Get_bgPluginRunning : WordBool; safecall;
    // configurePlugins :  
   procedure configurePlugins(nType:Integer);safecall;
    // getTimeString : method getTimeString 
   function getTimeString(dTime:Double):WideString;safecall;
   function Get_maximized : WordBool; safecall;
   function Get_top : Integer; safecall;
   procedure Set_top(const pVal:Integer); safecall;
   function Get_left : Integer; safecall;
   procedure Set_left(const pVal:Integer); safecall;
   function Get_width : Integer; safecall;
   procedure Set_width(const pVal:Integer); safecall;
   function Get_height : Integer; safecall;
   procedure Set_height(const pVal:Integer); safecall;
    // setWindowPos :  
   procedure setWindowPos(lTop:Integer;lLeft:Integer;lWidth:Integer;lHeight:Integer);safecall;
    // logData :  
   procedure logData(ID:WideString;data:WideString);safecall;
   function Get_powerPersonality : WideString; safecall;
    // navigateNamespace :  
   procedure navigateNamespace(address:WideString);safecall;
   function Get_exclusiveService : WideString; safecall;
   procedure Set_windowText(const Param1:WideString); safecall;
   function Get_resourceIdForDpi(iResourceId:SYSINT) : SYSINT; safecall;
    // titlebarVisible :  
   property titlebarVisible:WordBool read Get_titlebarVisible write Set_titlebarVisible;
    // titlebarAutoHide :  
   property titlebarAutoHide:WordBool read Get_titlebarAutoHide write Set_titlebarAutoHide;
    // currentTask :  
   property currentTask:WideString read Get_currentTask write Set_currentTask;
    // libraryBasketMode :  
   property libraryBasketMode:Integer read Get_libraryBasketMode write Set_libraryBasketMode;
    // libraryBasketWidth :  
   property libraryBasketWidth:Integer read Get_libraryBasketWidth;
    // breadcrumbItemCount :  
   property breadcrumbItemCount:Integer read Get_breadcrumbItemCount;
    // breadcrumbItemName :  
   property breadcrumbItemName[lIndex:Integer]:WideString read Get_breadcrumbItemName;
    // breadcrumbItemHasMenu :  
   property breadcrumbItemHasMenu[lIndex:Integer]:WordBool read Get_breadcrumbItemHasMenu;
    // settingsVisible :  
   property settingsVisible:WordBool read Get_settingsVisible write Set_settingsVisible;
    // playlistVisible :  
   property playlistVisible:WordBool read Get_playlistVisible write Set_playlistVisible;
    // fullScreenEnabled :  
   property fullScreenEnabled:WordBool read Get_fullScreenEnabled;
    // serviceLoginVisible :  
   property serviceLoginVisible:WordBool read Get_serviceLoginVisible;
    // serviceLoginSignedIn :  
   property serviceLoginSignedIn:WordBool read Get_serviceLoginSignedIn;
    // serviceGetInfo :  
   property serviceGetInfo[bstrItem:WideString]:OleVariant read Get_serviceGetInfo;
    // navigatePreviousEnabled :  
   property navigatePreviousEnabled:WordBool read Get_navigatePreviousEnabled;
    // navigateNextEnabled :  
   property navigateNextEnabled:WordBool read Get_navigateNextEnabled;
    // glassEnabled :  
   property glassEnabled:WordBool read Get_glassEnabled;
    // inVistaPlus :  
   property inVistaPlus:WordBool read Get_inVistaPlus;
    // taskbarVisible :  
   property taskbarVisible:WordBool read Get_taskbarVisible write Set_taskbarVisible;
    // DPI :  
   property DPI:Integer read Get_DPI;
    // previousEnabled :  
   property previousEnabled:WordBool read Get_previousEnabled;
    // playLibraryItemEnabled :  
   property playLibraryItemEnabled:WordBool read Get_playLibraryItemEnabled;
    // titlebarCurrentlyVisible :  
   property titlebarCurrentlyVisible:WordBool read Get_titlebarCurrentlyVisible;
    // menubarCurrentlyVisible :  
   property menubarCurrentlyVisible:WordBool read Get_menubarCurrentlyVisible;
    // bgPluginRunning :  
   property bgPluginRunning:WordBool read Get_bgPluginRunning;
    // maximized :  
   property maximized:WordBool read Get_maximized;
    // top :  
   property top:Integer read Get_top write Set_top;
    // left :  
   property left:Integer read Get_left write Set_left;
    // width :  
   property width:Integer read Get_width write Set_width;
    // height :  
   property height:Integer read Get_height write Set_height;
    // powerPersonality :  
   property powerPersonality:WideString read Get_powerPersonality;
    // exclusiveService :  
   property exclusiveService:WideString read Get_exclusiveService;
    // windowText :  
   property windowText:WideString write Set_windowText;
    // resourceIdForDpi :  
   property resourceIdForDpi[iResourceId:SYSINT]:SYSINT read Get_resourceIdForDpi;
  end;


// IAppDispatch : IAppDispatch: Not Public.  Internal interface used by Windows Media Player.

 IAppDispatchDisp = dispinterface
   ['{E41C88DD-2364-4FF7-A0F5-CA9859AF783F}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // breadcrumbItemClick :  
   procedure breadcrumbItemClick(lIndex:Integer);dispid 150;
    // gotoSkinMode :  
   procedure gotoSkinMode;dispid 105;
    // gotoPlayerMode :  
   procedure gotoPlayerMode;dispid 143;
    // gotoLibraryMode :  
   procedure gotoLibraryMode(lButton:Integer);dispid 144;
    // navigatePrevious :  
   procedure navigatePrevious;dispid 125;
    // navigateNext :  
   procedure navigateNext;dispid 126;
    // goFullScreen :  
   procedure goFullScreen;dispid 142;
    // serviceLogin :  
   procedure serviceLogin;dispid 134;
    // serviceLogout :  
   procedure serviceLogout;dispid 135;
    // navigateToAddress :  
   procedure navigateToAddress(address:WideString);dispid 130;
    // adjustLeft :  
   procedure adjustLeft(nDistance:Integer);dispid 106;
    // previous :  
   procedure previous;dispid 115;
    // configurePlugins :  
   procedure configurePlugins(nType:Integer);dispid 110;
    // getTimeString : method getTimeString 
   function getTimeString(dTime:Double):WideString;dispid 111;
    // setWindowPos :  
   procedure setWindowPos(lTop:Integer;lLeft:Integer;lWidth:Integer;lHeight:Integer);dispid 121;
    // logData :  
   procedure logData(ID:WideString;data:WideString);dispid 122;
    // navigateNamespace :  
   procedure navigateNamespace(address:WideString);dispid 128;
    // titlebarVisible :  
   property titlebarVisible:WordBool dispid 100;
    // titlebarAutoHide :  
   property titlebarAutoHide:WordBool dispid 101;
    // currentTask :  
   property currentTask:WideString dispid 102;
    // libraryBasketMode :  
   property libraryBasketMode:Integer dispid 145;
    // libraryBasketWidth :  
   property libraryBasketWidth:Integer  readonly dispid 146;
    // breadcrumbItemCount :  
   property breadcrumbItemCount:Integer  readonly dispid 147;
    // breadcrumbItemName :  
   property breadcrumbItemName[lIndex:Integer]:WideString  readonly dispid 148;
    // breadcrumbItemHasMenu :  
   property breadcrumbItemHasMenu[lIndex:Integer]:WordBool  readonly dispid 149;
    // settingsVisible :  
   property settingsVisible:WordBool dispid 103;
    // playlistVisible :  
   property playlistVisible:WordBool dispid 104;
    // fullScreenEnabled :  
   property fullScreenEnabled:WordBool  readonly dispid 141;
    // serviceLoginVisible :  
   property serviceLoginVisible:WordBool  readonly dispid 132;
    // serviceLoginSignedIn :  
   property serviceLoginSignedIn:WordBool  readonly dispid 133;
    // serviceGetInfo :  
   property serviceGetInfo[bstrItem:WideString]:OleVariant  readonly dispid 140;
    // navigatePreviousEnabled :  
   property navigatePreviousEnabled:WordBool  readonly dispid 123;
    // navigateNextEnabled :  
   property navigateNextEnabled:WordBool  readonly dispid 124;
    // glassEnabled :  
   property glassEnabled:WordBool  readonly dispid 131;
    // inVistaPlus :  
   property inVistaPlus:WordBool  readonly dispid 136;
    // taskbarVisible :  
   property taskbarVisible:WordBool dispid 107;
    // DPI :  
   property DPI:Integer  readonly dispid 116;
    // previousEnabled :  
   property previousEnabled:WordBool  readonly dispid 114;
    // playLibraryItemEnabled :  
   property playLibraryItemEnabled:WordBool  readonly dispid 139;
    // titlebarCurrentlyVisible :  
   property titlebarCurrentlyVisible:WordBool  readonly dispid 108;
    // menubarCurrentlyVisible :  
   property menubarCurrentlyVisible:WordBool  readonly dispid 137;
    // bgPluginRunning :  
   property bgPluginRunning:WordBool  readonly dispid 109;
    // maximized :  
   property maximized:WordBool  readonly dispid 113;
    // top :  
   property top:Integer dispid 117;
    // left :  
   property left:Integer dispid 118;
    // width :  
   property width:Integer dispid 119;
    // height :  
   property height:Integer dispid 120;
    // powerPersonality :  
   property powerPersonality:WideString  readonly dispid 127;
    // exclusiveService :  
   property exclusiveService:WideString  readonly dispid 129;
    // windowText :  
   property windowText:WideString writeonly dispid 138;
    // resourceIdForDpi :  
   property resourceIdForDpi[iResourceId:SYSINT]:SYSINT  readonly dispid 151;
  end;


// IWMPSafeBrowser : IWMPSafeBrowser: Not Public.  Internal interface used by Windows Media Player.

 IWMPSafeBrowser = interface(IDispatch)
   ['{EF870383-83AB-4EA9-BE48-56FA4251AF10}']
   function Get_URL : WideString; safecall;
   procedure Set_URL(const pVal:WideString); safecall;
   function Get_status : Integer; safecall;
   function Get_pendingDownloads : Integer; safecall;
    // showSAMIText : method showSAMIText 
   procedure showSAMIText(samiText:WideString);safecall;
    // showLyrics : method showLyrics 
   procedure showLyrics(lyrics:WideString);safecall;
    // loadSpecialPage : loads one of our special pages by name 
   procedure loadSpecialPage(pageName:WideString);safecall;
    // goBack : go back to the previous page 
   procedure goBack;safecall;
    // goForward : go forward through the current MRU 
   procedure goForward;safecall;
    // stop : stop loading page 
   procedure stop;safecall;
    // refresh : refresh the page 
   procedure refresh;safecall;
   function Get_baseURL : WideString; safecall;
   function Get_fullURL : WideString; safecall;
   function Get_secureLock : Integer; safecall;
   function Get_busy : WordBool; safecall;
    // showCert : show security certificate dialog 
   procedure showCert;safecall;
    // URL :  
   property URL:WideString read Get_URL write Set_URL;
    // status :  
   property status:Integer read Get_status;
    // pendingDownloads :  
   property pendingDownloads:Integer read Get_pendingDownloads;
    // baseURL :  
   property baseURL:WideString read Get_baseURL;
    // fullURL :  
   property fullURL:WideString read Get_fullURL;
    // secureLock :  
   property secureLock:Integer read Get_secureLock;
    // busy :  
   property busy:WordBool read Get_busy;
  end;


// IWMPSafeBrowser : IWMPSafeBrowser: Not Public.  Internal interface used by Windows Media Player.

 IWMPSafeBrowserDisp = dispinterface
   ['{EF870383-83AB-4EA9-BE48-56FA4251AF10}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // showSAMIText : method showSAMIText 
   procedure showSAMIText(samiText:WideString);dispid 8403;
    // showLyrics : method showLyrics 
   procedure showLyrics(lyrics:WideString);dispid 8404;
    // loadSpecialPage : loads one of our special pages by name 
   procedure loadSpecialPage(pageName:WideString);dispid 8405;
    // goBack : go back to the previous page 
   procedure goBack;dispid 8406;
    // goForward : go forward through the current MRU 
   procedure goForward;dispid 8407;
    // stop : stop loading page 
   procedure stop;dispid 8408;
    // refresh : refresh the page 
   procedure refresh;dispid 8409;
    // showCert : show security certificate dialog 
   procedure showCert;dispid 8413;
    // URL :  
   property URL:WideString dispid 8400;
    // status :  
   property status:Integer  readonly dispid 8401;
    // pendingDownloads :  
   property pendingDownloads:Integer  readonly dispid 8402;
    // baseURL :  
   property baseURL:WideString  readonly dispid 8410;
    // fullURL :  
   property fullURL:WideString  readonly dispid 8414;
    // secureLock :  
   property secureLock:Integer  readonly dispid 8411;
    // busy :  
   property busy:WordBool  readonly dispid 8412;
  end;


// IWMPObjectExtendedProps : IWMPObjectExtendedProps: Public interface for skin object model.

 IWMPObjectExtendedProps = interface(IDispatch)
   ['{21D077C1-4BAA-11D3-BD45-00C04F6EA5AE}']
   function Get_ID : WideString; safecall;
   function Get_elementType : WideString; safecall;
   function Get_left : Integer; safecall;
   procedure Set_left(const pVal:Integer); safecall;
   function Get_top : Integer; safecall;
   procedure Set_top(const pVal:Integer); safecall;
   function Get_right : Integer; safecall;
   procedure Set_right(const pVal:Integer); safecall;
   function Get_bottom : Integer; safecall;
   procedure Set_bottom(const pVal:Integer); safecall;
   function Get_width : Integer; safecall;
   procedure Set_width(const pVal:Integer); safecall;
   function Get_height : Integer; safecall;
   procedure Set_height(const pVal:Integer); safecall;
   function Get_zIndex : Integer; safecall;
   procedure Set_zIndex(const pVal:Integer); safecall;
   function Get_clippingImage : WideString; safecall;
   procedure Set_clippingImage(const pVal:WideString); safecall;
   function Get_clippingColor : WideString; safecall;
   procedure Set_clippingColor(const pVal:WideString); safecall;
   function Get_visible : WordBool; safecall;
   procedure Set_visible(const pVal:WordBool); safecall;
   function Get_enabled : WordBool; safecall;
   procedure Set_enabled(const pVal:WordBool); safecall;
   function Get_tabStop : WordBool; safecall;
   procedure Set_tabStop(const pVal:WordBool); safecall;
   function Get_passThrough : WordBool; safecall;
   procedure Set_passThrough(const pVal:WordBool); safecall;
   function Get_horizontalAlignment : WideString; safecall;
   procedure Set_horizontalAlignment(const pVal:WideString); safecall;
   function Get_verticalAlignment : WideString; safecall;
   procedure Set_verticalAlignment(const pVal:WideString); safecall;
    // moveTo : method moveTo 
   procedure moveTo(newX:Integer;newY:Integer;moveTime:Integer);safecall;
    // slideTo : method slideTo 
   procedure slideTo(newX:Integer;newY:Integer;moveTime:Integer);safecall;
    // moveSizeTo : method moveSizeTo 
   procedure moveSizeTo(newX:Integer;newY:Integer;newWidth:Integer;newHeight:Integer;moveTime:Integer;fSlide:WordBool);safecall;
   function Get_alphaBlend : Integer; safecall;
   procedure Set_alphaBlend(const pVal:Integer); safecall;
    // alphaBlendTo : method alphaBlendTo 
   procedure alphaBlendTo(newVal:Integer;alphaTime:Integer);safecall;
   function Get_accName : WideString; safecall;
   procedure Set_accName(const pszName:WideString); safecall;
   function Get_accDescription : WideString; safecall;
   procedure Set_accDescription(const pszDesc:WideString); safecall;
   function Get_accKeyboardShortcut : WideString; safecall;
   procedure Set_accKeyboardShortcut(const pszShortcut:WideString); safecall;
   function Get_resizeImages : WordBool; safecall;
   procedure Set_resizeImages(const pVal:WordBool); safecall;
   function Get_nineGridMargins : WideString; safecall;
   procedure Set_nineGridMargins(const pszMargins:WideString); safecall;
   function Get_resizeOptimize : WideString; safecall;
   procedure Set_resizeOptimize(const ppszResizeOptimize:WideString); safecall;
   function Get_rotation : Single; safecall;
   procedure Set_rotation(const pfVal:Single); safecall;
    // ID : property id 
   property ID:WideString read Get_ID;
    // elementType : property elementType 
   property elementType:WideString read Get_elementType;
    // left : property left 
   property left:Integer read Get_left write Set_left;
    // top : property top 
   property top:Integer read Get_top write Set_top;
    // right : property right 
   property right:Integer read Get_right write Set_right;
    // bottom : property bottom 
   property bottom:Integer read Get_bottom write Set_bottom;
    // width : property width 
   property width:Integer read Get_width write Set_width;
    // height : property height 
   property height:Integer read Get_height write Set_height;
    // zIndex : property zIndex 
   property zIndex:Integer read Get_zIndex write Set_zIndex;
    // clippingImage : property clippingImage 
   property clippingImage:WideString read Get_clippingImage write Set_clippingImage;
    // clippingColor : property clippingColor 
   property clippingColor:WideString read Get_clippingColor write Set_clippingColor;
    // visible : property visible 
   property visible:WordBool read Get_visible write Set_visible;
    // enabled : property enabled 
   property enabled:WordBool read Get_enabled write Set_enabled;
    // tabStop : property tabStop 
   property tabStop:WordBool read Get_tabStop write Set_tabStop;
    // passThrough : property passThrough 
   property passThrough:WordBool read Get_passThrough write Set_passThrough;
    // horizontalAlignment : property horizontalAlignment 
   property horizontalAlignment:WideString read Get_horizontalAlignment write Set_horizontalAlignment;
    // verticalAlignment : property verticalAlignment 
   property verticalAlignment:WideString read Get_verticalAlignment write Set_verticalAlignment;
    // alphaBlend : property alphaBlend 
   property alphaBlend:Integer read Get_alphaBlend write Set_alphaBlend;
    // accName : property accName 
   property accName:WideString read Get_accName write Set_accName;
    // accDescription : property accDescription 
   property accDescription:WideString read Get_accDescription write Set_accDescription;
    // accKeyboardShortcut : property accKeyboardShortcut	 
   property accKeyboardShortcut:WideString read Get_accKeyboardShortcut write Set_accKeyboardShortcut;
    // resizeImages : property resizeImages 
   property resizeImages:WordBool read Get_resizeImages write Set_resizeImages;
    // nineGridMargins : property nineGridMargins 
   property nineGridMargins:WideString read Get_nineGridMargins write Set_nineGridMargins;
    // resizeOptimize : property resizeOptimize 
   property resizeOptimize:WideString read Get_resizeOptimize write Set_resizeOptimize;
    // rotation : property rotation 
   property rotation:Single read Get_rotation write Set_rotation;
  end;


// IWMPObjectExtendedProps : IWMPObjectExtendedProps: Public interface for skin object model.

 IWMPObjectExtendedPropsDisp = dispinterface
   ['{21D077C1-4BAA-11D3-BD45-00C04F6EA5AE}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // moveTo : method moveTo 
   procedure moveTo(newX:Integer;newY:Integer;moveTime:Integer);dispid 2015;
    // slideTo : method slideTo 
   procedure slideTo(newX:Integer;newY:Integer;moveTime:Integer);dispid 2021;
    // moveSizeTo : method moveSizeTo 
   procedure moveSizeTo(newX:Integer;newY:Integer;newWidth:Integer;newHeight:Integer;moveTime:Integer;fSlide:WordBool);dispid 2026;
    // alphaBlendTo : method alphaBlendTo 
   procedure alphaBlendTo(newVal:Integer;alphaTime:Integer);dispid 2017;
    // ID : property id 
   property ID:WideString  readonly dispid 2000;
    // elementType : property elementType 
   property elementType:WideString  readonly dispid 2001;
    // left : property left 
   property left:Integer dispid 2002;
    // top : property top 
   property top:Integer dispid 2003;
    // right : property right 
   property right:Integer dispid 2022;
    // bottom : property bottom 
   property bottom:Integer dispid 2023;
    // width : property width 
   property width:Integer dispid 2004;
    // height : property height 
   property height:Integer dispid 2005;
    // zIndex : property zIndex 
   property zIndex:Integer dispid 2006;
    // clippingImage : property clippingImage 
   property clippingImage:WideString dispid 2007;
    // clippingColor : property clippingColor 
   property clippingColor:WideString dispid 2008;
    // visible : property visible 
   property visible:WordBool dispid 2009;
    // enabled : property enabled 
   property enabled:WordBool dispid 2010;
    // tabStop : property tabStop 
   property tabStop:WordBool dispid 2011;
    // passThrough : property passThrough 
   property passThrough:WordBool dispid 2012;
    // horizontalAlignment : property horizontalAlignment 
   property horizontalAlignment:WideString dispid 2013;
    // verticalAlignment : property verticalAlignment 
   property verticalAlignment:WideString dispid 2014;
    // alphaBlend : property alphaBlend 
   property alphaBlend:Integer dispid 2016;
    // accName : property accName 
   property accName:WideString dispid 2018;
    // accDescription : property accDescription 
   property accDescription:WideString dispid 2019;
    // accKeyboardShortcut : property accKeyboardShortcut	 
   property accKeyboardShortcut:WideString dispid 2020;
    // resizeImages : property resizeImages 
   property resizeImages:WordBool dispid 2024;
    // nineGridMargins : property nineGridMargins 
   property nineGridMargins:WideString dispid 2025;
    // resizeOptimize : property resizeOptimize 
   property resizeOptimize:WideString dispid 2027;
    // rotation : property rotation 
   property rotation:Single dispid 2028;
  end;


// IWMPLayoutSubView : IWMPLayoutSubView: Public interface for skin object model.

 IWMPLayoutSubView = interface(IWMPObjectExtendedProps)
   ['{72F486B1-0D43-11D3-BD3F-00C04F6EA5AE}']
   function Get_transparencyColor : WideString; safecall;
   procedure Set_transparencyColor(const pVal:WideString); safecall;
   function Get_backgroundColor : WideString; safecall;
   procedure Set_backgroundColor(const pVal:WideString); safecall;
   function Get_backgroundImage : WideString; safecall;
   procedure Set_backgroundImage(const pVal:WideString); safecall;
   function Get_backgroundTiled : WordBool; safecall;
   procedure Set_backgroundTiled(const pVal:WordBool); safecall;
   function Get_backgroundImageHueShift : Single; safecall;
   procedure Set_backgroundImageHueShift(const pVal:Single); safecall;
   function Get_backgroundImageSaturation : Single; safecall;
   procedure Set_backgroundImageSaturation(const pVal:Single); safecall;
   function Get_resizeBackgroundImage : WordBool; safecall;
   procedure Set_resizeBackgroundImage(const pVal:WordBool); safecall;
    // transparencyColor : property transparencyColor 
   property transparencyColor:WideString read Get_transparencyColor write Set_transparencyColor;
    // backgroundColor : property backgroundColor 
   property backgroundColor:WideString read Get_backgroundColor write Set_backgroundColor;
    // backgroundImage : property backgroundImage 
   property backgroundImage:WideString read Get_backgroundImage write Set_backgroundImage;
    // backgroundTiled : property backgroundTiled 
   property backgroundTiled:WordBool read Get_backgroundTiled write Set_backgroundTiled;
    // backgroundImageHueShift : property hueShift 
   property backgroundImageHueShift:Single read Get_backgroundImageHueShift write Set_backgroundImageHueShift;
    // backgroundImageSaturation : property saturation 
   property backgroundImageSaturation:Single read Get_backgroundImageSaturation write Set_backgroundImageSaturation;
    // resizeBackgroundImage : property resizeBackgroundImage 
   property resizeBackgroundImage:WordBool read Get_resizeBackgroundImage write Set_resizeBackgroundImage;
  end;


// IWMPLayoutSubView : IWMPLayoutSubView: Public interface for skin object model.

 IWMPLayoutSubViewDisp = dispinterface
   ['{72F486B1-0D43-11D3-BD3F-00C04F6EA5AE}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // moveTo : method moveTo 
   procedure moveTo(newX:Integer;newY:Integer;moveTime:Integer);dispid 2015;
    // slideTo : method slideTo 
   procedure slideTo(newX:Integer;newY:Integer;moveTime:Integer);dispid 2021;
    // moveSizeTo : method moveSizeTo 
   procedure moveSizeTo(newX:Integer;newY:Integer;newWidth:Integer;newHeight:Integer;moveTime:Integer;fSlide:WordBool);dispid 2026;
    // alphaBlendTo : method alphaBlendTo 
   procedure alphaBlendTo(newVal:Integer;alphaTime:Integer);dispid 2017;
    // ID : property id 
   property ID:WideString  readonly dispid 2000;
    // elementType : property elementType 
   property elementType:WideString  readonly dispid 2001;
    // left : property left 
   property left:Integer dispid 2002;
    // top : property top 
   property top:Integer dispid 2003;
    // right : property right 
   property right:Integer dispid 2022;
    // bottom : property bottom 
   property bottom:Integer dispid 2023;
    // width : property width 
   property width:Integer dispid 2004;
    // height : property height 
   property height:Integer dispid 2005;
    // zIndex : property zIndex 
   property zIndex:Integer dispid 2006;
    // clippingImage : property clippingImage 
   property clippingImage:WideString dispid 2007;
    // clippingColor : property clippingColor 
   property clippingColor:WideString dispid 2008;
    // visible : property visible 
   property visible:WordBool dispid 2009;
    // enabled : property enabled 
   property enabled:WordBool dispid 2010;
    // tabStop : property tabStop 
   property tabStop:WordBool dispid 2011;
    // passThrough : property passThrough 
   property passThrough:WordBool dispid 2012;
    // horizontalAlignment : property horizontalAlignment 
   property horizontalAlignment:WideString dispid 2013;
    // verticalAlignment : property verticalAlignment 
   property verticalAlignment:WideString dispid 2014;
    // alphaBlend : property alphaBlend 
   property alphaBlend:Integer dispid 2016;
    // accName : property accName 
   property accName:WideString dispid 2018;
    // accDescription : property accDescription 
   property accDescription:WideString dispid 2019;
    // accKeyboardShortcut : property accKeyboardShortcut	 
   property accKeyboardShortcut:WideString dispid 2020;
    // resizeImages : property resizeImages 
   property resizeImages:WordBool dispid 2024;
    // nineGridMargins : property nineGridMargins 
   property nineGridMargins:WideString dispid 2025;
    // resizeOptimize : property resizeOptimize 
   property resizeOptimize:WideString dispid 2027;
    // rotation : property rotation 
   property rotation:Single dispid 2028;
    // transparencyColor : property transparencyColor 
   property transparencyColor:WideString dispid 2300;
    // backgroundColor : property backgroundColor 
   property backgroundColor:WideString dispid 2301;
    // backgroundImage : property backgroundImage 
   property backgroundImage:WideString dispid 2302;
    // backgroundTiled : property backgroundTiled 
   property backgroundTiled:WordBool dispid 2303;
    // backgroundImageHueShift : property hueShift 
   property backgroundImageHueShift:Single dispid 2304;
    // backgroundImageSaturation : property saturation 
   property backgroundImageSaturation:Single dispid 2305;
    // resizeBackgroundImage : property resizeBackgroundImage 
   property resizeBackgroundImage:WordBool dispid 2306;
  end;


// IWMPLayoutView : IWMPLayoutView: Public interface for skin object model.

 IWMPLayoutView = interface(IWMPLayoutSubView)
   ['{172E905D-80D9-4C2F-B7CE-2CCB771787A2}']
   function Get_title : WideString; safecall;
   procedure Set_title(const pVal:WideString); safecall;
   function Get_category : WideString; safecall;
   procedure Set_category(const pVal:WideString); safecall;
   function Get_focusObjectID : WideString; safecall;
   procedure Set_focusObjectID(const pVal:WideString); safecall;
   function Get_titleBar : WordBool; safecall;
   function Get_resizable : WordBool; safecall;
   function Get_timerInterval : Integer; safecall;
   procedure Set_timerInterval(const pVal:Integer); safecall;
   function Get_minWidth : Integer; safecall;
   procedure Set_minWidth(const pVal:Integer); safecall;
   function Get_maxWidth : Integer; safecall;
   procedure Set_maxWidth(const pVal:Integer); safecall;
   function Get_minHeight : Integer; safecall;
   procedure Set_minHeight(const pVal:Integer); safecall;
   function Get_maxHeight : Integer; safecall;
   procedure Set_maxHeight(const pVal:Integer); safecall;
    // close : method close 
   procedure close;safecall;
    // minimize : method minimize 
   procedure minimize;safecall;
    // maximize : method maximize 
   procedure maximize;safecall;
    // restore : method restore 
   procedure restore;safecall;
    // size : method size 
   procedure size(bstrDirection:WideString);safecall;
    // returnToMediaCenter : method returnToMediaCenter 
   procedure returnToMediaCenter;safecall;
    // updateWindow : method updateWindow 
   procedure updateWindow;safecall;
   function Get_maximized : WordBool; safecall;
   function Get_minimized : WordBool; safecall;
    // title : property title 
   property title:WideString read Get_title write Set_title;
    // category : property category 
   property category:WideString read Get_category write Set_category;
    // focusObjectID : property focusObjectID 
   property focusObjectID:WideString read Get_focusObjectID write Set_focusObjectID;
    // titleBar : property titleBar 
   property titleBar:WordBool read Get_titleBar;
    // resizable : property resizable 
   property resizable:WordBool read Get_resizable;
    // timerInterval : property timerInterval 
   property timerInterval:Integer read Get_timerInterval write Set_timerInterval;
    // minWidth : property minWidth 
   property minWidth:Integer read Get_minWidth write Set_minWidth;
    // maxWidth : property maxWidth 
   property maxWidth:Integer read Get_maxWidth write Set_maxWidth;
    // minHeight : property minHeight 
   property minHeight:Integer read Get_minHeight write Set_minHeight;
    // maxHeight : property maxHeight 
   property maxHeight:Integer read Get_maxHeight write Set_maxHeight;
    // maximized :  
   property maximized:WordBool read Get_maximized;
    // minimized :  
   property minimized:WordBool read Get_minimized;
  end;


// IWMPLayoutView : IWMPLayoutView: Public interface for skin object model.

 IWMPLayoutViewDisp = dispinterface
   ['{172E905D-80D9-4C2F-B7CE-2CCB771787A2}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // moveTo : method moveTo 
   procedure moveTo(newX:Integer;newY:Integer;moveTime:Integer);dispid 2015;
    // slideTo : method slideTo 
   procedure slideTo(newX:Integer;newY:Integer;moveTime:Integer);dispid 2021;
    // moveSizeTo : method moveSizeTo 
   procedure moveSizeTo(newX:Integer;newY:Integer;newWidth:Integer;newHeight:Integer;moveTime:Integer;fSlide:WordBool);dispid 2026;
    // alphaBlendTo : method alphaBlendTo 
   procedure alphaBlendTo(newVal:Integer;alphaTime:Integer);dispid 2017;
    // close : method close 
   procedure close;dispid 2318;
    // minimize : method minimize 
   procedure minimize;dispid 2319;
    // maximize : method maximize 
   procedure maximize;dispid 2320;
    // restore : method restore 
   procedure restore;dispid 2321;
    // size : method size 
   procedure size(bstrDirection:WideString);dispid 2322;
    // returnToMediaCenter : method returnToMediaCenter 
   procedure returnToMediaCenter;dispid 2323;
    // updateWindow : method updateWindow 
   procedure updateWindow;dispid 2324;
    // ID : property id 
   property ID:WideString  readonly dispid 2000;
    // elementType : property elementType 
   property elementType:WideString  readonly dispid 2001;
    // left : property left 
   property left:Integer dispid 2002;
    // top : property top 
   property top:Integer dispid 2003;
    // right : property right 
   property right:Integer dispid 2022;
    // bottom : property bottom 
   property bottom:Integer dispid 2023;
    // width : property width 
   property width:Integer dispid 2004;
    // height : property height 
   property height:Integer dispid 2005;
    // zIndex : property zIndex 
   property zIndex:Integer dispid 2006;
    // clippingImage : property clippingImage 
   property clippingImage:WideString dispid 2007;
    // clippingColor : property clippingColor 
   property clippingColor:WideString dispid 2008;
    // visible : property visible 
   property visible:WordBool dispid 2009;
    // enabled : property enabled 
   property enabled:WordBool dispid 2010;
    // tabStop : property tabStop 
   property tabStop:WordBool dispid 2011;
    // passThrough : property passThrough 
   property passThrough:WordBool dispid 2012;
    // horizontalAlignment : property horizontalAlignment 
   property horizontalAlignment:WideString dispid 2013;
    // verticalAlignment : property verticalAlignment 
   property verticalAlignment:WideString dispid 2014;
    // alphaBlend : property alphaBlend 
   property alphaBlend:Integer dispid 2016;
    // accName : property accName 
   property accName:WideString dispid 2018;
    // accDescription : property accDescription 
   property accDescription:WideString dispid 2019;
    // accKeyboardShortcut : property accKeyboardShortcut	 
   property accKeyboardShortcut:WideString dispid 2020;
    // resizeImages : property resizeImages 
   property resizeImages:WordBool dispid 2024;
    // nineGridMargins : property nineGridMargins 
   property nineGridMargins:WideString dispid 2025;
    // resizeOptimize : property resizeOptimize 
   property resizeOptimize:WideString dispid 2027;
    // rotation : property rotation 
   property rotation:Single dispid 2028;
    // transparencyColor : property transparencyColor 
   property transparencyColor:WideString dispid 2300;
    // backgroundColor : property backgroundColor 
   property backgroundColor:WideString dispid 2301;
    // backgroundImage : property backgroundImage 
   property backgroundImage:WideString dispid 2302;
    // backgroundTiled : property backgroundTiled 
   property backgroundTiled:WordBool dispid 2303;
    // backgroundImageHueShift : property hueShift 
   property backgroundImageHueShift:Single dispid 2304;
    // backgroundImageSaturation : property saturation 
   property backgroundImageSaturation:Single dispid 2305;
    // resizeBackgroundImage : property resizeBackgroundImage 
   property resizeBackgroundImage:WordBool dispid 2306;
    // title : property title 
   property title:WideString dispid 2307;
    // category : property category 
   property category:WideString dispid 2308;
    // focusObjectID : property focusObjectID 
   property focusObjectID:WideString dispid 2309;
    // titleBar : property titleBar 
   property titleBar:WordBool  readonly dispid 2311;
    // resizable : property resizable 
   property resizable:WordBool  readonly dispid 2312;
    // timerInterval : property timerInterval 
   property timerInterval:Integer dispid 2313;
    // minWidth : property minWidth 
   property minWidth:Integer dispid 2314;
    // maxWidth : property maxWidth 
   property maxWidth:Integer dispid 2315;
    // minHeight : property minHeight 
   property minHeight:Integer dispid 2316;
    // maxHeight : property maxHeight 
   property maxHeight:Integer dispid 2317;
    // maximized :  
   property maximized:WordBool  readonly dispid 2326;
    // minimized :  
   property minimized:WordBool  readonly dispid 2327;
  end;


// IWMPEventObject : IWMPEventObject: Not Public.  Internal interface used by Windows Media Player.

 IWMPEventObject = interface(IDispatch)
   ['{5AF0BEC1-46AA-11D3-BD45-00C04F6EA5AE}']
   function Get_srcElement : IDispatch; safecall;
   function Get_altKey : WordBool; safecall;
   function Get_ctrlKey : WordBool; safecall;
   function Get_shiftKey : WordBool; safecall;
   function Get_fromElement : IDispatch; safecall;
   function Get_toElement : IDispatch; safecall;
   procedure Set_keyCode(const p:Integer); safecall;
   function Get_keyCode : Integer; safecall;
   function Get_button : Integer; safecall;
   function Get_x : Integer; safecall;
   function Get_y : Integer; safecall;
   function Get_clientX : Integer; safecall;
   function Get_clientY : Integer; safecall;
   function Get_offsetX : Integer; safecall;
   function Get_offsetY : Integer; safecall;
   function Get_screenX : Integer; safecall;
   function Get_screenY : Integer; safecall;
   function Get_screenWidth : Integer; safecall;
   function Get_screenHeight : Integer; safecall;
   function Get_penOrTouch : WordBool; safecall;
    // srcElement :  
   property srcElement:IDispatch read Get_srcElement;
    // altKey :  
   property altKey:WordBool read Get_altKey;
    // ctrlKey :  
   property ctrlKey:WordBool read Get_ctrlKey;
    // shiftKey :  
   property shiftKey:WordBool read Get_shiftKey;
    // fromElement :  
   property fromElement:IDispatch read Get_fromElement;
    // toElement :  
   property toElement:IDispatch read Get_toElement;
    // keyCode :  
   property keyCode:Integer read Get_keyCode write Set_keyCode;
    // button :  
   property button:Integer read Get_button;
    // x :  
   property x:Integer read Get_x;
    // y :  
   property y:Integer read Get_y;
    // clientX :  
   property clientX:Integer read Get_clientX;
    // clientY :  
   property clientY:Integer read Get_clientY;
    // offsetX :  
   property offsetX:Integer read Get_offsetX;
    // offsetY :  
   property offsetY:Integer read Get_offsetY;
    // screenX :  
   property screenX:Integer read Get_screenX;
    // screenY :  
   property screenY:Integer read Get_screenY;
    // screenWidth :  
   property screenWidth:Integer read Get_screenWidth;
    // screenHeight :  
   property screenHeight:Integer read Get_screenHeight;
    // penOrTouch :  
   property penOrTouch:WordBool read Get_penOrTouch;
  end;


// IWMPEventObject : IWMPEventObject: Not Public.  Internal interface used by Windows Media Player.

 IWMPEventObjectDisp = dispinterface
   ['{5AF0BEC1-46AA-11D3-BD45-00C04F6EA5AE}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // srcElement :  
   property srcElement:IDispatch  readonly dispid 2200;
    // altKey :  
   property altKey:WordBool  readonly dispid 2201;
    // ctrlKey :  
   property ctrlKey:WordBool  readonly dispid 2202;
    // shiftKey :  
   property shiftKey:WordBool  readonly dispid 2203;
    // fromElement :  
   property fromElement:IDispatch  readonly dispid 2204;
    // toElement :  
   property toElement:IDispatch  readonly dispid 2205;
    // keyCode :  
   property keyCode:Integer dispid 2206;
    // button :  
   property button:Integer  readonly dispid 2207;
    // x :  
   property x:Integer  readonly dispid 2208;
    // y :  
   property y:Integer  readonly dispid 2209;
    // clientX :  
   property clientX:Integer  readonly dispid 2210;
    // clientY :  
   property clientY:Integer  readonly dispid 2211;
    // offsetX :  
   property offsetX:Integer  readonly dispid 2212;
    // offsetY :  
   property offsetY:Integer  readonly dispid 2213;
    // screenX :  
   property screenX:Integer  readonly dispid 2214;
    // screenY :  
   property screenY:Integer  readonly dispid 2215;
    // screenWidth :  
   property screenWidth:Integer  readonly dispid 2216;
    // screenHeight :  
   property screenHeight:Integer  readonly dispid 2217;
    // penOrTouch :  
   property penOrTouch:WordBool  readonly dispid 2218;
  end;


// IWMPTheme : IWMPTheme: Public interface for skin object model.

 IWMPTheme = interface(IDispatch)
   ['{6FCAE13D-E492-4584-9C21-D2C052A2A33A}']
   function Get_title : WideString; safecall;
   function Get_version : Single; safecall;
   function Get_authorVersion : WideString; safecall;
   function Get_author : WideString; safecall;
   function Get_copyright : WideString; safecall;
   function Get_currentViewID : WideString; safecall;
   procedure Set_currentViewID(const pVal:WideString); safecall;
    // showErrorDialog : method showErrorDialog 
   procedure showErrorDialog;safecall;
    // logString : method logString 
   procedure logString(stringVal:WideString);safecall;
    // openView : method openView 
   procedure openView(viewID:WideString);safecall;
    // openViewRelative : method openView 
   function openViewRelative(viewID:WideString;x:Integer;y:Integer):IDispatch;safecall;
    // closeView : method closeView 
   procedure closeView(viewID:WideString);safecall;
    // openDialog : method openDialog 
   function openDialog(dialogType:WideString;parameters:WideString):WideString;safecall;
    // loadString : method loadString 
   function loadString(bstrString:WideString):WideString;safecall;
    // loadPreference : method loadPreference 
   function loadPreference(bstrName:WideString):WideString;safecall;
    // savePreference : method savePreference 
   procedure savePreference(bstrName:WideString;bstrValue:WideString);safecall;
    // playSound : method playSound 
   procedure playSound(bstrFilename:WideString);safecall;
    // openViewRelativeInternal : Microsoft internal use only 
   function openViewRelativeInternal(viewID:WideString;nIndex:Integer;x:Integer;y:Integer;nWidth:Integer;nHeight:Integer;bstrHorizontalAlignment:WideString;bstrVerticalAlignment:WideString):IDispatch;safecall;
    // setViewPosition : Microsoft internal use only 
   procedure setViewPosition(viewID:WideString;nIndex:Integer;x:Integer;y:Integer;nWidth:Integer;nHeight:Integer;bstrHorizontalAlignment:WideString;bstrVerticalAlignment:WideString);safecall;
    // title : property title 
   property title:WideString read Get_title;
    // version : property version 
   property version:Single read Get_version;
    // authorVersion : property authorVersion 
   property authorVersion:WideString read Get_authorVersion;
    // author : property author 
   property author:WideString read Get_author;
    // copyright : property copyright 
   property copyright:WideString read Get_copyright;
    // currentViewID : property title 
   property currentViewID:WideString read Get_currentViewID write Set_currentViewID;
  end;


// IWMPTheme : IWMPTheme: Public interface for skin object model.

 IWMPThemeDisp = dispinterface
   ['{6FCAE13D-E492-4584-9C21-D2C052A2A33A}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // showErrorDialog : method showErrorDialog 
   procedure showErrorDialog;dispid 2506;
    // logString : method logString 
   procedure logString(stringVal:WideString);dispid 2507;
    // openView : method openView 
   procedure openView(viewID:WideString);dispid 2508;
    // openViewRelative : method openView 
   function openViewRelative(viewID:WideString;x:Integer;y:Integer):IDispatch;dispid 2515;
    // closeView : method closeView 
   procedure closeView(viewID:WideString);dispid 2509;
    // openDialog : method openDialog 
   function openDialog(dialogType:WideString;parameters:WideString):WideString;dispid 2510;
    // loadString : method loadString 
   function loadString(bstrString:WideString):WideString;dispid 2511;
    // loadPreference : method loadPreference 
   function loadPreference(bstrName:WideString):WideString;dispid 2512;
    // savePreference : method savePreference 
   procedure savePreference(bstrName:WideString;bstrValue:WideString);dispid 2513;
    // playSound : method playSound 
   procedure playSound(bstrFilename:WideString);dispid 2514;
    // openViewRelativeInternal : Microsoft internal use only 
   function openViewRelativeInternal(viewID:WideString;nIndex:Integer;x:Integer;y:Integer;nWidth:Integer;nHeight:Integer;bstrHorizontalAlignment:WideString;bstrVerticalAlignment:WideString):IDispatch;dispid 2516;
    // setViewPosition : Microsoft internal use only 
   procedure setViewPosition(viewID:WideString;nIndex:Integer;x:Integer;y:Integer;nWidth:Integer;nHeight:Integer;bstrHorizontalAlignment:WideString;bstrVerticalAlignment:WideString);dispid 2518;
    // title : property title 
   property title:WideString  readonly dispid 2500;
    // version : property version 
   property version:Single  readonly dispid 2501;
    // authorVersion : property authorVersion 
   property authorVersion:WideString  readonly dispid 2502;
    // author : property author 
   property author:WideString  readonly dispid 2503;
    // copyright : property copyright 
   property copyright:WideString  readonly dispid 2504;
    // currentViewID : property title 
   property currentViewID:WideString dispid 2505;
  end;


// IWMPLayoutSettingsDispatch : IWMPLayoutSettingsDispatch: Not Public.  Internal interface used by Windows Media Player.

 IWMPLayoutSettingsDispatch = interface(IDispatch)
   ['{B2C2D18E-97AF-4B6A-A56B-2FFFF470FB81}']
   function Get_effectType : WideString; safecall;
   procedure Set_effectType(const pVal:WideString); safecall;
   function Get_effectPreset : Integer; safecall;
   procedure Set_effectPreset(const pVal:Integer); safecall;
   function Get_settingsView : WideString; safecall;
   procedure Set_settingsView(const pVal:WideString); safecall;
   function Get_videoZoom : Integer; safecall;
   procedure Set_videoZoom(const pVal:Integer); safecall;
   function Get_videoShrinkToFit : WordBool; safecall;
   procedure Set_videoShrinkToFit(const pVal:WordBool); safecall;
   function Get_videoStretchToFit : WordBool; safecall;
   procedure Set_videoStretchToFit(const pVal:WordBool); safecall;
   function Get_userVideoStretchToFit : WordBool; safecall;
   procedure Set_userVideoStretchToFit(const pVal:WordBool); safecall;
   function Get_showCaptions : WordBool; safecall;
   procedure Set_showCaptions(const pVal:WordBool); safecall;
   function Get_showTitles : WordBool; safecall;
   procedure Set_showTitles(const pVal:WordBool); safecall;
   function Get_showEffects : WordBool; safecall;
   procedure Set_showEffects(const pVal:WordBool); safecall;
   function Get_showFullScreenPlaylist : WordBool; safecall;
   procedure Set_showFullScreenPlaylist(const pVal:WordBool); safecall;
   function Get_contrastMode : WideString; safecall;
    // getNamedString : method getNamedString 
   function getNamedString(bstrName:WideString):WideString;safecall;
    // getDurationStringFromSeconds : method getDurationStringFromSeconds 
   function getDurationStringFromSeconds(lDurationVal:Integer):WideString;safecall;
   function Get_displayView : WideString; safecall;
   procedure Set_displayView(const pVal:WideString); safecall;
   function Get_metadataView : WideString; safecall;
   procedure Set_metadataView(const pVal:WideString); safecall;
   function Get_showSettings : WordBool; safecall;
   procedure Set_showSettings(const pVal:WordBool); safecall;
   function Get_showResizeBars : WordBool; safecall;
   procedure Set_showResizeBars(const pVal:WordBool); safecall;
   function Get_showPlaylist : WordBool; safecall;
   procedure Set_showPlaylist(const pVal:WordBool); safecall;
   function Get_showMetadata : WordBool; safecall;
   procedure Set_showMetadata(const pVal:WordBool); safecall;
   function Get_settingsWidth : Integer; safecall;
   procedure Set_settingsWidth(const pVal:Integer); safecall;
   function Get_settingsHeight : Integer; safecall;
   procedure Set_settingsHeight(const pVal:Integer); safecall;
   function Get_playlistWidth : Integer; safecall;
   procedure Set_playlistWidth(const pVal:Integer); safecall;
   function Get_playlistHeight : Integer; safecall;
   procedure Set_playlistHeight(const pVal:Integer); safecall;
   function Get_metadataWidth : Integer; safecall;
   procedure Set_metadataWidth(const pVal:Integer); safecall;
   function Get_metadataHeight : Integer; safecall;
   procedure Set_metadataHeight(const pVal:Integer); safecall;
   function Get_fullScreenAvailable : WordBool; safecall;
   procedure Set_fullScreenAvailable(const pVal:WordBool); safecall;
   function Get_fullScreenRequest : WordBool; safecall;
   procedure Set_fullScreenRequest(const pVal:WordBool); safecall;
   function Get_quickHide : WordBool; safecall;
   procedure Set_quickHide(const pVal:WordBool); safecall;
   function Get_displayPreset : Integer; safecall;
   procedure Set_displayPreset(const pVal:Integer); safecall;
   function Get_settingsPreset : Integer; safecall;
   procedure Set_settingsPreset(const pVal:Integer); safecall;
   function Get_metadataPreset : Integer; safecall;
   procedure Set_metadataPreset(const pVal:Integer); safecall;
   function Get_userDisplayView : WideString; safecall;
   function Get_userWMPDisplayView : WideString; safecall;
   function Get_userDisplayPreset : Integer; safecall;
   function Get_userWMPDisplayPreset : Integer; safecall;
   function Get_dynamicRangeControl : Integer; safecall;
   procedure Set_dynamicRangeControl(const pVal:Integer); safecall;
   function Get_slowRate : Single; safecall;
   procedure Set_slowRate(const pVal:Single); safecall;
   function Get_fastRate : Single; safecall;
   procedure Set_fastRate(const pVal:Single); safecall;
   function Get_buttonHueShift : Single; safecall;
   procedure Set_buttonHueShift(const pVal:Single); safecall;
   function Get_buttonSaturation : Single; safecall;
   procedure Set_buttonSaturation(const pVal:Single); safecall;
   function Get_backHueShift : Single; safecall;
   procedure Set_backHueShift(const pVal:Single); safecall;
   function Get_backSaturation : Single; safecall;
   procedure Set_backSaturation(const pVal:Single); safecall;
   function Get_vizRequest : Integer; safecall;
   procedure Set_vizRequest(const pVal:Integer); safecall;
   function Get_appColorLight : WideString; safecall;
   function Get_appColorMedium : WideString; safecall;
   function Get_appColorDark : WideString; safecall;
   function Get_toolbarButtonHighlight : WideString; safecall;
   function Get_toolbarButtonShadow : WideString; safecall;
   function Get_toolbarButtonFace : WideString; safecall;
   function Get_itemPlayingColor : WideString; safecall;
   function Get_itemPlayingBackgroundColor : WideString; safecall;
   function Get_itemErrorColor : WideString; safecall;
   function Get_appColorLimited : WordBool; safecall;
   function Get_appColorBlackBackground : WordBool; safecall;
   procedure Set_appColorBlackBackground(const pVal:WordBool); safecall;
   function Get_appColorVideoBorder : WideString; safecall;
   procedure Set_appColorVideoBorder(const pVal:WideString); safecall;
   function Get_appColorAux1 : WideString; safecall;
   function Get_appColorAux2 : WideString; safecall;
   function Get_appColorAux3 : WideString; safecall;
   function Get_appColorAux4 : WideString; safecall;
   function Get_appColorAux5 : WideString; safecall;
   function Get_appColorAux6 : WideString; safecall;
   function Get_appColorAux7 : WideString; safecall;
   function Get_appColorAux8 : WideString; safecall;
   function Get_appColorAux9 : WideString; safecall;
   function Get_appColorAux10 : WideString; safecall;
   function Get_appColorAux11 : WideString; safecall;
   function Get_appColorAux12 : WideString; safecall;
   function Get_appColorAux13 : WideString; safecall;
   function Get_appColorAux14 : WideString; safecall;
   function Get_appColorAux15 : WideString; safecall;
   function Get_status : WideString; safecall;
   procedure Set_status(const pVal:WideString); safecall;
   function Get_userWMPSettingsView : WideString; safecall;
   function Get_userWMPSettingsPreset : Integer; safecall;
   function Get_userWMPShowSettings : WordBool; safecall;
   function Get_userWMPMetadataView : WideString; safecall;
   function Get_userWMPMetadataPreset : Integer; safecall;
   function Get_userWMPShowMetadata : WordBool; safecall;
   function Get_captionsHeight : Integer; safecall;
   procedure Set_captionsHeight(const pVal:Integer); safecall;
   function Get_snapToVideo : WordBool; safecall;
   procedure Set_snapToVideo(const pVal:WordBool); safecall;
   function Get_pinFullScreenControls : WordBool; safecall;
   procedure Set_pinFullScreenControls(const pVal:WordBool); safecall;
   function Get_isMultiMon : WordBool; safecall;
   function Get_exclusiveHueShift : Single; safecall;
   procedure Set_exclusiveHueShift(const pVal:Single); safecall;
   function Get_exclusiveSaturation : Single; safecall;
   procedure Set_exclusiveSaturation(const pVal:Single); safecall;
   function Get_themeBkgColorIsActive : WordBool; safecall;
   procedure Set_themeBkgColorIsActive(const pVal:WordBool); safecall;
   function Get_themeBkgColorActive : WideString; safecall;
   function Get_themeBkgColorInactive : WideString; safecall;
    // effectType : property effectType 
   property effectType:WideString read Get_effectType write Set_effectType;
    // effectPreset : property effectPreset 
   property effectPreset:Integer read Get_effectPreset write Set_effectPreset;
    // settingsView : property settingsView 
   property settingsView:WideString read Get_settingsView write Set_settingsView;
    // videoZoom : property videoZoom 
   property videoZoom:Integer read Get_videoZoom write Set_videoZoom;
    // videoShrinkToFit : property videoShrinkToFit 
   property videoShrinkToFit:WordBool read Get_videoShrinkToFit write Set_videoShrinkToFit;
    // videoStretchToFit : property videoStretchToFit 
   property videoStretchToFit:WordBool read Get_videoStretchToFit write Set_videoStretchToFit;
    // userVideoStretchToFit : property userVideoStretchToFit 
   property userVideoStretchToFit:WordBool read Get_userVideoStretchToFit write Set_userVideoStretchToFit;
    // showCaptions : property showCaptions 
   property showCaptions:WordBool read Get_showCaptions write Set_showCaptions;
    // showTitles : property showTitles 
   property showTitles:WordBool read Get_showTitles write Set_showTitles;
    // showEffects : property showEffects 
   property showEffects:WordBool read Get_showEffects write Set_showEffects;
    // showFullScreenPlaylist : property showFullScreenPlaylist 
   property showFullScreenPlaylist:WordBool read Get_showFullScreenPlaylist write Set_showFullScreenPlaylist;
    // contrastMode : property contrastMode 
   property contrastMode:WideString read Get_contrastMode;
    // displayView : property displayView 
   property displayView:WideString read Get_displayView write Set_displayView;
    // metadataView : property metadataView 
   property metadataView:WideString read Get_metadataView write Set_metadataView;
    // showSettings : property showSettings 
   property showSettings:WordBool read Get_showSettings write Set_showSettings;
    // showResizeBars : property showResizeBars 
   property showResizeBars:WordBool read Get_showResizeBars write Set_showResizeBars;
    // showPlaylist : property showPlaylist 
   property showPlaylist:WordBool read Get_showPlaylist write Set_showPlaylist;
    // showMetadata : property showMetadata 
   property showMetadata:WordBool read Get_showMetadata write Set_showMetadata;
    // settingsWidth : property settingsWidth 
   property settingsWidth:Integer read Get_settingsWidth write Set_settingsWidth;
    // settingsHeight : property settingsHeight 
   property settingsHeight:Integer read Get_settingsHeight write Set_settingsHeight;
    // playlistWidth : property playlistWidth 
   property playlistWidth:Integer read Get_playlistWidth write Set_playlistWidth;
    // playlistHeight : property playlistHeight 
   property playlistHeight:Integer read Get_playlistHeight write Set_playlistHeight;
    // metadataWidth : property metadataWidth 
   property metadataWidth:Integer read Get_metadataWidth write Set_metadataWidth;
    // metadataHeight : property metadataHeight 
   property metadataHeight:Integer read Get_metadataHeight write Set_metadataHeight;
    // fullScreenAvailable : property fullScreenAvailable 
   property fullScreenAvailable:WordBool read Get_fullScreenAvailable write Set_fullScreenAvailable;
    // fullScreenRequest : property fullScreenRequest 
   property fullScreenRequest:WordBool read Get_fullScreenRequest write Set_fullScreenRequest;
    // quickHide : property quickHide 
   property quickHide:WordBool read Get_quickHide write Set_quickHide;
    // displayPreset : property displayPreset 
   property displayPreset:Integer read Get_displayPreset write Set_displayPreset;
    // settingsPreset : property settingsPreset 
   property settingsPreset:Integer read Get_settingsPreset write Set_settingsPreset;
    // metadataPreset : property metadataPreset 
   property metadataPreset:Integer read Get_metadataPreset write Set_metadataPreset;
    // userDisplayView : property userDisplayView 
   property userDisplayView:WideString read Get_userDisplayView;
    // userWMPDisplayView : property userWMPDisplayView 
   property userWMPDisplayView:WideString read Get_userWMPDisplayView;
    // userDisplayPreset : property userDisplayPreset 
   property userDisplayPreset:Integer read Get_userDisplayPreset;
    // userWMPDisplayPreset : property userWMPDisplayPreset 
   property userWMPDisplayPreset:Integer read Get_userWMPDisplayPreset;
    // dynamicRangeControl : property dynamicRangeControl 
   property dynamicRangeControl:Integer read Get_dynamicRangeControl write Set_dynamicRangeControl;
    // slowRate : property slowRate 
   property slowRate:Single read Get_slowRate write Set_slowRate;
    // fastRate : property fastRate 
   property fastRate:Single read Get_fastRate write Set_fastRate;
    // buttonHueShift : property buttonHueShift 
   property buttonHueShift:Single read Get_buttonHueShift write Set_buttonHueShift;
    // buttonSaturation : property buttonSaturation 
   property buttonSaturation:Single read Get_buttonSaturation write Set_buttonSaturation;
    // backHueShift : property backHueShift 
   property backHueShift:Single read Get_backHueShift write Set_backHueShift;
    // backSaturation : property backSaturation 
   property backSaturation:Single read Get_backSaturation write Set_backSaturation;
    // vizRequest : property vizRequest 
   property vizRequest:Integer read Get_vizRequest write Set_vizRequest;
    // appColorLight : property appColorLight 
   property appColorLight:WideString read Get_appColorLight;
    // appColorMedium : property appColorMedium 
   property appColorMedium:WideString read Get_appColorMedium;
    // appColorDark : property appColorDark 
   property appColorDark:WideString read Get_appColorDark;
    // toolbarButtonHighlight : property toolbarButtonHighlight 
   property toolbarButtonHighlight:WideString read Get_toolbarButtonHighlight;
    // toolbarButtonShadow : property toolbarButtonShadow 
   property toolbarButtonShadow:WideString read Get_toolbarButtonShadow;
    // toolbarButtonFace : property toolbarButtonFace 
   property toolbarButtonFace:WideString read Get_toolbarButtonFace;
    // itemPlayingColor : property itemPlayingColor 
   property itemPlayingColor:WideString read Get_itemPlayingColor;
    // itemPlayingBackgroundColor : property itemPlayingBackgroundColor 
   property itemPlayingBackgroundColor:WideString read Get_itemPlayingBackgroundColor;
    // itemErrorColor : property itemErrorColor 
   property itemErrorColor:WideString read Get_itemErrorColor;
    // appColorLimited : property AppColorLimited 
   property appColorLimited:WordBool read Get_appColorLimited;
    // appColorBlackBackground : property AppColorBlackBackground 
   property appColorBlackBackground:WordBool read Get_appColorBlackBackground write Set_appColorBlackBackground;
    // appColorVideoBorder : property appColorVideoBorder 
   property appColorVideoBorder:WideString read Get_appColorVideoBorder write Set_appColorVideoBorder;
    // appColorAux1 : auxiliary color 
   property appColorAux1:WideString read Get_appColorAux1;
    // appColorAux2 : auxiliary color 
   property appColorAux2:WideString read Get_appColorAux2;
    // appColorAux3 : auxiliary color 
   property appColorAux3:WideString read Get_appColorAux3;
    // appColorAux4 : auxiliary color 
   property appColorAux4:WideString read Get_appColorAux4;
    // appColorAux5 : auxiliary color 
   property appColorAux5:WideString read Get_appColorAux5;
    // appColorAux6 : auxiliary color 
   property appColorAux6:WideString read Get_appColorAux6;
    // appColorAux7 : auxiliary color 
   property appColorAux7:WideString read Get_appColorAux7;
    // appColorAux8 : auxiliary color 
   property appColorAux8:WideString read Get_appColorAux8;
    // appColorAux9 : auxiliary color 
   property appColorAux9:WideString read Get_appColorAux9;
    // appColorAux10 : auxiliary color 
   property appColorAux10:WideString read Get_appColorAux10;
    // appColorAux11 : auxiliary color 
   property appColorAux11:WideString read Get_appColorAux11;
    // appColorAux12 : auxiliary color 
   property appColorAux12:WideString read Get_appColorAux12;
    // appColorAux13 : auxiliary color 
   property appColorAux13:WideString read Get_appColorAux13;
    // appColorAux14 : auxiliary color 
   property appColorAux14:WideString read Get_appColorAux14;
    // appColorAux15 : auxiliary color 
   property appColorAux15:WideString read Get_appColorAux15;
    // status : status string for remote player (taskbar player) 
   property status:WideString read Get_status write Set_status;
    // userWMPSettingsView : property userWMPSettingsView 
   property userWMPSettingsView:WideString read Get_userWMPSettingsView;
    // userWMPSettingsPreset : property userWMPSettingsPreset 
   property userWMPSettingsPreset:Integer read Get_userWMPSettingsPreset;
    // userWMPShowSettings : property userWMPShowSettings 
   property userWMPShowSettings:WordBool read Get_userWMPShowSettings;
    // userWMPMetadataView : property userWMPMetadataView 
   property userWMPMetadataView:WideString read Get_userWMPMetadataView;
    // userWMPMetadataPreset : property userWMPMetadataPreset 
   property userWMPMetadataPreset:Integer read Get_userWMPMetadataPreset;
    // userWMPShowMetadata : property userWMPShowMetadata 
   property userWMPShowMetadata:WordBool read Get_userWMPShowMetadata;
    // captionsHeight : property captionsHeight 
   property captionsHeight:Integer read Get_captionsHeight write Set_captionsHeight;
    // snapToVideo : property snapToVideo 
   property snapToVideo:WordBool read Get_snapToVideo write Set_snapToVideo;
    // pinFullScreenControls : property pinFullScreenControls 
   property pinFullScreenControls:WordBool read Get_pinFullScreenControls write Set_pinFullScreenControls;
    // isMultiMon : property isMultiMon 
   property isMultiMon:WordBool read Get_isMultiMon;
    // exclusiveHueShift : property exclusiveHueShift 
   property exclusiveHueShift:Single read Get_exclusiveHueShift write Set_exclusiveHueShift;
    // exclusiveSaturation : property exclusiveSaturation 
   property exclusiveSaturation:Single read Get_exclusiveSaturation write Set_exclusiveSaturation;
    // themeBkgColorIsActive : themeBkgColorIsActive 
   property themeBkgColorIsActive:WordBool read Get_themeBkgColorIsActive write Set_themeBkgColorIsActive;
    // themeBkgColorActive : themeBkgColorActive 
   property themeBkgColorActive:WideString read Get_themeBkgColorActive;
    // themeBkgColorInactive : themeBkgColorInactive 
   property themeBkgColorInactive:WideString read Get_themeBkgColorInactive;
  end;


// IWMPLayoutSettingsDispatch : IWMPLayoutSettingsDispatch: Not Public.  Internal interface used by Windows Media Player.

 IWMPLayoutSettingsDispatchDisp = dispinterface
   ['{B2C2D18E-97AF-4B6A-A56B-2FFFF470FB81}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // getNamedString : method getNamedString 
   function getNamedString(bstrName:WideString):WideString;dispid 2810;
    // getDurationStringFromSeconds : method getDurationStringFromSeconds 
   function getDurationStringFromSeconds(lDurationVal:Integer):WideString;dispid 2815;
    // effectType : property effectType 
   property effectType:WideString dispid 2800;
    // effectPreset : property effectPreset 
   property effectPreset:Integer dispid 2801;
    // settingsView : property settingsView 
   property settingsView:WideString dispid 2802;
    // videoZoom : property videoZoom 
   property videoZoom:Integer dispid 2803;
    // videoShrinkToFit : property videoShrinkToFit 
   property videoShrinkToFit:WordBool dispid 2804;
    // videoStretchToFit : property videoStretchToFit 
   property videoStretchToFit:WordBool dispid 2805;
    // userVideoStretchToFit : property userVideoStretchToFit 
   property userVideoStretchToFit:WordBool dispid 2868;
    // showCaptions : property showCaptions 
   property showCaptions:WordBool dispid 2807;
    // showTitles : property showTitles 
   property showTitles:WordBool dispid 2808;
    // showEffects : property showEffects 
   property showEffects:WordBool dispid 2809;
    // showFullScreenPlaylist : property showFullScreenPlaylist 
   property showFullScreenPlaylist:WordBool dispid 2811;
    // contrastMode : property contrastMode 
   property contrastMode:WideString  readonly dispid 2813;
    // displayView : property displayView 
   property displayView:WideString dispid 2816;
    // metadataView : property metadataView 
   property metadataView:WideString dispid 2817;
    // showSettings : property showSettings 
   property showSettings:WordBool dispid 2818;
    // showResizeBars : property showResizeBars 
   property showResizeBars:WordBool dispid 2819;
    // showPlaylist : property showPlaylist 
   property showPlaylist:WordBool dispid 2820;
    // showMetadata : property showMetadata 
   property showMetadata:WordBool dispid 2821;
    // settingsWidth : property settingsWidth 
   property settingsWidth:Integer dispid 2822;
    // settingsHeight : property settingsHeight 
   property settingsHeight:Integer dispid 2823;
    // playlistWidth : property playlistWidth 
   property playlistWidth:Integer dispid 2824;
    // playlistHeight : property playlistHeight 
   property playlistHeight:Integer dispid 2825;
    // metadataWidth : property metadataWidth 
   property metadataWidth:Integer dispid 2826;
    // metadataHeight : property metadataHeight 
   property metadataHeight:Integer dispid 2827;
    // fullScreenAvailable : property fullScreenAvailable 
   property fullScreenAvailable:WordBool dispid 2828;
    // fullScreenRequest : property fullScreenRequest 
   property fullScreenRequest:WordBool dispid 2829;
    // quickHide : property quickHide 
   property quickHide:WordBool dispid 2830;
    // displayPreset : property displayPreset 
   property displayPreset:Integer dispid 2831;
    // settingsPreset : property settingsPreset 
   property settingsPreset:Integer dispid 2832;
    // metadataPreset : property metadataPreset 
   property metadataPreset:Integer dispid 2833;
    // userDisplayView : property userDisplayView 
   property userDisplayView:WideString  readonly dispid 2834;
    // userWMPDisplayView : property userWMPDisplayView 
   property userWMPDisplayView:WideString  readonly dispid 2835;
    // userDisplayPreset : property userDisplayPreset 
   property userDisplayPreset:Integer  readonly dispid 2836;
    // userWMPDisplayPreset : property userWMPDisplayPreset 
   property userWMPDisplayPreset:Integer  readonly dispid 2837;
    // dynamicRangeControl : property dynamicRangeControl 
   property dynamicRangeControl:Integer dispid 2838;
    // slowRate : property slowRate 
   property slowRate:Single dispid 2839;
    // fastRate : property fastRate 
   property fastRate:Single dispid 2840;
    // buttonHueShift : property buttonHueShift 
   property buttonHueShift:Single dispid 2841;
    // buttonSaturation : property buttonSaturation 
   property buttonSaturation:Single dispid 2842;
    // backHueShift : property backHueShift 
   property backHueShift:Single dispid 2843;
    // backSaturation : property backSaturation 
   property backSaturation:Single dispid 2844;
    // vizRequest : property vizRequest 
   property vizRequest:Integer dispid 2845;
    // appColorLight : property appColorLight 
   property appColorLight:WideString  readonly dispid 2847;
    // appColorMedium : property appColorMedium 
   property appColorMedium:WideString  readonly dispid 2848;
    // appColorDark : property appColorDark 
   property appColorDark:WideString  readonly dispid 2849;
    // toolbarButtonHighlight : property toolbarButtonHighlight 
   property toolbarButtonHighlight:WideString  readonly dispid 2856;
    // toolbarButtonShadow : property toolbarButtonShadow 
   property toolbarButtonShadow:WideString  readonly dispid 2857;
    // toolbarButtonFace : property toolbarButtonFace 
   property toolbarButtonFace:WideString  readonly dispid 2858;
    // itemPlayingColor : property itemPlayingColor 
   property itemPlayingColor:WideString  readonly dispid 2850;
    // itemPlayingBackgroundColor : property itemPlayingBackgroundColor 
   property itemPlayingBackgroundColor:WideString  readonly dispid 2851;
    // itemErrorColor : property itemErrorColor 
   property itemErrorColor:WideString  readonly dispid 2852;
    // appColorLimited : property AppColorLimited 
   property appColorLimited:WordBool  readonly dispid 2853;
    // appColorBlackBackground : property AppColorBlackBackground 
   property appColorBlackBackground:WordBool dispid 2854;
    // appColorVideoBorder : property appColorVideoBorder 
   property appColorVideoBorder:WideString dispid 2855;
    // appColorAux1 : auxiliary color 
   property appColorAux1:WideString  readonly dispid 2869;
    // appColorAux2 : auxiliary color 
   property appColorAux2:WideString  readonly dispid 2870;
    // appColorAux3 : auxiliary color 
   property appColorAux3:WideString  readonly dispid 2871;
    // appColorAux4 : auxiliary color 
   property appColorAux4:WideString  readonly dispid 2872;
    // appColorAux5 : auxiliary color 
   property appColorAux5:WideString  readonly dispid 2873;
    // appColorAux6 : auxiliary color 
   property appColorAux6:WideString  readonly dispid 2874;
    // appColorAux7 : auxiliary color 
   property appColorAux7:WideString  readonly dispid 2875;
    // appColorAux8 : auxiliary color 
   property appColorAux8:WideString  readonly dispid 2876;
    // appColorAux9 : auxiliary color 
   property appColorAux9:WideString  readonly dispid 2877;
    // appColorAux10 : auxiliary color 
   property appColorAux10:WideString  readonly dispid 2878;
    // appColorAux11 : auxiliary color 
   property appColorAux11:WideString  readonly dispid 2879;
    // appColorAux12 : auxiliary color 
   property appColorAux12:WideString  readonly dispid 2880;
    // appColorAux13 : auxiliary color 
   property appColorAux13:WideString  readonly dispid 2881;
    // appColorAux14 : auxiliary color 
   property appColorAux14:WideString  readonly dispid 2882;
    // appColorAux15 : auxiliary color 
   property appColorAux15:WideString  readonly dispid 2883;
    // status : status string for remote player (taskbar player) 
   property status:WideString dispid 2884;
    // userWMPSettingsView : property userWMPSettingsView 
   property userWMPSettingsView:WideString  readonly dispid 2859;
    // userWMPSettingsPreset : property userWMPSettingsPreset 
   property userWMPSettingsPreset:Integer  readonly dispid 2860;
    // userWMPShowSettings : property userWMPShowSettings 
   property userWMPShowSettings:WordBool  readonly dispid 2861;
    // userWMPMetadataView : property userWMPMetadataView 
   property userWMPMetadataView:WideString  readonly dispid 2862;
    // userWMPMetadataPreset : property userWMPMetadataPreset 
   property userWMPMetadataPreset:Integer  readonly dispid 2863;
    // userWMPShowMetadata : property userWMPShowMetadata 
   property userWMPShowMetadata:WordBool  readonly dispid 2864;
    // captionsHeight : property captionsHeight 
   property captionsHeight:Integer dispid 2865;
    // snapToVideo : property snapToVideo 
   property snapToVideo:WordBool dispid 2866;
    // pinFullScreenControls : property pinFullScreenControls 
   property pinFullScreenControls:WordBool dispid 2867;
    // isMultiMon : property isMultiMon 
   property isMultiMon:WordBool  readonly dispid 2887;
    // exclusiveHueShift : property exclusiveHueShift 
   property exclusiveHueShift:Single dispid 2888;
    // exclusiveSaturation : property exclusiveSaturation 
   property exclusiveSaturation:Single dispid 2889;
    // themeBkgColorIsActive : themeBkgColorIsActive 
   property themeBkgColorIsActive:WordBool dispid 2892;
    // themeBkgColorActive : themeBkgColorActive 
   property themeBkgColorActive:WideString  readonly dispid 2890;
    // themeBkgColorInactive : themeBkgColorInactive 
   property themeBkgColorInactive:WideString  readonly dispid 2891;
  end;


// IWMPWindow : IWMPWindow: Not Public.  Internal interface used by Windows Media Player.

 IWMPWindow = interface(IDispatch)
   ['{43D5AE92-4332-477C-8883-E0B3B063C5D2}']
    // setWindowPos : method setWindowPos 
   procedure setWindowPos(x:Integer;y:Integer;height:Integer;width:Integer);safecall;
   function Get_frameRate : Integer; safecall;
   procedure Set_frameRate(const pVal:Integer); safecall;
   function Get_mouseX : Integer; safecall;
   function Get_mouseY : Integer; safecall;
   procedure Set_onsizing(const Param1:IDispatch); safecall;
    // openViewAlwaysOnTop :  
   procedure openViewAlwaysOnTop(bstrViewID:WideString);safecall;
    // frameRate :  
   property frameRate:Integer read Get_frameRate write Set_frameRate;
    // mouseX :  
   property mouseX:Integer read Get_mouseX;
    // mouseY :  
   property mouseY:Integer read Get_mouseY;
    // onsizing :  
   property onsizing:IDispatch write Set_onsizing;
  end;


// IWMPWindow : IWMPWindow: Not Public.  Internal interface used by Windows Media Player.

 IWMPWindowDisp = dispinterface
   ['{43D5AE92-4332-477C-8883-E0B3B063C5D2}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // setWindowPos : method setWindowPos 
   procedure setWindowPos(x:Integer;y:Integer;height:Integer;width:Integer);dispid 3300;
    // openViewAlwaysOnTop :  
   procedure openViewAlwaysOnTop(bstrViewID:WideString);dispid 3305;
    // frameRate :  
   property frameRate:Integer dispid 3301;
    // mouseX :  
   property mouseX:Integer  readonly dispid 3302;
    // mouseY :  
   property mouseY:Integer  readonly dispid 3303;
    // onsizing :  
   property onsizing:IDispatch writeonly dispid 3304;
  end;


// IWMPBrandDispatch : IWMPBrandDispatch: Not Public.  Internal interface used by Windows Media Player.

 IWMPBrandDispatch = interface(IDispatch)
   ['{98BB02D4-ED74-43CC-AD6A-45888F2E0DCC}']
   function Get_fullServiceName : WideString; safecall;
   function Get_friendlyName : WideString; safecall;
   function Get_guideButtonText : WideString; safecall;
   function Get_guideButtonTip : WideString; safecall;
   function Get_guideMenuText : WideString; safecall;
   function Get_guideAccText : WideString; safecall;
   function Get_task1ButtonText : WideString; safecall;
   function Get_task1ButtonTip : WideString; safecall;
   function Get_task1MenuText : WideString; safecall;
   function Get_task1AccText : WideString; safecall;
   function Get_guideUrl : WideString; safecall;
   function Get_task1Url : WideString; safecall;
   function Get_imageLargeUrl : WideString; safecall;
   function Get_imageSmallUrl : WideString; safecall;
   function Get_imageMenuUrl : WideString; safecall;
   function Get_infoCenterUrl : WideString; safecall;
   function Get_albumInfoUrl : WideString; safecall;
   function Get_buyCDUrl : WideString; safecall;
   function Get_htmlViewUrl : WideString; safecall;
   function Get_navigateUrl : WideString; safecall;
   function Get_cookieUrl : WideString; safecall;
   function Get_downloadStatusUrl : WideString; safecall;
   function Get_colorPlayer : WideString; safecall;
   function Get_colorPlayerText : WideString; safecall;
   function Get_navigateDispid : Integer; safecall;
   function Get_navigateParams : WideString; safecall;
   function Get_navigatePane : WideString; safecall;
   function Get_selectedPane : WideString; safecall;
   procedure Set_selectedPane(const pVal:WideString); safecall;
    // setNavigateProps : method setNavigateProps 
   procedure setNavigateProps(bstrPane:WideString;lDispid:Integer;bstrParams:WideString);safecall;
    // getMediaParams : method getMediaParams 
   function getMediaParams(pObject:IUnknown;bstrURL:WideString):WideString;safecall;
   procedure Set_selectedTask(const Param1:Integer); safecall;
   function Get_contentPartnerSelected : WordBool; safecall;
    // fullServiceName : property fullServiceName 
   property fullServiceName:WideString read Get_fullServiceName;
    // friendlyName : property friendlyName 
   property friendlyName:WideString read Get_friendlyName;
    // guideButtonText : property guideButtonText 
   property guideButtonText:WideString read Get_guideButtonText;
    // guideButtonTip : property guideButtonTip 
   property guideButtonTip:WideString read Get_guideButtonTip;
    // guideMenuText : property guideMenuText 
   property guideMenuText:WideString read Get_guideMenuText;
    // guideAccText : property guideAccText 
   property guideAccText:WideString read Get_guideAccText;
    // task1ButtonText : property task1ButtonText 
   property task1ButtonText:WideString read Get_task1ButtonText;
    // task1ButtonTip : property task1ButtonTip 
   property task1ButtonTip:WideString read Get_task1ButtonTip;
    // task1MenuText : property task1MenuText 
   property task1MenuText:WideString read Get_task1MenuText;
    // task1AccText : property task1AccText 
   property task1AccText:WideString read Get_task1AccText;
    // guideUrl : property guideUrl 
   property guideUrl:WideString read Get_guideUrl;
    // task1Url : property task1Url 
   property task1Url:WideString read Get_task1Url;
    // imageLargeUrl : property imageLargeUrl 
   property imageLargeUrl:WideString read Get_imageLargeUrl;
    // imageSmallUrl : property imageSmallUrl 
   property imageSmallUrl:WideString read Get_imageSmallUrl;
    // imageMenuUrl : property imageMenuUrl 
   property imageMenuUrl:WideString read Get_imageMenuUrl;
    // infoCenterUrl : property infoCenterUrl 
   property infoCenterUrl:WideString read Get_infoCenterUrl;
    // albumInfoUrl : property albumInfoUrl 
   property albumInfoUrl:WideString read Get_albumInfoUrl;
    // buyCDUrl : property buyCDUrl 
   property buyCDUrl:WideString read Get_buyCDUrl;
    // htmlViewUrl : property htmlViewUrl 
   property htmlViewUrl:WideString read Get_htmlViewUrl;
    // navigateUrl : property navigateUrl 
   property navigateUrl:WideString read Get_navigateUrl;
    // cookieUrl : property cookieUrl 
   property cookieUrl:WideString read Get_cookieUrl;
    // downloadStatusUrl : property downloadStatusUrl 
   property downloadStatusUrl:WideString read Get_downloadStatusUrl;
    // colorPlayer : property colorPlayer 
   property colorPlayer:WideString read Get_colorPlayer;
    // colorPlayerText : property colorPlayerText 
   property colorPlayerText:WideString read Get_colorPlayerText;
    // navigateDispid : property navigateDispid 
   property navigateDispid:Integer read Get_navigateDispid;
    // navigateParams : property navigateParams 
   property navigateParams:WideString read Get_navigateParams;
    // navigatePane : property navigatePane 
   property navigatePane:WideString read Get_navigatePane;
    // selectedPane : property selectedPane 
   property selectedPane:WideString read Get_selectedPane write Set_selectedPane;
    // selectedTask : property selectedTask 
   property selectedTask:Integer write Set_selectedTask;
    // contentPartnerSelected : property contentPartnerSelected 
   property contentPartnerSelected:WordBool read Get_contentPartnerSelected;
  end;


// IWMPBrandDispatch : IWMPBrandDispatch: Not Public.  Internal interface used by Windows Media Player.

 IWMPBrandDispatchDisp = dispinterface
   ['{98BB02D4-ED74-43CC-AD6A-45888F2E0DCC}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // setNavigateProps : method setNavigateProps 
   procedure setNavigateProps(bstrPane:WideString;lDispid:Integer;bstrParams:WideString);dispid 3041;
    // getMediaParams : method getMediaParams 
   function getMediaParams(pObject:IUnknown;bstrURL:WideString):WideString;dispid 3042;
    // fullServiceName : property fullServiceName 
   property fullServiceName:WideString  readonly dispid 3040;
    // friendlyName : property friendlyName 
   property friendlyName:WideString  readonly dispid 3000;
    // guideButtonText : property guideButtonText 
   property guideButtonText:WideString  readonly dispid 3001;
    // guideButtonTip : property guideButtonTip 
   property guideButtonTip:WideString  readonly dispid 3002;
    // guideMenuText : property guideMenuText 
   property guideMenuText:WideString  readonly dispid 3003;
    // guideAccText : property guideAccText 
   property guideAccText:WideString  readonly dispid 3004;
    // task1ButtonText : property task1ButtonText 
   property task1ButtonText:WideString  readonly dispid 3005;
    // task1ButtonTip : property task1ButtonTip 
   property task1ButtonTip:WideString  readonly dispid 3006;
    // task1MenuText : property task1MenuText 
   property task1MenuText:WideString  readonly dispid 3007;
    // task1AccText : property task1AccText 
   property task1AccText:WideString  readonly dispid 3008;
    // guideUrl : property guideUrl 
   property guideUrl:WideString  readonly dispid 3017;
    // task1Url : property task1Url 
   property task1Url:WideString  readonly dispid 3018;
    // imageLargeUrl : property imageLargeUrl 
   property imageLargeUrl:WideString  readonly dispid 3021;
    // imageSmallUrl : property imageSmallUrl 
   property imageSmallUrl:WideString  readonly dispid 3022;
    // imageMenuUrl : property imageMenuUrl 
   property imageMenuUrl:WideString  readonly dispid 3023;
    // infoCenterUrl : property infoCenterUrl 
   property infoCenterUrl:WideString  readonly dispid 3024;
    // albumInfoUrl : property albumInfoUrl 
   property albumInfoUrl:WideString  readonly dispid 3025;
    // buyCDUrl : property buyCDUrl 
   property buyCDUrl:WideString  readonly dispid 3026;
    // htmlViewUrl : property htmlViewUrl 
   property htmlViewUrl:WideString  readonly dispid 3027;
    // navigateUrl : property navigateUrl 
   property navigateUrl:WideString  readonly dispid 3028;
    // cookieUrl : property cookieUrl 
   property cookieUrl:WideString  readonly dispid 3029;
    // downloadStatusUrl : property downloadStatusUrl 
   property downloadStatusUrl:WideString  readonly dispid 3030;
    // colorPlayer : property colorPlayer 
   property colorPlayer:WideString  readonly dispid 3031;
    // colorPlayerText : property colorPlayerText 
   property colorPlayerText:WideString  readonly dispid 3032;
    // navigateDispid : property navigateDispid 
   property navigateDispid:Integer  readonly dispid 3035;
    // navigateParams : property navigateParams 
   property navigateParams:WideString  readonly dispid 3036;
    // navigatePane : property navigatePane 
   property navigatePane:WideString  readonly dispid 3037;
    // selectedPane : property selectedPane 
   property selectedPane:WideString dispid 3038;
    // selectedTask : property selectedTask 
   property selectedTask:Integer writeonly dispid 3039;
    // contentPartnerSelected : property contentPartnerSelected 
   property contentPartnerSelected:WordBool  readonly dispid 3043;
  end;


// IWMPNowPlayingHelperDispatch : IWMPNowPlayingHelperDispatch: Not Public.  Internal interface used by Windows Media Player.

 IWMPNowPlayingHelperDispatch = interface(IDispatch)
   ['{504F112E-77CC-4E3C-A073-5371B31D9B36}']
   function Get_viewFriendlyName(bstrView:WideString) : WideString; safecall;
   function Get_viewPresetCount(bstrView:WideString) : Integer; safecall;
   function Get_viewPresetName(bstrView:WideString) : WideString; safecall;
   function Get_effectFriendlyName(bstrEffect:WideString) : WideString; safecall;
   function Get_effectPresetName(bstrEffect:WideString) : WideString; safecall;
    // resolveDisplayView : method resolveDisplayView 
   function resolveDisplayView(fSafe:WordBool):WideString;safecall;
    // isValidDisplayView : method isValidDisplayView 
   function isValidDisplayView(bstrView:WideString):WordBool;safecall;
    // getSkinFile : method getSkinFile 
   function getSkinFile:WideString;safecall;
   function Get_captionsAvailable : WordBool; safecall;
   function Get_linkAvailable : Integer; safecall;
   function Get_linkRequest : Integer; safecall;
   procedure Set_linkRequest(const pVal:Integer); safecall;
   function Get_linkRequestParams : WideString; safecall;
   procedure Set_linkRequestParams(const pVal:WideString); safecall;
    // getCurrentArtID : method getCurrentArtID 
   function getCurrentArtID(fLargeArt:WordBool):Integer;safecall;
    // getTimeString : method getTimeString 
   function getTimeString(dTime:Double):WideString;safecall;
    // getCurrentScriptCommand : method getCurrentScriptCommand 
   function getCurrentScriptCommand(bstrType:WideString):WideString;safecall;
    // calcLayout : method calcLayout 
   procedure calcLayout(lWidth:Integer;lHeight:Integer;vbCaptions:WordBool;vbBanner:WordBool);safecall;
    // getLayoutSize : method getLayoutSize 
   function getLayoutSize(nProp:Integer):Integer;safecall;
    // getRootPlaylist : method getRootPlaylist 
   function getRootPlaylist(pPlaylist:IDispatch):IDispatch;safecall;
    // getHTMLViewURL : method getHTMLViewURL 
   function getHTMLViewURL:WideString;safecall;
   function Get_editObj : IUnknown; safecall;
   procedure Set_editObj(const ppVal:IUnknown); safecall;
    // getStatusString : method getStatusString 
   function getStatusString(bstrStatusId:WideString):WideString;safecall;
    // getStatusPct : method getStatusPct 
   function getStatusPct(bstrStatusId:WideString):Integer;safecall;
    // getStatusResult : method getStatusResult 
   function getStatusResult(bstrStatusId:WideString):Integer;safecall;
    // getStatusIcon : method getStatusIcon 
   function getStatusIcon(bstrStatusId:WideString):Integer;safecall;
    // getStatusIdList : method getStatusIdList 
   function getStatusIdList:WideString;safecall;
   function Get_notificationString : WideString; safecall;
   function Get_htmlViewBaseURL : WideString; safecall;
   procedure Set_htmlViewBaseURL(const pVal:WideString); safecall;
   function Get_htmlViewFullURL : WideString; safecall;
   procedure Set_htmlViewFullURL(const pVal:WideString); safecall;
   function Get_htmlViewSecureLock : Integer; safecall;
   procedure Set_htmlViewSecureLock(const pVal:Integer); safecall;
   function Get_htmlViewBusy : WordBool; safecall;
   procedure Set_htmlViewBusy(const pVal:WordBool); safecall;
   function Get_htmlViewShowCert : WordBool; safecall;
   procedure Set_htmlViewShowCert(const pVal:WordBool); safecall;
   function Get_previousEnabled : WordBool; safecall;
   procedure Set_previousEnabled(const pVal:WordBool); safecall;
   function Get_doPreviousNow : WordBool; safecall;
   procedure Set_doPreviousNow(const pVal:WordBool); safecall;
   function Get_DPI : Integer; safecall;
    // clearColors : clear all user color info 
   procedure clearColors;safecall;
   function Get_lastMessage : WideString; safecall;
   procedure Set_lastMessage(const pVal:WideString); safecall;
   function Get_inVistaPlus : WordBool; safecall;
   function Get_isBidi : WordBool; safecall;
   function Get_isOCX : WordBool; safecall;
   function Get_hoverTransportsEnabled : WordBool; safecall;
    // initRipHelper :  
   procedure initRipHelper;safecall;
   function Get_isAudioCD : WordBool; safecall;
   procedure Set_isAudioCD(const pVal:WordBool); safecall;
   function Get_canRip : WordBool; safecall;
   procedure Set_canRip(const pVal:WordBool); safecall;
   function Get_isRipping : WordBool; safecall;
   procedure Set_isRipping(const pVal:WordBool); safecall;
   function Get_currentDrive : WideString; safecall;
   procedure Set_currentDrive(const pVal:WideString); safecall;
    // startRip :  
   procedure startRip;safecall;
    // stopRip :  
   procedure stopRip;safecall;
   function Get_showMMO : WordBool; safecall;
   procedure Set_showMMO(const pVal:WordBool); safecall;
   function Get_MMOVisible : WordBool; safecall;
   function Get_suggestionsVisible : WordBool; safecall;
   function Get_suggestionsTextColor : WideString; safecall;
   function Get_fontFace : WideString; safecall;
   function Get_fontSize : Integer; safecall;
   function Get_backgroundColor : WideString; safecall;
   function Get_doubleClickTime : Integer; safecall;
   function Get_playAgain : WordBool; safecall;
   function Get_previousPlaylistAvailable : WordBool; safecall;
   function Get_nextPlaylistAvailable : WordBool; safecall;
    // nextPlaylist :  
   procedure nextPlaylist;safecall;
    // previousPlaylist :  
   procedure previousPlaylist;safecall;
    // playOffsetMedia :  
   procedure playOffsetMedia(iOffset:Integer);safecall;
   function Get_basketVisible : WordBool; safecall;
   procedure Set_basketVisible(const pVal:WordBool); safecall;
   function Get_mmoTextColor : WideString; safecall;
   function Get_backgroundVisible : WordBool; safecall;
   procedure Set_backgroundEnabled(const pVal:WordBool); safecall;
   function Get_backgroundEnabled : WordBool; safecall;
   procedure Set_backgroundIndex(const pVal:Integer); safecall;
   function Get_backgroundIndex : Integer; safecall;
   function Get_upNext : WideString; safecall;
   function Get_playbackOverlayVisible : WordBool; safecall;
   function Get_remoted : WordBool; safecall;
   function Get_glassEnabled : WordBool; safecall;
   function Get_highContrast : WordBool; safecall;
   procedure Set_testHighContrast(const Param1:WideString); safecall;
   function Get_sessionPlaylistCount : Integer; safecall;
    // setGestureStatus :  
   procedure setGestureStatus(pObject:IDispatch;newVal:Integer);safecall;
   function Get_metadataString : WideString; safecall;
   procedure Set_metadataString(const pVal:WideString); safecall;
   function Get_albumArtAlpha : Integer; safecall;
   function Get_playerModeAlbumArtSelected : WordBool; safecall;
   function Get_inFullScreen : WordBool; safecall;
    // syncToAlbumArt :  
   procedure syncToAlbumArt(pObject:IDispatch;iOffsetFromCurrentMedia:Integer;bstrFallbackImage:WideString);safecall;
   function Get_resourceIdForDpi(iResourceId:SYSINT) : SYSINT; safecall;
    // viewFriendlyName : property viewFriendlyName 
   property viewFriendlyName[bstrView:WideString]:WideString read Get_viewFriendlyName;
    // viewPresetCount : property viewPresetCount 
   property viewPresetCount[bstrView:WideString]:Integer read Get_viewPresetCount;
    // viewPresetName : method viewPresetName 
   property viewPresetName[bstrView:WideString]:WideString read Get_viewPresetName;
    // effectFriendlyName : property effectFriendlyName 
   property effectFriendlyName[bstrEffect:WideString]:WideString read Get_effectFriendlyName;
    // effectPresetName : method effectPresetName 
   property effectPresetName[bstrEffect:WideString]:WideString read Get_effectPresetName;
    // captionsAvailable : method captionsAvailable 
   property captionsAvailable:WordBool read Get_captionsAvailable;
    // linkAvailable : property linkAvailable 
   property linkAvailable:Integer read Get_linkAvailable;
    // linkRequest : property linkRequest 
   property linkRequest:Integer read Get_linkRequest write Set_linkRequest;
    // linkRequestParams : property linkRequestParams 
   property linkRequestParams:WideString read Get_linkRequestParams write Set_linkRequestParams;
    // editObj :  
   property editObj:IUnknown read Get_editObj write Set_editObj;
    // notificationString :  
   property notificationString:WideString read Get_notificationString;
    // htmlViewBaseURL :  
   property htmlViewBaseURL:WideString read Get_htmlViewBaseURL write Set_htmlViewBaseURL;
    // htmlViewFullURL :  
   property htmlViewFullURL:WideString read Get_htmlViewFullURL write Set_htmlViewFullURL;
    // htmlViewSecureLock :  
   property htmlViewSecureLock:Integer read Get_htmlViewSecureLock write Set_htmlViewSecureLock;
    // htmlViewBusy :  
   property htmlViewBusy:WordBool read Get_htmlViewBusy write Set_htmlViewBusy;
    // htmlViewShowCert :  
   property htmlViewShowCert:WordBool read Get_htmlViewShowCert write Set_htmlViewShowCert;
    // previousEnabled :  
   property previousEnabled:WordBool read Get_previousEnabled write Set_previousEnabled;
    // doPreviousNow :  
   property doPreviousNow:WordBool read Get_doPreviousNow write Set_doPreviousNow;
    // DPI :  
   property DPI:Integer read Get_DPI;
    // lastMessage :  
   property lastMessage:WideString read Get_lastMessage write Set_lastMessage;
    // inVistaPlus :  
   property inVistaPlus:WordBool read Get_inVistaPlus;
    // isBidi :  
   property isBidi:WordBool read Get_isBidi;
    // isOCX :  
   property isOCX:WordBool read Get_isOCX;
    // hoverTransportsEnabled :  
   property hoverTransportsEnabled:WordBool read Get_hoverTransportsEnabled;
    // isAudioCD :  
   property isAudioCD:WordBool read Get_isAudioCD write Set_isAudioCD;
    // canRip :  
   property canRip:WordBool read Get_canRip write Set_canRip;
    // isRipping :  
   property isRipping:WordBool read Get_isRipping write Set_isRipping;
    // currentDrive :  
   property currentDrive:WideString read Get_currentDrive write Set_currentDrive;
    // showMMO :  
   property showMMO:WordBool read Get_showMMO write Set_showMMO;
    // MMOVisible :  
   property MMOVisible:WordBool read Get_MMOVisible;
    // suggestionsVisible :  
   property suggestionsVisible:WordBool read Get_suggestionsVisible;
    // suggestionsTextColor :  
   property suggestionsTextColor:WideString read Get_suggestionsTextColor;
    // fontFace :  
   property fontFace:WideString read Get_fontFace;
    // fontSize :  
   property fontSize:Integer read Get_fontSize;
    // backgroundColor :  
   property backgroundColor:WideString read Get_backgroundColor;
    // doubleClickTime :  
   property doubleClickTime:Integer read Get_doubleClickTime;
    // playAgain :  
   property playAgain:WordBool read Get_playAgain;
    // previousPlaylistAvailable :  
   property previousPlaylistAvailable:WordBool read Get_previousPlaylistAvailable;
    // nextPlaylistAvailable :  
   property nextPlaylistAvailable:WordBool read Get_nextPlaylistAvailable;
    // basketVisible :  
   property basketVisible:WordBool read Get_basketVisible write Set_basketVisible;
    // mmoTextColor :  
   property mmoTextColor:WideString read Get_mmoTextColor;
    // backgroundVisible :  
   property backgroundVisible:WordBool read Get_backgroundVisible;
    // backgroundEnabled :  
   property backgroundEnabled:WordBool read Get_backgroundEnabled write Set_backgroundEnabled;
    // backgroundIndex :  
   property backgroundIndex:Integer read Get_backgroundIndex write Set_backgroundIndex;
    // upNext :  
   property upNext:WideString read Get_upNext;
    // playbackOverlayVisible :  
   property playbackOverlayVisible:WordBool read Get_playbackOverlayVisible;
    // remoted :  
   property remoted:WordBool read Get_remoted;
    // glassEnabled :  
   property glassEnabled:WordBool read Get_glassEnabled;
    // highContrast :  
   property highContrast:WordBool read Get_highContrast;
    // testHighContrast :  
   property testHighContrast:WideString write Set_testHighContrast;
    // sessionPlaylistCount :  
   property sessionPlaylistCount:Integer read Get_sessionPlaylistCount;
    // metadataString :  
   property metadataString:WideString read Get_metadataString write Set_metadataString;
    // albumArtAlpha :  
   property albumArtAlpha:Integer read Get_albumArtAlpha;
    // playerModeAlbumArtSelected :  
   property playerModeAlbumArtSelected:WordBool read Get_playerModeAlbumArtSelected;
    // inFullScreen :  
   property inFullScreen:WordBool read Get_inFullScreen;
    // resourceIdForDpi :  
   property resourceIdForDpi[iResourceId:SYSINT]:SYSINT read Get_resourceIdForDpi;
  end;


// IWMPNowPlayingHelperDispatch : IWMPNowPlayingHelperDispatch: Not Public.  Internal interface used by Windows Media Player.

 IWMPNowPlayingHelperDispatchDisp = dispinterface
   ['{504F112E-77CC-4E3C-A073-5371B31D9B36}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // resolveDisplayView : method resolveDisplayView 
   function resolveDisplayView(fSafe:WordBool):WideString;dispid 2909;
    // isValidDisplayView : method isValidDisplayView 
   function isValidDisplayView(bstrView:WideString):WordBool;dispid 2910;
    // getSkinFile : method getSkinFile 
   function getSkinFile:WideString;dispid 2911;
    // getCurrentArtID : method getCurrentArtID 
   function getCurrentArtID(fLargeArt:WordBool):Integer;dispid 2917;
    // getTimeString : method getTimeString 
   function getTimeString(dTime:Double):WideString;dispid 2918;
    // getCurrentScriptCommand : method getCurrentScriptCommand 
   function getCurrentScriptCommand(bstrType:WideString):WideString;dispid 2919;
    // calcLayout : method calcLayout 
   procedure calcLayout(lWidth:Integer;lHeight:Integer;vbCaptions:WordBool;vbBanner:WordBool);dispid 2920;
    // getLayoutSize : method getLayoutSize 
   function getLayoutSize(nProp:Integer):Integer;dispid 2921;
    // getRootPlaylist : method getRootPlaylist 
   function getRootPlaylist(pPlaylist:IDispatch):IDispatch;dispid 2922;
    // getHTMLViewURL : method getHTMLViewURL 
   function getHTMLViewURL:WideString;dispid 2923;
    // getStatusString : method getStatusString 
   function getStatusString(bstrStatusId:WideString):WideString;dispid 2927;
    // getStatusPct : method getStatusPct 
   function getStatusPct(bstrStatusId:WideString):Integer;dispid 2939;
    // getStatusResult : method getStatusResult 
   function getStatusResult(bstrStatusId:WideString):Integer;dispid 2940;
    // getStatusIcon : method getStatusIcon 
   function getStatusIcon(bstrStatusId:WideString):Integer;dispid 2941;
    // getStatusIdList : method getStatusIdList 
   function getStatusIdList:WideString;dispid 2942;
    // clearColors : clear all user color info 
   procedure clearColors;dispid 2937;
    // initRipHelper :  
   procedure initRipHelper;dispid 2947;
    // startRip :  
   procedure startRip;dispid 2952;
    // stopRip :  
   procedure stopRip;dispid 2953;
    // nextPlaylist :  
   procedure nextPlaylist;dispid 2961;
    // previousPlaylist :  
   procedure previousPlaylist;dispid 2962;
    // playOffsetMedia :  
   procedure playOffsetMedia(iOffset:Integer);dispid 2972;
    // setGestureStatus :  
   procedure setGestureStatus(pObject:IDispatch;newVal:Integer);dispid 2980;
    // syncToAlbumArt :  
   procedure syncToAlbumArt(pObject:IDispatch;iOffsetFromCurrentMedia:Integer;bstrFallbackImage:WideString);dispid 2985;
    // viewFriendlyName : property viewFriendlyName 
   property viewFriendlyName[bstrView:WideString]:WideString  readonly dispid 2901;
    // viewPresetCount : property viewPresetCount 
   property viewPresetCount[bstrView:WideString]:Integer  readonly dispid 2902;
    // viewPresetName : method viewPresetName 
   property viewPresetName[bstrView:WideString]:WideString  readonly dispid 2903;
    // effectFriendlyName : property effectFriendlyName 
   property effectFriendlyName[bstrEffect:WideString]:WideString  readonly dispid 2904;
    // effectPresetName : method effectPresetName 
   property effectPresetName[bstrEffect:WideString]:WideString  readonly dispid 2905;
    // captionsAvailable : method captionsAvailable 
   property captionsAvailable:WordBool  readonly dispid 2912;
    // linkAvailable : property linkAvailable 
   property linkAvailable:Integer  readonly dispid 2913;
    // linkRequest : property linkRequest 
   property linkRequest:Integer dispid 2914;
    // linkRequestParams : property linkRequestParams 
   property linkRequestParams:WideString dispid 2915;
    // editObj :  
   property editObj:IUnknown dispid 2926;
    // notificationString :  
   property notificationString:WideString  readonly dispid 2928;
    // htmlViewBaseURL :  
   property htmlViewBaseURL:WideString dispid 2930;
    // htmlViewFullURL :  
   property htmlViewFullURL:WideString dispid 2933;
    // htmlViewSecureLock :  
   property htmlViewSecureLock:Integer dispid 2929;
    // htmlViewBusy :  
   property htmlViewBusy:WordBool dispid 2931;
    // htmlViewShowCert :  
   property htmlViewShowCert:WordBool dispid 2932;
    // previousEnabled :  
   property previousEnabled:WordBool dispid 2934;
    // doPreviousNow :  
   property doPreviousNow:WordBool dispid 2935;
    // DPI :  
   property DPI:Integer  readonly dispid 2936;
    // lastMessage :  
   property lastMessage:WideString dispid 2938;
    // inVistaPlus :  
   property inVistaPlus:WordBool  readonly dispid 2943;
    // isBidi :  
   property isBidi:WordBool  readonly dispid 2944;
    // isOCX :  
   property isOCX:WordBool  readonly dispid 2945;
    // hoverTransportsEnabled :  
   property hoverTransportsEnabled:WordBool  readonly dispid 2946;
    // isAudioCD :  
   property isAudioCD:WordBool dispid 2948;
    // canRip :  
   property canRip:WordBool dispid 2949;
    // isRipping :  
   property isRipping:WordBool dispid 2950;
    // currentDrive :  
   property currentDrive:WideString dispid 2951;
    // showMMO :  
   property showMMO:WordBool dispid 2954;
    // MMOVisible :  
   property MMOVisible:WordBool  readonly dispid 2971;
    // suggestionsVisible :  
   property suggestionsVisible:WordBool  readonly dispid 2955;
    // suggestionsTextColor :  
   property suggestionsTextColor:WideString  readonly dispid 2956;
    // fontFace :  
   property fontFace:WideString  readonly dispid 2964;
    // fontSize :  
   property fontSize:Integer  readonly dispid 2965;
    // backgroundColor :  
   property backgroundColor:WideString  readonly dispid 2966;
    // doubleClickTime :  
   property doubleClickTime:Integer  readonly dispid 2957;
    // playAgain :  
   property playAgain:WordBool  readonly dispid 2958;
    // previousPlaylistAvailable :  
   property previousPlaylistAvailable:WordBool  readonly dispid 2959;
    // nextPlaylistAvailable :  
   property nextPlaylistAvailable:WordBool  readonly dispid 2960;
    // basketVisible :  
   property basketVisible:WordBool dispid 2963;
    // mmoTextColor :  
   property mmoTextColor:WideString  readonly dispid 2967;
    // backgroundVisible :  
   property backgroundVisible:WordBool  readonly dispid 2968;
    // backgroundEnabled :  
   property backgroundEnabled:WordBool dispid 2969;
    // backgroundIndex :  
   property backgroundIndex:Integer dispid 2970;
    // upNext :  
   property upNext:WideString  readonly dispid 2973;
    // playbackOverlayVisible :  
   property playbackOverlayVisible:WordBool  readonly dispid 2974;
    // remoted :  
   property remoted:WordBool  readonly dispid 2975;
    // glassEnabled :  
   property glassEnabled:WordBool  readonly dispid 2976;
    // highContrast :  
   property highContrast:WordBool  readonly dispid 2977;
    // testHighContrast :  
   property testHighContrast:WideString writeonly dispid 2978;
    // sessionPlaylistCount :  
   property sessionPlaylistCount:{!! pointer !!} OleVariant  readonly dispid 2979;
    // metadataString :  
   property metadataString:WideString dispid 2981;
    // albumArtAlpha :  
   property albumArtAlpha:Integer  readonly dispid 2982;
    // playerModeAlbumArtSelected :  
   property playerModeAlbumArtSelected:WordBool  readonly dispid 2983;
    // inFullScreen :  
   property inFullScreen:WordBool  readonly dispid 2984;
    // resourceIdForDpi :  
   property resourceIdForDpi[iResourceId:SYSINT]:SYSINT  readonly dispid 2986;
  end;


// IWMPNowDoingDispatch : IWMPNowDoingDispatch: Not Public.  Internal interface used by Windows Media Player.

 IWMPNowDoingDispatch = interface(IDispatch)
   ['{2A2E0DA3-19FA-4F82-BE18-CD7D7A3B977F}']
    // hideBasket : method hideBasket 
   procedure hideBasket;safecall;
    // burnNavigateToStatus : method burnNavigateToStatus 
   procedure burnNavigateToStatus;safecall;
    // syncNavigateToStatus : method syncNavigateToStatus 
   procedure syncNavigateToStatus;safecall;
   function Get_DPI : Integer; safecall;
   function Get_mode : WideString; safecall;
   procedure Set_burn_selectedDrive(const pVal:Integer); safecall;
   function Get_burn_selectedDrive : Integer; safecall;
   function Get_sync_selectedDevice : Integer; safecall;
   procedure Set_sync_selectedDevice(const pVal:Integer); safecall;
   function Get_burn_numDiscsSpanned : Integer; safecall;
   function Get_editPlaylist : IDispatch; safecall;
   function Get_basketPlaylistName : WideString; safecall;
   function Get_isHighContrastMode : WordBool; safecall;
   function Get_allowRating : WordBool; safecall;
   function Get_burn_mediaType : WideString; safecall;
   function Get_burn_contentType : WideString; safecall;
   function Get_burn_freeSpace : Integer; safecall;
   function Get_burn_totalSpace : Integer; safecall;
   function Get_burn_driveName : WideString; safecall;
   function Get_burn_numDevices : Integer; safecall;
   function Get_burn_spaceToUse : Integer; safecall;
   function Get_burn_percentComplete : Integer; safecall;
   function Get_sync_spaceToUse : Integer; safecall;
   function Get_sync_spaceUsed : Integer; safecall;
   function Get_sync_totalSpace : Integer; safecall;
   function Get_sync_deviceName : WideString; safecall;
   function Get_sync_numDevices : Integer; safecall;
   function Get_sync_oemName : WideString; safecall;
   function Get_sync_percentComplete : Integer; safecall;
    // logData : method logData 
   procedure logData(ID:WideString;data:WideString);safecall;
    // formatTime : method formatTime 
   function formatTime(value:Integer):WideString;safecall;
    // DPI :  
   property DPI:Integer read Get_DPI;
    // mode : property mode 
   property mode:WideString read Get_mode;
    // burn_selectedDrive : property burn_selectedDrive 
   property burn_selectedDrive:Integer read Get_burn_selectedDrive write Set_burn_selectedDrive;
    // sync_selectedDevice : property sync_selectedDevice 
   property sync_selectedDevice:Integer read Get_sync_selectedDevice write Set_sync_selectedDevice;
    // burn_numDiscsSpanned : property burn_numDiscsSpanned 
   property burn_numDiscsSpanned:Integer read Get_burn_numDiscsSpanned;
    // editPlaylist : method editPlaylist 
   property editPlaylist:IDispatch read Get_editPlaylist;
    // basketPlaylistName : property basketPlaylistName 
   property basketPlaylistName:WideString read Get_basketPlaylistName;
    // isHighContrastMode : property isHighContrastMode 
   property isHighContrastMode:WordBool read Get_isHighContrastMode;
    // allowRating : property allowRating 
   property allowRating:WordBool read Get_allowRating;
    // burn_mediaType : property burn_mediaType 
   property burn_mediaType:WideString read Get_burn_mediaType;
    // burn_contentType : property burn_contentType 
   property burn_contentType:WideString read Get_burn_contentType;
    // burn_freeSpace : property burn_freeSpace 
   property burn_freeSpace:Integer read Get_burn_freeSpace;
    // burn_totalSpace : property burn_totalSpace 
   property burn_totalSpace:Integer read Get_burn_totalSpace;
    // burn_driveName : property burn_driveName 
   property burn_driveName:WideString read Get_burn_driveName;
    // burn_numDevices : property burn_numDevices 
   property burn_numDevices:Integer read Get_burn_numDevices;
    // burn_spaceToUse : property burn_spaceToUse 
   property burn_spaceToUse:Integer read Get_burn_spaceToUse;
    // burn_percentComplete : property burn_percentComplete 
   property burn_percentComplete:Integer read Get_burn_percentComplete;
    // sync_spaceToUse : property sync_spaceToUse 
   property sync_spaceToUse:Integer read Get_sync_spaceToUse;
    // sync_spaceUsed : property sync_spaceUsed 
   property sync_spaceUsed:Integer read Get_sync_spaceUsed;
    // sync_totalSpace : property sync_totalSpace 
   property sync_totalSpace:Integer read Get_sync_totalSpace;
    // sync_deviceName : property sync_deviceName 
   property sync_deviceName:WideString read Get_sync_deviceName;
    // sync_numDevices : property sync_numDevices 
   property sync_numDevices:Integer read Get_sync_numDevices;
    // sync_oemName : property sync_oemName 
   property sync_oemName:WideString read Get_sync_oemName;
    // sync_percentComplete : property sync_percentComplete 
   property sync_percentComplete:Integer read Get_sync_percentComplete;
  end;


// IWMPNowDoingDispatch : IWMPNowDoingDispatch: Not Public.  Internal interface used by Windows Media Player.

 IWMPNowDoingDispatchDisp = dispinterface
   ['{2A2E0DA3-19FA-4F82-BE18-CD7D7A3B977F}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // hideBasket : method hideBasket 
   procedure hideBasket;dispid 3222;
    // burnNavigateToStatus : method burnNavigateToStatus 
   procedure burnNavigateToStatus;dispid 3211;
    // syncNavigateToStatus : method syncNavigateToStatus 
   procedure syncNavigateToStatus;dispid 3220;
    // logData : method logData 
   procedure logData(ID:WideString;data:WideString);dispid 3224;
    // formatTime : method formatTime 
   function formatTime(value:Integer):WideString;dispid 3226;
    // DPI :  
   property DPI:Integer  readonly dispid 3223;
    // mode : property mode 
   property mode:WideString  readonly dispid 3200;
    // burn_selectedDrive : property burn_selectedDrive 
   property burn_selectedDrive:Integer dispid 3206;
    // sync_selectedDevice : property sync_selectedDevice 
   property sync_selectedDevice:Integer dispid 3216;
    // burn_numDiscsSpanned : property burn_numDiscsSpanned 
   property burn_numDiscsSpanned:Integer  readonly dispid 3208;
    // editPlaylist : method editPlaylist 
   property editPlaylist:IDispatch  readonly dispid 3225;
    // basketPlaylistName : property basketPlaylistName 
   property basketPlaylistName:WideString  readonly dispid 3227;
    // isHighContrastMode : property isHighContrastMode 
   property isHighContrastMode:WordBool  readonly dispid 3228;
    // allowRating : property allowRating 
   property allowRating:WordBool  readonly dispid 3229;
    // burn_mediaType : property burn_mediaType 
   property burn_mediaType:WideString  readonly dispid 3201;
    // burn_contentType : property burn_contentType 
   property burn_contentType:WideString  readonly dispid 3202;
    // burn_freeSpace : property burn_freeSpace 
   property burn_freeSpace:Integer  readonly dispid 3203;
    // burn_totalSpace : property burn_totalSpace 
   property burn_totalSpace:Integer  readonly dispid 3204;
    // burn_driveName : property burn_driveName 
   property burn_driveName:WideString  readonly dispid 3205;
    // burn_numDevices : property burn_numDevices 
   property burn_numDevices:Integer  readonly dispid 3207;
    // burn_spaceToUse : property burn_spaceToUse 
   property burn_spaceToUse:Integer  readonly dispid 3209;
    // burn_percentComplete : property burn_percentComplete 
   property burn_percentComplete:Integer  readonly dispid 3210;
    // sync_spaceToUse : property sync_spaceToUse 
   property sync_spaceToUse:Integer  readonly dispid 3212;
    // sync_spaceUsed : property sync_spaceUsed 
   property sync_spaceUsed:Integer  readonly dispid 3213;
    // sync_totalSpace : property sync_totalSpace 
   property sync_totalSpace:Integer  readonly dispid 3214;
    // sync_deviceName : property sync_deviceName 
   property sync_deviceName:WideString  readonly dispid 3215;
    // sync_numDevices : property sync_numDevices 
   property sync_numDevices:Integer  readonly dispid 3217;
    // sync_oemName : property sync_oemName 
   property sync_oemName:WideString  readonly dispid 3218;
    // sync_percentComplete : property sync_percentComplete 
   property sync_percentComplete:Integer  readonly dispid 3219;
  end;


// IWMPHoverPreviewDispatch : IWMPHoverPreviewDispatch: Not Public.  Internal interface used by Windows Media Player.

 IWMPHoverPreviewDispatch = interface(IDispatch)
   ['{946B023E-044C-4473-8018-74954F09DC7E}']
   function Get_title : WideString; safecall;
   function Get_album : WideString; safecall;
   function Get_URL : WideString; safecall;
   procedure Set_image(const Param1:IDispatch); safecall;
   procedure Set_autoClick(const Param1:WordBool); safecall;
   procedure Set_previewClick(const Param1:WordBool); safecall;
    // dismiss :  
   procedure dismiss;safecall;
    // title :  
   property title:WideString read Get_title;
    // album :  
   property album:WideString read Get_album;
    // URL :  
   property URL:WideString read Get_URL;
    // image :  
   property image:IDispatch write Set_image;
    // autoClick :  
   property autoClick:WordBool write Set_autoClick;
    // previewClick :  
   property previewClick:WordBool write Set_previewClick;
  end;


// IWMPHoverPreviewDispatch : IWMPHoverPreviewDispatch: Not Public.  Internal interface used by Windows Media Player.

 IWMPHoverPreviewDispatchDisp = dispinterface
   ['{946B023E-044C-4473-8018-74954F09DC7E}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // dismiss :  
   procedure dismiss;dispid 3156;
    // title :  
   property title:WideString  readonly dispid 3150;
    // album :  
   property album:WideString  readonly dispid 3151;
    // URL :  
   property URL:WideString  readonly dispid 3153;
    // image :  
   property image:IDispatch writeonly dispid 3152;
    // autoClick :  
   property autoClick:WordBool writeonly dispid 3155;
    // previewClick :  
   property previewClick:WordBool writeonly dispid 3154;
  end;


// IWMPButtonCtrlEvents : IWMPButtonCtrlEvents: Public interface for skin object model.

 IWMPButtonCtrlEvents = dispinterface
   ['{BB17FFF7-1692-4555-918A-6AF7BFACEDD2}']
    // onclick : event ondragbegin 
   function onclick:HResult;dispid 5120;
  end;


// IWMPButtonCtrl : IWMPButtonCtrl: Public interface for skin object model.

 IWMPButtonCtrl = interface(IDispatch)
   ['{87291B50-0C8E-11D3-BB2A-00A0C93CA73A}']
   function Get_image : WideString; safecall;
   procedure Set_image(const pVal:WideString); safecall;
   function Get_hoverImage : WideString; safecall;
   procedure Set_hoverImage(const pVal:WideString); safecall;
   function Get_downImage : WideString; safecall;
   procedure Set_downImage(const pVal:WideString); safecall;
   function Get_disabledImage : WideString; safecall;
   procedure Set_disabledImage(const pVal:WideString); safecall;
   function Get_hoverDownImage : WideString; safecall;
   procedure Set_hoverDownImage(const pVal:WideString); safecall;
   function Get_tiled : WordBool; safecall;
   procedure Set_tiled(const pVal:WordBool); safecall;
   function Get_transparencyColor : WideString; safecall;
   procedure Set_transparencyColor(const pVal:WideString); safecall;
   function Get_down : WordBool; safecall;
   procedure Set_down(const pVal:WordBool); safecall;
   function Get_sticky : WordBool; safecall;
   procedure Set_sticky(const pVal:WordBool); safecall;
   function Get_upToolTip : WideString; safecall;
   procedure Set_upToolTip(const pVal:WideString); safecall;
   function Get_downToolTip : WideString; safecall;
   procedure Set_downToolTip(const pVal:WideString); safecall;
   function Get_cursor : WideString; safecall;
   procedure Set_cursor(const pVal:WideString); safecall;
    // image :  
   property image:WideString read Get_image write Set_image;
    // hoverImage :  
   property hoverImage:WideString read Get_hoverImage write Set_hoverImage;
    // downImage :  
   property downImage:WideString read Get_downImage write Set_downImage;
    // disabledImage :  
   property disabledImage:WideString read Get_disabledImage write Set_disabledImage;
    // hoverDownImage :  
   property hoverDownImage:WideString read Get_hoverDownImage write Set_hoverDownImage;
    // tiled :  
   property tiled:WordBool read Get_tiled write Set_tiled;
    // transparencyColor :  
   property transparencyColor:WideString read Get_transparencyColor write Set_transparencyColor;
    // down :  
   property down:WordBool read Get_down write Set_down;
    // sticky :  
   property sticky:WordBool read Get_sticky write Set_sticky;
    // upToolTip :  
   property upToolTip:WideString read Get_upToolTip write Set_upToolTip;
    // downToolTip :  
   property downToolTip:WideString read Get_downToolTip write Set_downToolTip;
    // cursor :  
   property cursor:WideString read Get_cursor write Set_cursor;
  end;


// IWMPButtonCtrl : IWMPButtonCtrl: Public interface for skin object model.

 IWMPButtonCtrlDisp = dispinterface
   ['{87291B50-0C8E-11D3-BB2A-00A0C93CA73A}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // image :  
   property image:WideString dispid 5102;
    // hoverImage :  
   property hoverImage:WideString dispid 5103;
    // downImage :  
   property downImage:WideString dispid 5104;
    // disabledImage :  
   property disabledImage:WideString dispid 5105;
    // hoverDownImage :  
   property hoverDownImage:WideString dispid 5106;
    // tiled :  
   property tiled:WordBool dispid 5107;
    // transparencyColor :  
   property transparencyColor:WideString dispid 5108;
    // down :  
   property down:WordBool dispid 5109;
    // sticky :  
   property sticky:WordBool dispid 5110;
    // upToolTip :  
   property upToolTip:WideString dispid 5112;
    // downToolTip :  
   property downToolTip:WideString dispid 5113;
    // cursor :  
   property cursor:WideString dispid 5114;
  end;


// IWMPListBoxCtrl : IWMPListBoxCtrl: Public interface for skin object model.

 IWMPListBoxCtrl = interface(IDispatch)
   ['{FC1880CE-83B9-43A7-A066-C44CE8C82583}']
   function Get_selectedItem : Integer; safecall;
   procedure Set_selectedItem(const pnPos:Integer); safecall;
   function Get_sorted : WordBool; safecall;
   procedure Set_sorted(const pVal:WordBool); safecall;
   function Get_multiselect : WordBool; safecall;
   procedure Set_multiselect(const pVal:WordBool); safecall;
   function Get_readOnly : WordBool; safecall;
   procedure Set_readOnly(const pVal:WordBool); safecall;
   function Get_foregroundColor : WideString; safecall;
   procedure Set_foregroundColor(const pVal:WideString); safecall;
   function Get_backgroundColor : WideString; safecall;
   procedure Set_backgroundColor(const pVal:WideString); safecall;
   function Get_fontSize : Integer; safecall;
   procedure Set_fontSize(const pVal:Integer); safecall;
   function Get_fontStyle : WideString; safecall;
   procedure Set_fontStyle(const pVal:WideString); safecall;
   function Get_fontFace : WideString; safecall;
   procedure Set_fontFace(const pVal:WideString); safecall;
   function Get_itemCount : Integer; safecall;
   function Get_firstVisibleItem : Integer; safecall;
   procedure Set_firstVisibleItem(const pVal:Integer); safecall;
   procedure Set_popUp(const Param1:WordBool); safecall;
   function Get_focusItem : Integer; safecall;
   procedure Set_focusItem(const pVal:Integer); safecall;
   function Get_border : WordBool; safecall;
   procedure Set_border(const pVal:WordBool); safecall;
    // getItem : method getItem 
   function getItem(nPos:Integer):WideString;safecall;
    // insertItem : method insertItem 
   procedure insertItem(nPos:Integer;newVal:WideString);safecall;
    // appendItem : method appendItem 
   procedure appendItem(newVal:WideString);safecall;
    // replaceItem : method replaceItem 
   procedure replaceItem(nPos:Integer;newVal:WideString);safecall;
    // deleteItem : method deleteItem 
   procedure deleteItem(nPos:Integer);safecall;
    // deleteAll : method deleteAll 
   procedure deleteAll;safecall;
    // findItem : method findItem 
   function findItem(nStartIndex:Integer;newVal:WideString):Integer;safecall;
    // getNextSelectedItem : method getNextSelectedItem 
   function getNextSelectedItem(nStartIndex:Integer):Integer;safecall;
    // setSelectedState : method setSelectedState 
   procedure setSelectedState(nPos:Integer;vbSelected:WordBool);safecall;
    // show : method show 
   procedure show;safecall;
    // dismiss : method dismiss 
   procedure dismiss;safecall;
    // selectedItem :  
   property selectedItem:Integer read Get_selectedItem write Set_selectedItem;
    // sorted :  
   property sorted:WordBool read Get_sorted write Set_sorted;
    // multiselect :  
   property multiselect:WordBool read Get_multiselect write Set_multiselect;
    // readOnly :  
   property readOnly:WordBool read Get_readOnly write Set_readOnly;
    // foregroundColor :  
   property foregroundColor:WideString read Get_foregroundColor write Set_foregroundColor;
    // backgroundColor :  
   property backgroundColor:WideString read Get_backgroundColor write Set_backgroundColor;
    // fontSize :  
   property fontSize:Integer read Get_fontSize write Set_fontSize;
    // fontStyle :  
   property fontStyle:WideString read Get_fontStyle write Set_fontStyle;
    // fontFace :  
   property fontFace:WideString read Get_fontFace write Set_fontFace;
    // itemCount :  
   property itemCount:Integer read Get_itemCount;
    // firstVisibleItem :  
   property firstVisibleItem:Integer read Get_firstVisibleItem write Set_firstVisibleItem;
    // popUp :  
   property popUp:WordBool write Set_popUp;
    // focusItem :  
   property focusItem:Integer read Get_focusItem write Set_focusItem;
    // border :  
   property border:WordBool read Get_border write Set_border;
  end;


// IWMPListBoxCtrl : IWMPListBoxCtrl: Public interface for skin object model.

 IWMPListBoxCtrlDisp = dispinterface
   ['{FC1880CE-83B9-43A7-A066-C44CE8C82583}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // getItem : method getItem 
   function getItem(nPos:Integer):WideString;dispid 6111;
    // insertItem : method insertItem 
   procedure insertItem(nPos:Integer;newVal:WideString);dispid 6112;
    // appendItem : method appendItem 
   procedure appendItem(newVal:WideString);dispid 6113;
    // replaceItem : method replaceItem 
   procedure replaceItem(nPos:Integer;newVal:WideString);dispid 6114;
    // deleteItem : method deleteItem 
   procedure deleteItem(nPos:Integer);dispid 6115;
    // deleteAll : method deleteAll 
   procedure deleteAll;dispid 6116;
    // findItem : method findItem 
   function findItem(nStartIndex:Integer;newVal:WideString):Integer;dispid 6117;
    // getNextSelectedItem : method getNextSelectedItem 
   function getNextSelectedItem(nStartIndex:Integer):Integer;dispid 6118;
    // setSelectedState : method setSelectedState 
   procedure setSelectedState(nPos:Integer;vbSelected:WordBool);dispid 6122;
    // show : method show 
   procedure show;dispid 6123;
    // dismiss : method dismiss 
   procedure dismiss;dispid 6124;
    // selectedItem :  
   property selectedItem:Integer dispid 6108;
    // sorted :  
   property sorted:WordBool dispid 6100;
    // multiselect :  
   property multiselect:WordBool dispid 6101;
    // readOnly :  
   property readOnly:WordBool dispid 6102;
    // foregroundColor :  
   property foregroundColor:WideString dispid 6103;
    // backgroundColor :  
   property backgroundColor:WideString dispid 6104;
    // fontSize :  
   property fontSize:Integer dispid 6105;
    // fontStyle :  
   property fontStyle:WideString dispid 6106;
    // fontFace :  
   property fontFace:WideString dispid 6107;
    // itemCount :  
   property itemCount:Integer  readonly dispid 6109;
    // firstVisibleItem :  
   property firstVisibleItem:Integer dispid 6110;
    // popUp :  
   property popUp:WordBool writeonly dispid 6120;
    // focusItem :  
   property focusItem:Integer dispid 6121;
    // border :  
   property border:WordBool dispid 6125;
  end;


// IWMPListBoxItem : IWMPListBoxItem: Public interface for skin object model.

 IWMPListBoxItem = interface(IDispatch)
   ['{D255DFB8-C22A-42CF-B8B7-F15D7BCF65D6}']
   procedure Set_value(const Param1:WideString); safecall;
    // value : property value 
   property value:WideString write Set_value;
  end;


// IWMPListBoxItem : IWMPListBoxItem: Public interface for skin object model.

 IWMPListBoxItemDisp = dispinterface
   ['{D255DFB8-C22A-42CF-B8B7-F15D7BCF65D6}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // value : property value 
   property value:WideString writeonly dispid 6119;
  end;


// IWMPPlaylistCtrlColumn : IWMPPlaylistCtrlColumn: Public interface for skin object model.

 IWMPPlaylistCtrlColumn = interface(IDispatch)
   ['{63D9D30F-AE4C-4678-8CA8-5720F4FE4419}']
   function Get_columnName : WideString; safecall;
   procedure Set_columnName(const pVal:WideString); safecall;
   function Get_columnID : WideString; safecall;
   procedure Set_columnID(const pVal:WideString); safecall;
   function Get_columnResizeMode : WideString; safecall;
   procedure Set_columnResizeMode(const pVal:WideString); safecall;
   function Get_columnWidth : Integer; safecall;
   procedure Set_columnWidth(const pVal:Integer); safecall;
    // columnName : property columnName 
   property columnName:WideString read Get_columnName write Set_columnName;
    // columnID : property columnID 
   property columnID:WideString read Get_columnID write Set_columnID;
    // columnResizeMode : property columnResizeMode 
   property columnResizeMode:WideString read Get_columnResizeMode write Set_columnResizeMode;
    // columnWidth : property columnWidth 
   property columnWidth:Integer read Get_columnWidth write Set_columnWidth;
  end;


// IWMPPlaylistCtrlColumn : IWMPPlaylistCtrlColumn: Public interface for skin object model.

 IWMPPlaylistCtrlColumnDisp = dispinterface
   ['{63D9D30F-AE4C-4678-8CA8-5720F4FE4419}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // columnName : property columnName 
   property columnName:WideString dispid 5670;
    // columnID : property columnID 
   property columnID:WideString dispid 5671;
    // columnResizeMode : property columnResizeMode 
   property columnResizeMode:WideString dispid 5672;
    // columnWidth : property columnWidth 
   property columnWidth:Integer dispid 5673;
  end;


// IWMPSliderCtrlEvents : IWMPSliderCtrlEvents: Public interface for skin object model.

 IWMPSliderCtrlEvents = dispinterface
   ['{CDAC14D2-8BE4-11D3-BB48-00A0C93CA73A}']
    // ondragbegin : event ondragbegin 
   function ondragbegin:HResult;dispid 5430;
    // ondragend : event ondragend 
   function ondragend:HResult;dispid 5431;
    // onpositionchange : event onpositionchange 
   function onpositionchange:HResult;dispid 5432;
  end;


// IWMPSliderCtrl : IWMPSliderCtrl: Public interface for skin object model.

 IWMPSliderCtrl = interface(IDispatch)
   ['{F2BF2C8F-405F-11D3-BB39-00A0C93CA73A}']
   function Get_direction : WideString; safecall;
   procedure Set_direction(const pVal:WideString); safecall;
   function Get_slide : WordBool; safecall;
   procedure Set_slide(const pVal:WordBool); safecall;
   function Get_tiled : WordBool; safecall;
   procedure Set_tiled(const pVal:WordBool); safecall;
   function Get_foregroundColor : WideString; safecall;
   procedure Set_foregroundColor(const pVal:WideString); safecall;
   function Get_foregroundEndColor : WideString; safecall;
   procedure Set_foregroundEndColor(const pVal:WideString); safecall;
   function Get_backgroundColor : WideString; safecall;
   procedure Set_backgroundColor(const pVal:WideString); safecall;
   function Get_backgroundEndColor : WideString; safecall;
   procedure Set_backgroundEndColor(const pVal:WideString); safecall;
   function Get_disabledColor : WideString; safecall;
   procedure Set_disabledColor(const pVal:WideString); safecall;
   function Get_transparencyColor : WideString; safecall;
   procedure Set_transparencyColor(const pVal:WideString); safecall;
   function Get_foregroundImage : WideString; safecall;
   procedure Set_foregroundImage(const pVal:WideString); safecall;
   function Get_backgroundImage : WideString; safecall;
   procedure Set_backgroundImage(const pVal:WideString); safecall;
   function Get_backgroundHoverImage : WideString; safecall;
   procedure Set_backgroundHoverImage(const pVal:WideString); safecall;
   function Get_disabledImage : WideString; safecall;
   procedure Set_disabledImage(const pVal:WideString); safecall;
   function Get_thumbImage : WideString; safecall;
   procedure Set_thumbImage(const pVal:WideString); safecall;
   function Get_thumbHoverImage : WideString; safecall;
   procedure Set_thumbHoverImage(const pVal:WideString); safecall;
   function Get_thumbDownImage : WideString; safecall;
   procedure Set_thumbDownImage(const pVal:WideString); safecall;
   function Get_thumbDisabledImage : WideString; safecall;
   procedure Set_thumbDisabledImage(const pVal:WideString); safecall;
   function Get_min : Single; safecall;
   procedure Set_min(const pVal:Single); safecall;
   function Get_max : Single; safecall;
   procedure Set_max(const pVal:Single); safecall;
   function Get_value : Single; safecall;
   procedure Set_value(const pVal:Single); safecall;
   function Get_toolTip : WideString; safecall;
   procedure Set_toolTip(const pVal:WideString); safecall;
   function Get_cursor : WideString; safecall;
   procedure Set_cursor(const pVal:WideString); safecall;
   function Get_borderSize : SYSINT; safecall;
   procedure Set_borderSize(const pVal:SYSINT); safecall;
   function Get_foregroundHoverImage : WideString; safecall;
   procedure Set_foregroundHoverImage(const pVal:WideString); safecall;
   function Get_foregroundProgress : Single; safecall;
   procedure Set_foregroundProgress(const pVal:Single); safecall;
   function Get_useForegroundProgress : WordBool; safecall;
   procedure Set_useForegroundProgress(const pVal:WordBool); safecall;
    // direction : property direction 
   property direction:WideString read Get_direction write Set_direction;
    // slide : property slide 
   property slide:WordBool read Get_slide write Set_slide;
    // tiled : property tiled 
   property tiled:WordBool read Get_tiled write Set_tiled;
    // foregroundColor : property foregroundColor 
   property foregroundColor:WideString read Get_foregroundColor write Set_foregroundColor;
    // foregroundEndColor : property foregroundEndColor 
   property foregroundEndColor:WideString read Get_foregroundEndColor write Set_foregroundEndColor;
    // backgroundColor : property backgroundColor 
   property backgroundColor:WideString read Get_backgroundColor write Set_backgroundColor;
    // backgroundEndColor : property backgroundEndColor 
   property backgroundEndColor:WideString read Get_backgroundEndColor write Set_backgroundEndColor;
    // disabledColor : property disabledColor 
   property disabledColor:WideString read Get_disabledColor write Set_disabledColor;
    // transparencyColor : property transparencyColor 
   property transparencyColor:WideString read Get_transparencyColor write Set_transparencyColor;
    // foregroundImage : property foregroundImage 
   property foregroundImage:WideString read Get_foregroundImage write Set_foregroundImage;
    // backgroundImage : property backgroundImage 
   property backgroundImage:WideString read Get_backgroundImage write Set_backgroundImage;
    // backgroundHoverImage : property backgroundHoverImage 
   property backgroundHoverImage:WideString read Get_backgroundHoverImage write Set_backgroundHoverImage;
    // disabledImage : property disabledImage 
   property disabledImage:WideString read Get_disabledImage write Set_disabledImage;
    // thumbImage : property thumbImage 
   property thumbImage:WideString read Get_thumbImage write Set_thumbImage;
    // thumbHoverImage : property thumbHoverImage 
   property thumbHoverImage:WideString read Get_thumbHoverImage write Set_thumbHoverImage;
    // thumbDownImage : property thumbDownImage 
   property thumbDownImage:WideString read Get_thumbDownImage write Set_thumbDownImage;
    // thumbDisabledImage : property thumbDisabledImage 
   property thumbDisabledImage:WideString read Get_thumbDisabledImage write Set_thumbDisabledImage;
    // min : property min 
   property min:Single read Get_min write Set_min;
    // max : property max 
   property max:Single read Get_max write Set_max;
    // value : property value 
   property value:Single read Get_value write Set_value;
    // toolTip : property toolTip 
   property toolTip:WideString read Get_toolTip write Set_toolTip;
    // cursor : property cursor 
   property cursor:WideString read Get_cursor write Set_cursor;
    // borderSize : property borderSize 
   property borderSize:SYSINT read Get_borderSize write Set_borderSize;
    // foregroundHoverImage : property foregroundHoverImage 
   property foregroundHoverImage:WideString read Get_foregroundHoverImage write Set_foregroundHoverImage;
    // foregroundProgress : property foregroundValue 
   property foregroundProgress:Single read Get_foregroundProgress write Set_foregroundProgress;
    // useForegroundProgress : property useForegroundValue 
   property useForegroundProgress:WordBool read Get_useForegroundProgress write Set_useForegroundProgress;
  end;


// IWMPSliderCtrl : IWMPSliderCtrl: Public interface for skin object model.

 IWMPSliderCtrlDisp = dispinterface
   ['{F2BF2C8F-405F-11D3-BB39-00A0C93CA73A}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // direction : property direction 
   property direction:WideString dispid 5400;
    // slide : property slide 
   property slide:WordBool dispid 5402;
    // tiled : property tiled 
   property tiled:WordBool dispid 5403;
    // foregroundColor : property foregroundColor 
   property foregroundColor:WideString dispid 5404;
    // foregroundEndColor : property foregroundEndColor 
   property foregroundEndColor:WideString dispid 5405;
    // backgroundColor : property backgroundColor 
   property backgroundColor:WideString dispid 5406;
    // backgroundEndColor : property backgroundEndColor 
   property backgroundEndColor:WideString dispid 5407;
    // disabledColor : property disabledColor 
   property disabledColor:WideString dispid 5408;
    // transparencyColor : property transparencyColor 
   property transparencyColor:WideString dispid 5409;
    // foregroundImage : property foregroundImage 
   property foregroundImage:WideString dispid 5410;
    // backgroundImage : property backgroundImage 
   property backgroundImage:WideString dispid 5411;
    // backgroundHoverImage : property backgroundHoverImage 
   property backgroundHoverImage:WideString dispid 5412;
    // disabledImage : property disabledImage 
   property disabledImage:WideString dispid 5413;
    // thumbImage : property thumbImage 
   property thumbImage:WideString dispid 5414;
    // thumbHoverImage : property thumbHoverImage 
   property thumbHoverImage:WideString dispid 5415;
    // thumbDownImage : property thumbDownImage 
   property thumbDownImage:WideString dispid 5416;
    // thumbDisabledImage : property thumbDisabledImage 
   property thumbDisabledImage:WideString dispid 5417;
    // min : property min 
   property min:Single dispid 5418;
    // max : property max 
   property max:Single dispid 5419;
    // value : property value 
   property value:Single dispid 5420;
    // toolTip : property toolTip 
   property toolTip:WideString dispid 5421;
    // cursor : property cursor 
   property cursor:WideString dispid 5422;
    // borderSize : property borderSize 
   property borderSize:SYSINT dispid 5423;
    // foregroundHoverImage : property foregroundHoverImage 
   property foregroundHoverImage:WideString dispid 5424;
    // foregroundProgress : property foregroundValue 
   property foregroundProgress:Single dispid 5425;
    // useForegroundProgress : property useForegroundValue 
   property useForegroundProgress:WordBool dispid 5426;
  end;


// IWMPVideoCtrlEvents : IWMPVideoCtrlEvents: Public interface for skin object model.

 IWMPVideoCtrlEvents = dispinterface
   ['{A85C0477-714C-4A06-B9F6-7C8CA38B45DC}']
    // onvideostart : event onvideostart 
   function onvideostart:HResult;dispid 5720;
    // onvideoend : event onvideostart 
   function onvideoend:HResult;dispid 5721;
  end;


// IWMPVideoCtrl : IWMPVideoCtrl: Public interface for skin object model.

 IWMPVideoCtrl = interface(IDispatch)
   ['{61CECF10-FC3A-11D2-A1CD-005004602752}']
   procedure Set_windowless(const pbClipped:WordBool); safecall;
   function Get_windowless : WordBool; safecall;
   procedure Set_cursor(const pbstrCursor:WideString); safecall;
   function Get_cursor : WideString; safecall;
   procedure Set_backgroundColor(const pbstrColor:WideString); safecall;
   function Get_backgroundColor : WideString; safecall;
   procedure Set_maintainAspectRatio(const pbMaintainAspectRatio:WordBool); safecall;
   function Get_maintainAspectRatio : WordBool; safecall;
   procedure Set_toolTip(const bstrToolTip:WideString); safecall;
   function Get_toolTip : WideString; safecall;
   function Get_fullScreen : WordBool; safecall;
   procedure Set_fullScreen(const pbFullScreen:WordBool); safecall;
   procedure Set_shrinkToFit(const pbShrinkToFit:WordBool); safecall;
   function Get_shrinkToFit : WordBool; safecall;
   procedure Set_stretchToFit(const pbStretchToFit:WordBool); safecall;
   function Get_stretchToFit : WordBool; safecall;
   procedure Set_zoom(const pzoom:Integer); safecall;
   function Get_zoom : Integer; safecall;
    // windowless :  
   property windowless:WordBool read Get_windowless write Set_windowless;
    // cursor :  
   property cursor:WideString read Get_cursor write Set_cursor;
    // backgroundColor :  
   property backgroundColor:WideString read Get_backgroundColor write Set_backgroundColor;
    // maintainAspectRatio :  
   property maintainAspectRatio:WordBool read Get_maintainAspectRatio write Set_maintainAspectRatio;
    // toolTip :  
   property toolTip:WideString read Get_toolTip write Set_toolTip;
    // fullScreen :  
   property fullScreen:WordBool read Get_fullScreen write Set_fullScreen;
    // shrinkToFit :  
   property shrinkToFit:WordBool read Get_shrinkToFit write Set_shrinkToFit;
    // stretchToFit :  
   property stretchToFit:WordBool read Get_stretchToFit write Set_stretchToFit;
    // zoom :  
   property zoom:Integer read Get_zoom write Set_zoom;
  end;


// IWMPVideoCtrl : IWMPVideoCtrl: Public interface for skin object model.

 IWMPVideoCtrlDisp = dispinterface
   ['{61CECF10-FC3A-11D2-A1CD-005004602752}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // windowless :  
   property windowless:WordBool dispid 5700;
    // cursor :  
   property cursor:WideString dispid 5701;
    // backgroundColor :  
   property backgroundColor:WideString dispid 5702;
    // maintainAspectRatio :  
   property maintainAspectRatio:WordBool dispid 5704;
    // toolTip :  
   property toolTip:WideString dispid 5706;
    // fullScreen :  
   property fullScreen:WordBool dispid 5707;
    // shrinkToFit :  
   property shrinkToFit:WordBool dispid 5703;
    // stretchToFit :  
   property stretchToFit:WordBool dispid 5708;
    // zoom :  
   property zoom:Integer dispid 5709;
  end;


// IWMPEffectsCtrl : IWMPEffectsCtrl: Public interface for skin object model.

 IWMPEffectsCtrl = interface(IDispatch)
   ['{A9EFAB80-0A60-4C3F-BBD1-4558DD2A9769}']
   function Get_windowed : WordBool; safecall;
   procedure Set_windowed(const pVal:WordBool); safecall;
   function Get_allowAll : WordBool; safecall;
   procedure Set_allowAll(const pVal:WordBool); safecall;
   procedure Set_currentEffectType(const pVal:WideString); safecall;
   function Get_currentEffectType : WideString; safecall;
   function Get_currentEffectTitle : WideString; safecall;
    // next : method next 
   procedure next;safecall;
    // previous : method previous 
   procedure previous;safecall;
    // settings : method settings 
   procedure settings;safecall;
   function Get_currentEffect : IDispatch; safecall;
   procedure Set_currentEffect(const p:IDispatch); safecall;
    // nextEffect : method nextEffect 
   procedure nextEffect;safecall;
    // previousEffect : method previousEffect 
   procedure previousEffect;safecall;
    // nextPreset : method nextPreset 
   procedure nextPreset;safecall;
    // previousPreset : method previousPreset 
   procedure previousPreset;safecall;
   function Get_currentPreset : Integer; safecall;
   procedure Set_currentPreset(const pVal:Integer); safecall;
   function Get_currentPresetTitle : WideString; safecall;
   function Get_currentEffectPresetCount : Integer; safecall;
   function Get_fullScreen : WordBool; safecall;
   procedure Set_fullScreen(const pbFullScreen:WordBool); safecall;
   function Get_effectCanGoFullScreen : WordBool; safecall;
   function Get_effectHasPropertyPage : WordBool; safecall;
   function Get_effectCount : Integer; safecall;
   function Get_effectTitle(index:Integer) : WideString; safecall;
   function Get_effectType(index:Integer) : WideString; safecall;
    // windowed : property windowed 
   property windowed:WordBool read Get_windowed write Set_windowed;
    // allowAll : property allowAll 
   property allowAll:WordBool read Get_allowAll write Set_allowAll;
    // currentEffectType : property currentEffectType 
   property currentEffectType:WideString read Get_currentEffectType write Set_currentEffectType;
    // currentEffectTitle : property currentEffectTitle 
   property currentEffectTitle:WideString read Get_currentEffectTitle;
    // currentEffect : property currentEffect 
   property currentEffect:IDispatch read Get_currentEffect write Set_currentEffect;
    // currentPreset : property currentPreset 
   property currentPreset:Integer read Get_currentPreset write Set_currentPreset;
    // currentPresetTitle : property currentPresetTitle 
   property currentPresetTitle:WideString read Get_currentPresetTitle;
    // currentEffectPresetCount : property currentEffectPresetCount 
   property currentEffectPresetCount:Integer read Get_currentEffectPresetCount;
    // fullScreen : property fullScreen 
   property fullScreen:WordBool read Get_fullScreen write Set_fullScreen;
    // effectCanGoFullScreen : property canGoFullScreen 
   property effectCanGoFullScreen:WordBool read Get_effectCanGoFullScreen;
    // effectHasPropertyPage : property canGoFullScreen 
   property effectHasPropertyPage:WordBool read Get_effectHasPropertyPage;
    // effectCount : property effectCount 
   property effectCount:Integer read Get_effectCount;
    // effectTitle : property effectTitle(index) 
   property effectTitle[index:Integer]:WideString read Get_effectTitle;
    // effectType : property effectType(index) 
   property effectType[index:Integer]:WideString read Get_effectType;
  end;


// IWMPEffectsCtrl : IWMPEffectsCtrl: Public interface for skin object model.

 IWMPEffectsCtrlDisp = dispinterface
   ['{A9EFAB80-0A60-4C3F-BBD1-4558DD2A9769}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // next : method next 
   procedure next;dispid 5502;
    // previous : method previous 
   procedure previous;dispid 5503;
    // settings : method settings 
   procedure settings;dispid 5504;
    // nextEffect : method nextEffect 
   procedure nextEffect;dispid 5509;
    // previousEffect : method previousEffect 
   procedure previousEffect;dispid 5510;
    // nextPreset : method nextPreset 
   procedure nextPreset;dispid 5511;
    // previousPreset : method previousPreset 
   procedure previousPreset;dispid 5512;
    // windowed : property windowed 
   property windowed:WordBool dispid 5500;
    // allowAll : property allowAll 
   property allowAll:WordBool dispid 5501;
    // currentEffectType : property currentEffectType 
   property currentEffectType:WideString dispid 5507;
    // currentEffectTitle : property currentEffectTitle 
   property currentEffectTitle:WideString  readonly dispid 5506;
    // currentEffect : property currentEffect 
   property currentEffect:IDispatch dispid 5505;
    // currentPreset : property currentPreset 
   property currentPreset:Integer dispid 5513;
    // currentPresetTitle : property currentPresetTitle 
   property currentPresetTitle:WideString  readonly dispid 5514;
    // currentEffectPresetCount : property currentEffectPresetCount 
   property currentEffectPresetCount:Integer  readonly dispid 5515;
    // fullScreen : property fullScreen 
   property fullScreen:WordBool dispid 5516;
    // effectCanGoFullScreen : property canGoFullScreen 
   property effectCanGoFullScreen:WordBool  readonly dispid 5517;
    // effectHasPropertyPage : property canGoFullScreen 
   property effectHasPropertyPage:WordBool  readonly dispid 5518;
    // effectCount : property effectCount 
   property effectCount:Integer  readonly dispid 5520;
    // effectTitle : property effectTitle(index) 
   property effectTitle[index:Integer]:WideString  readonly dispid 5521;
    // effectType : property effectType(index) 
   property effectType[index:Integer]:WideString  readonly dispid 5522;
  end;


// IWMPEqualizerSettingsCtrl : IWMPEqualizerSettingsCtrl: Public interface for skin object model.

 IWMPEqualizerSettingsCtrl = interface(IDispatch)
   ['{2BD3716F-A914-49FB-8655-996D5F495498}']
   function Get_bypass : WordBool; safecall;
   procedure Set_bypass(const pVal:WordBool); safecall;
   function Get_gainLevel1 : Single; safecall;
   procedure Set_gainLevel1(const pflLevel:Single); safecall;
   function Get_gainLevel2 : Single; safecall;
   procedure Set_gainLevel2(const pflLevel:Single); safecall;
   function Get_gainLevel3 : Single; safecall;
   procedure Set_gainLevel3(const pflLevel:Single); safecall;
   function Get_gainLevel4 : Single; safecall;
   procedure Set_gainLevel4(const pflLevel:Single); safecall;
   function Get_gainLevel5 : Single; safecall;
   procedure Set_gainLevel5(const pflLevel:Single); safecall;
   function Get_gainLevel6 : Single; safecall;
   procedure Set_gainLevel6(const pflLevel:Single); safecall;
   function Get_gainLevel7 : Single; safecall;
   procedure Set_gainLevel7(const pflLevel:Single); safecall;
   function Get_gainLevel8 : Single; safecall;
   procedure Set_gainLevel8(const pflLevel:Single); safecall;
   function Get_gainLevel9 : Single; safecall;
   procedure Set_gainLevel9(const pflLevel:Single); safecall;
   function Get_gainLevel10 : Single; safecall;
   procedure Set_gainLevel10(const pflLevel:Single); safecall;
   function Get_gainLevels(iIndex:Integer) : Single; safecall;
   procedure Set_gainLevels(const iIndex:Integer; const pargainLevels:Single); safecall;
    // reset_ : method reset 
   procedure reset_;safecall;
   function Get_bands : Integer; safecall;
    // nextPreset : method nextPreset 
   procedure nextPreset;safecall;
    // previousPreset : method previousPreset 
   procedure previousPreset;safecall;
   function Get_currentPreset : Integer; safecall;
   procedure Set_currentPreset(const pVal:Integer); safecall;
   function Get_currentPresetTitle : WideString; safecall;
   function Get_presetCount : Integer; safecall;
   function Get_enhancedAudio : WordBool; safecall;
   procedure Set_enhancedAudio(const pfVal:WordBool); safecall;
   function Get_speakerSize : Integer; safecall;
   procedure Set_speakerSize(const plVal:Integer); safecall;
   function Get_currentSpeakerName : WideString; safecall;
   function Get_truBassLevel : Integer; safecall;
   procedure Set_truBassLevel(const plTruBassLevel:Integer); safecall;
   function Get_wowLevel : Integer; safecall;
   procedure Set_wowLevel(const plWowLevel:Integer); safecall;
   function Get_splineTension : Single; safecall;
   procedure Set_splineTension(const pflSplineTension:Single); safecall;
   function Get_enableSplineTension : WordBool; safecall;
   procedure Set_enableSplineTension(const pfEnableSplineTension:WordBool); safecall;
   function Get_presetTitle(iIndex:Integer) : WideString; safecall;
   function Get_normalization : WordBool; safecall;
   procedure Set_normalization(const pfVal:WordBool); safecall;
   function Get_normalizationAverage : Single; safecall;
   function Get_normalizationPeak : Single; safecall;
   function Get_crossFade : WordBool; safecall;
   procedure Set_crossFade(const pfVal:WordBool); safecall;
   function Get_crossFadeWindow : Integer; safecall;
   procedure Set_crossFadeWindow(const plWindow:Integer); safecall;
    // bypass : property bypass 
   property bypass:WordBool read Get_bypass write Set_bypass;
    // gainLevel1 : property gainLevel1 
   property gainLevel1:Single read Get_gainLevel1 write Set_gainLevel1;
    // gainLevel2 : property gainLevel2 
   property gainLevel2:Single read Get_gainLevel2 write Set_gainLevel2;
    // gainLevel3 : property gainLevel3 
   property gainLevel3:Single read Get_gainLevel3 write Set_gainLevel3;
    // gainLevel4 : property gainLevel4 
   property gainLevel4:Single read Get_gainLevel4 write Set_gainLevel4;
    // gainLevel5 : property gainLevel5 
   property gainLevel5:Single read Get_gainLevel5 write Set_gainLevel5;
    // gainLevel6 : property gainLevel6 
   property gainLevel6:Single read Get_gainLevel6 write Set_gainLevel6;
    // gainLevel7 : property gainLevel7 
   property gainLevel7:Single read Get_gainLevel7 write Set_gainLevel7;
    // gainLevel8 : property gainLevel8 
   property gainLevel8:Single read Get_gainLevel8 write Set_gainLevel8;
    // gainLevel9 : property gainLevel9 
   property gainLevel9:Single read Get_gainLevel9 write Set_gainLevel9;
    // gainLevel10 : property gainLevel10 
   property gainLevel10:Single read Get_gainLevel10 write Set_gainLevel10;
    // gainLevels : property gainLevels 
   property gainLevels[iIndex:Integer]:Single read Get_gainLevels write Set_gainLevels;
    // bands :  
   property bands:Integer read Get_bands;
    // currentPreset : property currentPreset 
   property currentPreset:Integer read Get_currentPreset write Set_currentPreset;
    // currentPresetTitle : property currentPresetTitle 
   property currentPresetTitle:WideString read Get_currentPresetTitle;
    // presetCount : property presetCount 
   property presetCount:Integer read Get_presetCount;
    // enhancedAudio : property enhancedAudio 
   property enhancedAudio:WordBool read Get_enhancedAudio write Set_enhancedAudio;
    // speakerSize : property speakerSize 
   property speakerSize:Integer read Get_speakerSize write Set_speakerSize;
    // currentSpeakerName : property currentSpeakerName 
   property currentSpeakerName:WideString read Get_currentSpeakerName;
    // truBassLevel : property truBassLevel 
   property truBassLevel:Integer read Get_truBassLevel write Set_truBassLevel;
    // wowLevel : property wowLevel 
   property wowLevel:Integer read Get_wowLevel write Set_wowLevel;
    // splineTension : property splineTension 
   property splineTension:Single read Get_splineTension write Set_splineTension;
    // enableSplineTension : property enableSplineTension 
   property enableSplineTension:WordBool read Get_enableSplineTension write Set_enableSplineTension;
    // presetTitle : property presetTitle 
   property presetTitle[iIndex:Integer]:WideString read Get_presetTitle;
    // normalization : property normalization 
   property normalization:WordBool read Get_normalization write Set_normalization;
    // normalizationAverage : property normalizationAverage 
   property normalizationAverage:Single read Get_normalizationAverage;
    // normalizationPeak : property normalizationPeak 
   property normalizationPeak:Single read Get_normalizationPeak;
    // crossFade : property crossFade 
   property crossFade:WordBool read Get_crossFade write Set_crossFade;
    // crossFadeWindow : property crossFadeWindow 
   property crossFadeWindow:Integer read Get_crossFadeWindow write Set_crossFadeWindow;
  end;


// IWMPEqualizerSettingsCtrl : IWMPEqualizerSettingsCtrl: Public interface for skin object model.

 IWMPEqualizerSettingsCtrlDisp = dispinterface
   ['{2BD3716F-A914-49FB-8655-996D5F495498}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // reset_ : method reset 
   procedure reset_;dispid 5814;
    // nextPreset : method nextPreset 
   procedure nextPreset;dispid 5816;
    // previousPreset : method previousPreset 
   procedure previousPreset;dispid 5817;
    // bypass : property bypass 
   property bypass:WordBool dispid 5800;
    // gainLevel1 : property gainLevel1 
   property gainLevel1:Single dispid 5804;
    // gainLevel2 : property gainLevel2 
   property gainLevel2:Single dispid 5805;
    // gainLevel3 : property gainLevel3 
   property gainLevel3:Single dispid 5806;
    // gainLevel4 : property gainLevel4 
   property gainLevel4:Single dispid 5807;
    // gainLevel5 : property gainLevel5 
   property gainLevel5:Single dispid 5808;
    // gainLevel6 : property gainLevel6 
   property gainLevel6:Single dispid 5809;
    // gainLevel7 : property gainLevel7 
   property gainLevel7:Single dispid 5810;
    // gainLevel8 : property gainLevel8 
   property gainLevel8:Single dispid 5811;
    // gainLevel9 : property gainLevel9 
   property gainLevel9:Single dispid 5812;
    // gainLevel10 : property gainLevel10 
   property gainLevel10:Single dispid 5813;
    // gainLevels : property gainLevels 
   property gainLevels[iIndex:Integer]:Single dispid 5815;
    // bands :  
   property bands:Integer  readonly dispid 5801;
    // currentPreset : property currentPreset 
   property currentPreset:Integer dispid 5818;
    // currentPresetTitle : property currentPresetTitle 
   property currentPresetTitle:WideString  readonly dispid 5819;
    // presetCount : property presetCount 
   property presetCount:Integer  readonly dispid 5820;
    // enhancedAudio : property enhancedAudio 
   property enhancedAudio:WordBool dispid 5821;
    // speakerSize : property speakerSize 
   property speakerSize:Integer dispid 5822;
    // currentSpeakerName : property currentSpeakerName 
   property currentSpeakerName:WideString  readonly dispid 5823;
    // truBassLevel : property truBassLevel 
   property truBassLevel:Integer dispid 5824;
    // wowLevel : property wowLevel 
   property wowLevel:Integer dispid 5825;
    // splineTension : property splineTension 
   property splineTension:Single dispid 5827;
    // enableSplineTension : property enableSplineTension 
   property enableSplineTension:WordBool dispid 5826;
    // presetTitle : property presetTitle 
   property presetTitle[iIndex:Integer]:WideString  readonly dispid 5828;
    // normalization : property normalization 
   property normalization:WordBool dispid 5829;
    // normalizationAverage : property normalizationAverage 
   property normalizationAverage:Single  readonly dispid 5830;
    // normalizationPeak : property normalizationPeak 
   property normalizationPeak:Single  readonly dispid 5831;
    // crossFade : property crossFade 
   property crossFade:WordBool dispid 5832;
    // crossFadeWindow : property crossFadeWindow 
   property crossFadeWindow:Integer dispid 5833;
  end;


// IWMPVideoSettingsCtrl : IWMPVideoSettingsCtrl: Public interface for skin object model.

 IWMPVideoSettingsCtrl = interface(IDispatch)
   ['{07EC23DA-EF73-4BDE-A40F-F269E0B7AFD6}']
   function Get_brightness : Integer; safecall;
   procedure Set_brightness(const pVal:Integer); safecall;
   function Get_contrast : Integer; safecall;
   procedure Set_contrast(const pVal:Integer); safecall;
   function Get_hue : Integer; safecall;
   procedure Set_hue(const pVal:Integer); safecall;
   function Get_saturation : Integer; safecall;
   procedure Set_saturation(const pVal:Integer); safecall;
    // reset_ : method reset 
   procedure reset_;safecall;
    // brightness : property brightness 
   property brightness:Integer read Get_brightness write Set_brightness;
    // contrast : property contrast 
   property contrast:Integer read Get_contrast write Set_contrast;
    // hue : property hue 
   property hue:Integer read Get_hue write Set_hue;
    // saturation : property saturation 
   property saturation:Integer read Get_saturation write Set_saturation;
  end;


// IWMPVideoSettingsCtrl : IWMPVideoSettingsCtrl: Public interface for skin object model.

 IWMPVideoSettingsCtrlDisp = dispinterface
   ['{07EC23DA-EF73-4BDE-A40F-F269E0B7AFD6}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // reset_ : method reset 
   procedure reset_;dispid 5904;
    // brightness : property brightness 
   property brightness:Integer dispid 5900;
    // contrast : property contrast 
   property contrast:Integer dispid 5901;
    // hue : property hue 
   property hue:Integer dispid 5902;
    // saturation : property saturation 
   property saturation:Integer dispid 5903;
  end;


// IWMPLibraryTreeCtrl : IWMPLibraryTreeCtrl: Not Public.  Internal interface used by Windows Media Player.

 IWMPLibraryTreeCtrl = interface(IDispatch)
   ['{B738FCAE-F089-45DF-AED6-034B9E7DB632}']
   function Get_dropDownVisible : WordBool; safecall;
   procedure Set_dropDownVisible(const pVal:WordBool); safecall;
   function Get_foregroundColor : WideString; safecall;
   procedure Set_foregroundColor(const pVal:WideString); safecall;
   function Get_backgroundColor : WideString; safecall;
   procedure Set_backgroundColor(const pVal:WideString); safecall;
   function Get_fontSize : Integer; safecall;
   procedure Set_fontSize(const pVal:Integer); safecall;
   function Get_fontStyle : WideString; safecall;
   procedure Set_fontStyle(const pVal:WideString); safecall;
   function Get_fontFace : WideString; safecall;
   procedure Set_fontFace(const pVal:WideString); safecall;
   function Get_filter : WideString; safecall;
   procedure Set_filter(const pVal:WideString); safecall;
   function Get_expandState : WideString; safecall;
   procedure Set_expandState(const pVal:WideString); safecall;
   function Get_Playlist : IWMPPlaylist; safecall;
   procedure Set_Playlist(const ppPlaylist:IWMPPlaylist); safecall;
   function Get_selectedPlaylist : IWMPPlaylist; safecall;
   function Get_selectedMedia : IWMPMedia; safecall;
    // dropDownVisible : property dropDownVisible 
   property dropDownVisible:WordBool read Get_dropDownVisible write Set_dropDownVisible;
    // foregroundColor : property foregroundColor 
   property foregroundColor:WideString read Get_foregroundColor write Set_foregroundColor;
    // backgroundColor : property backgroundColor 
   property backgroundColor:WideString read Get_backgroundColor write Set_backgroundColor;
    // fontSize : property fontSize 
   property fontSize:Integer read Get_fontSize write Set_fontSize;
    // fontStyle : property fontStyle 
   property fontStyle:WideString read Get_fontStyle write Set_fontStyle;
    // fontFace : property fontFace 
   property fontFace:WideString read Get_fontFace write Set_fontFace;
    // filter : property filter 
   property filter:WideString read Get_filter write Set_filter;
    // expandState : property expandState 
   property expandState:WideString read Get_expandState write Set_expandState;
    // Playlist : property playlist 
   property Playlist:IWMPPlaylist read Get_Playlist write Set_Playlist;
    // selectedPlaylist : property selectedPlaylist 
   property selectedPlaylist:IWMPPlaylist read Get_selectedPlaylist;
    // selectedMedia : property selectedMedia 
   property selectedMedia:IWMPMedia read Get_selectedMedia;
  end;


// IWMPLibraryTreeCtrl : IWMPLibraryTreeCtrl: Not Public.  Internal interface used by Windows Media Player.

 IWMPLibraryTreeCtrlDisp = dispinterface
   ['{B738FCAE-F089-45DF-AED6-034B9E7DB632}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // dropDownVisible : property dropDownVisible 
   property dropDownVisible:WordBool dispid 6401;
    // foregroundColor : property foregroundColor 
   property foregroundColor:WideString dispid 6402;
    // backgroundColor : property backgroundColor 
   property backgroundColor:WideString dispid 6403;
    // fontSize : property fontSize 
   property fontSize:Integer dispid 6404;
    // fontStyle : property fontStyle 
   property fontStyle:WideString dispid 6405;
    // fontFace : property fontFace 
   property fontFace:WideString dispid 6406;
    // filter : property filter 
   property filter:WideString dispid 6407;
    // expandState : property expandState 
   property expandState:WideString dispid 6408;
    // Playlist : property playlist 
   property Playlist:IWMPPlaylist dispid 6409;
    // selectedPlaylist : property selectedPlaylist 
   property selectedPlaylist:IWMPPlaylist  readonly dispid 6410;
    // selectedMedia : property selectedMedia 
   property selectedMedia:IWMPMedia  readonly dispid 6411;
  end;


// IWMPEditCtrl : IWMPEditCtrl: Public interface for skin object model.

 IWMPEditCtrl = interface(IDispatch)
   ['{70E1217C-C617-4CFD-BD8A-69CA2043E70B}']
   function Get_value : WideString; safecall;
   procedure Set_value(const pVal:WideString); safecall;
   function Get_border : WordBool; safecall;
   procedure Set_border(const pVal:WordBool); safecall;
   function Get_justification : WideString; safecall;
   procedure Set_justification(const pVal:WideString); safecall;
   function Get_editStyle : WideString; safecall;
   procedure Set_editStyle(const pVal:WideString); safecall;
   function Get_wordWrap : WordBool; safecall;
   procedure Set_wordWrap(const pVal:WordBool); safecall;
   function Get_readOnly : WordBool; safecall;
   procedure Set_readOnly(const pVal:WordBool); safecall;
   function Get_foregroundColor : WideString; safecall;
   procedure Set_foregroundColor(const pVal:WideString); safecall;
   function Get_backgroundColor : WideString; safecall;
   procedure Set_backgroundColor(const pVal:WideString); safecall;
   function Get_fontSize : Integer; safecall;
   procedure Set_fontSize(const pVal:Integer); safecall;
   function Get_fontStyle : WideString; safecall;
   procedure Set_fontStyle(const pVal:WideString); safecall;
   function Get_fontFace : WideString; safecall;
   procedure Set_fontFace(const pVal:WideString); safecall;
   function Get_textLimit : Integer; safecall;
   procedure Set_textLimit(const pVal:Integer); safecall;
   function Get_lineCount : Integer; safecall;
    // getLine : method getLine 
   function getLine(nIndex:Integer):WideString;safecall;
    // getSelectionStart : method getSelectionStart 
   function getSelectionStart:Integer;safecall;
    // getSelectionEnd : method getSelectionEnd 
   function getSelectionEnd:Integer;safecall;
    // setSelection : method setSelection 
   procedure setSelection(nStart:Integer;nEnd:Integer);safecall;
    // replaceSelection : method replaceSelection 
   procedure replaceSelection(newVal:WideString);safecall;
    // getLineIndex : method getLineIndex 
   function getLineIndex(nIndex:Integer):Integer;safecall;
    // getLineFromChar : method getLineFromChar 
   function getLineFromChar(nPosition:Integer):Integer;safecall;
    // value : property value 
   property value:WideString read Get_value write Set_value;
    // border : property border 
   property border:WordBool read Get_border write Set_border;
    // justification : property justification 
   property justification:WideString read Get_justification write Set_justification;
    // editStyle : property editStyle 
   property editStyle:WideString read Get_editStyle write Set_editStyle;
    // wordWrap : property wordWrap 
   property wordWrap:WordBool read Get_wordWrap write Set_wordWrap;
    // readOnly : property readOnly 
   property readOnly:WordBool read Get_readOnly write Set_readOnly;
    // foregroundColor : property foregroundColor 
   property foregroundColor:WideString read Get_foregroundColor write Set_foregroundColor;
    // backgroundColor : property backgroundColor 
   property backgroundColor:WideString read Get_backgroundColor write Set_backgroundColor;
    // fontSize : property fontSize 
   property fontSize:Integer read Get_fontSize write Set_fontSize;
    // fontStyle : property fontStyle 
   property fontStyle:WideString read Get_fontStyle write Set_fontStyle;
    // fontFace : property fontFace 
   property fontFace:WideString read Get_fontFace write Set_fontFace;
    // textLimit : property textLimit 
   property textLimit:Integer read Get_textLimit write Set_textLimit;
    // lineCount : property lineCount 
   property lineCount:Integer read Get_lineCount;
  end;


// IWMPEditCtrl : IWMPEditCtrl: Public interface for skin object model.

 IWMPEditCtrlDisp = dispinterface
   ['{70E1217C-C617-4CFD-BD8A-69CA2043E70B}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // getLine : method getLine 
   function getLine(nIndex:Integer):WideString;dispid 6012;
    // getSelectionStart : method getSelectionStart 
   function getSelectionStart:Integer;dispid 6013;
    // getSelectionEnd : method getSelectionEnd 
   function getSelectionEnd:Integer;dispid 6014;
    // setSelection : method setSelection 
   procedure setSelection(nStart:Integer;nEnd:Integer);dispid 6015;
    // replaceSelection : method replaceSelection 
   procedure replaceSelection(newVal:WideString);dispid 6016;
    // getLineIndex : method getLineIndex 
   function getLineIndex(nIndex:Integer):Integer;dispid 6017;
    // getLineFromChar : method getLineFromChar 
   function getLineFromChar(nPosition:Integer):Integer;dispid 6018;
    // value : property value 
   property value:WideString dispid 0;
    // border : property border 
   property border:WordBool dispid 6000;
    // justification : property justification 
   property justification:WideString dispid 6001;
    // editStyle : property editStyle 
   property editStyle:WideString dispid 6002;
    // wordWrap : property wordWrap 
   property wordWrap:WordBool dispid 6003;
    // readOnly : property readOnly 
   property readOnly:WordBool dispid 6004;
    // foregroundColor : property foregroundColor 
   property foregroundColor:WideString dispid 6005;
    // backgroundColor : property backgroundColor 
   property backgroundColor:WideString dispid 6006;
    // fontSize : property fontSize 
   property fontSize:Integer dispid 6007;
    // fontStyle : property fontStyle 
   property fontStyle:WideString dispid 6008;
    // fontFace : property fontFace 
   property fontFace:WideString dispid 6009;
    // textLimit : property textLimit 
   property textLimit:Integer dispid 6010;
    // lineCount : property lineCount 
   property lineCount:Integer  readonly dispid 6011;
  end;


// IWMPSkinList : IWMPSkinlist: interface for skin object model.

 IWMPSkinList = interface(IDispatch)
   ['{8CEA03A2-D0C5-4E97-9C38-A676A639A51D}']
    // updateBasketColumns : property basketVisible 
   procedure updateBasketColumns;safecall;
    // highContrastChange : property highContrastChange 
   procedure highContrastChange;safecall;
  end;


// IWMPSkinList : IWMPSkinlist: interface for skin object model.

 IWMPSkinListDisp = dispinterface
   ['{8CEA03A2-D0C5-4E97-9C38-A676A639A51D}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // updateBasketColumns : property basketVisible 
   procedure updateBasketColumns;dispid 6050;
    // highContrastChange : property highContrastChange 
   procedure highContrastChange;dispid 6051;
  end;


// IWMPPluginUIHost : IWMPPluginUIHost: Not Public.  Internal interface used by Windows Media Player.

 IWMPPluginUIHost = interface(IDispatch)
   ['{5D0AD945-289E-45C5-A9C6-F301F0152108}']
   function Get_backgroundColor : WideString; safecall;
   procedure Set_backgroundColor(const pVal:WideString); safecall;
   function Get_objectID : WideString; safecall;
   procedure Set_objectID(const pVal:WideString); safecall;
    // getProperty : method getProperty 
   function getProperty(bstrName:WideString):OleVariant;safecall;
    // setProperty : method setProperty 
   procedure setProperty(bstrName:WideString;newVal:OleVariant);safecall;
    // backgroundColor : property backgroundColor 
   property backgroundColor:WideString read Get_backgroundColor write Set_backgroundColor;
    // objectID : property objectID 
   property objectID:WideString read Get_objectID write Set_objectID;
  end;


// IWMPPluginUIHost : IWMPPluginUIHost: Not Public.  Internal interface used by Windows Media Player.

 IWMPPluginUIHostDisp = dispinterface
   ['{5D0AD945-289E-45C5-A9C6-F301F0152108}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // getProperty : method getProperty 
   function getProperty(bstrName:WideString):OleVariant;dispid 6203;
    // setProperty : method setProperty 
   procedure setProperty(bstrName:WideString;newVal:OleVariant);dispid 6204;
    // backgroundColor : property backgroundColor 
   property backgroundColor:WideString dispid 6201;
    // objectID : property objectID 
   property objectID:WideString dispid 6202;
  end;


// IWMPMenuCtrl : IWMPMenuCtrl: Not Public.  Internal interface used by Windows Media Player.

 IWMPMenuCtrl = interface(IDispatch)
   ['{158A7ADC-33DA-4039-A553-BDDBBE389F5C}']
    // deleteAllItems : method deleteAllItems 
   procedure deleteAllItems;safecall;
    // appendItem : method appendItem 
   procedure appendItem(nID:Integer;bstrItem:WideString);safecall;
    // appendSeparator : method appendSeparator 
   procedure appendSeparator;safecall;
    // enableItem : property enableItem 
   procedure enableItem(nID:Integer;newVal:WordBool);safecall;
    // checkItem : property checkItem 
   procedure checkItem(nID:Integer;newVal:WordBool);safecall;
    // checkRadioItem : property checkRadioItem 
   procedure checkRadioItem(nID:Integer;newVal:WordBool);safecall;
   function Get_showFlags : Integer; safecall;
   procedure Set_showFlags(const pVal:Integer); safecall;
    // show : method show 
   function show:Integer;safecall;
    // showEx : method showEx 
   procedure showEx(nID:Integer);safecall;
    // showFlags : property showFlags 
   property showFlags:Integer read Get_showFlags write Set_showFlags;
  end;


// IWMPMenuCtrl : IWMPMenuCtrl: Not Public.  Internal interface used by Windows Media Player.

 IWMPMenuCtrlDisp = dispinterface
   ['{158A7ADC-33DA-4039-A553-BDDBBE389F5C}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // deleteAllItems : method deleteAllItems 
   procedure deleteAllItems;dispid 6301;
    // appendItem : method appendItem 
   procedure appendItem(nID:Integer;bstrItem:WideString);dispid 6302;
    // appendSeparator : method appendSeparator 
   procedure appendSeparator;dispid 6303;
    // enableItem : property enableItem 
   procedure enableItem(nID:Integer;newVal:WordBool);dispid 6304;
    // checkItem : property checkItem 
   procedure checkItem(nID:Integer;newVal:WordBool);dispid 6305;
    // checkRadioItem : property checkRadioItem 
   procedure checkRadioItem(nID:Integer;newVal:WordBool);dispid 6306;
    // show : method show 
   function show:Integer;dispid 6308;
    // showEx : method showEx 
   procedure showEx(nID:Integer);dispid 6309;
    // showFlags : property showFlags 
   property showFlags:Integer dispid 6307;
  end;


// IWMPAutoMenuCtrl : IWMPAutoMenuCtrl: Not Public.  Internal interface used by Windows Media Player.

 IWMPAutoMenuCtrl = interface(IDispatch)
   ['{1AD13E0B-4F3A-41DF-9BE2-F9E6FE0A7875}']
    // show : method show 
   procedure show(newVal:WideString);safecall;
  end;


// IWMPAutoMenuCtrl : IWMPAutoMenuCtrl: Not Public.  Internal interface used by Windows Media Player.

 IWMPAutoMenuCtrlDisp = dispinterface
   ['{1AD13E0B-4F3A-41DF-9BE2-F9E6FE0A7875}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // show : method show 
   procedure show(newVal:WideString);dispid 6501;
  end;


// IWMPRegionalButtonCtrl : IWMPRegionalButtonCtrl: Public interface for skin object model.

 IWMPRegionalButtonCtrl = interface(IDispatch)
   ['{58D507B1-2354-11D3-BD41-00C04F6EA5AE}']
   function Get_image : WideString; safecall;
   procedure Set_image(const pVal:WideString); safecall;
   function Get_hoverImage : WideString; safecall;
   procedure Set_hoverImage(const pVal:WideString); safecall;
   function Get_downImage : WideString; safecall;
   procedure Set_downImage(const pVal:WideString); safecall;
   function Get_hoverDownImage : WideString; safecall;
   procedure Set_hoverDownImage(const pVal:WideString); safecall;
   function Get_hoverHoverImage : WideString; safecall;
   procedure Set_hoverHoverImage(const pVal:WideString); safecall;
   function Get_disabledImage : WideString; safecall;
   procedure Set_disabledImage(const pVal:WideString); safecall;
   function Get_mappingImage : WideString; safecall;
   procedure Set_mappingImage(const pVal:WideString); safecall;
   function Get_transparencyColor : WideString; safecall;
   procedure Set_transparencyColor(const pVal:WideString); safecall;
   function Get_cursor : WideString; safecall;
   procedure Set_cursor(const pVal:WideString); safecall;
   function Get_showBackground : WordBool; safecall;
   procedure Set_showBackground(const pVal:WordBool); safecall;
   function Get_radio : WordBool; safecall;
   procedure Set_radio(const pVal:WordBool); safecall;
   function Get_buttonCount : Integer; safecall;
    // createButton : method CreateButton 
   function createButton:IDispatch;safecall;
    // getButton : method GetButton 
   function getButton(nButton:Integer):IDispatch;safecall;
    // Click : method Click 
   procedure Click(nButton:Integer);safecall;
   function Get_hueShift : Single; safecall;
   procedure Set_hueShift(const pVal:Single); safecall;
   function Get_saturation : Single; safecall;
   procedure Set_saturation(const pVal:Single); safecall;
    // image : property Image 
   property image:WideString read Get_image write Set_image;
    // hoverImage : property HoverImage 
   property hoverImage:WideString read Get_hoverImage write Set_hoverImage;
    // downImage : property DownImage 
   property downImage:WideString read Get_downImage write Set_downImage;
    // hoverDownImage : property HoverDownImage 
   property hoverDownImage:WideString read Get_hoverDownImage write Set_hoverDownImage;
    // hoverHoverImage : property hoverHoverImage 
   property hoverHoverImage:WideString read Get_hoverHoverImage write Set_hoverHoverImage;
    // disabledImage : property DisabledImage 
   property disabledImage:WideString read Get_disabledImage write Set_disabledImage;
    // mappingImage : property MappingImage 
   property mappingImage:WideString read Get_mappingImage write Set_mappingImage;
    // transparencyColor : property TransparencyColor 
   property transparencyColor:WideString read Get_transparencyColor write Set_transparencyColor;
    // cursor : property Cursor 
   property cursor:WideString read Get_cursor write Set_cursor;
    // showBackground : property ShowBackground 
   property showBackground:WordBool read Get_showBackground write Set_showBackground;
    // radio : property Radio 
   property radio:WordBool read Get_radio write Set_radio;
    // buttonCount : property ButtonCount 
   property buttonCount:Integer read Get_buttonCount;
    // hueShift : property hueShift 
   property hueShift:Single read Get_hueShift write Set_hueShift;
    // saturation : property saturation 
   property saturation:Single read Get_saturation write Set_saturation;
  end;


// IWMPRegionalButtonCtrl : IWMPRegionalButtonCtrl: Public interface for skin object model.

 IWMPRegionalButtonCtrlDisp = dispinterface
   ['{58D507B1-2354-11D3-BD41-00C04F6EA5AE}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // createButton : method CreateButton 
   function createButton:IDispatch;dispid 5312;
    // getButton : method GetButton 
   function getButton(nButton:Integer):IDispatch;dispid 5313;
    // Click : method Click 
   procedure Click(nButton:Integer);dispid 5314;
    // image : property Image 
   property image:WideString dispid 5300;
    // hoverImage : property HoverImage 
   property hoverImage:WideString dispid 5301;
    // downImage : property DownImage 
   property downImage:WideString dispid 5302;
    // hoverDownImage : property HoverDownImage 
   property hoverDownImage:WideString dispid 5303;
    // hoverHoverImage : property hoverHoverImage 
   property hoverHoverImage:WideString dispid 5317;
    // disabledImage : property DisabledImage 
   property disabledImage:WideString dispid 5304;
    // mappingImage : property MappingImage 
   property mappingImage:WideString dispid 5305;
    // transparencyColor : property TransparencyColor 
   property transparencyColor:WideString dispid 5306;
    // cursor : property Cursor 
   property cursor:WideString dispid 5308;
    // showBackground : property ShowBackground 
   property showBackground:WordBool dispid 5309;
    // radio : property Radio 
   property radio:WordBool dispid 5310;
    // buttonCount : property ButtonCount 
   property buttonCount:Integer  readonly dispid 5311;
    // hueShift : property hueShift 
   property hueShift:Single dispid 5315;
    // saturation : property saturation 
   property saturation:Single dispid 5316;
  end;


// IWMPRegionalButtonEvents : IWMPRegionalButtonEvents: Public interface for skin object model.

 IWMPRegionalButtonEvents = dispinterface
   ['{50FC8D31-67AC-11D3-BD4C-00C04F6EA5AE}']
    // onblur : event onblur 
   function onblur:HResult;dispid 5360;
    // onfocus : event onfocus 
   function onfocus:HResult;dispid 5361;
    // onclick : event onclick 
   function onclick:HResult;dispid 5362;
    // ondblclick : event ondblclick 
   function ondblclick:HResult;dispid 5363;
    // onmousedown : event onmousedown 
   function onmousedown:HResult;dispid 5364;
    // onmouseup : event onmouseup 
   function onmouseup:HResult;dispid 5365;
    // onmousemove : event onmousemove 
   function onmousemove:HResult;dispid 5366;
    // onmouseover : event onmouseover 
   function onmouseover:HResult;dispid 5367;
    // onmouseout : event onmouseout 
   function onmouseout:HResult;dispid 5368;
    // onkeypress : event onkeypress 
   function onkeypress:HResult;dispid 5369;
    // onkeydown : event onkeydown 
   function onkeydown:HResult;dispid 5370;
    // onkeyup : event onkeyup 
   function onkeyup:HResult;dispid 5371;
  end;


// IWMPRegionalButton : IWMPRegionalButton: Public interface for skin object model.

 IWMPRegionalButton = interface(IDispatch)
   ['{58D507B2-2354-11D3-BD41-00C04F6EA5AE}']
   function Get_upToolTip : WideString; safecall;
   procedure Set_upToolTip(const pVal:WideString); safecall;
   function Get_downToolTip : WideString; safecall;
   procedure Set_downToolTip(const pVal:WideString); safecall;
   function Get_mappingColor : WideString; safecall;
   procedure Set_mappingColor(const pVal:WideString); safecall;
   function Get_enabled : WordBool; safecall;
   procedure Set_enabled(const pVal:WordBool); safecall;
   function Get_sticky : WordBool; safecall;
   procedure Set_sticky(const pVal:WordBool); safecall;
   function Get_down : WordBool; safecall;
   procedure Set_down(const pVal:WordBool); safecall;
   function Get_index : Integer; safecall;
   function Get_tabStop : WordBool; safecall;
   procedure Set_tabStop(const pVal:WordBool); safecall;
   function Get_cursor : WideString; safecall;
   procedure Set_cursor(const pVal:WideString); safecall;
    // Click : method Click 
   procedure Click;safecall;
   function Get_accName : WideString; safecall;
   procedure Set_accName(const pszName:WideString); safecall;
   function Get_accDescription : WideString; safecall;
   procedure Set_accDescription(const pszDescription:WideString); safecall;
   function Get_accKeyboardShortcut : WideString; safecall;
   procedure Set_accKeyboardShortcut(const pszShortcut:WideString); safecall;
    // upToolTip : property UpToolTip 
   property upToolTip:WideString read Get_upToolTip write Set_upToolTip;
    // downToolTip : property DownToolTip 
   property downToolTip:WideString read Get_downToolTip write Set_downToolTip;
    // mappingColor : property MappingColor 
   property mappingColor:WideString read Get_mappingColor write Set_mappingColor;
    // enabled : property Enabled 
   property enabled:WordBool read Get_enabled write Set_enabled;
    // sticky : property Sticky 
   property sticky:WordBool read Get_sticky write Set_sticky;
    // down : property Down 
   property down:WordBool read Get_down write Set_down;
    // index : property Index 
   property index:Integer read Get_index;
    // tabStop : property TabStop 
   property tabStop:WordBool read Get_tabStop write Set_tabStop;
    // cursor : property Cursor 
   property cursor:WideString read Get_cursor write Set_cursor;
    // accName : property AccName 
   property accName:WideString read Get_accName write Set_accName;
    // accDescription : property AccDescription 
   property accDescription:WideString read Get_accDescription write Set_accDescription;
    // accKeyboardShortcut : property accKeyboardShortcut 
   property accKeyboardShortcut:WideString read Get_accKeyboardShortcut write Set_accKeyboardShortcut;
  end;


// IWMPRegionalButton : IWMPRegionalButton: Public interface for skin object model.

 IWMPRegionalButtonDisp = dispinterface
   ['{58D507B2-2354-11D3-BD41-00C04F6EA5AE}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // Click : method Click 
   procedure Click;dispid 5344;
    // upToolTip : property UpToolTip 
   property upToolTip:WideString dispid 5330;
    // downToolTip : property DownToolTip 
   property downToolTip:WideString dispid 5331;
    // mappingColor : property MappingColor 
   property mappingColor:WideString dispid 5332;
    // enabled : property Enabled 
   property enabled:WordBool dispid 5333;
    // sticky : property Sticky 
   property sticky:WordBool dispid 5339;
    // down : property Down 
   property down:WordBool dispid 5340;
    // index : property Index 
   property index:Integer  readonly dispid 5341;
    // tabStop : property TabStop 
   property tabStop:WordBool dispid 5342;
    // cursor : property Cursor 
   property cursor:WideString dispid 5343;
    // accName : property AccName 
   property accName:WideString dispid 5345;
    // accDescription : property AccDescription 
   property accDescription:WideString dispid 5346;
    // accKeyboardShortcut : property accKeyboardShortcut 
   property accKeyboardShortcut:WideString dispid 5347;
  end;


// IWMPCustomSliderCtrlEvents : IWMPCustomSliderCtrlEvents: Public interface for skin object model.

 IWMPCustomSliderCtrlEvents = dispinterface
   ['{95F45AA4-ED0A-11D2-BA67-0000F80855E6}']
    // ondragbegin : event ondragbegin 
   function ondragbegin:HResult;dispid 5020;
    // ondragend : event ondragend 
   function ondragend:HResult;dispid 5021;
    // onpositionchange : event onpositionchange 
   function onpositionchange:HResult;dispid 5022;
  end;


// IWMPCustomSlider : IWMPCustomSlider: Public interface for skin object model.

 IWMPCustomSlider = interface(IDispatch)
   ['{95F45AA2-ED0A-11D2-BA67-0000F80855E6}']
   function Get_cursor : WideString; safecall;
   procedure Set_cursor(const pVal:WideString); safecall;
   function Get_min : Single; safecall;
   procedure Set_min(const pVal:Single); safecall;
   function Get_max : Single; safecall;
   procedure Set_max(const pVal:Single); safecall;
   function Get_value : Single; safecall;
   procedure Set_value(const pVal:Single); safecall;
   function Get_toolTip : WideString; safecall;
   procedure Set_toolTip(const pVal:WideString); safecall;
   function Get_positionImage : WideString; safecall;
   procedure Set_positionImage(const pVal:WideString); safecall;
   function Get_image : WideString; safecall;
   procedure Set_image(const pVal:WideString); safecall;
   function Get_hoverImage : WideString; safecall;
   procedure Set_hoverImage(const pVal:WideString); safecall;
   function Get_disabledImage : WideString; safecall;
   procedure Set_disabledImage(const pVal:WideString); safecall;
   function Get_downImage : WideString; safecall;
   procedure Set_downImage(const pVal:WideString); safecall;
   function Get_transparencyColor : WideString; safecall;
   procedure Set_transparencyColor(const pVal:WideString); safecall;
    // cursor : property cursor 
   property cursor:WideString read Get_cursor write Set_cursor;
    // min : property min 
   property min:Single read Get_min write Set_min;
    // max : property max 
   property max:Single read Get_max write Set_max;
    // value : property value 
   property value:Single read Get_value write Set_value;
    // toolTip : property toolTip 
   property toolTip:WideString read Get_toolTip write Set_toolTip;
    // positionImage : property positionImage 
   property positionImage:WideString read Get_positionImage write Set_positionImage;
    // image : property image 
   property image:WideString read Get_image write Set_image;
    // hoverImage : property hoverImage 
   property hoverImage:WideString read Get_hoverImage write Set_hoverImage;
    // disabledImage : property disabledImage 
   property disabledImage:WideString read Get_disabledImage write Set_disabledImage;
    // downImage : property downImage 
   property downImage:WideString read Get_downImage write Set_downImage;
    // transparencyColor : property transparancyColor 
   property transparencyColor:WideString read Get_transparencyColor write Set_transparencyColor;
  end;


// IWMPCustomSlider : IWMPCustomSlider: Public interface for skin object model.

 IWMPCustomSliderDisp = dispinterface
   ['{95F45AA2-ED0A-11D2-BA67-0000F80855E6}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // cursor : property cursor 
   property cursor:WideString dispid 5009;
    // min : property min 
   property min:Single dispid 5005;
    // max : property max 
   property max:Single dispid 5006;
    // value : property value 
   property value:Single dispid 5010;
    // toolTip : property toolTip 
   property toolTip:WideString dispid 5011;
    // positionImage : property positionImage 
   property positionImage:WideString dispid 5002;
    // image : property image 
   property image:WideString dispid 5001;
    // hoverImage : property hoverImage 
   property hoverImage:WideString dispid 5003;
    // disabledImage : property disabledImage 
   property disabledImage:WideString dispid 5004;
    // downImage : property downImage 
   property downImage:WideString dispid 5012;
    // transparencyColor : property transparancyColor 
   property transparencyColor:WideString dispid 5008;
  end;


// IWMPTextCtrl : IWMPTextCtrl: Public interface for skin object model.

 IWMPTextCtrl = interface(IDispatch)
   ['{237DAC8E-0E32-11D3-A2E2-00C04F79F88E}']
   function Get_backgroundColor : WideString; safecall;
   procedure Set_backgroundColor(const pVal:WideString); safecall;
   function Get_fontFace : WideString; safecall;
   procedure Set_fontFace(const pVal:WideString); safecall;
   function Get_fontStyle : WideString; safecall;
   procedure Set_fontStyle(const pVal:WideString); safecall;
   function Get_fontSize : Integer; safecall;
   procedure Set_fontSize(const pVal:Integer); safecall;
   function Get_foregroundColor : WideString; safecall;
   procedure Set_foregroundColor(const pVal:WideString); safecall;
   function Get_hoverBackgroundColor : WideString; safecall;
   procedure Set_hoverBackgroundColor(const pVal:WideString); safecall;
   function Get_hoverForegroundColor : WideString; safecall;
   procedure Set_hoverForegroundColor(const pVal:WideString); safecall;
   function Get_hoverFontStyle : WideString; safecall;
   procedure Set_hoverFontStyle(const pVal:WideString); safecall;
   function Get_value : WideString; safecall;
   procedure Set_value(const pVal:WideString); safecall;
   function Get_toolTip : WideString; safecall;
   procedure Set_toolTip(const pVal:WideString); safecall;
   function Get_disabledFontStyle : WideString; safecall;
   procedure Set_disabledFontStyle(const pVal:WideString); safecall;
   function Get_disabledForegroundColor : WideString; safecall;
   procedure Set_disabledForegroundColor(const pVal:WideString); safecall;
   function Get_disabledBackgroundColor : WideString; safecall;
   procedure Set_disabledBackgroundColor(const pVal:WideString); safecall;
   function Get_fontSmoothing : WordBool; safecall;
   procedure Set_fontSmoothing(const pVal:WordBool); safecall;
   function Get_justification : WideString; safecall;
   procedure Set_justification(const pVal:WideString); safecall;
   function Get_wordWrap : WordBool; safecall;
   procedure Set_wordWrap(const pVal:WordBool); safecall;
   function Get_cursor : WideString; safecall;
   procedure Set_cursor(const pVal:WideString); safecall;
   function Get_scrolling : WordBool; safecall;
   procedure Set_scrolling(const pVal:WordBool); safecall;
   function Get_scrollingDirection : WideString; safecall;
   procedure Set_scrollingDirection(const pVal:WideString); safecall;
   function Get_scrollingDelay : SYSINT; safecall;
   procedure Set_scrollingDelay(const pVal:SYSINT); safecall;
   function Get_scrollingAmount : SYSINT; safecall;
   procedure Set_scrollingAmount(const pVal:SYSINT); safecall;
   function Get_textWidth : SYSINT; safecall;
   function Get_onGlass : WordBool; safecall;
   procedure Set_onGlass(const pVal:WordBool); safecall;
   function Get_disableGlassBlurBackground : WordBool; safecall;
   procedure Set_disableGlassBlurBackground(const pVal:WordBool); safecall;
    // backgroundColor : property backgroundColor 
   property backgroundColor:WideString read Get_backgroundColor write Set_backgroundColor;
    // fontFace : property fontFace 
   property fontFace:WideString read Get_fontFace write Set_fontFace;
    // fontStyle : property fontStyle 
   property fontStyle:WideString read Get_fontStyle write Set_fontStyle;
    // fontSize : property fontSize 
   property fontSize:Integer read Get_fontSize write Set_fontSize;
    // foregroundColor : property foregroundColor 
   property foregroundColor:WideString read Get_foregroundColor write Set_foregroundColor;
    // hoverBackgroundColor : property hoverBackgroundColor 
   property hoverBackgroundColor:WideString read Get_hoverBackgroundColor write Set_hoverBackgroundColor;
    // hoverForegroundColor : property hoverForegroundColor 
   property hoverForegroundColor:WideString read Get_hoverForegroundColor write Set_hoverForegroundColor;
    // hoverFontStyle : property hoverFontStyle 
   property hoverFontStyle:WideString read Get_hoverFontStyle write Set_hoverFontStyle;
    // value : property value 
   property value:WideString read Get_value write Set_value;
    // toolTip : property toolTip 
   property toolTip:WideString read Get_toolTip write Set_toolTip;
    // disabledFontStyle : property disabledFontStyle 
   property disabledFontStyle:WideString read Get_disabledFontStyle write Set_disabledFontStyle;
    // disabledForegroundColor : property disabledForegroundColor 
   property disabledForegroundColor:WideString read Get_disabledForegroundColor write Set_disabledForegroundColor;
    // disabledBackgroundColor : property disabledBackgroundColor 
   property disabledBackgroundColor:WideString read Get_disabledBackgroundColor write Set_disabledBackgroundColor;
    // fontSmoothing : property fontSmoothing 
   property fontSmoothing:WordBool read Get_fontSmoothing write Set_fontSmoothing;
    // justification : property justification 
   property justification:WideString read Get_justification write Set_justification;
    // wordWrap : property wordWrap 
   property wordWrap:WordBool read Get_wordWrap write Set_wordWrap;
    // cursor : property cursor 
   property cursor:WideString read Get_cursor write Set_cursor;
    // scrolling : property scrolling 
   property scrolling:WordBool read Get_scrolling write Set_scrolling;
    // scrollingDirection : property scrollingDirection 
   property scrollingDirection:WideString read Get_scrollingDirection write Set_scrollingDirection;
    // scrollingDelay : property scrollingDelay 
   property scrollingDelay:SYSINT read Get_scrollingDelay write Set_scrollingDelay;
    // scrollingAmount : property scrollingAmount 
   property scrollingAmount:SYSINT read Get_scrollingAmount write Set_scrollingAmount;
    // textWidth : property textWidth 
   property textWidth:SYSINT read Get_textWidth;
    // onGlass : property onGlass 
   property onGlass:WordBool read Get_onGlass write Set_onGlass;
    // disableGlassBlurBackground : property disableGlassBlurBackground 
   property disableGlassBlurBackground:WordBool read Get_disableGlassBlurBackground write Set_disableGlassBlurBackground;
  end;


// IWMPTextCtrl : IWMPTextCtrl: Public interface for skin object model.

 IWMPTextCtrlDisp = dispinterface
   ['{237DAC8E-0E32-11D3-A2E2-00C04F79F88E}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // backgroundColor : property backgroundColor 
   property backgroundColor:WideString dispid 5201;
    // fontFace : property fontFace 
   property fontFace:WideString dispid 5206;
    // fontStyle : property fontStyle 
   property fontStyle:WideString dispid 5207;
    // fontSize : property fontSize 
   property fontSize:Integer dispid 5208;
    // foregroundColor : property foregroundColor 
   property foregroundColor:WideString dispid 5209;
    // hoverBackgroundColor : property hoverBackgroundColor 
   property hoverBackgroundColor:WideString dispid 5210;
    // hoverForegroundColor : property hoverForegroundColor 
   property hoverForegroundColor:WideString dispid 5211;
    // hoverFontStyle : property hoverFontStyle 
   property hoverFontStyle:WideString dispid 5212;
    // value : property value 
   property value:WideString dispid 5213;
    // toolTip : property toolTip 
   property toolTip:WideString dispid 5214;
    // disabledFontStyle : property disabledFontStyle 
   property disabledFontStyle:WideString dispid 5215;
    // disabledForegroundColor : property disabledForegroundColor 
   property disabledForegroundColor:WideString dispid 5216;
    // disabledBackgroundColor : property disabledBackgroundColor 
   property disabledBackgroundColor:WideString dispid 5217;
    // fontSmoothing : property fontSmoothing 
   property fontSmoothing:WordBool dispid 5221;
    // justification : property justification 
   property justification:WideString dispid 5222;
    // wordWrap : property wordWrap 
   property wordWrap:WordBool dispid 5223;
    // cursor : property cursor 
   property cursor:WideString dispid 5224;
    // scrolling : property scrolling 
   property scrolling:WordBool dispid 5225;
    // scrollingDirection : property scrollingDirection 
   property scrollingDirection:WideString dispid 5226;
    // scrollingDelay : property scrollingDelay 
   property scrollingDelay:SYSINT dispid 5227;
    // scrollingAmount : property scrollingAmount 
   property scrollingAmount:SYSINT dispid 5228;
    // textWidth : property textWidth 
   property textWidth:SYSINT  readonly dispid 5229;
    // onGlass : property onGlass 
   property onGlass:WordBool dispid 5230;
    // disableGlassBlurBackground : property disableGlassBlurBackground 
   property disableGlassBlurBackground:WordBool dispid 5231;
  end;


// ITaskCntrCtrl : ITaskCntrCtrl: Not Public.  Internal interface used by Windows Media Player.

 ITaskCntrCtrl = interface(IDispatch)
   ['{891EADB1-1C45-48B0-B704-49A888DA98C4}']
   function Get_CurrentContainer : IUnknown; safecall;
   procedure Set_CurrentContainer(const ppUnk:IUnknown); safecall;
    // Activate :  
   procedure Activate;safecall;
    // CurrentContainer :  
   property CurrentContainer:IUnknown read Get_CurrentContainer write Set_CurrentContainer;
  end;


// ITaskCntrCtrl : ITaskCntrCtrl: Not Public.  Internal interface used by Windows Media Player.

 ITaskCntrCtrlDisp = dispinterface
   ['{891EADB1-1C45-48B0-B704-49A888DA98C4}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // Activate :  
   procedure Activate;dispid 1610743810;
    // CurrentContainer :  
   property CurrentContainer:IUnknown dispid 1610743808;
  end;


// _WMPCoreEvents : _WMPCoreEvents: Public interface.

 _WMPCoreEvents = dispinterface
   ['{D84CCA96-CCE2-11D2-9ECC-0000F8085981}']
    // OpenStateChange : Sent when the control changes OpenState 
   procedure OpenStateChange(NewState:Integer);dispid 5001;
    // PlayStateChange : Sent when the control changes PlayState 
   procedure PlayStateChange(NewState:Integer);dispid 5101;
    // AudioLanguageChange : Sent when the current audio language has changed 
   procedure AudioLanguageChange(LangID:Integer);dispid 5102;
    // StatusChange : Sent when the status string changes 
   procedure StatusChange;dispid 5002;
    // ScriptCommand : Sent when a synchronized command or URL is received 
   procedure ScriptCommand(scType:WideString;Param:WideString);dispid 5301;
    // NewStream : Sent when a new stream is started in a channel 
   procedure NewStream;dispid 5403;
    // Disconnect : Sent when the control is disconnected from the server 
   procedure Disconnect(Result:Integer);dispid 5401;
    // Buffering : Sent when the control begins or ends buffering 
   procedure Buffering(Start:WordBool);dispid 5402;
    // Error : Sent when the control has an error condition 
   procedure Error;dispid 5501;
    // Warning : Sent when the control encounters a problem 
   procedure Warning(WarningType:Integer;Param:Integer;Description:WideString);dispid 5601;
    // EndOfStream : Sent when the end of file is reached 
   procedure EndOfStream(Result:Integer);dispid 5201;
    // PositionChange : Indicates that the current position of the movie has changed 
   procedure PositionChange(oldPosition:Double;newPosition:Double);dispid 5202;
    // MarkerHit : Sent when a marker is reached 
   procedure MarkerHit(MarkerNum:Integer);dispid 5203;
    // DurationUnitChange : Indicates that the unit used to express duration and position has changed 
   procedure DurationUnitChange(NewDurationUnit:Integer);dispid 5204;
    // CdromMediaChange : Indicates that the CD ROM media has changed 
   procedure CdromMediaChange(CdromNum:Integer);dispid 5701;
    // PlaylistChange : Sent when a playlist changes 
   procedure PlaylistChange(Playlist:IDispatch;change:WMPPlaylistChangeEventType);dispid 5801;
    // CurrentPlaylistChange : Sent when the current playlist changes 
   procedure CurrentPlaylistChange(change:WMPPlaylistChangeEventType);dispid 5804;
    // CurrentPlaylistItemAvailable : Sent when a current playlist item becomes available 
   procedure CurrentPlaylistItemAvailable(bstrItemName:WideString);dispid 5805;
    // MediaChange : Sent when a media object changes 
   procedure MediaChange(Item:IDispatch);dispid 5802;
    // CurrentMediaItemAvailable : Sent when a current media item becomes available 
   procedure CurrentMediaItemAvailable(bstrItemName:WideString);dispid 5803;
    // CurrentItemChange : Sent when the item selection on the current playlist changes 
   procedure CurrentItemChange(pdispMedia:IDispatch);dispid 5806;
    // MediaCollectionChange : Sent when the media collection needs to be requeried 
   procedure MediaCollectionChange;dispid 5807;
    // MediaCollectionAttributeStringAdded : Sent when an attribute string is added in the media collection 
   procedure MediaCollectionAttributeStringAdded(bstrAttribName:WideString;bstrAttribVal:WideString);dispid 5808;
    // MediaCollectionAttributeStringRemoved : Sent when an attribute string is removed from the media collection 
   procedure MediaCollectionAttributeStringRemoved(bstrAttribName:WideString;bstrAttribVal:WideString);dispid 5809;
    // MediaCollectionAttributeStringChanged : Sent when an attribute string is changed in the media collection 
   procedure MediaCollectionAttributeStringChanged(bstrAttribName:WideString;bstrOldAttribVal:WideString;bstrNewAttribVal:WideString);dispid 5820;
    // PlaylistCollectionChange : Sent when playlist collection needs to be requeried 
   procedure PlaylistCollectionChange;dispid 5810;
    // PlaylistCollectionPlaylistAdded : Sent when a playlist is added to the playlist collection 
   procedure PlaylistCollectionPlaylistAdded(bstrPlaylistName:WideString);dispid 5811;
    // PlaylistCollectionPlaylistRemoved : Sent when a playlist is removed from the playlist collection 
   procedure PlaylistCollectionPlaylistRemoved(bstrPlaylistName:WideString);dispid 5812;
    // PlaylistCollectionPlaylistSetAsDeleted : Sent when a playlist has been set or reset as deleted 
   procedure PlaylistCollectionPlaylistSetAsDeleted(bstrPlaylistName:WideString;varfIsDeleted:WordBool);dispid 5818;
    // ModeChange : Playlist playback mode has changed 
   procedure ModeChange(ModeName:WideString;NewValue:WordBool);dispid 5819;
    // MediaError : Sent when the media object has an error condition 
   procedure MediaError(pMediaObject:IDispatch);dispid 5821;
    // OpenPlaylistSwitch : Current playlist switch with no open state change 
   procedure OpenPlaylistSwitch(pItem:IDispatch);dispid 5823;
    // DomainChange : Send a current domain 
   procedure DomainChange(strDomain:WideString);dispid 5822;
    // StringCollectionChange : Sent when a string collection changes 
   procedure StringCollectionChange(pdispStringCollection:IDispatch;change:WMPStringCollectionChangeEventType;lCollectionIndex:Integer);dispid 5824;
    // MediaCollectionMediaAdded : Sent when a media is added to the local library 
   procedure MediaCollectionMediaAdded(pdispMedia:IDispatch);dispid 5825;
    // MediaCollectionMediaRemoved : Sent when a media is removed from the local library 
   procedure MediaCollectionMediaRemoved(pdispMedia:IDispatch);dispid 5826;
  end;


// IWMPGraphEventHandler : IWMPGraphEventHandler: Not Public.  Internal interface used by Windows Media Player.

 IWMPGraphEventHandler = interface(IDispatch)
   ['{6B550945-018F-11D3-B14A-00C04F79FAA6}']
    // NotifyGraphStateChange : Notifies graph state changes 
   procedure NotifyGraphStateChange(punkGraph:ULONG_PTR;lGraphState:Integer);safecall;
    // AsyncNotifyGraphStateChange : Notifies graph state changes asynchronously 
   procedure AsyncNotifyGraphStateChange(punkGraph:ULONG_PTR;lGraphState:Integer);safecall;
    // NotifyRateChange : Notifies changes in playback rate 
   procedure NotifyRateChange(punkGraph:ULONG_PTR;dRate:Double);safecall;
    // NotifyPlaybackEnd : Notifies the end of playback 
   procedure NotifyPlaybackEnd(punkGraph:ULONG_PTR;bstrQueuedUrl:WideString;dwCurrentContext:ULONG_PTR);safecall;
    // NotifyStreamEnd : Notifies the end of a stream 
   procedure NotifyStreamEnd(punkGraph:ULONG_PTR);safecall;
    // NotifyScriptCommand : Notifies that a script command was encountered 
   procedure NotifyScriptCommand(punkGraph:ULONG_PTR;bstrCommand:WideString;bstrParam:WideString);safecall;
    // NotifyEarlyScriptCommand : Notifies that a script command was encountered 
   procedure NotifyEarlyScriptCommand(punkGraph:ULONG_PTR;bstrCommand:WideString;bstrParam:WideString;dTime:Double);safecall;
    // NotifyMarkerHit : Notifies that a marker was encountered 
   procedure NotifyMarkerHit(punkGraph:ULONG_PTR;lMarker:Integer);safecall;
    // NotifyGraphError : Notifies that an error has occurred 
   procedure NotifyGraphError(punkGraph:ULONG_PTR;lErrMajor:Integer;lErrMinor:Integer;lCondition:Integer;bstrInfo:WideString;punkGraphData:IUnknown);safecall;
    // NotifyAcquireCredentials : Spawns the Acquire Credentials dialog 
   procedure NotifyAcquireCredentials(punkGraph:ULONG_PTR;bstrRealm:WideString;bstrSite:WideString;bstrUser:WideString;bstrPassword:WideString;var pdwFlags:LongWord;out pfCancel:WordBool);safecall;
    // NotifyUntrustedLicense : Spawns the untrusted license dialog 
   procedure NotifyUntrustedLicense(punkGraph:ULONG_PTR;bstrURL:WideString;out pfCancel:WordBool);safecall;
    // NotifyLicenseDialog : Notifies a communication with the license dialog 
   procedure NotifyLicenseDialog(punkGraph:ULONG_PTR;bstrURL:WideString;bstrContent:WideString;var pPostData:Byte;dwPostDataSize:LongWord;lResult:Integer);safecall;
    // NotifyNeedsIndividualization : Notifies a communication with the Individualization dialog 
   procedure NotifyNeedsIndividualization(punkGraph:ULONG_PTR;out pfResult:WordBool);safecall;
    // NotifyNewMetadata : Notifies that new metadata is avail 
   procedure NotifyNewMetadata(punkGraph:ULONG_PTR);safecall;
    // NotifyNewMediaCaps : Notifies that new capabilities are avail 
   procedure NotifyNewMediaCaps(punkGraph:ULONG_PTR);safecall;
    // NotifyDisconnect : Notifies that the graph's connection to the media has been lost. 
   procedure NotifyDisconnect(punkGraph:ULONG_PTR;lResult:Integer);safecall;
    // NotifySave : Notifies that the graph save operation started/stopped. 
   procedure NotifySave(punkGraph:ULONG_PTR;fStarted:Integer;lResult:Integer);safecall;
    // NotifyDelayClose : Notifies if the close call needs to be delayed. 
   procedure NotifyDelayClose(punkGraph:ULONG_PTR;fDelay:WordBool);safecall;
    // NotifyDVD : Notifies when domain changes, parental control and region needs to be handled. 
   procedure NotifyDVD(punkGraph:ULONG_PTR;lEventCode:Integer;lParam1:Integer;lParam2:Integer);safecall;
    // NotifyRequestAppThreadAction : Requests a callback into the graph on the apps thread 
   procedure NotifyRequestAppThreadAction(punkGraph:ULONG_PTR;dwAction:LongWord);safecall;
    // NotifyPrerollReady : Notifies that a prerolled graph is ready to play with no more buffering 
   procedure NotifyPrerollReady(punkGraph:ULONG_PTR);safecall;
    // NotifyNewIcons : Notifies core that our DirectShow filters have new icons to display 
   procedure NotifyNewIcons(punkGraph:ULONG_PTR);safecall;
    // NotifyStepComplete : Notifies core that our step operation has completed 
   procedure NotifyStepComplete(punkGraph:ULONG_PTR);safecall;
    // NotifyNewBitrate : Notifies core that our bitrate has changed 
   procedure NotifyNewBitrate(punkGraph:ULONG_PTR;dwBitrate:LongWord);safecall;
    // NotifyGraphCreationPreRender :  
   procedure NotifyGraphCreationPreRender(punkGraph:ULONG_PTR;punkFilterGraph:ULONG_PTR;punkCardeaEncConfig:ULONG_PTR;phrContinue:ULONG_PTR;hEventToSet:ULONG_PTR);safecall;
    // NotifyGraphCreationPostRender :  
   procedure NotifyGraphCreationPostRender(punkGraph:ULONG_PTR;punkFilterGraph:ULONG_PTR;phrContinue:ULONG_PTR;hEventToSet:ULONG_PTR);safecall;
    // NotifyGraphUserEvent : Signals a user event from the renderer 
   procedure NotifyGraphUserEvent(punkGraph:ULONG_PTR;EventCode:Integer);safecall;
    // NotifyRevocation : Notifies a communication with the Revocation dialog 
   procedure NotifyRevocation(punkGraph:ULONG_PTR;out pfResult:WordBool);safecall;
    // NotifyNeedsWMGraphIndividualization : Notifies a communication with the Individualization dialog 
   procedure NotifyNeedsWMGraphIndividualization(punkGraph:ULONG_PTR;phWnd:ULONG_PTR;hIndivEvent:ULONG_PTR;out pfCancel:WordBool;out pfResult:WordBool);safecall;
    // NotifyNeedsFullscreen : Notifies core that the content requires fullscreen mode 
   procedure NotifyNeedsFullscreen(punkGraph:ULONG_PTR);safecall;
  end;


// IWMPGraphEventHandler : IWMPGraphEventHandler: Not Public.  Internal interface used by Windows Media Player.

 IWMPGraphEventHandlerDisp = dispinterface
   ['{6B550945-018F-11D3-B14A-00C04F79FAA6}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // NotifyGraphStateChange : Notifies graph state changes 
   procedure NotifyGraphStateChange(punkGraph:ULONG_PTR;lGraphState:Integer);dispid 8151;
    // AsyncNotifyGraphStateChange : Notifies graph state changes asynchronously 
   procedure AsyncNotifyGraphStateChange(punkGraph:ULONG_PTR;lGraphState:Integer);dispid 8173;
    // NotifyRateChange : Notifies changes in playback rate 
   procedure NotifyRateChange(punkGraph:ULONG_PTR;dRate:Double);dispid 8153;
    // NotifyPlaybackEnd : Notifies the end of playback 
   procedure NotifyPlaybackEnd(punkGraph:ULONG_PTR;bstrQueuedUrl:WideString;dwCurrentContext:ULONG_PTR);dispid 8157;
    // NotifyStreamEnd : Notifies the end of a stream 
   procedure NotifyStreamEnd(punkGraph:ULONG_PTR);dispid 8156;
    // NotifyScriptCommand : Notifies that a script command was encountered 
   procedure NotifyScriptCommand(punkGraph:ULONG_PTR;bstrCommand:WideString;bstrParam:WideString);dispid 8158;
    // NotifyEarlyScriptCommand : Notifies that a script command was encountered 
   procedure NotifyEarlyScriptCommand(punkGraph:ULONG_PTR;bstrCommand:WideString;bstrParam:WideString;dTime:Double);dispid 8172;
    // NotifyMarkerHit : Notifies that a marker was encountered 
   procedure NotifyMarkerHit(punkGraph:ULONG_PTR;lMarker:Integer);dispid 8159;
    // NotifyGraphError : Notifies that an error has occurred 
   procedure NotifyGraphError(punkGraph:ULONG_PTR;lErrMajor:Integer;lErrMinor:Integer;lCondition:Integer;bstrInfo:WideString;punkGraphData:IUnknown);dispid 8160;
    // NotifyAcquireCredentials : Spawns the Acquire Credentials dialog 
   procedure NotifyAcquireCredentials(punkGraph:ULONG_PTR;bstrRealm:WideString;bstrSite:WideString;bstrUser:WideString;bstrPassword:WideString;var pdwFlags:LongWord;out pfCancel:WordBool);dispid 8161;
    // NotifyUntrustedLicense : Spawns the untrusted license dialog 
   procedure NotifyUntrustedLicense(punkGraph:ULONG_PTR;bstrURL:WideString;out pfCancel:WordBool);dispid 8178;
    // NotifyLicenseDialog : Notifies a communication with the license dialog 
   procedure NotifyLicenseDialog(punkGraph:ULONG_PTR;bstrURL:WideString;bstrContent:WideString;var pPostData:Byte;dwPostDataSize:LongWord;lResult:Integer);dispid 8162;
    // NotifyNeedsIndividualization : Notifies a communication with the Individualization dialog 
   procedure NotifyNeedsIndividualization(punkGraph:ULONG_PTR;out pfResult:WordBool);dispid 8163;
    // NotifyNewMetadata : Notifies that new metadata is avail 
   procedure NotifyNewMetadata(punkGraph:ULONG_PTR);dispid 8165;
    // NotifyNewMediaCaps : Notifies that new capabilities are avail 
   procedure NotifyNewMediaCaps(punkGraph:ULONG_PTR);dispid 8166;
    // NotifyDisconnect : Notifies that the graph's connection to the media has been lost. 
   procedure NotifyDisconnect(punkGraph:ULONG_PTR;lResult:Integer);dispid 8167;
    // NotifySave : Notifies that the graph save operation started/stopped. 
   procedure NotifySave(punkGraph:ULONG_PTR;fStarted:Integer;lResult:Integer);dispid 8168;
    // NotifyDelayClose : Notifies if the close call needs to be delayed. 
   procedure NotifyDelayClose(punkGraph:ULONG_PTR;fDelay:WordBool);dispid 8169;
    // NotifyDVD : Notifies when domain changes, parental control and region needs to be handled. 
   procedure NotifyDVD(punkGraph:ULONG_PTR;lEventCode:Integer;lParam1:Integer;lParam2:Integer);dispid 8170;
    // NotifyRequestAppThreadAction : Requests a callback into the graph on the apps thread 
   procedure NotifyRequestAppThreadAction(punkGraph:ULONG_PTR;dwAction:LongWord);dispid 8171;
    // NotifyPrerollReady : Notifies that a prerolled graph is ready to play with no more buffering 
   procedure NotifyPrerollReady(punkGraph:ULONG_PTR);dispid 8174;
    // NotifyNewIcons : Notifies core that our DirectShow filters have new icons to display 
   procedure NotifyNewIcons(punkGraph:ULONG_PTR);dispid 8177;
    // NotifyStepComplete : Notifies core that our step operation has completed 
   procedure NotifyStepComplete(punkGraph:ULONG_PTR);dispid 8179;
    // NotifyNewBitrate : Notifies core that our bitrate has changed 
   procedure NotifyNewBitrate(punkGraph:ULONG_PTR;dwBitrate:LongWord);dispid 8180;
    // NotifyGraphCreationPreRender :  
   procedure NotifyGraphCreationPreRender(punkGraph:ULONG_PTR;punkFilterGraph:ULONG_PTR;punkCardeaEncConfig:ULONG_PTR;phrContinue:ULONG_PTR;hEventToSet:ULONG_PTR);dispid 8181;
    // NotifyGraphCreationPostRender :  
   procedure NotifyGraphCreationPostRender(punkGraph:ULONG_PTR;punkFilterGraph:ULONG_PTR;phrContinue:ULONG_PTR;hEventToSet:ULONG_PTR);dispid 8182;
    // NotifyGraphUserEvent : Signals a user event from the renderer 
   procedure NotifyGraphUserEvent(punkGraph:ULONG_PTR;EventCode:Integer);dispid 8186;
    // NotifyRevocation : Notifies a communication with the Revocation dialog 
   procedure NotifyRevocation(punkGraph:ULONG_PTR;out pfResult:WordBool);dispid 8183;
    // NotifyNeedsWMGraphIndividualization : Notifies a communication with the Individualization dialog 
   procedure NotifyNeedsWMGraphIndividualization(punkGraph:ULONG_PTR;phWnd:ULONG_PTR;hIndivEvent:ULONG_PTR;out pfCancel:WordBool;out pfResult:WordBool);dispid 8184;
    // NotifyNeedsFullscreen : Notifies core that the content requires fullscreen mode 
   procedure NotifyNeedsFullscreen(punkGraph:ULONG_PTR);dispid 8185;
  end;


// IBattery : IBattery: Not Public.  Internal interface used by Windows Media Player.

 IBattery = interface(IDispatch)
   ['{F8578BFA-CD8F-4CE1-A684-5B7E85FCA7DC}']
   function Get_presetCount : Integer; safecall;
   function Get_preset(nIndex:Integer) : IDispatch; safecall;
    // presetCount :  
   property presetCount:Integer read Get_presetCount;
    // preset :  
   property preset[nIndex:Integer]:IDispatch read Get_preset;
  end;


// IBattery : IBattery: Not Public.  Internal interface used by Windows Media Player.

 IBatteryDisp = dispinterface
   ['{F8578BFA-CD8F-4CE1-A684-5B7E85FCA7DC}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // presetCount :  
   property presetCount:Integer  readonly dispid 1;
    // preset :  
   property preset[nIndex:Integer]:IDispatch  readonly dispid 2;
  end;


// IBatteryPreset : IBatteryPreset: Not Public.  Internal interface used by Windows Media Player.

 IBatteryPreset = interface(IDispatch)
   ['{40C6BDE7-9C90-49D4-AD20-BEF81A6C5F22}']
   function Get_title : WideString; safecall;
   procedure Set_title(const pVal:WideString); safecall;
    // title :  
   property title:WideString read Get_title write Set_title;
  end;


// IBatteryPreset : IBatteryPreset: Not Public.  Internal interface used by Windows Media Player.

 IBatteryPresetDisp = dispinterface
   ['{40C6BDE7-9C90-49D4-AD20-BEF81A6C5F22}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // title :  
   property title:WideString dispid 1;
  end;


// IBatteryRandomPreset : IBatteryRandomPreset: Not Public.  Internal interface used by Windows Media Player.

 IBatteryRandomPreset = interface(IBatteryPreset)
   ['{F85E2D65-207D-48DB-84B1-915E1735DB17}']
  end;


// IBatteryRandomPreset : IBatteryRandomPreset: Not Public.  Internal interface used by Windows Media Player.

 IBatteryRandomPresetDisp = dispinterface
   ['{F85E2D65-207D-48DB-84B1-915E1735DB17}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // title :  
   property title:WideString dispid 1;
  end;


// IBatterySavedPreset : IBatterySavedPreset: Not Public.  Internal interface used by Windows Media Player.

 IBatterySavedPreset = interface(IBatteryPreset)
   ['{876E7208-0172-4EBB-B08B-2E1D30DFE44C}']
  end;


// IBatterySavedPreset : IBatterySavedPreset: Not Public.  Internal interface used by Windows Media Player.

 IBatterySavedPresetDisp = dispinterface
   ['{876E7208-0172-4EBB-B08B-2E1D30DFE44C}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // title :  
   property title:WideString dispid 1;
  end;


// IBarsEffect : IBarsEffect: Not Public.  Internal interface used by Windows Media Player.

 IBarsEffect = interface(IDispatch)
   ['{33E9291A-F6A9-11D2-9435-00A0C92A2F2D}']
   function Get_displayMode : Integer; safecall;
   procedure Set_displayMode(const pVal:Integer); safecall;
   function Get_showPeaks : WordBool; safecall;
   procedure Set_showPeaks(const pVal:WordBool); safecall;
   function Get_peakHangTime : Integer; safecall;
   procedure Set_peakHangTime(const pVal:Integer); safecall;
   function Get_peakFallbackAcceleration : Single; safecall;
   procedure Set_peakFallbackAcceleration(const pVal:Single); safecall;
   function Get_peakFallbackSpeed : Single; safecall;
   procedure Set_peakFallbackSpeed(const pVal:Single); safecall;
   function Get_levelFallbackAcceleration : Single; safecall;
   procedure Set_levelFallbackAcceleration(const pVal:Single); safecall;
   function Get_levelFallbackSpeed : Single; safecall;
   procedure Set_levelFallbackSpeed(const pVal:Single); safecall;
   function Get_backgroundColor : WideString; safecall;
   procedure Set_backgroundColor(const pVal:WideString); safecall;
   function Get_levelColor : WideString; safecall;
   procedure Set_levelColor(const pVal:WideString); safecall;
   function Get_peakColor : WideString; safecall;
   procedure Set_peakColor(const pVal:WideString); safecall;
   function Get_horizontalSpacing : Integer; safecall;
   procedure Set_horizontalSpacing(const pVal:Integer); safecall;
   function Get_levelWidth : Integer; safecall;
   procedure Set_levelWidth(const pVal:Integer); safecall;
   function Get_levelScale : Single; safecall;
   procedure Set_levelScale(const pVal:Single); safecall;
   function Get_fadeRate : Integer; safecall;
   procedure Set_fadeRate(const pVal:Integer); safecall;
   function Get_fadeMode : Integer; safecall;
   procedure Set_fadeMode(const pVal:Integer); safecall;
   function Get_transparent : WordBool; safecall;
   procedure Set_transparent(const pVal:WordBool); safecall;
    // displayMode : property displayMode 
   property displayMode:Integer read Get_displayMode write Set_displayMode;
    // showPeaks : property showPeaks 
   property showPeaks:WordBool read Get_showPeaks write Set_showPeaks;
    // peakHangTime : property peakHangTime 
   property peakHangTime:Integer read Get_peakHangTime write Set_peakHangTime;
    // peakFallbackAcceleration : property peakFallbackAcceleration 
   property peakFallbackAcceleration:Single read Get_peakFallbackAcceleration write Set_peakFallbackAcceleration;
    // peakFallbackSpeed : property peakFallbackSpeed 
   property peakFallbackSpeed:Single read Get_peakFallbackSpeed write Set_peakFallbackSpeed;
    // levelFallbackAcceleration : property levelFallbackAcceleration 
   property levelFallbackAcceleration:Single read Get_levelFallbackAcceleration write Set_levelFallbackAcceleration;
    // levelFallbackSpeed : property levelFallbackSpeed 
   property levelFallbackSpeed:Single read Get_levelFallbackSpeed write Set_levelFallbackSpeed;
    // backgroundColor : property backgroundColor 
   property backgroundColor:WideString read Get_backgroundColor write Set_backgroundColor;
    // levelColor : property levelColor 
   property levelColor:WideString read Get_levelColor write Set_levelColor;
    // peakColor : property peakColor 
   property peakColor:WideString read Get_peakColor write Set_peakColor;
    // horizontalSpacing : property horizontalSpacing 
   property horizontalSpacing:Integer read Get_horizontalSpacing write Set_horizontalSpacing;
    // levelWidth : property levelWidth 
   property levelWidth:Integer read Get_levelWidth write Set_levelWidth;
    // levelScale : property levelScale 
   property levelScale:Single read Get_levelScale write Set_levelScale;
    // fadeRate : property fadeRate 
   property fadeRate:Integer read Get_fadeRate write Set_fadeRate;
    // fadeMode : property fadeMode 
   property fadeMode:Integer read Get_fadeMode write Set_fadeMode;
    // transparent : property transparent 
   property transparent:WordBool read Get_transparent write Set_transparent;
  end;


// IBarsEffect : IBarsEffect: Not Public.  Internal interface used by Windows Media Player.

 IBarsEffectDisp = dispinterface
   ['{33E9291A-F6A9-11D2-9435-00A0C92A2F2D}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // displayMode : property displayMode 
   property displayMode:Integer dispid 8000;
    // showPeaks : property showPeaks 
   property showPeaks:WordBool dispid 8001;
    // peakHangTime : property peakHangTime 
   property peakHangTime:Integer dispid 8002;
    // peakFallbackAcceleration : property peakFallbackAcceleration 
   property peakFallbackAcceleration:Single dispid 8003;
    // peakFallbackSpeed : property peakFallbackSpeed 
   property peakFallbackSpeed:Single dispid 8004;
    // levelFallbackAcceleration : property levelFallbackAcceleration 
   property levelFallbackAcceleration:Single dispid 8005;
    // levelFallbackSpeed : property levelFallbackSpeed 
   property levelFallbackSpeed:Single dispid 8006;
    // backgroundColor : property backgroundColor 
   property backgroundColor:WideString dispid 8007;
    // levelColor : property levelColor 
   property levelColor:WideString dispid 8008;
    // peakColor : property peakColor 
   property peakColor:WideString dispid 8009;
    // horizontalSpacing : property horizontalSpacing 
   property horizontalSpacing:Integer dispid 8010;
    // levelWidth : property levelWidth 
   property levelWidth:Integer dispid 8012;
    // levelScale : property levelScale 
   property levelScale:Single dispid 8013;
    // fadeRate : property fadeRate 
   property fadeRate:Integer dispid 8014;
    // fadeMode : property fadeMode 
   property fadeMode:Integer dispid 8015;
    // transparent : property transparent 
   property transparent:WordBool dispid 8016;
  end;


// IWMPExternal : IWMPExternal: Public interface for scripting object model.

 IWMPExternal = interface(IDispatch)
   ['{E2CC638C-FD2C-409B-A1EA-5DDB72DC8E84}']
   function Get_version : WideString; safecall;
   function Get_appColorLight : WideString; safecall;
   procedure Set_OnColorChange(const Param1:IDispatch); safecall;
    // version :  
   property version:WideString read Get_version;
    // appColorLight :  
   property appColorLight:WideString read Get_appColorLight;
    // OnColorChange :  
   property OnColorChange:IDispatch write Set_OnColorChange;
  end;


// IWMPExternal : IWMPExternal: Public interface for scripting object model.

 IWMPExternalDisp = dispinterface
   ['{E2CC638C-FD2C-409B-A1EA-5DDB72DC8E84}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // version :  
   property version:WideString  readonly dispid 10005;
    // appColorLight :  
   property appColorLight:WideString  readonly dispid 10012;
    // OnColorChange :  
   property OnColorChange:IDispatch writeonly dispid 10018;
  end;


// IWMPExternalColors : IWMPExternalColors: Public interface for scripting object model.

 IWMPExternalColors = interface(IWMPExternal)
   ['{D10CCDFF-472D-498C-B5FE-3630E5405E0A}']
   function Get_appColorMedium : WideString; safecall;
   function Get_appColorDark : WideString; safecall;
   function Get_appColorButtonHighlight : WideString; safecall;
   function Get_appColorButtonShadow : WideString; safecall;
   function Get_appColorButtonHoverFace : WideString; safecall;
    // appColorMedium :  
   property appColorMedium:WideString read Get_appColorMedium;
    // appColorDark :  
   property appColorDark:WideString read Get_appColorDark;
    // appColorButtonHighlight :  
   property appColorButtonHighlight:WideString read Get_appColorButtonHighlight;
    // appColorButtonShadow :  
   property appColorButtonShadow:WideString read Get_appColorButtonShadow;
    // appColorButtonHoverFace :  
   property appColorButtonHoverFace:WideString read Get_appColorButtonHoverFace;
  end;


// IWMPExternalColors : IWMPExternalColors: Public interface for scripting object model.

 IWMPExternalColorsDisp = dispinterface
   ['{D10CCDFF-472D-498C-B5FE-3630E5405E0A}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // version :  
   property version:WideString  readonly dispid 10005;
    // appColorLight :  
   property appColorLight:WideString  readonly dispid 10012;
    // OnColorChange :  
   property OnColorChange:IDispatch writeonly dispid 10018;
    // appColorMedium :  
   property appColorMedium:WideString  readonly dispid 10013;
    // appColorDark :  
   property appColorDark:WideString  readonly dispid 10014;
    // appColorButtonHighlight :  
   property appColorButtonHighlight:WideString  readonly dispid 10015;
    // appColorButtonShadow :  
   property appColorButtonShadow:WideString  readonly dispid 10016;
    // appColorButtonHoverFace :  
   property appColorButtonHoverFace:WideString  readonly dispid 10017;
  end;


// IWMPSubscriptionServiceLimited : IWMPSubscriptionServiceLimited: Public interface for scripting object model.

 IWMPSubscriptionServiceLimited = interface(IWMPExternalColors)
   ['{54DF358E-CF38-4010-99F1-F44B0E9000E5}']
    // NavigateTaskPaneURL :  
   procedure NavigateTaskPaneURL(bstrKeyName:WideString;bstrTaskPane:WideString;bstrParams:WideString);safecall;
   procedure Set_SelectedTaskPane(const bstrTaskPane:WideString); safecall;
   function Get_SelectedTaskPane : WideString; safecall;
    // SelectedTaskPane :  
   property SelectedTaskPane:WideString read Get_SelectedTaskPane write Set_SelectedTaskPane;
  end;


// IWMPSubscriptionServiceLimited : IWMPSubscriptionServiceLimited: Public interface for scripting object model.

 IWMPSubscriptionServiceLimitedDisp = dispinterface
   ['{54DF358E-CF38-4010-99F1-F44B0E9000E5}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // NavigateTaskPaneURL :  
   procedure NavigateTaskPaneURL(bstrKeyName:WideString;bstrTaskPane:WideString;bstrParams:WideString);dispid 10026;
    // version :  
   property version:WideString  readonly dispid 10005;
    // appColorLight :  
   property appColorLight:WideString  readonly dispid 10012;
    // OnColorChange :  
   property OnColorChange:IDispatch writeonly dispid 10018;
    // appColorMedium :  
   property appColorMedium:WideString  readonly dispid 10013;
    // appColorDark :  
   property appColorDark:WideString  readonly dispid 10014;
    // appColorButtonHighlight :  
   property appColorButtonHighlight:WideString  readonly dispid 10015;
    // appColorButtonShadow :  
   property appColorButtonShadow:WideString  readonly dispid 10016;
    // appColorButtonHoverFace :  
   property appColorButtonHoverFace:WideString  readonly dispid 10017;
    // SelectedTaskPane :  
   property SelectedTaskPane:WideString dispid 10027;
  end;


// IWMPSubscriptionServiceExternal : IWMPSubscriptionServiceExternal: Public interface for scripting object model.

 IWMPSubscriptionServiceExternal = interface(IWMPSubscriptionServiceLimited)
   ['{2E922378-EE70-4CEB-BBAB-CE7CE4A04816}']
   function Get_DownloadManager : IWMPDownloadManager; safecall;
    // DownloadManager :  
   property DownloadManager:IWMPDownloadManager read Get_DownloadManager;
  end;


// IWMPSubscriptionServiceExternal : IWMPSubscriptionServiceExternal: Public interface for scripting object model.

 IWMPSubscriptionServiceExternalDisp = dispinterface
   ['{2E922378-EE70-4CEB-BBAB-CE7CE4A04816}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // NavigateTaskPaneURL :  
   procedure NavigateTaskPaneURL(bstrKeyName:WideString;bstrTaskPane:WideString;bstrParams:WideString);dispid 10026;
    // version :  
   property version:WideString  readonly dispid 10005;
    // appColorLight :  
   property appColorLight:WideString  readonly dispid 10012;
    // OnColorChange :  
   property OnColorChange:IDispatch writeonly dispid 10018;
    // appColorMedium :  
   property appColorMedium:WideString  readonly dispid 10013;
    // appColorDark :  
   property appColorDark:WideString  readonly dispid 10014;
    // appColorButtonHighlight :  
   property appColorButtonHighlight:WideString  readonly dispid 10015;
    // appColorButtonShadow :  
   property appColorButtonShadow:WideString  readonly dispid 10016;
    // appColorButtonHoverFace :  
   property appColorButtonHoverFace:WideString  readonly dispid 10017;
    // SelectedTaskPane :  
   property SelectedTaskPane:WideString dispid 10027;
    // DownloadManager :  
   property DownloadManager:IWMPDownloadManager  readonly dispid 10009;
  end;


// IWMPDownloadManager : IWMPDownloadManager: Public interface.

 IWMPDownloadManager = interface(IDispatch)
   ['{E15E9AD1-8F20-4CC4-9EC7-1A328CA86A0D}']
    // getDownloadCollection : Returns a specific download collection 
   function getDownloadCollection(lCollectionId:Integer):IWMPDownloadCollection;safecall;
    // createDownloadCollection : Creates a download collection 
   function createDownloadCollection:IWMPDownloadCollection;safecall;
  end;


// IWMPDownloadManager : IWMPDownloadManager: Public interface.

 IWMPDownloadManagerDisp = dispinterface
   ['{E15E9AD1-8F20-4CC4-9EC7-1A328CA86A0D}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // getDownloadCollection : Returns a specific download collection 
   function getDownloadCollection(lCollectionId:Integer):IWMPDownloadCollection;dispid 1151;
    // createDownloadCollection : Creates a download collection 
   function createDownloadCollection:IWMPDownloadCollection;dispid 1152;
  end;


// IWMPDownloadCollection : IWMPDownloadCollection: Public interface.

 IWMPDownloadCollection = interface(IDispatch)
   ['{0A319C7F-85F9-436C-B88E-82FD88000E1C}']
   function Get_ID : Integer; safecall;
   function Get_count : Integer; safecall;
    // Item : Returns a pending download object 
   function Item(lItem:Integer):IWMPDownloadItem2;safecall;
    // startDownload : Queues a download 
   function startDownload(bstrSourceURL:WideString;bstrType:WideString):IWMPDownloadItem2;safecall;
    // removeItem : Remove a download from the collection. Cancel if in progress. 
   procedure removeItem(lItem:Integer);safecall;
    // clear : Clear the download collection 
   procedure clear;safecall;
    // ID : Returns the unique identifier of the collection 
   property ID:Integer read Get_ID;
    // count : Returns the number of pending downloads 
   property count:Integer read Get_count;
  end;


// IWMPDownloadCollection : IWMPDownloadCollection: Public interface.

 IWMPDownloadCollectionDisp = dispinterface
   ['{0A319C7F-85F9-436C-B88E-82FD88000E1C}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // Item : Returns a pending download object 
   function Item(lItem:Integer):IWMPDownloadItem2;dispid 1203;
    // startDownload : Queues a download 
   function startDownload(bstrSourceURL:WideString;bstrType:WideString):IWMPDownloadItem2;dispid 1204;
    // removeItem : Remove a download from the collection. Cancel if in progress. 
   procedure removeItem(lItem:Integer);dispid 1205;
    // clear : Clear the download collection 
   procedure clear;dispid 1206;
    // ID : Returns the unique identifier of the collection 
   property ID:Integer  readonly dispid 1201;
    // count : Returns the number of pending downloads 
   property count:Integer  readonly dispid 1202;
  end;


// IWMPDownloadItem : IWMPDownloadItem: Public interface.

 IWMPDownloadItem = interface(IDispatch)
   ['{C9470E8E-3F6B-46A9-A0A9-452815C34297}']
   function Get_sourceURL : WideString; safecall;
   function Get_size : Integer; safecall;
   function Get_type_ : WideString; safecall;
   function Get_progress : Integer; safecall;
   function Get_downloadState : WMPSubscriptionDownloadState; safecall;
    // pause : Pauses the download 
   procedure pause;safecall;
    // resume : Resumes the download 
   procedure resume;safecall;
    // cancel : Cancels the download 
   procedure cancel;safecall;
    // sourceURL : Returns the source URL of the download 
   property sourceURL:WideString read Get_sourceURL;
    // size : Returns the size of the download 
   property size:Integer read Get_size;
    // type : Returns the type of the download 
   property type_:WideString read Get_type_;
    // progress : Returns the progress (in bytes) of the download 
   property progress:Integer read Get_progress;
    // downloadState : Returns the state of the download 
   property downloadState:WMPSubscriptionDownloadState read Get_downloadState;
  end;


// IWMPDownloadItem : IWMPDownloadItem: Public interface.

 IWMPDownloadItemDisp = dispinterface
   ['{C9470E8E-3F6B-46A9-A0A9-452815C34297}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // pause : Pauses the download 
   procedure pause;dispid 1256;
    // resume : Resumes the download 
   procedure resume;dispid 1257;
    // cancel : Cancels the download 
   procedure cancel;dispid 1258;
    // sourceURL : Returns the source URL of the download 
   property sourceURL:WideString  readonly dispid 1251;
    // size : Returns the size of the download 
   property size:Integer  readonly dispid 1252;
    // type : Returns the type of the download 
   property type_:WideString  readonly dispid 1253;
    // progress : Returns the progress (in bytes) of the download 
   property progress:Integer  readonly dispid 1254;
    // downloadState : Returns the state of the download 
   property downloadState:WMPSubscriptionDownloadState  readonly dispid 1255;
  end;


// IWMPDownloadItem2 : IWMPDownloadItem2: Public interface.

 IWMPDownloadItem2 = interface(IWMPDownloadItem)
   ['{9FBB3336-6DA3-479D-B8FF-67D46E20A987}']
    // getItemInfo : Returns the value of specified attribute for this download item 
   function getItemInfo(bstrItemName:WideString):WideString;safecall;
  end;

// IWMPDownloadItem2 : IWMPDownloadItem2: Public interface.

 IWMPDownloadItem2Disp = dispinterface
   ['{9FBB3336-6DA3-479D-B8FF-67D46E20A987}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // pause : Pauses the download 
   procedure pause;dispid 1256;
    // resume : Resumes the download 
   procedure resume;dispid 1257;
    // cancel : Cancels the download 
   procedure cancel;dispid 1258;
    // getItemInfo : Returns the value of specified attribute for this download item 
   function getItemInfo(bstrItemName:WideString):WideString;dispid 1301;
    // sourceURL : Returns the source URL of the download 
   property sourceURL:WideString  readonly dispid 1251;
    // size : Returns the size of the download 
   property size:Integer  readonly dispid 1252;
    // type : Returns the type of the download 
   property type_:WideString  readonly dispid 1253;
    // progress : Returns the progress (in bytes) of the download 
   property progress:Integer  readonly dispid 1254;
    // downloadState : Returns the state of the download 
   property downloadState:WMPSubscriptionDownloadState  readonly dispid 1255;
  end;


// IWMPSubscriptionServicePlayMedia : IWMPSubscriptionServicePlayMedia: Public interface for scripting object model.

 IWMPSubscriptionServicePlayMedia = interface(IWMPSubscriptionServiceLimited)
   ['{5F0248C1-62B3-42D7-B927-029119E6AD14}']
    // playMedia : method playMedia 
   procedure playMedia(bstrURL:WideString);safecall;
  end;


// IWMPSubscriptionServicePlayMedia : IWMPSubscriptionServicePlayMedia: Public interface for scripting object model.

 IWMPSubscriptionServicePlayMediaDisp = dispinterface
   ['{5F0248C1-62B3-42D7-B927-029119E6AD14}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // NavigateTaskPaneURL :  
   procedure NavigateTaskPaneURL(bstrKeyName:WideString;bstrTaskPane:WideString;bstrParams:WideString);dispid 10026;
    // playMedia : method playMedia 
   procedure playMedia(bstrURL:WideString);dispid 10004;
    // version :  
   property version:WideString  readonly dispid 10005;
    // appColorLight :  
   property appColorLight:WideString  readonly dispid 10012;
    // OnColorChange :  
   property OnColorChange:IDispatch writeonly dispid 10018;
    // appColorMedium :  
   property appColorMedium:WideString  readonly dispid 10013;
    // appColorDark :  
   property appColorDark:WideString  readonly dispid 10014;
    // appColorButtonHighlight :  
   property appColorButtonHighlight:WideString  readonly dispid 10015;
    // appColorButtonShadow :  
   property appColorButtonShadow:WideString  readonly dispid 10016;
    // appColorButtonHoverFace :  
   property appColorButtonHoverFace:WideString  readonly dispid 10017;
    // SelectedTaskPane :  
   property SelectedTaskPane:WideString dispid 10027;
  end;


// IWMPDiscoExternal : IWMPDiscoExternal: Public interface for scripting object model.

 IWMPDiscoExternal = interface(IWMPSubscriptionServiceExternal)
   ['{A915CEA2-72DF-41E1-A576-EF0BAE5E5169}']
   procedure Set_OnLoginChange(const Param1:IDispatch); safecall;
   function Get_userLoggedIn : WordBool; safecall;
    // attemptLogin :  
   procedure attemptLogin;safecall;
   function Get_accountType : WideString; safecall;
   procedure Set_OnViewChange(const Param1:IDispatch); safecall;
    // changeView :  
   procedure changeView(bstrLibraryLocationType:WideString;bstrLibraryLocationID:WideString;bstrFilter:WideString;bstrViewParams:WideString);safecall;
    // changeViewOnlineList :  
   procedure changeViewOnlineList(bstrLibraryLocationType:WideString;bstrLibraryLocationID:WideString;bstrParams:WideString;bstrFriendlyName:WideString;bstrListType:WideString;bstrViewMode:WideString);safecall;
   function Get_libraryLocationType : WideString; safecall;
   function Get_libraryLocationID : WideString; safecall;
   function Get_selectedItemType : WideString; safecall;
   function Get_selectedItemID : WideString; safecall;
   function Get_filter : WideString; safecall;
   function Get_task : WideString; safecall;
   function Get_viewParameters : WideString; safecall;
    // cancelNavigate :  
   procedure cancelNavigate;safecall;
    // showPopup :  
   procedure showPopup(lPopupIndex:Integer;bstrParameters:WideString);safecall;
    // addToBasket :  
   procedure addToBasket(bstrViewType:WideString;bstrViewIDs:WideString);safecall;
   function Get_basketTitle : WideString; safecall;
    // play :  
   procedure play(bstrLibraryLocationType:WideString;bstrLibraryLocationIDs:WideString);safecall;
    // download :  
   procedure download(bstrViewType:WideString;bstrViewIDs:WideString);safecall;
    // buy :  
   procedure buy(bstrViewType:WideString;bstrViewIDs:WideString);safecall;
    // saveCurrentViewToLibrary :  
   procedure saveCurrentViewToLibrary(bstrFriendlyListType:WideString;fDynamic:WordBool);safecall;
    // authenticate :  
   procedure authenticate(lAuthenticationIndex:Integer);safecall;
    // sendMessage :  
   procedure sendMessage(bstrMsg:WideString;bstrParam:WideString);safecall;
   procedure Set_OnSendMessageComplete(const Param1:IDispatch); safecall;
   procedure Set_ignoreIEHistory(const Param1:WordBool); safecall;
   function Get_pluginRunning : WordBool; safecall;
   function Get_templateBeingDisplayedInLocalLibrary : WordBool; safecall;
   procedure Set_OnChangeViewError(const Param1:IDispatch); safecall;
   procedure Set_OnChangeViewOnlineListError(const Param1:IDispatch); safecall;
    // OnLoginChange :  
   property OnLoginChange:IDispatch write Set_OnLoginChange;
    // userLoggedIn :  
   property userLoggedIn:WordBool read Get_userLoggedIn;
    // accountType :  
   property accountType:WideString read Get_accountType;
    // OnViewChange :  
   property OnViewChange:IDispatch write Set_OnViewChange;
    // libraryLocationType :  
   property libraryLocationType:WideString read Get_libraryLocationType;
    // libraryLocationID :  
   property libraryLocationID:WideString read Get_libraryLocationID;
    // selectedItemType :  
   property selectedItemType:WideString read Get_selectedItemType;
    // selectedItemID :  
   property selectedItemID:WideString read Get_selectedItemID;
    // filter :  
   property filter:WideString read Get_filter;
    // task :  
   property task:WideString read Get_task;
    // viewParameters :  
   property viewParameters:WideString read Get_viewParameters;
    // basketTitle :  
   property basketTitle:WideString read Get_basketTitle;
    // OnSendMessageComplete :  
   property OnSendMessageComplete:IDispatch write Set_OnSendMessageComplete;
    // ignoreIEHistory :  
   property ignoreIEHistory:WordBool write Set_ignoreIEHistory;
    // pluginRunning :  
   property pluginRunning:WordBool read Get_pluginRunning;
    // templateBeingDisplayedInLocalLibrary :  
   property templateBeingDisplayedInLocalLibrary:WordBool read Get_templateBeingDisplayedInLocalLibrary;
    // OnChangeViewError :  
   property OnChangeViewError:IDispatch write Set_OnChangeViewError;
    // OnChangeViewOnlineListError :  
   property OnChangeViewOnlineListError:IDispatch write Set_OnChangeViewOnlineListError;
  end;


// IWMPDiscoExternal : IWMPDiscoExternal: Public interface for scripting object model.

 IWMPDiscoExternalDisp = dispinterface
   ['{A915CEA2-72DF-41E1-A576-EF0BAE5E5169}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // NavigateTaskPaneURL :  
   procedure NavigateTaskPaneURL(bstrKeyName:WideString;bstrTaskPane:WideString;bstrParams:WideString);dispid 10026;
    // attemptLogin :  
   procedure attemptLogin;dispid 10030;
    // changeView :  
   procedure changeView(bstrLibraryLocationType:WideString;bstrLibraryLocationID:WideString;bstrFilter:WideString;bstrViewParams:WideString);dispid 10033;
    // changeViewOnlineList :  
   procedure changeViewOnlineList(bstrLibraryLocationType:WideString;bstrLibraryLocationID:WideString;bstrParams:WideString;bstrFriendlyName:WideString;bstrListType:WideString;bstrViewMode:WideString);dispid 10034;
    // cancelNavigate :  
   procedure cancelNavigate;dispid 10042;
    // showPopup :  
   procedure showPopup(lPopupIndex:Integer;bstrParameters:WideString);dispid 10043;
    // addToBasket :  
   procedure addToBasket(bstrViewType:WideString;bstrViewIDs:WideString);dispid 10044;
    // play :  
   procedure play(bstrLibraryLocationType:WideString;bstrLibraryLocationIDs:WideString);dispid 10046;
    // download :  
   procedure download(bstrViewType:WideString;bstrViewIDs:WideString);dispid 10047;
    // buy :  
   procedure buy(bstrViewType:WideString;bstrViewIDs:WideString);dispid 10048;
    // saveCurrentViewToLibrary :  
   procedure saveCurrentViewToLibrary(bstrFriendlyListType:WideString;fDynamic:WordBool);dispid 10049;
    // authenticate :  
   procedure authenticate(lAuthenticationIndex:Integer);dispid 10050;
    // sendMessage :  
   procedure sendMessage(bstrMsg:WideString;bstrParam:WideString);dispid 10051;
    // version :  
   property version:WideString  readonly dispid 10005;
    // appColorLight :  
   property appColorLight:WideString  readonly dispid 10012;
    // OnColorChange :  
   property OnColorChange:IDispatch writeonly dispid 10018;
    // appColorMedium :  
   property appColorMedium:WideString  readonly dispid 10013;
    // appColorDark :  
   property appColorDark:WideString  readonly dispid 10014;
    // appColorButtonHighlight :  
   property appColorButtonHighlight:WideString  readonly dispid 10015;
    // appColorButtonShadow :  
   property appColorButtonShadow:WideString  readonly dispid 10016;
    // appColorButtonHoverFace :  
   property appColorButtonHoverFace:WideString  readonly dispid 10017;
    // SelectedTaskPane :  
   property SelectedTaskPane:WideString dispid 10027;
    // DownloadManager :  
   property DownloadManager:IWMPDownloadManager  readonly dispid 10009;
    // OnLoginChange :  
   property OnLoginChange:IDispatch writeonly dispid 10028;
    // userLoggedIn :  
   property userLoggedIn:WordBool  readonly dispid 10029;
    // accountType :  
   property accountType:WideString  readonly dispid 10031;
    // OnViewChange :  
   property OnViewChange:IDispatch writeonly dispid 10032;
    // libraryLocationType :  
   property libraryLocationType:WideString  readonly dispid 10035;
    // libraryLocationID :  
   property libraryLocationID:WideString  readonly dispid 10036;
    // selectedItemType :  
   property selectedItemType:WideString  readonly dispid 10037;
    // selectedItemID :  
   property selectedItemID:WideString  readonly dispid 10038;
    // filter :  
   property filter:WideString  readonly dispid 10039;
    // task :  
   property task:WideString  readonly dispid 10040;
    // viewParameters :  
   property viewParameters:WideString  readonly dispid 10041;
    // basketTitle :  
   property basketTitle:WideString  readonly dispid 10045;
    // OnSendMessageComplete :  
   property OnSendMessageComplete:IDispatch writeonly dispid 10052;
    // ignoreIEHistory :  
   property ignoreIEHistory:WordBool writeonly dispid 10053;
    // pluginRunning :  
   property pluginRunning:WordBool  readonly dispid 10054;
    // templateBeingDisplayedInLocalLibrary :  
   property templateBeingDisplayedInLocalLibrary:WordBool  readonly dispid 10055;
    // OnChangeViewError :  
   property OnChangeViewError:IDispatch writeonly dispid 10056;
    // OnChangeViewOnlineListError :  
   property OnChangeViewOnlineListError:IDispatch writeonly dispid 10057;
  end;


// IWMPCDDVDWizardExternal : IWMPCDDVDWizardExternal: Not Public.  Internal interface used by Windows Media Player.

 IWMPCDDVDWizardExternal = interface(IWMPExternalColors)
   ['{2D7EF888-1D3C-484A-A906-9F49D99BB344}']
    // WriteNames :  
   procedure WriteNames(bstrTOC:WideString;bstrMetadata:WideString);safecall;
    // ReturnToMainTask :  
   procedure ReturnToMainTask;safecall;
    // WriteNamesEx :  
   procedure WriteNamesEx(type_:WMP_WRITENAMESEX_TYPE;bstrTypeId:WideString;bstrMetadata:WideString;fRenameRegroupFiles:WordBool);safecall;
    // GetMDQByRequestID :  
   function GetMDQByRequestID(bstrRequestID:WideString):WideString;safecall;
    // EditMetadata :  
   procedure EditMetadata;safecall;
    // IsMetadataAvailableForEdit :  
   function IsMetadataAvailableForEdit:WordBool;safecall;
    // BuyCD :  
   procedure BuyCD(bstrTitle:WideString;bstrArtist:WideString;bstrAlbum:WideString;bstrUFID:WideString;bstrWMID:WideString);safecall;
  end;


// IWMPCDDVDWizardExternal : IWMPCDDVDWizardExternal: Not Public.  Internal interface used by Windows Media Player.

 IWMPCDDVDWizardExternalDisp = dispinterface
   ['{2D7EF888-1D3C-484A-A906-9F49D99BB344}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // WriteNames :  
   procedure WriteNames(bstrTOC:WideString;bstrMetadata:WideString);dispid 10001;
    // ReturnToMainTask :  
   procedure ReturnToMainTask;dispid 10002;
    // WriteNamesEx :  
   procedure WriteNamesEx(type_:WMP_WRITENAMESEX_TYPE;bstrTypeId:WideString;bstrMetadata:WideString;fRenameRegroupFiles:WordBool);dispid 10007;
    // GetMDQByRequestID :  
   function GetMDQByRequestID(bstrRequestID:WideString):WideString;dispid 10008;
    // EditMetadata :  
   procedure EditMetadata;dispid 10011;
    // IsMetadataAvailableForEdit :  
   function IsMetadataAvailableForEdit:WordBool;dispid 10010;
    // BuyCD :  
   procedure BuyCD(bstrTitle:WideString;bstrArtist:WideString;bstrAlbum:WideString;bstrUFID:WideString;bstrWMID:WideString);dispid 10023;
    // version :  
   property version:WideString  readonly dispid 10005;
    // appColorLight :  
   property appColorLight:WideString  readonly dispid 10012;
    // OnColorChange :  
   property OnColorChange:IDispatch writeonly dispid 10018;
    // appColorMedium :  
   property appColorMedium:WideString  readonly dispid 10013;
    // appColorDark :  
   property appColorDark:WideString  readonly dispid 10014;
    // appColorButtonHighlight :  
   property appColorButtonHighlight:WideString  readonly dispid 10015;
    // appColorButtonShadow :  
   property appColorButtonShadow:WideString  readonly dispid 10016;
    // appColorButtonHoverFace :  
   property appColorButtonHoverFace:WideString  readonly dispid 10017;
  end;


// IWMPBaseExternal : IWMPBaseExternal: Public interface for scripting object model.

 IWMPBaseExternal = interface(IWMPExternal)
   ['{F81B2A59-02BC-4003-8B2F-C124AF66FC66}']
  end;


// IWMPBaseExternal : IWMPBaseExternal: Public interface for scripting object model.

 IWMPBaseExternalDisp = dispinterface
   ['{F81B2A59-02BC-4003-8B2F-C124AF66FC66}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // version :  
   property version:WideString  readonly dispid 10005;
    // appColorLight :  
   property appColorLight:WideString  readonly dispid 10012;
    // OnColorChange :  
   property OnColorChange:IDispatch writeonly dispid 10018;
  end;


// IWMPOfflineExternal : IWMPOfflineExternal: Not Public.  Internal interface used by Windows Media Player..

 IWMPOfflineExternal = interface(IWMPExternal)
   ['{3148E685-B243-423D-8341-8480D6EFF674}']
    // forceOnline :  
   procedure forceOnline;safecall;
  end;


// IWMPOfflineExternal : IWMPOfflineExternal: Not Public.  Internal interface used by Windows Media Player..

 IWMPOfflineExternalDisp = dispinterface
   ['{3148E685-B243-423D-8341-8480D6EFF674}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // forceOnline :  
   procedure forceOnline;dispid 10025;
    // version :  
   property version:WideString  readonly dispid 10005;
    // appColorLight :  
   property appColorLight:WideString  readonly dispid 10012;
    // OnColorChange :  
   property OnColorChange:IDispatch writeonly dispid 10018;
  end;


// IWMPDMRAVTransportService : IWMPDMRAVTransportService Interface

 IWMPDMRAVTransportService = interface(IDispatch)
   ['{4E195DB1-9E29-47FC-9CE1-DE9937D32925}']
   function Get_TransportState : WideString; safecall;
   function Get_TransportStatus : WideString; safecall;
   function Get_PlaybackStorageMedium : WideString; safecall;
   function Get_RecordStorageMedium : WideString; safecall;
   function Get_PossiblePlaybackStorageMedia : WideString; safecall;
   function Get_PossibleRecordStorageMedia : WideString; safecall;
   function Get_CurrentPlayMode : WideString; safecall;
   function Get_TransportPlaySpeed : WideString; safecall;
   function Get_RecordMediumWriteStatus : WideString; safecall;
   function Get_CurrentRecordQualityMode : WideString; safecall;
   function Get_PossibleRecordQualityModes : WideString; safecall;
   function Get_NumberOfTracks : LongWord; safecall;
   function Get_CurrentTrack : LongWord; safecall;
   function Get_CurrentTrackDuration : WideString; safecall;
   function Get_CurrentMediaDuration : WideString; safecall;
   function Get_CurrentTrackMetaData : WideString; safecall;
   function Get_CurrentTrackURI : WideString; safecall;
   function Get_AVTransportURI : WideString; safecall;
   function Get_AVTransportURIMetaData : WideString; safecall;
   function Get_NextAVTransportURI : WideString; safecall;
   function Get_NextAVTransportURIMetaData : WideString; safecall;
   function Get_RelativeTimePosition : WideString; safecall;
   function Get_AbsoluteTimePosition : WideString; safecall;
   function Get_RelativeCounterPosition : Integer; safecall;
   function Get_AbsoluteCounterPosition : Integer; safecall;
   function Get_CurrentTransportActions : WideString; safecall;
   function Get_LastChange : WideString; safecall;
   function Get_A_ARG_TYPE_SeekMode : WideString; safecall;
   function Get_A_ARG_TYPE_SeekTarget : WideString; safecall;
   function Get_A_ARG_TYPE_InstanceID : LongWord; safecall;
   function Get_CurrentProtocolInfo : WideString; safecall;
    // SetAVTransportURI : Method SetAVTransportURI 
   procedure SetAVTransportURI(punkRemoteEndpointInfo:IUnknown;ulInstanceID:LongWord;bstrCurrentURI:WideString;bstrCurrentURIMetaData:WideString);safecall;
    // GetMediaInfo : Method GetMediaInfo 
   procedure GetMediaInfo(ulInstanceID:LongWord;out pulNumTracks:LongWord;out pbstrMediaDuration:WideString;out pbstrCurrentURI:WideString;out pbstrCurrentURIMetaData:WideString;out pbstrNextURI:WideString;out pNextURIMetaData:WideString;out pbstrPlayMedium:WideString;out pbstrRecordMedium:WideString;out pbstrWriteStatus:WideString);safecall;
    // GetTransportInfo : Method GetTransportInfo 
   procedure GetTransportInfo(ulInstanceID:LongWord;out pbstrCurrentTransportState:WideString;out pbstrCurrentTransportStatus:WideString;out pbstrCurrentSpeed:WideString);safecall;
    // GetPositionInfo : Method GetPositionInfo 
   procedure GetPositionInfo(ulInstanceID:LongWord;out pTrack:LongWord;out pbstrTrackDuration:WideString;out pbstrTrackMetaData:WideString;out pbstrTrackURI:WideString;out pbstrRelTime:WideString;out pbstrAbsTime:WideString;out plRelCount:Integer;out plAbsCount:Integer);safecall;
    // GetDeviceCapabilities : Method GetDeviceCapabilities 
   procedure GetDeviceCapabilities(ulInstanceID:LongWord;out pbstrPlayMedia:WideString;out pbstrRecMedia:WideString;out pbstrRecQualityModes:WideString);safecall;
    // GetTransportSettings : Method GetTransportSettings 
   procedure GetTransportSettings(ulInstanceID:LongWord;out pbstrPlayMode:WideString;out pbstrRecQualityMode:WideString);safecall;
    // stop : Method Stop 
   procedure stop(ulInstanceID:LongWord);safecall;
    // play : Method Play 
   procedure play(ulInstanceID:LongWord;bstrSpeed:WideString);safecall;
    // pause : Method Pause 
   procedure pause(ulInstanceID:LongWord);safecall;
    // Seek : Method Seek 
   procedure Seek(ulInstanceID:LongWord;bstrUnit:WideString;bstrTarget:WideString);safecall;
    // next : Method Next 
   procedure next(ulInstanceID:LongWord);safecall;
    // previous : Method Previous 
   procedure previous(ulInstanceID:LongWord);safecall;
    // GetCurrentTransportActions : Method GetCurrentTransportActions 
   procedure GetCurrentTransportActions(ulInstanceID:LongWord;var pbstrActions:WideString);safecall;
    // SetNextAVTransportURI : Method SetNextAVTransportURI 
   procedure SetNextAVTransportURI(punkRemoteEndpointInfo:IUnknown;ulInstanceID:LongWord;bstrNextURI:WideString;bstrNextURIMetaData:WideString);safecall;
    // TransportState : Property TransportState 
   property TransportState:WideString read Get_TransportState;
    // TransportStatus : Property TransportStatus 
   property TransportStatus:WideString read Get_TransportStatus;
    // PlaybackStorageMedium : Property PlaybackStorageMedium 
   property PlaybackStorageMedium:WideString read Get_PlaybackStorageMedium;
    // RecordStorageMedium : Property RecordStorageMedium 
   property RecordStorageMedium:WideString read Get_RecordStorageMedium;
    // PossiblePlaybackStorageMedia : Property PossiblePlaybackStorageMedia 
   property PossiblePlaybackStorageMedia:WideString read Get_PossiblePlaybackStorageMedia;
    // PossibleRecordStorageMedia : Property PossibleRecordStorageMedia 
   property PossibleRecordStorageMedia:WideString read Get_PossibleRecordStorageMedia;
    // CurrentPlayMode : Property CurrentPlayMode 
   property CurrentPlayMode:WideString read Get_CurrentPlayMode;
    // TransportPlaySpeed : Property TransportPlaySpeed 
   property TransportPlaySpeed:WideString read Get_TransportPlaySpeed;
    // RecordMediumWriteStatus : Property RecordMediumWriteStatus 
   property RecordMediumWriteStatus:WideString read Get_RecordMediumWriteStatus;
    // CurrentRecordQualityMode : Property CurrentRecordQualityMode 
   property CurrentRecordQualityMode:WideString read Get_CurrentRecordQualityMode;
    // PossibleRecordQualityModes : Property PossibleRecordQualityModes 
   property PossibleRecordQualityModes:WideString read Get_PossibleRecordQualityModes;
    // NumberOfTracks : Property NumberOfTracks 
   property NumberOfTracks:LongWord read Get_NumberOfTracks;
    // CurrentTrack : Property CurrentTrack 
   property CurrentTrack:LongWord read Get_CurrentTrack;
    // CurrentTrackDuration : Property CurrentTrackDuration 
   property CurrentTrackDuration:WideString read Get_CurrentTrackDuration;
    // CurrentMediaDuration : Property CurrentMediaDuration 
   property CurrentMediaDuration:WideString read Get_CurrentMediaDuration;
    // CurrentTrackMetaData : Property CurrentTrackMetaData 
   property CurrentTrackMetaData:WideString read Get_CurrentTrackMetaData;
    // CurrentTrackURI : Property CurrentTrackURI 
   property CurrentTrackURI:WideString read Get_CurrentTrackURI;
    // AVTransportURI : Property AVTransportURI 
   property AVTransportURI:WideString read Get_AVTransportURI;
    // AVTransportURIMetaData : Property AVTransportURIMetaData 
   property AVTransportURIMetaData:WideString read Get_AVTransportURIMetaData;
    // NextAVTransportURI : Property NextAVTransportURI 
   property NextAVTransportURI:WideString read Get_NextAVTransportURI;
    // NextAVTransportURIMetaData : Property NextAVTransportURIMetaData 
   property NextAVTransportURIMetaData:WideString read Get_NextAVTransportURIMetaData;
    // RelativeTimePosition : Property RelativeTimePosition 
   property RelativeTimePosition:WideString read Get_RelativeTimePosition;
    // AbsoluteTimePosition : Property AbsoluteTimePosition 
   property AbsoluteTimePosition:WideString read Get_AbsoluteTimePosition;
    // RelativeCounterPosition : Property RelativeCounterPosition 
   property RelativeCounterPosition:Integer read Get_RelativeCounterPosition;
    // AbsoluteCounterPosition : Property AbsoluteCounterPosition 
   property AbsoluteCounterPosition:Integer read Get_AbsoluteCounterPosition;
    // CurrentTransportActions : Property CurrentTransportActions 
   property CurrentTransportActions:WideString read Get_CurrentTransportActions;
    // LastChange : Property LastChange 
   property LastChange:WideString read Get_LastChange;
    // A_ARG_TYPE_SeekMode : Property A_ARG_TYPE_SeekMode 
   property A_ARG_TYPE_SeekMode:WideString read Get_A_ARG_TYPE_SeekMode;
    // A_ARG_TYPE_SeekTarget : Property A_ARG_TYPE_SeekTarget 
   property A_ARG_TYPE_SeekTarget:WideString read Get_A_ARG_TYPE_SeekTarget;
    // A_ARG_TYPE_InstanceID : Property A_ARG_TYPE_InstanceID 
   property A_ARG_TYPE_InstanceID:LongWord read Get_A_ARG_TYPE_InstanceID;
    // CurrentProtocolInfo : Property CurrentProtocolInfo 
   property CurrentProtocolInfo:WideString read Get_CurrentProtocolInfo;
  end;


// IWMPDMRAVTransportService : IWMPDMRAVTransportService Interface

 IWMPDMRAVTransportServiceDisp = dispinterface
   ['{4E195DB1-9E29-47FC-9CE1-DE9937D32925}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // SetAVTransportURI : Method SetAVTransportURI 
   procedure SetAVTransportURI(punkRemoteEndpointInfo:IUnknown;ulInstanceID:LongWord;bstrCurrentURI:WideString;bstrCurrentURIMetaData:WideString);dispid 31;
    // GetMediaInfo : Method GetMediaInfo 
   procedure GetMediaInfo(ulInstanceID:LongWord;out pulNumTracks:LongWord;out pbstrMediaDuration:WideString;out pbstrCurrentURI:WideString;out pbstrCurrentURIMetaData:WideString;out pbstrNextURI:WideString;out pNextURIMetaData:WideString;out pbstrPlayMedium:WideString;out pbstrRecordMedium:WideString;out pbstrWriteStatus:WideString);dispid 33;
    // GetTransportInfo : Method GetTransportInfo 
   procedure GetTransportInfo(ulInstanceID:LongWord;out pbstrCurrentTransportState:WideString;out pbstrCurrentTransportStatus:WideString;out pbstrCurrentSpeed:WideString);dispid 34;
    // GetPositionInfo : Method GetPositionInfo 
   procedure GetPositionInfo(ulInstanceID:LongWord;out pTrack:LongWord;out pbstrTrackDuration:WideString;out pbstrTrackMetaData:WideString;out pbstrTrackURI:WideString;out pbstrRelTime:WideString;out pbstrAbsTime:WideString;out plRelCount:Integer;out plAbsCount:Integer);dispid 35;
    // GetDeviceCapabilities : Method GetDeviceCapabilities 
   procedure GetDeviceCapabilities(ulInstanceID:LongWord;out pbstrPlayMedia:WideString;out pbstrRecMedia:WideString;out pbstrRecQualityModes:WideString);dispid 36;
    // GetTransportSettings : Method GetTransportSettings 
   procedure GetTransportSettings(ulInstanceID:LongWord;out pbstrPlayMode:WideString;out pbstrRecQualityMode:WideString);dispid 37;
    // stop : Method Stop 
   procedure stop(ulInstanceID:LongWord);dispid 38;
    // play : Method Play 
   procedure play(ulInstanceID:LongWord;bstrSpeed:WideString);dispid 39;
    // pause : Method Pause 
   procedure pause(ulInstanceID:LongWord);dispid 40;
    // Seek : Method Seek 
   procedure Seek(ulInstanceID:LongWord;bstrUnit:WideString;bstrTarget:WideString);dispid 41;
    // next : Method Next 
   procedure next(ulInstanceID:LongWord);dispid 42;
    // previous : Method Previous 
   procedure previous(ulInstanceID:LongWord);dispid 43;
    // GetCurrentTransportActions : Method GetCurrentTransportActions 
   procedure GetCurrentTransportActions(ulInstanceID:LongWord;var pbstrActions:WideString);dispid 44;
    // SetNextAVTransportURI : Method SetNextAVTransportURI 
   procedure SetNextAVTransportURI(punkRemoteEndpointInfo:IUnknown;ulInstanceID:LongWord;bstrNextURI:WideString;bstrNextURIMetaData:WideString);dispid 32;
    // TransportState : Property TransportState 
   property TransportState:WideString  readonly dispid 1;
    // TransportStatus : Property TransportStatus 
   property TransportStatus:WideString  readonly dispid 2;
    // PlaybackStorageMedium : Property PlaybackStorageMedium 
   property PlaybackStorageMedium:WideString  readonly dispid 3;
    // RecordStorageMedium : Property RecordStorageMedium 
   property RecordStorageMedium:WideString  readonly dispid 4;
    // PossiblePlaybackStorageMedia : Property PossiblePlaybackStorageMedia 
   property PossiblePlaybackStorageMedia:WideString  readonly dispid 5;
    // PossibleRecordStorageMedia : Property PossibleRecordStorageMedia 
   property PossibleRecordStorageMedia:WideString  readonly dispid 6;
    // CurrentPlayMode : Property CurrentPlayMode 
   property CurrentPlayMode:WideString  readonly dispid 7;
    // TransportPlaySpeed : Property TransportPlaySpeed 
   property TransportPlaySpeed:WideString  readonly dispid 8;
    // RecordMediumWriteStatus : Property RecordMediumWriteStatus 
   property RecordMediumWriteStatus:WideString  readonly dispid 9;
    // CurrentRecordQualityMode : Property CurrentRecordQualityMode 
   property CurrentRecordQualityMode:WideString  readonly dispid 10;
    // PossibleRecordQualityModes : Property PossibleRecordQualityModes 
   property PossibleRecordQualityModes:WideString  readonly dispid 11;
    // NumberOfTracks : Property NumberOfTracks 
   property NumberOfTracks:LongWord  readonly dispid 12;
    // CurrentTrack : Property CurrentTrack 
   property CurrentTrack:LongWord  readonly dispid 13;
    // CurrentTrackDuration : Property CurrentTrackDuration 
   property CurrentTrackDuration:WideString  readonly dispid 14;
    // CurrentMediaDuration : Property CurrentMediaDuration 
   property CurrentMediaDuration:WideString  readonly dispid 15;
    // CurrentTrackMetaData : Property CurrentTrackMetaData 
   property CurrentTrackMetaData:WideString  readonly dispid 16;
    // CurrentTrackURI : Property CurrentTrackURI 
   property CurrentTrackURI:WideString  readonly dispid 17;
    // AVTransportURI : Property AVTransportURI 
   property AVTransportURI:WideString  readonly dispid 18;
    // AVTransportURIMetaData : Property AVTransportURIMetaData 
   property AVTransportURIMetaData:WideString  readonly dispid 19;
    // NextAVTransportURI : Property NextAVTransportURI 
   property NextAVTransportURI:WideString  readonly dispid 20;
    // NextAVTransportURIMetaData : Property NextAVTransportURIMetaData 
   property NextAVTransportURIMetaData:WideString  readonly dispid 21;
    // RelativeTimePosition : Property RelativeTimePosition 
   property RelativeTimePosition:WideString  readonly dispid 22;
    // AbsoluteTimePosition : Property AbsoluteTimePosition 
   property AbsoluteTimePosition:WideString  readonly dispid 23;
    // RelativeCounterPosition : Property RelativeCounterPosition 
   property RelativeCounterPosition:Integer  readonly dispid 24;
    // AbsoluteCounterPosition : Property AbsoluteCounterPosition 
   property AbsoluteCounterPosition:Integer  readonly dispid 25;
    // CurrentTransportActions : Property CurrentTransportActions 
   property CurrentTransportActions:WideString  readonly dispid 26;
    // LastChange : Property LastChange 
   property LastChange:WideString  readonly dispid 27;
    // A_ARG_TYPE_SeekMode : Property A_ARG_TYPE_SeekMode 
   property A_ARG_TYPE_SeekMode:WideString  readonly dispid 28;
    // A_ARG_TYPE_SeekTarget : Property A_ARG_TYPE_SeekTarget 
   property A_ARG_TYPE_SeekTarget:WideString  readonly dispid 29;
    // A_ARG_TYPE_InstanceID : Property A_ARG_TYPE_InstanceID 
   property A_ARG_TYPE_InstanceID:LongWord  readonly dispid 30;
    // CurrentProtocolInfo : Property CurrentProtocolInfo 
   property CurrentProtocolInfo:WideString  readonly dispid 45;
  end;


// IWMPDMRConnectionManagerService : 

 IWMPDMRConnectionManagerService = interface(IDispatch)
   ['{FB61CD38-8DE7-4479-8B76-A8D097C20C70}']
   function Get_SourceProtocolInfo : WideString; safecall;
   function Get_SinkProtocolInfo : WideString; safecall;
   function Get_CurrentConnectionIDs : WideString; safecall;
   function Get_A_ARG_TYPE_ConnectionStatus : WideString; safecall;
   function Get_A_ARG_TYPE_ConnectionManager : WideString; safecall;
   function Get_A_ARG_TYPE_Direction : WideString; safecall;
   function Get_A_ARG_TYPE_ProtocolInfo : WideString; safecall;
   function Get_A_ARG_TYPE_ConnectionID : Integer; safecall;
   function Get_A_ARG_TYPE_AVTransportID : Integer; safecall;
   function Get_A_ARG_TYPE_RcsID : Integer; safecall;
    // GetProtocolInfo : Method GetProtocolInfo 
   procedure GetProtocolInfo(var pbstrSource:WideString;var pbstrSink:WideString);safecall;
    // GetCurrentConnectionIDs : Method GetCurrentConnectionIDs 
   procedure GetCurrentConnectionIDs(var pbstrConnectionIDs:WideString);safecall;
    // GetCurrentConnectionInfo : Method GetCurrentConnectionInfo 
   procedure GetCurrentConnectionInfo(lConnectionID:Integer;var plResID:Integer;var plAVTransportID:Integer;var pbstrProtocolInfo:WideString;var pbstrPeerConnectionManager:WideString;var plPeerConnectionID:Integer;var pbstrDirection:WideString;var pbstrStatus:WideString);safecall;
    // SourceProtocolInfo : Property SourceProtocolInfo 
   property SourceProtocolInfo:WideString read Get_SourceProtocolInfo;
    // SinkProtocolInfo : Property SinkProtocolInfo 
   property SinkProtocolInfo:WideString read Get_SinkProtocolInfo;
    // CurrentConnectionIDs : Property CurrentConnectionIDs 
   property CurrentConnectionIDs:WideString read Get_CurrentConnectionIDs;
    // A_ARG_TYPE_ConnectionStatus : Property A_ARG_TYPE_ConnectionStatus 
   property A_ARG_TYPE_ConnectionStatus:WideString read Get_A_ARG_TYPE_ConnectionStatus;
    // A_ARG_TYPE_ConnectionManager : Property A_ARG_TYPE_ConnectionManager 
   property A_ARG_TYPE_ConnectionManager:WideString read Get_A_ARG_TYPE_ConnectionManager;
    // A_ARG_TYPE_Direction : Property A_ARG_TYPE_Direction 
   property A_ARG_TYPE_Direction:WideString read Get_A_ARG_TYPE_Direction;
    // A_ARG_TYPE_ProtocolInfo : Property A_ARG_TYPE_ProtocolInfo 
   property A_ARG_TYPE_ProtocolInfo:WideString read Get_A_ARG_TYPE_ProtocolInfo;
    // A_ARG_TYPE_ConnectionID : Property A_ARG_TYPE_ConnectionID 
   property A_ARG_TYPE_ConnectionID:Integer read Get_A_ARG_TYPE_ConnectionID;
    // A_ARG_TYPE_AVTransportID : Property A_ARG_TYPE_AVTransportID 
   property A_ARG_TYPE_AVTransportID:Integer read Get_A_ARG_TYPE_AVTransportID;
    // A_ARG_TYPE_RcsID : Property A_ARG_TYPE_RcsID 
   property A_ARG_TYPE_RcsID:Integer read Get_A_ARG_TYPE_RcsID;
  end;


// IWMPDMRConnectionManagerService : 

 IWMPDMRConnectionManagerServiceDisp = dispinterface
   ['{FB61CD38-8DE7-4479-8B76-A8D097C20C70}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // GetProtocolInfo : Method GetProtocolInfo 
   procedure GetProtocolInfo(var pbstrSource:WideString;var pbstrSink:WideString);dispid 11;
    // GetCurrentConnectionIDs : Method GetCurrentConnectionIDs 
   procedure GetCurrentConnectionIDs(var pbstrConnectionIDs:WideString);dispid 12;
    // GetCurrentConnectionInfo : Method GetCurrentConnectionInfo 
   procedure GetCurrentConnectionInfo(lConnectionID:Integer;var plResID:Integer;var plAVTransportID:Integer;var pbstrProtocolInfo:WideString;var pbstrPeerConnectionManager:WideString;var plPeerConnectionID:Integer;var pbstrDirection:WideString;var pbstrStatus:WideString);dispid 13;
    // SourceProtocolInfo : Property SourceProtocolInfo 
   property SourceProtocolInfo:WideString  readonly dispid 1;
    // SinkProtocolInfo : Property SinkProtocolInfo 
   property SinkProtocolInfo:WideString  readonly dispid 2;
    // CurrentConnectionIDs : Property CurrentConnectionIDs 
   property CurrentConnectionIDs:WideString  readonly dispid 3;
    // A_ARG_TYPE_ConnectionStatus : Property A_ARG_TYPE_ConnectionStatus 
   property A_ARG_TYPE_ConnectionStatus:WideString  readonly dispid 4;
    // A_ARG_TYPE_ConnectionManager : Property A_ARG_TYPE_ConnectionManager 
   property A_ARG_TYPE_ConnectionManager:WideString  readonly dispid 5;
    // A_ARG_TYPE_Direction : Property A_ARG_TYPE_Direction 
   property A_ARG_TYPE_Direction:WideString  readonly dispid 6;
    // A_ARG_TYPE_ProtocolInfo : Property A_ARG_TYPE_ProtocolInfo 
   property A_ARG_TYPE_ProtocolInfo:WideString  readonly dispid 7;
    // A_ARG_TYPE_ConnectionID : Property A_ARG_TYPE_ConnectionID 
   property A_ARG_TYPE_ConnectionID:Integer  readonly dispid 8;
    // A_ARG_TYPE_AVTransportID : Property A_ARG_TYPE_AVTransportID 
   property A_ARG_TYPE_AVTransportID:Integer  readonly dispid 9;
    // A_ARG_TYPE_RcsID : Property A_ARG_TYPE_RcsID 
   property A_ARG_TYPE_RcsID:Integer  readonly dispid 10;
  end;


// IWMPDMRRenderingControlService : IWMPDMRRenderingControlService Interface

 IWMPDMRRenderingControlService = interface(IDispatch)
   ['{FF4B1BDA-19F0-42CF-8DDA-19162950C543}']
   function Get_LastChange : WideString; safecall;
   function Get_PresetNameList : WideString; safecall;
   function Get_mute : WordBool; safecall;
   function Get_volume : Word; safecall;
   function Get_A_ARG_TYPE_Channel : WideString; safecall;
   function Get_A_ARG_TYPE_InstanceID : LongWord; safecall;
   function Get_A_ARG_TYPE_PresetName : WideString; safecall;
    // ListPresets : Method ListPresets 
   procedure ListPresets(ulInstanceID:LongWord;var pbstrCurrentPresetList:WideString);safecall;
    // SelectPreset : Method SelectPreset 
   procedure SelectPreset(ulInstanceID:LongWord;bstrPresetName:WideString);safecall;
    // GetMute : Method GetMute 
   procedure GetMute(ulInstanceID:LongWord;bstrChannel:WideString;var pbCurrentMute:WordBool);safecall;
    // SetMute : Method SetMute 
   procedure SetMute(ulInstanceID:LongWord;bstrChannel:WideString;bDesiredMute:WordBool);safecall;
    // GetVolume : Method GetVolume 
   procedure GetVolume(ulInstanceID:LongWord;bstrChannel:WideString;var puiCurrentVolume:Word);safecall;
    // SetVolume : Method SetVolume 
   procedure SetVolume(ulInstanceID:LongWord;bstrChannel:WideString;uiDesiredVolume:Word);safecall;
    // LastChange : Property LastChange 
   property LastChange:WideString read Get_LastChange;
    // PresetNameList : Property PresetNameList 
   property PresetNameList:WideString read Get_PresetNameList;
    // mute : Property Mute 
   property mute:WordBool read Get_mute;
    // volume : Property Volume 
   property volume:Word read Get_volume;
    // A_ARG_TYPE_Channel : Property A_ARG_TYPE_Channel 
   property A_ARG_TYPE_Channel:WideString read Get_A_ARG_TYPE_Channel;
    // A_ARG_TYPE_InstanceID : Property A_ARG_TYPE_InstanceID 
   property A_ARG_TYPE_InstanceID:LongWord read Get_A_ARG_TYPE_InstanceID;
    // A_ARG_TYPE_PresetName : Property A_ARG_TYPE_PresetName 
   property A_ARG_TYPE_PresetName:WideString read Get_A_ARG_TYPE_PresetName;
  end;


// IWMPDMRRenderingControlService : IWMPDMRRenderingControlService Interface

 IWMPDMRRenderingControlServiceDisp = dispinterface
   ['{FF4B1BDA-19F0-42CF-8DDA-19162950C543}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // ListPresets : Method ListPresets 
   procedure ListPresets(ulInstanceID:LongWord;var pbstrCurrentPresetList:WideString);dispid 8;
    // SelectPreset : Method SelectPreset 
   procedure SelectPreset(ulInstanceID:LongWord;bstrPresetName:WideString);dispid 9;
    // GetMute : Method GetMute 
   procedure GetMute(ulInstanceID:LongWord;bstrChannel:WideString;var pbCurrentMute:WordBool);dispid 10;
    // SetMute : Method SetMute 
   procedure SetMute(ulInstanceID:LongWord;bstrChannel:WideString;bDesiredMute:WordBool);dispid 11;
    // GetVolume : Method GetVolume 
   procedure GetVolume(ulInstanceID:LongWord;bstrChannel:WideString;var puiCurrentVolume:Word);dispid 12;
    // SetVolume : Method SetVolume 
   procedure SetVolume(ulInstanceID:LongWord;bstrChannel:WideString;uiDesiredVolume:Word);dispid 13;
    // LastChange : Property LastChange 
   property LastChange:WideString  readonly dispid 1;
    // PresetNameList : Property PresetNameList 
   property PresetNameList:WideString  readonly dispid 2;
    // mute : Property Mute 
   property mute:WordBool  readonly dispid 3;
    // volume : Property Volume 
   property volume:Word  readonly dispid 4;
    // A_ARG_TYPE_Channel : Property A_ARG_TYPE_Channel 
   property A_ARG_TYPE_Channel:WideString  readonly dispid 5;
    // A_ARG_TYPE_InstanceID : Property A_ARG_TYPE_InstanceID 
   property A_ARG_TYPE_InstanceID:LongWord  readonly dispid 6;
    // A_ARG_TYPE_PresetName : Property A_ARG_TYPE_PresetName 
   property A_ARG_TYPE_PresetName:WideString  readonly dispid 7;
  end;

//CoClasses
  T_WMPOCXEventsOpenStateChange = procedure(Sender: TObject;NewState:Integer) of object;
  T_WMPOCXEventsPlayStateChange = procedure(Sender: TObject;NewState:Integer) of object;
  T_WMPOCXEventsAudioLanguageChange = procedure(Sender: TObject;LangID:Integer) of object;
  T_WMPOCXEventsStatusChange = procedure(Sender: TObject) of object;
  T_WMPOCXEventsScriptCommand = procedure(Sender: TObject;scType:WideString;Param:WideString) of object;
  T_WMPOCXEventsNewStream = procedure(Sender: TObject) of object;
  T_WMPOCXEventsDisconnect = procedure(Sender: TObject;Result:Integer) of object;
  T_WMPOCXEventsBuffering = procedure(Sender: TObject;Start:WordBool) of object;
  T_WMPOCXEventsError = procedure(Sender: TObject) of object;
  T_WMPOCXEventsWarning = procedure(Sender: TObject;WarningType:Integer;Param:Integer;Description:WideString) of object;
  T_WMPOCXEventsEndOfStream = procedure(Sender: TObject;Result:Integer) of object;
  T_WMPOCXEventsPositionChange = procedure(Sender: TObject;oldPosition:Double;newPosition:Double) of object;
  T_WMPOCXEventsMarkerHit = procedure(Sender: TObject;MarkerNum:Integer) of object;
  T_WMPOCXEventsDurationUnitChange = procedure(Sender: TObject;NewDurationUnit:Integer) of object;
  T_WMPOCXEventsCdromMediaChange = procedure(Sender: TObject;CdromNum:Integer) of object;
  T_WMPOCXEventsPlaylistChange = procedure(Sender: TObject;Playlist:IDispatch;change:WMPPlaylistChangeEventType) of object;
  T_WMPOCXEventsCurrentPlaylistChange = procedure(Sender: TObject;change:WMPPlaylistChangeEventType) of object;
  T_WMPOCXEventsCurrentPlaylistItemAvailable = procedure(Sender: TObject;bstrItemName:WideString) of object;
  T_WMPOCXEventsMediaChange = procedure(Sender: TObject;Item:IDispatch) of object;
  T_WMPOCXEventsCurrentMediaItemAvailable = procedure(Sender: TObject;bstrItemName:WideString) of object;
  T_WMPOCXEventsCurrentItemChange = procedure(Sender: TObject;pdispMedia:IDispatch) of object;
  T_WMPOCXEventsMediaCollectionChange = procedure(Sender: TObject) of object;
  T_WMPOCXEventsMediaCollectionAttributeStringAdded = procedure(Sender: TObject;bstrAttribName:WideString;bstrAttribVal:WideString) of object;
  T_WMPOCXEventsMediaCollectionAttributeStringRemoved = procedure(Sender: TObject;bstrAttribName:WideString;bstrAttribVal:WideString) of object;
  T_WMPOCXEventsMediaCollectionAttributeStringChanged = procedure(Sender: TObject;bstrAttribName:WideString;bstrOldAttribVal:WideString;bstrNewAttribVal:WideString) of object;
  T_WMPOCXEventsPlaylistCollectionChange = procedure(Sender: TObject) of object;
  T_WMPOCXEventsPlaylistCollectionPlaylistAdded = procedure(Sender: TObject;bstrPlaylistName:WideString) of object;
  T_WMPOCXEventsPlaylistCollectionPlaylistRemoved = procedure(Sender: TObject;bstrPlaylistName:WideString) of object;
  T_WMPOCXEventsPlaylistCollectionPlaylistSetAsDeleted = procedure(Sender: TObject;bstrPlaylistName:WideString;varfIsDeleted:WordBool) of object;
  T_WMPOCXEventsModeChange = procedure(Sender: TObject;ModeName:WideString;NewValue:WordBool) of object;
  T_WMPOCXEventsMediaError = procedure(Sender: TObject;pMediaObject:IDispatch) of object;
  T_WMPOCXEventsOpenPlaylistSwitch = procedure(Sender: TObject;pItem:IDispatch) of object;
  T_WMPOCXEventsDomainChange = procedure(Sender: TObject;strDomain:WideString) of object;
  T_WMPOCXEventsSwitchedToPlayerApplication = procedure(Sender: TObject) of object;
  T_WMPOCXEventsSwitchedToControl = procedure(Sender: TObject) of object;
  T_WMPOCXEventsPlayerDockedStateChange = procedure(Sender: TObject) of object;
  T_WMPOCXEventsPlayerReconnect = procedure(Sender: TObject) of object;
  T_WMPOCXEventsClick = procedure(Sender: TObject;nButton:Smallint;nShiftState:Smallint;fX:Integer;fY:Integer) of object;
  T_WMPOCXEventsDoubleClick = procedure(Sender: TObject;nButton:Smallint;nShiftState:Smallint;fX:Integer;fY:Integer) of object;
  T_WMPOCXEventsKeyDown = procedure(Sender: TObject;nKeyCode:Smallint;nShiftState:Smallint) of object;
  T_WMPOCXEventsKeyPress = procedure(Sender: TObject;nKeyAscii:Smallint) of object;
  T_WMPOCXEventsKeyUp = procedure(Sender: TObject;nKeyCode:Smallint;nShiftState:Smallint) of object;
  T_WMPOCXEventsMouseDown = procedure(Sender: TObject;nButton:Smallint;nShiftState:Smallint;fX:Integer;fY:Integer) of object;
  T_WMPOCXEventsMouseMove = procedure(Sender: TObject;nButton:Smallint;nShiftState:Smallint;fX:Integer;fY:Integer) of object;
  T_WMPOCXEventsMouseUp = procedure(Sender: TObject;nButton:Smallint;nShiftState:Smallint;fX:Integer;fY:Integer) of object;
  T_WMPOCXEventsDeviceConnect = procedure(Sender: TObject;pDevice:IWMPSyncDevice) of object;
  T_WMPOCXEventsDeviceDisconnect = procedure(Sender: TObject;pDevice:IWMPSyncDevice) of object;
  T_WMPOCXEventsDeviceStatusChange = procedure(Sender: TObject;pDevice:IWMPSyncDevice;NewStatus:WMPDeviceStatus) of object;
  T_WMPOCXEventsDeviceSyncStateChange = procedure(Sender: TObject;pDevice:IWMPSyncDevice;NewState:WMPSyncState) of object;
  T_WMPOCXEventsDeviceSyncError = procedure(Sender: TObject;pDevice:IWMPSyncDevice;pMedia:IDispatch) of object;
  T_WMPOCXEventsCreatePartnershipComplete = procedure(Sender: TObject;pDevice:IWMPSyncDevice;hrResult:HResult) of object;
  T_WMPOCXEventsDeviceEstimation = procedure(Sender: TObject;pDevice:IWMPSyncDevice;hrResult:HResult;qwEstimatedUsedSpace:Int64;qwEstimatedSpace:Int64) of object;
  T_WMPOCXEventsCdromRipStateChange = procedure(Sender: TObject;pCdromRip:IWMPCdromRip;wmprs:WMPRipState) of object;
  T_WMPOCXEventsCdromRipMediaError = procedure(Sender: TObject;pCdromRip:IWMPCdromRip;pMedia:IDispatch) of object;
  T_WMPOCXEventsCdromBurnStateChange = procedure(Sender: TObject;pCdromBurn:IWMPCdromBurn;wmpbs:WMPBurnState) of object;
  T_WMPOCXEventsCdromBurnMediaError = procedure(Sender: TObject;pCdromBurn:IWMPCdromBurn;pMedia:IDispatch) of object;
  T_WMPOCXEventsCdromBurnError = procedure(Sender: TObject;pCdromBurn:IWMPCdromBurn;hrError:HResult) of object;
  T_WMPOCXEventsLibraryConnect = procedure(Sender: TObject;pLibrary:IWMPLibrary) of object;
  T_WMPOCXEventsLibraryDisconnect = procedure(Sender: TObject;pLibrary:IWMPLibrary) of object;
  T_WMPOCXEventsFolderScanStateChange = procedure(Sender: TObject;wmpfss:WMPFolderScanState) of object;
  T_WMPOCXEventsStringCollectionChange = procedure(Sender: TObject;pdispStringCollection:IDispatch;change:WMPStringCollectionChangeEventType;lCollectionIndex:Integer) of object;
  T_WMPOCXEventsMediaCollectionMediaAdded = procedure(Sender: TObject;pdispMedia:IDispatch) of object;
  T_WMPOCXEventsMediaCollectionMediaRemoved = procedure(Sender: TObject;pdispMedia:IDispatch) of object;


  CoWindowsMediaPlayer = Class
  Public
    Class Function Create: IWMPPlayer4;
    Class Function CreateRemote(const MachineName: string): IWMPPlayer4;
  end;

  TEvsWindowsMediaPlayer = Class(TEventSink)
  Private
    FOnOpenStateChange:T_WMPOCXEventsOpenStateChange;
    FOnPlayStateChange:T_WMPOCXEventsPlayStateChange;
    FOnAudioLanguageChange:T_WMPOCXEventsAudioLanguageChange;
    FOnStatusChange:T_WMPOCXEventsStatusChange;
    FOnScriptCommand:T_WMPOCXEventsScriptCommand;
    FOnNewStream:T_WMPOCXEventsNewStream;
    FOnDisconnect:T_WMPOCXEventsDisconnect;
    FOnBuffering:T_WMPOCXEventsBuffering;
    FOnError:T_WMPOCXEventsError;
    FOnWarning:T_WMPOCXEventsWarning;
    FOnEndOfStream:T_WMPOCXEventsEndOfStream;
    FOnPositionChange:T_WMPOCXEventsPositionChange;
    FOnMarkerHit:T_WMPOCXEventsMarkerHit;
    FOnDurationUnitChange:T_WMPOCXEventsDurationUnitChange;
    FOnCdromMediaChange:T_WMPOCXEventsCdromMediaChange;
    FOnPlaylistChange:T_WMPOCXEventsPlaylistChange;
    FOnCurrentPlaylistChange:T_WMPOCXEventsCurrentPlaylistChange;
    FOnCurrentPlaylistItemAvailable:T_WMPOCXEventsCurrentPlaylistItemAvailable;
    FOnMediaChange:T_WMPOCXEventsMediaChange;
    FOnCurrentMediaItemAvailable:T_WMPOCXEventsCurrentMediaItemAvailable;
    FOnCurrentItemChange:T_WMPOCXEventsCurrentItemChange;
    FOnMediaCollectionChange:T_WMPOCXEventsMediaCollectionChange;
    FOnMediaCollectionAttributeStringAdded:T_WMPOCXEventsMediaCollectionAttributeStringAdded;
    FOnMediaCollectionAttributeStringRemoved:T_WMPOCXEventsMediaCollectionAttributeStringRemoved;
    FOnMediaCollectionAttributeStringChanged:T_WMPOCXEventsMediaCollectionAttributeStringChanged;
    FOnPlaylistCollectionChange:T_WMPOCXEventsPlaylistCollectionChange;
    FOnPlaylistCollectionPlaylistAdded:T_WMPOCXEventsPlaylistCollectionPlaylistAdded;
    FOnPlaylistCollectionPlaylistRemoved:T_WMPOCXEventsPlaylistCollectionPlaylistRemoved;
    FOnPlaylistCollectionPlaylistSetAsDeleted:T_WMPOCXEventsPlaylistCollectionPlaylistSetAsDeleted;
    FOnModeChange:T_WMPOCXEventsModeChange;
    FOnMediaError:T_WMPOCXEventsMediaError;
    FOnOpenPlaylistSwitch:T_WMPOCXEventsOpenPlaylistSwitch;
    FOnDomainChange:T_WMPOCXEventsDomainChange;
    FOnSwitchedToPlayerApplication:T_WMPOCXEventsSwitchedToPlayerApplication;
    FOnSwitchedToControl:T_WMPOCXEventsSwitchedToControl;
    FOnPlayerDockedStateChange:T_WMPOCXEventsPlayerDockedStateChange;
    FOnPlayerReconnect:T_WMPOCXEventsPlayerReconnect;
    FOnClick:T_WMPOCXEventsClick;
    FOnDoubleClick:T_WMPOCXEventsDoubleClick;
    FOnKeyDown:T_WMPOCXEventsKeyDown;
    FOnKeyPress:T_WMPOCXEventsKeyPress;
    FOnKeyUp:T_WMPOCXEventsKeyUp;
    FOnMouseDown:T_WMPOCXEventsMouseDown;
    FOnMouseMove:T_WMPOCXEventsMouseMove;
    FOnMouseUp:T_WMPOCXEventsMouseUp;
    FOnDeviceConnect:T_WMPOCXEventsDeviceConnect;
    FOnDeviceDisconnect:T_WMPOCXEventsDeviceDisconnect;
    FOnDeviceStatusChange:T_WMPOCXEventsDeviceStatusChange;
    FOnDeviceSyncStateChange:T_WMPOCXEventsDeviceSyncStateChange;
    FOnDeviceSyncError:T_WMPOCXEventsDeviceSyncError;
    FOnCreatePartnershipComplete:T_WMPOCXEventsCreatePartnershipComplete;
    FOnDeviceEstimation:T_WMPOCXEventsDeviceEstimation;
    FOnCdromRipStateChange:T_WMPOCXEventsCdromRipStateChange;
    FOnCdromRipMediaError:T_WMPOCXEventsCdromRipMediaError;
    FOnCdromBurnStateChange:T_WMPOCXEventsCdromBurnStateChange;
    FOnCdromBurnMediaError:T_WMPOCXEventsCdromBurnMediaError;
    FOnCdromBurnError:T_WMPOCXEventsCdromBurnError;
    FOnLibraryConnect:T_WMPOCXEventsLibraryConnect;
    FOnLibraryDisconnect:T_WMPOCXEventsLibraryDisconnect;
    FOnFolderScanStateChange:T_WMPOCXEventsFolderScanStateChange;
    FOnStringCollectionChange:T_WMPOCXEventsStringCollectionChange;
    FOnMediaCollectionMediaAdded:T_WMPOCXEventsMediaCollectionMediaAdded;
    FOnMediaCollectionMediaRemoved:T_WMPOCXEventsMediaCollectionMediaRemoved;

    fServer:IWMPPlayer4;
    procedure EventSinkInvoke(Sender: TObject; DispID: Integer;
          const IID: TGUID; LocaleID: Integer; Flags: Word;
          Params: tagDISPPARAMS; VarResult, ExcepInfo, ArgErr: Pointer);
  Public
    constructor Create(TheOwner: TComponent); override;
    property ComServer:IWMPPlayer4 read fServer;
    property OnOpenStateChange : T_WMPOCXEventsOpenStateChange read FOnOpenStateChange write FOnOpenStateChange;
    property OnPlayStateChange : T_WMPOCXEventsPlayStateChange read FOnPlayStateChange write FOnPlayStateChange;
    property OnAudioLanguageChange : T_WMPOCXEventsAudioLanguageChange read FOnAudioLanguageChange write FOnAudioLanguageChange;
    property OnStatusChange : T_WMPOCXEventsStatusChange read FOnStatusChange write FOnStatusChange;
    property OnScriptCommand : T_WMPOCXEventsScriptCommand read FOnScriptCommand write FOnScriptCommand;
    property OnNewStream : T_WMPOCXEventsNewStream read FOnNewStream write FOnNewStream;
    property OnDisconnect : T_WMPOCXEventsDisconnect read FOnDisconnect write FOnDisconnect;
    property OnBuffering : T_WMPOCXEventsBuffering read FOnBuffering write FOnBuffering;
    property OnError : T_WMPOCXEventsError read FOnError write FOnError;
    property OnWarning : T_WMPOCXEventsWarning read FOnWarning write FOnWarning;
    property OnEndOfStream : T_WMPOCXEventsEndOfStream read FOnEndOfStream write FOnEndOfStream;
    property OnPositionChange : T_WMPOCXEventsPositionChange read FOnPositionChange write FOnPositionChange;
    property OnMarkerHit : T_WMPOCXEventsMarkerHit read FOnMarkerHit write FOnMarkerHit;
    property OnDurationUnitChange : T_WMPOCXEventsDurationUnitChange read FOnDurationUnitChange write FOnDurationUnitChange;
    property OnCdromMediaChange : T_WMPOCXEventsCdromMediaChange read FOnCdromMediaChange write FOnCdromMediaChange;
    property OnPlaylistChange : T_WMPOCXEventsPlaylistChange read FOnPlaylistChange write FOnPlaylistChange;
    property OnCurrentPlaylistChange : T_WMPOCXEventsCurrentPlaylistChange read FOnCurrentPlaylistChange write FOnCurrentPlaylistChange;
    property OnCurrentPlaylistItemAvailable : T_WMPOCXEventsCurrentPlaylistItemAvailable read FOnCurrentPlaylistItemAvailable write FOnCurrentPlaylistItemAvailable;
    property OnMediaChange : T_WMPOCXEventsMediaChange read FOnMediaChange write FOnMediaChange;
    property OnCurrentMediaItemAvailable : T_WMPOCXEventsCurrentMediaItemAvailable read FOnCurrentMediaItemAvailable write FOnCurrentMediaItemAvailable;
    property OnCurrentItemChange : T_WMPOCXEventsCurrentItemChange read FOnCurrentItemChange write FOnCurrentItemChange;
    property OnMediaCollectionChange : T_WMPOCXEventsMediaCollectionChange read FOnMediaCollectionChange write FOnMediaCollectionChange;
    property OnMediaCollectionAttributeStringAdded : T_WMPOCXEventsMediaCollectionAttributeStringAdded read FOnMediaCollectionAttributeStringAdded write FOnMediaCollectionAttributeStringAdded;
    property OnMediaCollectionAttributeStringRemoved : T_WMPOCXEventsMediaCollectionAttributeStringRemoved read FOnMediaCollectionAttributeStringRemoved write FOnMediaCollectionAttributeStringRemoved;
    property OnMediaCollectionAttributeStringChanged : T_WMPOCXEventsMediaCollectionAttributeStringChanged read FOnMediaCollectionAttributeStringChanged write FOnMediaCollectionAttributeStringChanged;
    property OnPlaylistCollectionChange : T_WMPOCXEventsPlaylistCollectionChange read FOnPlaylistCollectionChange write FOnPlaylistCollectionChange;
    property OnPlaylistCollectionPlaylistAdded : T_WMPOCXEventsPlaylistCollectionPlaylistAdded read FOnPlaylistCollectionPlaylistAdded write FOnPlaylistCollectionPlaylistAdded;
    property OnPlaylistCollectionPlaylistRemoved : T_WMPOCXEventsPlaylistCollectionPlaylistRemoved read FOnPlaylistCollectionPlaylistRemoved write FOnPlaylistCollectionPlaylistRemoved;
    property OnPlaylistCollectionPlaylistSetAsDeleted : T_WMPOCXEventsPlaylistCollectionPlaylistSetAsDeleted read FOnPlaylistCollectionPlaylistSetAsDeleted write FOnPlaylistCollectionPlaylistSetAsDeleted;
    property OnModeChange : T_WMPOCXEventsModeChange read FOnModeChange write FOnModeChange;
    property OnMediaError : T_WMPOCXEventsMediaError read FOnMediaError write FOnMediaError;
    property OnOpenPlaylistSwitch : T_WMPOCXEventsOpenPlaylistSwitch read FOnOpenPlaylistSwitch write FOnOpenPlaylistSwitch;
    property OnDomainChange : T_WMPOCXEventsDomainChange read FOnDomainChange write FOnDomainChange;
    property OnSwitchedToPlayerApplication : T_WMPOCXEventsSwitchedToPlayerApplication read FOnSwitchedToPlayerApplication write FOnSwitchedToPlayerApplication;
    property OnSwitchedToControl : T_WMPOCXEventsSwitchedToControl read FOnSwitchedToControl write FOnSwitchedToControl;
    property OnPlayerDockedStateChange : T_WMPOCXEventsPlayerDockedStateChange read FOnPlayerDockedStateChange write FOnPlayerDockedStateChange;
    property OnPlayerReconnect : T_WMPOCXEventsPlayerReconnect read FOnPlayerReconnect write FOnPlayerReconnect;
    property OnClick : T_WMPOCXEventsClick read FOnClick write FOnClick;
    property OnDoubleClick : T_WMPOCXEventsDoubleClick read FOnDoubleClick write FOnDoubleClick;
    property OnKeyDown : T_WMPOCXEventsKeyDown read FOnKeyDown write FOnKeyDown;
    property OnKeyPress : T_WMPOCXEventsKeyPress read FOnKeyPress write FOnKeyPress;
    property OnKeyUp : T_WMPOCXEventsKeyUp read FOnKeyUp write FOnKeyUp;
    property OnMouseDown : T_WMPOCXEventsMouseDown read FOnMouseDown write FOnMouseDown;
    property OnMouseMove : T_WMPOCXEventsMouseMove read FOnMouseMove write FOnMouseMove;
    property OnMouseUp : T_WMPOCXEventsMouseUp read FOnMouseUp write FOnMouseUp;
    property OnDeviceConnect : T_WMPOCXEventsDeviceConnect read FOnDeviceConnect write FOnDeviceConnect;
    property OnDeviceDisconnect : T_WMPOCXEventsDeviceDisconnect read FOnDeviceDisconnect write FOnDeviceDisconnect;
    property OnDeviceStatusChange : T_WMPOCXEventsDeviceStatusChange read FOnDeviceStatusChange write FOnDeviceStatusChange;
    property OnDeviceSyncStateChange : T_WMPOCXEventsDeviceSyncStateChange read FOnDeviceSyncStateChange write FOnDeviceSyncStateChange;
    property OnDeviceSyncError : T_WMPOCXEventsDeviceSyncError read FOnDeviceSyncError write FOnDeviceSyncError;
    property OnCreatePartnershipComplete : T_WMPOCXEventsCreatePartnershipComplete read FOnCreatePartnershipComplete write FOnCreatePartnershipComplete;
    property OnDeviceEstimation : T_WMPOCXEventsDeviceEstimation read FOnDeviceEstimation write FOnDeviceEstimation;
    property OnCdromRipStateChange : T_WMPOCXEventsCdromRipStateChange read FOnCdromRipStateChange write FOnCdromRipStateChange;
    property OnCdromRipMediaError : T_WMPOCXEventsCdromRipMediaError read FOnCdromRipMediaError write FOnCdromRipMediaError;
    property OnCdromBurnStateChange : T_WMPOCXEventsCdromBurnStateChange read FOnCdromBurnStateChange write FOnCdromBurnStateChange;
    property OnCdromBurnMediaError : T_WMPOCXEventsCdromBurnMediaError read FOnCdromBurnMediaError write FOnCdromBurnMediaError;
    property OnCdromBurnError : T_WMPOCXEventsCdromBurnError read FOnCdromBurnError write FOnCdromBurnError;
    property OnLibraryConnect : T_WMPOCXEventsLibraryConnect read FOnLibraryConnect write FOnLibraryConnect;
    property OnLibraryDisconnect : T_WMPOCXEventsLibraryDisconnect read FOnLibraryDisconnect write FOnLibraryDisconnect;
    property OnFolderScanStateChange : T_WMPOCXEventsFolderScanStateChange read FOnFolderScanStateChange write FOnFolderScanStateChange;
    property OnStringCollectionChange : T_WMPOCXEventsStringCollectionChange read FOnStringCollectionChange write FOnStringCollectionChange;
    property OnMediaCollectionMediaAdded : T_WMPOCXEventsMediaCollectionMediaAdded read FOnMediaCollectionMediaAdded write FOnMediaCollectionMediaAdded;
    property OnMediaCollectionMediaRemoved : T_WMPOCXEventsMediaCollectionMediaRemoved read FOnMediaCollectionMediaRemoved write FOnMediaCollectionMediaRemoved;

  end;

  TIWMPButtonCtrlEventsonclick = procedure(Sender: TObject) of object;


  CoWMPButtonCtrl = Class
  Public
    Class Function Create: IWMPButtonCtrl;
    Class Function CreateRemote(const MachineName: string): IWMPButtonCtrl;
  end;

  TEvsWMPButtonCtrl = Class(TEventSink)
  Private
    FOnonclick:TIWMPButtonCtrlEventsonclick;

    fServer:IWMPButtonCtrl;
    procedure EventSinkInvoke(Sender: TObject; DispID: Integer;
          const IID: TGUID; LocaleID: Integer; Flags: Word;
          Params: tagDISPPARAMS; VarResult, ExcepInfo, ArgErr: Pointer);
  Public
    constructor Create(TheOwner: TComponent); override;
    property ComServer:IWMPButtonCtrl read fServer;
    property Ononclick : TIWMPButtonCtrlEventsonclick read FOnonclick write FOnonclick;

  end;

  CoWMPListBoxCtrl = Class
  Public
    Class Function Create: IWMPListBoxCtrl;
    Class Function CreateRemote(const MachineName: string): IWMPListBoxCtrl;
  end;

  TIWMPSliderCtrlEventsondragbegin = procedure(Sender: TObject) of object;
  TIWMPSliderCtrlEventsondragend = procedure(Sender: TObject) of object;
  TIWMPSliderCtrlEventsonpositionchange = procedure(Sender: TObject) of object;


  CoWMPSliderCtrl = Class
  Public
    Class Function Create: IWMPSliderCtrl;
    Class Function CreateRemote(const MachineName: string): IWMPSliderCtrl;
  end;

  TEvsWMPSliderCtrl = Class(TEventSink)
  Private
    FOnondragbegin:TIWMPSliderCtrlEventsondragbegin;
    FOnondragend:TIWMPSliderCtrlEventsondragend;
    FOnonpositionchange:TIWMPSliderCtrlEventsonpositionchange;

    fServer:IWMPSliderCtrl;
    procedure EventSinkInvoke(Sender: TObject; DispID: Integer;
          const IID: TGUID; LocaleID: Integer; Flags: Word;
          Params: tagDISPPARAMS; VarResult, ExcepInfo, ArgErr: Pointer);
  Public
    constructor Create(TheOwner: TComponent); override;
    property ComServer:IWMPSliderCtrl read fServer;
    property Onondragbegin : TIWMPSliderCtrlEventsondragbegin read FOnondragbegin write FOnondragbegin;
    property Onondragend : TIWMPSliderCtrlEventsondragend read FOnondragend write FOnondragend;
    property Ononpositionchange : TIWMPSliderCtrlEventsonpositionchange read FOnonpositionchange write FOnonpositionchange;

  end;

  TIWMPVideoCtrlEventsonvideostart = procedure(Sender: TObject) of object;
  TIWMPVideoCtrlEventsonvideoend = procedure(Sender: TObject) of object;


  CoWMPVideoCtrl = Class
  Public
    Class Function Create: IWMPVideoCtrl;
    Class Function CreateRemote(const MachineName: string): IWMPVideoCtrl;
  end;

  TEvsWMPVideoCtrl = Class(TEventSink)
  Private
    FOnonvideostart:TIWMPVideoCtrlEventsonvideostart;
    FOnonvideoend:TIWMPVideoCtrlEventsonvideoend;

    fServer:IWMPVideoCtrl;
    procedure EventSinkInvoke(Sender: TObject; DispID: Integer;
          const IID: TGUID; LocaleID: Integer; Flags: Word;
          Params: tagDISPPARAMS; VarResult, ExcepInfo, ArgErr: Pointer);
  Public
    constructor Create(TheOwner: TComponent); override;
    property ComServer:IWMPVideoCtrl read fServer;
    property Ononvideostart : TIWMPVideoCtrlEventsonvideostart read FOnonvideostart write FOnonvideostart;
    property Ononvideoend : TIWMPVideoCtrlEventsonvideoend read FOnonvideoend write FOnonvideoend;

  end;

  CoWMPEffects = Class
  Public
    Class Function Create: IWMPEffectsCtrl;
    Class Function CreateRemote(const MachineName: string): IWMPEffectsCtrl;
  end;

  CoWMPEqualizerSettingsCtrl = Class
  Public
    Class Function Create: IWMPEqualizerSettingsCtrl;
    Class Function CreateRemote(const MachineName: string): IWMPEqualizerSettingsCtrl;
  end;

  CoWMPVideoSettingsCtrl = Class
  Public
    Class Function Create: IWMPVideoSettingsCtrl;
    Class Function CreateRemote(const MachineName: string): IWMPVideoSettingsCtrl;
  end;

  CoWMPLibraryTreeCtrl = Class
  Public
    Class Function Create: IWMPLibraryTreeCtrl;
    Class Function CreateRemote(const MachineName: string): IWMPLibraryTreeCtrl;
  end;

  CoWMPEditCtrl = Class
  Public
    Class Function Create: IWMPEditCtrl;
    Class Function CreateRemote(const MachineName: string): IWMPEditCtrl;
  end;

  CoWMPSkinList = Class
  Public
    Class Function Create: IWMPSkinList;
    Class Function CreateRemote(const MachineName: string): IWMPSkinList;
  end;

  CoWMPMenuCtrl = Class
  Public
    Class Function Create: IWMPMenuCtrl;
    Class Function CreateRemote(const MachineName: string): IWMPMenuCtrl;
  end;

  CoWMPAutoMenuCtrl = Class
  Public
    Class Function Create: IWMPAutoMenuCtrl;
    Class Function CreateRemote(const MachineName: string): IWMPAutoMenuCtrl;
  end;

  CoWMPRegionalButtonCtrl = Class
  Public
    Class Function Create: IWMPRegionalButtonCtrl;
    Class Function CreateRemote(const MachineName: string): IWMPRegionalButtonCtrl;
  end;

  TIWMPRegionalButtonEventsonblur = procedure(Sender: TObject) of object;
  TIWMPRegionalButtonEventsonfocus = procedure(Sender: TObject) of object;
  TIWMPRegionalButtonEventsonclick = procedure(Sender: TObject) of object;
  TIWMPRegionalButtonEventsondblclick = procedure(Sender: TObject) of object;
  TIWMPRegionalButtonEventsonmousedown = procedure(Sender: TObject) of object;
  TIWMPRegionalButtonEventsonmouseup = procedure(Sender: TObject) of object;
  TIWMPRegionalButtonEventsonmousemove = procedure(Sender: TObject) of object;
  TIWMPRegionalButtonEventsonmouseover = procedure(Sender: TObject) of object;
  TIWMPRegionalButtonEventsonmouseout = procedure(Sender: TObject) of object;
  TIWMPRegionalButtonEventsonkeypress = procedure(Sender: TObject) of object;
  TIWMPRegionalButtonEventsonkeydown = procedure(Sender: TObject) of object;
  TIWMPRegionalButtonEventsonkeyup = procedure(Sender: TObject) of object;


  CoWMPRegionalButton = Class
  Public
    Class Function Create: IWMPRegionalButton;
    Class Function CreateRemote(const MachineName: string): IWMPRegionalButton;
  end;

  TEvsWMPRegionalButton = Class(TEventSink)
  Private
    FOnonblur:TIWMPRegionalButtonEventsonblur;
    FOnonfocus:TIWMPRegionalButtonEventsonfocus;
    FOnonclick:TIWMPRegionalButtonEventsonclick;
    FOnondblclick:TIWMPRegionalButtonEventsondblclick;
    FOnonmousedown:TIWMPRegionalButtonEventsonmousedown;
    FOnonmouseup:TIWMPRegionalButtonEventsonmouseup;
    FOnonmousemove:TIWMPRegionalButtonEventsonmousemove;
    FOnonmouseover:TIWMPRegionalButtonEventsonmouseover;
    FOnonmouseout:TIWMPRegionalButtonEventsonmouseout;
    FOnonkeypress:TIWMPRegionalButtonEventsonkeypress;
    FOnonkeydown:TIWMPRegionalButtonEventsonkeydown;
    FOnonkeyup:TIWMPRegionalButtonEventsonkeyup;

    fServer:IWMPRegionalButton;
    procedure EventSinkInvoke(Sender: TObject; DispID: Integer;
          const IID: TGUID; LocaleID: Integer; Flags: Word;
          Params: tagDISPPARAMS; VarResult, ExcepInfo, ArgErr: Pointer);
  Public
    constructor Create(TheOwner: TComponent); override;
    property ComServer:IWMPRegionalButton read fServer;
    property Ononblur : TIWMPRegionalButtonEventsonblur read FOnonblur write FOnonblur;
    property Ononfocus : TIWMPRegionalButtonEventsonfocus read FOnonfocus write FOnonfocus;
    property Ononclick : TIWMPRegionalButtonEventsonclick read FOnonclick write FOnonclick;
    property Onondblclick : TIWMPRegionalButtonEventsondblclick read FOnondblclick write FOnondblclick;
    property Ononmousedown : TIWMPRegionalButtonEventsonmousedown read FOnonmousedown write FOnonmousedown;
    property Ononmouseup : TIWMPRegionalButtonEventsonmouseup read FOnonmouseup write FOnonmouseup;
    property Ononmousemove : TIWMPRegionalButtonEventsonmousemove read FOnonmousemove write FOnonmousemove;
    property Ononmouseover : TIWMPRegionalButtonEventsonmouseover read FOnonmouseover write FOnonmouseover;
    property Ononmouseout : TIWMPRegionalButtonEventsonmouseout read FOnonmouseout write FOnonmouseout;
    property Ononkeypress : TIWMPRegionalButtonEventsonkeypress read FOnonkeypress write FOnonkeypress;
    property Ononkeydown : TIWMPRegionalButtonEventsonkeydown read FOnonkeydown write FOnonkeydown;
    property Ononkeyup : TIWMPRegionalButtonEventsonkeyup read FOnonkeyup write FOnonkeyup;

  end;

  TIWMPCustomSliderCtrlEventsondragbegin = procedure(Sender: TObject) of object;
  TIWMPCustomSliderCtrlEventsondragend = procedure(Sender: TObject) of object;
  TIWMPCustomSliderCtrlEventsonpositionchange = procedure(Sender: TObject) of object;


  CoWMPCustomSliderCtrl = Class
  Public
    Class Function Create: IWMPCustomSlider;
    Class Function CreateRemote(const MachineName: string): IWMPCustomSlider;
  end;

  TEvsWMPCustomSliderCtrl = Class(TEventSink)
  Private
    FOnondragbegin:TIWMPCustomSliderCtrlEventsondragbegin;
    FOnondragend:TIWMPCustomSliderCtrlEventsondragend;
    FOnonpositionchange:TIWMPCustomSliderCtrlEventsonpositionchange;

    fServer:IWMPCustomSlider;
    procedure EventSinkInvoke(Sender: TObject; DispID: Integer;
          const IID: TGUID; LocaleID: Integer; Flags: Word;
          Params: tagDISPPARAMS; VarResult, ExcepInfo, ArgErr: Pointer);
  Public
    constructor Create(TheOwner: TComponent); override;
    property ComServer:IWMPCustomSlider read fServer;
    property Onondragbegin : TIWMPCustomSliderCtrlEventsondragbegin read FOnondragbegin write FOnondragbegin;
    property Onondragend : TIWMPCustomSliderCtrlEventsondragend read FOnondragend write FOnondragend;
    property Ononpositionchange : TIWMPCustomSliderCtrlEventsonpositionchange read FOnonpositionchange write FOnonpositionchange;

  end;

  CoWMPTextCtrl = Class
  Public
    Class Function Create: IWMPTextCtrl;
    Class Function CreateRemote(const MachineName: string): IWMPTextCtrl;
  end;

  CoWMPPlaylistCtrl = Class
  Public
    Class Function Create: IWMPPlaylistCtrl;
    Class Function CreateRemote(const MachineName: string): IWMPPlaylistCtrl;
  end;

  T_WMPCoreEventsOpenStateChange = procedure(Sender: TObject;NewState:Integer) of object;
  T_WMPCoreEventsPlayStateChange = procedure(Sender: TObject;NewState:Integer) of object;
  T_WMPCoreEventsAudioLanguageChange = procedure(Sender: TObject;LangID:Integer) of object;
  T_WMPCoreEventsStatusChange = procedure(Sender: TObject) of object;
  T_WMPCoreEventsScriptCommand = procedure(Sender: TObject;scType:WideString;Param:WideString) of object;
  T_WMPCoreEventsNewStream = procedure(Sender: TObject) of object;
  T_WMPCoreEventsDisconnect = procedure(Sender: TObject;Result:Integer) of object;
  T_WMPCoreEventsBuffering = procedure(Sender: TObject;Start:WordBool) of object;
  T_WMPCoreEventsError = procedure(Sender: TObject) of object;
  T_WMPCoreEventsWarning = procedure(Sender: TObject;WarningType:Integer;Param:Integer;Description:WideString) of object;
  T_WMPCoreEventsEndOfStream = procedure(Sender: TObject;Result:Integer) of object;
  T_WMPCoreEventsPositionChange = procedure(Sender: TObject;oldPosition:Double;newPosition:Double) of object;
  T_WMPCoreEventsMarkerHit = procedure(Sender: TObject;MarkerNum:Integer) of object;
  T_WMPCoreEventsDurationUnitChange = procedure(Sender: TObject;NewDurationUnit:Integer) of object;
  T_WMPCoreEventsCdromMediaChange = procedure(Sender: TObject;CdromNum:Integer) of object;
  T_WMPCoreEventsPlaylistChange = procedure(Sender: TObject;Playlist:IDispatch;change:WMPPlaylistChangeEventType) of object;
  T_WMPCoreEventsCurrentPlaylistChange = procedure(Sender: TObject;change:WMPPlaylistChangeEventType) of object;
  T_WMPCoreEventsCurrentPlaylistItemAvailable = procedure(Sender: TObject;bstrItemName:WideString) of object;
  T_WMPCoreEventsMediaChange = procedure(Sender: TObject;Item:IDispatch) of object;
  T_WMPCoreEventsCurrentMediaItemAvailable = procedure(Sender: TObject;bstrItemName:WideString) of object;
  T_WMPCoreEventsCurrentItemChange = procedure(Sender: TObject;pdispMedia:IDispatch) of object;
  T_WMPCoreEventsMediaCollectionChange = procedure(Sender: TObject) of object;
  T_WMPCoreEventsMediaCollectionAttributeStringAdded = procedure(Sender: TObject;bstrAttribName:WideString;bstrAttribVal:WideString) of object;
  T_WMPCoreEventsMediaCollectionAttributeStringRemoved = procedure(Sender: TObject;bstrAttribName:WideString;bstrAttribVal:WideString) of object;
  T_WMPCoreEventsMediaCollectionAttributeStringChanged = procedure(Sender: TObject;bstrAttribName:WideString;bstrOldAttribVal:WideString;bstrNewAttribVal:WideString) of object;
  T_WMPCoreEventsPlaylistCollectionChange = procedure(Sender: TObject) of object;
  T_WMPCoreEventsPlaylistCollectionPlaylistAdded = procedure(Sender: TObject;bstrPlaylistName:WideString) of object;
  T_WMPCoreEventsPlaylistCollectionPlaylistRemoved = procedure(Sender: TObject;bstrPlaylistName:WideString) of object;
  T_WMPCoreEventsPlaylistCollectionPlaylistSetAsDeleted = procedure(Sender: TObject;bstrPlaylistName:WideString;varfIsDeleted:WordBool) of object;
  T_WMPCoreEventsModeChange = procedure(Sender: TObject;ModeName:WideString;NewValue:WordBool) of object;
  T_WMPCoreEventsMediaError = procedure(Sender: TObject;pMediaObject:IDispatch) of object;
  T_WMPCoreEventsOpenPlaylistSwitch = procedure(Sender: TObject;pItem:IDispatch) of object;
  T_WMPCoreEventsDomainChange = procedure(Sender: TObject;strDomain:WideString) of object;
  T_WMPCoreEventsStringCollectionChange = procedure(Sender: TObject;pdispStringCollection:IDispatch;change:WMPStringCollectionChangeEventType;lCollectionIndex:Integer) of object;
  T_WMPCoreEventsMediaCollectionMediaAdded = procedure(Sender: TObject;pdispMedia:IDispatch) of object;
  T_WMPCoreEventsMediaCollectionMediaRemoved = procedure(Sender: TObject;pdispMedia:IDispatch) of object;


  CoWMPCore = Class
  Public
    Class Function Create: IWMPCore3;
    Class Function CreateRemote(const MachineName: string): IWMPCore3;
  end;

  TEvsWMPCore = Class(TEventSink)
  Private
    FOnOpenStateChange:T_WMPCoreEventsOpenStateChange;
    FOnPlayStateChange:T_WMPCoreEventsPlayStateChange;
    FOnAudioLanguageChange:T_WMPCoreEventsAudioLanguageChange;
    FOnStatusChange:T_WMPCoreEventsStatusChange;
    FOnScriptCommand:T_WMPCoreEventsScriptCommand;
    FOnNewStream:T_WMPCoreEventsNewStream;
    FOnDisconnect:T_WMPCoreEventsDisconnect;
    FOnBuffering:T_WMPCoreEventsBuffering;
    FOnError:T_WMPCoreEventsError;
    FOnWarning:T_WMPCoreEventsWarning;
    FOnEndOfStream:T_WMPCoreEventsEndOfStream;
    FOnPositionChange:T_WMPCoreEventsPositionChange;
    FOnMarkerHit:T_WMPCoreEventsMarkerHit;
    FOnDurationUnitChange:T_WMPCoreEventsDurationUnitChange;
    FOnCdromMediaChange:T_WMPCoreEventsCdromMediaChange;
    FOnPlaylistChange:T_WMPCoreEventsPlaylistChange;
    FOnCurrentPlaylistChange:T_WMPCoreEventsCurrentPlaylistChange;
    FOnCurrentPlaylistItemAvailable:T_WMPCoreEventsCurrentPlaylistItemAvailable;
    FOnMediaChange:T_WMPCoreEventsMediaChange;
    FOnCurrentMediaItemAvailable:T_WMPCoreEventsCurrentMediaItemAvailable;
    FOnCurrentItemChange:T_WMPCoreEventsCurrentItemChange;
    FOnMediaCollectionChange:T_WMPCoreEventsMediaCollectionChange;
    FOnMediaCollectionAttributeStringAdded:T_WMPCoreEventsMediaCollectionAttributeStringAdded;
    FOnMediaCollectionAttributeStringRemoved:T_WMPCoreEventsMediaCollectionAttributeStringRemoved;
    FOnMediaCollectionAttributeStringChanged:T_WMPCoreEventsMediaCollectionAttributeStringChanged;
    FOnPlaylistCollectionChange:T_WMPCoreEventsPlaylistCollectionChange;
    FOnPlaylistCollectionPlaylistAdded:T_WMPCoreEventsPlaylistCollectionPlaylistAdded;
    FOnPlaylistCollectionPlaylistRemoved:T_WMPCoreEventsPlaylistCollectionPlaylistRemoved;
    FOnPlaylistCollectionPlaylistSetAsDeleted:T_WMPCoreEventsPlaylistCollectionPlaylistSetAsDeleted;
    FOnModeChange:T_WMPCoreEventsModeChange;
    FOnMediaError:T_WMPCoreEventsMediaError;
    FOnOpenPlaylistSwitch:T_WMPCoreEventsOpenPlaylistSwitch;
    FOnDomainChange:T_WMPCoreEventsDomainChange;
    FOnStringCollectionChange:T_WMPCoreEventsStringCollectionChange;
    FOnMediaCollectionMediaAdded:T_WMPCoreEventsMediaCollectionMediaAdded;
    FOnMediaCollectionMediaRemoved:T_WMPCoreEventsMediaCollectionMediaRemoved;

    fServer:IWMPCore3;
    procedure EventSinkInvoke(Sender: TObject; DispID: Integer;
          const IID: TGUID; LocaleID: Integer; Flags: Word;
          Params: tagDISPPARAMS; VarResult, ExcepInfo, ArgErr: Pointer);
  Public
    constructor Create(TheOwner: TComponent); override;
    property ComServer:IWMPCore3 read fServer;
    property OnOpenStateChange : T_WMPCoreEventsOpenStateChange read FOnOpenStateChange write FOnOpenStateChange;
    property OnPlayStateChange : T_WMPCoreEventsPlayStateChange read FOnPlayStateChange write FOnPlayStateChange;
    property OnAudioLanguageChange : T_WMPCoreEventsAudioLanguageChange read FOnAudioLanguageChange write FOnAudioLanguageChange;
    property OnStatusChange : T_WMPCoreEventsStatusChange read FOnStatusChange write FOnStatusChange;
    property OnScriptCommand : T_WMPCoreEventsScriptCommand read FOnScriptCommand write FOnScriptCommand;
    property OnNewStream : T_WMPCoreEventsNewStream read FOnNewStream write FOnNewStream;
    property OnDisconnect : T_WMPCoreEventsDisconnect read FOnDisconnect write FOnDisconnect;
    property OnBuffering : T_WMPCoreEventsBuffering read FOnBuffering write FOnBuffering;
    property OnError : T_WMPCoreEventsError read FOnError write FOnError;
    property OnWarning : T_WMPCoreEventsWarning read FOnWarning write FOnWarning;
    property OnEndOfStream : T_WMPCoreEventsEndOfStream read FOnEndOfStream write FOnEndOfStream;
    property OnPositionChange : T_WMPCoreEventsPositionChange read FOnPositionChange write FOnPositionChange;
    property OnMarkerHit : T_WMPCoreEventsMarkerHit read FOnMarkerHit write FOnMarkerHit;
    property OnDurationUnitChange : T_WMPCoreEventsDurationUnitChange read FOnDurationUnitChange write FOnDurationUnitChange;
    property OnCdromMediaChange : T_WMPCoreEventsCdromMediaChange read FOnCdromMediaChange write FOnCdromMediaChange;
    property OnPlaylistChange : T_WMPCoreEventsPlaylistChange read FOnPlaylistChange write FOnPlaylistChange;
    property OnCurrentPlaylistChange : T_WMPCoreEventsCurrentPlaylistChange read FOnCurrentPlaylistChange write FOnCurrentPlaylistChange;
    property OnCurrentPlaylistItemAvailable : T_WMPCoreEventsCurrentPlaylistItemAvailable read FOnCurrentPlaylistItemAvailable write FOnCurrentPlaylistItemAvailable;
    property OnMediaChange : T_WMPCoreEventsMediaChange read FOnMediaChange write FOnMediaChange;
    property OnCurrentMediaItemAvailable : T_WMPCoreEventsCurrentMediaItemAvailable read FOnCurrentMediaItemAvailable write FOnCurrentMediaItemAvailable;
    property OnCurrentItemChange : T_WMPCoreEventsCurrentItemChange read FOnCurrentItemChange write FOnCurrentItemChange;
    property OnMediaCollectionChange : T_WMPCoreEventsMediaCollectionChange read FOnMediaCollectionChange write FOnMediaCollectionChange;
    property OnMediaCollectionAttributeStringAdded : T_WMPCoreEventsMediaCollectionAttributeStringAdded read FOnMediaCollectionAttributeStringAdded write FOnMediaCollectionAttributeStringAdded;
    property OnMediaCollectionAttributeStringRemoved : T_WMPCoreEventsMediaCollectionAttributeStringRemoved read FOnMediaCollectionAttributeStringRemoved write FOnMediaCollectionAttributeStringRemoved;
    property OnMediaCollectionAttributeStringChanged : T_WMPCoreEventsMediaCollectionAttributeStringChanged read FOnMediaCollectionAttributeStringChanged write FOnMediaCollectionAttributeStringChanged;
    property OnPlaylistCollectionChange : T_WMPCoreEventsPlaylistCollectionChange read FOnPlaylistCollectionChange write FOnPlaylistCollectionChange;
    property OnPlaylistCollectionPlaylistAdded : T_WMPCoreEventsPlaylistCollectionPlaylistAdded read FOnPlaylistCollectionPlaylistAdded write FOnPlaylistCollectionPlaylistAdded;
    property OnPlaylistCollectionPlaylistRemoved : T_WMPCoreEventsPlaylistCollectionPlaylistRemoved read FOnPlaylistCollectionPlaylistRemoved write FOnPlaylistCollectionPlaylistRemoved;
    property OnPlaylistCollectionPlaylistSetAsDeleted : T_WMPCoreEventsPlaylistCollectionPlaylistSetAsDeleted read FOnPlaylistCollectionPlaylistSetAsDeleted write FOnPlaylistCollectionPlaylistSetAsDeleted;
    property OnModeChange : T_WMPCoreEventsModeChange read FOnModeChange write FOnModeChange;
    property OnMediaError : T_WMPCoreEventsMediaError read FOnMediaError write FOnMediaError;
    property OnOpenPlaylistSwitch : T_WMPCoreEventsOpenPlaylistSwitch read FOnOpenPlaylistSwitch write FOnOpenPlaylistSwitch;
    property OnDomainChange : T_WMPCoreEventsDomainChange read FOnDomainChange write FOnDomainChange;
    property OnStringCollectionChange : T_WMPCoreEventsStringCollectionChange read FOnStringCollectionChange write FOnStringCollectionChange;
    property OnMediaCollectionMediaAdded : T_WMPCoreEventsMediaCollectionMediaAdded read FOnMediaCollectionMediaAdded write FOnMediaCollectionMediaAdded;
    property OnMediaCollectionMediaRemoved : T_WMPCoreEventsMediaCollectionMediaRemoved read FOnMediaCollectionMediaRemoved write FOnMediaCollectionMediaRemoved;

  end;

implementation

uses comobj;

Class Function CoWindowsMediaPlayer.Create: IWMPPlayer4;
begin
  Result := CreateComObject(CLASS_WindowsMediaPlayer) as IWMPPlayer4;
end;

Class Function CoWindowsMediaPlayer.CreateRemote(const MachineName: string): IWMPPlayer4;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_WindowsMediaPlayer) as IWMPPlayer4;
end;

constructor TEvsWindowsMediaPlayer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  OnInvoke:=EventSinkInvoke;
  fServer:=CoWindowsMediaPlayer.Create;
  Connect(fServer,_WMPOCXEvents);
end;

procedure TEvsWindowsMediaPlayer.EventSinkInvoke(Sender: TObject; DispID: Integer;
  const IID: TGUID; LocaleID: Integer; Flags: Word; Params: tagDISPPARAMS;
  VarResult, ExcepInfo, ArgErr: Pointer);
begin
  case DispID of
    5001: if assigned(OnOpenStateChange) then
          OnOpenStateChange(Self, OleVariant(Params.rgvarg[0]));
    5101: if assigned(OnPlayStateChange) then
          OnPlayStateChange(Self, OleVariant(Params.rgvarg[0]));
    5102: if assigned(OnAudioLanguageChange) then
          OnAudioLanguageChange(Self, OleVariant(Params.rgvarg[0]));
    5002: if assigned(OnStatusChange) then
          OnStatusChange(Self);
    5301: if assigned(OnScriptCommand) then
          OnScriptCommand(Self, OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    5403: if assigned(OnNewStream) then
          OnNewStream(Self);
    5401: if assigned(OnDisconnect) then
          OnDisconnect(Self, OleVariant(Params.rgvarg[0]));
    5402: if assigned(OnBuffering) then
          OnBuffering(Self, OleVariant(Params.rgvarg[0]));
    5501: if assigned(OnError) then
          OnError(Self);
    5601: if assigned(OnWarning) then
          OnWarning(Self, OleVariant(Params.rgvarg[2]), OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    5201: if assigned(OnEndOfStream) then
          OnEndOfStream(Self, OleVariant(Params.rgvarg[0]));
    5202: if assigned(OnPositionChange) then
          OnPositionChange(Self, OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    5203: if assigned(OnMarkerHit) then
          OnMarkerHit(Self, OleVariant(Params.rgvarg[0]));
    5204: if assigned(OnDurationUnitChange) then
          OnDurationUnitChange(Self, OleVariant(Params.rgvarg[0]));
    5701: if assigned(OnCdromMediaChange) then
          OnCdromMediaChange(Self, OleVariant(Params.rgvarg[0]));
    5801: if assigned(OnPlaylistChange) then
          OnPlaylistChange(Self, OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    5804: if assigned(OnCurrentPlaylistChange) then
          OnCurrentPlaylistChange(Self, OleVariant(Params.rgvarg[0]));
    5805: if assigned(OnCurrentPlaylistItemAvailable) then
          OnCurrentPlaylistItemAvailable(Self, OleVariant(Params.rgvarg[0]));
    5802: if assigned(OnMediaChange) then
          OnMediaChange(Self, OleVariant(Params.rgvarg[0]));
    5803: if assigned(OnCurrentMediaItemAvailable) then
          OnCurrentMediaItemAvailable(Self, OleVariant(Params.rgvarg[0]));
    5806: if assigned(OnCurrentItemChange) then
          OnCurrentItemChange(Self, OleVariant(Params.rgvarg[0]));
    5807: if assigned(OnMediaCollectionChange) then
          OnMediaCollectionChange(Self);
    5808: if assigned(OnMediaCollectionAttributeStringAdded) then
          OnMediaCollectionAttributeStringAdded(Self, OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    5809: if assigned(OnMediaCollectionAttributeStringRemoved) then
          OnMediaCollectionAttributeStringRemoved(Self, OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    5820: if assigned(OnMediaCollectionAttributeStringChanged) then
          OnMediaCollectionAttributeStringChanged(Self, OleVariant(Params.rgvarg[2]), OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    5810: if assigned(OnPlaylistCollectionChange) then
          OnPlaylistCollectionChange(Self);
    5811: if assigned(OnPlaylistCollectionPlaylistAdded) then
          OnPlaylistCollectionPlaylistAdded(Self, OleVariant(Params.rgvarg[0]));
    5812: if assigned(OnPlaylistCollectionPlaylistRemoved) then
          OnPlaylistCollectionPlaylistRemoved(Self, OleVariant(Params.rgvarg[0]));
    5818: if assigned(OnPlaylistCollectionPlaylistSetAsDeleted) then
          OnPlaylistCollectionPlaylistSetAsDeleted(Self, OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    5819: if assigned(OnModeChange) then
          OnModeChange(Self, OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    5821: if assigned(OnMediaError) then
          OnMediaError(Self, OleVariant(Params.rgvarg[0]));
    5823: if assigned(OnOpenPlaylistSwitch) then
          OnOpenPlaylistSwitch(Self, OleVariant(Params.rgvarg[0]));
    5822: if assigned(OnDomainChange) then
          OnDomainChange(Self, OleVariant(Params.rgvarg[0]));
    6501: if assigned(OnSwitchedToPlayerApplication) then
          OnSwitchedToPlayerApplication(Self);
    6502: if assigned(OnSwitchedToControl) then
          OnSwitchedToControl(Self);
    6503: if assigned(OnPlayerDockedStateChange) then
          OnPlayerDockedStateChange(Self);
    6504: if assigned(OnPlayerReconnect) then
          OnPlayerReconnect(Self);
    6505: if assigned(OnClick) then
          OnClick(Self, OleVariant(Params.rgvarg[3]), OleVariant(Params.rgvarg[2]), OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    6506: if assigned(OnDoubleClick) then
          OnDoubleClick(Self, OleVariant(Params.rgvarg[3]), OleVariant(Params.rgvarg[2]), OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    6507: if assigned(OnKeyDown) then
          OnKeyDown(Self, OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    6508: if assigned(OnKeyPress) then
          OnKeyPress(Self, OleVariant(Params.rgvarg[0]));
    6509: if assigned(OnKeyUp) then
          OnKeyUp(Self, OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    6510: if assigned(OnMouseDown) then
          OnMouseDown(Self, OleVariant(Params.rgvarg[3]), OleVariant(Params.rgvarg[2]), OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    6511: if assigned(OnMouseMove) then
          OnMouseMove(Self, OleVariant(Params.rgvarg[3]), OleVariant(Params.rgvarg[2]), OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    6512: if assigned(OnMouseUp) then
          OnMouseUp(Self, OleVariant(Params.rgvarg[3]), OleVariant(Params.rgvarg[2]), OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    6513: if assigned(OnDeviceConnect) then
          OnDeviceConnect(Self, OleVariant(Params.rgvarg[0]));
    6514: if assigned(OnDeviceDisconnect) then
          OnDeviceDisconnect(Self, OleVariant(Params.rgvarg[0]));
    6515: if assigned(OnDeviceStatusChange) then
          OnDeviceStatusChange(Self, OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    6516: if assigned(OnDeviceSyncStateChange) then
          OnDeviceSyncStateChange(Self, OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    6517: if assigned(OnDeviceSyncError) then
          OnDeviceSyncError(Self, OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
//    6518: if assigned(OnCreatePartnershipComplete) then
//          OnCreatePartnershipComplete(Self, OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
//    6527: if assigned(OnDeviceEstimation) then
//          OnDeviceEstimation(Self, OleVariant(Params.rgvarg[3]), OleVariant(Params.rgvarg[2]), OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    6519: if assigned(OnCdromRipStateChange) then
          OnCdromRipStateChange(Self, OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    6520: if assigned(OnCdromRipMediaError) then
          OnCdromRipMediaError(Self, OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    6521: if assigned(OnCdromBurnStateChange) then
          OnCdromBurnStateChange(Self, OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    6522: if assigned(OnCdromBurnMediaError) then
          OnCdromBurnMediaError(Self, OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
//    6523: if assigned(OnCdromBurnError) then
//          OnCdromBurnError(Self, OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    6524: if assigned(OnLibraryConnect) then
          OnLibraryConnect(Self, OleVariant(Params.rgvarg[0]));
    6525: if assigned(OnLibraryDisconnect) then
          OnLibraryDisconnect(Self, OleVariant(Params.rgvarg[0]));
    6526: if assigned(OnFolderScanStateChange) then
          OnFolderScanStateChange(Self, OleVariant(Params.rgvarg[0]));
    5824: if assigned(OnStringCollectionChange) then
          OnStringCollectionChange(Self, OleVariant(Params.rgvarg[2]), OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    5825: if assigned(OnMediaCollectionMediaAdded) then
          OnMediaCollectionMediaAdded(Self, OleVariant(Params.rgvarg[0]));
    5826: if assigned(OnMediaCollectionMediaRemoved) then
          OnMediaCollectionMediaRemoved(Self, OleVariant(Params.rgvarg[0]));

  end;
end;

Class Function CoWMPButtonCtrl.Create: IWMPButtonCtrl;
begin
  Result := CreateComObject(CLASS_WMPButtonCtrl) as IWMPButtonCtrl;
end;

Class Function CoWMPButtonCtrl.CreateRemote(const MachineName: string): IWMPButtonCtrl;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_WMPButtonCtrl) as IWMPButtonCtrl;
end;

constructor TEvsWMPButtonCtrl.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  OnInvoke:=EventSinkInvoke;
  fServer:=CoWMPButtonCtrl.Create;
  Connect(fServer,IWMPButtonCtrlEvents);
end;

procedure TEvsWMPButtonCtrl.EventSinkInvoke(Sender: TObject; DispID: Integer;
  const IID: TGUID; LocaleID: Integer; Flags: Word; Params: tagDISPPARAMS;
  VarResult, ExcepInfo, ArgErr: Pointer);
begin
  case DispID of
    5120: if assigned(Ononclick) then
          Ononclick(Self);

  end;
end;

Class Function CoWMPListBoxCtrl.Create: IWMPListBoxCtrl;
begin
  Result := CreateComObject(CLASS_WMPListBoxCtrl) as IWMPListBoxCtrl;
end;

Class Function CoWMPListBoxCtrl.CreateRemote(const MachineName: string): IWMPListBoxCtrl;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_WMPListBoxCtrl) as IWMPListBoxCtrl;
end;

Class Function CoWMPSliderCtrl.Create: IWMPSliderCtrl;
begin
  Result := CreateComObject(CLASS_WMPSliderCtrl) as IWMPSliderCtrl;
end;

Class Function CoWMPSliderCtrl.CreateRemote(const MachineName: string): IWMPSliderCtrl;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_WMPSliderCtrl) as IWMPSliderCtrl;
end;

constructor TEvsWMPSliderCtrl.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  OnInvoke:=EventSinkInvoke;
  fServer:=CoWMPSliderCtrl.Create;
  Connect(fServer,IWMPSliderCtrlEvents);
end;

procedure TEvsWMPSliderCtrl.EventSinkInvoke(Sender: TObject; DispID: Integer;
  const IID: TGUID; LocaleID: Integer; Flags: Word; Params: tagDISPPARAMS;
  VarResult, ExcepInfo, ArgErr: Pointer);
begin
  case DispID of
    5430: if assigned(Onondragbegin) then
          Onondragbegin(Self);
    5431: if assigned(Onondragend) then
          Onondragend(Self);
    5432: if assigned(Ononpositionchange) then
          Ononpositionchange(Self);

  end;
end;

Class Function CoWMPVideoCtrl.Create: IWMPVideoCtrl;
begin
  Result := CreateComObject(CLASS_WMPVideoCtrl) as IWMPVideoCtrl;
end;

Class Function CoWMPVideoCtrl.CreateRemote(const MachineName: string): IWMPVideoCtrl;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_WMPVideoCtrl) as IWMPVideoCtrl;
end;

constructor TEvsWMPVideoCtrl.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  OnInvoke:=EventSinkInvoke;
  fServer:=CoWMPVideoCtrl.Create;
  Connect(fServer,IWMPVideoCtrlEvents);
end;

procedure TEvsWMPVideoCtrl.EventSinkInvoke(Sender: TObject; DispID: Integer;
  const IID: TGUID; LocaleID: Integer; Flags: Word; Params: tagDISPPARAMS;
  VarResult, ExcepInfo, ArgErr: Pointer);
begin
  case DispID of
    5720: if assigned(Ononvideostart) then
          Ononvideostart(Self);
    5721: if assigned(Ononvideoend) then
          Ononvideoend(Self);

  end;
end;

Class Function CoWMPEffects.Create: IWMPEffectsCtrl;
begin
  Result := CreateComObject(CLASS_WMPEffects) as IWMPEffectsCtrl;
end;

Class Function CoWMPEffects.CreateRemote(const MachineName: string): IWMPEffectsCtrl;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_WMPEffects) as IWMPEffectsCtrl;
end;

Class Function CoWMPEqualizerSettingsCtrl.Create: IWMPEqualizerSettingsCtrl;
begin
  Result := CreateComObject(CLASS_WMPEqualizerSettingsCtrl) as IWMPEqualizerSettingsCtrl;
end;

Class Function CoWMPEqualizerSettingsCtrl.CreateRemote(const MachineName: string): IWMPEqualizerSettingsCtrl;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_WMPEqualizerSettingsCtrl) as IWMPEqualizerSettingsCtrl;
end;

Class Function CoWMPVideoSettingsCtrl.Create: IWMPVideoSettingsCtrl;
begin
  Result := CreateComObject(CLASS_WMPVideoSettingsCtrl) as IWMPVideoSettingsCtrl;
end;

Class Function CoWMPVideoSettingsCtrl.CreateRemote(const MachineName: string): IWMPVideoSettingsCtrl;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_WMPVideoSettingsCtrl) as IWMPVideoSettingsCtrl;
end;

Class Function CoWMPLibraryTreeCtrl.Create: IWMPLibraryTreeCtrl;
begin
  Result := CreateComObject(CLASS_WMPLibraryTreeCtrl) as IWMPLibraryTreeCtrl;
end;

Class Function CoWMPLibraryTreeCtrl.CreateRemote(const MachineName: string): IWMPLibraryTreeCtrl;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_WMPLibraryTreeCtrl) as IWMPLibraryTreeCtrl;
end;

Class Function CoWMPEditCtrl.Create: IWMPEditCtrl;
begin
  Result := CreateComObject(CLASS_WMPEditCtrl) as IWMPEditCtrl;
end;

Class Function CoWMPEditCtrl.CreateRemote(const MachineName: string): IWMPEditCtrl;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_WMPEditCtrl) as IWMPEditCtrl;
end;

Class Function CoWMPSkinList.Create: IWMPSkinList;
begin
  Result := CreateComObject(CLASS_WMPSkinList) as IWMPSkinList;
end;

Class Function CoWMPSkinList.CreateRemote(const MachineName: string): IWMPSkinList;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_WMPSkinList) as IWMPSkinList;
end;

Class Function CoWMPMenuCtrl.Create: IWMPMenuCtrl;
begin
  Result := CreateComObject(CLASS_WMPMenuCtrl) as IWMPMenuCtrl;
end;

Class Function CoWMPMenuCtrl.CreateRemote(const MachineName: string): IWMPMenuCtrl;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_WMPMenuCtrl) as IWMPMenuCtrl;
end;

Class Function CoWMPAutoMenuCtrl.Create: IWMPAutoMenuCtrl;
begin
  Result := CreateComObject(CLASS_WMPAutoMenuCtrl) as IWMPAutoMenuCtrl;
end;

Class Function CoWMPAutoMenuCtrl.CreateRemote(const MachineName: string): IWMPAutoMenuCtrl;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_WMPAutoMenuCtrl) as IWMPAutoMenuCtrl;
end;

Class Function CoWMPRegionalButtonCtrl.Create: IWMPRegionalButtonCtrl;
begin
  Result := CreateComObject(CLASS_WMPRegionalButtonCtrl) as IWMPRegionalButtonCtrl;
end;

Class Function CoWMPRegionalButtonCtrl.CreateRemote(const MachineName: string): IWMPRegionalButtonCtrl;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_WMPRegionalButtonCtrl) as IWMPRegionalButtonCtrl;
end;

Class Function CoWMPRegionalButton.Create: IWMPRegionalButton;
begin
  Result := CreateComObject(CLASS_WMPRegionalButton) as IWMPRegionalButton;
end;

Class Function CoWMPRegionalButton.CreateRemote(const MachineName: string): IWMPRegionalButton;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_WMPRegionalButton) as IWMPRegionalButton;
end;

constructor TEvsWMPRegionalButton.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  OnInvoke:=EventSinkInvoke;
  fServer:=CoWMPRegionalButton.Create;
  Connect(fServer,IWMPRegionalButtonEvents);
end;

procedure TEvsWMPRegionalButton.EventSinkInvoke(Sender: TObject; DispID: Integer;
  const IID: TGUID; LocaleID: Integer; Flags: Word; Params: tagDISPPARAMS;
  VarResult, ExcepInfo, ArgErr: Pointer);
begin
  case DispID of
    5360: if assigned(Ononblur) then
          Ononblur(Self);
    5361: if assigned(Ononfocus) then
          Ononfocus(Self);
    5362: if assigned(Ononclick) then
          Ononclick(Self);
    5363: if assigned(Onondblclick) then
          Onondblclick(Self);
    5364: if assigned(Ononmousedown) then
          Ononmousedown(Self);
    5365: if assigned(Ononmouseup) then
          Ononmouseup(Self);
    5366: if assigned(Ononmousemove) then
          Ononmousemove(Self);
    5367: if assigned(Ononmouseover) then
          Ononmouseover(Self);
    5368: if assigned(Ononmouseout) then
          Ononmouseout(Self);
    5369: if assigned(Ononkeypress) then
          Ononkeypress(Self);
    5370: if assigned(Ononkeydown) then
          Ononkeydown(Self);
    5371: if assigned(Ononkeyup) then
          Ononkeyup(Self);

  end;
end;

Class Function CoWMPCustomSliderCtrl.Create: IWMPCustomSlider;
begin
  Result := CreateComObject(CLASS_WMPCustomSliderCtrl) as IWMPCustomSlider;
end;

Class Function CoWMPCustomSliderCtrl.CreateRemote(const MachineName: string): IWMPCustomSlider;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_WMPCustomSliderCtrl) as IWMPCustomSlider;
end;

constructor TEvsWMPCustomSliderCtrl.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  OnInvoke:=EventSinkInvoke;
  fServer:=CoWMPCustomSliderCtrl.Create;
  Connect(fServer,IWMPCustomSliderCtrlEvents);
end;

procedure TEvsWMPCustomSliderCtrl.EventSinkInvoke(Sender: TObject; DispID: Integer;
  const IID: TGUID; LocaleID: Integer; Flags: Word; Params: tagDISPPARAMS;
  VarResult, ExcepInfo, ArgErr: Pointer);
begin
  case DispID of
    5020: if assigned(Onondragbegin) then
          Onondragbegin(Self);
    5021: if assigned(Onondragend) then
          Onondragend(Self);
    5022: if assigned(Ononpositionchange) then
          Ononpositionchange(Self);

  end;
end;

Class Function CoWMPTextCtrl.Create: IWMPTextCtrl;
begin
  Result := CreateComObject(CLASS_WMPTextCtrl) as IWMPTextCtrl;
end;

Class Function CoWMPTextCtrl.CreateRemote(const MachineName: string): IWMPTextCtrl;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_WMPTextCtrl) as IWMPTextCtrl;
end;

Class Function CoWMPPlaylistCtrl.Create: IWMPPlaylistCtrl;
begin
  Result := CreateComObject(CLASS_WMPPlaylistCtrl) as IWMPPlaylistCtrl;
end;

Class Function CoWMPPlaylistCtrl.CreateRemote(const MachineName: string): IWMPPlaylistCtrl;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_WMPPlaylistCtrl) as IWMPPlaylistCtrl;
end;

Class Function CoWMPCore.Create: IWMPCore3;
begin
  Result := CreateComObject(CLASS_WMPCore) as IWMPCore3;
end;

Class Function CoWMPCore.CreateRemote(const MachineName: string): IWMPCore3;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_WMPCore) as IWMPCore3;
end;

constructor TEvsWMPCore.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  OnInvoke:=EventSinkInvoke;
  fServer:=CoWMPCore.Create;
  Connect(fServer,_WMPCoreEvents);
end;

procedure TEvsWMPCore.EventSinkInvoke(Sender: TObject; DispID: Integer;
  const IID: TGUID; LocaleID: Integer; Flags: Word; Params: tagDISPPARAMS;
  VarResult, ExcepInfo, ArgErr: Pointer);
begin
  case DispID of
    5001: if assigned(OnOpenStateChange) then
          OnOpenStateChange(Self, OleVariant(Params.rgvarg[0]));
    5101: if assigned(OnPlayStateChange) then
          OnPlayStateChange(Self, OleVariant(Params.rgvarg[0]));
    5102: if assigned(OnAudioLanguageChange) then
          OnAudioLanguageChange(Self, OleVariant(Params.rgvarg[0]));
    5002: if assigned(OnStatusChange) then
          OnStatusChange(Self);
    5301: if assigned(OnScriptCommand) then
          OnScriptCommand(Self, OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    5403: if assigned(OnNewStream) then
          OnNewStream(Self);
    5401: if assigned(OnDisconnect) then
          OnDisconnect(Self, OleVariant(Params.rgvarg[0]));
    5402: if assigned(OnBuffering) then
          OnBuffering(Self, OleVariant(Params.rgvarg[0]));
    5501: if assigned(OnError) then
          OnError(Self);
    5601: if assigned(OnWarning) then
          OnWarning(Self, OleVariant(Params.rgvarg[2]), OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    5201: if assigned(OnEndOfStream) then
          OnEndOfStream(Self, OleVariant(Params.rgvarg[0]));
    5202: if assigned(OnPositionChange) then
          OnPositionChange(Self, OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    5203: if assigned(OnMarkerHit) then
          OnMarkerHit(Self, OleVariant(Params.rgvarg[0]));
    5204: if assigned(OnDurationUnitChange) then
          OnDurationUnitChange(Self, OleVariant(Params.rgvarg[0]));
    5701: if assigned(OnCdromMediaChange) then
          OnCdromMediaChange(Self, OleVariant(Params.rgvarg[0]));
    5801: if assigned(OnPlaylistChange) then
          OnPlaylistChange(Self, OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    5804: if assigned(OnCurrentPlaylistChange) then
          OnCurrentPlaylistChange(Self, OleVariant(Params.rgvarg[0]));
    5805: if assigned(OnCurrentPlaylistItemAvailable) then
          OnCurrentPlaylistItemAvailable(Self, OleVariant(Params.rgvarg[0]));
    5802: if assigned(OnMediaChange) then
          OnMediaChange(Self, OleVariant(Params.rgvarg[0]));
    5803: if assigned(OnCurrentMediaItemAvailable) then
          OnCurrentMediaItemAvailable(Self, OleVariant(Params.rgvarg[0]));
    5806: if assigned(OnCurrentItemChange) then
          OnCurrentItemChange(Self, OleVariant(Params.rgvarg[0]));
    5807: if assigned(OnMediaCollectionChange) then
          OnMediaCollectionChange(Self);
    5808: if assigned(OnMediaCollectionAttributeStringAdded) then
          OnMediaCollectionAttributeStringAdded(Self, OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    5809: if assigned(OnMediaCollectionAttributeStringRemoved) then
          OnMediaCollectionAttributeStringRemoved(Self, OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    5820: if assigned(OnMediaCollectionAttributeStringChanged) then
          OnMediaCollectionAttributeStringChanged(Self, OleVariant(Params.rgvarg[2]), OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    5810: if assigned(OnPlaylistCollectionChange) then
          OnPlaylistCollectionChange(Self);
    5811: if assigned(OnPlaylistCollectionPlaylistAdded) then
          OnPlaylistCollectionPlaylistAdded(Self, OleVariant(Params.rgvarg[0]));
    5812: if assigned(OnPlaylistCollectionPlaylistRemoved) then
          OnPlaylistCollectionPlaylistRemoved(Self, OleVariant(Params.rgvarg[0]));
    5818: if assigned(OnPlaylistCollectionPlaylistSetAsDeleted) then
          OnPlaylistCollectionPlaylistSetAsDeleted(Self, OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    5819: if assigned(OnModeChange) then
          OnModeChange(Self, OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    5821: if assigned(OnMediaError) then
          OnMediaError(Self, OleVariant(Params.rgvarg[0]));
    5823: if assigned(OnOpenPlaylistSwitch) then
          OnOpenPlaylistSwitch(Self, OleVariant(Params.rgvarg[0]));
    5822: if assigned(OnDomainChange) then
          OnDomainChange(Self, OleVariant(Params.rgvarg[0]));
    5824: if assigned(OnStringCollectionChange) then
          OnStringCollectionChange(Self, OleVariant(Params.rgvarg[2]), OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    5825: if assigned(OnMediaCollectionMediaAdded) then
          OnMediaCollectionMediaAdded(Self, OleVariant(Params.rgvarg[0]));
    5826: if assigned(OnMediaCollectionMediaRemoved) then
          OnMediaCollectionMediaRemoved(Self, OleVariant(Params.rgvarg[0]));

  end;
end;

end.
