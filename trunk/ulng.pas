{
Seksi Commander
----------------------------
Implementing of localization core

Licence  : GNU GPL v 2.0
Author   : radek.cervinka@centrum.cz

contributors:
 * Pavel Letko (1330-1367),(2200-2211),(2220-2232)
}

unit uLng;

interface

const
//   1-999 - messages
//1000-?   - components (Captions & other stuff )

  clngMsgNotDelete=1;
  clngMsgFileExistsRwrt=2;
  clngMsgFileChangedSave=3;
  clngMsgNewFile=4;
  clngMsgDelFlDr=5;
  clngMsgDelSel=6;
  clngMsgCpFlDr=7;
  clngMsgCpSel=8;
  clngMsgRenFlDr=9;
  clngMsgRenSel=10;
  clngMsgErrForceDir=11;
  clngMsgSelected=12;
  clngMsgPopUpHotAdd=13;
  clngMsgPopUpHotCnf=14;
  clngDlgButtons=15;
  clngSpaceMsg=16;
  clngSelectDir=17;
  clngMarkPlus=18;
  clngMarkMinus=19;
  clngMaskInput=20;
  clngFreeMsg=21;
  clngMsgErrDirExists=22;

  clngSavePosition=30;
  clngPositionSaved=31;

//-----
  clngbutStart=980;
  clngbutCancel=981;
  clngbutClose=982;
  clngbutStop=983;
  clngbutSkip=984;
  clngbutAll=985;
  clngbutFind=986;
//----
  clngActExit=1000;
  clngActView=1001;
  clngActEdit=1002;
  clngActCopy=1003;
  clngActRename=1004;
  clngActMkDir=1005;
  clngActDelte=1006;
  clngActMenu=1007;
  clngActMultiRename=1008;
  clngActRunTerm=1009;

//Column in TListView
  clngColName=1020;
  clngColExt=1021;
  clngColSize=1022;
  clngColDate=1023;
  clngColAttr=1024;

// Menu
// File
  clngMnuFile=1040;
  clngMnuFileLink=1041;
  clngMnuFileSymLink=1042;
  clngMnuFileChAttr=1043;
  clngMnuFileProp=1044;
  clngMnuFileCalc=1045;
  clngMnuFileSplit=1046;
  clngMnuFileCombine=1047;
  clngMnuFileShowSys=1048;
  clngMnuFileCmpCnt=1049;
  clngMnuFileChown=1050;

//Mark
  clngMnuMark=1060;
  clngMnuMarkSelGr=1061;
  clngMnuMarkUnSelGr=1062;
  clngMnuMarkSelAll=1063;
  clngMnuMarkUnSelAll=1064;
  clngMnuMarkInvSel=1065;
  clngMnuMarkCmpDir=1066;

//Commands
  clngMnuCmd=1080;
  clngMnuCmdSearch=1081;
  clngMnuCmdHotDir=1082;
  clngMnuCmdSrcTrg=1083;
  clngMnuCmdSrcEkvTrg=1084;

//Show
  clngMnuShw=1100;
  clngMnuShwName=1101;
  clngMnuShwExt=1102;
  clngMnuShwSize=1103;
  clngMnuShwDate=1104;
  clngMnuShwAttr=1105;
  clngMnuShwRevOrd=1106;
  clngMnuShwReRead=1107;

//Configuration
  clngMnuCnf=1120;
  clngMnuCnfOpt=1121;

//Help

  clngMnuHlp=1140;
  clngMnuHlpAbout=1141;
// dialog Options

  clngDlgOpt=1160;
  clngDlgOptSelLng=1161;
  clngDlgOptTerm=1162;
  clngDlgOptSelDir=1163;
  clngDlgOptLynx=1164;
  clngDlgOptCaseSens=1165;
  clngDlgOptShortFileSize=1166;
  clngDlgOptBehaviourTab=1167;
  clngDlgOptToolsTab=1168;
  clngDlgOptExtEdit= 1169;
  clngDlgOptExtView= 1170;
  clngDlgOptExtDiff= 1171;
  clngDlgOptRunTerm= 1172;
  clngDlgOptFonts=1173;
  clngDlgOptMainFont=1174;
  clngDlgOptEditorFont=1175;
  clngDlgOptViewerFont=1176;
  clngDlgOptSeparateExt=1177;

// MkDir
  clngDlgMkDir=1180;
  clngDlgMkDirInput=1181;

// CopyDlg
  clngDlgCp=1200;
//  clngDlgCpSrc=1201;
  clngDlgCpType=1202;

// MoveDlg
  clngDlgMv=1220;
//  clngDlgMvSrc=1221;
  clngDlgMvType=1222;
  clngDlgDel=1223;

// Editor & viewer
  clngEditFile=1240;
  clngEditPrev=1241;
  clngEditNext=1242;
  clngEditExit=1243;
  clngEditSynt=1245;
  clngEditNew =1246;
  clngEditOpen=1247;
  clngEditSave=1248;
  clngEditSvAs=1249;
  clngEditEdit=1250;
  clngEditUndo=1251;
  clngEditCut =1252;
  clngEditCopy=1253;
  clngEditPast=1254;
  clngEditFind=1255;
  clngEditRplc=1256;
  clngEditImage=1257;
  clngEditStretch=1258;
  clngViewText=1259;
  clngViewBin=1260;
  clngViewHex=1261;
  clngViewWrap=1262;
  clngViewView=1263;
  clngViewAbout=1264;
  clngViewAboutText=1265;
  clngViewSearch=1266;
//viewer find dialog
  clngViewFnd=1267;
  clngViewFndCase=1268;
  clngViewProcFile=1269;
  clngViewGraphics=1270;
  clngViewCpClip=1271;
  clngViewSelectAll=1272;

  // find dialog
  clngFindFile=1280;
  clngFindStandard=1281;
  clngFindAdvanced=1282;
  clngFindFileDir=1283;
  clngFindFileMask=1284;
  clngFindFndInFl=1285;
  clngFindData=1286;
  clngFindCase=1287;
  clngFindDirNoEx=1288;
  clngFindWhereBeg=1289;
  clngFindShowView=1290;
  clngFindScaned=1291;

  // MultiReplace
  clngMrnViewOldName=1330;
  clngMrnViewNewName=1331;
  clngMrnViewFilePath=1332;
  clngMrnPopupNameNext=1333;
  clngMrnPopupName=1334;
  clngMrnPopupNameX=1335;
  clngMrnPopupNameXX=1336;
  clngMrnPopupExtenNext=1337;
  clngMrnPopupExten=1338;
  clngMrnPopupExtenX=1339;
  clngMrnPopupExtenXX=1340;
  clngMrnPopupCounter=1341;
  clngMrnPopupTimeNext=1342;
  clngMrnPopupYear=1343;
  clngMrnPopupMonth=1344;
  clngMrnPopupDay=1345;
  clngMrnPopupHour=1346;
  clngMrnPopupMinute=1347;
  clngMrnPopupSecond=1348;
  clngMrnSaveTitle=1349;
  clngMrnMask=1350;
  clngMrnLabelName=1351;
  clngMrnLabelExten=1352;
  clngMrnFindReplace=1353;
  clngMrnLabelFind=1354;
  clngMrnLabelReplace=1355;
  clngMrnCounter=1356;
  clngMrnLabelStartNb=1357;
  clngMrnLabelInterval=1358;
  clngMrnLabelWidth=1359;
  clngMrnFileStyle=1360;
  clngMrnCmNoChange=1361;
  clngMrnCmUpperCase=1362;
  clngMrnCmLowerCase=1363;
  clngMrnCmFirstBig=1364;
  clngMrnLog=1365;
  clngMrnCheckLog=1366;
  clngMrnBtnRestore=1367;

  // chmod dialog
  clngAttrChmod=1500;
  clngAttrOwner=1501;
  clngAttrGroup=1502;
  clngAttrOther=1503;
  clngAttrRead=1504;
  clngAttrWrite=1505;
  clngAttrExec=1506;
  clngAttrTextRep=1507;
// symlink
  clngSymLink= 1520;
  clngSymLinkDst=1521;
  clngSymLinkNew=1522;
  clngSymErrCreate=1523;
// hardlink
  clngHardLink= 1530;
  clngHardLinkDst=1531;
  clngHardLinkNew=1532;
  clngHardErrCreate=1533;
// shift+f4
  clngShiftF4file=1540;
  clngShiftF4Open=1541;
  clngShiftF4FileName=1542;

// search & replace in editor   
  clngEditSearch=1560;
  clngEditReplace=1561;
  clngEditSearchFor=1562;
  clngEditReplaceWith=1563;
  clngEditSearchFrw=1564;
  clngEditSearchBack=1565;
  clngEditSearchCase=1566;
  clngEditSearchWholeWord=1567;
  clngEditSearchCaret=1568;
  clngEditSearchSelect=1569;

{mate}
//Chown/Chgrp dialog
  clngChownDlg=1580;
  clngChownOwner=1581;
  clngChownGroup=1582;
{/mate}

  clngEditCfg  =1600;
  clngEditCfgForm=1601;
  clngEditCfgSample=1602;
  clngEditCfgBold=1603;
  clngEditCfgItalic=1604;
  clngEditCfgUline=1605;
  clngEditCfgStrike=1606;
  clngEditCfgDefined=1607;
  clngEditCfgLoadOK=1608;

  clngPropsTitle=2000;
  clngPropsYes=2001;
  clngPropsNo=2002;
  clngPropsClose=2003;
  clngPropsNext=2004;
  clngPropsFile=2005;
  clngPropsFolder=2006;
  clngPropsSpBlkDev=2007;
  clngPropsSpChrDev=2008;
  clngPropsNmdPipe=2009;
  clngPropsSymLink=2010;
  clngPropsSocket=2011;
  clngPropsUnknownType=2012;

  clngPropsAttrRead=2013;
  clngPropsAttrWrite=2014;
  clngPropsAttrExec=2015;
  clngPropsAttrSetUID=2016;
  clngPropsAttrSetGID=2017;
  clngPropsAttrSticky=2018;

  clngPropsStrName=2020;
  clngPropsStrPath=2021;
  clngPropsStrType=2022;
  clngPropsStrSize=2023;
  clngPropsStrLastAccess=2024;
  clngPropsStrLastChange=2025;
  clngPropsStrLastStatus=2026;
  clngPropsStrOwner=2027;
  clngPropsStrGroup=2028;
  clngPropsStrSymlink=2029;
  clngPropsStrAttrs=2030;
  clngPropsStrOther=2031;
  clngPropsStrBits=2032;
  clngPropsStrAttrAlt=2033;

  clngCompareDiffs=2101;

// Linker
  clngLinkColumnNameFile=2200;
  clngLinkBtnExit=2201;
  clngButOk=2202;
  clngLinkControlItem=2203;
  clngLinkHintDel=2204;
  clngLinkHintDown=2205;
  clngLinkHintDown2=2206;
  clngLinkHintUp=2207;
  clngLinkHintUp2=2208;
  clngLinkSaveTo=2209;
  clngLinkDialogSave=2210;
  clngLinkMsgOK=2211;

// Splitter
  clngSplitBtnCancel=2220;
  clngSplitBtnOK=2221;
  clngSplitGrBxFile=2222;
  clngSplitLbDir=2223;
  clngSplitLbFile=2224;
  clngSplitGrBxSize=2225;
  clngSplitGrBxWatch=2226;
  clngSplitErrFileSize=2227;
  clngSplitErrDirectory=2228;
  clngSplitErrSplitFile=2229;
  clngSplitMsgCreated=2230;
  clngSplitMsgSize=2231;
  clngSplitSelDir=2232;

procedure lngLoadLng(const sFileName:String);
function lngGetString (id:Integer):String;
procedure DoLoadLng;

implementation
uses
  Classes, SysUtils, uGlobs, uGlobsPaths;
var
  strLngList:TStringList=nil;

procedure lngLoadLng(const sFileName:String);
var
  lngFile:TextFile;
  s:String;
  sId:String;
begin
  writeln('Loading lng file:',sFileName);
  strLngList:=TStringList.Create;
  assign(lngFile,sFileName);
  reset(lngFile);
  while not eof(lngFile) do
  begin
    Readln(lngFile,s);
    s:=Trim(s);
    if (s='') or (s[1]=';') then Continue; //; is remark
    sId:=Copy(s,1,pos(':',s)-1);
    s:=Copy(s,pos(':',s)+1,length(s)-length(sId));
    strLngList.AddObject(s,TObject(StrToInt(sId)));
  end;
  closefile(lngFile);
end;

function lngGetString (id:Integer):String;
var
  i:Integer;
begin
// in first use load lng list

  Result:='';
  for i:=0 to strLngList.Count-1 do
     if Integer(strLngList.Objects[i])=id then
     begin
       Result:=strLngList.Strings[i];
       Break;
     end;
  if Result='' then
    Result:=Format('Error in lng file for id=%d!',[id]);
end;

procedure DoLoadLng;
begin
  if not Assigned(strLngList) then
    lngLoadLng(gpLngDir+gLng);
end;

initialization
//  lngLoadLng(gpLngDir+gLng);
finalization
  if assigned(strLngList) then
    FreeAndNil(strLngList);
end.
