{
Seksi Commander
----------------------------
Licence  : GNU GPL v 2.0
Author   : radek.cervinka@centrum.cz

Main Dialog window and other stuff

contributors:


}

unit uTypes;

interface
type

// what is showed in panel
  TPanelMode= (pmDirectory, pmArchive, pmVFS);

// plugin types
  TPluginType = (ptDSX, ptWCX, ptWDX, ptWFX, ptWLX);

  //base structure for storing file informations
  TFileRecItem = record
//      iType       : Integer;
      bIsLink     : Boolean;
      bLinkIsDir  : Boolean;
      sLinkTo     : AnsiString;
      sName       : AnsiString;
      sNameNoExt  : AnsiString;
      sExt        : AnsiString;
      iSize       : Int64;
      fTimeI      : Double;
      sTime       : AnsiString;
      bExecutable : Boolean;
      sModeStr    : String[11];
      iMode       : Cardinal; // from stats
      bSysFile    : Boolean;
      iIconID     : Integer; // index ICO in imagelist
      sOwner      : AnsiString;
      sGroup      : AnsiString;
{mate}
      iOwner      : Cardinal;
      iGroup      : Cardinal;
{/mate}
      sPath       : AnsiString; // used only in block operation
      bSelected   : Boolean;
      iDirSize    : Int64;
      pContainer  : Pointer; // used in sorting function
  end;

  PFileRecItem=^TFileRecItem;

implementation

end.
