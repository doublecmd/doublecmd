unit uFileSourceProperty;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type

  TFileSourceProperty = (

    {en
       Set, if the files are available directly (for example: real file system).
       Not sure what it would do yet, but I'll leave it for now.
    }
    fspDirectAccess,

    {en
       Set, if filenames are case sensitive.
    }
    fspCaseSensitive,

    {en
       Set, if the file source has virtual files
       (like a VFS list, or results from searching, etc.).
       Non-virtual files are all files that are physical
       (regardless if they are directly accessible).
    }
    fspVirtual,

    {en
       Set, if the files are links to local files that available directly
       (for example: results from searching, etc.).
    }
    fspLinksToLocalFiles,

    {en
       Set, if the file source uses TFileSourceConnection objects
       for access by operations.
    }
    fspUsesConnections,
    {en
       Set, if the file source supports file listing in main thread only.
    }
    fspListInMainThread

  );

  TFileSourceProperties = set of TFileSourceProperty;

implementation

end.

