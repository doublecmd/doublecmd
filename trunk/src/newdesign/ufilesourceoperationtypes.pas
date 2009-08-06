unit uFileSourceOperationTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type

  // Capabilities.
  // (or make a separate type TFileSourceCapability with fsc... ?)
  TFileSourceOperationType = (
    fsoList,
    fsoCopyIn,
    fsoCopyOut,
    fsoDelete,
    fsoWipe,
    fsoCreateDirectory,
    //fsoCreateFile,
    //fsoCreateLink,
    fsoCalcChecksum,
    fsoCalcStatistics,  // Should probably always be supported if fsoList is supported.
    fsoSetName,
    fsoSetAttribute,
    fsoExecute
    //fsoSetPath / fsoChangePath
  );

  TFileSourceOperationTypes = set of TFileSourceOperationType;

implementation

end.

