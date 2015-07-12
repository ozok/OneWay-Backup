unit UnitFileCopyPair;

interface

uses Generics.Collections;

type
  TFileCopyPair = record
    SourceFile: string;
    DestFile: string;
  end;

  TFileCopyPairs = TList<TFileCopyPair>;

implementation

end.
