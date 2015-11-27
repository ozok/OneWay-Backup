unit UnitFolderCreatePair;

interface

uses Generics.Collections;

type
  TFolderCreatePair = record
    Directory: string;
    Attributes: Integer;
  end;

  TFolderCreatePairs = TList<TFolderCreatePair>;

implementation

end.
