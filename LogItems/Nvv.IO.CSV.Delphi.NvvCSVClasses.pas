(*****************************************************
 ***    Copyright (c) 2014 Vladimir Nikitenko      ***
 ***      Code Project Open License (CPOL)         ***
 *** (http://www.codeproject.com/info/cpol10.aspx) ***
 *****************************************************)

 // Version 2.0

unit Nvv.IO.CSV.Delphi.NvvCSVClasses;

interface

uses Classes, Generics.Collections, SysUtils;

{ Classes TnvvCSVReader, TnvvCSVFileReader and TnvvCSVStringReader work with
  CSV formatted data as it defined in “RFC 4180. Common Format and MIME Type for
  Comma-Separated Values (CSV) Files”: http://tools.ietf.org/html/rfc4180
  According to Wikipedia, “A general standard for the CSV file format does not
  exist, but RFC 4180 provides a de facto standard for some aspects of it.”

  While mentioned classes are RFC4180-compliant, they provide (public) properties
  that allow handling of other existing variations of CSV format as well. }

type

  TnvvCSVField = class(TObject)
    private
      FValue : string; {field value in original format, i.e. string - as it is in CSV file}
      FName : string;
    public
      constructor Create; virtual;
      property Value: string read FValue write FValue;
      property Name: string read FName write FName;
  end;


  TnvvCSVReader = class(TObject)
  private
    type
      TCSVParserState = ( psChkBegOfRecord, psChkBegOfField, psNotQuoted, psQuoted,
                          psChkIntrnQuote, psChkAfterCR );
      TCSVParserEvent = ( peDataChar, peQuote, peFieldSeparator, peCR, peLF, peEof );

  private
    FActive: Boolean;  //true when file is Open
    FFields: TObjectList<TnvvCSVField>;
    FCSVTextReader: TTextReader;
    FBof: Boolean;
    FEof: Boolean;
    FHeaderPresent: Boolean;
    FReadingHeader: Boolean;
    FFlag_ExitMainLoop: Boolean;
    FIndexOfLastProcessedField: Integer; //zero-based
    FIndexOfLastProcessedRecord: Integer; //zero-based
    FBuffer_FieldValue: TCharArray;
    FBufFldVal_Capacity: Integer;
    FBufFldVal_Content_Length: Integer;

    FParserState: TCSVParserState;
    FBuffer_ReadFromFile: Char;
    FIgnoreSpecialCharacters: Boolean;
    FUseFieldQuoting: Boolean;
    FIgnoreEmptyLines: Boolean;
    FFieldCount_AutoDetect: Boolean;
    FFieldCount_AutoDetect_InProgress: Boolean;
    FFieldSeparatorChar: Char;
    FQuoteChar: Char;
    FASCIIonly: Boolean;
    FOnFieldCountAutoDetectComplete : TNotifyEvent;

    FBufferReadFromStream: TCharArray;
    FBufferReadFromStream_Capacity: Integer;
    FBufferReadFromStream_ContentLength: Integer;
    FBufferReadFromStream_NextCharIndex: Integer;

    procedure DoOpen;
    procedure DoClose;
    procedure OpenCSVSourceAndCreateStream;
    procedure CloseCSVSourceAndDestroyStream;
    procedure ParsingLoop_Main;
    procedure ClearFieldDataInaccessibleWhileFileClosed;
    procedure ClearFieldDataInaccessibleWhileEof;
    procedure Reset_for_Close;
    procedure Reset_for_EndOfFile;
    procedure Reset_for_NextRecord;
    procedure Reset_for_NextField;
    procedure DoEndOfFile;
    procedure AddToFieldValueBuffer;
    procedure Handle_QuoteAtBegOfValue( var ASwitchToState: TCSVParserState );
    procedure Handle_EndOfLine;
    procedure Handle_EmptyLine;
    procedure Handle_EndOfFileAtBegOfRecord;
    procedure Handle_EndOfFileAfterLastFieldInRecord;
    procedure SetFieldCount_AutoDetect_InProgress(const Value: Boolean);
    procedure SetHeaderPresent(const Value: Boolean);
    procedure SetFieldCount_AutoDetect(const Value: Boolean);
    function GetFieldCount: Integer;
    procedure SetFieldCount(const Value: Integer);
    function GetFieldSeparatorCharCode: Integer;
    procedure SetFieldSeparatorCharCode(const Value: Integer);
    procedure SetUseFieldQuoting(const Value: Boolean);
    function GetQuoteCharCode: Integer;
    procedure SetQuoteCharCode(const Value: Integer);
    procedure SetIgnoreEmptyLines(const Value: Boolean);
    procedure SetASCIIonly(const Value: Boolean);
    procedure SetIgnoreSpecialCharacters(const Value: Boolean);
    procedure SetActive(const Value: Boolean);
    function GetFields(Index: Integer): TnvvCSVField;
    function GetRecordCountProcessedSoFar: Integer;

    property FieldCount_AutoDetect_InProgress: Boolean
      read FFieldCount_AutoDetect_InProgress
      write SetFieldCount_AutoDetect_InProgress;

  protected
    const
      CR = #$0D;
      LF = #$0A;
      DQUOTE = #$22;  // double quote
      COMMA = #$2C;
      CharCodeHex20 = #$20;
      CharCodeHex7E = #$7E;

      MsgStr_OperationNotAllowedInActiveState = 'Operation is not allowed in Active state';
      MsgStr_OperationNotAllowedInInactiveState = 'Operation is not allowed in Inactive state';
      MsgStr_WrongNumberOfFields = 'Wrong number of fields.';
      MsgStr_WrongFieldValueFormat = 'Wrong format of field value.';

    {Derived classes must override/implement CreateDataSourceReader returning
     instance of appropriate reader: TStreamReader if CSV data is in a file,
     TStringReader if is in string and so on}
    function CreateDataSourceReader: TTextReader; virtual; abstract;

    procedure DoEndOfField; virtual;
    procedure DoEndOfLine; virtual;
    function AllowFieldCountChangeEvenInOpenState: Boolean; virtual;
    procedure OnFieldCountAutoDetectCompleted; virtual;

    procedure Throw_ErrorWithNoParam( AMsg: string );
    procedure Throw_ErrorWithRecordAndFieldNum( AMsg: string );

    property CSVTextReader: TTextReader read FCSVTextReader;

  public
    constructor Create( ABufferReadFromStreamCapacityInChars: Integer = 512 ); virtual;
    destructor Destroy; override;

    procedure Open;
    procedure Close;
    procedure Next;

    // -------------- Input parameters (begin)
    // Attempt to change input parameter values in active state causes exception

    { HeaderPresent:
      - False (by default): All records are considered as records with values.
      - True: Values in the very first record are considered as field names. }
    property HeaderPresent: Boolean read FHeaderPresent write SetHeaderPresent;

    { FieldCount auto detection is done during/within Open on the base of very
      first record (or on the base of first non-empty record when IgnoreEmptyLines=true).
      "Autodetected" FieldCount is accessible after Open (Active := true) is complete }
    property FieldCount_AutoDetect: Boolean
      read FFieldCount_AutoDetect write SetFieldCount_AutoDetect;

    { FieldCount is always >= 0. Attempt to assign negative value is ignored.
      If FieldCount_AutoDetect is true then assigned value is meaningless and
      will be replaced during Open(). }
    property FieldCount: Integer read GetFieldCount write SetFieldCount;

    { FieldSeparatorCharCode:
      - It is a code (!) and not a char. Virtually any (see also property ASCIIonly)
        unicode character including special characters like TAB, etc. can be
        used as field separator.}
    property FieldSeparatorCharCode: Integer
      read GetFieldSeparatorCharCode write SetFieldSeparatorCharCode;

    { UseFieldQuoting
      - False: In this case it is assumed that "quote char" is never used for field
               value surrounding and is considered as ordinary data character
               provided that code is in data char code range, i.e. not special
               character. In other words, value specified as QuoteCharCode (see below)
               is meaningless.
      - True (by default): Field value may or may not be enclosed in characters
                           specified in QuoteCharCode (see below). }
    property UseFieldQuoting: Boolean read FUseFieldQuoting write SetUseFieldQuoting;

    { QuoteCharCode:
      - It is a code (!) and not a char. Virtually any (see also property ASCIIonly)
        unicode character can be used as “quote char”. It is assumed that this
        character is also used as escape character }
    property QuoteCharCode: Integer read GetQuoteCharCode write SetQuoteCharCode;

    { IgnoreEmptyLines:
      - False (by default): Presence of empty lines in source, which is
        indication of wrong input data format, causes exception.
      - True: Empty lines are ignored. }
    property IgnoreEmptyLines: Boolean read FIgnoreEmptyLines write SetIgnoreEmptyLines;

    { ASCIIOnly:
      - False (by default): Full Unicode range of characters is handled. Characters
                            with codes less than 0x20 are considered as “special characters”
                            (see property IgnoreSpecialCharacters below).
      - True: Only ASCII range of characters is handled. Characters with codes
              outside range 0x20 – 0x7E are considered as “special characters”
              (see property IgnoreSpecialCharacters below). }
    property ASCIIonly: Boolean read FASCIIonly write SetASCIIonly;

    { IgnoreSpecialCharacters
      - False (by default): Presence of “special characters”, as they defined above
                            in property ASCIIonly description, causes exception.
                            This does not affect line breaks, field separator and
                            quote characters even if last two are from the “special
                            character” range.
      - True: “Special characters” are ignored except line breaks, field separator
              and quote characters even if last two are from the “special character” range. }
    property IgnoreSpecialCharacters: Boolean
      read FIgnoreSpecialCharacters write SetIgnoreSpecialCharacters;
   // ------------------- Input parameters (end)

    property Active: Boolean read FActive write SetActive;
    property Bof: Boolean read FBof;
    property Eof: Boolean read FEof;

    {Fields is "zero-based" ( 0 <= Index <= FieldCount-1)}
    property Fields[Index: Integer]: TnvvCSVField read GetFields;

    property RecordCountProcessedSoFar: Integer read GetRecordCountProcessedSoFar;

    property OnFieldCountAutoDetectComplete : TNotifyEvent
               read FOnFieldCountAutoDetectComplete
               write FOnFieldCountAutoDetectComplete;
  end; {TnvvCSVReader class}



  TStreamReaderConstructorKind = ( srckFile,  srckFileBOM, srckFileEncodingBOMBuffsize );


  TnvvCSVFileReader = class(TnvvCSVReader)
  private
    FFileName: string;
    FStreamReader_Encoding: TEncoding;
    FStreamReader_DetectBOM: Boolean;
    FStreamReader_InternBufferSize: Integer;
    FStreamReader_ConstructorKind: TStreamReaderConstructorKind;
  protected
    function CreateDataSourceReader: TTextReader; override;
  public
    constructor Create( ABufferReadFromStreamCapacityInChars: Integer = 512 ); override;

    procedure SetFile( const AFileName: string ); overload;
    procedure SetFile( const AFileName: string; ADetectBOM: Boolean ); overload;
    procedure SetFile( const AFileName: string; AEncoding: TEncoding;
      ADetectBOM: Boolean = False; AStreamReaderInternBufferSize: Integer = 1024 ); overload;

    property FileName: string read FFileName;
    property StreamReader_Encoding: TEncoding read FStreamReader_Encoding;
    property StreamReader_DetectBOM: Boolean read FStreamReader_DetectBOM;
    property StreamReader_InternBufferSize: Integer read FStreamReader_InternBufferSize;
    property StreamReader_ConstructorKind: TStreamReaderConstructorKind
                                             read FStreamReader_ConstructorKind;
  end;


  TnvvCSVStringReader = class(TnvvCSVReader)
  private
    FDataString: string;
    procedure SetDataString(const Value: string);
  protected
    function CreateDataSourceReader: TTextReader; override;
  public
    constructor Create( ABufferReadFromStreamCapacityInChars: Integer = 512 ); override;

    property DataString: string read FDataString write SetDataString;
  end;


implementation

{ TnvvCSVField }

constructor TnvvCSVField.Create;
begin
  FValue := '';
  FName := '';
end;

{ TnvvCSVReader }

const
  cBufFldValCapacityIncrement = 128;

constructor TnvvCSVReader.Create( ABufferReadFromStreamCapacityInChars: Integer = 512 );
begin
  // Input property default values
  FHeaderPresent := False;
  FFieldCount_AutoDetect := True;
  FFieldSeparatorChar := COMMA;
  FUseFieldQuoting := True;
  FQuoteChar := DQUOTE;
  FIgnoreEmptyLines := False;
  FASCIIonly := False;
  FIgnoreSpecialCharacters := False;

  FActive := False;
  FBof := False;
  FEof := False;

  FReadingHeader := False;
  FFlag_ExitMainLoop := False;

  FFields := TObjectList<TnvvCSVField>.Create(True {owns Objects !!!});
  FCSVTextReader := nil;
  FBufFldVal_Capacity := 0;
  FBufFldVal_Content_Length := 0;

  if (ABufferReadFromStreamCapacityInChars > 0) then
    FBufferReadFromStream_Capacity := ABufferReadFromStreamCapacityInChars
  else
    FBufferReadFromStream_Capacity := 512;
  SetLength(FBufferReadFromStream, FBufferReadFromStream_Capacity);
  FBufferReadFromStream_ContentLength := 0;
  FBufferReadFromStream_NextCharIndex := 0;
end;

destructor TnvvCSVReader.Destroy;
begin
  Close; {will destroy TextReader if necessary}
  FBufferReadFromStream := nil;
  FBuffer_FieldValue := nil;
  FFields.Free; {will destroy all objects because owns them}
  inherited;
end;

procedure TnvvCSVReader.DoOpen;
begin
  try
    Reset_for_Close;//initial state is set here
    OpenCSVSourceAndCreateStream;
    FBof := true;
    FEof := ( FCSVTextReader.Peek < 0 );

    FieldCount_AutoDetect_InProgress := FieldCount_AutoDetect and (not Eof);
    if (HeaderPresent and (not Eof)) then
    begin
        FReadingHeader := true;
        try
          Next;
        finally
          FReadingHeader := false;
        end;
    end;
    if (not Eof) then
      Next;
  except
    DoClose;
    raise;
  end;{try-except}
end;{DoOpen}

procedure TnvvCSVReader.DoClose;
begin
  Reset_for_Close;
end;

procedure TnvvCSVReader.OpenCSVSourceAndCreateStream;
begin
  CloseCSVSourceAndDestroyStream;
  FCSVTextReader := CreateDataSourceReader;
end;

procedure TnvvCSVReader.CloseCSVSourceAndDestroyStream;
begin
  FCSVTextReader.Free;
  FCSVTextReader := nil;
end;

procedure TnvvCSVReader.ParsingLoop_Main;
var
  lParserEvent: TCSVParserEvent;
  lReadingComplete: Boolean;
  lCharAsInteger: Integer;
  lSwitchToState: TCSVParserState;
begin
  while (not FFlag_ExitMainLoop) do
  begin
    // GetEvent - begin
    lParserEvent := peEof; //just to init it with something (EndOfFile makes most sense)
    repeat
      lReadingComplete := true;

      //Get next char from FbufferReadFromStream. Read into FbufferReadFromStream from TextReader when necessary === BEGIN
      lCharAsInteger := 0; //any not negative
      if (FBufferReadFromStream_NextCharIndex >= FBufferReadFromStream_ContentLength) then
      begin
        FBufferReadFromStream_NextCharIndex := 0;
        FBufferReadFromStream_ContentLength :=
          FCSVTextReader.Read(FBufferReadFromStream, 0, FBufferReadFromStream_Capacity);
        if (FBufferReadFromStream_ContentLength <= 0) then //compare "= 0" just in case
        begin
          FBufferReadFromStream_ContentLength := 0;
          lCharAsInteger := -1;
        end;
      end;
      if not(lCharAsInteger < 0) then
      begin
        lCharAsInteger := Integer(FBufferReadFromStream[FBufferReadFromStream_NextCharIndex]);
        Inc(FBufferReadFromStream_NextCharIndex);
      end;
      //Get next char from FbufferReadFromStream === END

      if (lCharAsInteger <= 0) then
      begin
        // reached end of the stream
        FBuffer_ReadFromFile := Char(0);  // just in case put "empty" char there
        lParserEvent := peEof;
       end
      else
      begin
        FBuffer_ReadFromFile := Char(lCharAsInteger);
        if (FBuffer_ReadFromFile = CR) then
          lParserEvent := peCR
        else if (FBuffer_ReadFromFile = LF) then
          lParserEvent := peLF
        else if (FBuffer_ReadFromFile = FFieldSeparatorChar) then
          lParserEvent := peFieldSeparator
        else if (FBuffer_ReadFromFile = FQuoteChar) then
          lParserEvent := peQuote
        else if ( (FBuffer_ReadFromFile >= CharCodeHex20) and
                  ( (not ASCIIonly) or (ASCIIonly and  (FBuffer_ReadFromFile <= Char(CharCodeHex7E))) ) ) then
          lParserEvent := peDataChar
        else if (IgnoreSpecialCharacters) then
          lReadingComplete := false
        else
          Throw_ErrorWithRecordAndFieldNum( Format(
                      'Invalid character (HexCode = %.8x). Input file is invalid.',
                      [Integer(FBuffer_ReadFromFile)] ));
      end;
    until (lReadingComplete);
    // GetEvent - end

    lSwitchToState := psChkBegOfRecord; // init just to get rid of possible compiler's warning
    case FParserState of
      psChkBegOfRecord:
        case lParserEvent of
          peDataChar:
          begin
            AddToFieldValueBuffer;
            FParserState := psNotQuoted;
          end;
          peQuote:
          begin
            Handle_QuoteAtBegOfValue( lSwitchToState );
            FParserState := lSwitchToState;
          end;
          peFieldSeparator:
          begin
            DoEndOfField();
            FParserState := psChkBegOfField;
          end;
          peCR:
          begin
            // empty line
            Handle_EmptyLine;
            FParserState := psChkAfterCR;
          end;
          peLF:
            //empty line that delimited by single LF
            Handle_EmptyLine;
            //not changing state
          peEof:
            //Possible situations:
            //    1. Last line of file has line break before Eof.
            //    2. Last line without line break before Eof  containing empty value in
            //    the file with one field per record.
            //    There is no way to differentiate them. Therefore let's assume that
            //    it is always situation "1".
            Handle_EndOfFileAtBegOfRecord;
            // Not changing state. BTW it does not matter here
        end;{case lParserEvent}
      psChkBegOfField:
        case lParserEvent of
          peDataChar:
          begin
            AddToFieldValueBuffer;
            FParserState := psNotQuoted;
          end;
          peQuote:
          begin
            Handle_QuoteAtBegOfValue( lSwitchToState );
            FParserState := lSwitchToState;
          end;
          peFieldSeparator:
            //empty value
            DoEndOfField;
            //Not changing state
          peCR:
          begin
            //empty value
            Handle_EndOfLine;
            FParserState := psChkAfterCR;
          end;
          peLF:
          begin
            Handle_EndOfLine;
            FParserState := psChkBegOfRecord;
          end;
          peEof:
          begin
            //empty value
            Handle_EndOfFileAfterLastFieldInRecord;
            FParserState := psChkBegOfRecord; //actually does not matter
          end;
        end;{case lParserEvent}
      psNotQuoted:
        case lParserEvent of
          peDataChar,
          peQuote:
            AddToFieldValueBuffer;
            //Not changing state
          peFieldSeparator:
          begin
            DoEndOfField;
            FParserState := psChkBegOfField;
          end;
          peCR:
          begin
            Handle_EndOfLine;
            FParserState := psChkAfterCR;
          end;
          peLF:
          begin
            Handle_EndOfLine;
            FParserState := psChkBegOfRecord;
          end;
          peEof:
          begin
            Handle_EndOfFileAfterLastFieldInRecord;
            FParserState := psChkBegOfRecord;
          end;
        end;{case lParserEvent}
      psQuoted:
        case lParserEvent of
          peDataChar,
          peFieldSeparator,
          peCR,
          peLF:
            AddToFieldValueBuffer;
            //Not changing state
          peQuote:
            FParserState := psChkIntrnQuote;
          peEof:
          begin
            FParserState := psChkBegOfRecord; //probably does not matter
            Throw_ErrorWithRecordAndFieldNum(MsgStr_WrongFieldValueFormat);
          end;
        end;{case lParserEvent}
      psChkIntrnQuote:
        case lParserEvent of
          peDataChar:
          begin
            {if quote was closing quote then here we expect field separator,
             end of line or end of file}
            FParserState := psChkBegOfRecord;  //probably does not matter
            Throw_ErrorWithRecordAndFieldNum(MsgStr_WrongFieldValueFormat);
          end;
          peQuote:
          begin
            //internal quote character
            AddToFieldValueBuffer;
            FParserState := psQuoted;
          end;
          peFieldSeparator:
          begin
            DoEndOfField;
            FParserState := psChkBegOfField;
          end;
          peCR:
          begin
            Handle_EndOfLine;
            FParserState := psChkAfterCR;
          end;
          peLF:
          begin
            Handle_EndOfLine;
            FParserState := psChkBegOfRecord;
          end;
          peEof:
          begin
            Handle_EndOfFileAfterLastFieldInRecord;
            FParserState := psChkBegOfRecord;
          end;
        end;{case lParserEvent}
      psChkAfterCR:
        case lParserEvent of
          peDataChar:
          begin
            AddToFieldValueBuffer;
            FParserState := psNotQuoted;
          end;
          peQuote:
          begin
            Handle_QuoteAtBegOfValue( lSwitchToState );
            FParserState := lSwitchToState;
          end;
          peFieldSeparator:
          begin
            //empty value
            DoEndOfField;
            FParserState := psChkBegOfField;
          end;
          peCR:
            //empty line
            Handle_EmptyLine;
            //Not changing state
          peLF:
            //Line delimiter <CR><LF>
            FParserState := psChkBegOfRecord;
          peEof:
          begin
            //Same situation as in state psChkBegOfRecord (see more in State_BegOfRecord_Handler)
            Handle_EndOfFileAtBegOfRecord;
            FParserState := psChkBegOfRecord; //actually does not matter
          end;
        end;{case lParserEvent}
    end; {case FParserState}
  end; {while (not FFlag_ExitMainLoop)}
end;{ParsingLoop_Main}

procedure TnvvCSVReader.ClearFieldDataInaccessibleWhileFileClosed;
var
  i: Integer;
begin
  ClearFieldDataInaccessibleWhileEof;
  for i := 0 to FieldCount-1 do
    Fields[i].Name := '';
end;

procedure TnvvCSVReader.ClearFieldDataInaccessibleWhileEof;
var
  i: Integer;
begin
  for i := 0 to FieldCount-1 do
    Fields[i].Value := '';
end;

procedure TnvvCSVReader.Reset_for_Close;
begin
  FBufFldVal_Capacity := 0;
  SetLength(FBuffer_FieldValue, FBufFldVal_Capacity);
  FBufFldVal_Content_Length := 0;//just in case

  FieldCount_AutoDetect_InProgress := False;
  Reset_for_EndOfFile;
  Reset_for_NextRecord;
  Reset_for_NextField;
  ClearFieldDataInaccessibleWhileFileClosed;
  FIndexOfLastProcessedRecord := -1; //zero-based
  FBof := False;
  FEof := False;
  FParserState := psChkBegOfRecord;
end;

procedure TnvvCSVReader.Reset_for_EndOfFile;
begin
  CloseCSVSourceAndDestroyStream;
  ClearFieldDataInaccessibleWhileEof;
end;

procedure TnvvCSVReader.Reset_for_NextRecord;
begin
  FIndexOfLastProcessedField := -1;
  Inc(FIndexOfLastProcessedRecord);
end;

procedure TnvvCSVReader.Reset_for_NextField;
begin
  FBufFldVal_Content_Length := 0;
end;

procedure TnvvCSVReader.DoEndOfFile;
begin
  FEof := True;
  Reset_for_EndOfFile;
  FFlag_ExitMainLoop := True;
end;

procedure TnvvCSVReader.AddToFieldValueBuffer;
begin
  if (FBufFldVal_Content_Length >= FBufFldVal_Capacity) then
  begin
    FBufFldVal_Capacity := FBufFldVal_Capacity + cBufFldValCapacityIncrement;
    SetLength(FBuffer_FieldValue, FBufFldVal_Capacity);
  end;
  FBuffer_FieldValue[FBufFldVal_Content_Length] := FBuffer_ReadFromFile;
  Inc(FBufFldVal_Content_Length);
end;

procedure TnvvCSVReader.Handle_QuoteAtBegOfValue(var ASwitchToState: TCSVParserState);
begin
  if (UseFieldQuoting) then
    ASwitchToState := psQuoted
  else
  begin
    AddToFieldValueBuffer;
    ASwitchToState := psNotQuoted;
  end;
end;

procedure TnvvCSVReader.Handle_EndOfLine;
begin
  // in exact order:
  DoEndOfField;
  DoEndOfLine;
end;

procedure TnvvCSVReader.Handle_EmptyLine;
begin
  if (not IgnoreEmptyLines) then
    Handle_EndOfLine;
end;

procedure TnvvCSVReader.Handle_EndOfFileAtBegOfRecord;
begin
  DoEndOfFile;
end;

procedure TnvvCSVReader.Handle_EndOfFileAfterLastFieldInRecord;
begin
  { We cannot handle this situation as Eof (and clear field values) because
    current Next is reading this last record and it is not read by user yet.
    Therefore let's delay setting of "our" Eof until next Next.
    In other words, we are simulating end of this line and Eof at the beginning
    of "next empty" line instead. As a result, we will always "have" situation
    that last line has CR/LF. }
  Handle_EndOfLine;
end;

procedure TnvvCSVReader.SetFieldCount_AutoDetect_InProgress(const Value: Boolean);
begin
  if (FFieldCount_AutoDetect_InProgress <> Value) then
  begin
    FFieldCount_AutoDetect_InProgress := Value;
    if (FFieldCount_AutoDetect_InProgress) then
      FieldCount := 0;
  end;
end;

procedure TnvvCSVReader.DoEndOfField;
var lCsvVal: string;
begin
  if (FieldCount_AutoDetect_InProgress) then
    FieldCount := FieldCount + 1;

  if ( FIndexOfLastProcessedField >= (FieldCount-1) ) then
    Throw_ErrorWithRecordAndFieldNum(MsgStr_WrongNumberOfFields)
  else
  begin
    Inc(FIndexOfLastProcessedField);

    SetString( lCsvVal, PChar(@FBuffer_FieldValue[0]), FBufFldVal_Content_Length );

    if (FReadingHeader) then
      Fields[FIndexOfLastProcessedField].Name := lCsvVal
    else
      Fields[FIndexOfLastProcessedField].Value := lCsvVal;
    Reset_for_NextField;
  end;
end;

procedure TnvvCSVReader.DoEndOfLine;
begin
  if (FIndexOfLastProcessedField <> (FieldCount - 1)) then
    Throw_ErrorWithRecordAndFieldNum(MsgStr_WrongNumberOfFields)
  else
  begin
    Reset_for_NextRecord;
    FFlag_ExitMainLoop := True;
  end;

  if (FieldCount_AutoDetect_InProgress) then
  begin
    FieldCount_AutoDetect_InProgress := False;
    OnFieldCountAutoDetectCompleted;
  end;
end;

function TnvvCSVReader.AllowFieldCountChangeEvenInOpenState: Boolean;
begin
  Result := FieldCount_AutoDetect_InProgress;
end;

procedure TnvvCSVReader.OnFieldCountAutoDetectCompleted;
begin
  if Assigned(FOnFieldCountAutoDetectComplete) then
    FOnFieldCountAutoDetectComplete(Self);
end;

procedure TnvvCSVReader.Throw_ErrorWithNoParam(AMsg: string);
begin
  raise Exception.Create( Self.ClassName + ': ' + #13 + AMsg );
end;

procedure TnvvCSVReader.Throw_ErrorWithRecordAndFieldNum(AMsg: string);
begin
  Throw_ErrorWithNoParam( AMsg + #13 +
                         'Record #: ' +
                           IntToStr( FIndexOfLastProcessedRecord
                                     + 1{in process} + 1{make it 1-based} ) +
                         '; Field #: ' +
                           IntToStr( FIndexOfLastProcessedField
                                     + 1{in process} + 1{make it 1-based} ) +
                         ' (both start from 1)' );
end;

procedure TnvvCSVReader.Open;
begin
  Active := True;
end;

procedure TnvvCSVReader.Close;
begin
  Active := False;
end;

procedure TnvvCSVReader.Next;
begin
  if (Active) then
  begin
    if (not Eof) then
    begin
      FFlag_ExitMainLoop := False;
      try
        ParsingLoop_Main;
        FBof := False;
      except
        Close; //<----- Close if error during parsing
        raise;
      end;
    end
    else
    begin
      //Reading “beyond” end of file. Next does nothing.
      //Field values will state cleared, which is done at setting Eof.
    end;
  end
  else
    Throw_ErrorWithNoParam('Getting next record.' + #13 +
                           MsgStr_OperationNotAllowedInInactiveState);
end;{Next}

procedure TnvvCSVReader.SetHeaderPresent(const Value: Boolean);
begin
  if (FHeaderPresent <> Value) then
    if (not Active) then
      FHeaderPresent := Value
    else
      Throw_ErrorWithNoParam('Modifying HeaderPresent input parameter.' + #13 +
                              MsgStr_OperationNotAllowedInActiveState);
end;

procedure TnvvCSVReader.SetFieldCount_AutoDetect(const Value: Boolean);
begin
  if (FFieldCount_AutoDetect <> Value) then
    if (not Active) then
      FFieldCount_AutoDetect := Value
    else
      Throw_ErrorWithNoParam('Modifying FieldCount_AutoDetect input parameter.' + #13 +
                             MsgStr_OperationNotAllowedInActiveState);
end;

function TnvvCSVReader.GetFieldCount: Integer;
begin
  Result := FFields.Count;
end;

procedure TnvvCSVReader.SetFieldCount(const Value: Integer);
var
  i: Integer;
begin
  if ( (Value >= 0)and(FieldCount <> Value) ) then
    if ( (not Active) or (Active and AllowFieldCountChangeEvenInOpenState) ) then
    begin
      if (Value > FieldCount) then
        for i:=FieldCount to Value-1 do
          FFields.Add( TnvvCSVField.Create )
      else{Value < FieldCount}
        for i:=FieldCount-1 downto Value do
          FFields.Delete(i);{also will free object because owns it}
    end
    else
      Throw_ErrorWithNoParam('Modifying field count.'+ #13 +
                             MsgStr_OperationNotAllowedInActiveState);
end;

function TnvvCSVReader.GetFieldSeparatorCharCode: Integer;
begin
  Result := Integer(FFieldSeparatorChar);
end;

procedure TnvvCSVReader.SetFieldSeparatorCharCode(const Value: Integer);
begin
  if ( FFieldSeparatorChar <> Char(Value) ) then
    if (not Active) then
        FFieldSeparatorChar := Char(Value)
    else
        Throw_ErrorWithNoParam('Modifying FieldSeparatorCharCode input parameter.' + #13 +
                               MsgStr_OperationNotAllowedInActiveState);
end;

procedure TnvvCSVReader.SetUseFieldQuoting(const Value: Boolean);
begin
  if (FUseFieldQuoting <> Value) then
    if (not Active) then
      FUseFieldQuoting := Value
    else
      Throw_ErrorWithNoParam('Modifying UseFieldDblQuoting input parameter.' + #13 +
                             MsgStr_OperationNotAllowedInActiveState);
end;

function TnvvCSVReader.GetQuoteCharCode: Integer;
begin
  Result := Integer(FQuoteChar);
end;

procedure TnvvCSVReader.SetQuoteCharCode(const Value: Integer);
begin
  if (FQuoteChar <> Char(Value)) then
    if (not Active) then
      FQuoteChar := Char(Value)
    else
      Throw_ErrorWithNoParam('Modifying QuoteCharCode input parameter.' + #13 +
                             MsgStr_OperationNotAllowedInActiveState);
end;

procedure TnvvCSVReader.SetIgnoreEmptyLines(const Value: Boolean);
begin
  if ( FIgnoreEmptyLines <> Value ) then
    if (not Active) then
      FIgnoreEmptyLines := Value
    else
      Throw_ErrorWithNoParam('Modifuing IgnoreEmptyLines input parameter.' + #13 +
                             MsgStr_OperationNotAllowedInActiveState);
end;

procedure TnvvCSVReader.SetASCIIonly(const Value: Boolean);
begin
  if (FASCIIonly <> Value) then
    if (not Active) then
      FASCIIonly := Value
    else
      Throw_ErrorWithNoParam('Modifying ASCIIonly input parameter.' + #13 +
                             MsgStr_OperationNotAllowedInActiveState);
end;

procedure TnvvCSVReader.SetIgnoreSpecialCharacters(const Value: Boolean);
begin
  if ( FIgnoreSpecialCharacters <> Value ) then
    if (not Active) then
      FIgnoreSpecialCharacters := Value
    else
      Throw_ErrorWithNoParam('Modifying IgnoreSpecialCharacters input parameter.' + #13 +
                             MsgStr_OperationNotAllowedInActiveState);
end;

procedure TnvvCSVReader.SetActive(const Value: Boolean);
begin
  if (FActive <> Value) then
  begin
    FActive := Value;
    if (FActive) then
      try
        DoOpen;
      except
        FActive := false;
        DoClose;
        raise;
      end
    else
      DoClose;
  end;
end;

function TnvvCSVReader.GetFields(Index: Integer): TnvvCSVField;
begin
  Result := FFields[Index];
end;

function TnvvCSVReader.GetRecordCountProcessedSoFar: Integer;
begin
  if (FIndexOfLastProcessedRecord < 0 ) then
    Result := 0
  else
    Result := FIndexOfLastProcessedRecord + 1; //FIndexOfLastProcessedRecord is zero-based
end;


{ TnvvCSVFileReader }

constructor TnvvCSVFileReader.Create( ABufferReadFromStreamCapacityInChars: Integer = 512 );
begin
  inherited Create(ABufferReadFromStreamCapacityInChars);
  FFileName := '';
  FStreamReader_Encoding := nil;
  FStreamReader_DetectBOM := False;
  FStreamReader_InternBufferSize := 0;
  FStreamReader_ConstructorKind := srckFile;
end;

function TnvvCSVFileReader.CreateDataSourceReader: TTextReader;
begin
  Result := nil;
  case FStreamReader_ConstructorKind of
    srckFile:
      Result := TStreamReader.Create(FFileName);
    srckFileBOM:
      Result := TStreamReader.Create(FFileName, FStreamReader_DetectBOM);
    srckFileEncodingBOMBuffsize:
      Result := TStreamReader.Create( FFileName, FStreamReader_Encoding,
                                      FStreamReader_DetectBOM, FStreamReader_InternBufferSize);
  end;
end;

procedure TnvvCSVFileReader.SetFile(const AFileName: string);
begin
  if (not Active) then
  begin
    FFileName := AFileName;
    FStreamReader_ConstructorKind := srckFile;
  end
  else
    Throw_ErrorWithNoParam('TnvvCSVFileReader.SetFile.' + #13 +
                           MsgStr_OperationNotAllowedInActiveState);
end;

procedure TnvvCSVFileReader.SetFile(const AFileName: string; ADetectBOM: Boolean);
begin
  if (not Active) then
  begin
    FFileName := AFileName;
    FStreamReader_DetectBOM := ADetectBOM;
    FStreamReader_ConstructorKind := srckFileBOM;
  end
  else
    Throw_ErrorWithNoParam('TnvvCSVFileReader.SetFile.' + #13 +
                           MsgStr_OperationNotAllowedInActiveState);
end;

procedure TnvvCSVFileReader.SetFile(const AFileName: string; AEncoding: TEncoding;
  ADetectBOM: Boolean = False; AStreamReaderInternBufferSize: Integer = 1024);
begin
  if (not Active) then
  begin
    FFileName := AFileName;
    FStreamReader_Encoding := AEncoding;
    FStreamReader_DetectBOM := ADetectBOM;
    FStreamReader_InternBufferSize := AStreamReaderInternBufferSize;
    FStreamReader_ConstructorKind := srckFileEncodingBOMBuffsize;
  end
  else
    Throw_ErrorWithNoParam('TnvvCSVFileReader.SetFile.' + #13 +
                           MsgStr_OperationNotAllowedInActiveState);
end;

{ TnvvCSVStringReader }

constructor TnvvCSVStringReader.Create( ABufferReadFromStreamCapacityInChars: Integer = 512 );
begin
  inherited Create(ABufferReadFromStreamCapacityInChars);
  FDataString := '';
end;

function TnvvCSVStringReader.CreateDataSourceReader: TTextReader;
begin
  Result := TStringReader.Create(FDataString);
end;

procedure TnvvCSVStringReader.SetDataString(const Value: string);
begin
  if (not Active) then
    FDataString := Value
  else
    Throw_ErrorWithNoParam('Modifying input CSV string.' + #13 +
                           MsgStr_OperationNotAllowedInActiveState);
end;

end.

