{
  References;
    - Datasets without databases by Marco Cantú (http://edn.embarcadero.com/article/20587)
    - Gerald Nunn's TGXBaseDataset
    - http://stackoverflow.com/questions/9533760/how-can-i-get-a-dataset-of-in-memory-objects
    - https://code.google.com/p/dduce/
}

unit mVirtualDataSet;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

{$I mDefines.inc}

uses
  Classes, SysUtils, Variants, contnrs, {$IFDEF GUI}Forms,{$ENDIF}
  {$IFNDEF FPC}
  Data.DbConsts,
  {$ENDIF}
  DB,
  mDataFieldsStandardSetup, mSortConditions, mDataProviderInterfaces,
  mFilter, mSummary, mFields, mVirtualDatasetProvider;

{$REGION 'Documentation'}
{
Notes: A record Buffer is in the following format
           - physical record Buffer
               (offset 0, length RecordSize)

//           - calculated fields Buffer
//               (offset dsCalcFldOfs, length CalcFieldSize)
//           - bookmark data
//               (offset dsBookmarkOfs, length BookmarkSize)
           - TRecordInfo data
               (offset dsRecordInfoOfs, length sizeof(TDataSetRecordInfo))
        A key Buffer is in the following format
           - physical record Buffer
               (offset 0, length RecordSize)
           - TKeyRecordInfo data
               (offset btKeyInfoOfs, length sizeof(TKeyRecordInfo))
        TDataSet maintains an array of record Buffers.
        TffTable maintains an array of key Buffers, one for each of
          the TffKeyEditType enum values

1.0

2.0 support for nested tables

2.1 calculated field support

}

{
  Internal Data layout:
+---------------+-----------------------+------------------+----------------------+
|PHYSICAL DATA  | CALCULATED FIELDS     |Bookmark arrays   | VARIABLE LENGTH PTRS |
|FRecordSize    | FCalcRecordSize bytes |FBookmarkArraySize| FVarLengthRecordSize |
+---------------+-----------------------+------------------+----------------------+
^               ^                       ^                  ^
0                       StartBookmarks     StartVarLength

Blobsfields in the internal buffer are pointers to the blob data.
}

{

+-----------------------------------------------------------
|                  DATASET RECORD BUFFER
|                    FRecordBufferSize
+---------------+--------------------+----------------------
|PHYSICAL DATA  | Record Info        |  Calculated fields
| FRecordSize   | TRecordInfo bytes  |
+---------------+--------------------+----------------------
^               ^                    ^
0               FRecordInfoOffset    FCalcFieldsOffset

a DataSet maintains a list (TList) of recordbuffers, holding the physical data

}
{
sources:
Marco Cantu : http://www.marcocantu.com/code/md6htm/MdDataPack.htm

}
{$ENDREGION}

type
  TmCustomVirtualDataset = class;
  TmVirtualDataset       = class;

  EVirtualDatasetError = class(Exception);

  TDeleteRecordEvent = procedure(
    Sender : TmCustomVirtualDataset;
    Index  : Longint
  ) of object;

  TGetRecordCountEvent = procedure(
        Sender : TmCustomVirtualDataset;
    var Count  : Longint
  ) of object;

  TGetFieldValueEvent = procedure(
        Sender : TmCustomVirtualDataset;
        Field  : TField;
        Index  : Longint;
    var Value  : variant
  ) of object;

  TPostDataEvent = procedure(
    Sender : TmCustomVirtualDataset;
    Index  : Longint
  ) of object;

  TLocateEvent = procedure(
          Sender    : TmCustomVirtualDataset;
    const KeyFields : string;
    const KeyValues : Variant;
          Options   : TLocateOptions;
      var Index     : Longint
  ) of object;

  TLookupValueEvent = procedure(
          Sender       : TmCustomVirtualDataset;
    const KeyFields    : string;
    const KeyValues    : Variant;
    const ResultFields : string;
      var Value: Variant
  ) of object;

  PRecordInfo = ^TRecordInfo;
  TRecordInfo = record
    Bookmark     : Longint;
    BookmarkFlag : TBookmarkFlag;
  end;


  TmVirtualDatasetSortableManager = class;
  TmVirtualDatasetFilterManager = class;
  TmVirtualDatasetSummaryManager = class;

  TmOnFilterVirtualDataset = procedure (Sender : TObject) of object;

  { TmCustomVirtualDataset }

  TmCustomVirtualDataset = class(TDataSet)
  strict private
    FInternalOpen     : Boolean;
    FCurrentRecord    : Longint;      // current record (0 to FRecordCount - 1)
    FFilterBuffer     : TRecordBuffer;
    FReadOnly         : Boolean;
    FRecordBufferSize : Integer;      // TRecordInfo

    FModifiedFields : TList;
    FOldValueBuffer : TRecordBuffer;
    FLastFieldListCheckSum : {$IFDEF FPC}QWord(*PtrUInt*){$ELSE}NativeUInt{$ENDIF};
    FLastFieldListCheckSumValid : boolean;

    FOnDeleteRecord   : TDeleteRecordEvent;
    FOnGetFieldValue  : TGetFieldValueEvent;
    FOnGetRecordCount : TGetRecordCountEvent;
    FOnPostData       : TPostDataEvent;
    FOnLocate         : TLocateEvent;
    FOnLookupValue    : TLookupValueEvent;
    FAutomaticInitFieldsFormat : boolean;

    FVirtualDatasetProvider : TmVirtualDatasetDataProvider;
    FSortManager : TmVirtualDatasetSortableManager;
    FFilterManager : TmVirtualDatasetFilterManager;
    FSummaryManager : TmVirtualDatasetSummaryManager;

    procedure DateTimeToNative(
      ADataType : TFieldType;
      AData     : TDateTime;
      ABuffer   : Pointer
    );

    function GetTopIndex: Longint;
    function GetTopRecNo: Longint;
    procedure SetTopIndex(Value: Integer);
    procedure UpdateDisplayFormats;
  protected
    FSorted : boolean;
    FFiltered : boolean;
    FOnFilter : TmOnFilterVirtualDataset;

    function DoSort : boolean;
    procedure ClearSort;

    function DoFilter : boolean;
    procedure ClearFilter;

    // event dispatch methods
    procedure DoDeleteRecord(AIndex: Integer); virtual;
    procedure DoGetFieldValue(AField : TField; AIndex : Integer; out AValue : variant); virtual;
    procedure DoPostData(AIndex: Integer); virtual;

    function InternalGetRecord(ABuffer  : TRecordBuffer; AGetMode : TGetMode; ADoCheck : Boolean): TGetResult; virtual;

    function GetActiveRecBuf(out ARecBuf: TRecordBuffer): Boolean;

    procedure VariantToBuffer(AField : TField; AVariant : Variant; ABuffer : Pointer; ANativeFormat : Boolean = True);
//    procedure BufferToVariant(AField : TField; ABuffer : Pointer; out AVariant : Variant; ANativeFormat : Boolean = True);

    // Standard overrides
    function GetCanModify: Boolean; override;
    function GetRecNo: Longint; override;
    function GetRecordCount: Integer; override;
    procedure DataEvent(Event: TDataEvent; Info: Ptrint); override;
    procedure DoOnNewRecord; override;
    procedure InternalEdit; override;
    procedure SetRecNo(Value: Longint); override;

    // Abstract overrides
    function AllocRecordBuffer: TRecordBuffer; override;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;
    procedure GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag; override;

    // abstract methods required for all datasets
    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode;
      DoCheck: Boolean): TGetResult; override;
    procedure InternalClose; override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
    function IsCursorOpen: Boolean; override;

    function GetRecordSize: Word; override;
    procedure InternalAddRecord(Buffer: Pointer; AAppend: Boolean); override;
//    procedure InternalCreateFields; virtual;
    procedure InternalDelete; override;
    procedure InternalFirst; override;
    procedure InternalGotoBookmark(ABookmark: Pointer); override;
    procedure InternalInitRecord(Buffer: TRecordBuffer); override;
    procedure InternalLast; override;
    procedure InternalPost; override;
    procedure InternalSetToRecord(Buffer: TRecordBuffer); override;
    procedure SetBookmarkFlag(Buffer: TRecordBuffer;
      Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;

    procedure DoAfterOpen; override;

    property ModifiedFields: TList read FModifiedFields;

    property RecordBufferSize: Integer read FRecordBufferSize write FRecordBufferSize;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Refresh; reintroduce;

    procedure SetFieldData(
      Field        : TField;
      Buffer       : {$IFDEF FPC}pointer;{$ELSE}TValueBuffer;{$ENDIF}
      NativeFormat : Boolean
    ); overload; override;

    { Standard public overrides }
    function BookmarkValid(ABookmark: TBookmark): Boolean; override;
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Longint; override;
    {$IFNDEF FPC}
    function GetBlobFieldData(FieldNo: Integer; var Buffer: TBlobByteData) : Integer; override;
    {$ENDIF}
    function Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean; override;
    function Lookup(const KeyFields: string; const KeyValues: Variant; const ResultFields: string): Variant; override;
    function SortManager : ISortableDatasetManager;
    function FilterManager : IFilterDatasetManager;
    function SummaryManager : ISummaryDatasetManager;

    function GetFieldData(Field: TField; {$IFNDEF FPC}var Buffer: TValueBuffer{$ELSE}Buffer: Pointer{$ENDIF}): Boolean; override;

    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;

    property TopIndex: Integer read GetTopIndex write SetTopIndex;

    property TopRecNo: Integer read GetTopRecNo;


    property OnDeleteRecord: TDeleteRecordEvent read FOnDeleteRecord write FOnDeleteRecord;
    property OnGetFieldValue: TGetFieldValueEvent read FOnGetFieldValue write FOnGetFieldValue;
    property OnGetRecordCount: TGetRecordCountEvent read FOnGetRecordCount write FOnGetRecordCount;
    property OnLocate: TLocateEvent read FOnLocate write FOnLocate;

    property OnLookupValue: TLookupValueEvent read FOnLookupValue write FOnLookupValue;

    property OnPostData: TPostDataEvent read FOnPostData write FOnPostData;
    property AutomaticInitFieldsFormat : boolean read FAutomaticInitFieldsFormat write FAutomaticInitFieldsFormat;

    property DatasetDataProvider : TmVirtualDatasetDataProvider read FVirtualDatasetProvider write FVirtualDatasetProvider;
    property OnFilter : TmOnFilterVirtualDataset read FOnFilter write FOnFilter;
  end;

  TmVirtualDataset = class(TmCustomVirtualDataset)
  published
    property Active;
    property Filtered;
    property ReadOnly;

    property AfterCancel;
    property AfterClose;
    property AfterDelete;
    property AfterEdit;
    property AfterInsert;
    property AfterOpen;
    property AfterPost;
    property AfterRefresh;
    property AfterScroll;
    property BeforeCancel;
    property BeforeClose;
    property BeforeDelete;
    property BeforeEdit;
    property BeforeInsert;
    property BeforeOpen;
    property BeforePost;
    property BeforeRefresh;
    property BeforeScroll;

    property OnCalcFields;
    property OnDeleteError;
    property OnDeleteRecord;
    property OnEditError;
    property OnFilterRecord;
    property OnGetFieldValue;
    property OnGetRecordCount;
    property OnNewRecord;
    property OnLookupValue;
    property OnLocate;
    property OnPostData;
    property OnPostError;
  end;

  { TmVirtualDatasetSortableManager }

  TmVirtualDatasetSortableManager = class ({$IFNDEF FPC}TJavaInterfacedObject, {$ENDIF}ISortableDatasetManager)
  protected
    FVirtualDataset : TmCustomVirtualDataset;
  public
    function GetSorted : boolean;
    function GetSortByConditions : TSortByConditions;
    function Sort : boolean;
    procedure ClearSort;
  end;


  { TmVirtualDatasetFilterManager }

  TmVirtualDatasetFilterManager = class ({$IFNDEF FPC}TJavaInterfacedObject, {$ENDIF}IFilterDatasetManager)
  protected
    FVirtualDataset : TmCustomVirtualDataset;
  public
    procedure GetUniqueStringValuesForField (const aFieldName : string; aList : TStringList);
    function DoFilter : boolean;
    function GetFiltered : boolean;
    function GetFilters : TmFilters;
    procedure RemoveFilter;
    procedure RemoveFilterForField (const aFieldName: string);
    procedure RemoveFilterForFields(const aFieldNames: TStrings);
  end;

  { TmVirtualDatasetSummaryManager }

  TmVirtualDatasetSummaryManager = class ({$IFNDEF FPC}TJavaInterfacedObject, {$ENDIF}ISummaryDatasetManager)
  protected
    FVirtualDataset : TmCustomVirtualDataset;
    FListeners : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;

    function GetSummaryDefinitions : TmSummaryDefinitions;
    function GetSummaryValues : TmSummaryValues;
    procedure RefreshSummaries;
    procedure NotifyChanges;
    procedure RegisterListener (aOnRefresh : TNotifyEvent);
  end;

procedure VirtualDatasetError(
  const AMessage : string;
        ADataset : TmCustomVirtualDataset = nil
);

procedure VirtualDatasetErrorFmt(
  const AMessage : string;
  const AArgs    : array of const;
        ADataset : TmCustomVirtualDataset = nil
);



implementation

uses
  Math {$IFNDEF FPC},WideStrUtils,
  WinApi.ActiveX, WinApi.Windows, Data.FmtBcd{$ENDIF};

resourcestring
  SUnsupportedFieldType = 'Unsupported field type (%s) in field %s.';
  SPersistentFieldsRequired ='Virtual dataset can only be used with persistent fields.';
  SIndexOutOfRange = 'Index out of range';
  {$IFDEF FPC}
  SFieldReadOnly = 'Field ''%s'' cannot be modified';
  SNotEditing = 'Dataset not in edit or insert mode';
  //SCircularDataLink = 'Circular datalinks are not allowed';
  {$ENDIF}

type

  { TNotifyEventShell }

  TNotifyEventShell = class
  public
    event : TNotifyEvent;
    constructor Create (aEvent : TNotifyEvent);
  end;

procedure VirtualDatasetError(const AMessage: string;
  ADataset: TmCustomVirtualDataset = nil);
begin
  if Assigned(ADataset) then
    raise EVirtualDatasetError.Create(Format('%s: %s', [ADataset.Name, AMessage]))
  else
    raise EVirtualDatasetError.Create(AMessage);
end;

procedure VirtualDatasetErrorFmt(const AMessage: string;
  const AArgs: array of const; ADataset: TmCustomVirtualDataset = nil);
begin
  VirtualDatasetError(Format(AMessage, AArgs), ADataset);
end;

function FieldListCheckSum(Dataset: TDataSet): {$IFDEF FPC}QWord{$ELSE}NativeUInt{$ENDIF};
var
  I: Integer;
begin
  Result   := 0;
  for I    := 0 to Dataset.Fields.Count - 1 do
    Result := Result + (NativeUInt(pointer(Dataset.Fields[I])) shr (I mod 16));
end;

{ TNotifyEventShell }

constructor TNotifyEventShell.Create(aEvent: TNotifyEvent);
begin
  event := aEvent;
end;

{ TmVirtualDatasetSummaryManager }

constructor TmVirtualDatasetSummaryManager.Create;
begin
  FListeners := TObjectList.Create (true);
end;

destructor TmVirtualDatasetSummaryManager.Destroy;
begin
  FListeners.Free;
end;

function TmVirtualDatasetSummaryManager.GetSummaryDefinitions: TmSummaryDefinitions;
begin
  Result := FVirtualDataset.DatasetDataProvider.SummaryDefinitions;
end;

function TmVirtualDatasetSummaryManager.GetSummaryValues: TmSummaryValues;
begin
  Result := FVirtualDataset.DatasetDataProvider.SummaryValues;
end;

procedure TmVirtualDatasetSummaryManager.RefreshSummaries;
begin
  FVirtualDataset.DatasetDataProvider.CalculateSummaries;
  Self.NotifyChanges;
end;

procedure TmVirtualDatasetSummaryManager.NotifyChanges;
var
  i : integer;
begin
  for i := 0 to FListeners.Count - 1 do
    (FListeners.Items[i] as TNotifyEventShell).event(Self);
end;

procedure TmVirtualDatasetSummaryManager.RegisterListener(aOnRefresh: TNotifyEvent);
begin
  FListeners.Add(TNotifyEventShell.Create(aOnRefresh));
end;

{ TmVirtualDatasetFilterManager }

procedure TmVirtualDatasetFilterManager.GetUniqueStringValuesForField(const aFieldName: string; aList: TStringList);
begin
  FVirtualDataset.DatasetDataProvider.GetUniqueStringValuesForField(aFieldName, aList);
end;

function TmVirtualDatasetFilterManager.DoFilter: boolean;
begin
  Result := FVirtualDataset.DoFilter;
end;

function TmVirtualDatasetFilterManager.GetFiltered: boolean;
begin
  Result := FVirtualDataset.FFiltered;
end;

function TmVirtualDatasetFilterManager.GetFilters: TmFilters;
begin
  Result := FVirtualDataset.DatasetDataProvider.FilterConditions;
end;

procedure TmVirtualDatasetFilterManager.RemoveFilter;
begin
  FVirtualDataset.ClearFilter;
end;

procedure TmVirtualDatasetFilterManager.RemoveFilterForField(const aFieldName: string);
begin
  FVirtualDataset.DatasetDataProvider.FilterConditions.ClearForField(aFieldName);
  FVirtualDataset.DoFilter;
end;

procedure TmVirtualDatasetFilterManager.RemoveFilterForFields(const aFieldNames: TStrings);
var
  i: integer;
begin
  for i := 0 to aFieldNames.Count - 1 do
    FVirtualDataset.DatasetDataProvider.FilterConditions.ClearForField(aFieldNames[i]);
  FVirtualDataset.DoFilter;
end;

{ TmVirtualDatasetSortableManager }

function TmVirtualDatasetSortableManager.GetSorted: boolean;
begin
  Result := FVirtualDataset.FSorted;
end;

function TmVirtualDatasetSortableManager.GetSortByConditions: TSortByConditions;
begin
  Result := FVirtualDataset.DatasetDataProvider.SortConditions;
end;

function TmVirtualDatasetSortableManager.Sort: boolean;
begin
  Result := FVirtualDataset.DoSort;
end;

procedure TmVirtualDatasetSortableManager.ClearSort;
begin
  FVirtualDataset.ClearSort;
end;


procedure TmCustomVirtualDataset.AfterConstruction;
begin
  inherited;
  FLastFieldListCheckSumValid := false;
  FInternalOpen                  := False;
  FReadOnly                      := False;
  FModifiedFields                := TList.Create;
  FAutomaticInitFieldsFormat := true;
  FSortManager := TmVirtualDatasetSortableManager.Create;
  FSortManager.FVirtualDataset := Self;
  FFilterManager := TmVirtualDatasetFilterManager.Create;
  FFilterManager.FVirtualDataset := Self;
  FSummaryManager := TmVirtualDatasetSummaryManager.Create;
  FSummaryManager.FVirtualDataset := Self;
  FSorted := false;
  FFiltered:= false;
end;

procedure TmCustomVirtualDataset.BeforeDestruction;
begin
  FModifiedFields.Free;
  FSortManager.Free;
  FFilterManager.Free;
  FSummaryManager.Free;
  inherited;
end;

procedure TmCustomVirtualDataset.Refresh;
begin
  if Assigned(FVirtualDatasetProvider) then
  begin
    FVirtualDatasetProvider.Refresh(FSorted, FFiltered);
    FVirtualDatasetProvider.CalculateSummaries;
    FSummaryManager.NotifyChanges;
  end;
  inherited Refresh;
end;


function TmCustomVirtualDataset.GetRecordCount: Integer;
begin
  assert (Assigned(FVirtualDatasetProvider));
  Result := FVirtualDatasetProvider.GetRecordCount;

  if Assigned(FOnGetRecordCount) then
    FOnGetRecordCount(Self, Result);
end;

function TmCustomVirtualDataset.GetRecordSize: Word;
begin
  Result := SizeOf(TRecordInfo);
end;

function TmCustomVirtualDataset.GetTopIndex: Longint;
begin
  if BufferCount = 0 then
    Result := -1
  else
    Result := PRecordInfo(Buffers[0])^.Bookmark;
end;

procedure TmCustomVirtualDataset.SetTopIndex(Value: Integer);
begin
  ClearBuffers;
  FCurrentRecord := Value;

  if GetRecord(Buffers[0], gmCurrent, True) = grOK then
  // Only fetch next records when Eof and Bof are false
  begin
    ActivateBuffers;
    GetNextRecords;
  end;
  DataEvent(deDataSetChange, 0);
end;

function TmCustomVirtualDataset.GetTopRecNo: Longint;
begin
  Result := TopIndex + 1;
end;

function TmCustomVirtualDataset.GetRecNo: Longint;
var
  RecBuf: TRecordBuffer;
begin
  CheckActive;
  Result := -1;
  if GetActiveRecBuf(RecBuf) and (PRecordInfo(RecBuf)^.BookmarkFlag = bfCurrent) then
    Result := PRecordInfo(RecBuf)^.Bookmark + 1;
end;

procedure TmCustomVirtualDataset.SetRecNo(Value: Longint);
begin
  CheckBrowseMode;
  Value := Min(Max(Value, 1), RecordCount);
  if RecNo <> Value then
  begin
    DoBeforeScroll;
    FCurrentRecord := Value - 1;
    Resync([rmCenter]);
    DoAfterScroll;
  end;
end;

procedure TmCustomVirtualDataset.UpdateDisplayFormats;
var
  i: integer;
  tmpField: TField;
begin
  for i := 0 to FVirtualDatasetProvider.VirtualFieldDefs.Count - 1 do
  begin
    if (FVirtualDatasetProvider.VirtualFieldDefs.VirtualFieldDefs[i].DefaultFormat <> '') then
    begin
      tmpField := Self.FieldByName(FVirtualDatasetProvider.VirtualFieldDefs.VirtualFieldDefs[i].Name);
      if Assigned(tmpField) and (tmpField is TNumericField) then
        (tmpField as TNumericField).DisplayFormat:= FVirtualDatasetProvider.VirtualFieldDefs.VirtualFieldDefs[i].DefaultFormat;
    end;
  end;
end;

function TmCustomVirtualDataset.GetCanModify: Boolean;
begin
  Result := not FReadOnly;
end;

function TmCustomVirtualDataset.AllocRecordBuffer: TRecordBuffer;
begin
  if not(csDestroying in ComponentState) then
    Result := AllocMem(FRecordBufferSize)
  else
    Result := nil;
end;

function TmCustomVirtualDataset.BookmarkValid(ABookmark: TBookmark): Boolean;
begin
  if Assigned(ABookmark) and (PLongint(ABookmark)^ >= 0) and
    (PLongint(ABookmark)^ < RecordCount) then
    Result := True
  else
    Result := False;
end;

(*
procedure TmCustomVirtualDataset.BufferToVariant(AField: TField; ABuffer: Pointer; out AVariant: Variant; ANativeFormat: Boolean);
begin
  case AField.DataType of
    ftString, ftFixedChar, ftGuid:
      AVariant := AnsiString(PAnsiChar(ABuffer));
    ftWideString, ftFixedWideChar:
      AVariant := WideString(PWideChar(ABuffer));
    ftAutoInc, ftInteger:
      AVariant := Integer(ABuffer^);
    ftSmallint:
      AVariant := SmallInt(ABuffer^);
    ftWord:
      AVariant := Word(ABuffer^);
    ftBoolean:
      AVariant := {$IFDEF FPC}(PInteger(ABuffer)^ = 1){$ELSE}WordBool(ABuffer^){$ENDIF};
    ftFloat, ftCurrency:
      AVariant := Double(ABuffer^);
    ftDate, ftTime, ftDateTime:
      if ANativeFormat then
        DataConvert(AField, ABuffer, @TVarData(AVariant).VDate, False)
      else
        AVariant := TDateTime(ABuffer^);
    ftBCD:
      if ANativeFormat then
        DataConvert(AField, ABuffer, @TVarData(AVariant).VCurrency, False)
      else
        AVariant := Currency(ABuffer^);
  else
    DatabaseErrorFmt(SUnsupportedFieldType, [FieldTypeNames[AField.DataType],
        AField.DisplayName]);
  end;
end;*)

function TmCustomVirtualDataset.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Longint;
begin
  if (Bookmark1 = nil) then
  begin
    if (Bookmark2 = nil) then
      Result := 0
    else
      Result := -1
  end
  else
  begin
    if (Bookmark2 = nil) then
      Result := 1
    else
    begin
      if PInteger(Bookmark1)^ < PInteger(Bookmark2)^ then
        Result := -1
      else if PInteger(Bookmark1)^ > PInteger(Bookmark2)^ then
        Result := 1
      else
        Result := 0;
    end;
  end;
end;

procedure TmCustomVirtualDataset.DataEvent(Event: TDataEvent; Info: Ptrint);
begin
  case Event of
    deLayoutChange:
      if Active and (FLastFieldListCheckSumValid) and (FieldListCheckSum(Self) <> FLastFieldListCheckSum) then
        FLastFieldListCheckSumValid := false;
  end;
  inherited;
end;

procedure TmCustomVirtualDataset.DoDeleteRecord(AIndex: Integer);
begin
  assert (Assigned(FVirtualDatasetProvider));
  FVirtualDatasetProvider.DeleteRecord(AIndex);
  if Assigned(FOnDeleteRecord) then
    FOnDeleteRecord(Self, AIndex);
end;

procedure TmCustomVirtualDataset.DoGetFieldValue(AField: TField; AIndex: Integer;
  out AValue: variant);
begin
  assert (Assigned(FVirtualDatasetProvider));
  FVirtualDatasetProvider.GetFieldValue(aField.FieldName, aIndex, aValue);
  if Assigned(FOnGetFieldValue) then
    FOnGetFieldValue(Self, AField, AIndex, AValue);
end;

procedure TmCustomVirtualDataset.DoOnNewRecord;
begin
  FModifiedFields.Clear;
  if FOldValueBuffer = nil then
    FOldValueBuffer := AllocRecordBuffer;
  InitRecord(FOldValueBuffer);
  inherited DoOnNewRecord;
end;

procedure TmCustomVirtualDataset.DoPostData(AIndex: Integer);
begin
  assert (Assigned(FVirtualDatasetProvider));
  if State in dsEditModes then
  begin
    if State = dsEdit then
    begin
      FVirtualDatasetProvider.EditRecord(AIndex, Self.ModifiedFields);
    end
    else
    if State = dsInsert then
    begin
      FVirtualDatasetProvider.InsertRecord(AIndex, Self.ModifiedFields);
    end;
  end;
  if Assigned(FOnPostData) then
    FOnPostData(Self, AIndex);
end;

procedure TmCustomVirtualDataset.DateTimeToNative(ADataType: TFieldType;
  AData: TDateTime; ABuffer: Pointer);
var
  TimeStamp: TTimeStamp;
begin
  TimeStamp := DateTimeToTimeStamp(AData);
  case ADataType of
    ftDate: Integer(ABuffer^) := TimeStamp.Date;
    ftTime: Integer(ABuffer^) := TimeStamp.Time;
  else
    TDateTime(ABuffer^) := TimeStampToMSecs(TimeStamp);
  end;
end;

procedure TmCustomVirtualDataset.FreeRecordBuffer(var Buffer: TRecordBuffer);
begin
  if Buffer <> nil then
    FreeMem(Buffer);
//  FinalizeBuffer(Buffer);
  //Finalize(PVariantList(Buffer + SizeOf(TRecordInfo))^, Fields.Count);
//  FreeMem(Buffer);
end;

function TmCustomVirtualDataset.GetActiveRecBuf(out ARecBuf: TRecordBuffer) : Boolean;
begin
  ARecBuf := nil;
  case State of
    dsBlockRead, dsBrowse:
      if IsEmpty then
        ARecBuf := nil
      else
        ARecBuf := {$IFDEF FPC}PChar{$ELSE}PByte{$ENDIF}(ActiveBuffer);

    dsEdit, dsInsert, dsNewValue:
      ARecBuf := {$IFDEF FPC}PChar{$ELSE}PByte{$ENDIF}(ActiveBuffer);

    dsCalcFields, dsInternalCalc:
      ARecBuf := {$IFDEF FPC}PChar{$ELSE}PByte{$ENDIF}(CalcBuffer);

    dsFilter:
      ARecBuf := FFilterBuffer;
  end;
  Result := ARecBuf <> nil;
end;

{$IFNDEF FPC}
function TmCustomVirtualDataset.GetBlobFieldData(FieldNo: Integer;
  var Buffer: TBlobByteData): Integer;
begin
  Result := inherited GetBlobFieldData(FieldNo, Buffer);
end;
{$ENDIF}

procedure TmCustomVirtualDataset.GetBookmarkData(Buffer: TRecordBuffer;
  Data: Pointer);
begin
  PLongint(Data)^ := PRecordInfo(Buffer)^.Bookmark;
end;

function TmCustomVirtualDataset.GetBookmarkFlag(Buffer: TRecordBuffer)
  : TBookmarkFlag;
begin
  Result := PRecordInfo(Buffer)^.BookmarkFlag;
end;

procedure TmCustomVirtualDataset.VariantToBuffer(AField: TField; AVariant: Variant; ABuffer: Pointer; ANativeFormat: Boolean);

  procedure CurrToBuffer(const C: Currency);
  begin
    if ANativeFormat then
      DataConvert(AField, @C, ABuffer, True)
    else
      Currency(ABuffer^) := C;
  end;

var
  Size: Integer;
  {$IFDEF FPC}
  TempWideStr : WideString;
  TempWideChar: WideChar;
  {$ENDIF}
begin
  case AField.DataType of
    ftGuid, ftFixedChar, ftString:
      begin
        if (VarType(AVariant) = varString) or (VarType(AVariant) = varolestr) then
        begin
          Size := Min(Length(AVariant), AField.Size);
          PAnsiChar(ABuffer)[Size] := #0;
          Move(PChar(string(AVariant))^, PChar(ABuffer)^, Size);
        end
        else
        begin
          Size := Length(AVariant);
          if Size = 0 then
            PAnsiChar(ABuffer)[0] := #0
          else
          begin
            {$IFDEF FPC}
            PAnsiChar(ABuffer)[0] := #0;
            {$ELSE}
            WideCharToMultiByte(0, 0, tagVariant(AVariant).bStrVal,
              Size + 1, ABuffer,
              AField.Size, nil, nil);
            {$ENDIF}
          end;
        end;
      end;
    ftFixedWideChar, ftWideString:
      begin
        {$IFDEF FPC}
        TempWideStr := VarToWideStr(AVariant);
        if TempWideStr = '' then
          PWideChar(ABuffer)[0] := #0
        else
        begin
          Size := Min(Length(TempWideStr), AField.Size * SizeOf(TempWideChar));
          PUnicodeChar(ABuffer)[Size] := #0;
          Move(PUnicodeChar(string(TempWideStr))^, PUnicodeChar(ABuffer)^, Size);
        end;
        {$ELSE}
        if tagVariant(AVariant).bStrVal = nil then
          PWideChar(ABuffer)[0] := #0
        else
          WStrCopy(ABuffer, tagVariant(AVariant).bStrVal);
        {$ENDIF}
      end;
    ftSmallint:
      begin
        {$IFNDEF FPC}
        if tagVariant(AVariant).vt = VT_UI1 then
          SmallInt(ABuffer^) := Byte(tagVariant(AVariant).cVal)
        else
          SmallInt(ABuffer^) := tagVariant(AVariant).iVal;
        {$ENDIF}
      end;
    ftWord:
      begin
        {$IFNDEF FPC}
        if tagVariant(AVariant).vt = VT_UI1 then
          Word(ABuffer^) := tagVariant(AVariant).bVal
        else
          Word(ABuffer^) := tagVariant(AVariant).uiVal;
        {$ENDIF}
      end;
    ftAutoInc, ftInteger:
      begin
        Integer(ABuffer^) := AVariant;
      end;
    ftFloat, ftCurrency:
      begin
        {$IFDEF FPC}
        Double(ABuffer^) := AVariant;
        {$ELSE}
        if tagVariant(AVariant).vt = VT_R8 then
          Double(ABuffer^) := tagVariant(AVariant).dblVal
        else
          Double(ABuffer^) := AVariant;
        {$ENDIF}
      end;
    {$IFNDEF FPC}
    ftFMTBCD:
      TBcd(ABuffer^) := VarToBcd(AVariant);
    {$ENDIF}
    ftBCD:
      {$IFDEF FPC}
      CurrToBuffer(AVariant);
      {$ELSE}
      if tagVariant(AVariant).vt = VT_CY then
        CurrToBuffer(tagVariant(AVariant).cyVal)
      else
        CurrToBuffer(AVariant);
      {$ENDIF}
    ftBoolean:
      begin
        {$IFDEF FPC}
        if AVariant then
          PInteger(ABuffer)^ := 1
        else
          PInteger(ABuffer)^ := 0;
        {$ELSE}
        VarAsType(AVariant, VT_BOOL);
        WordBool(ABuffer^) := tagVariant(AVariant).vbool;
        {$ENDIF}
      end;
    ftDate, ftTime, ftDateTime:
      begin
        DateTimeToNative(AField.DataType, AVariant, ABuffer);
      end;
(*    ftBytes, ftVarBytes:
      if ANativeFormat then
        DataConvert(AField, @AVariant, ABuffer, True)
      else
        OleVariant(ABuffer^) := AVariant;*)
  else
    DatabaseErrorFmt(SUnsupportedFieldType, [FieldTypeNames[AField.DataType],
        AField.DisplayName]);
  end;

end;

(*
function TmCustomVirtualDataset.GetMasterSource: TDataSource;
begin
  if Assigned(MasterDataLink) then
    Result := MasterDataLink.DataSource
  else
    Result := nil;
end;
*)

function TmCustomVirtualDataset.GetRecord(Buffer: TRecordBuffer;
  GetMode: TGetMode; DoCheck: Boolean): TGetResult;
var
  Accept    : Boolean;
  SaveState : TDataSetState;
begin
  if Filtered and Assigned(OnFilterRecord) then
  begin
    FFilterBuffer := Buffer;
    SaveState     := SetTempState(dsFilter);
    try
      Accept := True;
      repeat
        Result := InternalGetRecord(Buffer, GetMode, DoCheck);
        if Result = grOK then
        begin
          OnFilterRecord(Self, Accept);
          if not Accept and (GetMode = gmCurrent) then
            Result := grError;
        end;
      until Accept or (Result <> grOK);
    except
      {$IFNDEF NOGUI}
      Application.HandleException(Self);
      {$ENDIF}
      Result := grError;
    end;
    RestoreState(SaveState);
  end
  else
    Result := InternalGetRecord(Buffer, GetMode, DoCheck);
end;

function TmCustomVirtualDataset.InternalGetRecord(ABuffer: TRecordBuffer;
  AGetMode: TGetMode; ADoCheck: Boolean): TGetResult;
var
  iRecCount: Longint;
begin
  try
    Result := grOK;
    case AGetMode of
      gmNext:
        begin
          iRecCount := RecordCount;
          if FCurrentRecord < iRecCount then
            Inc(FCurrentRecord);
          if FCurrentRecord >= iRecCount then
            Result := grEOF;
        end;
      gmPrior:
        begin
          if FCurrentRecord <= 0 then
            FCurrentRecord := -1
          else
          begin
            iRecCount := RecordCount;
            FCurrentRecord  := Min(FCurrentRecord - 1, iRecCount - 1);
          end;
          if FCurrentRecord < 0 then
            Result := grBOF;
        end;
      gmCurrent:
        begin
          iRecCount := RecordCount;
          if FCurrentRecord < 0 then
            Result := grBOF
          else if FCurrentRecord >= iRecCount then
            Result := grEOF;
        end;
    end;

    if Result = grOK then
    begin
      with PRecordInfo(ABuffer)^ do
      begin
        Bookmark     := FCurrentRecord;
        BookmarkFlag := bfCurrent;
      end;
      GetCalcFields(ABuffer);
    end;
  except
    if ADoCheck then
      raise;
    Result := grError;
  end;
end;

procedure TmCustomVirtualDataset.InternalGotoBookmark(ABookmark: Pointer);
begin
  FCurrentRecord := PLongint(ABookmark)^;
end;

procedure TmCustomVirtualDataset.InternalAddRecord(Buffer: Pointer;
  AAppend: Boolean);
begin
end;

procedure TmCustomVirtualDataset.InternalClose;
begin
  FInternalOpen := False;
  BindFields(False);
  FieldDefs.Updated := False;
  if FOldValueBuffer <> nil then
  begin
    try
      FreeMem(FOldValueBuffer);
    finally
      FOldValueBuffer := nil;
    end;
  end;
end;

(*procedure TmCustomVirtualDataset.InternalCreateFields;
begin
  // TmCustomVirtualDataset requires persistent fields to be defined
  if DefaultFields then
    VirtualDatasetError(SPersistentFieldsRequired, Self);
end;*)

procedure TmCustomVirtualDataset.InternalDelete;
var
  RecBuf: TRecordBuffer;
begin
  GetActiveRecBuf(RecBuf);
  DoDeleteRecord(PRecordInfo(RecBuf)^.Bookmark);
end;

procedure TmCustomVirtualDataset.InternalEdit;
begin
  FModifiedFields.Clear;

  if FOldValueBuffer = nil then
    FOldValueBuffer := AllocRecordBuffer;
end;

procedure TmCustomVirtualDataset.InternalFirst;
begin
  FCurrentRecord := -1;
end;

procedure TmCustomVirtualDataset.InternalHandleException;
begin
  {$IFNDEF NOGUI}
  Application.HandleException(Self);
  {$ENDIF}
end;

procedure TmCustomVirtualDataset.InternalInitFieldDefs;
begin
  assert (Assigned(FVirtualDatasetProvider));
  FieldDefs.Clear;
  FVirtualDatasetProvider.FillFieldDefsOfDataset(Self.FieldDefs, Self.ReadOnly);
end;

procedure TmCustomVirtualDataset.InternalInitRecord(Buffer: TRecordBuffer);
begin
  FillChar(Buffer[0], FRecordBufferSize, 0);
end;

procedure TmCustomVirtualDataset.InternalLast;
begin
  FCurrentRecord := RecordCount;
end;

procedure TmCustomVirtualDataset.InternalOpen;
begin
  FInternalOpen := True;
  InternalFirst;
  BookmarkSize := SizeOf(Longint);
  FieldDefs.Updated := False;
  FieldDefs.Update;
  Fields.Clear;
  CreateFields;
  BindFields(True);
  FRecordBufferSize := SizeOf(TRecordInfo); // + (Fields.Count * SizeOf(Variant));

  FVirtualDatasetProvider.SetDefaultVisibilityOfFields (Fields);
end;

procedure TmCustomVirtualDataset.InternalPost;
var
  RecBuf: TRecordBuffer;
begin
  UpdateCursorPos;
  GetActiveRecBuf(RecBuf);
  if PRecordInfo(RecBuf)^.BookmarkFlag = bfEof then
    DoPostData(-1)
  else
    DoPostData(PRecordInfo(RecBuf)^.Bookmark);
end;

procedure TmCustomVirtualDataset.InternalSetToRecord(Buffer: TRecordBuffer);
begin
  if PRecordInfo(Buffer)^.BookmarkFlag in [bfCurrent, bfInserted] then
  begin
    FCurrentRecord := PRecordInfo(Buffer)^.Bookmark;
  end;
end;

function TmCustomVirtualDataset.IsCursorOpen: Boolean;
begin
  Result := FInternalOpen;
end;

function TmCustomVirtualDataset.Locate(const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
var
  P: Longint;
begin
  if Assigned(FOnLocate) then
  begin
    P := -1;
    FOnLocate(Self, KeyFields, KeyValues, Options, P);
    Result := P <> -1;
    if Result and (P <> FCurrentRecord) then
    begin
      DoBeforeScroll;
      FCurrentRecord := P;
      Resync([rmCenter]);
      DoAfterScroll;
    end;
  end
  else
    Result := False;
end;

function TmCustomVirtualDataset.Lookup(const KeyFields: string;
  const KeyValues: Variant; const ResultFields: string): Variant;
begin
  if Assigned(FOnLookupValue) then
  begin
    Result := Null;
    FOnLookupValue(Self, KeyFields, KeyValues, ResultFields, Result);
  end
  else
    Result := inherited Lookup(KeyFields, KeyValues, ResultFields);
end;

function TmCustomVirtualDataset.SortManager: ISortableDatasetManager;
begin
  Result := FSortManager;
end;

function TmCustomVirtualDataset.FilterManager: IFilterDatasetManager;
begin
  Result := FFilterManager;
end;

function TmCustomVirtualDataset.SummaryManager: ISummaryDatasetManager;
begin
  Result := FSummaryManager;
end;


function TmCustomVirtualDataset.DoSort : boolean;
begin
  Result := false;
  if not Active then
    Exit;
  Result := FVirtualDatasetProvider.Refresh(true, FFiltered);
  if Result then
  begin
    FSorted := true;
    Resync([]);
  end
  else
  begin
    FSorted := false;
  end;
end;

procedure TmCustomVirtualDataset.ClearSort;
begin
  FSorted := false;
  FVirtualDatasetProvider.SortConditions.Clear;
  if Assigned(FVirtualDatasetProvider) then
    FVirtualDatasetProvider.Refresh(false, FFiltered);
  Resync([]);
end;

function TmCustomVirtualDataset.DoFilter: boolean;
begin
  Result := false;
  if not Active then
    Exit;
  Result := FVirtualDatasetProvider.Refresh(FSorted, true);
  if Result then
  begin
    FFiltered := true;
    Resync([]);
  end
  else
  begin
    FFiltered := false;
  end;
  FSummaryManager.NotifyChanges;
  if Assigned(FOnFilter) then
    FOnFilter(Self);
end;

procedure TmCustomVirtualDataset.ClearFilter;
begin
  FFiltered := false;
  if Assigned(FVirtualDatasetProvider) then
  begin
    FVirtualDatasetProvider.FilterConditions.Clear;
    FVirtualDatasetProvider.Refresh(FSorted, false);
  end;
  Resync([]);
  FSummaryManager.NotifyChanges;
  if Assigned(FOnFilter) then
    FOnFilter(Self);
end;

(*
procedure TmCustomVirtualDataset.MasterChanged(Sender: TObject);
begin
  if not Active then
    Exit;
  InternalFirst;
  Resync([]);
end;*)

(*
procedure TmCustomVirtualDataset.MasterDisabled(Sender: TObject);
begin
  if not Active then
    Exit;
  // Suggestion from Roman Linde
  // Do not reset cursor position because:
  // Second problem is with "MasterDisabled". Procedure executes when I call
  // "Enable controls" changing active record earlier set with "Locate".
  // I want to locate record with disabled controls and enabling controls should
  // not change active record?
  // FCurrent := -1;
  Resync([]);
end;*)

procedure TmCustomVirtualDataset.SetBookmarkFlag(Buffer: TRecordBuffer;
  Value: TBookmarkFlag);
begin
  PRecordInfo(Buffer)^.BookmarkFlag := Value;
end;

procedure TmCustomVirtualDataset.SetBookmarkData(Buffer: TRecordBuffer;
  Data: Pointer);
begin
  if PRecordInfo(Buffer)^.BookmarkFlag in [bfCurrent, bfInserted] then
    PRecordInfo(Buffer)^.Bookmark := PLongint(Data)^
  else
    PRecordInfo(Buffer)^.Bookmark := -1;
end;

procedure TmCustomVirtualDataset.SetFieldData(Field: TField;
  Buffer: {$IFDEF FPC}pointer;{$ELSE}TValueBuffer;{$ENDIF} NativeFormat: Boolean);
begin
end;

procedure TmCustomVirtualDataset.DoAfterOpen;
begin
  if FAutomaticInitFieldsFormat then
    ApplyStandardSettingsToFields(Self, '#,##0.00');
  UpdateDisplayFormats;
  inherited DoAfterOpen;
end;


function TmCustomVirtualDataset.GetFieldData(Field: TField; {$IFNDEF FPC}var Buffer: TValueBuffer{$ELSE}Buffer: Pointer{$ENDIF}): Boolean;
var
  RecBuf: TRecordBuffer;
  Data  : variant;
  Value : Variant;

  procedure RefreshBuffers;
  begin
    FLastFieldListCheckSum:= FieldListCheckSum(Self);
    FLastFieldListCheckSumValid:= (FLastFieldListCheckSum <> 0);
    UpdateCursorPos;
    Resync([]);
  end;

begin
  if not FLastFieldListCheckSumValid then
    RefreshBuffers;
  if (State = dsOldValue) and (FModifiedFields.IndexOf(Field) <> -1) then
  // Requesting the old value of a modified field
  begin
    Result := True;
    RecBuf := FOldValueBuffer;
  end
  else
    Result := GetActiveRecBuf(RecBuf);

  if not Result then
    Exit;

  DoGetFieldValue(Field, PRecordInfo(RecBuf)^.Bookmark, Data);
  if VarType(Data) = vtBoolean then
    Value := VarAsType(Data, vtBoolean)
  else
    Value := Data;

  Result := not VarIsNull(Data);
  if Result and (Buffer <> nil) then
  begin
    if VarType(Data) = vtBoolean then
      Value := VarAsType(Data, vtBoolean)
    else
      Value := Data;
    VariantToBuffer(Field, Value, Buffer, False);   {TODO -oTS -cGeneral : take a look at NativeFormat }
  end;
end;


end.

{
The following documentation is taken from the fcl-db package and provides a
good understanding of TDataSet internals.

Contents
========

+ General remarks
+ Fields system
+ The buffers
+ Dataset implementation
+ Scalable Datasets.

===============
General remarks
===============

- All fields and descendents implemented.
- No calculated fields.
- No persistent fields; this must be added later.

=============
Fields system
=============

Buffers are completely handled by the Dataset. Fields don't handle
their own buffers. Only during validation, the FValueBuffer of the
field is used.

This allows the dataset to allocate a number of buffers for the current
record and the N next records. (getnextrecords/getpriorrecords method)

This means that all field mechanisms MUST pass through GetData/SetData,
since FValueBuffer is only valid during validation.

===========
The Buffers
===========

A buffer contains all the data for 1 record of the dataset, and also
the bookmark information. (bookmarkinformation is REQUIRED)

The dataset allocates by default 'DefaultBufferCount+1' records(buffers)
This constant can be changed, at the beginning of dataset.inc;
if you know you'll be working with big datasets, you can
increase this constant.

The buffers are stored as pchars in the FBuffers array;
The following constants are userd when handling this array:

FBuffercount : The number of buffers allocated, minus one.
FRecordCount : The number of buffers that is actually filled in.
FActiveBuffer : The index of the active record in TDataset
FCurrentRecord : The index of the supposedly active record in the underlaying
                 dataset (ie. the index in the last call to SetToInternalRecord)
                 call CursopPosChanged to reset FCurrentRecord if the active
                 record in the underlaying dataset has changed

So the following picture follows from this:

+---------------+
|  0            |
+---------------+
|  1            |
+---------------+
|               |
   ...
|               |
+---------------+
| FActivebuffer |
+---------------+
|               |
    ...
|               |
+---------------+
|FRecordCount-1 |
+---------------+
|               |
  ...
|               |
+---------------+
| FBufferCount  |
+---------------+

The array is zero based.

The following methods are used to manipulate the array:

GetNextRecords: Tries to fill up the entire array, going forward
GetPriorRecords: tries to fill up the entire array, going backward
GetNextRecord: gets the next record. Shifts the array if FrecordCount=BufferCount-1
GetPriorRecord: gets the previous record. Shifts the array if FrecordCount=BufferCount-1

For the last 2 methods: the underlying record pointer must be on the
last/first record in the dataset, or it will go wrong.

resync tries to refresh the array from the underlying dataset; it uses the
bookmarks for that.

=======================
Dataset implementations
=======================

TDataset does most of the work associated with fields, buffers and
navigating/editing/adding/removing records of some source of data.
There are, however, some methods that need to be filled in so that
a real TDataset can be implemented.

In order to have a working Dataset, the following Methods  need to be
overridden in order to make a dataset descendant:

function AllocRecordBuffer: PChar; virtual; abstract;
-----------------------------------------------------

Must allocate enough memory to store a complete record in the dataset.
Optionally, this buffer must contain enough memory to store bookmarkdata.
The descendent must be able to construct a bookmark from this buffer.

procedure FreeRecordBuffer(var Buffer: PChar); virtual; abstract;
-----------------------------------------------------------------

Must free the memory allocated in the AllocRecordBuffer call.

procedure GetBookmarkData(Buffer: PChar; Data: Pointer); virtual; abstract;
---------------------------------------------------------------------------

Puts the bookmarkdata for Buffer into the area pointed to by Data.

function GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; virtual; abstract;
--------------------------------------------------------------------------

Returns the bookmarkflag associated with Buffer.

function GetFieldData(Field: TField; Buffer: Pointer): Boolean; virtual; abstract;
----------------------------------------------------------------------------------

Puts the data for field Field from the active buffer into Buffer.
This is called whenever a field value is demanded, so it must be
efficient.

function GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult; virtual; abstract;
-----------------------------------------------------------------------------------

This method must do 3 things:
1) Get the record data for the next/current/previous record, depending
   on the GetMode value. It should return
    grOK    if all was OK.
    grBOF   if the previous record was requested, and we are at the start.
    grEOF   if the next record was requested, and we are at the end.
    grError if an error occurred.

2) If DoCheck is True, and the result is grError, then an exception must be
    raised.

3) It should initialize bookmark data for this record with flag 'bfCurrent'
   This data can be stored in the bufer, if space was allocated for it with
   AllocRecordBuffer.

function GetRecordSize: Word; virtual; abstract;
------------------------------------------------

Should return the record size; this includes ONLY the data portion
of teh buffer; it excludes any bookmark or housekeeping info you may
have put in the buffer.

procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); virtual; abstract;
---------------------------------------------------------------------------------

Adds a record to the dataset. The record's data is in Buffer and Append
indicates whether the record should be appended (True) or Inserted (False).
Note that for SQL based datasets, this has no meaning.

procedure InternalClose; virtual; abstract;
-------------------------------------------

Closes the dataset. Any resources allocated in InternalOpen should be freed
here.

procedure InternalDelete; virtual; abstract;
--------------------------------------------

Deletes the current Record.

procedure InternalFirst; virtual; abstract;
-------------------------------------------

This is called when 'First' is called; After this method, getrecord
should return 'grBOF' if the previous record is requested, and it should
return the next record if the next record is requested.

procedure InternalGotoBookmark(ABookmark: Pointer); virtual; abstract;
----------------------------------------------------------------------

Set the record position on the position that is associated with the
ABookMark data. The ABookMark data is the data that is acquired through
the GetBookMarkData call, and should be kept for each record.

procedure InternalHandleException; virtual; abstract;
-----------------------------------------------------

Not needed yet. Just implement an empty call.

procedure InternalInitFieldDefs; virtual; abstract;
---------------------------------------------------

This method should be called from InternalOpen, and should
initialize FieldDef definitions for all fields in a record.
It should add these definitions to the FFielddefs object.

procedure InternalInitRecord(Buffer: PChar); virtual; abstract;
---------------------------------------------------------------

This method is called to initialize a field buffer when the dataset
is put into edit or append mode. Mostly,you'll want to zero out the
buffer.

procedure InternalLast; virtual; abstract;
------------------------------------------

This is called when 'Last' is called; After this method, getrecord
should return 'grEOF' if the next record is requested, and it should
return the last record if the previous record is requested.

procedure InternalOpen; virtual; abstract;
------------------------------------------

Open the dataset. You must call internalinitfielddefs;
if DefaultFields is True, then you must call CreateFields,
which will create the necessary TFields from the fielddefs.

procedure InternalPost; virtual; abstract;
------------------------------------------

Post the data in the active buffer to the underlying dataset.

procedure InternalSetToRecord(Buffer: PChar); virtual; abstract;
----------------------------------------------------------------

Set the current record to the record in Buffer; if bookmark data
is specified in this buffer, that data can be used to determine which
record this should be.

function IsCursorOpen: Boolean; virtual; abstract;
--------------------------------------------------

This function should return True if data is available, even if the dataset
is not active.

procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); virtual; abstract;
----------------------------------------------------------------------------------

Set the bookmarkflag 'Value' on the data in Buffer.

procedure SetBookmarkData(Buffer: PChar; Data: Pointer); virtual; abstract;
---------------------------------------------------------------------------

Move the bookmarkdata in 'Data' to the bookmarkdata associated with Buffer

procedure SetFieldData(Field: TField; Buffer: Pointer); virtual; abstract;
--------------------------------------------------------------------------

Move the data in associated with Field from Buffer to the activebuffer.

=================
Scalable datasets
=================

In order to have Scalable database access, the concept of TDatabase and
TDBDataset is introduced. The idea is that, in a visual IDE, the change
from one database to another is achieved by simply removing one TDatabase
descendent (Say, TMySqlDatabase) with another (Say, TPostGreSQLDatabase)
and that the Datasets remain untouched.

In order to make this possible, the following scheme is used:

when a TDBdataset descendant is put on Active, it requests a TRecordSet
from the TDatabase. The TRecordSet is an abstract object that should be
implemented together with each database. The TDBDataset then uses the
TRecordSet to navigate through the records and edit/add/modify them.
The TDBdataset implements the abstract methods of Tdataset in order to
achive this.

There will be 2 descendants of TDBdataset: TTable and TQuery; both will
implement the last abstract methods of TDataset in order to achieve a
complete TDataset implementation.

TDBDataset implements most of the initialization of fields, so the
implementation of TRecordSet will be as bare bones as possible.

What is needed:
---------------

Some properties describing the data:

FieldCount : Number of fields in a record;
FieldTypes[Index] : Types of the fields (TFieldType), zero based.
FieldNames[Index] : Names of the fields. Zero based.
FieldSizes[index] : Size of the fields, zero based.
BookmarkSize        : Size of a bookmark.

Some properties with the data content:

FieldBuffers[Index] : Buffers containing the actual data of the current record.
                      (Nil if the field is empty)
                      This data should be of size indicated FieldSizes, and
                      in a format that matches the fieldtype.
BookMarkBuffer      : Buffer with the current bookmark.

Some methods
------------

OpenRecordSet : Opens the recordset; it should initialize the FieldCount
                and FieldTypes, FieldNames, and FieldSizes array data.

CloseRecordSet : Do whatever is needed to close the recordset.

GotoBookMark : go to the record described by the bookmark. Returns True
               if successfull, false if not.

Next  : Goto the next record. Returns true or false
Prior : Goto previous record. Returns true or false
First : Goto the first record. Returns True or false
Last  : Goto the last record. Returns True or False

AppendBuffer : Append a buffer to the records.
}

