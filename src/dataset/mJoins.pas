unit mJoins;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, contnrs, sysutils,
  mVirtualDataSetInterfaces, mMaps;

const
  PREFIX_JOIN_SEPARATOR = '$';

type
  TBuildExternalEntityKeyFromDatum = function (aSource : IVDDatum): TObject of object;

  { TmBuiltInJoin }

  TmBuiltInJoin = class
  strict private
    FPrefix : string;
    FBuildExternalEntityKeyFunction : TBuildExternalEntityKeyFromDatum;
    FActive : boolean;
    FProvider : IVDListDataProvider;

    procedure SetPrefix(AValue: string);
  public
    constructor Create;

    property Active : boolean read FActive write FActive;
    property Prefix : string read FPrefix write SetPrefix;
    property BuildExternalEntityKeyFunction : TBuildExternalEntityKeyFromDatum read FBuildExternalEntityKeyFunction write FBuildExternalEntityKeyFunction;
    property Provider : IVDListDataProvider read FProvider write FProvider;
  end;

  { TmBuiltInJoins }

  TmBuiltInJoins = class
  strict private
    FList : TObjectList;
    FDictionary : TmStringDictionary;
  public
    constructor Create;
    destructor Destroy; override;

    function Count : integer;
    procedure Clear;
    function Add: TmBuiltInJoin;
    function Get (const aIndex : integer) : TmBuiltInJoin;
    function FindByPrefix (const aPrefix : string) : TmBuiltInJoin;
  end;

  { TmJoin }

  TmJoin = class
  strict private
    FPrefix : string;
    FSourceFieldNames : TStringList;
    FDestinationFieldNames : TStringList;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  procedure ExtractPrefixAndFieldName (const aSource : String; out aPrefix : string; out aFieldName : string);

implementation

procedure ExtractPrefixAndFieldName(const aSource: String; out aPrefix: string; out aFieldName: string);
var
  i : integer;
begin
  aPrefix := '';
  aFieldName := aSource;
  i := Pos(PREFIX_JOIN_SEPARATOR, aSource);
  if i > 0 then
  begin
    aPrefix := Copy(aSource, 1, i - 1);
    aFieldName := Copy(aSource, i+1, 99999);
  end;
end;

{ TmJoin }

constructor TmJoin.Create;
begin
  FPrefix := '';
  FSourceFieldNames := TStringList.Create;
  FDestinationFieldNames := TStringList.Create;
end;

destructor TmJoin.Destroy;
begin
  FSourceFieldNames.Free;
  FDestinationFieldNames.Free;
  inherited Destroy;
end;

{ TmBuiltInJoins }

constructor TmBuiltInJoins.Create;
begin
  FList := TObjectList.Create(true);
  FDictionary := TmStringDictionary.Create();
end;

destructor TmBuiltInJoins.Destroy;
begin
  FDictionary.Free;
  FList.Free;
  inherited Destroy;
end;

function TmBuiltInJoins.Count: integer;
begin
  Result := FList.Count;;
end;

procedure TmBuiltInJoins.Clear;
begin
  FList.Clear;
  FDictionary.Clear;
end;

function TmBuiltInJoins.Add: TmBuiltInJoin;
begin
  Result := TmBuiltInJoin.Create;
  FList.Add(Result);
  FDictionary.Clear;
end;

function TmBuiltInJoins.Get(const aIndex: integer): TmBuiltInJoin;
begin
  Result := FList.Items[aIndex] as TmBuiltInJoin;
  FDictionary.Clear;
end;

function TmBuiltInJoins.FindByPrefix(const aPrefix: string): TmBuiltInJoin;
var
  i : integer;
begin
  Result := nil;
  if FDictionary.Count = 0 then
  begin
    for i := 0 to Count - 1 do
      FDictionary.Add(uppercase(Get(i).Prefix), Get(i));
  end;
  Result := FDictionary.Find(uppercase(aPrefix)) as TmBuiltInJoin;
end;

{ TmBuiltInJoin }

procedure TmBuiltInJoin.SetPrefix(AValue: string);
begin
  if FPrefix=AValue then Exit;
  FPrefix:=AValue;
  if Copy(FPrefix, Length(FPrefix), 1) <> PREFIX_JOIN_SEPARATOR then
    FPrefix := FPrefix + PREFIX_JOIN_SEPARATOR;
end;

constructor TmBuiltInJoin.Create;
begin
  FActive:= false;
  FPrefix:= '';
  FBuildExternalEntityKeyFunction:= nil;
  FProvider:= nil;
end;

end.
