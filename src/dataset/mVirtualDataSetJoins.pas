unit mVirtualDataSetJoins;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, contnrs, sysutils,
  mVirtualDataSetInterfaces, mMaps, mVirtualFieldDefs, mLog;

const
  PREFIX_JOIN_SEPARATOR = '$';

type
  TBuildExternalEntityKeyFromDatumFunction = function (aSource : IVDDatum): string of object;
  TFindDatumByStringKeyFunction = function (aStringKey : string): IVDDatum of object;

  { TmBuiltInJoin }

  TmBuiltInJoin = class
  strict private
    FPrefix : string;
    FDoBuildExternalEntityKey : TBuildExternalEntityKeyFromDatumFunction;
    FDoFindDatumByStringKey : TFindDatumByStringKeyFunction;
    FVirtualFieldDefs : TmVirtualFieldDefs;

    procedure SetPrefix(AValue: string);
  public
    constructor Create;
    destructor Destroy; override;

    property Prefix : string read FPrefix write SetPrefix;
    property DoBuildExternalEntityKey : TBuildExternalEntityKeyFromDatumFunction read FDoBuildExternalEntityKey write FDoBuildExternalEntityKey;
    property DoFindDatumByStringKey : TFindDatumByStringKeyFunction read FDoFindDatumByStringKey write FDoFindDatumByStringKey;
    property VirtualFieldDefs : TmVirtualFieldDefs read FVirtualFieldDefs;
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

  procedure ExtractPrefixAndFieldName (const aSource : String; var aPrefix, aFieldName : string);

implementation

//var
//  logger : TmLog;

procedure ExtractPrefixAndFieldName(const aSource: String; var aPrefix, aFieldName: string);
var
  i : integer;
begin
  aPrefix := '';
  aFieldName := aSource;
  i := Pos(PREFIX_JOIN_SEPARATOR, aSource);
  if i > 0 then
  begin
    aPrefix := Copy(aSource, 1, i);
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
end;

function TmBuiltInJoins.FindByPrefix(const aPrefix: string): TmBuiltInJoin;
var
  i : integer;
  tmp : string;
begin
  Result := nil;
  if FDictionary.Count = 0 then
  begin
    for i := 0 to Count - 1 do
    begin
      tmp := uppercase((FList.Items[i] as TmBuiltInJoin).Prefix);
      // logger.Debug('[TmBuiltInJoins.FindByPrefix] Add built-in join: ' + tmp);
      FDictionary.Add(tmp, Get(i));
    end;
  end;
  Result := FDictionary.Find(uppercase(aPrefix)) as TmBuiltInJoin;
end;

{ TmBuiltInJoin }

procedure TmBuiltInJoin.SetPrefix(AValue: string);
begin
  if FPrefix=AValue then Exit;
  FPrefix:=Uppercase(AValue);
  if Copy(FPrefix, Length(FPrefix), 1) <> PREFIX_JOIN_SEPARATOR then
    FPrefix := FPrefix + PREFIX_JOIN_SEPARATOR;
end;

constructor TmBuiltInJoin.Create;
begin
  FPrefix:= '';
  FDoBuildExternalEntityKey:= nil;
  FDoFindDatumByStringKey:= nil;
  FVirtualFieldDefs := TmVirtualFieldDefs.Create;
end;

destructor TmBuiltInJoin.Destroy;
begin
  FVirtualFieldDefs.Free;
  inherited Destroy;
end;

//initialization
//  logger := logManager.AddLog('mVirtualDataSetJoins');

end.
