unit mBooleanDataProvider;

{$IFDEF FPC}
  {$MODE DELPHI}
 {$ENDIF}
 
interface

uses
  mInterfaces, mVirtualFieldDefs, mVirtualDataSetInterfaces;

type

  { TBooleanDatum }

  TBooleanDatum = class (TJavaInterfacedObject, IVDDatum)
  strict private
    const FLD_VALUE = 'VALUE';
  strict private
    FValue : boolean;
  public
    constructor Create(aValue : boolean);

    class procedure FillVirtualFieldDefs (aFieldDefs : TVirtualFieldDefs; aPrefix : String);
    class function GetKeyField : String;
    function GetPropertyByFieldName(aFieldName : String) : Variant;
  end;

  { TBooleanDataProvider }

  TBooleanDataProvider = class (TJavaInterfacedObject, IVDListDataProvider)
  strict private
    FTrueValue : TBooleanDatum;
    FFalseValue : TBooleanDatum;
  public
    constructor Create;
    destructor Destroy; override;
    function Count : integer;
    function GetDatum(aIndex : integer) : IVDDatum;
  end;

implementation

uses
  SysUtils;

{ TBooleanDataProvider }

constructor TBooleanDataProvider.Create;
begin
  FTrueValue := TBooleanDatum.Create(true);
  FFalseValue := TBooleanDatum.Create(false);
end;

destructor TBooleanDataProvider.Destroy;
begin
  FTrueValue.Free;
  FFalseValue.Free;
  inherited Destroy;
end;

function TBooleanDataProvider.Count: integer;
begin
  Result := 2;
end;

function TBooleanDataProvider.GetDatum(aIndex: integer): IVDDatum;
begin
  if aIndex = 0 then
    Result := FTrueValue
  else
    Result := FFalseValue;
end;

{ TBooleanDatum }

constructor TBooleanDatum.Create(aValue: boolean);
begin
  FValue := aValue;
end;

class procedure TBooleanDatum.FillVirtualFieldDefs(aFieldDefs: TVirtualFieldDefs; aPrefix: String);
begin
  with aFieldDefs.AddFieldDef do
  begin
    Name := aPrefix + FLD_VALUE;
    DataType:= vftString;
    Size := 10;
  end;
end;

class function TBooleanDatum.GetKeyField: String;
begin
  Result := FLD_VALUE;
end;

function TBooleanDatum.GetPropertyByFieldName(aFieldName: String): Variant;
begin
  Result := BoolToStr(FValue, true);
end;

end.
