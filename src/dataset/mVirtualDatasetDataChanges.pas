unit mVirtualDatasetDataChanges;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  contnrs,
  mMaps, mVirtualDataSetInterfaces, mNullables;

type

  { TVDDatumChanged }

  TVDDatumChanged = class
  protected
    FChangedDatum: IVDDatum;
    FRegisteredProperties: TmStringDictionary;
    FList: TObjectList;

    procedure RegisterProperty (const aFieldName: string; aProperty : TAbstractNullable);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure ChangePropertyValue (const aFieldName: string; const aValue: variant);
    function PropertyIsChanged (const aFieldName: string) : boolean;
    function IsChanged: boolean;

    property ChangedDatum: IVDDatum read FChangedDatum write FChangedDatum;
  end;

implementation

uses
  mBaseClassesAsObjects;

{ TVDDatumChanged }

constructor TVDDatumChanged.Create;
begin
  FRegisteredProperties:= TmStringDictionary.Create();
  FList := TObjectList.Create(false);
end;

destructor TVDDatumChanged.Destroy;
begin
  FRegisteredProperties.Free;
  FList.Free;
  inherited Destroy;
end;

procedure TVDDatumChanged.RegisterProperty(const aFieldName: string; aProperty : TAbstractNullable);
begin
  FList.Add(aProperty);
  FRegisteredProperties.Add(aFieldName, aProperty);
end;

procedure TVDDatumChanged.ChangePropertyValue(const aFieldName: string; const aValue: variant);
begin
  (FRegisteredProperties.Find(aFieldName) as TAbstractNullable).CheckIfDifferentAndAssign(aValue);
end;

function TVDDatumChanged.PropertyIsChanged(const aFieldName: string): boolean;
begin
  Result := (FRegisteredProperties.Find(aFieldName) as TAbstractNullable).TagChanged;
end;

function TVDDatumChanged.IsChanged: boolean;
var
  i : integer;
begin
  Result := false;
  for i := 0 to FList.Count - 1 do
  begin
    if (FList.Items[i] as TAbstractNullable).TagChanged then
    begin
      Result := true;
      exit;
    end;
  end;
end;

end.
