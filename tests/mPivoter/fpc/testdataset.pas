unit TestDataset;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  mNullables, mDataProviderFieldDefs, mDataProviderInterfaces, mDataProvider;

type

  { THuman_Key }

  THuman_Key = class ({$IFNDEF FPC}TJavaInterfacedObject, {$ENDIF}IVDDatumKey)
  strict private
    FId : TNullableString;
  public
    constructor Create;
    function AsString : string;
    procedure Assign(aSource : TObject);

    property Id : TNullableString read FId;
  end;

  THuman = class ({$IFNDEF FPC}TJavaInterfacedObject, {$ENDIF}IVDDatum)
  strict private
    FKey : THuman_Key;
    FName : TNullableString;
    FGender : TNullableString;
    FBirthDay : TNullableDateTime;
    FEyes : TNullableString;
    FHeight : TNullableDouble;
  public
    const FLD_ID = 'ID';
    const FLD_NAME = 'NAME';
    const FLD_GENDER = 'GENDER';
    const FLD_BIRTHDAY = 'BIRTHDAY';
    const FLD_EYES = 'EYES';
    const FLD_HEIGHT = 'HEIGHT';
  public
    constructor Create;
    destructor Destroy; override;

    class procedure FillVirtualFieldDefs (aFieldDefs : TmVirtualFieldDefs; const aPrefix : String);
    function GetPropertyByFieldName(const aFieldName : String) : Variant;
    class function GetKeyField :String;
    procedure Assign(aSource : THuman);
    function Clone : IVDDatum;
    function GetDatumKey : IVDDatumKey;
    function AsObject : TObject;

    property Key : THuman_Key read FKey;
    property Name : TNullableString read FName;
    property Gender : TNullableString read FGender;
    property BirthDay : TNullableDateTime read FBirthDay;
    property Eyes : TNullableString read FEyes;
    property Height : TNullableDouble read FHeight;
  end;

  { THumans }

  THumans = class (TmDataProvider)
  public
    procedure Add (aHuman : THuman); overload;
    function Add : THuman; overload;
    function Get(aIndex : integer) : THuman;
    procedure FillVirtualFieldDefs (aFieldDefs : TmVirtualFieldDefs; const aPrefix : String); override;
    function GetKeyFieldName : String; override;
    procedure GetMinimumFields(aFieldsForLookup : TStringList); override;
  end;


implementation

{ THumans }

procedure THumans.Add(aHuman: THuman);
begin
  InternalAdd(aHuman);
end;

function THumans.Add: THuman;
begin
  Result := THuman.Create;
  Add(Result);
end;

function THumans.Get(aIndex: integer): THuman;
begin
  Result := InternalGetDatum(aIndex) as THuman;
end;

procedure THumans.FillVirtualFieldDefs(aFieldDefs: TmVirtualFieldDefs; const aPrefix: String);
begin
  THuman.FillVirtualFieldDefs(aFieldDefs, aPrefix);
end;

function THumans.GetKeyFieldName: String;
begin
  Result := THuman.GetKeyField;
end;

procedure THumans.GetMinimumFields(aFieldsForLookup: TStringList);
begin
  inherited GetMinimumFields(aFieldsForLookup);
end;

{ THuman }

constructor THuman.Create;
begin
  FKey := THuman_Key.Create;
  FName := TNullableString.Create;
  FGender := TNullableString.Create;
  FBirthDay := TNullableDateTime.Create;
  FEyes := TNullableString.Create;
  FHeight := TNullableDouble.Create;
end;

destructor THuman.Destroy;
begin
  FKey.Free;
  FName.Free;
  FGender.Free;
  FBirthDay.Free;
  FEyes.Free;
  FHeight.Free;
end;

class procedure THuman.FillVirtualFieldDefs(aFieldDefs: TmVirtualFieldDefs; const aPrefix: String);
begin
  with aFieldDefs.AddFieldDef do
  begin
    Name := aPrefix + FLD_ID;
    DataType:= vftString;
    Size := 50;
  end;
  with aFieldDefs.AddFieldDef do
  begin
    Name := aPrefix + FLD_NAME;
    DataType:= vftString;
    Size := 50;
  end;
  with aFieldDefs.AddFieldDef do
  begin
    Name := aPrefix + FLD_GENDER;
    DataType:= vftString;
    Size := 1;
  end;
  with aFieldDefs.AddFieldDef do
  begin
    Name := aPrefix + FLD_BIRTHDAY;
    DataType:= vftDate;
  end;
  with aFieldDefs.AddFieldDef do
  begin
    Name := aPrefix + FLD_EYES;
    DataType:= vftString;
    Size := 50;
  end;
  with aFieldDefs.AddFieldDef do
  begin
    Name := aPrefix + FLD_HEIGHT;
    DataType:= vftFloat;
  end;
end;

function THuman.GetPropertyByFieldName(const aFieldName: String): Variant;
begin
  Result := null;
  if aFieldName = FLD_ID then
    Result := FKey.Id.AsVariant
  else if aFieldName = FLD_NAME then
    Result := FName.AsVariant
  else if aFieldName = FLD_GENDER then
    Result := FGender.AsVariant
  else if aFieldName = FLD_BIRTHDAY then
    Result := FBirthDay.AsVariant
  else if aFieldName = FLD_EYES then
    Result := FEyes.AsVariant
  else if aFieldName = FLD_HEIGHT then
    Result := FHeight.AsVariant;
end;

class function THuman.GetKeyField: String;
begin
  Result := FLD_ID;
end;

procedure THuman.Assign(aSource: THuman);
begin
  Self.Key.Assign(aSource.Key);
  Self.Name.Assign(aSource.Name);
  Self.Gender.Assign(aSource.Gender);
  Self.BirthDay.Assign(aSource.BirthDay);
  Self.Eyes.Assign(aSource.Eyes);
  Self.Height.Assign(aSource.Height);
end;

function THuman.Clone: IVDDatum;
var
  h : THuman;
begin
  h := THuman.Create;
  h.Assign(Self);
  Result := h;
end;

function THuman.GetDatumKey: IVDDatumKey;
begin
  Result := FKey;
end;

function THuman.AsObject: TObject;
begin
  Result := Self;
end;

{ THuman_Key }

constructor THuman_Key.Create;
begin
  FId := TNullableString.Create;
end;

function THuman_Key.AsString: string;
begin
  Result := FId.AsString;
end;

procedure THuman_Key.Assign(aSource: TObject);
begin
  FId.Assign((aSource as THuman_Key).Id);
end;

end.

