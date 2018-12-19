unit mBaseEntities;

interface

uses
  contnrs,
  mMaps, mNullables, mDataProviderInterfaces;

type



  TBaseEntity = class
  protected
    FProperties : TObjectList;
    FPropertiesIndex : TmStringDictionary;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;


  TEntityKey = class abstract({$IFNDEF FPC}TJavaInterfacedObject, {$ENDIF}IVDDatumKey)
  public
    procedure Assign(aSource : TObject); virtual; abstract;
    function AsString : string; virtual; abstract;
  end;


  { TEntity }

  TEntity = class abstract({$IFNDEF FPC}TJavaInterfacedObject, {$ENDIF}IVDDatum)
  strict private
  protected
    function GetKey : TEntityKey; virtual; abstract;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function GetDatumKey : IVDDatumKey; virtual;
    function GetPropertyByFieldName(const aFieldName : String) : Variant; virtual;
    function AsObject : TObject; virtual;
  end;

  TEntitiesRegister = class
  strict private

  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TEntity }

function TEntity.GetDatumKey: IVDDatumKey;
begin
  Result := GetKey;
end;

function TEntity.GetPropertyByFieldName(const aFieldName: String): Variant;
begin

end;

function TEntity.AsObject: TObject;
begin
  Result := Self;
end;

end.
