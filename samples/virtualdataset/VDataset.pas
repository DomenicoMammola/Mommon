unit VDataset;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, DB,
  contnrs, sysutils,
  mVirtualDataSet, mVirtualFieldDefs, mVirtualDatasetPivoter;

type

  TCiccio = class
  private
    FVString : string;
    FVDouble : double;
    FVInteger : integer;
  public
  published
    property ValueString : string read FVString write FVString;
    property ValueFloat : double read FVDouble write FVDouble;
    property ValueInteger : integer read FVInteger write FVInteger;
  end;


  { TListVirtualDatasetDataProvider }

  TListVirtualDatasetDataProvider = class (TmVirtualDatasetDataProvider)
  private
    FList : TList;
    FGarbageCollector : TObjectList;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Add (AObj : TObject);

    procedure GetFieldValue (const AFieldName: String; const AIndex: Cardinal;
      out AValue: variant); override;
    procedure DeleteRecord (const AIndex :integer); override;
    procedure EditRecord (const AIndex : integer; AModifiedFields : TList); override;
    procedure InsertRecord (const AIndex : integer; AModifiedFields : TList); override;
    function GetRecordCount : integer; override;
  end;

implementation

{ TListVirtualDatasetDataProvider }

constructor TListVirtualDatasetDataProvider.Create;
begin
  inherited;
  FList := TList.Create;
  FGarbageCollector:= TObjectList.Create(true);;
  with Self.VirtualFieldDefs.AddFieldDef do
  begin
    Name := 'ValueString';
    DataType := vftString;
    Size := 30;
  end;
  with Self.VirtualFieldDefs.AddFieldDef do
  begin
    Name := 'ValueInteger';
    DataType := vftInteger;
  end;
  with Self.VirtualFieldDefs.AddFieldDef do
  begin
    Name := 'ValueFloat';
    DataType := vftFloat;
  end;
end;

procedure TListVirtualDatasetDataProvider.DeleteRecord(const AIndex :integer);
begin
  //
end;

destructor TListVirtualDatasetDataProvider.Destroy;
begin
  FList.Free;
  FGarbageCollector.Free;
  inherited;
end;

procedure TListVirtualDatasetDataProvider.Add(AObj: TObject);
begin
  FList.Add(AObj);
  FGarbageCollector.Add(AObj);
end;

procedure TListVirtualDatasetDataProvider.EditRecord(const AIndex : integer; AModifiedFields : TList);
var
  O : TCiccio;
  i : integer;
  F : TField;
begin
  O := TObject(FList[AIndex]) as TCiccio;
  for i := 0 to AModifiedFields.Count - 1 do
  begin
    F := TObject(AModifiedFields.Items[i]) as TField;
    if F.FieldName = 'ValueString' then
    begin
      O.ValueString := F.AsString;
    end
    else
    if F.FieldName = 'ValueInteger' then
    begin
      O.ValueInteger := F.AsInteger;
    end
    else
    begin
      O.ValueFloat := F.AsFloat;
    end;
  end;

end;

procedure TListVirtualDatasetDataProvider.GetFieldValue(const AFieldName: String; const AIndex: Cardinal;
      out AValue: variant);
var
  AObject : TCiccio;
begin
  if AIndex >= 0 then
  begin
    AObject := TCiccio(FList.Items[AIndex]);
    if CompareText(AFieldName, 'ValueString') = 0 then
      AValue := (AObject as TCiccio).ValueString
    else
    if CompareText(AFieldName, 'ValueInteger') = 0 then
      AValue := (AObject as TCiccio).ValueInteger
    else
    if CompareText(AFieldName, 'ValueFloat') = 0 then
      AValue := (AObject as TCiccio).ValueFloat;
  end;
end;

function TListVirtualDatasetDataProvider.GetRecordCount: integer;
begin
  Result := FList.Count;
end;

procedure TListVirtualDatasetDataProvider.InsertRecord(const AIndex : integer; AModifiedFields : TList);
begin
  //

end;


end.
