// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mDataProviderFieldDefs;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, DB,
  mMaps;

type

  TmVirtualFieldDefType = (vftUnknown, vftInteger, vftBoolean, vftString, vftFloat, vftCurrency,
    vftDate, vftTime, vftDateTime, vftWideString, vftTimeStamp, vftBCD);


  { TmVirtualFieldDef }

  TmVirtualFieldDef = class(TCollectionItem)
  strict private
    FName : string;
    FDataType : TmVirtualFieldDefType;
    FSize : integer;
    FReadOnly : boolean;
    FRequired : boolean;
    FPrecision : integer;
    FDefaultFormat : string;
    procedure SetDataType(AValue: TmVirtualFieldDefType);
    procedure SetName(AValue: string);
  private
    FOnChangeName : TNotifyEvent;
  public
    constructor Create(ACollection: TCollection); override;
    procedure Assign(const aSource : TmVirtualFieldDef); reintroduce;
  public
    property Name : string read FName write SetName;
    property DataType : TmVirtualFieldDefType read FDataType write SetDataType;
    property Size : integer read FSize write FSize default 0;
    property Required : boolean read FRequired write FRequired default false;
    property ReadOnly : boolean read FReadOnly write FReadOnly default false;
    property Precision : integer read FPrecision write FPrecision default 0;
    property DefaultFormat: string read FDefaultFormat write FDefaultFormat;
  end;

  { TmVirtualFieldDefs }

  TmVirtualFieldDefs = class(TCollection)
  strict private
    FIndex : TmStringDictionary;
    function GetVirtualFieldDef(I: Integer): TmVirtualFieldDef;
    procedure OnChangeFieldName (aSender : TObject);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    function AddFieldDef: TmVirtualFieldDef;
    procedure Assign (const aSource : TmVirtualFieldDefs); reintroduce;
    function FindByName (const aName : string) : TmVirtualFieldDef;
    procedure ExtractFieldNames (aList: TStringList);

    property VirtualFieldDefs[I: Integer]: TmVirtualFieldDef read GetVirtualFieldDef; default;
  end;

  function FromTmVirtualFieldDefTypeToTFieldType (aSource : TmVirtualFieldDefType) : TFieldType;

implementation

uses
  SysUtils;

function FromTmVirtualFieldDefTypeToTFieldType(aSource: TmVirtualFieldDefType): TFieldType;
begin
  Result := ftUnknown;
  case aSource of
    vftInteger : Result := ftInteger;
    vftBoolean : Result := ftBoolean;
    vftString : Result := ftString;
    vftFloat : Result := ftFloat;
    vftCurrency: Result := ftCurrency;
    vftDate : Result := ftDate;
    vftTime : Result := ftTime;
    vftDateTime : Result := ftDateTime;
    vftWideString : Result := ftWideString;
    vftTimeStamp : Result := ftTimeStamp;
    vftBCD : Result := ftBCD;
  end;
end;

{ TmVirtualFieldDef }

procedure TmVirtualFieldDef.SetDataType(AValue: TmVirtualFieldDefType);
begin
  if FDataType=AValue then Exit;
  FDataType:=AValue;
end;

procedure TmVirtualFieldDef.SetName(AValue: string);
begin
  if FName=AValue then Exit;
  FName:=uppercase(AValue);
  if Assigned(FOnChangeName) then
    FOnChangeName(Self);
end;

constructor TmVirtualFieldDef.Create(ACollection: TCollection);
begin
  inherited;

  FDataType:= vftUnknown;
  FName := '';
  FOnChangeName:= nil;
  FDefaultFormat:= '';
end;

procedure TmVirtualFieldDef.Assign(const aSource: TmVirtualFieldDef);
begin
  Self.Name := aSource.Name;
  Self.DataType := aSource.DataType;
  Self.Size := aSource.Size;
  Self.Required := aSource.Required;
  Self.ReadOnly := aSource.ReadOnly;
  Self.Precision := aSource.Precision;
end;

{ TmVirtualFieldDefs }

function TmVirtualFieldDefs.AddFieldDef: TmVirtualFieldDef;
begin
  Result := Add as TmVirtualFieldDef;
  FIndex.Clear;
  Result.FOnChangeName:= Self.OnChangeFieldName;
end;

procedure TmVirtualFieldDefs.Assign(const aSource: TmVirtualFieldDefs);
var
  i : integer;
begin
  Self.Clear;
  FIndex.Clear;
  for i := 0 to aSource.Count - 1 do
  begin
    Self.AddFieldDef.Assign(aSource.GetVirtualFieldDef(i));
  end;
end;

function TmVirtualFieldDefs.FindByName(const aName: string): TmVirtualFieldDef;
var
  i : integer;
begin
  if FIndex.Count = 0 then
  begin
    for i := 0 to Self.Count - 1 do
      FIndex.Add(Uppercase(Self.VirtualFieldDefs[i].Name), Self.VirtualFieldDefs[i]);
  end;
  Result := FIndex.Find(Uppercase(aName)) as TmVirtualFieldDef;
end;

procedure TmVirtualFieldDefs.ExtractFieldNames(aList: TStringList);
var
  i : integer;
begin
  aList.Clear;
  for i := 0 to Self.Count -1 do
    aList.Add(VirtualFieldDefs[i].Name);
end;


constructor TmVirtualFieldDefs.Create;
begin
  inherited Create(TmVirtualFieldDef);
  FIndex := TmStringDictionary.Create(false);
end;

destructor TmVirtualFieldDefs.Destroy;
begin
  FIndex.Free;
  inherited Destroy;
end;

function TmVirtualFieldDefs.GetVirtualFieldDef(I: Integer): TmVirtualFieldDef;
begin
  Result := Items[I] as TmVirtualFieldDef;
end;

procedure TmVirtualFieldDefs.OnChangeFieldName(aSender: TObject);
begin
  FIndex.Clear;
end;

end.
