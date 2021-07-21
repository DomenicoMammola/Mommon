// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mVirtualDatasetFormulas;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, contnrs, db,
  mDataProviderFieldDefs, StrHashMap;

type

  TmFormulaFieldType = (fftFloat, fftString, fftDateTime);

  { TmFormulaField }

  TmFormulaField = class
  strict private
    FDataType : TmFormulaFieldType;
    FSize : integer;
    FFormula : String;
    FName : string;
    procedure SetName(AValue: string);
  private
    FOnChangeName : TNotifyEvent;
  public
    constructor Create;
    procedure ResetSize;

    property DataType : TmFormulaFieldType read FDataType write FDataType;
    property Size : integer read FSize write FSize;
    property Formula : string read FFormula write FFormula;
    property Name : string read FName write SetName;
  end;

  { TmFormulaFields }

  TmFormulaFields = class
  strict private
    FList : TObjectList;
    FIndex : TStringHashMap;
    procedure OnChangeFieldName (aSender : TObject);
  public
    constructor Create;
    destructor Destroy; override;

    function Add : TmFormulaField;
    function Count : integer;
    function Get (const aIndex : integer) : TmFormulaField;
    function FindByName (const aName : string) : TmFormulaField;
    procedure Delete (aName : string);
    procedure Clear;
  end;

  function FromTmFormulaFieldTypeToTFieldType (const aSource : TmFormulaFieldType) : TFieldType;
  function TmFormulaFieldTypeToString(const aSource : TmFormulaFieldType) : string;
  function StringToTmFormulaFieldType(const aValue : string): TmFormulaFieldType;

implementation

uses
  sysutils;

function FromTmFormulaFieldTypeToTFieldType(const aSource: TmFormulaFieldType): TFieldType;
begin
  if aSource = fftFloat then
    Result := ftFloat
  else if aSource = fftDateTime then
    Result := ftDateTime
  else
    Result := ftString;
end;

function TmFormulaFieldTypeToString(const aSource: TmFormulaFieldType): string;
begin
  if aSource = fftFloat then
    Result := 'float'
  else if aSource = fftDateTime then
    Result := 'datetime'
  else
    Result := 'string';
end;

function StringToTmFormulaFieldType(const aValue: string): TmFormulaFieldType;
begin
  if aValue = 'float' then
    Result := fftFloat
  else if aValue = 'datetime' then
    Result := fftDateTime
  else Result := fftString;
end;

{ TmFormulaFields }

procedure TmFormulaFields.OnChangeFieldName(aSender: TObject);
begin
  FIndex.Clear;
end;

constructor TmFormulaFields.Create;
begin
  FList := TObjectList.Create(true);
  FIndex := TStringHashMap.Create;
end;

destructor TmFormulaFields.Destroy;
begin
  FIndex.Free;
  FList.Free;
  inherited Destroy;
end;

function TmFormulaFields.Add: TmFormulaField;
begin
  Result := TmFormulaField.Create;
  Result.FOnChangeName:= Self.OnChangeFieldName;
  FList.Add(Result);
  FIndex.Clear;
end;

function TmFormulaFields.Count: integer;
begin
  Result := FList.Count;
end;

function TmFormulaFields.Get(const aIndex: integer): TmFormulaField;
begin
  Result := FList.Items[aIndex] as TmFormulaField;
end;

function TmFormulaFields.FindByName(const aName: string): TmFormulaField;
var
  i : integer;
  tmp : pointer;
begin
  if FIndex.Count = 0 then
  begin
    for i := 0 to Self.Count -1 do
      FIndex.Add(Uppercase(Self.Get(i).Name), Self.Get(i));
  end;
  if FIndex.Find(Uppercase(aName), tmp) then
    Result := TmFormulaField(tmp)
  else
    Result := nil;
end;

procedure TmFormulaFields.Delete(aName: string);
var
  i : integer;
begin
  for i := 0 to Self.Count - 1 do
  begin
    if CompareText(Self.Get(i).Name, aName) = 0 then
    begin
      FList.Delete(i);
      FIndex.Clear;
      exit;
    end;
  end;
end;

procedure TmFormulaFields.Clear;
begin
  FList.Clear;
  FIndex.Clear;
end;

{ TmFormulaField }

procedure TmFormulaField.SetName(AValue: string);
begin
  if FName=AValue then Exit;
  FName:=uppercase(AValue);
  if Assigned(FOnChangeName) then
    FOnChangeName(Self);
end;

constructor TmFormulaField.Create;
begin
  FDataType:= fftFloat;
  ResetSize;
  FName := '';
  FOnChangeName:= nil;
end;

procedure TmFormulaField.ResetSize;
begin
  FSize := 0;
end;

end.
