// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mVirtualFieldDefs;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, DB,
  StrHashMap;

type

  TVirtualFieldDefType = (vftUnknown, vftInteger, vftBoolean, vftString, vftFloat, vftCurrency,
    vftDate, vftTime, vftDateTime, vftWideString, vftTimeStamp, vftBCD);


  { TVirtualFieldDef }

  TVirtualFieldDef = class(TCollectionItem)
  strict private
    FName : string;
    FDataType : TVirtualFieldDefType;
    FSize : integer;
    FReadOnly : boolean;
    FRequired : boolean;
    FPrecision : integer;
    procedure SetDataType(AValue: TVirtualFieldDefType);
    procedure SetName(AValue: string);
  private
    FOnChangeName : TNotifyEvent;
  public
    constructor Create(ACollection: TCollection); override;
    procedure Assign(const aSource : TVirtualFieldDef);
  public
    property Name : string read FName write SetName;
    property DataType : TVirtualFieldDefType read FDataType write SetDataType;
    property Size : integer read FSize write FSize default 0;
    property Required : boolean read FRequired write FRequired default false;
    property ReadOnly : boolean read FReadOnly write FReadOnly default false;
    property Precision : integer read FPrecision write FPrecision default 0;
  end;

  { TVirtualFieldDefs }

  TVirtualFieldDefs = class(TCollection)
  strict private
    FIndex : TStringHashMap;
    function GetVirtualFieldDef(I: Integer): TVirtualFieldDef;
    procedure OnChangeFieldName (aSender : TObject);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    function AddFieldDef: TVirtualFieldDef;
    procedure Assign (const aSource : TVirtualFieldDefs);
    function FindByName (const aName : string) : TVirtualFieldDef;

    property VirtualFieldDefs[I: Integer]: TVirtualFieldDef read GetVirtualFieldDef; default;
  end;

  function FromTVirtualFieldDefTypeToTFieldType (aSource : TVirtualFieldDefType) : TFieldType;

implementation

uses
  SysUtils;

function FromTVirtualFieldDefTypeToTFieldType(aSource: TVirtualFieldDefType): TFieldType;
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

{ TVirtualFieldDef }

procedure TVirtualFieldDef.SetDataType(AValue: TVirtualFieldDefType);
begin
  if FDataType=AValue then Exit;
  FDataType:=AValue;
end;

procedure TVirtualFieldDef.SetName(AValue: string);
begin
  if FName=AValue then Exit;
  FName:=uppercase(AValue);
  if Assigned(FOnChangeName) then
    FOnChangeName(Self);
end;

constructor TVirtualFieldDef.Create(ACollection: TCollection);
begin
  inherited;

  FDataType:= vftUnknown;
  FName := '';
  FOnChangeName:= nil;
end;

procedure TVirtualFieldDef.Assign(const aSource: TVirtualFieldDef);
begin
  Self.Name := aSource.Name;
  Self.DataType := aSource.DataType;
  Self.Size := aSource.Size;
  Self.Required := aSource.Required;
  Self.ReadOnly := aSource.ReadOnly;
  Self.Precision := aSource.Precision;
end;

{ TVirtualFieldDefs }

function TVirtualFieldDefs.AddFieldDef: TVirtualFieldDef;
begin
  Result := Add as TVirtualFieldDef;
  FIndex.Clear;
  Result.FOnChangeName:= Self.OnChangeFieldName;
end;

procedure TVirtualFieldDefs.Assign(const aSource: TVirtualFieldDefs);
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

function TVirtualFieldDefs.FindByName(const aName: string): TVirtualFieldDef;
var
  tmp : pointer;
  i : integer;
begin
  if FIndex.Count = 0 then
  begin
    for i := 0 to Self.Count - 1 do
      FIndex.Add(Uppercase(Self.VirtualFieldDefs[i].Name), Self.VirtualFieldDefs[i]);
  end;
  if FIndex.Find(Uppercase(aName), tmp) then
    Result := TVirtualFieldDef(tmp)
  else
    Result := nil;
end;


constructor TVirtualFieldDefs.Create;
begin
  inherited Create(TVirtualFieldDef);
  FIndex := TStringHashMap.Create();
end;

destructor TVirtualFieldDefs.Destroy;
begin
  FIndex.Free;
  inherited Destroy;
end;

function TVirtualFieldDefs.GetVirtualFieldDef(I: Integer): TVirtualFieldDef;
begin
  Result := Items[I] as TVirtualFieldDef;
end;

procedure TVirtualFieldDefs.OnChangeFieldName(aSender: TObject);
begin
  FIndex.Clear;
end;

end.
