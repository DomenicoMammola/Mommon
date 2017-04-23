// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mQuickReadOnlyVirtualDataSet;

interface

uses
  DB, Classes,
  mVirtualDataSet, mVirtualDataSetInterfaces;

const
  KEY_FIELD_NAME = 'KEY';

type

  { TReadOnlyVirtualDatasetProvider }

  TReadOnlyVirtualDatasetProvider = class (TVirtualDatasetDataProvider)
  strict private
//    FDataProvider : TVDListDataProvider;
    FIDataProvider : IVDListDataProvider;
  public
    constructor Create; override;
    destructor Destroy; override;

//    procedure Init (aDataProvider : TVDListDataProvider); overload;
    procedure Init (aDataProvider : IVDListDataProvider); //overload;

    procedure GetFieldValue (AField: TField; AIndex: Integer; var AValue: variant); override;
    procedure DeleteRecord (AIndex :integer); override;
    procedure EditRecord (AIndex : integer; AModifiedFields : TList); override;
    procedure InsertRecord (AIndex : integer; AModifiedFields : TList); override;
    function GetRecordCount : integer; override;
  end;

implementation

uses
  SysUtils;

{ TReadOnlyVirtualDatasetProvider }

constructor TReadOnlyVirtualDatasetProvider.Create;
begin
  inherited Create;
  //FDataProvider := nil;
end;

destructor TReadOnlyVirtualDatasetProvider.Destroy;
begin
  inherited Destroy;
end;

(*
procedure TReadOnlyVirtualDatasetProvider.Init(aDataProvider: TVDListDataProvider);
begin
  FDataProvider := aDataProvider;
end;*)

procedure TReadOnlyVirtualDatasetProvider.Init(aDataProvider: IVDListDataProvider);
begin
//  FDataProvider := nil;
  FIDataProvider := aDataProvider;
end;

procedure TReadOnlyVirtualDatasetProvider.GetFieldValue(AField: TField; AIndex: Integer; var AValue: variant);
var
  //tmp : TVDDatum;
  tmpI : IVDDatum;
begin
  AValue := Null;
  if (aIndex >= 0) then
  begin
   if CompareText(aField.FieldName, KEY_FIELD_NAME) = 0 then
     aValue := aIndex
   else
   begin
(*     if Assigned(FDataProvider) then
     begin
       tmp := FDataProvider.GetDatum(AIndex);
       aValue := tmp.GetPropertyByFieldName(AField.FieldName);
     end
     else
     begin*)
       tmpI := FIDataProvider.GetDatum(aIndex);
       aValue := tmpI.GetPropertyByFieldName(AField.FieldName);
//     end;
   end;
  end;
end;

procedure TReadOnlyVirtualDatasetProvider.DeleteRecord(AIndex: integer);
begin
  // do nothing
end;

procedure TReadOnlyVirtualDatasetProvider.EditRecord(AIndex: integer; AModifiedFields: TList);
begin
  // do nothing
end;

procedure TReadOnlyVirtualDatasetProvider.InsertRecord(AIndex: integer; AModifiedFields: TList);
begin
  // do nothing
end;

function TReadOnlyVirtualDatasetProvider.GetRecordCount: integer;
begin
(*  if Assigned(FDataProvider) then
    Result := FDataProvider.Count
  else*)
    Result := FIDataProvider.Count;
end;

end.
