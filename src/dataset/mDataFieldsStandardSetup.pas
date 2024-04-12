// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mDataFieldsStandardSetup;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  DB,
  mFields;


function GenerateDisplayLabel(aSourceString : String) : String;
procedure ApplyStandardSettingsToFields (aDataset : TDataset; aStandardFloatFormat : String; const aPreserveDisplayFloatFormatIfAny : boolean = false); overload;
procedure ApplyStandardSettingsToFields (aFields : TmFields; aStandardFloatFormat : String; const aPreserveDisplayFloatFormatIfAny : boolean = false); overload;

implementation

uses
  SysUtils, StrUtils,
  mDataFieldsUtility;

function GenerateDisplayLabel(aSourceString: String): String;
var
  i : integer;
  curChar : Char;
  doCapitalize : boolean;
begin
  Result := '';
  doCapitalize := true;
  for i := 1 to Length(aSourceString) do
  begin
    curChar := aSourceString[i];
    case curChar of
      '_', #9, #13, #10:
        begin
          Result := Result + ' ';
          doCapitalize:= true;
        end;
      SEPARATOR_FIELDS_FROM_JOIN, SEPARATOR_FIELDS_FROM_INTERNAL_REFERENCE :
        begin
          Result := Result + '.';
          doCapitalize := true;
        end;
      else
        begin
          if doCapitalize then
          begin
            doCapitalize:= false;
            Result := Result + UpperCase(curChar);
          end
          else
          begin
            Result := Result + LowerCase(curChar);
            if curChar = '\' then
              doCapitalize := true;
          end;
        end;
    end;
  end;
  (*
  Result := SysUtils.StringReplace(aSourceString, '_', ' ', [rfReplaceAll]);
  Result := SysUtils.StringReplace(Result, SEPARATOR_FIELDS_FROM_JOIN, '.', [rfReplaceAll]);
  Result := SysUtils.StringReplace(Result, SEPARATOR_FIELDS_FROM_INTERNAL_REFERENCE, '.', [rfReplaceAll]);
  Result := StrUtils.AnsiPropercase(Result,[' ', #9, '\', #13, #10, '.']);
  *)
end;

procedure ApplyStandardSettingsToFields(aDataset: TDataset; aStandardFloatFormat : String; const aPreserveDisplayFloatFormatIfAny : boolean = false);
var
  i : integer;
begin
  for i := 0 to aDataset.Fields.Count - 1 do
  begin
    if aDataset.Fields[i] is TFloatField then
    begin
      if (not aPreserveDisplayFloatFormatIfAny) or (aPreserveDisplayFloatFormatIfAny and ((aDataset.Fields[i] as TFloatField).EditFormat = '')) then
      begin
        (aDataset.Fields[i] as TFloatField).EditFormat:= aStandardFloatFormat;
        (aDataset.Fields[i] as TFloatField).DisplayFormat:= aStandardFloatFormat;
      end;
    end;

    if aDataset.Fields[i].DisplayLabel <> '' then
      aDataset.Fields[i].DisplayLabel := GenerateDisplayLabel(aDataset.Fields[i].DisplayLabel)
    else
      aDataset.Fields[i].DisplayLabel := GenerateDisplayLabel(aDataset.Fields[i].FieldName);

    if IsSystemField(aDataset.Fields[i].FieldName) then
      aDataset.Fields[i].Visible:= false;
  end;
end;

procedure ApplyStandardSettingsToFields(aFields: TmFields; aStandardFloatFormat: String; const aPreserveDisplayFloatFormatIfAny : boolean = false);
var
  i : integer;
begin
  for i := 0 to aFields.Count - 1 do
  begin
    if FieldTypeIsFloat(aFields.Get(i).DataType) then
    begin
      if (not aPreserveDisplayFloatFormatIfAny) or (aPreserveDisplayFloatFormatIfAny and (aFields.Get(i).DisplayFormat = '')) then
      begin
        aFields.Get(i).EditFormat:= aStandardFloatFormat;
        aFields.Get(i).DisplayFormat := aStandardFloatFormat;
      end;
    end;

    if aFields.Get(i).DisplayLabel <> '' then
      aFields.Get(i).DisplayLabel := GenerateDisplayLabel(aFields.Get(i).DisplayLabel)
    else
      aFields.Get(i).DisplayLabel := GenerateDisplayLabel(aFields.Get(i).FieldName);

    if IsSystemField(aFields.Get(i).FieldName) then
      aFields.Get(i).Visible:= false;
  end;
end;

end.
