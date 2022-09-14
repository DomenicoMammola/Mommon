// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mDatasetStandardSetup;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  DB;


function GenerateDisplayLabel(aSourceString : String) : String;
procedure ApplyStandardSettingsToFields (aDataset : TDataset; aStandardFloatFormat : String);

implementation

uses
  SysUtils, StrUtils,
  mFields;

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

procedure ApplyStandardSettingsToFields(aDataset: TDataset; aStandardFloatFormat : String);
var
  i : integer;
begin
  for i := 0 to aDataset.Fields.Count - 1 do
  begin
    if aDataset.Fields[i] is TFloatField then
    begin
      (aDataset.Fields[i] as TFloatField).EditFormat:= aStandardFloatFormat;
      (aDataset.Fields[i] as TFloatField).DisplayFormat:= aStandardFloatFormat;
    end;

    if aDataset.Fields[i].DisplayLabel <> '' then
      aDataset.Fields[i].DisplayLabel := GenerateDisplayLabel(aDataset.Fields[i].DisplayLabel)
    else
      aDataset.Fields[i].DisplayLabel := GenerateDisplayLabel(aDataset.Fields[i].FieldName);

    if IsSystemField(aDataset.Fields[i].FieldName) then
      aDataset.Fields[i].Visible:= false;
  end;

end;

end.
