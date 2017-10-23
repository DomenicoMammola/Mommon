// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mSQLDialectExpertImplSQLServer;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes,
  mDatabaseConnectionClasses,
  mSQLDialectExpertImpl, mSQLDialectExpertImplRegister, mIntList, mDoubleList;

type

  { TSQLDialectExpertImplSQLServer }

  TSQLDialectExpertImplSQLServer = class (TSQLDialectExpertImpl)
  public
    constructor Create; override;
    function GetSQLForParameter (aParam : TmQueryParameter) : string; override;
  end;


implementation

uses
  SysUtils,
  mSQLServerSQLDialect;

{ TSQLDialectExpertImplSQLServer }

constructor TSQLDialectExpertImplSQLServer.Create;
begin
  // do nothing
end;

function TSQLDialectExpertImplSQLServer.GetSQLForParameter(aParam: TmQueryParameter): string;
var
  StringList : TStringList;
  DoubleList : TDoubleList;
  IntegerList : TIntegerList;
  i : integer;
  Separator : string;
begin
  Result := '';
  Separator := '';
  if aParam.IsNull then
  begin
    Result := 'NULL';
  end
  else
  begin
    case aParam.DataType of
      ptDate:
        begin
          if aParam.HasMultipleValues then
          begin
            IntegerList := TIntegerList.Create;
            try
              aParam.AsDateList(IntegerList);
              for i := 0 to IntegerList.Count -1 do
              begin
                Result := Result + Separator + DateToSQLString(IntegerList.Items[i]);
                Separator := ',';
              end;
            finally
              IntegerList.Free;
            end;
          end
          else
            Result := DateToSQLString(aParam.AsDate);
        end;
      ptDateTime:
        begin
          if aParam.HasMultipleValues then
          begin
            DoubleList := TDoubleList.Create;
            try
              aParam.AsDateTimeList(DoubleList);
              for i := 0 to DoubleList.Count -1 do
              begin
                Result := Result + Separator + DateTimeToSQLString(DoubleList.Nums[i]);
                Separator := ',';
              end;
            finally
              DoubleList.Free;
            end;
          end
          else
            Result := DateTimeToSQLString(aParam.AsDateTime);
        end;
      ptString:
        begin
          if aParam.HasMultipleValues then
          begin
            StringList := TStringList.Create;
            try
              aParam.AsStringList(StringList);
              for i := 0 to StringList.Count -1 do
              begin
                Result := Result + Separator + StringToSQLString(StringList.Strings[i]);
                Separator := ',';
              end;
            finally
              StringList.Free;
            end;
          end
          else
            Result := StringToSQLString(aParam.AsString);
        end;
      ptWideString:
        begin
          if aParam.HasMultipleValues then
          begin
            StringList := TStringList.Create;
            try
              aParam.AsStringList(StringList);
              for i := 0 to StringList.Count -1 do
              begin
                Result := Result + Separator + StringToSQLString(StringList.Strings[i]);
                Separator := ',';
              end;
            finally
              StringList.Free;
            end;
          end
          else
            Result := StringToSQLString(aParam.AsWideString);
        end;
      ptInteger:
        begin
          if aParam.HasMultipleValues then
          begin
            IntegerList := TIntegerList.Create;
            try
              aParam.AsIntegerList(IntegerList);
              for i := 0 to IntegerList.Count -1 do
              begin
                Result := Result + Separator + IntToStr(IntegerList.Items[i]);
                Separator := ',';
              end;
            finally
              IntegerList.Free;
            end;
          end
          else
            Result := IntToStr(aParam.AsInteger);
        end;
      ptFloat:
        begin
          if aParam.HasMultipleValues then
          begin
            DoubleList := TDoubleList.Create;
            try
              aParam.AsFloatList(DoubleList);
              for i := 0 to DoubleList.Count -1 do
              begin
                Result := Result + Separator + FloatToSQLString(DoubleList.Nums[i]);
                Separator := ',';
              end;
            finally
              DoubleList.Free;
            end;
          end
          else
            Result := FloatToSQLString(aParam.AsFloat);
        end;
    end;
  end;
end;

initialization
  GetSQLDialectExpertsRegister.RegisterImplementations(dvSQLServer, TSQLDialectExpertImplSQLServer);

end.
