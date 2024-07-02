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
  mDatabaseConnectionClasses, mFilterOperators,
  mSQLDialectExpertImpl, mSQLDialectExpertImplRegister, mIntList, mDoubleList;

type

  { TSQLDialectExpertImplSQLServer }

  TSQLDialectExpertImplSQLServer = class (TSQLDialectExpertImpl)
  public
    constructor Create; override;
    function GetSQLForParameter (aParam : TmQueryParameter) : string; override;
    function GetSQLForConditionOperator (const aOperator: TmFilterOperator) : string; override;
  end;


implementation

uses
  sysutils, strutils,
  mSQLDialectSQLServer;

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
  Separator, escapeChar : string;
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
      ptBoolean:
        raise Exception.Create('unimplemented');
      ptDate:
        begin
          if (aParam.FilterOperator = TmFilterOperator.foIn) then
          begin
            IntegerList := TIntegerList.Create;
            try
              aParam.AsDateList(IntegerList);
              Result := Result + '(';
              for i := 0 to IntegerList.Count -1 do
              begin
                Result := Result + Separator + DateToSQLString(IntegerList.Items[i]);
                Separator := ',';
              end;
              Result := Result + ')';
            finally
              IntegerList.Free;
            end;
          end
          else if (aParam.FilterOperator = TmFilterOperator.foBetween) then
          begin
            IntegerList := TIntegerList.Create;
            try
              aParam.AsDateList(IntegerList);
              if IntegerList.Count = 2 then
                Result := DateToSQLString(IntegerList.Items[0]) + ' AND ' + DateToSQLString(IntegerList.Items[1])
              else
                raise Exception.Create('Wrong number of values for BETWEEN operator');
            finally
              IntegerList.Free;
            end;
          end
          else
            Result := DateToSQLString(aParam.AsDate);
        end;
      ptTime:
      begin
        if (aParam.FilterOperator = TmFilterOperator.foIn) then
        begin
          DoubleList := TDoubleList.Create;
          try
            aParam.AsDateTimeList(DoubleList);
            Result := Result + '(';
            for i := 0 to DoubleList.Count -1 do
            begin
              Result := Result + Separator + TimeToSQLString(DoubleList.Nums[i]);
              Separator := ',';
            end;
            Result := Result + ')';
          finally
            DoubleList.Free;
          end;
        end else if (aParam.FilterOperator = TmFilterOperator.foBetween) then
        begin
          DoubleList := TDoubleList.Create;
          try
            aParam.AsDateTimeList(DoubleList);
            if DoubleList.Count = 2 then
              Result := TimeToSQLString(DoubleList.Items[0]) + ' AND ' + TimeToSQLString(DoubleList.Items[1])
            else
              raise Exception.Create('Wrong number of values for BETWEEN operator');
          finally
            DoubleList.Free;
          end;
        end
        else
          Result := TimeToSQLString(aParam.AsTime);
      end;
      ptDateTime:
        begin
          if (aParam.FilterOperator = TmFilterOperator.foIn) then
          begin
            DoubleList := TDoubleList.Create;
            try
              aParam.AsDateTimeList(DoubleList);
              Result := Result + '(';
              for i := 0 to DoubleList.Count -1 do
              begin
                Result := Result + Separator + DateTimeToSQLString(DoubleList.Nums[i]);
                Separator := ',';
              end;
              Result := Result + ')';
            finally
              DoubleList.Free;
            end;
          end else if (aParam.FilterOperator = TmFilterOperator.foBetween) then
          begin
            DoubleList := TDoubleList.Create;
            try
              aParam.AsDateTimeList(DoubleList);
              if DoubleList.Count = 2 then
                Result := DateTimeToSQLString(DoubleList.Items[0]) + ' AND ' + DateTimeToSQLString(DoubleList.Items[1])
              else
                raise Exception.Create('Wrong number of values for BETWEEN operator');
            finally
              DoubleList.Free;
            end;
          end
          else
            Result := DateTimeToSQLString(aParam.AsDateTime);
        end;
      ptString:
        begin
          if (aParam.FilterOperator = TmFilterOperator.foIn) then
          begin
            StringList := TStringList.Create;
            try
              aParam.AsStringList(StringList);
              Result := Result + '(';
              for i := 0 to StringList.Count -1 do
              begin
                Result := Result + Separator + StringToSQLString(StringList.Strings[i]);
                Separator := ',';
              end;
              Result := Result + ')';
            finally
              StringList.Free;
            end;
          end else if (aParam.FilterOperator = TmFilterOperator.foBetween) then
          begin
            StringList := TStringList.Create;
            try
              aParam.AsStringList(StringList);
              if StringList.Count = 2 then
                Result := StringToSQLString(StringList.Strings[0]) + ' AND ' + StringToSQLString(StringList.Strings[1])
              else
                raise Exception.Create('Wrong number of values for BETWEEN operator');
            finally
              StringList.Free;
            end;
          end else if (aParam.FilterOperator = TmFilterOperator.foLike) then
          begin
            if AnsiContainsStr(aParam.AsString, '_') then
            begin
              escapeChar:= GetEscapeChar(aParam.AsString);
              Result := StringToSQLString('%' + StringReplace(aParam.AsString, '_', escapeChar + '_', [rfReplaceAll]) + '%') + ' ESCAPE ''' + escapeChar + '''';
            end
            else
              Result := StringToSQLString('%' + aParam.AsString + '%')
          end
          else if (aParam.FilterOperator = TmFilterOperator.foStartWith) then
          begin
            if AnsiContainsStr(aParam.AsString, '_') then
            begin
              escapeChar:= GetEscapeChar(aParam.AsString);
              Result := StringToSQLString(StringReplace(aParam.AsString, '_', escapeChar + '_', [rfReplaceAll]) + '%') + ' ESCAPE ''' + escapeChar + '''';
            end
            else
              Result := StringToSQLString(aParam.AsString + '%');
          end
          else if (aParam.FilterOperator = TmFilterOperator.foEndWith) then
          begin
            if AnsiContainsStr(aParam.AsString, '_') then
            begin
              escapeChar:= GetEscapeChar(aParam.AsString);
              Result := StringToSQLString('%' + StringReplace(aParam.AsString, '_', escapeChar + '_', [rfReplaceAll])) + ' ESCAPE ''' + escapeChar + '''';
            end
            else
              Result := StringToSQLString('%' + aParam.AsString);
          end
          else
            Result := StringToSQLString(aParam.AsString);
        end;
      ptWideString:
        begin
          if (aParam.FilterOperator = TmFilterOperator.foIn) then
          begin
            StringList := TStringList.Create;
            try
              aParam.AsStringList(StringList);
              Result := Result + '(';
              for i := 0 to StringList.Count -1 do
              begin
                Result := Result + Separator + StringToSQLString(StringList.Strings[i]);
                Separator := ',';
              end;
              Result := Result + ')';
            finally
              StringList.Free;
            end;
          end
          else if (aParam.FilterOperator = TmFilterOperator.foBetween) then
          begin
            StringList := TStringList.Create;
            try
              aParam.AsStringList(StringList);
              if StringList.Count = 2 then
                Result := StringToSQLString(StringList.Strings[0]) + ' AND ' + StringToSQLString(StringList.Strings[1])
              else
                raise Exception.Create('Wrong number of values for BETWEEN operator');
            finally
              StringList.Free;
            end;
          end else if (aParam.FilterOperator = TmFilterOperator.foLike) then
          begin
            if AnsiContainsStr(aParam.AsString, '_') then
            begin
              escapeChar:= GetEscapeChar(aParam.AsString);
              Result := StringToSQLString('%' + StringReplace(aParam.AsString, '_', escapeChar + '_', [rfReplaceAll]) + '%') + ' ESCAPE ''' + escapeChar + '''';
            end
            else
              Result := StringToSQLString('%' + aParam.AsString + '%')
          end
          else if (aParam.FilterOperator = TmFilterOperator.foStartWith) then
          begin
            if AnsiContainsStr(aParam.AsString, '_') then
            begin
              escapeChar:= GetEscapeChar(aParam.AsString);
              Result := StringToSQLString(StringReplace(aParam.AsString, '_', escapeChar + '_', [rfReplaceAll]) + '%') + ' ESCAPE ''' + escapeChar + '''';
            end
            else
              Result := StringToSQLString(aParam.AsString + '%');
          end
          else if (aParam.FilterOperator = TmFilterOperator.foEndWith) then
          begin
            if AnsiContainsStr(aParam.AsString, '_') then
            begin
              escapeChar:= GetEscapeChar(aParam.AsString);
              Result := StringToSQLString('%' + StringReplace(aParam.AsString, '_', escapeChar + '_', [rfReplaceAll])) + ' ESCAPE ''' + escapeChar + '''';
            end
            else
              Result := StringToSQLString('%' + aParam.AsString);
          end
          else
            Result := StringToSQLString(aParam.AsString);
        end;
      ptInteger:
        begin
          if (aParam.FilterOperator = TmFilterOperator.foIn) then
          begin
            IntegerList := TIntegerList.Create;
            try
              aParam.AsIntegerList(IntegerList);
              Result := Result + '(';
              for i := 0 to IntegerList.Count -1 do
              begin
                Result := Result + Separator + IntToStr(IntegerList.Items[i]);
                Separator := ',';
              end;
              Result := Result + ')';
            finally
              IntegerList.Free;
            end;
          end else if (aParam.FilterOperator = TmFilterOperator.foBetween) then
          begin
            IntegerList := TIntegerList.Create;
            try
              aParam.AsIntegerList(IntegerList);
              if IntegerList.Count = 2 then
                Result := IntToStr(IntegerList.Items[0]) + ' AND ' + IntToStr(IntegerList.Items[1])
              else
                raise Exception.Create('Wrong number of values for BETWEEN operator');
            finally
              IntegerList.Free;
            end;
          end
          else
            Result := IntToStr(aParam.AsInteger);
        end;
      ptFloat:
        begin
          if (aParam.FilterOperator = TmFilterOperator.foIn) then
          begin
            DoubleList := TDoubleList.Create;
            try
              aParam.AsFloatList(DoubleList);
              Result := Result + '(';
              for i := 0 to DoubleList.Count -1 do
              begin
                Result := Result + Separator + FloatToSQLString(DoubleList.Nums[i]);
                Separator := ',';
              end;
              Result := Result + ')';
            finally
              DoubleList.Free;
            end;
          end else if (aParam.FilterOperator = TmFilterOperator.foBetween) then
          begin
            DoubleList := TDoubleList.Create;
            try
              aParam.AsFloatList(DoubleList);
              if DoubleList.Count = 2 then
                Result := FloatToSQLString(DoubleList.Items[0]) + ' AND ' + FloatToSQLString(DoubleList.Items[1])
              else
                raise Exception.Create('Wrong number of values for BETWEEN operator');
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

function TSQLDialectExpertImplSQLServer.GetSQLForConditionOperator(const aOperator: TmFilterOperator): string;
begin
  if (aOperator = foNotEq) then  // null-safe equality operator, see https://learn.microsoft.com/en-us/sql/t-sql/queries/is-distinct-from-transact-sql?view=sql-server-ver16
    Result := 'IS DISTINCT FROM'
  else
    Result:=inherited GetSQLForConditionOperator(aOperator);
end;

initialization
  GetSQLDialectExpertsRegister.RegisterImplementations(dvSQLServer, TSQLDialectExpertImplSQLServer);

end.
