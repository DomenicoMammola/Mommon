// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mDataPump;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

{$I mDefines.inc}

uses
  DB, Classes,
  mDatabaseConnection, mDatabaseConnectionClasses,
  mProgress, mThreads,
  mDataPumpConfiguration;

type

  { TReplicaEngine }

  TReplicaEngine = class
  strict private
    FCanTerminate : boolean;

    procedure InternalReplicaTable (aProgress: ImProgress; aData : TObject; aJobResult : TJobResult);
    procedure DoTerminate(const aJobsResult : TJobResults);
  public
    function ProcessTables (const aTables : TDataReplicaTables; const aMaxConcurrentThreads : integer = -1) : boolean;
  end;

  function DoReplicaTable (const aTable : TDataReplicaTableToTable) : boolean;


implementation

uses
  SysUtils, md5, variants,
  mSQLBuilder, mFilterOperators, mMaps, mUtility, mLog;

type

  { TParameterTypeShell }

  TParameterTypeShell = class
  public
    ParamType : TmParameterDataType;

    constructor Create(const aParameterType : TmParameterDataType);
  end;

var
  logger : TmLog;

function ComposeDestinationSelectQuery (aTable: TDataReplicaTableToTable; const aDestinationSB : TmSQLBuilder): String;
var
  i : integer;
  comma, andStr : String;
  curFieldToField: TDataReplicaFieldToField;
begin
  Result := 'select ';
  comma := '';
  for i := 0 to aTable.FieldsMapping.Count - 1 do
  begin
    Result := Result + comma + aDestinationSB.SQLDialectExpert.GetSQLForFieldname(aTable.FieldsMapping.Get(i).DestinationField.AsString);
    comma := ','
  end;
  Result := Result + ' from ' + aDestinationSB.SQLDialectExpert.GetSQLForTablename(aTable.DestinationTableName);
  Result := Result + ' where ';
  andStr := '';
  for i := 0 to aTable.SourceKeyFields.Count - 1 do
  begin
    curFieldToField := aTable.FieldsMapping.GetBySourceFieldName(aTable.SourceKeyFields.Strings[i]);
    if not Assigned(curFieldToField) then
      raise Exception.Create('Key field ' + aTable.SourceKeyFields.Strings[i] + ' not found in source query');
    Result := Result + andStr + aDestinationSB.SQLSnippetForCondition(curFieldToField.DestinationField.AsString, foEq, aTable.SourceKeyFields.Strings[i]);
    andStr := 'and';
  end;
end;

function ComposeDestinationInsertQuery (aTable: TDataReplicaTableToTable; const aDestinationSB : TmSQLBuilder): String;
var
  i: integer;
  comma: String;
begin
  Result := 'insert into ' + aDestinationSB.SQLDialectExpert.GetSQLForTablename(aTable.DestinationTableName) + ' (';
  comma := '';
  for i:= 0 to aTable.FieldsMapping.Count - 1 do
  begin
    Result := Result + comma + aDestinationSB.SQLDialectExpert.GetSQLForFieldname(aTable.FieldsMapping.Get(i).DestinationField.AsString);
    comma := ','
  end;
  Result := Result + ') values (';
  comma := '';
  for i:= 0 to aTable.FieldsMapping.Count - 1 do
  begin
    Result := Result + comma + aDestinationSB.SQLSnippetForValue(aTable.FieldsMapping.Get(i).SourceField.AsString);
    comma := ','
  end;
  Result := Result + ')';
end;

function ComposeDestinationUpdateQuery (aTable: TDataReplicaTableToTable; const aDestinationSB : TmSQLBuilder): String;
var
  i : integer;
  comma, andStr: String;
  curFieldToField: TDataReplicaFieldToField;
begin
  Result := 'update ' + aDestinationSB.SQLDialectExpert.GetSQLForTablename(aTable.DestinationTableName) + ' set ';
  comma := '';
  for i:= 0 to aTable.FieldsMapping.Count - 1 do
  begin
    if aTable.SourceKeyFields.IndexOf(aTable.FieldsMapping.Get(i).SourceField.AsString) < 0 then
    begin
      Result := Result + comma + aDestinationSB.SQLDialectExpert.GetSQLForFieldname(aTable.FieldsMapping.Get(i).DestinationField.AsString) + '=' + aDestinationSB.SQLSnippetForValue(aTable.FieldsMapping.Get(i).SourceField.AsString);
      comma := ','
    end;
  end;
  Result := Result + ' where ';
  andStr := '';
  for i := 0 to aTable.SourceKeyFields.Count - 1 do
  begin
    curFieldToField := aTable.FieldsMapping.GetBySourceFieldName(aTable.SourceKeyFields.Strings[i]);
    Result := Result + andStr + aDestinationSB.SQLSnippetForCondition(curFieldToField.DestinationField.AsString, foEq, aTable.SourceKeyFields.Strings[i]);
    andStr := 'and';
  end;
end;

function DoReplicaTable(const aTable : TDataReplicaTableToTable): boolean;
var
  q: integer;
  sourceConnection, destinationConnection: TmDatabaseConnection;
  sourceQuery, destinationQuery: TmDatabaseQuery;
  SQLBuilderDestination, SQLBuilderInsert, SQLBuilderUpdate: TmSQLBuilder;
  performInsert, performUpdate: boolean;
  command : TmDatabaseCommand;
  sourcefld, destinationfld : TField;
  destinationFieldsMap, sourceFieldsMap : TmStringDictionary;
  tmpsql: String;
  tmpparameter : TmQueryParameter;
  paramshell : TParameterTypeShell;
  rows, performedInserts, performedUpdates : longint;
begin
  Result := false;
  sourceConnection := TmDatabaseConnection.Create(aTable.SourceConnectionInfo);
  destinationConnection := TmDatabaseConnection.Create(aTable.DestinationConnectionInfo);
  try
    sourceConnection.Connect;
    destinationConnection.Connect;

    destinationConnection.StartTransaction;
    try
      sourceQuery := TmDatabaseQuery.Create;
      command := TmDatabaseCommand.Create;
      destinationQuery := TmDatabaseQuery.Create;
      SQLBuilderDestination:=  TmSQLBuilder.Create;
      SQLBuilderInsert := TmSQLBuilder.Create;
      SQLBuilderUpdate := TmSQLBuilder.Create;
      destinationFieldsMap := TmStringDictionary.Create(true);
      sourceFieldsMap := TmStringDictionary.Create(false);
      try
        sourceQuery.DatabaseConnection:= sourceConnection;
        destinationQuery.DatabaseConnection:= destinationConnection;
        command.DatabaseConnection:= destinationConnection;
        SQLBuilderDestination.VendorType:= destinationConnection.ConnectionInfo.VendorType;
        SQLBuilderInsert.VendorType:= SQLBuilderDestination.VendorType;
        SQLBuilderUpdate.VendorType:= SQLBuilderDestination.VendorType;

        aTable.FieldsMapping.RebuildIndexes;

        sourceQuery.SetSQL(aTable.SourceSelectQuery);
        SQLBuilderDestination.PrepareSQL(ComposeDestinationSelectQuery(aTable, SQLBuilderDestination));
        SQLBuilderInsert.PrepareSQL(ComposeDestinationInsertQuery(aTable, SQLBuilderInsert));
        SQLBuilderUpdate.PrepareSQL(ComposeDestinationUpdateQuery(aTable, SQLBuilderUpdate));

        sourceFieldsMap.Clear;
        sourceQuery.Open;
        rows := 0;
        performedInserts := 0;
        performedUpdates := 0;
        while not sourceQuery.Eof do
        begin
          if sourceFieldsMap.Count = 0 then
          begin
            for q := 0 to sourceQuery.AsDataset.Fields.Count - 1 do
              sourceFieldsMap.Add(Uppercase(sourceQuery.AsDataset.Fields[q].FieldName), sourceQuery.AsDataset.Fields[q]);
          end;

          for q := 0 to aTable.SourceKeyFields.Count - 1 do
          begin
            sourcefld := sourceFieldsMap.Find(Uppercase(aTable.SourceKeyFields.Strings[q])) as TField;
            if not Assigned(sourcefld) then
              raise Exception.Create('Field ' + aTable.SourceKeyFields.Strings[q] + ' not found in source query');
            SQLBuilderDestination.ParamByName(aTable.SourceKeyFields.Strings[q]).Assign(sourcefld);
          end;

          tmpsql := SQLBuilderDestination.BuildSQL;
          destinationQuery.SetSQL(tmpsql);
          destinationQuery.Open;
          performInsert := destinationQuery.Eof;
          performUpdate := false;
          if destinationFieldsMap.Count = 0 then
          begin
            for q := 0 to destinationQuery.AsDataset.Fields.Count - 1 do
            begin
              destinationFieldsMap.Add(Uppercase(destinationQuery.AsDataset.Fields[q].FieldName), TParameterTypeShell.Create(DataTypeToParameterDataType(destinationQuery.AsDataset.Fields[q].DataType)));
            end;
          end;
          if not performInsert then
          begin
            for q := 0 to aTable.FieldsMapping.Count - 1 do
            begin
              sourcefld := sourceFieldsMap.Find(aTable.FieldsMapping.Get(q).SourceField.AsUppercaseString) as TField;
              destinationfld := destinationQuery.AsDataset.FieldByName(aTable.FieldsMapping.Get(q).DestinationField.AsString);
              performUpdate := MD5Print(MD5String(sourcefld.AsString)) <> MD5Print(MD5String(destinationfld.AsString));
              if performUpdate then
              begin
                {$IFDEF DEBUG}
                logger.Debug('Value [' + destinationfld.AsString +'] of field ' + destinationfld.FieldName + ' of table ' + aTable.DestinationTableName + ' is different from value [' + sourcefld.AsString + '] of original field ' + sourcefld.FieldName );
                {$ENDIF}
                break;
              end;
            end;
          end;
          destinationQuery.Close;

          if performInsert then
          begin
            for q := 0 to aTable.FieldsMapping.Count - 1 do
            begin
              sourcefld := sourceFieldsMap.Find(aTable.FieldsMapping.Get (q).SourceField.AsUppercaseString) as TField;
              if not Assigned(sourcefld) then
                raise Exception.Create('Field ' + aTable.FieldsMapping.Get (q).SourceField.AsString + ' not found as source field in configuration');
              tmpparameter := SQLBuilderInsert.ParamByName(sourcefld.FieldName);
              if not Assigned(tmpparameter) then
                raise Exception.Create('Parameter ' + sourcefld.FieldName + ' not found in insert query');

              paramshell := destinationFieldsMap.Find(aTable.FieldsMapping.Get(q).DestinationField.AsUppercaseString) as TParameterTypeShell;
              if not Assigned(paramshell) then
                raise Exception.Create('Field ' + aTable.FieldsMapping.Get(q).DestinationField.AsString + ' not found in destination table');
              tmpparameter.Assign(sourcefld.AsVariant, paramshell.ParamType);
            end;
            tmpsql:= SQLBuilderInsert.BuildSQL;
            command.SetSQL(tmpsql);
            //writeln(tmpSql);
            command.Execute;
            inc(performedInserts);
          end
          else
          if performUpdate then begin
            for q := 0 to aTable.FieldsMapping.Count - 1 do
            begin
              sourcefld := sourceFieldsMap.Find(aTable.FieldsMapping.Get (q).SourceField.AsUppercaseString) as TField;
              SQLBuilderUpdate.ParamByName(sourcefld.FieldName).Assign(sourcefld.AsVariant, (destinationFieldsMap.Find(aTable.FieldsMapping.Get(q).DestinationField.AsUppercaseString) as TParameterTypeShell).ParamType);
            end;
            tmpsql:= SQLBuilderUpdate.BuildSQL;
            //writeln(tmpSQL);
            command.SetSQL(tmpsql);
            command.Execute;
            inc(performedUpdates);
          end;

          sourceQuery.Next;
          inc(rows);
        end;
      finally
        sourceFieldsMap.Free;
        destinationFieldsMap.Free;
        SQLBuilderDestination.Free;
        SQLBuilderInsert.Free;
        SQLBuilderUpdate.Free;
        destinationQuery.Free;
        sourceQuery.Free;
        command.Free;
      end;

      destinationConnection.Commit;
    except
      on e : Exception do
      begin
        destinationConnection.Rollback;
        raise;
      end;
    end;

    sourceConnection.Close;
    destinationConnection.Close;
  finally
    destinationConnection.Free;
    sourceConnection.Free;
  end;

  {$IFDEF DEBUG}
  logger.Debug('Found ' + IntToStr(rows) + ' rows in source table of destination table ' + aTable.DestinationTableName);
  logger.Debug('Performed ' + IntToStr(performedInserts) + ' insert commands to destination table ' + aTable.DestinationTableName);
  logger.Debug('Performed ' + IntToStr(performedUpdates) + ' update commands to destination table ' + aTable.DestinationTableName);
  {$ENDIF}

  Result := true;
end;

{ TReplicaEngine }

procedure TReplicaEngine.InternalReplicaTable(aProgress: ImProgress; aData : TObject; aJobResult: TJobResult);
begin
  if DoReplicaTable(aData as TDataReplicaTableToTable) then
    aJobResult.ReturnCode:= 1
  else
    aJobResult.ReturnCode:= -1;
end;

procedure TReplicaEngine.DoTerminate(const aJobsResult: TJobResults);
begin
  FCanTerminate:= true;
end;

function TReplicaEngine.ProcessTables(const aTables: TDataReplicaTables; const aMaxConcurrentThreads: integer): boolean;
var
  batchexec : TmBatchExecutor;
  newjob : TmJob;
  i : integer;
begin
  Result := false;
  if aTables.Count = 0 then
    exit;
  batchexec := TmBatchExecutor.Create;
  try
    if aMaxConcurrentThreads > 0 then
      batchexec.MaxConcurrentThreads:= aMaxConcurrentThreads
    else
      batchexec.MaxConcurrentThreads:= GetCPUCores;

    for i := 0 to aTables.Count - 1 do
    begin
      newjob := batchexec.QueueJob;
      newjob.Description:= aTables.Get(i).DestinationTableName;
      newjob.DoJobProcedure := InternalReplicaTable;
      newJob.Data := aTables.Get(i);
    end;

    FCanTerminate:= false;
    batchexec.Execute(DoTerminate);
    while not FCanTerminate do
    begin
      sleep(100);
      {$IFDEF NOGUI}
      CheckSynchronize;
      {$ENDIF}
    end;
  finally
    batchexec.Free;
  end;
end;

{ TParameterTypeShell }

constructor TParameterTypeShell.Create(const aParameterType: TmParameterDataType);
begin
  ParamType:= aParameterType;
end;

initialization
  logger := logManager.AddLog('mDataPump');

end.
