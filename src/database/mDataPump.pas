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
  DB, Classes, CustApp, {$IFDEF GUI}Forms,{$ENDIF}
  mDatabaseConnection, mDatabaseConnectionClasses,
  mProgress, mThreads, mPerformedOperationResults,
  mDataPumpConfiguration;

resourcestring
  SMsgStartProcessingTable = 'Start processing table %s...';
  SMsgProcessedRows = 'Processed %d rows against table %s...';
  SMsgTableDone = 'Table %s done (%d rows processed)';
  SMsgTableDeleteRows = 'Deleted all rows of table %s';

type

  { TReplicaEngine }

  TReplicaEngine = class
  strict private
    FCanTerminate : boolean;
    FApplication : TCustomApplication;

    procedure InternalReplicaTable (aProgress: ImProgress; aData : TObject; aJobResult : TJobResult);
    procedure DoTerminate(const aJobsResult : TJobResults);
  public
    function ProcessTables ({$ifdef gui}aParentForm : TForm;{$endif} aApplication : TCustomApplication; const aTables : TDataReplicaTables; const aResults : IPerformedOperationResults = nil; const aMaxConcurrentThreads : integer = -1) : boolean;
  end;

  function DoReplicaTable (const aTable : TDataReplicaTableToTable; aProgress: ImProgress; aResults : IPerformedOperationResults) : boolean;


implementation

uses
  SysUtils, md5, variants, contnrs,
  mSQLBuilder, mFilterOperators, mMaps, mUtility, mLog, mBaseClassesAsObjects;

const
  KEY_SEPARATOR = '#@#';

type

  { TParameterTypeShell }

  TParameterTypeShell = class
  public
    ParamType : TmParameterDataType;

    constructor Create(const aParameterType : TmParameterDataType);
  end;

  TJobData = class
  public
    results : IPerformedOperationResults;
    table : TDataReplicaTableToTable;
  end;

var
  logger : TmLog;

function ComposeDestinationSelectQuery (aTable: TDataReplicaTableToTable; const aDestinationSB : TmSQLBuilder): String;
var
  i : integer;
  comma, andStr, tmpAlias : String;
  curFieldToField: TDataReplicaFieldToField;
begin
  tmpAlias := LowerCase(GenerateRandomIdString(5, true));
  Result := 'select ';
  comma := '';
  for i := 0 to aTable.FieldsMapping.Count - 1 do
  begin
    Result := Result + comma + tmpAlias + '.' + aDestinationSB.SQLDialectExpert.GetSQLForFieldname(aTable.FieldsMapping.Get(i).DestinationField.AsString, foEq);
    comma := ','
  end;

  Result := Result + ' from ';

  if aTable.DestinationSelectQuery <> '' then
    Result := Result + '(' + aTable.DestinationSelectQuery + ')'
  else
    Result := Result + aDestinationSB.SQLDialectExpert.GetSQLForTablename(aTable.DestinationTableName);

  Result := Result + ' ' + tmpAlias;

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


function ComposeDestinationSelectAllQuery (aTable: TDataReplicaTableToTable; const aDestinationSB : TmSQLBuilder): String;
var
  i : integer;
  comma : String;
begin
  Result := 'select ';
  comma := '';
  for i := 0 to aTable.FieldsMapping.Count - 1 do
  begin
    Result := Result + comma + aDestinationSB.SQLDialectExpert.GetSQLForFieldname(aTable.FieldsMapping.Get(i).DestinationField.AsString, foEq);
    comma := ','
  end;
  Result := Result + ' from ' + aDestinationSB.SQLDialectExpert.GetSQLForTablename(aTable.DestinationTableName);
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
    Result := Result + comma + aDestinationSB.SQLDialectExpert.GetSQLForFieldname(aTable.FieldsMapping.Get(i).DestinationField.AsString, foEq);
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
      Result := Result + comma + aDestinationSB.SQLDialectExpert.GetSQLForFieldname(aTable.FieldsMapping.Get(i).DestinationField.AsString, foEq) + '=' + aDestinationSB.SQLSnippetForValue(aTable.FieldsMapping.Get(i).SourceField.AsString);
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

function ComposeDestinationDeleteQuery (aTable: TDataReplicaTableToTable; const aDestinationSB : TmSQLBuilder): String;
var
  i : integer;
  andStr: String;
  curFieldToField: TDataReplicaFieldToField;
begin
  Result := 'delete from ' + aDestinationSB.SQLDialectExpert.GetSQLForTablename(aTable.DestinationTableName);
  Result := Result + ' where ';
  andStr := '';
  for i := 0 to aTable.SourceKeyFields.Count - 1 do
  begin
    curFieldToField := aTable.FieldsMapping.GetBySourceFieldName(aTable.SourceKeyFields.Strings[i]);
    Result := Result + andStr + aDestinationSB.SQLSnippetForCondition(curFieldToField.DestinationField.AsString, foEq, aTable.SourceKeyFields.Strings[i]);
    andStr := 'and';
  end;
end;

function ComposeDestinationClearAllQuery (aTable: TDataReplicaTableToTable; const aClearAllSB : TmSQLBuilder): String;
begin
  Result := 'delete from ' + aClearAllSB.SQLDialectExpert.GetSQLForTablename(aTable.DestinationTableName);
end;

function DoReplicaTable(const aTable : TDataReplicaTableToTable; aProgress: ImProgress; aResults : IPerformedOperationResults): boolean;
var
  q: integer;
  sourceConnection, destinationConnection: TmDatabaseConnection;
  sourceQuery, destinationQuery: TmDatabaseQuery;
  SQLBuilderDestination, SQLBuilderInsert, SQLBuilderUpdate, SQLBuilderDestinationDelete, SQLBuilderClearAll, SQLBuilderDestinationSelectAll: TmSQLBuilder;
  performInsert, performUpdate: boolean;
  command : TmDatabaseCommand;
  sourcefield, destinationfield : TField;
  destinationFieldsMap, sourceFieldsMap, sourceKeysMap: TmStringDictionary;
  tmpsql, tmp, msg: String;
  tmpparameter : TmQueryParameter;
  paramshell : TParameterTypeShell;
  rows, performedInserts, performedUpdates, performedDeletes : longint;
  curKeyString : String;
  curFieldToField: TDataReplicaFieldToField;
  deleteOperationsToBePerformed : TStringList;
begin
  Result := false;

  aProgress.Notify(Format(SMsgStartProcessingTable, [aTable.DestinationTableName]));

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
      SQLBuilderDestinationDelete := TmSQLBuilder.Create;
      SQLBuilderClearAll := TmSQLBuilder.Create;
      SQLBuilderDestinationSelectAll := TmSQLBuilder.Create;
      destinationFieldsMap := TmStringDictionary.Create(true);
      sourceFieldsMap := TmStringDictionary.Create(false);
      sourceKeysMap := TmStringDictionary.Create(false);
      try
        sourceQuery.DatabaseConnection:= sourceConnection;
        destinationQuery.DatabaseConnection:= destinationConnection;
        command.DatabaseConnection:= destinationConnection;
        SQLBuilderDestination.VendorType:= destinationConnection.ConnectionInfo.VendorType;
        SQLBuilderInsert.VendorType:= SQLBuilderDestination.VendorType;
        SQLBuilderUpdate.VendorType:= SQLBuilderDestination.VendorType;
        SQLBuilderDestinationDelete.VendorType:= SQLBuilderDestination.VendorType;
        SQLBuilderClearAll.VendorType:= SQLBuilderDestination.VendorType;
        SQLBuilderDestinationSelectAll.VendorType:= SQLBuilderDestination.VendorType;

        aTable.FieldsMapping.RebuildIndexes;

        sourceQuery.SetSQL(aTable.SourceSelectQuery);
        SQLBuilderDestination.PrepareSQL(ComposeDestinationSelectQuery(aTable, SQLBuilderDestination));
        SQLBuilderInsert.PrepareSQL(ComposeDestinationInsertQuery(aTable, SQLBuilderInsert));
        SQLBuilderUpdate.PrepareSQL(ComposeDestinationUpdateQuery(aTable, SQLBuilderUpdate));
        SQLBuilderDestinationDelete.PrepareSQL(ComposeDestinationDeleteQuery(aTable, SQLBuilderDestinationDelete));
        SQLBuilderDestinationSelectAll.PrepareSQL(ComposeDestinationSelectAllQuery(aTable, SQLBuilderDestinationSelectAll));

        if aTable.PerformClearBefore then
        begin
          SQLBuilderClearAll.PrepareSQL(ComposeDestinationClearAllQuery(aTable, SQLBuilderClearAll));

          tmpsql:= SQLBuilderClearAll.BuildSQL;
          try
            command.SetSQL(tmpsql);
            command.Execute;
            aProgress.Notify(Format(SMsgTableDeleteRows,[aTable.DestinationTableName]));
            if Assigned(aResults) then
              aResults.AddResult(Format(SMsgTableDeleteRows,[aTable.DestinationTableName]));
          except
            on e : Exception do
            begin
              msg := 'Error in query ' + tmpsql;
              logger.Error(msg);
              logger.Error(e.Message);
              if Assigned(aResults) then
              begin
                aResults.AddError(msg);
                aResults.AddError(e.Message);
              end;
              raise;
            end;
          end;
        end;

        {$IFDEF DEBUG}
        logger.Debug('Source query: ' + aTable.SourceSelectQuery );
        {$ENDIF}

        sourceFieldsMap.Clear;
        try
          sourceQuery.Open;
          logger.Info('Opened source query');
        except
          on e : Exception do
          begin
            msg := 'Error in query ' + aTable.SourceSelectQuery;
            logger.Error(msg);
            logger.Error(e.Message);
            if Assigned(aResults) then
            begin
              aResults.AddError(msg);
              aResults.AddError(e.Message);
            end;
            raise;
          end;
        end;


        rows := 0;
        performedInserts := 0;
        performedUpdates := 0;
        performedDeletes := 0;
        while not sourceQuery.Eof do
        begin
          if sourceFieldsMap.Count = 0 then
          begin
            for q := 0 to sourceQuery.AsDataset.Fields.Count - 1 do
            begin
              sourceFieldsMap.Add(Uppercase(sourceQuery.AsDataset.Fields[q].FieldName), sourceQuery.AsDataset.Fields[q]);
              {$IFDEF DEBUG}
              logger.Debug('Add source field ' + Uppercase(sourceQuery.AsDataset.Fields[q].FieldName) + ' to map...' );
              {$ENDIF}
            end;
          end;

          curKeyString:= '';
          for q := 0 to aTable.SourceKeyFields.Count - 1 do
          begin
            sourcefield := sourceFieldsMap.Find(Uppercase(aTable.SourceKeyFields.Strings[q])) as TField;
            if not Assigned(sourcefield) then
            begin
              msg := 'Field "' + aTable.SourceKeyFields.Strings[q] + '" not found in source query';
              logger.Error(msg);
              if Assigned(aResults) then
                aResults.AddError(msg);
              raise Exception.Create(msg);
            end;
            SQLBuilderDestination.ParamByName(aTable.SourceKeyFields.Strings[q]).Assign(sourcefield);
            if aTable.AllowDelete then
              curKeyString:= curKeyString + KEY_SEPARATOR + sourcefield.AsString;
          end;
          if aTable.AllowDelete then
            sourceKeysMap.Add(curKeyString, sourceKeysMap);

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
              sourcefield := sourceFieldsMap.Find(aTable.FieldsMapping.Get(q).SourceField.AsUppercaseString) as TField;
              if not Assigned(sourcefield) then
                raise Exception.Create('Source field missing! ' + aTable.FieldsMapping.Get(q).SourceField.AsUppercaseString + ' Source fields map count: ' + IntToStr(sourceFieldsMap.Count));

              destinationfield := destinationQuery.AsDataset.FieldByName(aTable.FieldsMapping.Get(q).DestinationField.AsString);
              performUpdate := MD5Print(MD5String(sourcefield.AsString)) <> MD5Print(MD5String(destinationfield.AsString));
              {$IFDEF DEBUG}
              logger.Debug('COMPARING Value [' + destinationfield.AsString +'] of field ' + destinationfield.FieldName + ' of table ' + aTable.DestinationTableName + ' AND [' + sourcefield.AsString + '] of original field ' + sourcefield.FieldName );
              {$ENDIF}
              if performUpdate then
              begin
                {$IFDEF DEBUG}
                logger.Debug('Value [' + destinationfield.AsString +'] of field ' + destinationfield.FieldName + ' of table ' + aTable.DestinationTableName + ' is different from value [' + sourcefield.AsString + '] of original field ' + sourcefield.FieldName );
                {$ENDIF}
                break;
              end;
            end;
          end;
          destinationQuery.Close;

          if performInsert then
          begin
            if aTable.AllowInsert then
            begin
              for q := 0 to aTable.FieldsMapping.Count - 1 do
              begin
                sourcefield := sourceFieldsMap.Find(aTable.FieldsMapping.Get (q).SourceField.AsUppercaseString) as TField;
                if not Assigned(sourcefield) then
                begin
                  msg := 'Field "' + aTable.FieldsMapping.Get (q).SourceField.AsString + '" not found as source field in configuration';
                  logger.Error(msg);
                  if Assigned(aResults) then
                    aResults.AddError(msg);
                  raise Exception.Create(msg);
                end;
                tmpparameter := SQLBuilderInsert.ParamByName(sourcefield.FieldName);
                if not Assigned(tmpparameter) then
                begin
                  msg := 'Parameter "' + sourcefield.FieldName + '" not found in insert query';
                  logger.Error(msg);
                  if Assigned(aResults) then
                    aResults.AddError(msg);
                  raise Exception.Create(msg);
                end;

                paramshell := destinationFieldsMap.Find(aTable.FieldsMapping.Get(q).DestinationField.AsUppercaseString) as TParameterTypeShell;
                if not Assigned(paramshell) then
                begin
                  msg := 'Field "' + aTable.FieldsMapping.Get(q).DestinationField.AsString + '" not found in destination table';
                  logger.Error(msg);
                  if Assigned(aResults) then
                    aResults.AddError(msg);
                  raise Exception.Create(msg);
                end;
                try
                  tmpparameter.Assign(sourcefield.AsVariant, paramshell.ParamType);
                except
                  on e : Exception do
                  begin
                    msg := 'source field ' + sourcefield.FieldName + ' Error:' + e.Message;
                    logger.Error(msg);
                    if Assigned(aResults) then
                      aResults.AddError(msg);
                    raise;
                  end;
                end;
              end;
              tmpsql:= SQLBuilderInsert.BuildSQL;
              try
                command.SetSQL(tmpsql);
                command.Execute;
              except
                on e : Exception do
                begin
                  msg := 'Error in query ' + tmpsql;
                  logger.Error(msg);
                  logger.Error(e.Message);
                  if Assigned(aResults) then
                  begin
                    aResults.AddError(msg);
                    aResults.AddError(e.Message);
                  end;
                  raise;
                end;
              end;
              inc(performedInserts);
            end;
          end
          else
          if performUpdate then begin
            if aTable.AllowUpdate then
            begin
              for q := 0 to aTable.FieldsMapping.Count - 1 do
              begin
                sourcefield := sourceFieldsMap.Find(aTable.FieldsMapping.Get (q).SourceField.AsUppercaseString) as TField;
                SQLBuilderUpdate.ParamByName(sourcefield.FieldName).Assign(sourcefield.AsVariant, (destinationFieldsMap.Find(aTable.FieldsMapping.Get(q).DestinationField.AsUppercaseString) as TParameterTypeShell).ParamType);
              end;
              tmpsql:= SQLBuilderUpdate.BuildSQL;
              try
                command.SetSQL(tmpsql);
                command.Execute;
              except
                on e : Exception do
                begin
                  msg := 'Error in query ' + tmpsql;
                  logger.Error(msg);
                  logger.Error(e.Message);
                  if Assigned(aResults) then
                  begin
                    aResults.AddError(msg);
                    aResults.AddError(e.Message);
                  end;
                  raise;
                end;
              end;
              inc(performedUpdates);
            end;
          end;

          sourceQuery.Next;
          inc(rows);
          if rows mod 10 = 0 then
            aProgress.Notify(Format(SMsgProcessedRows,[rows, aTable.DestinationTableName]));
        end;
        aProgress.Notify(Format(SMsgTableDone,[aTable.DestinationTableName, rows]));

        sourceQuery.Close;
        logger.Info('Source query closed');





        if aTable.AllowDelete then
        begin
          deleteOperationsToBePerformed := TStringList.Create;
          try
            tmpsql := SQLBuilderDestinationSelectAll.BuildSQL;
            destinationQuery.SetSQL(tmpsql);
            destinationQuery.Open;
            // let's read every row of the destination table
            while not destinationQuery.Eof do
            begin
              curKeyString:= '';
              for q := 0 to aTable.SourceKeyFields.Count - 1 do
              begin
                // composing the key strings that will be searched on the sourcekeysmap
                curFieldToField := aTable.FieldsMapping.GetBySourceFieldName(aTable.SourceKeyFields.Strings[q]);
                if not Assigned(curFieldToField) then
                  raise Exception.Create('Key field ' + aTable.SourceKeyFields.Strings[q] + ' not found in source query');
                curKeyString := curKeyString + KEY_SEPARATOR + destinationQuery.AsDataset.FieldByName(curFieldToField.DestinationField.AsUppercaseString).AsString;
              end;

              if not Assigned(sourceKeysMap.Find(curKeyString)) then // current row of destination table is not found in source table
              begin
                for q := 0 to aTable.SourceKeyFields.Count - 1 do
                begin
                  curFieldToField := aTable.FieldsMapping.GetBySourceFieldName(aTable.SourceKeyFields.Strings[q]);
                  SQLBuilderDestinationDelete.ParamByName(curFieldToField.SourceField.AsString).Assign(destinationQuery.AsDataset.FieldByName(curFieldToField.DestinationField.AsString).AsVariant, (destinationFieldsMap.Find(curFieldToField.DestinationField.AsUppercaseString) as TParameterTypeShell).ParamType);
                end;
                deleteOperationsToBePerformed.Add(SQLBuilderDestinationDelete.BuildSQL);
              end;

              destinationQuery.Next;
            end;
            destinationQuery.Close;

            for q := 0 to deleteOperationsToBePerformed.Count -1 do
            begin
              try
                command.SetSQL(deleteOperationsToBePerformed.Strings[q]);
                command.Execute;
              except
                on e : Exception do
                begin
                  msg := 'Error in query ' + tmpsql;
                  logger.Error(msg);
                  logger.Error(e.Message);
                  if Assigned(aResults) then
                  begin
                    aResults.AddError(msg);
                    aResults.AddError(e.Message);
                  end;
                  raise;
                end;
              end;
              inc(performedDeletes);
            end;

          finally
            deleteOperationsToBePerformed.Free;
          end;
        end;
      finally
        sourceFieldsMap.Free;
        destinationFieldsMap.Free;
        sourceKeysMap.Free;
        SQLBuilderDestination.Free;
        SQLBuilderInsert.Free;
        SQLBuilderUpdate.Free;
        SQLBuilderDestinationDelete.Free;
        SQLBuilderClearAll.Free;
        SQLBuilderDestinationSelectAll.Free;
        destinationQuery.Free;
        sourceQuery.Free;
        command.Free;
      end;

      destinationConnection.Commit;
    except
      on e : Exception do
      begin
        destinationConnection.Rollback;
        logger.Error(e.Message);
        logger.Error('Last SQL script:' + GetLastSQLScript(tmp));
        writeln(GetLastSQLScript(tmp));
        raise;
      end;
    end;

    sourceConnection.Close;
    destinationConnection.Close;
  finally
    FreeAndNil(destinationConnection);
    FreeAndNil(sourceConnection);
  end;

  logger.Info('Found ' + IntToStr(rows) + ' rows in source table of destination table ' + aTable.DestinationTableName);
  logger.Info('Performed ' + IntToStr(performedInserts) + ' insert commands to destination table ' + aTable.DestinationTableName);
  logger.Info('Performed ' + IntToStr(performedUpdates) + ' update commands to destination table ' + aTable.DestinationTableName);
  logger.Info('Performed ' + IntToStr(performedDeletes) + ' delete commands to destination table ' + aTable.DestinationTableName);

  if Assigned(aResults) then
  begin
    aResults.AddResult('Found ' + IntToStr(rows) + ' rows in source table of destination table ' + aTable.DestinationTableName);
    aResults.AddResult('Performed ' + IntToStr(performedInserts) + ' insert commands to destination table ' + aTable.DestinationTableName);
    aResults.AddResult('Performed ' + IntToStr(performedUpdates) + ' update commands to destination table ' + aTable.DestinationTableName);
    aResults.AddResult('Performed ' + IntToStr(performedDeletes) + ' delete commands to destination table ' + aTable.DestinationTableName);
  end;

  Result := true;
end;

{ TReplicaEngine }

procedure TReplicaEngine.InternalReplicaTable(aProgress: ImProgress; aData : TObject; aJobResult: TJobResult);
begin
  if DoReplicaTable((aData as TJobData).table, aProgress, (aData as TJobData).results) then
    aJobResult.ReturnCode:= 1
  else
    aJobResult.ReturnCode:= -1;
end;

procedure TReplicaEngine.DoTerminate(const aJobsResult: TJobResults);
begin
  FCanTerminate:= true;
end;

function TReplicaEngine.ProcessTables({$ifdef gui}aParentForm : TForm;{$endif} aApplication : TCustomApplication; const aTables: TDataReplicaTables; const aResults : IPerformedOperationResults; const aMaxConcurrentThreads: integer): boolean;
var
  batchexec : TmBatchExecutor;
  newjob : TmJob;
  i : integer;
  tmpJobData : TJobData;
  jobDataList : TObjectList;
begin
  Result := false;
  if aTables.Count = 0 then
    exit;

  jobDataList:= TObjectList.Create(true);
  batchexec := TmBatchExecutor.Create;
  try
    if aMaxConcurrentThreads > 0 then
      batchexec.MaxConcurrentThreads:= aMaxConcurrentThreads
    else
      batchexec.MaxConcurrentThreads:= GetCPUCores;

    if Assigned(aResults) and (batchexec.MaxConcurrentThreads > 1) and (aTables.Count > 1) then
      aResults.SetThreadSafe;

    for i := 0 to aTables.Count - 1 do
    begin
      newjob := batchexec.QueueJob;
      newjob.Description:= aTables.Get(i).DestinationTableName;
      newjob.Application:= aApplication;
      newjob.DoJobProcedure := InternalReplicaTable;
      tmpJobData := TJobData.Create;
      jobDataList.Add(tmpJobData);
      tmpJobData.table := aTables.Get(i);
      tmpJobData.results := aResults;
      newJob.Data := tmpJobData;
    end;

    FCanTerminate:= false;
    batchexec.Execute({$ifdef gui}aParentForm,{$endif}DoTerminate);
    while not FCanTerminate do
    begin
      sleep(100);
      {$IFDEF NOGUI}
      CheckSynchronize;
      {$ENDIF}
    end;
  finally
    batchexec.Free;
    jobDataList.Free;
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
