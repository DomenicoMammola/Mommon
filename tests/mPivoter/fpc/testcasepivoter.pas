unit testcasepivoter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  mNullables, mPivoter,
  TestDataset;

type

  TTestPivoter= class(TTestCase)
  strict private
    FHumans : THumans;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestHookUp;
  end;

implementation

uses
  DB,
  mIntList, LazLogger;

procedure GetWillAndGrace (aHumans : THumans);
begin
  aHumans.Clear;
  with aHumans.Add do
  begin
    Key.Id.Value := '1';
    Name.Value:= 'Jack';
    Eyes.Value:= 'Brown';
    Gender.Value:= 'M';
    BirthDay.Value:= EncodeDate(1968, 5, 25);
    Height.Value := 180.5;
  end;
  with aHumans.Add do
  begin
    Key.Id.Value := '2';
    Name.Value:= 'Grace';
    Eyes.Value:= 'Brown';
    Gender.Value:= 'F';
    BirthDay.Value:= EncodeDate(1965, 11, 3);
    Height.Value := 174;
  end;
  with aHumans.Add do
  begin
    Key.Id.Value := '3';
    Name.Value:= 'Will';
    Eyes.Value:= 'Brown';
    Gender.Value:= 'M';
    BirthDay.Value:= EncodeDate(1964, 10, 1);
    Height.Value := 177;
  end;
  with aHumans.Add do
  begin
    Key.Id.Value := '4';
    Name.Value:= 'Karen';
    Eyes.Value:= 'Black';
    Gender.Value:= 'F';
    BirthDay.Value:= EncodeDate(1962, 9, 2);
    Height.Value := 169;
  end;
end;


procedure TTestPivoter.TestHookUp;
var
  pivoter : TmPivoter;
  keys : TStringList;
  i, p, w : integer;
  tmpEyes, tmpGender : String;
  tmpList : TCardinalList;
begin
  GetWillAndGrace(FHumans);
  pivoter := TmPivoter.Create;
  try
    pivoter.DataProvider := FHumans;

    with pivoter.VerticalGroupByDefs.Add do
    begin
      FieldName:= THuman.FLD_EYES;
      DataType:= ftString;
      OperationKind :=gpoDistinct;
    end;

    with pivoter.VerticalGroupByDefs.Add do
    begin
      FieldName:= THuman.FLD_GENDER;
      DataType:= ftString;
      OperationKind :=gpoDistinct;
    end;

    pivoter.CalculateHierarchy;
    keys := TStringList.Create;
    try
      for p := 0 to pivoter.VerticalValues[0].Count - 1 do
      begin
        tmpEyes := pivoter.VerticalValues[0].GetValue(p);

        for w := 0 to pivoter.VerticalValues[1].Count- 1 do
        begin
          tmpGender := pivoter.VerticalValues[1].GetValue(w);
          keys.Clear;
          keys.Add(tmpEyes);
          keys.Add(tmpGender);

          tmpList := pivoter.GetRecords(keys, nil);
          if tmpEyes = 'Brown' then
            CheckTrue(Assigned(tmpList))
          else
            if (tmpEyes = 'Black') and (tmpGender = 'F') then
              CheckTrue(Assigned(tmpList))
            else if (tmpEyes = 'Black') and (tmpGender = 'M') then
              CheckFalse(Assigned(tmpList));

          if Assigned(tmpList) then
          begin
            if (tmpEyes = 'Brown') and (tmpGender = 'F') then
              CheckEquals(tmpList.Count, 1)
            else if (tmpEyes = 'Brown') and (tmpGender = 'M') then
              CheckEquals(tmpList.Count, 2)
            else if (tmpEyes = 'Black') then
              CheckEquals(tmpList.Count, 1);

            DebugLn(tmpEyes + ' - ' + tmpGender);
            for i := 0 to tmpList.Count - 1 do
            begin
              DebugLn('* ' + FHumans.Get(tmpList.Items[i]).Name.AsString);
            end;
          end;
        end;

      end;
    finally
      keys.Free;
    end;

    for i := 0 to pivoter.VerticalValues.Get(0).Count - 1 do
    begin
      DebugLn('V.V.' + IntToStr(i) + '=' + pivoter.VerticalValues.Get(0).GetValue(i));
    end;

    for i := 0 to pivoter.VerticalValues.Get(1).Count - 1 do
    begin
      DebugLn('V.V.' + IntToStr(i) + '=' + pivoter.VerticalValues.Get(1).GetValue(i));
    end;

  finally
    pivoter.Free;
  end;

end;

procedure TTestPivoter.SetUp;
begin
  FHumans := THumans.Create(true);
end;

procedure TTestPivoter.TearDown;
begin
  FHumans.Free;
end;



initialization

  RegisterTest(TTestPivoter);
end.

