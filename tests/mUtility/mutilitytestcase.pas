unit mUtilityTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, dateutils,
  mUtility, mISO6346Utility;

type

  { TTestCaseUtility }

  TTestCaseUtility= class(TTestCase)
  published
    procedure TestSillyCryptDecrypt;
    procedure TestAddRemoveZeros;
    procedure TestGetCPUCores;
    procedure TestISO6346Containers;
    procedure TestISO6346MRNs;
    procedure TestHumanReadableUniqueIdentifiers;
    procedure TestEncodeDecodeTimestampForFilename;
    procedure TestEscapeString;
    procedure TestDateTimeConversions;
  end;

implementation

procedure TTestCaseUtility.TestSillyCryptDecrypt;
var
  pwd : String;
begin
  pwd := 'zucc$';
  CheckEquals(SillyCryptDecrypt(SillyCryptDecrypt('pimpiripettanuse', pwd), pwd), 'pimpiripettanuse');
  pwd := 'zucc_123_4edhjshdfhòsàà???!K!LKLD';
  CheckEquals(SillyCryptDecrypt(SillyCryptDecrypt('pimpiripettanuse', pwd), pwd), 'pimpiripettanuse');
  CheckEquals(SillyCryptDecrypt(SillyCryptDecrypt('!"_@@Dsklsdjk22a*^?L', pwd), pwd), '!"_@@Dsklsdjk22a*^?L');
  pwd := '树树树树';
  CheckEquals(SillyCryptDecrypt(SillyCryptDecrypt('pimpiripettanuse', pwd), pwd), 'pimpiripettanuse');
  CheckEquals(SillyCryptDecrypt(SillyCryptDecrypt('花的房子', pwd), pwd), '花的房子');
  pwd := 'zucc$';
  CheckEquals(SillyCryptDecrypt(SillyCryptDecrypt('花的房子', pwd), pwd), '花的房子');
end;

procedure TTestCaseUtility.TestAddRemoveZeros;
begin
  CheckEquals('12002', RemoveZerosFromFront('0012002'));
  CheckEquals('', RemoveZerosFromFront('0'));
  CheckEquals('', RemoveZerosFromFront('000000000'));
  CheckEquals('5', RemoveZerosFromFront('5'));
  CheckEquals('50', RemoveZerosFromFront('50'));
  CheckEquals('50', RemoveZerosFromFront('050'));

  CheckEquals('012', AddZerosFront(12, 3));
  CheckEquals('12', AddZerosFront(12, 2));
  CheckEquals('12', AddZerosFront(12, 1));
  CheckEquals('0012', AddZerosFront(12, 4));
end;

procedure TTestCaseUtility.TestGetCPUCores;
begin
  CheckTrue(GetCPUCores > 0);
end;

procedure TTestCaseUtility.TestISO6346Containers;
var
  error : string;
begin
  CheckTrue(IsContainerCodeValid('APRU5795945', error));
  CheckTrue(IsContainerCodeValid('SUDU3994180', error));
  CheckTrue(IsContainerCodeValid('SUDU6914949', error));
  CheckTrue(IsContainerCodeValid('CAXU4398690', error));
  CheckTrue(IsContainerCodeValid('INBU5384192', error));
  CheckTrue(IsContainerCodeValid('TTNU5352337', error));
  CheckTrue(IsContainerCodeValid('KHLU1202730', error));
  CheckTrue(IsContainerCodeValid('ROLU2053102', error));
  CheckTrue(IsContainerCodeValid('MLCU9318022', error));
  CheckTrue(IsContainerCodeValid('GESU2668436', error));
  CheckTrue(IsContainerCodeValid('SUDU6837992', error));
  CheckTrue(IsContainerCodeValid('SUDU5872176', error));
  CheckTrue(IsContainerCodeValid('ZCSU8701054', error));
  CheckTrue(IsContainerCodeValid('SUDU2838347', error));
  CheckTrue(IsContainerCodeValid('SUDU4877086', error));
  CheckTrue(IsContainerCodeValid('SUDU5407575', error));
  CheckTrue(IsContainerCodeValid('IPXU3793633', error));
  CheckTrue(IsContainerCodeValid('IPXU3046324', error));
  CheckTrue(IsContainerCodeValid('TCKU9962198', error));
  CheckTrue(IsContainerCodeValid('GESU5922708', error));
  CheckTrue(IsContainerCodeValid('AMZU4202873', error));
  CheckTrue(IsContainerCodeValid('AMFU4260740', error));
  CheckTrue(IsContainerCodeValid('AMFU4261449', error));
  CheckTrue(IsContainerCodeValid('AMFU4260777', error));
  CheckTrue(IsContainerCodeValid('AMFU4261942', error));
  CheckTrue(IsContainerCodeValid('SUDU4864200', error));
  CheckTrue(IsContainerCodeValid('SUDU9802829', error));
  CheckTrue(IsContainerCodeValid('GFRU9001607', error));
  CheckTrue(IsContainerCodeValid('SZLU9800964', error));
  CheckTrue(IsContainerCodeValid('TRLU1729099', error));
  CheckTrue(IsContainerCodeValid('SUDU4872510', error));
  CheckTrue(IsContainerCodeValid('SUDU9805598', error));
  CheckTrue(IsContainerCodeValid('SUDU6531359', error));
  CheckTrue(IsContainerCodeValid('FSCU9034206', error));
  CheckTrue(IsContainerCodeValid('TTNU3579790', error));
  CheckTrue(IsContainerCodeValid('SUDU3763366', error));
  CheckTrue(IsContainerCodeValid('CADU7017518', error));
  CheckTrue(IsContainerCodeValid('FSCU9026427', error));
  CheckTrue(IsContainerCodeValid('TRLU5425601', error));
  CheckTrue(IsContainerCodeValid('SUDU1410884', error));
  CheckTrue(IsContainerCodeValid('DFSU2372572', error));
  CheckTrue(IsContainerCodeValid('SUDU4810441', error));
  CheckTrue(IsContainerCodeValid('SUDU1836376', error));
  CheckTrue(IsContainerCodeValid('SUDU3451202', error));
  CheckTrue(IsContainerCodeValid('CADU2029455', error));
  CheckTrue(IsContainerCodeValid('SUDU3971971', error));
  CheckTrue(IsContainerCodeValid('GRIU1236388', error));
  CheckTrue(IsContainerCodeValid('AMFU5023002', error));
  CheckTrue(IsContainerCodeValid('SUDU5765901', error));
  CheckTrue(IsContainerCodeValid('CMCU4330238', error));
  CheckTrue(IsContainerCodeValid('CRXU9544873', error));
  CheckTrue(IsContainerCodeValid('SUDU5684630', error));
  CheckTrue(IsContainerCodeValid('SUDU4591045', error));
  CheckTrue(IsContainerCodeValid('SUDU8536160', error));
  CheckTrue(IsContainerCodeValid('SUDU5663896', error));
  CheckTrue(IsContainerCodeValid('CAXU9376693', error));
  CheckTrue(IsContainerCodeValid('SUDU5756941', error));
  CheckTrue(IsContainerCodeValid('CRTU7004857', error));
  CheckTrue(IsContainerCodeValid('TRIU0257748', error));
  CheckTrue(IsContainerCodeValid('SUDU2836220', error));
  CheckTrue(IsContainerCodeValid('SUDU1367155', error));
  CheckTrue(IsContainerCodeValid('SUDU1645127', error));
  CheckTrue(IsContainerCodeValid('CLHU3061779', error));
  CheckTrue(IsContainerCodeValid('SUDU6019018', error));
  CheckTrue(IsContainerCodeValid('CLHU8578110', error));
  CheckTrue(IsContainerCodeValid('ZCSU8731125', error));
  CheckTrue(IsContainerCodeValid('BMOU4041191', error));
  CheckTrue(IsContainerCodeValid('CLHU8901645', error));
  CheckTrue(IsContainerCodeValid('SUDU4860144', error));
  CheckTrue(IsContainerCodeValid('SUDU9802916', error));
  CheckTrue(IsContainerCodeValid('CAIU8469953', error));
  CheckTrue(IsContainerCodeValid('BSIU4055673', error));
  CheckTrue(IsContainerCodeValid('TRLU6558255', error));
  CheckTrue(IsContainerCodeValid('UESU4775226', error));
  CheckTrue(IsContainerCodeValid('GESU5500791', error));
  CheckTrue(IsContainerCodeValid('ECMU9117280', error));
  CheckTrue(IsContainerCodeValid('SUDU1461407', error));
  CheckTrue(IsContainerCodeValid('SUDU3677308', error));
  CheckTrue(IsContainerCodeValid('SUDU4660547', error));
  CheckTrue(IsContainerCodeValid('GLDU4047286', error));
  CheckTrue(IsContainerCodeValid('GRIU4218011', error));
  CheckTrue(IsContainerCodeValid('SUDU5441815', error));
  CheckTrue(IsContainerCodeValid('KHLU9442001', error));
  CheckTrue(IsContainerCodeValid('GLDU4026724', error));
  CheckTrue(IsContainerCodeValid('SUDU4649363', error));
  CheckTrue(IsContainerCodeValid('SUDU5451984', error));
  CheckTrue(IsContainerCodeValid('TRLU1234567', error));
  CheckFalse(IsContainerCodeValid('SUDU1234567', error));
  CheckTrue(IsContainerCodeValid('SUDU2834613', error));
  CheckTrue(IsContainerCodeValid('SUDU2842096', error));
  CheckTrue(IsContainerCodeValid('CADU7044175', error));
  CheckTrue(IsContainerCodeValid('SUDU2839127', error));
  CheckTrue(IsContainerCodeValid('SUDU2835230', error));
  CheckTrue(IsContainerCodeValid('TRIU0251610', error));
  CheckTrue(IsContainerCodeValid('TRIU0237510', error));
  CheckTrue(IsContainerCodeValid('TRIU0251185', error));
  CheckTrue(IsContainerCodeValid('SUDU4678206', error));
  CheckTrue(IsContainerCodeValid('CADU4006021', error));
  CheckTrue(IsContainerCodeValid('SUDU5472307', error));
  CheckTrue(IsContainerCodeValid('SUDU7346550', error));
  CheckTrue(IsContainerCodeValid('SUDU5412545', error));
  CheckTrue(IsContainerCodeValid('SUDU2821344', error));
  CheckTrue(IsContainerCodeValid('SUDU2842305', error));
  CheckTrue(IsContainerCodeValid('BHCU2227086', error));
  CheckTrue(IsContainerCodeValid('CRXU7203353', error));
  CheckTrue(IsContainerCodeValid('CAIU8465916', error));
  CheckTrue(IsContainerCodeValid('GESU9257695', error));
  CheckTrue(IsContainerCodeValid('GESU9488610', error));
  CheckTrue(IsContainerCodeValid('AMFU3118650', error));
  CheckTrue(IsContainerCodeValid('TGHU3239824', error));
  CheckTrue(IsContainerCodeValid('TCKU3616270', error));
  CheckTrue(IsContainerCodeValid('SUDU6861155', error));
  CheckTrue(IsContainerCodeValid('CAXU9649210', error));
  CheckTrue(IsContainerCodeValid('SUDU1317940', error));
  CheckTrue(IsContainerCodeValid('SUDU1690635', error));
  CheckTrue(IsContainerCodeValid('SUDU5490281', error));
  CheckTrue(IsContainerCodeValid('IPXU3997299', error));
  CheckTrue(IsContainerCodeValid('CADU2015298', error));
  CheckTrue(IsContainerCodeValid('SUDU1633276', error));
  CheckTrue(IsContainerCodeValid('ZCSU8880026', error));

  CheckTrue(IsContainerCodeValid('CSQU3054383', error));
  CheckFalse(IsContainerCodeValid('CSQU3054381', error));
  CheckFalse(IsContainerCodeValid('CSQU3054382', error));
  CheckFalse(IsContainerCodeValid('CSQU3054384', error));
  CheckFalse(IsContainerCodeValid('CSQU3054385', error));
  CheckFalse(IsContainerCodeValid('CSQU3054386', error));
  CheckFalse(IsContainerCodeValid('CSQU3054387', error));
  CheckFalse(IsContainerCodeValid('CSQU3054388', error));
  CheckFalse(IsContainerCodeValid('CSQU3054389', error));
  CheckFalse(IsContainerCodeValid('', error));
  CheckFalse(IsContainerCodeValid('12345678901', error));
  CheckFalse(IsContainerCodeValid('CSQK3054383', error));
  CheckFalse(IsContainerCodeValid('CSU3054383', error));
  CheckFalse(IsContainerCodeValid('CS2U3054383', error));
  CheckFalse(IsContainerCodeValid('CSQU30A4383', error));
  CheckFalse(IsContainerCodeValid('CSQU 30A438', error));
  CheckFalse(IsContainerCodeValid('CSQU30A438 ', error));
  CheckFalse(IsContainerCodeValid('CSQU30A4.38', error));
end;

procedure TTestCaseUtility.TestISO6346MRNs;
var
  error : string;
begin
  CheckTrue(IsMRNCodeValid('99IT9876AB88901209', error));
  CheckTrue(IsMRNCodeValid('18ITQUU1T0001760E6', error));
  CheckTrue(IsMRNCodeValid('18ITQFC1T0002230E9', error));
  CheckTrue(IsMRNCodeValid('18ITQUU1T0001762E4', error));
  CheckTrue(IsMRNCodeValid('18ITQUU1T0001738E5', error));
  CheckTrue(IsMRNCodeValid('18ITQUU010000009E8', error));
  CheckTrue(IsMRNCodeValid('18ITQVI3T0000261T0', error));
  CheckTrue(IsMRNCodeValid('15ITQTC1T0001943E9', error));
  CheckTrue(IsMRNCodeValid('15ITQTC1T0001944E8', error));
  CheckTrue(IsMRNCodeValid('15ITQTC1T0001946E6', error));
  CheckTrue(IsMRNCodeValid('15ITQVG1T0000557E7', error));
  CheckTrue(IsMRNCodeValid('15ITQTC1T0001941E0', error));
  CheckTrue(IsMRNCodeValid('14DE655109374181E3', error));
  CheckTrue(IsMRNCodeValid('14SK5864EX41590205', error));
  CheckTrue(IsMRNCodeValid('14ITQXL080074154T5', error));
  CheckTrue(IsMRNCodeValid('15ITQTR1T0000519E8', error));
  CheckTrue(IsMRNCodeValid('16ITQ1V1T0389206E4', error));
  CheckTrue(IsMRNCodeValid('16ITQ1W1T0093613E6', error));
  CheckTrue(IsMRNCodeValid('16ITQ1V8T0019167T3', error));
  CheckTrue(IsMRNCodeValid('16ITQW4030000001T6', error));
  CheckTrue(IsMRNCodeValid('16ITQ1W1T0097479E8', error));
  CheckTrue(IsMRNCodeValid('16ITQ1W1T0097491E4', error));
  CheckTrue(IsMRNCodeValid('16ITQ1W1T0096937E4', error));
  CheckTrue(IsMRNCodeValid('16ITQV01T0041190E6', error));
  CheckTrue(IsMRNCodeValid('16ITQTC1T0036103E8', error));
  CheckTrue(IsMRNCodeValid('16ITQFC1T0024493E5', error));
  CheckTrue(IsMRNCodeValid('15ITQTC1T0001942E0', error));
  CheckTrue(IsMRNCodeValid('16ITQFC1T0024237E0', error));
  CheckFalse(IsMRNCodeValid('16ITQFC1T0024237E7', error));
  CheckFalse(IsMRNCodeValid('1AITQFC1T0024237E7', error));
  CheckFalse(IsMRNCodeValid('162TQFC1T0024237E0', error));
  CheckFalse(IsMRNCodeValid('16ITQ_C1T0024237E0', error));
  CheckFalse(IsMRNCodeValid('16ITQFC1T002423E0', error));
end;

procedure TTestCaseUtility.TestHumanReadableUniqueIdentifiers;
var
  list : TStringList;
  tmp : String;
  i : integer;
begin
  list := TStringList.Create;
  try
    for i := 0 to 1000 do
    begin
      tmp := CreateHumanReadableUniqueIdentier('EN');
      // WriteLn(tmp);
      CheckTrue(list.IndexOf(tmp) < 0, 'Duplicated EN id:' + tmp + ' #'+ IntToStr(i));
      list.Add(tmp);
    end;
  finally
    list.Free;
  end;

  list := TStringList.Create;
  try
    for i := 1 to 1000 do
    begin
      tmp := CreateHumanReadableUniqueIdentier('IT');
      // WriteLn(tmp);
      CheckTrue(list.IndexOf(tmp) < 0, 'Duplicated IT id:' + tmp + ' #'+ IntToStr(i));
      list.Add(tmp);
    end;
  finally
    list.Free;
  end;


end;

procedure TTestCaseUtility.TestEncodeDecodeTimestampForFilename;
begin
  CheckEquals(GetTimeStampForFileName(EncodeDate(2019,8,7), false), '20190807');
  CheckEquals(DecodeTimeStampForFileName('20190807'), EncodeDate(2019,8,7));
  CheckEquals(GetTimeStampForFileName(EncodeDate(2019,12,7), false), '20191207');
  CheckEquals(DecodeTimeStampForFileName('20191207'), EncodeDate(2019,12,7));
  CheckEquals(GetTimeStampForFileName(EncodeDateTime(2019,12,7, 11, 33, 0, 0)), '20191207-113300');
  CheckEquals(DecodeTimeStampForFileName('20191207-113300'), EncodeDateTime(2019,12,7, 11, 33, 0, 0));
  CheckEquals(GetTimeStampForFileName(EncodeDateTime(2019,12,7, 0, 0, 0, 0)), '20191207-000000');
  CheckEquals(DecodeTimeStampForFileName('20191207-000000'), EncodeDateTime(2019,12,7, 0, 0, 0, 0));

end;

procedure TTestCaseUtility.TestEscapeString;
begin
  CheckEquals('\/\\', RevertEscapedStringValue(EscapeStringValue('\/\\', 'json'), 'json'));
  CheckEquals('''green'' is the new "black_"', RevertEscapedStringValue(EscapeStringValue('''green'' is the new "black_"', 'sql'), 'sql'));
  CheckEquals('top' + Chr(9), RevertEscapedStringValue(EscapeStringValue('top' + Chr(9), 'json'), 'json'));
  CheckEquals('\\\\', RevertEscapedStringValue(EscapeStringValue('\\\\', 'json'), 'json'));
  CheckEquals('____O O_____', RevertEscapedStringValue(EscapeStringValue('____O O_____', 'json'), 'json'));
  CheckEquals('\\myserver\dir\dir2\pippo.txt', RevertEscapedStringValue(EscapeStringValue('\\myserver\dir\dir2\pippo.txt', 'json'), 'json'));
end;

procedure TTestCaseUtility.TestDateTimeConversions;
var
  outValue, refValue : TDateTime;
begin
  refValue := EncodeDate(2022, 4, 30);
  CheckTrue(TryToUnderstandDateString('30/04/2022', outValue));
  CheckEquals(refValue, outValue);
  CheckTrue(TryToUnderstandDateString('30/04/22', outValue));
  CheckEquals(refValue, outValue);
  CheckTrue(TryToUnderstandDateString('30-04-22', outValue));
  CheckEquals(refValue, outValue);
  CheckTrue(TryToUnderstandDateString('300422', outValue));
  CheckEquals(refValue, outValue);
  CheckTrue(TryToUnderstandDateString('30042022', outValue));
  CheckEquals(refValue, outValue);
  CheckTrue(TryToUnderstandDateString('30\04\2022', outValue));
  CheckEquals(refValue, outValue);
  CheckTrue(TryToUnderstandDateString('30 04 2022', outValue));
  CheckEquals(refValue, outValue);
  CheckTrue(TryToUnderstandDateString('30 Apr 2022', outValue));
  CheckEquals(refValue, outValue);
  CheckTrue(TryToUnderstandDateString('30 April 2022', outValue));
  CheckEquals(refValue, outValue);

  refValue := EncodeTime(11, 23, 0, 0);
  CheckTrue(TryToUnderstandTimeString('11:23:00', outValue));
  CheckEquals(refValue, outValue);
  CheckTrue(TryToUnderstandTimeString('112300', outValue));
  CheckEquals(refValue, outValue);
  CheckTrue(TryToUnderstandTimeString('112301', outValue));
  CheckNotEquals(refValue, outValue);
  CheckTrue(TryToUnderstandTimeString('11.23.00', outValue));
  CheckEquals(refValue, outValue);
  CheckTrue(TryToUnderstandTimeString('11:23', outValue));
  CheckEquals(refValue, outValue);

  refValue := EncodeDateTime(2022, 4, 30, 11, 23, 0, 0);
  CheckTrue(TryToUnderstandDateTimeString('30/04/2022 11:23:00', outValue));
  CheckEquals(refValue, outValue);
  CheckTrue(TryToUnderstandDateTimeString('30 Apr 2022 11:23:00', outValue));
  CheckEquals(refValue, outValue);
end;



initialization

  RegisterTest(TTestCaseUtility);
end.

