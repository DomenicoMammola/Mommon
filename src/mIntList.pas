// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mIntList;

interface

uses Classes
{$IFDEF FPC}
  , fgl
{$ELSE}
  ,System.Types, System.Generics.Collections
{$ENDIF};

type

  {$IFDEF FPC}
  mUnsignedInt = PtrUInt;
  {$ELSE}
  mUnsignedInt = cardinal;
  {$ENDIF}

  TCustomCardinalList = class(TList)
  protected
    function Get(Index: Integer): mUnsignedInt; virtual;
    procedure Put(Index: Integer; const Value: mUnsignedInt);
    procedure Inst(Index : Integer; Num: mUnsignedInt);
  public
    function GetText : string;
    function Add(Num: mUnsignedInt): Integer; virtual;
    procedure AddIntegers (IntegerList : TCustomCardinalList);
    function First: mUnsignedInt;
    function IndexOf(Num: mUnsignedInt): Integer; virtual;
    function Last: mUnsignedInt;
    function Remove(Num: mUnsignedInt): Integer;
    procedure Pack(RemoveNum: mUnsignedInt);
    procedure Sort; virtual;
    procedure SaveToFile (const FileName : string);
    procedure SaveToStream (Stream: TStream);

    property Items[Index: Integer]: mUnsignedInt read Get write Put; default;
    property Nums[Index: Integer]: mUnsignedInt read Get write Put;
  end;

  TCardinalList = class (TCustomCardinalList)
  public
    procedure Insert(Index: integer; Num: mUnsignedInt);
  end;

  TSortedCardinalList = class (TCustomCardinalList)
  private
    FSorted : boolean;
    function InternalGet(Index: Integer): mUnsignedInt;
  protected
    function Get(Index: Integer): mUnsignedInt; override;
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    function IndexOf(Num: mUnsignedInt): Integer; override;
    procedure Sort; override;
    procedure AddSorted(Num: mUnsignedInt);
  end;

  {$IFDEF FPC}
  TIntegerList = specialize TFPGList<Integer>;
  {$ELSE}
  TIntegerList = TList<Integer>;
  {$ENDIF}


implementation

uses
  SysUtils;

function SortBySmall(Item1, Item2: Pointer): Integer;
begin
  if mUnsignedInt( Item1 )>mUnsignedInt( Item2 ) then Result := 1
    else if mUnsignedInt( Item1 )<mUnsignedInt( Item2 ) then Result := -1
      else Result := 0;
end;

function TCustomCardinalList.Get(Index: Integer): mUnsignedInt;
begin
  Result := mUnsignedInt( inherited Get( Index ) );
end;

function TCustomCardinalList.GetText: string;
var
  i : integer;
begin
  Result := '';
  for i := 0 to Self.Count - 1 do
  begin
    if i = 0 then
      Result := IntToStr(Self.Nums[i])
    else
      Result := Result + '#' + sLineBreak + IntToStr(Self.Nums[i]);
  end;
end;

procedure TCustomCardinalList.Put(Index: Integer; const Value: mUnsignedInt);
begin
  inherited Put( Index, Pointer( Value ) );
end;

function TCustomCardinalList.Add(Num: mUnsignedInt): Integer;
begin
  Result := inherited Add( pointer(Num) );
end;

function TCustomCardinalList.First: mUnsignedInt;
begin
  Result := Get( 0 );
end;

function TCustomCardinalList.IndexOf(Num: mUnsignedInt): Integer;
begin
  Result := inherited IndexOf( Pointer( Num ) );
end;

procedure TCustomCardinalList.Inst(Index: Integer; Num : mUnsignedInt);
begin
  inherited Insert( Index, Pointer( Num ) );
end;

function TCustomCardinalList.Last: mUnsignedInt;
begin
  Result := Get( Count-1 );
end;

function TCustomCardinalList.Remove(Num: mUnsignedInt): Integer;
begin
   Result := inherited Remove( Pointer( Num ) );
end;

procedure TCustomCardinalList.Pack(RemoveNum: mUnsignedInt);
var
  idx : Integer;
begin
  idx := 0;
  while idx<Count do begin
    if Get( idx )=RemoveNum then Delete( idx )
      else inc( idx );
  end;
end;

procedure TCustomCardinalList.SaveToFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TCustomCardinalList.SaveToStream(Stream: TStream);
var
  S: string;
begin
  S := GetText;
  Stream.WriteBuffer(Pointer(S)^, Length(S));
end;

procedure TCustomCardinalList.Sort;
begin
  inherited Sort({$IFDEF FPC}@{$ENDIF}SortBySmall );
end;

procedure TCustomCardinalList.AddIntegers(IntegerList: TCustomCardinalList);
var
  ICounter : integer;
begin
  for ICounter := 0 to IntegerList.Count -1 do
  begin
    Add(IntegerList.Items[ICounter]);
  end;
end;

{ TSortedCardinalList }

procedure TSortedCardinalList.AddSorted(Num: mUnsignedInt);
  procedure InsertInto (aStart, aEnd : integer);
  var
    tempValue, midIndex : integer;
  begin
    tempValue := (aEnd - aStart);
    if tempValue <= 1 then
      Self.Inst(aStart + 1, Num)
    else
    begin
      MidIndex := aStart + (tempValue div 2);
      if InternalGet(MidIndex) < Num then
        InsertInto(MidIndex, aEnd)
      else
        InsertInto(aStart, MidIndex);
    end;
  end;
var
  tempCount : integer;
begin
  Self.Sort;
  tempCount := Self.Count;
  if tempCount = 0 then
    inherited Add(Num)
  else
  begin
    if Num < Self.InternalGet(0) then
      Self.Inst(0, Num)
    else
    if Num > Self.InternalGet(tempCount-1) then
      inherited Add(Num)
    else
    begin
      InsertInto(0, tempCount - 1);
    end;
  end;
  Self.FSorted := true;
end;


function TSortedCardinalList.Get(Index: Integer): mUnsignedInt;
begin
  if not FSorted then
    Self.Sort;
  Result := InternalGet(Index);
end;

function TSortedCardinalList.IndexOf(Num: mUnsignedInt): Integer;
  function MidIndexOf(aStart, aEnd : integer) : integer;
  var
    tempValue, midIndex : integer;
  begin
    Result := -1;
    tempValue := (aEnd - aStart);
    if tempValue <= 1 then
    begin
      if Num = Self.Get(aStart) then
        Result := aStart
      else
      if Num = Self.Get(aEnd) then
        Result := aEnd;
    end
    else
    begin
      MidIndex := aStart + (tempValue div 2);
      if Get(MidIndex) < Num then
        Result := MidIndexOf(MidIndex, aEnd)
      else
        Result := MidIndexOf(aStart, MidIndex);
    end;
  end;
var
  tempLast : integer;
begin
  if not FSorted then
    Self.Sort;

  Result := -1;

  if Self.Count = 0 then
    exit;

  if Num < Self.Get(0) then
    exit
  else
  begin
    tempLast := Count - 1;
    if Num > Self.Get(tempLast) then
      exit
    else
      Result := MidIndexOf(0, tempLast);
  end;
end;

function TSortedCardinalList.InternalGet(Index: Integer): mUnsignedInt;
begin
Result := inherited Get(Index);
end;

procedure TSortedCardinalList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  FSorted := false;
end;

procedure TSortedCardinalList.Sort;
begin
  if FSorted then
    exit;
  inherited;
  FSorted := true;
end;

{ TCardinalList }

procedure TCardinalList.Insert(Index: Integer; Num: mUnsignedInt);
begin
  Inst(Index, Num);
end;

end.
