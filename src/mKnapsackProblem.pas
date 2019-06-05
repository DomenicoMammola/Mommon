// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mKnapsackProblem;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, contnrs;

type

  { TEquipment }

  TEquipment = class
  strict private
    FReference : pointer;
    FWeight : integer;
    FValue : integer;
  public
    constructor Create;

    property Reference : pointer read FReference write FReference;
    property Weight : integer read FWeight write FWeight;
    property Value : integer read FValue write FValue;
  end;

  { TEquipmentList }

  TEquipmentList = class
  strict private
    FList : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;

    function Count : integer;
    function Add : TEquipment;
    function Get(const aIndex: integer) : TEquipment;
  end;


// re-worked from: https://rosettacode.org/wiki/Knapsack_problem/0-1#Pascal
procedure OptimizeKnapsack (const aKnapsackWeight : integer; const aEquipmentList : TEquipmentList; aOptimizedEquipmentList : TList);
// re-worked from: "Discrete optimization algorithms with pascal programs" by Syslo, Deo, Kowalik
procedure OptimizeKnapsackMartelloToth(const aKnapsackWeight : integer; const aEquipmentList : TEquipmentList; aOptimizedEquipmentList : TList);

implementation

uses
  Math;


procedure OptimizeKnapsack(const aKnapsackWeight : integer; const aEquipmentList: TEquipmentList; aOptimizedEquipmentList: TList);
var
  i, j, maxValue, weightLeft : integer;
  maxArray : array of array of integer;
begin
  SetLength(maxArray, aEquipmentList.Count + 1, aKnapsackWeight + 1);

  for j := 0 to aKnapsackWeight do
    maxArray[0, j] := 0;

  for i := 0 to aEquipmentList.Count - 1 do
  begin
    for j := 0 to aKnapsackWeight do
    begin
      if aEquipmentList.Get(i).Weight > j then
        maxArray[i + 1, j] := maxArray[i, j]
      else
        maxArray[i + 1, j] := max(maxArray[i, j], maxArray[i, j- aEquipmentList.Get(i).Weight] + aEquipmentList.Get(i).Value);
    end;
  end;

  //get the highest total value by testing every value
  maxValue := 0;
  for i := 0 to aEquipmentList.Count - 1 do
  begin
    for j := 0 to aKnapsackWeight do
    begin
      if maxArray[i + 1,j] > maxValue then
          maxValue := maxArray[i + 1, j];
    end;
  end;


  //Work backwards through the items to find those items that go in the Knapsack
  weightLeft:= aKnapsackWeight;
  for i := aEquipmentList.Count -1  downto 0 do
  begin
    if maxArray[i + 1, weightLeft] = maxValue then
    begin
      if maxArray[i, weightLeft - aEquipmentList.Get(i).Weight] = maxValue - aEquipmentList.Get(i).Value then
      begin
        aOptimizedEquipmentList.Add(aEquipmentList.Get(i).Reference);
        maxValue:= maxValue - aEquipmentList.Get(i).Value;
        weightLeft:= weightLeft - aEquipmentList.Get(i).Weight;
      end;
    end;
  end;
end;

procedure OptimizeKnapsackMarteLLoToth(const aKnapsackWeight: integer; const aEquipmentList: TEquipmentList; aOptimizedEquipmentList: TList);
var
  n: integer;
  p, w, x: array of integer;
  v : integer;
  profit, count : integer;
  d, i, j, k, L, LL, lim, m, pp, q, r, t, ww: integer;
  b, step2, step4, step56, step7, stop: boolean;
  min, pd, wd, y, zd : array of integer;

  procedure WorkVar;
  var
    w1, p1: integer;
    ff : integer;
  begin
    for ff := i to L do
      y[ff-1]:= 1;
    p1 := pp;
    pd[i-1] := p1;
    w1:= v-ww;
    wd[i-1]:= w1;
    zd[i-1]:= L+1;
    for ff:= i+1 to L do
    begin
      p1 := p1 - p[ff-2];
      pd[ff-1] := p1;
      w1:= w1 - w[ff-2];
      wd[ff-1]:= w1;
      zd[ff-1]:= L + 1;
    end;
    for ff:= L+1 to LL do
    begin
      pd[ff-1]:= 0;
      wd[ff-1]:= 0;
      zd[ff-1]:= ff;
    end;
    v := ww;
    q:= q + pp;
    LL:= L;
  end;

begin
  m := -1;
  n := aEquipmentList.Count;
  SetLength(p, n+1);
  SetLength(w, n+1);
  SetLength(x, n);
  SetLength(min, n);
  SetLength(pd, n);
  SetLength(wd, n);
  SetLength(y, n);
  SetLength(zd, n);

  for i := 0 to aEquipmentList.Count -1 do
  begin
    p[i] := aEquipmentList.Get(i).Value;
    w[i] := aEquipmentList.Get(i).Weight;
  end;
  v := aKnapsackWeight;

  count:= 1;
  pp:= 0;
  ww:= v;
  L:= 0;
  while ww >= w[L] do
  begin
    L := L + 1;
    pp:= pp + p[L-1];
    ww:= ww - w[L-1];
  end;
  stop := (ww=0);
  if stop then
  begin // the GREEDY solution is optimal
    profit := pp;
    for j := 1 to L do
      x[j-1]:= 1;
    for j := L+1 to n do
      x[j-1] := 0;
  end
  else
  begin // initialization
    r:= v+1;
    min[n]:= r;
    for j := n downto 2 do
    begin
      if w[j-1] < r then
        r := w[j-1];
      min[j-2] := r;
    end;
    p[n]:= 0;
    w[n]:= v+1;
    lim:= pp + trunc(ww*p[L+1]/w[L+1]);
    r:= pp+ trunc(p[L]-(w[L]-ww)*p[L-1]/w[L-1]);
    if r > lim then
      lim:= r;
    for j:= 1 to n do
      y[j-1] := 0;
    profit := 0;
    q := 0;
    i := 1;
    LL := n;
    step2 := false;
    step4 := true;
    repeat
      while step2 do
      begin // steps 2 and 3 - building a new solution
        count := count + 1;
        if w[i-1] <= v then
        begin // step 3
          step2 := false;
          pp := pd[i-1];
          ww := v - wd [i-1];
          L := zd[i-1];
          b := true;
          while (L <= n) and b do
          begin
            b := w[L-1] <= ww;
            if b then
            begin
              pp := pp + p[L-1];
              ww := ww- w[L-1];
              L := L + 1;
            end;
          end;
          L := L -1;
          if (ww > 0) and (L < n) then
          begin
            step56 := profit >= q + pp + trunc(ww * p[L]/w[L]);
            step4 := not step56;
          end
          else
          begin
            step56 := true;
            step4 := false;
            if profit < q + pp then
            begin
              profit := q + pp;
              for j := 1 to i-1 do
                x[j-1]:=y[j-1];
              for j := 1 to L do
              x[j-1] := 1;
              for j := L + 1 to n do
                x[j-1] := 0;
              stop := profit = lim;
              step56 := not stop;
            end;
          end;
        end // step 3
        else
        begin
          step56 := profit >= q + trunc(v* p[i]/w[i]);
          step4 := false;
          step2 := not step56;
          if step2 then
            i := i +1;
        end;
      end; // while step2
      if step4 then
      begin // step 4 - saving current solution
        WorkVar;
        if L < n -2 then
        begin
          i:= L + 2;
          step56 := v < min[i-2];
          step2 := not step56;
          step7 := false;
        end
        else
        begin
          step56 := true;
          if L = n -2 then
          begin
            if v >= w[n-1] then
            begin
              q := q + p[n-1];
              v := v - w[n-1];
              y[n-1]:= 1
            end;
            i := n-1;
          end
          else
            i := n;
        end;
        if step56 then
        begin // step 5 - saving current optimal solution
          if profit < q then
          begin // better solution has been found
            profit := q;
            for j := 1 to n do
              x[j-1] := y [j-1];
            stop := profit = lim;
            step56 := not stop;
          end;
          if step56 and (y[n-1] = 1) then
          begin
            q := q - p[n-1];
            v:= v + w[n-1];
            y[n] := 0;
          end;
        end; // step56
      end; // step4

      if step56 then
      begin // steps 6 and 7 - backtracking
        stop := true;
        k := i;
        while stop and (k > 1) do
        begin
          k := k -1;
          stop := y[k-1]=0;
        end;
        step7 := not stop;
        if step7 then
        begin
          r:= v;
          y[k-1] := 0;
          q:= q - p[k-1];
          v := v + w[k-1];
          step2 := r >= min[k-1];
          step7 := not step2;
          if step2 then
            i := k +1
          else
          begin
            i := k;
            m := k+1;
          end;
        end;
        while step7 do
        begin // step 7 - process of substitution
          step56 := (m>n) or (profit >= q + trunc(v*p[m-1]/w[m-1]));
          step7 := not step56;
          step2 := step7;
          step4 := step7;
          if step7 then
          begin
            d := w[m-1] - w[k-1];
            t:= r- d;
            if d = 0 then
              m := m +1
            else
            if d > 0 then
            begin
              if (t < 0) or (profit >= q + p[m-1]) then
                m := m + 1
              else
              begin
                profit := q + p[m-1];
                for j := 1 to k do
                  x[j-1] := y [j-1];
                for j := k + 1 to n do
                  x[j-1] := 0;
                x[m-1] := 1;
                stop := profit = lim;
                step7 := not stop;
                if step7 then
                begin
                  r := t;
                  k := m;
                  m := m + 1;
                end;
              end;
            end // if d > 0
            else
            begin // d < 0
              if t < min[m-1] then
                m := m + 1
              else
              begin
                step7 := false;
                step56 := q + p[m-1] + trunc(t*p[m-1]/w[m-1]) <= profit;
                step2 := not step56;
                step4 := step2;
                if step2 then
                begin
                  q := q + p[m-1];
                  v := v- w[m-1];
                  y[m-1] := 1;
                  i := m + 1;
                  pd[m-1] := p [m-1];
                  wd[m-1] := w[m-1];
                  zd[m-1] := m+1;
                  for j := m+1 to LL do
                  begin
                    pd[j-1] := 0;
                    wd[j-1] := 0;
                    zd[j-1] := j;
                  end;
                  LL := m;

                end; // step2
              end;
            end; // d < 0

          end; // if step7


        end;// while step7

      end; // step56

    until stop;

  end; // not stop

  aOptimizedEquipmentList.Clear;
  for i := 0 to n -1 do
  begin
    if x[i] = 1 then
      aOptimizedEquipmentList.Add(aEquipmentList.Get(i).Reference);
  end;
end;

{ TEquipment }

constructor TEquipment.Create;
begin
  FReference:= nil;
  FWeight:= 0;
  FValue:= 0;
end;

{ TEquipmentList }

constructor TEquipmentList.Create;
begin
  FList := TObjectList.Create(true);
end;

destructor TEquipmentList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TEquipmentList.Count: integer;
begin
  Result := FList.Count;
end;

function TEquipmentList.Add: TEquipment;
begin
  Result := TEquipment.Create;
  FList.Add(Result);
end;

function TEquipmentList.Get(const aIndex: integer): TEquipment;
begin
  Result := FList.Items[aIndex] as TEquipment;
end;

end.
