// http://www.ibrtses.com/delphi/binarytree.html
//
//  Taken from Nicklaus Wirth :
//    Algorithmen und Datenstrukturen ( in Pascal )
//    Balanced Binary Trees p 250 ++
//
//
// Fixed By Giacomo Policicchio
// pgiacomo@tiscalinet.it
// 19/05/2000
//

unit mBinaryTree;

interface

uses classes;

type
  TmBinTreeItem = class
  private
    left,right:TmBinTreeItem;
    bal:-1..1;
    Count:integer;
  public
    constructor Create;
    // a < self :-1  a=self :0  a > self :+1
    function Compare(aItem : TmBinTreeItem):Shortint; virtual; abstract;
    //procedure CopyTo(ToItem : TmBinTreeItem); virtual; abstract;
    procedure List; virtual;
  end;

 TmBinTree=class(TPersistent)
   root:TmBinTreeItem;
  private
   ItemCount:integer;
   procedure Delete(item:TmBinTreeItem; var parentItem:TmBinTreeItem; var h:boolean; var ok:boolean);
   procedure SearchAndInsert(item:TmBinTreeItem; parentItem : TmBinTreeItem; var h:boolean;Var Found:boolean);
   function SearchItem(item:TmBinTreeItem;var p:TmBinTreeItem):boolean;
   procedure BalanceLeft(var parentItem : TmBinTreeItem;var h:boolean; IsDeleting:boolean);
   procedure BalanceRight(var parentItem : TmBinTreeItem;var h:boolean; IsDeleting:boolean);
   procedure listitems(var p:TmBinTreeItem);
  public
    constructor Create;
    destructor Destroy; override;

    function Add (item : TmBinTreeItem) : boolean;
    function Remove (item:TmBinTreeItem) : boolean;
    function Search (item:TmBinTreeItem) : boolean;
    procedure list;      // uses item.list through listitems recursively
  end;


implementation

//=================================================================
constructor TmBinTreeItem.Create;
begin
  inherited Create;
  Count := 0;
end;

//=================================================================

constructor TmBinTree.Create;
begin
 inherited Create;
 root:=nil;
 ItemCount:=0;
end;

destructor TmBinTree.Destroy;
begin
  inherited;
 while root <> nil do remove(root);
 inherited destroy;
end;

procedure TmBinTree.SearchAndInsert (item:TmBinTreeItem; parentItem : TmBinTreeItem; var h : boolean; var found:boolean);
begin
  found := false;
  if parentItem = nil then
  begin        // word not in tree, insert it
    parentItem := item;
    h := true;
    if Self.root = nil then
      root := parentItem;
    parentItem.Count := 1;
    parentItem.left:=nil;
    parentItem.right:=nil;
    parentItem.bal:=0;
  end
  else
  if (item.compare(parentItem) > 0) then      // new < current
  begin
    SearchAndInsert(item, parentItem.left, h, found);
    if h and (not found) then
      BalanceLeft(parentItem, h, false);
  end
  else
  if (item.compare(parentItem) < 0) then     // new > current
  begin
    SearchAndInsert(item, parentItem.right, h, found);
    if h and not found then
      balanceRight(parentItem, h, false);
  end
  else
  begin
    inc(parentItem.count);
    h := false;
    found := true;
  end;
end;      //searchAndInsert

// returns true and a pointer to the equal item if found, false otherwise
function TmBinTree.SearchItem(item:TmBinTreeItem;Var p:TmBinTreeItem):boolean;
begin
 result:=false;
 if (p=nil) then result:=false // empty
 else begin
  if (item.compare(p) =0) then result:=true
  else begin
   if (item.compare(p) >0) then result:=searchitem(item,p.left)
   else begin
    if (item.compare(p) <0) then result:=searchitem(item,p.right)
   end;
  end;
 end;
end;


procedure TmBinTree.BalanceRight(var parentItem : TmBinTreeItem; var h:boolean; IsDeleting:boolean);
var
  p1,p2 : TmBinTreeItem;
begin
  case parentItem.bal of
    -1:
      begin
        parentItem.bal:=0;
        if not IsDeleting then
          h := false;
      end;
    0:
      begin
        parentItem.bal := +1;
        if IsDeleting then
          h:=false;
      end;
    +1:
      begin    // new balancing
        p1 := parentItem.right;
        if (p1.bal=+1) or ((p1.bal=0) and IsDeleting) then
        begin  // single rr rotation
          parentItem.right := p1.left;
          p1.left := parentItem;
          if not IsDeleting then
            parentItem.bal := 0
          else
          begin
            if p1.bal=0 then
            begin
              parentItem.bal := +1;
              p1.bal := -1;
              h:=false;
            end
            else
            begin
              parentItem.bal:=0;
              p1.bal:=0;
                              (* h:=false; *)
            end;
          end;
          parentItem := p1;
        end
        else
        begin  // double rl rotation
          p2 := p1.left;
          p1.left := p2.right;
          p2.right := p1;
          parentItem.right := p2.left;
          p2.left := parentItem;
          if p2.bal=+1 then
            parentItem.bal:=-1
          else
            parentItem.bal:=0;
          if p2.bal=-1 then
            p1.bal:=+1
          else
            p1.bal:=0;
          parentItem := p2;
          if IsDeleting then
            p2.bal:=0;
        end;
        if not IsDeleting then
        begin
          parentItem.bal := 0;
          h := false;
        end;
      end;
  end; // case
end;

procedure TmBinTree.BalanceLeft(var parentItem : TmBinTreeItem; var h:boolean; IsDeleting:boolean);
var
  p1,p2:TmBinTreeItem;
Begin
  case parentItem.bal of
    1:
      begin
        parentItem.bal := 0;
        if not IsDeleting then
          h := false;
      end;
    0:
      begin
        parentItem.bal := -1;
        if IsDeleting then
          h := false;
      end;
    -1:(* if (p.Left<>nil) or not dl then *)
      begin   // new balancing
        p1 := parentItem.left;
        if (p1.bal = -1) or ((p1.bal=0) and IsDeleting) then
        begin   // single ll rotation
          parentItem.left := p1.right;
          p1.right := parentItem;
          if not IsDeleting then
            parentItem.bal:=0
          else
          begin
            if p1.bal=0 then
            begin
              parentItem.bal := -1;
              p1.bal := +1;
              h:=false;
            end
            else
            begin
              parentItem.bal := 0;
              p1.bal := 0;
              (* h:=false; *)
            end;
          end;
          parentItem:=p1;
        end
        else
        begin //double lr rotation
          p2 := p1.right;
          p1.right := p2.left;
          p2.left := p1;
          parentItem.left := p2.right;
          p2.right := parentItem;
          if p2.bal=-1 then
            parentItem.bal:=+1
          else
            parentItem.bal:=0;
          if p2.bal=+1 then
            p1.bal:=-1
          else
            p1.bal:=0;
          parentItem:=p2;
          if IsDeleting then
            p2.bal:=0;
        end;
        if not IsDeleting then
        begin
          parentItem.bal:=0;
          h:=false;
        end;
      end; { -1 }
  end; { case }
end;


procedure TmBinTree.Delete(item:TmBinTreeItem; var parentItem:TmBinTreeItem; var h:boolean; var ok:boolean);
var
  q : TmBinTreeItem; //h=false;

  procedure del(var r:TmBinTreeItem; var h:boolean);
  begin //h=false
    if r.right <> nil then
    begin
      del(r.right,h);
      if h then
        BalanceLeft(r, h, True);
    end
    else
    begin
      r.CopyTo(q);  { q.key:=r.key; }
      q.count := r.count;
      q := r;
      r := r.left;
      h := true;
    end;
  end;

begin { main of delete }
  ok:=true;
  if (parentItem = nil) then
  begin
    Ok:=false;
    h:=false;
  end
  else
  if (item.compare(parentItem) > 0){(x < p^.key)} then
  begin
    delete(item, parentItem.left, h, ok);
    if h then
      BalanceRight(parentItem, h, true);
  end
  else
  if (item.compare(parentItem) < 0){(x > p^.key)}then
  begin
    delete(item, parentItem.right, h, ok);
    if h then
      BalanceLeft(parentItem, h, True);
    end
  else
  begin // remove q
    q := parentItem;
    if q.right=nil then
    begin
      parentItem := q.left;
      h:=true;
    end
    else
    if (q.left=nil) then
    begin
      parentItem := q.right;
      h:=true;
    end
    else
    begin
      del(q.left,h);
      if h then
        BalanceRight(parentItem, h, True);
    end;
    q.free; {dispose(q)};
  end;
end; { delete }

function TmBinTree.Add(item:TmBinTreeItem):boolean;
var
  h, found:boolean;
begin
  SearchAndInsert(item, root, h, found);
  Result := found;
end;          

function TmBinTree.remove(item:TmBinTreeItem):boolean;
var
  h,ok : boolean;
begin
  Delete(item,root,h,ok);
  remove := ok;
end;

Function TmBinTree.Search(item:TmBinTreeItem):Boolean;
begin
  Result := SearchItem(item,root);
end;

procedure TmBinTree.listitems(var p:TmBinTreeItem);
begin
 if p<>nil then begin
  if (p.left <> nil) then listitems(p.left);
  p.list;
  if (p.right <> nil) then listitems(p.right);
 end;
end;

procedure TmBinTree.list;      // uses item.list recursively
begin
 listitems(root);
end;

procedure TmBinTreeItem.List;
begin
  //
end;

end.