(***************************************************************************

    Copyright 1998-2010, Christian Aymon (cyamon software, www.cyamon.com)

    This file is part of ''Discover''.

    ''Discover'' is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    any later version.

    ''Discover'' is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Discover.  If not, see <http://www.gnu.org/licenses/>.

 
***************************************************************************)
unit Objects;

{ O+,F+,X+,I-,S-}

interface
  uses
    Classes;

type
  TSavableObject = class(TObject)
  private
    FOnChanged : TNotifyEvent;
  protected
    procedure SetOnChanged(aOnChanged : TNotifyEvent); virtual;
  public
    procedure Save(aStream : TStream); virtual;
    property OnChanged : TNotifyEvent read FOnChanged write SetOnChanged;
    procedure HasChanged;
  end {TSavableObject};

  TOwlCollection = class(TSavableObject)
  public
    constructor Create;
    destructor Destroy; override;
    function At(Index: Integer): pointer;
    procedure AtDelete(Index: Integer); virtual;
    procedure AtFree(Index: Integer); virtual;
    procedure AtInsert(Index: Integer; Item: Pointer); virtual;
    procedure AtPut(Index: Integer; Item: Pointer);
    procedure DeleteAll;
    procedure FreeOneItem(Item: Pointer);
    procedure FreeAll;
    function IndexOf(Item: Pointer): Integer; virtual;
    procedure Insert(Item: Pointer); virtual;
    procedure Pack;
  protected
    procedure FreeItem(Item: Pointer); virtual;
    procedure SetItemOnChanged(Item : pointer); virtual;
    procedure SetOnChanged(aOnChanged : TNotifyEvent); override;
  private
    List : TList;
    function GetCount : integer;
  public
    property Count : integer read GetCount;
  end;

{ TSortedCollection object }

  TSortedCollection = class(TOwlCollection)
    Duplicates: Boolean;
    constructor Create;
    function Compare(Key1, Key2: Pointer): Integer; virtual; abstract;
    function IndexOf(Item: Pointer): Integer; override;
    procedure Insert(Item: Pointer); override;
    function KeyOf(Item: Pointer): Pointer; virtual;
    function Search(Key: Pointer; var Index: Integer): Boolean; virtual;
  end;

{ TStrCollection object }

  TStrCollection = class(TSortedCollection)
    function Compare(Key1, Key2: Pointer): Integer; override;
    procedure FreeItem(Item: Pointer); override;
    procedure SetItemOnChanged(Item : pointer); override;
  end;

implementation
  uses
    SysUtils;
{~t}
(*********************)
(* TOwlCollection.At *)
(*********************)

function TOwlCollection.At(Index: Integer): pointer;
  begin
    At := List.Items[Index];
end {TOwlCollection.At};


(***************************)
(* TOwlCollection.AtDelete *)
(***************************)

procedure TOwlCollection.AtDelete(Index: Integer);
  begin
    List.Delete(Index);
    HasChanged;
end {TOwlCollection.AtDelete};


(*************************)
(* TOwlCollection.AtFree *)
(*************************)

procedure TOwlCollection.AtFree(Index: Integer);
var
  Item: Pointer;
begin
  Item := At(Index);
  AtDelete(Index);
  FreeItem(Item);
end {TOwlCollection.AtFree};


(***************************)
(* TOwlCollection.AtInsert *)
(***************************)

procedure TOwlCollection.AtInsert(Index: Integer; Item: Pointer);
  begin
    List.Insert(Index, Item);
    SetItemOnChanged(Item);
    HasChanged;
end {TOwlCollection.AtInsert};


(************************)
(* TOwlCollection.AtPut *)
(************************)

procedure TOwlCollection.AtPut(Index: Integer; Item: Pointer);
  begin
    with List do begin
      {Si la liste n'a pas la taille convenable on insère des éléments
       nil}
      while Index >= Count do
        Add(nil);
      Items[Index] := Item;
    end {with};
    SetItemOnChanged(Item);
    HasChanged;
end {TOwlCollection.AtPut};


(*************************)
(* TOwlCollection.Create *)
(*************************)

constructor TOwlCollection.Create;
begin
  Inherited Create;
  List := TList.Create;
end {TOwlCollection.Create};


(****************************)
(* TOwlCollection.DeleteAll *)
(****************************)

procedure TOwlCollection.DeleteAll;
begin
  List.Count := 0;
  HasChanged;
end {TOwlCollection.DeleteAll};


(**************************)
(* TOwlCollection.Destroy *)
(**************************)

destructor TOwlCollection.Destroy;
begin
  FOnChanged := nil;
  FreeAll;
  List.Free;
  inherited Destroy;
end {TOwlCollection.Destroy};


(**************************)
(* TOwlCollection.FreeAll *)
(**************************)

procedure TOwlCollection.FreeAll;
var
  I: Integer;
begin
  for I := 0 to List.Count - 1 do
    FreeItem(At(I));
  DeleteAll;
end {TOwlCollection.FreeAll};


(***************************)
(* TOwlCollection.FreeItem *)
(***************************)

procedure TOwlCollection.FreeItem(Item: Pointer);
begin
  if Item <> nil then
    (TObject(Item) as TObject).Free;
end {TOwlCollection.FreeItem};


(******************************)
(* TOwlCollection.FreeOneItem *)
(******************************)

procedure TOwlCollection.FreeOneItem(Item: Pointer);
begin
  AtDelete(IndexOf(Item));
  FreeItem(Item);
end {TOwlCollection.FreeOneItem};


(***************************)
(* TOwlCollection.GetCount *)
(***************************)

function TOwlCollection.GetCount : integer;
  begin
    Result := List.Count;
end {TOwlCollection.GetCount};


(**************************)
(* TOwlCollection.IndexOf *)
(**************************)

function TOwlCollection.IndexOf(Item: Pointer): Integer;
  begin
    IndexOf := List.IndexOf(Item);
end {TOwlCollection.IndexOf};


(*************************)
(* TOwlCollection.Insert *)
(*************************)

procedure TOwlCollection.Insert(Item: Pointer);
begin
  List.Add(Item);
  SetItemOnChanged(Item);
  HasChanged;
end {TOwlCollection.Insert};


(***********************)
(* TOwlCollection.Pack *)
(***********************)

procedure TOwlCollection.Pack;
  begin
    List.Pack;
end {TOwlCollection.Pack};


(***********************************)
(* TOwlCollection.SetItemOnChanged *)
(***********************************)

procedure TOwlCollection.SetItemOnChanged(Item : pointer);
begin
  if Assigned(OnChanged) then
    (TObject(Item) as TSavableObject).OnChanged := OnChanged;
end {TOwlCollection.SetItemOnChanged};


(*******************************)
(* TOwlCollection.SetOnChanged *)
(*******************************)

procedure TOwlCollection.SetOnChanged;
  var
    i : integer;
begin
  inherited SetOnChanged(aOnChanged);
  for i := 0 to pred(Count) do
    SetItemOnChanged(At(i));
end {TOwlCollection.SetOnChanged};


(*****************************)
(* TSavableObject.HasChanged *)
(*****************************)

procedure TSavableObject.HasChanged;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end {TSavableObject.HasChanged};


(***********************)
(* TSavableObject.Save *)
(***********************)

procedure TSavableObject.Save;
  begin
    raise Exception.Create('Call to TSavableObject.Save');
end {TSavableObject.Save};


(*******************************)
(* TSavableObject.SetOnChanged *)
(*******************************)

procedure TSavableObject.SetOnChanged;
begin
  FOnChanged := aOnChanged;
end {TSavableObject.SetOnChanged};


(****************************)
(* TSortedCollection.Create *)
(****************************)

constructor TSortedCollection.Create;
begin
  Inherited Create;
  Duplicates := False;
end {TSortedCollection.Create};


(*****************************)
(* TSortedCollection.IndexOf *)
(*****************************)

function TSortedCollection.IndexOf(Item: Pointer): Integer;
var
  I: Integer;
begin
  IndexOf := -1;
  if Search(KeyOf(Item), I) then
  begin
    if Duplicates then
      while (I < List.Count) and (Item <> List.Items[I]) do Inc(I);
    if I < List.Count then IndexOf := I;
  end;
end {TSortedCollection.IndexOf};


(****************************)
(* TSortedCollection.Insert *)
(****************************)

procedure TSortedCollection.Insert(Item: Pointer);
var
  I: Integer;
begin
  if not Search(KeyOf(Item), I) or Duplicates then AtInsert(I, Item);
end {TSortedCollection.Insert};


(***************************)
(* TSortedCollection.KeyOf *)
(***************************)

function TSortedCollection.KeyOf(Item: Pointer): Pointer;
begin
  KeyOf := Item;
end {TSortedCollection.KeyOf};


(****************************)
(* TSortedCollection.Search *)
(****************************)

function TSortedCollection.Search(Key: Pointer; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Search := False;
  L := 0;
  H := List.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := Compare(KeyOf(List.Items[I]), Key);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Search := True;
        if not Duplicates then L := I;
      end;
    end;
  end;
  Index := L;
end {TSortedCollection.Search};


(**************************)
(* TStrCollection.Compare *)
(**************************)

function TStrCollection.Compare(Key1, Key2: Pointer): Integer;
begin
  Compare := StrComp(PWideChar(Key1), PWideChar(Key2));
end {TStrCollection.Compare};


(***************************)
(* TStrCollection.FreeItem *)
(***************************)

procedure TStrCollection.FreeItem(Item: Pointer);
begin
  StrDispose(PWideChar(Item));
end {TStrCollection.FreeItem};


(***********************************)
(* TStrCollection.SetItemOnChanged *)
(***********************************)

procedure TStrCollection.SetItemOnChanged(Item : pointer);
begin
end {TStrCollection.SetItemOnChanged};


{~b}
end.

