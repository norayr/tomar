unit FeedListUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, dbf;

type
  TLogProc = procedure(const S: string) of object;

procedure LoadFeedItemsIntoList(AListView: TListView; AFeedItemsDb, AReadStatusDb: TDbf;
  const AURL: string; IsYouTubeFeed: Boolean; ALogProc: TLogProc = nil);
procedure MarkAllListItemsAsRead(AListView: TListView; AReadStatusDb: TDbf;
  const AFeedURL: string; ALogProc: TLogProc = nil);

implementation

uses
  FeedModel, FeedDbUtils, RssUtils;

function CompareFeedEntriesDescending(Item1, Item2: Pointer): Integer;
var
  A, B: TFeedListEntry;
begin
  A := TFeedListEntry(Item1);
  B := TFeedListEntry(Item2);

  Result := CompareStr(B.SortKey, A.SortKey);
  if Result = 0 then
    Result := CompareText(A.Title, B.Title);
end;

procedure LogMessage(ALogProc: TLogProc; const S: string);
begin
  if Assigned(ALogProc) then
    ALogProc(S);
end;

procedure LoadFeedItemsIntoList(AListView: TListView; AFeedItemsDb, AReadStatusDb: TDbf;
  const AURL: string; IsYouTubeFeed: Boolean; ALogProc: TLogProc = nil);
var
  ListItem: TListItem;
  Entries: TList;
  Entry: TFeedListEntry;
  I: Integer;
begin
  if not Assigned(AListView) then
    Exit;

  Entries := TList.Create;
  AListView.Items.BeginUpdate;
  try
    AListView.Items.Clear;

    if not Assigned(AFeedItemsDb) or not AFeedItemsDb.Active then
      Exit;

    if AFeedItemsDb.RecordCount = 0 then
      Exit;

    AFeedItemsDb.First;
    while not AFeedItemsDb.EOF do
    begin
      if AFeedItemsDb.FieldByName('FEEDURL').AsString = AURL then
      begin
        Entry := TFeedListEntry.Create;
        Entry.Title := AFeedItemsDb.FieldByName('TITLE').AsString;
        Entry.PubDate := AFeedItemsDb.FieldByName('PUBDATE').AsString;
        Entry.ContentValue := AFeedItemsDb.FieldByName('CONTENT').AsString;
        Entry.LinkValue := AFeedItemsDb.FieldByName('LINK').AsString;
        Entry.ItemKeyValue := AFeedItemsDb.FieldByName('ITEMKEY').AsString;
        Entry.Unread := not FeedDbUtils.IsItemRead(AReadStatusDb, AURL, Entry.ItemKeyValue, Entry.LinkValue, ALogProc);
        Entry.SortKey := FeedDateSortKey(Entry.PubDate);
        Entries.Add(Entry);
      end;

      AFeedItemsDb.Next;
    end;

    Entries.Sort(@CompareFeedEntriesDescending);

    for I := 0 to Entries.Count - 1 do
    begin
      Entry := TFeedListEntry(Entries[I]);
      ListItem := AListView.Items.Add;
      ListItem.Caption := Entry.Title;
      ListItem.SubItems.Add(Entry.PubDate);

      if IsYouTubeFeed then
        ListItem.SubItems.Add(Entry.LinkValue)
      else
        ListItem.SubItems.Add(Entry.ContentValue);

      ListItem.SubItems.Add(Entry.LinkValue);
      ListItem.SubItems.Add(Entry.ItemKeyValue);

      if Entry.Unread then
        ListItem.Data := Pointer(1)
      else
        ListItem.Data := nil;
    end;
  finally
    for I := 0 to Entries.Count - 1 do
      TObject(Entries[I]).Free;
    Entries.Free;
    AListView.Items.EndUpdate;
  end;
end;

procedure MarkAllListItemsAsRead(AListView: TListView; AReadStatusDb: TDbf;
  const AFeedURL: string; ALogProc: TLogProc = nil);
var
  i: Integer;
  ItemLink, ItemKey: string;
begin
  if not Assigned(AListView) then
    Exit;

  LogMessage(ALogProc, '=== Mark All As Read Debug ===');
  LogMessage(ALogProc, Format('Feed URL parameter: "%s"', [AFeedURL]));
  LogMessage(ALogProc, 'Total items: ' + IntToStr(AListView.Items.Count));
  LogMessage(ALogProc, '');

  for i := 0 to AListView.Items.Count - 1 do
  begin
    if AListView.Items[i].SubItems.Count > 2 then
    begin
      ItemLink := AListView.Items[i].SubItems[2];
      if AListView.Items[i].SubItems.Count > 3 then
        ItemKey := AListView.Items[i].SubItems[3]
      else
        ItemKey := ItemLink;

      LogMessage(ALogProc, Format('[%d] Key: %s', [i, ItemKey]));
      FeedDbUtils.MarkItemAsRead(AReadStatusDb, AFeedURL, ItemKey, ItemLink, ALogProc);
      AListView.Items[i].Data := nil;
    end
    else
      LogMessage(ALogProc, Format('[%d] SKIPPED: SubItems.Count=%d (need >2)',
        [i, AListView.Items[i].SubItems.Count]));
  end;
end;

end.
