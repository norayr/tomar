unit FeedTreeUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, dbf, FeedModel;

procedure UpdateFeedNodeText(Node: TTreeNode; AFeedItemsDb, AReadStatusDb: TDbf);
procedure UpdateAllFeedNodeTexts(ATreeView: TTreeView; AFeedItemsDb, AReadStatusDb: TDbf);

implementation

uses
  FeedDbUtils;

function StripUnreadCount(const AText: string): string;
begin
  Result := AText;
  if Pos(' (', Result) > 0 then
    Result := Copy(Result, 1, Pos(' (', Result) - 1);
end;

procedure UpdateFeedNodeText(Node: TTreeNode; AFeedItemsDb, AReadStatusDb: TDbf);
var
  NodeData: TFeedNodeData;
  UnreadCount: Integer;
  BaseText: string;
begin
  if not Assigned(Node) then
    Exit;

  NodeData := TFeedNodeData(Node.Data);
  if not Assigned(NodeData) or NodeData.IsFolder then
    Exit;

  UnreadCount := FeedDbUtils.GetUnreadCount(AFeedItemsDb, AReadStatusDb, NodeData.FeedURL);
  BaseText := StripUnreadCount(Node.Text);

  if UnreadCount > 0 then
    Node.Text := BaseText + ' (' + IntToStr(UnreadCount) + ')'
  else
    Node.Text := BaseText;
end;

procedure UpdateAllFeedNodeTexts(ATreeView: TTreeView; AFeedItemsDb, AReadStatusDb: TDbf);
var
  i, UnreadCount: Integer;
  Node: TTreeNode;
  NodeData: TFeedNodeData;
  BaseText, FeedURL, LinkValue, ItemKeyValue, ReadKey1, ReadKey2: string;
  ReadKeys, Counts: TStringList;
begin
  if not Assigned(ATreeView) then
    Exit;
  if not Assigned(AFeedItemsDb) or not AFeedItemsDb.Active then
    Exit;

  ReadKeys := FeedDbUtils.BuildReadKeyCache(AReadStatusDb);
  Counts := TStringList.Create;
  try
    Counts.Sorted := False;
    Counts.Duplicates := dupAccept;

    if AFeedItemsDb.RecordCount > 0 then
    begin
      AFeedItemsDb.First;
      while not AFeedItemsDb.EOF do
      begin
        FeedURL := AFeedItemsDb.FieldByName('FEEDURL').AsString;
        LinkValue := AFeedItemsDb.FieldByName('LINK').AsString;
        ItemKeyValue := AFeedItemsDb.FieldByName('ITEMKEY').AsString;

        ReadKey1 := FeedURL + #9 + ItemKeyValue;
        ReadKey2 := FeedURL + #9 + LinkValue;
        if (ReadKeys.IndexOf(ReadKey1) < 0) and
           ((LinkValue = '') or (ReadKeys.IndexOf(ReadKey2) < 0)) then
        begin
          FeedDbUtils.IncrementCountValue(Counts, FeedURL);
        end;

        AFeedItemsDb.Next;
      end;
    end;

    ATreeView.Items.BeginUpdate;
    try
      for i := 0 to ATreeView.Items.Count - 1 do
      begin
        Node := ATreeView.Items[i];
        if Assigned(Node) and Assigned(Node.Data) then
        begin
          NodeData := TFeedNodeData(Node.Data);
          if Assigned(NodeData) and (not NodeData.IsFolder) then
          begin
            BaseText := StripUnreadCount(Node.Text);
            UnreadCount := FeedDbUtils.GetCountValue(Counts, NodeData.FeedURL);
            if UnreadCount > 0 then
              Node.Text := BaseText + ' (' + IntToStr(UnreadCount) + ')'
            else
              Node.Text := BaseText;
          end;
        end;
      end;
    finally
      ATreeView.Items.EndUpdate;
    end;

    ATreeView.Invalidate;
  finally
    Counts.Free;
    ReadKeys.Free;
  end;
end;

end.
