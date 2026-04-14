unit FeedFetchUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, XMLRead, fphttpclient;

type
  TMarkFeedItemsAsNotSeenProc = procedure(const AURL: string) of object;
  TSaveFeedItemProc = procedure(const AFeedURL, AItemKey, ATitle, APubDate, AContent, ALink: string) of object;

function MakeItemKey(const AGuid, AId, ALink, ATitle, APubDate: string): string;
function ConvertYouTubeURLToFeed(AHttpClient: TFPHTTPClient; const AUrl: string): string;
procedure ParseFeedResponse(const AURL, Response: string;
  AMarkFeedItemsAsNotSeen: TMarkFeedItemsAsNotSeenProc;
  ASaveFeedItem: TSaveFeedItemProc;
  out IsYouTubeFeed: Boolean);

implementation

function MakeItemKey(const AGuid, AId, ALink, ATitle, APubDate: string): string;
begin
  if Trim(AGuid) <> '' then
    Exit(Copy(Trim(AGuid), 1, 255));

  if Trim(AId) <> '' then
    Exit(Copy(Trim(AId), 1, 255));

  if Trim(ALink) <> '' then
    Exit(Copy(Trim(ALink), 1, 255));

  Result := Copy(Trim(ATitle) + '|' + Trim(APubDate), 1, 255);
end;

function ConvertYouTubeURLToFeed(AHttpClient: TFPHTTPClient; const AUrl: string): string;
var
  ChannelId: string;
  PageSource: string;
  Stream: TStringStream;
  PosStart, PosEnd: Integer;
  CleanUrl: string;
begin
  Result := AUrl;

  if Pos('feeds/videos.xml', AUrl) > 0 then
    Exit;

  if (Pos('youtube.com', LowerCase(AUrl)) = 0) and
     (Pos('youtu.be', LowerCase(AUrl)) = 0) then
    Exit;

  CleanUrl := AUrl;
  PosStart := Pos('?', CleanUrl);
  if PosStart > 0 then
    CleanUrl := Copy(CleanUrl, 1, PosStart - 1);
  while (Length(CleanUrl) > 0) and (CleanUrl[Length(CleanUrl)] = '/') do
    Delete(CleanUrl, Length(CleanUrl), 1);

  PosStart := Pos('/channel/', CleanUrl);
  if PosStart > 0 then
  begin
    ChannelId := Copy(CleanUrl, PosStart + 9, Length(CleanUrl));
    PosEnd := Pos('/', ChannelId);
    if PosEnd > 0 then
      ChannelId := Copy(ChannelId, 1, PosEnd - 1);

    if ChannelId <> '' then
    begin
      Result := 'https://www.youtube.com/feeds/videos.xml?channel_id=' + ChannelId;
      Exit;
    end;
  end;

  if ((Pos('/@', CleanUrl) > 0) or (Pos('/user/', CleanUrl) > 0) or
      (Pos('/c/', CleanUrl) > 0)) and Assigned(AHttpClient) then
  begin
    Stream := TStringStream.Create('');
    try
      try
        AHttpClient.Get(AUrl, Stream);
        PageSource := Stream.DataString;

        PosStart := Pos('rel="alternate" type="application/rss+xml"', PageSource);
        if PosStart > 0 then
        begin
          PosStart := Pos('href="', PageSource, PosStart);
          if PosStart > 0 then
          begin
            PosStart := PosStart + 6;
            PosEnd := PosStart;
            while (PosEnd <= Length(PageSource)) and (PageSource[PosEnd] <> '"') do
              Inc(PosEnd);
            Result := Copy(PageSource, PosStart, PosEnd - PosStart);
            if Pos('feeds/videos.xml?channel_id=', Result) > 0 then
              Exit;
          end;
        end;

        PosStart := Pos('rel="canonical"', PageSource);
        if PosStart > 0 then
        begin
          PosStart := Pos('href="', PageSource, PosStart);
          if PosStart > 0 then
          begin
            PosStart := PosStart + 6;
            PosEnd := PosStart;
            while (PosEnd <= Length(PageSource)) and (PageSource[PosEnd] <> '"') do
              Inc(PosEnd);
            ChannelId := Copy(PageSource, PosStart, PosEnd - PosStart);
            PosStart := Pos('/channel/', ChannelId);
            if PosStart > 0 then
            begin
              ChannelId := Copy(ChannelId, PosStart + 9, Length(ChannelId));
              Result := 'https://www.youtube.com/feeds/videos.xml?channel_id=' + ChannelId;
              Exit;
            end;
          end;
        end;

        PosStart := Pos('"channelId":"', PageSource);
        if PosStart > 0 then
        begin
          PosStart := PosStart + 13;
          PosEnd := PosStart;
          while (PosEnd <= Length(PageSource)) and (PageSource[PosEnd] <> '"') do
            Inc(PosEnd);
          ChannelId := Copy(PageSource, PosStart, PosEnd - PosStart);
        end
        else
        begin
          PosStart := Pos('channel_id=', PageSource);
          if PosStart > 0 then
          begin
            PosStart := PosStart + 11;
            PosEnd := PosStart;
            while (PosEnd <= Length(PageSource)) and
                  (PageSource[PosEnd] in ['A'..'Z', 'a'..'z', '0'..'9', '_', '-']) do
              Inc(PosEnd);
            ChannelId := Copy(PageSource, PosStart, PosEnd - PosStart);
          end;
        end;

        if ChannelId <> '' then
          Result := 'https://www.youtube.com/feeds/videos.xml?channel_id=' + ChannelId;
      except
        on E: Exception do
          Result := AUrl;
      end;
    finally
      Stream.Free;
    end;
  end;
end;

procedure ParseFeedResponse(const AURL, Response: string;
  AMarkFeedItemsAsNotSeen: TMarkFeedItemsAsNotSeenProc;
  ASaveFeedItem: TSaveFeedItemProc;
  out IsYouTubeFeed: Boolean);
var
  Doc: TXMLDocument;
  ItemNode, ChildNode: TDOMNode;
  Title, Link, Description, PubDate, VideoId, GuidText, IdText, ItemKey: string;
  Stream: TStringStream;
begin
  Stream := TStringStream.Create(Response);
  try
    ReadXMLFile(Doc, Stream);
    try
      IsYouTubeFeed := False;
      if Assigned(Doc.DocumentElement) then
      begin
        if (Doc.DocumentElement.FindNode('yt:channelId') <> nil) or
           (Pos('youtube.com', AURL) > 0) then
          IsYouTubeFeed := True;
      end;

      if Assigned(AMarkFeedItemsAsNotSeen) then
        AMarkFeedItemsAsNotSeen(AURL);

      ItemNode := Doc.DocumentElement.FindNode('channel');
      if not Assigned(ItemNode) then
        ItemNode := Doc.DocumentElement;

      ItemNode := ItemNode.FirstChild;
      while Assigned(ItemNode) do
      begin
        if ItemNode.NodeName = 'item' then
        begin
          Title := '';
          Link := '';
          Description := '';
          PubDate := '';
          GuidText := '';
          IdText := '';

          ChildNode := ItemNode.FirstChild;
          while Assigned(ChildNode) do
          begin
            if ChildNode.NodeName = 'title' then
              Title := ChildNode.TextContent
            else if ChildNode.NodeName = 'link' then
              Link := ChildNode.TextContent
            else if (ChildNode.NodeName = 'description') or
                    (ChildNode.NodeName = 'content:encoded') then
              Description := ChildNode.TextContent
            else if ChildNode.NodeName = 'pubDate' then
              PubDate := ChildNode.TextContent
            else if ChildNode.NodeName = 'guid' then
              GuidText := ChildNode.TextContent;

            ChildNode := ChildNode.NextSibling;
          end;

          ItemKey := MakeItemKey(GuidText, IdText, Link, Title, PubDate);

          if Assigned(ASaveFeedItem) then
          begin
            if IsYouTubeFeed then
              ASaveFeedItem(AURL, ItemKey, Title, PubDate, Link, Link)
            else
              ASaveFeedItem(AURL, ItemKey, Title, PubDate, Description, Link);
          end;
        end
        else if ItemNode.NodeName = 'entry' then
        begin
          Title := '';
          Link := '';
          Description := '';
          PubDate := '';
          VideoId := '';
          GuidText := '';
          IdText := '';

          ChildNode := ItemNode.FirstChild;
          while Assigned(ChildNode) do
          begin
            if ChildNode.NodeName = 'title' then
              Title := ChildNode.TextContent
            else if ChildNode.NodeName = 'link' then
            begin
              if Assigned(ChildNode.Attributes) and (ChildNode.Attributes.GetNamedItem('rel') <> nil) then
              begin
                if ChildNode.Attributes.GetNamedItem('rel').NodeValue = 'alternate' then
                begin
                  if ChildNode.Attributes.GetNamedItem('href') <> nil then
                    Link := ChildNode.Attributes.GetNamedItem('href').NodeValue;
                end;
              end
              else if Assigned(ChildNode.Attributes) and (ChildNode.Attributes.GetNamedItem('href') <> nil) then
                Link := ChildNode.Attributes.GetNamedItem('href').NodeValue;
            end
            else if ChildNode.NodeName = 'yt:videoId' then
              VideoId := ChildNode.TextContent
            else if (ChildNode.NodeName = 'summary') or
                    (ChildNode.NodeName = 'content') then
              Description := ChildNode.TextContent
            else if (ChildNode.NodeName = 'published') or
                    (ChildNode.NodeName = 'updated') then
              PubDate := ChildNode.TextContent
            else if ChildNode.NodeName = 'id' then
              IdText := ChildNode.TextContent;

            ChildNode := ChildNode.NextSibling;
          end;

          if IsYouTubeFeed and (Link = '') and (VideoId <> '') then
            Link := 'https://www.youtube.com/watch?v=' + VideoId;

          ItemKey := MakeItemKey(GuidText, IdText, Link, Title, PubDate);

          if Assigned(ASaveFeedItem) then
          begin
            if IsYouTubeFeed then
              ASaveFeedItem(AURL, ItemKey, Title, PubDate, Link, Link)
            else
              ASaveFeedItem(AURL, ItemKey, Title, PubDate, Description, Link);
          end;
        end;

        ItemNode := ItemNode.NextSibling;
      end;
    finally
      Doc.Free;
    end;
  finally
    Stream.Free;
  end;
end;

end.
