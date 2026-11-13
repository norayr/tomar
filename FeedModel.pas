unit FeedModel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TFeedNodeData = class
    FeedURL: string;
    IsFolder: Boolean;
    CollectionKind: Integer; // 0 = subscription, 1 = Watched, 2 = Favorites
  end;

  TFeedListEntry = class
    Title: string;
    PubDate: string;
    ContentValue: string;
    LinkValue: string;
    ItemKeyValue: string;
    Unread: Boolean;
    SortKey: string;
  end;

implementation

end.
