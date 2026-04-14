unit FeedModel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TFeedNodeData = class
    FeedURL: string;
    IsFolder: Boolean;
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
