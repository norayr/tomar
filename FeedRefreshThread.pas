unit FeedRefreshThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, SyncObjs, FeedModel;

type
  TFeedRefreshResult = class
  private
    procedure AddItem(const AFeedURL, AItemKey, ATitle, APubDate, AContent, ALink: string);
  public
    FeedURL: string;
    IsYouTubeFeed: Boolean;
    ErrorMessage: string;
    Items: TObjectList;
    constructor Create(const AFeedURL: string);
    destructor Destroy; override;
  end;

  TFeedRefreshThread = class(TThread)
  private
    FURLs: TStringList;
    FResults: TObjectList;
    FLock: TCriticalSection;
    FDone: Boolean;
    FErrorMessage: string;
    function GetDone: Boolean;
    function GetTotalFeeds: Integer;
    function GetErrorMessage: string;
    procedure DownloadProgress(Sender: TObject; const ContentLength, CurrentPos: Int64);
  protected
    procedure Execute; override;
  public
    constructor Create(AURLs: TStrings);
    destructor Destroy; override;
    // Ownership of a completed, immutable result passes to the caller.
    function TakeResult: TFeedRefreshResult;
    property Done: Boolean read GetDone;
    property TotalFeeds: Integer read GetTotalFeeds;
    property ErrorMessage: string read GetErrorMessage;
  end;

implementation

uses
  fphttpclient, opensslsockets, FeedFetchUtils;

constructor TFeedRefreshResult.Create(const AFeedURL: string);
begin
  inherited Create;
  FeedURL := AFeedURL;
  Items := TObjectList.Create(True);
end;

destructor TFeedRefreshResult.Destroy;
begin
  Items.Free;
  inherited Destroy;
end;

procedure TFeedRefreshResult.AddItem(const AFeedURL, AItemKey, ATitle, APubDate,
  AContent, ALink: string);
var
  Item: TFeedListEntry;
begin
  Item := TFeedListEntry.Create;
  Item.ItemKeyValue := AItemKey;
  Item.Title := ATitle;
  Item.PubDate := APubDate;
  Item.ContentValue := AContent;
  Item.LinkValue := ALink;
  Items.Add(Item);
end;

constructor TFeedRefreshThread.Create(AURLs: TStrings);
var
  I: Integer;
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FLock := TCriticalSection.Create;
  FResults := TObjectList.Create(True);
  FURLs := TStringList.Create;
  for I := 0 to AURLs.Count - 1 do
    if (AURLs[I] <> '') and (FURLs.IndexOf(AURLs[I]) < 0) then
      FURLs.Add(AURLs[I]);
end;

destructor TFeedRefreshThread.Destroy;
begin
  // TThread.Destroy terminates and joins the worker before its data is freed.
  // No synchronized UI callbacks are used, so closing cannot deadlock on one.
  inherited Destroy;
  FResults.Free;
  FURLs.Free;
  FLock.Free;
end;

procedure TFeedRefreshThread.DownloadProgress(Sender: TObject;
  const ContentLength, CurrentPos: Int64);
begin
  if Terminated then
    raise EAbort.Create('Feed refresh cancelled');
end;

procedure TFeedRefreshThread.Execute;
var
  I: Integer;
  Client: TFPHTTPClient;
  FeedResult: TFeedRefreshResult;
  Response: string;
begin
  try
    try
      for I := 0 to FURLs.Count - 1 do
      begin
        if Terminated then Exit;
        FeedResult := TFeedRefreshResult.Create(FURLs[I]);
        try
          Client := nil;
          try
            try
              // This client and parser belong exclusively to the worker.
              Client := TFPHTTPClient.Create(nil);
              Client.AllowRedirect := True;
              Client.ConnectTimeout := 10000;
              Client.IOTimeout := 15000;
              Client.OnDataReceived := @DownloadProgress;
              Response := Client.Get(FeedResult.FeedURL);
              if Terminated then Exit;
              ParseFeedResponse(FeedResult.FeedURL, Response, nil,
                @FeedResult.AddItem, FeedResult.IsYouTubeFeed);
            except
              on E: Exception do
                FeedResult.ErrorMessage := E.Message;
            end;
          finally
            Client.Free;
          end;

          if Terminated then Exit;
          FLock.Acquire;
          try
            FResults.Add(FeedResult);
            FeedResult := nil;
          finally
            FLock.Release;
          end;
        finally
          FeedResult.Free;
        end;
      end;
    except
      on E: Exception do
      begin
        FLock.Acquire;
        try
          FErrorMessage := E.Message;
        finally
          FLock.Release;
        end;
      end;
    end;
  finally
    FLock.Acquire;
    try
      FDone := True;
    finally
      FLock.Release;
    end;
  end;
end;

function TFeedRefreshThread.TakeResult: TFeedRefreshResult;
begin
  Result := nil;
  FLock.Acquire;
  try
    if FResults.Count > 0 then
      Result := TFeedRefreshResult(FResults.Extract(FResults[0]));
  finally
    FLock.Release;
  end;
end;

function TFeedRefreshThread.GetDone: Boolean;
begin
  FLock.Acquire;
  try
    Result := FDone;
  finally
    FLock.Release;
  end;
end;

function TFeedRefreshThread.GetTotalFeeds: Integer;
begin
  Result := FURLs.Count;
end;

function TFeedRefreshThread.GetErrorMessage: string;
begin
  FLock.Acquire;
  try
    Result := FErrorMessage;
  finally
    FLock.Release;
  end;
end;

end.
