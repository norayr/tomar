program FeedRefreshTests;

{$mode objfpc}{$H+}

uses
  cthreads, Classes, SysUtils, FeedModel, FeedRefreshThread;

procedure Check(ACondition: Boolean; const AMessage: string);
begin
  if not ACondition then raise Exception.Create(AMessage);
end;

var
  URLs: TStringList;
  Worker: TFeedRefreshThread;
  FeedResult: TFeedRefreshResult;
  BaseURL: string;
  Deadline: QWord;
  Successes, Failures: Integer;
  WorkerDone, ReceivedWhileRunning: Boolean;
begin
  BaseURL := ParamStr(1);
  URLs := TStringList.Create;
  try
    URLs.Add(BaseURL + '/slow');
    URLs.Add(BaseURL + '/broken');
    URLs.Add(BaseURL + '/atom');
    URLs.Add(BaseURL + '/missing');
    URLs.Add(BaseURL + '/empty');
    URLs.Add(BaseURL + '/slow-last');
    URLs.Add(BaseURL + '/slow');
    Worker := TFeedRefreshThread.Create(URLs);
    try
      Check(Worker.TotalFeeds = 6, 'Duplicate URLs must be fetched only once');
      URLs.Clear;
      Worker.Start;
      Sleep(50);
      Check(not Worker.Done, 'Starting a slow refresh must not wait for the request');
      Successes := 0;
      Failures := 0;
      ReceivedWhileRunning := False;
      Deadline := GetTickCount64 + 10000;
      repeat
        Check(GetTickCount64 < Deadline, 'Refresh did not finish');
        WorkerDone := Worker.Done;
        FeedResult := Worker.TakeResult;
        if not Assigned(FeedResult) then
        begin
          if WorkerDone then Break;
          Sleep(10);
          Continue;
        end;
        if not WorkerDone then ReceivedWhileRunning := True;
        try
          if FeedResult.ErrorMessage <> '' then
            Inc(Failures)
          else
          begin
            Inc(Successes);
            if FeedResult.FeedURL = BaseURL + '/slow' then
            begin
              Check(FeedResult.Items.Count = 2, 'RSS items must be parsed in the worker');
              Check(TFeedListEntry(FeedResult.Items[0]).ItemKeyValue = 'old', 'RSS item key');
            end
            else if FeedResult.FeedURL = BaseURL + '/atom' then
            begin
              Check(FeedResult.Items.Count = 1, 'Atom item must be parsed');
              Check(TFeedListEntry(FeedResult.Items[0]).PubDate = '2026-10-01T12:00:00Z',
                'Publication date must win over updated date');
            end
            else if FeedResult.FeedURL = BaseURL + '/empty' then
              Check(FeedResult.Items.Count = 0, 'An empty feed is still a successful refresh');
          end;
        finally
          FeedResult.Free;
        end;
      until False;
      Check(Successes = 4, 'Successful feeds must survive individual failures');
      Check(Failures = 2, 'Malformed XML and HTTP errors must be reported');
      Check(ReceivedWhileRunning, 'Results must be available before the entire batch finishes');
      Check(Worker.ErrorMessage = '', 'No unexpected worker failure');
    finally
      Worker.Free;
    end;

    URLs.Add(BaseURL + '/slow');
    Worker := TFeedRefreshThread.Create(URLs);
    Worker.Free; // Closing before Start must also be safe.

    Worker := TFeedRefreshThread.Create(URLs);
    Worker.Start;
    Sleep(50);
    Worker.Terminate;
    Worker.Free; // Joining during a request must not need a UI callback.
    WriteLn('background refresh tests passed');
  finally
    URLs.Free;
  end;
end.
