program FeedRefreshUITests;

{$mode objfpc}{$H+}

uses
  cthreads, Interfaces, Forms, Classes, SysUtils, Controls, StdCtrls, ComCtrls,
  Menus, ExtCtrls, Gtk2, dbf, MainForm, FeedDbUtils, RssUtils;

type
  TPopupAction = class
    Timer: TTimer;
    Menu: TPopupMenu;
    Caption: string;
    procedure Activate(Sender: TObject);
  end;

  TNavigationProbe = class
    Timer: TTimer;
    Tree: TTreeView;
    FirstNode, SecondNode: TTreeNode;
    LastTick, MaxGap, MaxSelectionTime: QWord;
    Clicks: Integer;
    procedure Tick(Sender: TObject);
  end;

procedure Check(ACondition: Boolean; const AMessage: string);
begin
  if not ACondition then raise Exception.Create(AMessage);
end;

function FindMenu(AMenu: TPopupMenu; const ACaption: string): TMenuItem; forward;

procedure TPopupAction.Activate(Sender: TObject);
begin
  Timer.Enabled := False;
  gtk_menu_shell_activate_item(PGtkMenuShell(Menu.Handle),
    PGtkWidget(FindMenu(Menu, Caption).Handle), True);
end;

procedure RefreshFromPopup(AMenu: TPopupMenu; const ACaption: string);
var
  Action: TPopupAction;
  Started: QWord;
begin
  Action := TPopupAction.Create;
  try
    Action.Menu := AMenu;
    Action.Caption := ACaption;
    Action.Timer := TTimer.Create(nil);
    Action.Timer.Interval := 20;
    Action.Timer.OnTimer := @Action.Activate;
    Started := GetTickCount64;
    AMenu.PopUp(100, 100);
    Check(GetTickCount64 - Started < 500, 'The native popup must close before the refresh finishes');
    Check(gtk_grab_get_current <> PGtkWidget(AMenu.Handle), 'The popup must release its input grab');
  finally
    Action.Timer.Free;
    Action.Free;
  end;
end;

procedure TNavigationProbe.Tick(Sender: TObject);
var
  Started, Gap: QWord;
begin
  Started := GetTickCount64;
  Gap := Started - LastTick;
  if Gap > MaxGap then MaxGap := Gap;
  LastTick := Started;
  if Tree.Selected = FirstNode then Tree.Selected := SecondNode
  else Tree.Selected := FirstNode;
  Gap := GetTickCount64 - Started;
  if Gap > MaxSelectionTime then MaxSelectionTime := Gap;
  Inc(Clicks);
end;

function FindMenu(AMenu: TPopupMenu; const ACaption: string): TMenuItem;
var
  I: Integer;
begin
  for I := 0 to AMenu.Items.Count - 1 do
    if AMenu.Items[I].Caption = ACaption then Exit(AMenu.Items[I]);
  raise Exception.Create('Menu not found: ' + ACaption);
end;

procedure ClickItem(AList: TListView; AItem: TListItem);
var
  X, Y: Integer;
begin
  Application.ProcessMessages;
  for Y := 0 to AList.ClientHeight - 1 do
    for X := 1 to 10 do
      if AList.GetItemAt(X * 10, Y) = AItem then
      begin
        AList.OnMouseDown(AList, mbLeft, [], X * 10, Y);
        Exit;
      end;
  raise Exception.Create('Could not hit-test the cached article');
end;

var
  Tree: TTreeView;
  List: TListView;
  Status: TLabel;
  RefreshButton: TButton;
  Popup: TPopupMenu;
  SlowNode, OutsideNode, FolderNode, LargeNode: TTreeNode;
  ReadDb, ItemsDb: TDbf;
  I, OriginalCount: Integer;
  BaseURL: string;
  Deadline: QWord;
  Component: TComponent;
  Probe: TNavigationProbe;

procedure RunTests;
begin
  BaseURL := ParamStr(1);
  Check((ParamStr(2) <> '') and
    (GetUserDir = IncludeTrailingPathDelimiter(ParamStr(2))),
    'UI tests require an explicitly supplied isolated HOME');
  FeedDbUtils.InitializeDatabases(ReadDb, ItemsDb, TomarDataDir);
  try
    FeedDbUtils.SaveOrUpdateFeedItem(ItemsDb, BaseURL + '/slow', 'old', 'Old item',
      'Thu, 01 Oct 2026 12:00:00 GMT', 'Cached content', BaseURL + '/old');
    FeedDbUtils.SaveOrUpdateFeedItem(ItemsDb, BaseURL + '/outside', 'outside', 'Outside item',
      'Thu, 01 Oct 2026 12:00:00 GMT', 'Outside cached content', BaseURL + '/outside-item');
    FeedDbUtils.SaveOrUpdateFeedItem(ItemsDb, BaseURL + '/broken', 'cached', 'Cached item',
      'Thu, 01 Oct 2026 12:00:00 GMT', 'Cached content', BaseURL + '/cached');
    // Match a realistically sized cache, not just a handful of fixture posts.
    for I := 1 to 41000 do
      FeedDbUtils.SaveOrUpdateFeedItem(ItemsDb, BaseURL + '/archive' + IntToStr(I mod 100),
        'archive' + IntToStr(I), 'Archived item', '', 'Cached content', '', 0);
    for I := 0 to 1999 do
      FeedDbUtils.SaveOrUpdateFeedItem(ItemsDb, BaseURL + '/large',
        'large' + IntToStr(I), 'Cached large item', '', 'Cached content', '', 0);
    for I := 1 to 3345 do
    begin
      ReadDb.Append;
      ReadDb.FieldByName('FEEDURL').AsString := BaseURL + '/archive' + IntToStr(I mod 100);
      ReadDb.FieldByName('ITEMLINK').AsString := 'archive' + IntToStr(I);
      ReadDb.FieldByName('ITEMHASH').AsString := IntToHex(I, 32);
      ReadDb.FieldByName('ISREAD').AsBoolean := True;
      ReadDb.Post;
    end;
  finally
    ItemsDb.Free;
    ReadDb.Free;
  end;

  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CaptureExceptions := False;
  Application.CreateForm(TFormMain, FormMain);
  try
    Tree := nil;
    List := nil;
    Status := nil;
    RefreshButton := nil;
    for I := 0 to FormMain.ComponentCount - 1 do
    begin
      Component := FormMain.Components[I];
      if Component is TTreeView then Tree := TTreeView(Component);
      if Component is TListView then List := TListView(Component);
      if Component is TLabel then Status := TLabel(Component);
      if (Component is TButton) and (TButton(Component).Hint = 'Refresh all feeds') then
        RefreshButton := TButton(Component);
    end;
    Check(Assigned(Tree) and Assigned(List) and Assigned(Status) and Assigned(RefreshButton),
      'Refresh controls must exist');
    FormMain.Show;
    Application.ProcessMessages;
    Popup := Tree.PopupMenu;
    FolderNode := Tree.Items[0];
    SlowNode := Tree.Items[1];
    OutsideNode := Tree.Items[4];
    LargeNode := Tree.Items[5];
    Tree.Selected := FolderNode;
    RefreshFromPopup(Popup, 'Refresh Feeds');
    Check(not RefreshButton.Enabled, 'Toolbar refresh must be disabled immediately');
    Popup.OnPopup(Popup);
    for I := 0 to Popup.Items.Count - 1 do
      if Popup.Items[I].Caption <> '-' then
        Check(not Popup.Items[I].Enabled, 'Tree actions must be disabled during refresh');

    OriginalCount := Tree.Items.Count;
    // Direct invocation must be guarded as well as disabled in the menu.
    FindMenu(Popup, 'Add Feed').OnClick(nil);
    FindMenu(Popup, 'Add Folder').OnClick(nil);
    FindMenu(Popup, 'Delete').OnClick(nil);
    FindMenu(Popup, 'Move Up').OnClick(nil);
    FindMenu(Popup, 'Refresh All Feeds').OnClick(nil);
    Check(Tree.Items.Count = OriginalCount, 'Busy handlers must not change the tree');

    Tree.Selected := OutsideNode;
    Check(List.Items.Count = 1, 'Cached feeds must remain readable');
    Tree.Selected := SlowNode;
    Check(List.Items.Count = 1, 'Cached items remain while the request is running');
    List.Selected := List.Items[0];
    ClickItem(List, List.Items[0]);
    Check(List.Items[0].Data = nil, 'Reading during refresh must mark the item read');

    Deadline := GetTickCount64 + 10000;
    while not RefreshButton.Enabled do
    begin
      Check(GetTickCount64 < Deadline, 'UI refresh did not finish');
      Application.ProcessMessages;
      Sleep(10);
    end;
    Check(Pos('Refreshed 2 feeds.', Status.Caption) = 1, 'Success count must be accurate');
    Check(Pos('1 failed.', Status.Caption) > 0, 'Failure count must be visible');
    Check(Pos('/broken', Status.Hint) > 0, 'Failure details must be available');
    Check(List.Items.Count = 2, 'New items must appear in the selected feed');
    Check(Assigned(List.Selected) and (List.Selected.SubItems[3] = 'old'),
      'Refresh must preserve the selected article');
    Check(List.Selected.Data = nil, 'Refresh must not undo read status');
    Popup.OnPopup(Popup);
    Check(FindMenu(Popup, 'Add Feed').Enabled, 'Adding must be re-enabled');
    Check(FindMenu(Popup, 'Delete').Enabled, 'Deleting must be re-enabled');
    Check(FindMenu(Popup, 'Refresh Feed').Enabled, 'Refreshing must be re-enabled');

    RefreshFromPopup(Popup, 'Refresh Feed');
    Tree.Selected := OutsideNode;
    Deadline := GetTickCount64 + 10000;
    while not RefreshButton.Enabled do
    begin
      Check(GetTickCount64 < Deadline, 'Single-feed refresh did not finish');
      Application.ProcessMessages;
      Sleep(10);
    end;
    Check((List.Items.Count = 1) and (List.Items[0].SubItems[3] = 'outside'),
      'A completed request must not replace another selected feed');

    Tree.Selected := LargeNode;
    FindMenu(Popup, 'Refresh Feed').OnClick(nil);
    Tree.Selected := OutsideNode;
    Probe := TNavigationProbe.Create;
    try
      Probe.Tree := Tree;
      Probe.FirstNode := SlowNode;
      Probe.SecondNode := OutsideNode;
      Probe.LastTick := GetTickCount64;
      Probe.Timer := TTimer.Create(nil);
      Probe.Timer.Interval := 25;
      Probe.Timer.OnTimer := @Probe.Tick;
      Deadline := GetTickCount64 + 30000;
      while not RefreshButton.Enabled do
      begin
        Check(GetTickCount64 < Deadline, 'Large-feed refresh did not finish');
        Application.ProcessMessages;
        Sleep(5);
      end;
      Probe.Tick(nil);
      Check(Probe.Clicks >= 3, 'Navigation must run while refresh results are being saved');
      Check(Probe.MaxGap < 500, 'Saving a large feed must not monopolize the UI thread');
      Check(Probe.MaxSelectionTime < 500, 'Selecting a cached feed must remain responsive');
      WriteLn('large-cache UI heartbeat: ', Probe.MaxGap, ' ms; feed selection: ',
        Probe.MaxSelectionTime, ' ms');
    finally
      Probe.Timer.Free;
      Probe.Free;
    end;

    // Start another slow request, then close without processing any UI results.
    Tree.Selected := SlowNode;
    FindMenu(Popup, 'Refresh Feed').OnClick(nil);
    Sleep(50);
  finally
    FreeAndNil(FormMain);
  end;

  FeedDbUtils.InitializeDatabases(ReadDb, ItemsDb, TomarDataDir);
  try
    Check(FeedDbUtils.IsItemRead(ReadDb, BaseURL + '/slow', 'old'), 'Read status must persist');
    Check(FeedDbUtils.GetUnreadCount(ItemsDb, ReadDb, BaseURL + '/slow') = 1,
      'Refreshing must not automatically read the new item');
    Check(FeedDbUtils.GetUnreadCount(ItemsDb, ReadDb, BaseURL + '/broken') = 1,
      'A failed refresh must keep cached articles');
    Check(FeedDbUtils.GetUnreadCount(ItemsDb, ReadDb, BaseURL + '/large') = 4000,
      'Sliced updates must preserve existing items and append new ones exactly once');
    FeedDbUtils.MarkAllFeedItemsAsRead(ItemsDb, ReadDb, BaseURL + '/slow');
    Check(FeedDbUtils.GetUnreadCount(ItemsDb, ReadDb, BaseURL + '/slow') = 0,
      'Mark all read must operate on cached items');
  finally
    ItemsDb.Free;
    ReadDb.Free;
  end;
  WriteLn('background refresh UI tests passed');
end;

begin
  try
    RunTests;
  except
    on E: Exception do
    begin
      WriteLn(StdErr, E.ClassName, ': ', E.Message);
      DumpExceptionBackTrace(StdErr);
      Halt(1);
    end;
  end;
end.
