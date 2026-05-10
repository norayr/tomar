unit MainForm;

{$mode objfpc}{$H+}

{ Uncomment to enable developer/debug UI & logging }
{.$DEFINE RSSREADER_DEBUG}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, Menus, Process, DOM, XMLRead, XMLWrite, fphttpclient, IpHtml, ipmsg, opensslsockets,
  FPImage, FPReadPNG, FPReadJPEG, FPReadGIF, db, dbf, md5, Clipbrd, DateUtils, HtmlProvider, FeedFetchUtils, FeedModel, FeedConfigUtils, FeedListUtils
  {$IFDEF LCLGTK2}
  , x, Gtk2, Gdk2, Gdk2x, xatom
  {$ENDIF};

type
  { TFormMain }

  TFormMain = class(TForm)
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FTreeView: TTreeView;
    FListView: TListView;
    FHtmlPanel: TIpHtmlPanel;
    FHtmlPopup: TPopupMenu;
    FWatchPanel: TPanel;
    FWatchButton: TButton;
    FMpvProcess: TProcess;
    FMpvTimer: TTimer;
    FCurrentURL: string; // to track the link under mouse
    FCurrentVideoURL: string;
    FYoutubePlayerAvailable: Boolean;

    FLeftPanel, FRightPanel, FTopPanel, FBottomPanel: TPanel;
    FWinPropertySet: Boolean;
    FInResize: Boolean;
    FLastPortraitLayout: Boolean;
    FLayoutInitialized: Boolean;

    FLongPressTimer: TTimer;
    FLongPressPoint: TPoint;
    FLongPressControl: TControl;  // Track which control triggered long press

    FSplitter1, FSplitter2: TSplitter;
    FPopupMenu: TPopupMenu;
    FHttpClient: TFPHTTPClient;
    FDataProvider: TCustomHtmlDataProvider;
    FReadStatusDb: TDbf;
    FFeedItemsDb: TDbf;
    FLoadingFeed: Boolean; // Flag to prevent selection during loading
    FInSelectItem: Boolean;
{$IFDEF RSSREADER_DEBUG}
    FDebugLog: TStringList;  // Debug log (optional)
{$ENDIF}

    procedure CreateControls;
    procedure UpdateLayout;
    procedure InitializeDatabase;
    procedure DebugLog(const S: string);
    procedure TreeViewSelectionChanged(Sender: TObject);
    procedure ListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure ListViewCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure TreeViewPopup(Sender: TObject);

    // TreeView events
    procedure TreeViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TreeViewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TreeViewMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure LongPressTimerTick(Sender: TObject);

    procedure HtmlPanelHotURL(Sender: TObject; const URL: string);
    procedure HtmlPanelContextPopup(Sender: TObject; MousePos: TPoint;
       var Handled: Boolean);
    procedure MenuCopyLinkClick(Sender: TObject);
    procedure WatchButtonClick(Sender: TObject);
    procedure MpvTimerTick(Sender: TObject);

    procedure MenuAddFolderClick(Sender: TObject);
    procedure MenuAddFeedClick(Sender: TObject);
    procedure MenuDeleteClick(Sender: TObject);
    procedure MenuRefreshClick(Sender: TObject);
    procedure MenuRefreshFeedsClick(Sender: TObject);
    procedure MenuRefreshAllClick(Sender: TObject);
    procedure MenuMarkAllReadClick(Sender: TObject);
    procedure MenuShowDebugLogClick(Sender: TObject);
    procedure ListViewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure RefreshFeedAction(const AFeedURL: string);
    procedure MarkFeedReadAction(const AFeedURL: string);

    procedure LoadFeedList;
    procedure SaveFeedList;
    procedure LoadRSSFeed(const AURL: string);
    procedure LoadFeedItemsFromDb(const AURL: string; IsYouTubeFeed: Boolean);
    procedure MarkFeedItemsAsNotSeen(const AURL: string);
    procedure SaveOrUpdateFeedItem(const AFeedURL, AItemKey, ATitle, APubDate, AContent, ALink: string);
    function MakeItemKey(const AGuid, AId, ALink, ATitle, APubDate: string): string;
    function GetSelectedNodeData: TFeedNodeData;
    procedure FreeNodeData(Node: TTreeNode);

    function ComputeItemHash(const AFeedURL, AItemKey: string): string;
    function IsItemRead(const AFeedURL, AItemKey: string; const ALegacyLink: string = ''): Boolean;
    procedure MarkItemAsRead(const AFeedURL, AItemKey: string; const ALegacyLink: string = '');
    procedure MarkAllItemsAsRead(const AFeedURL: string);
    function GetUnreadCount(const AFeedURL: string): Integer;

    function ConvertYouTubeURLToFeed(const AUrl: string): string;
    function IsExecutableInPath(const ProgName: string): Boolean;
    function CanWatchYouTubeLocally: Boolean;
    procedure SetWatchButtonURL(const AURL: string);
  public

  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

uses
  LCLType, LCLIntf, BaseUnix, RssUtils, FeedDbUtils, FeedTreeUtils, FeedActionUtils;
  //FeedDBCleanup;


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

{ TFormMain }

procedure TFormMain.FormActivate(Sender: TObject);
{$IFDEF LCLGTK2}
var
  Widget: PGtkWidget;
  Atom: TGdkAtom;
  Value: Cardinal;
{$ENDIF}
begin
  {$IFDEF LCLGTK2}
  if FWinPropertySet then Exit;

  Widget := PGtkWidget(Handle);
  if (Widget <> nil) and (Widget^.window <> nil) then
  begin
    { Tell Hildon / Maemo Leste that this window supports portrait mode. }
    Atom := gdk_atom_intern('_HILDON_PORTRAIT_MODE_SUPPORT', False);
    Value := 1;

    if Atom <> 0 then
    begin
      gdk_property_change(Widget^.window, Atom,
        gdk_x11_xatom_to_atom(XA_CARDINAL), 32,
        GDK_PROP_MODE_REPLACE, @Value, 1);
      FWinPropertySet := True;
    end;
  end;
  {$ENDIF}
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  if FInResize then Exit;

  FInResize := True;
  try
    UpdateLayout;
  finally
    FInResize := False;
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  Duplicates: TStringList;
begin
  Caption := 'Տոմար';
  Width := 1000;
  Height := 700;
  Position := poScreenCenter;
  Constraints.MinWidth := 240;
  Constraints.MinHeight := 240;

  FWinPropertySet := False;
  FInResize := False;
  FLastPortraitLayout := False;
  FLayoutInitialized := False;
  OnActivate := @FormActivate;
  OnResize := @FormResize;

  FHttpClient := TFPHTTPClient.Create(nil);
  FHttpClient.AllowRedirect := True;

  FDataProvider := TCustomHtmlDataProvider.Create(Self);
  FLoadingFeed := False;
  FCurrentVideoURL := '';
  FYoutubePlayerAvailable := CanWatchYouTubeLocally;

{$IFDEF RSSREADER_DEBUG}
  FDebugLog := TStringList.Create;
{$ENDIF}

  InitializeDatabase;

  // test
  //Duplicates := CheckForDuplicateFeeds(CONFIG_FILE);
  //try
  //  if Duplicates.Count > 0 then
  //    ShowMessage('Duplicate feeds found: ' + Duplicates.CommaText);
  //finally
  //  Duplicates.Free;
  //end;

  //CleanupOrphanedReadStatus(CONFIG_FILE, FReadStatusDb);
  // end of test cleanup
  CreateControls;
  LoadFeedList;
  FeedTreeUtils.UpdateAllFeedNodeTexts(FTreeView, FFeedItemsDb, FReadStatusDb);
end;

procedure TFormMain.FormDestroy(Sender: TObject);
var
  i: Integer;
  Node: TTreeNode;
begin
  // Free all node data
  if Assigned(FTreeView) then
  begin
    for i := 0 to FTreeView.Items.Count - 1 do
    begin
      Node := FTreeView.Items[i];
      if Assigned(Node.Data) then
      begin
        TFeedNodeData(Node.Data).Free;
        Node.Data := nil;
      end;
    end;
  end;

  if Assigned(FReadStatusDb) then
  begin
    if FReadStatusDb.Active then
      FReadStatusDb.Close;
    FReadStatusDb.Free;
  end;

  if Assigned(FFeedItemsDb) then
  begin
    if FFeedItemsDb.Active then
      FFeedItemsDb.Close;
    FFeedItemsDb.Free;
  end;

{$IFDEF RSSREADER_DEBUG}
  FreeAndNil(FDebugLog);
{$ENDIF}

  FreeAndNil(FMpvTimer);
  FreeAndNil(FMpvProcess);

  FHttpClient.Free;
end;

procedure TFormMain.CreateControls;
var
  LeftToolbar: TPanel;
  BtnAddFeed, BtnAddFolder, BtnRefreshAll: TButton;
  MenuItem: TMenuItem;
begin
  // Left Panel with TreeView
  FLeftPanel := TPanel.Create(Self);
  FLeftPanel.Parent := Self;
  FLeftPanel.Align := alLeft;
  { Initial size is calculated by UpdateLayout from the current window size. }
  FLeftPanel.Width := 1;
  FLeftPanel.Caption := '';
  FLeftPanel.BevelOuter := bvNone;
  FLeftPanel.Constraints.MinWidth := 120;

  // Touch-friendly toolbar for empty/narrow tree pane.
  LeftToolbar := TPanel.Create(Self);
  LeftToolbar.Parent := FLeftPanel;
  LeftToolbar.Align := alTop;
  LeftToolbar.Height := 42;
  LeftToolbar.Caption := '';
  LeftToolbar.BevelOuter := bvNone;

  BtnAddFeed := TButton.Create(Self);
  BtnAddFeed.Parent := LeftToolbar;
  BtnAddFeed.Align := alLeft;
  BtnAddFeed.Width := 58;
  BtnAddFeed.Caption := '＋📰';
  BtnAddFeed.Hint := 'Add feed';
  BtnAddFeed.ShowHint := True;
  BtnAddFeed.OnClick := @MenuAddFeedClick;

  BtnAddFolder := TButton.Create(Self);
  BtnAddFolder.Parent := LeftToolbar;
  BtnAddFolder.Align := alLeft;
  BtnAddFolder.Width := 58;
  BtnAddFolder.Caption := '＋📁';
  BtnAddFolder.Hint := 'Add folder';
  BtnAddFolder.ShowHint := True;
  BtnAddFolder.OnClick := @MenuAddFolderClick;

  BtnRefreshAll := TButton.Create(Self);
  BtnRefreshAll.Parent := LeftToolbar;
  BtnRefreshAll.Align := alLeft;
  BtnRefreshAll.Width := 48;
  BtnRefreshAll.Caption := '⟳';
  BtnRefreshAll.Hint := 'Refresh all feeds';
  BtnRefreshAll.ShowHint := True;
  BtnRefreshAll.OnClick := @MenuRefreshAllClick;

  FTreeView := TTreeView.Create(Self);
  FTreeView.Parent := FLeftPanel;
  FTreeView.Align := alClient;
  FTreeView.ReadOnly := True;
  FTreeView.OnSelectionChanged := @TreeViewSelectionChanged;
  FTreeView.PopupMenu := TPopupMenu.Create(Self);

  // For long press as right click
  FTreeView.OnMouseDown := @TreeViewMouseDown;
  FTreeView.OnMouseUp := @TreeViewMouseUp;
  FTreeView.OnMouseMove := @TreeViewMouseMove;

  // Create popup menu
  FPopupMenu := FTreeView.PopupMenu;

  MenuItem := TMenuItem.Create(FPopupMenu);
  MenuItem.Caption := 'Add Folder';
  MenuItem.OnClick := @MenuAddFolderClick;
  FPopupMenu.Items.Add(MenuItem);

  MenuItem := TMenuItem.Create(FPopupMenu);
  MenuItem.Caption := 'Add Feed';
  MenuItem.OnClick := @MenuAddFeedClick;
  FPopupMenu.Items.Add(MenuItem);

  MenuItem := TMenuItem.Create(FPopupMenu);
  MenuItem.Caption := '-';
  FPopupMenu.Items.Add(MenuItem);

  MenuItem := TMenuItem.Create(FPopupMenu);
  MenuItem.Caption := 'Refresh Feed';
  MenuItem.OnClick := @MenuRefreshClick;
  FPopupMenu.Items.Add(MenuItem);

  MenuItem := TMenuItem.Create(FPopupMenu);
  MenuItem.Caption := 'Refresh Feeds';
  MenuItem.OnClick := @MenuRefreshFeedsClick;
  FPopupMenu.Items.Add(MenuItem);

  MenuItem := TMenuItem.Create(FPopupMenu);
  MenuItem.Caption := 'Refresh All Feeds';
  MenuItem.OnClick := @MenuRefreshAllClick;
  FPopupMenu.Items.Add(MenuItem);

  MenuItem := TMenuItem.Create(FPopupMenu);
  MenuItem.Caption := 'Mark All as Read';
  MenuItem.OnClick := @MenuMarkAllReadClick;
  FPopupMenu.Items.Add(MenuItem);

  MenuItem := TMenuItem.Create(FPopupMenu);
  MenuItem.Caption := 'Delete';
  MenuItem.OnClick := @MenuDeleteClick;
  FPopupMenu.Items.Add(MenuItem);

  MenuItem := TMenuItem.Create(FPopupMenu);
  MenuItem.Caption := '-';
  FPopupMenu.Items.Add(MenuItem);
{$IFDEF RSSREADER_DEBUG}

  MenuItem := TMenuItem.Create(FPopupMenu);
  MenuItem.Caption := 'Show Debug Log';
  MenuItem.OnClick := @MenuShowDebugLogClick;
  FPopupMenu.Items.Add(MenuItem);
{$ENDIF}

  FTreeView.PopupMenu.OnPopup := @TreeViewPopup;

  // Splitter
  FSplitter1 := TSplitter.Create(Self);
  FSplitter1.Parent := Self;
  FSplitter1.Align := alLeft;
  FSplitter1.Width := 12;
  // Force the left/right splitter to sit after the tree pane, not before it.
  // With Align=alLeft, LCL uses Left to decide the order among left-aligned controls.
  FSplitter1.Left := FLeftPanel.Left + FLeftPanel.Width;
  FSplitter1.ResizeStyle := rsUpdate;
  FSplitter1.AutoSnap := False;
  FSplitter1.MinSize := 120;
  FSplitter1.Beveled := True;
  FSplitter1.Cursor := crHSplit;
  FSplitter1.Color := clMedGray;

  // Right Panel
  FRightPanel := TPanel.Create(Self);
  FRightPanel.Parent := Self;
  FRightPanel.Align := alClient;
  FRightPanel.Caption := '';
  FRightPanel.BevelOuter := bvNone;

  // Top panel with ListView
  FTopPanel := TPanel.Create(Self);
  FTopPanel.Parent := FRightPanel;
  FTopPanel.Align := alTop;
  { Initial size is calculated by UpdateLayout from the current window size. }
  FTopPanel.Height := 1;
  FTopPanel.Caption := '';
  FTopPanel.BevelOuter := bvNone;
  FTopPanel.Constraints.MinHeight := 90;

  FListView := TListView.Create(Self);
  FListView.Parent := FTopPanel;
  FListView.Align := alClient;
  //FListView.ViewStyle := vsReport;
  FListView.ViewStyle := vsList;
  FListView.RowSelect := True;
  FListView.ReadOnly := True;
  //FListView.OnSelectItem := @ListViewSelectItem;
  //FListView.OnClick := @ListViewClick;
  FListView.OnMouseDown := @ListViewMouseDown;

  //FListView.OwnerDraw := False;
  //FListView.OwnerDraw := True;
  FListView.OnCustomDrawItem := @ListViewCustomDrawItem;
  //FListView.OnCustomDrawItem := nil;
  FListView.HideSelection := False; // Ensure selection is visible
  FListView.MultiSelect := False; // Ensure only one item can be selected

  with FListView.Columns.Add do
  begin
    Caption := 'Title';
    Width := 400;
  end;
  with FListView.Columns.Add do
  begin
    Caption := 'Date';
    Width := 150;
  end;

  // Splitter 2
  FSplitter2 := TSplitter.Create(Self);
  FSplitter2.Parent := FRightPanel;
  FSplitter2.Align := alTop;
  FSplitter2.Height := 12;
  // Force the vertical splitter to sit below the post list / above the post view.
  // With Align=alTop, LCL uses Top to decide the order among top-aligned controls.
  FSplitter2.Top := FTopPanel.Top + FTopPanel.Height;
  FSplitter2.ResizeStyle := rsUpdate;
  FSplitter2.AutoSnap := False;
  FSplitter2.MinSize := 120;
  FSplitter2.Beveled := True;
  FSplitter2.Cursor := crVSplit;
  FSplitter2.Color := clMedGray;

  // Bottom panel with HTML viewer
  FBottomPanel := TPanel.Create(Self);
  FBottomPanel.Parent := FRightPanel;
  FBottomPanel.Align := alClient;
  FBottomPanel.Caption := '';
  FBottomPanel.BevelOuter := bvNone;
  FBottomPanel.Constraints.MinHeight := 90;

  FWatchPanel := TPanel.Create(Self);
  FWatchPanel.Parent := FBottomPanel;
  FWatchPanel.Align := alTop;
  FWatchPanel.Height := 42;
  FWatchPanel.Caption := '';
  FWatchPanel.BevelOuter := bvNone;
  FWatchPanel.Visible := False;

  FWatchButton := TButton.Create(Self);
  FWatchButton.Parent := FWatchPanel;
  FWatchButton.Align := alLeft;
  FWatchButton.Width := 110;
  FWatchButton.Caption := 'Watch';
  FWatchButton.OnClick := @WatchButtonClick;

  FMpvTimer := TTimer.Create(Self);
  FMpvTimer.Interval := 1000;
  FMpvTimer.Enabled := False;
  FMpvTimer.OnTimer := @MpvTimerTick;

  FHtmlPanel := TIpHtmlPanel.Create(Self);
  FHtmlPanel.Parent := FBottomPanel;
  FHtmlPanel.Align := alClient;
  FHtmlPanel.AllowTextSelect := True;
  FHtmlPanel.DataProvider := FDataProvider;

  FHtmlPanel.OnHotURL := @HtmlPanelHotURL;
  FHtmlPanel.OnContextPopup := @HtmlPanelContextPopup;
  FHtmlPopup := TPopupMenu.Create(Self);

  //long-press timer
  FLongPressTimer := TTimer.Create(Self);
  FLongPressTimer.Interval := 500;  // ms
  FLongPressTimer.Enabled := False;
  FLongPressTimer.OnTimer := @LongPressTimerTick;

  MenuItem := TMenuItem.Create(FHtmlPopup);
  MenuItem.Caption := 'Copy Link';
  MenuItem.OnClick := @MenuCopyLinkClick;
  FHtmlPopup.Items.Add(MenuItem);
  FHtmlPanel.PopupMenu := FHtmlPopup;

  UpdateLayout;
end;

procedure TFormMain.UpdateLayout;
const
  SplitSize = 12;
  ToolbarH = 42;
var
  IsPortrait: Boolean;
  TreeSize, ListSize, ContentSize: Integer;

  function ClampInt(AValue, AMin, AMax: Integer): Integer;
  begin
    Result := AValue;
    if Result < AMin then Result := AMin;
    if Result > AMax then Result := AMax;
  end;

begin
  if (FLeftPanel = nil) or (FRightPanel = nil) or
     (FTopPanel = nil) or (FBottomPanel = nil) or
     (FSplitter1 = nil) or (FSplitter2 = nil) then Exit;

  if (ClientWidth < 20) or (ClientHeight < 20) then Exit;

  IsPortrait := ClientHeight >= ClientWidth;

  { Do not undo a user's splitter drag on ordinary resizes.
    Reflow only at startup and when the window changes orientation. }
  if FLayoutInitialized and (FLastPortraitLayout = IsPortrait) then Exit;

  { Avoid autosnapping panes away on small phone screens. }
  FSplitter1.AutoSnap := False;
  FSplitter2.AutoSnap := False;

  if IsPortrait then
  begin
    { Phone / portrait: tree, post list, and post view are stacked vertically.
      Defaults are proportional to the current form size, not fixed pixels. }
    TreeSize := ClampInt((ClientHeight * 28) div 100,
      ToolbarH + 70, (ClientHeight * 45) div 100);

    FLeftPanel.Align := alTop;
    FLeftPanel.Height := TreeSize;
    FLeftPanel.Constraints.MinWidth := 0;
    FLeftPanel.Constraints.MinHeight := ToolbarH + 70;

    FSplitter1.Align := alTop;
    FSplitter1.Height := SplitSize;
    FSplitter1.Cursor := crVSplit;
    FSplitter1.MinSize := ToolbarH + 70;

    FRightPanel.Align := alClient;

    ContentSize := ClientHeight - TreeSize - SplitSize;
    if ContentSize < 1 then ContentSize := 1;
    { Post list and post view start near 50/50 in the remaining space. }
    ListSize := ClampInt((ContentSize - SplitSize) div 2,
      70, (ContentSize * 70) div 100);

    FTopPanel.Align := alTop;
    FTopPanel.Height := ListSize;
    FTopPanel.Constraints.MinHeight := 70;

    FSplitter2.Align := alTop;
    FSplitter2.Height := SplitSize;
    FSplitter2.Cursor := crVSplit;
    FSplitter2.MinSize := 70;

    FBottomPanel.Align := alClient;
  end
  else
  begin
    { Desktop / landscape: tree on the left, post list above post view.
      Tree starts at about 28% of the form width. }
    TreeSize := ClampInt((ClientWidth * 28) div 100,
      120, (ClientWidth * 45) div 100);

    FLeftPanel.Align := alLeft;
    FLeftPanel.Width := TreeSize;
    FLeftPanel.Constraints.MinWidth := 120;
    FLeftPanel.Constraints.MinHeight := 0;

    FSplitter1.Align := alLeft;
    FSplitter1.Width := SplitSize;
    FSplitter1.Cursor := crHSplit;
    FSplitter1.MinSize := 120;

    FRightPanel.Align := alClient;

    { Post list and post view start near 50/50 of the window height. }
    ListSize := ClampInt((ClientHeight - SplitSize) div 2,
      90, (ClientHeight * 70) div 100);

    FTopPanel.Align := alTop;
    FTopPanel.Height := ListSize;
    FTopPanel.Constraints.MinHeight := 90;

    FSplitter2.Align := alTop;
    FSplitter2.Height := SplitSize;
    FSplitter2.Cursor := crVSplit;
    FSplitter2.MinSize := 90;

    FBottomPanel.Align := alClient;
  end;

  { Keep splitter controls immediately after the pane they resize. }
  if IsPortrait then
  begin
    FLeftPanel.Top := 0;
    FSplitter1.Top := FLeftPanel.Top + FLeftPanel.Height;
  end
  else
  begin
    FLeftPanel.Left := 0;
    FSplitter1.Left := FLeftPanel.Left + FLeftPanel.Width;
  end;

  FTopPanel.Top := 0;
  FSplitter2.Top := FTopPanel.Top + FTopPanel.Height;

  FLastPortraitLayout := IsPortrait;
  FLayoutInitialized := True;
end;

// TreeView long-press handlers
procedure TFormMain.TreeViewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FLongPressPoint := Point(X, Y);
    FLongPressControl := FTreeView;
    FLongPressTimer.Enabled := True;
  end;
end;

procedure TFormMain.TreeViewMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FLongPressTimer.Enabled := False;
end;

procedure TFormMain.TreeViewMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if FLongPressTimer.Enabled and (FLongPressControl = FTreeView) then
  begin
    if (Abs(X - FLongPressPoint.X) > 10) or
       (Abs(Y - FLongPressPoint.Y) > 10) then
      FLongPressTimer.Enabled := False;
  end;
end;

// HtmlPanel long-press handlers
// Timer tick - trigger popup menu
procedure TFormMain.LongPressTimerTick(Sender: TObject);
var
  ScreenPoint: TPoint;
begin
  FLongPressTimer.Enabled := False;

  // Only handle TreeView long press
  if FLongPressControl = FTreeView then
  begin
    ScreenPoint := FTreeView.ClientToScreen(FLongPressPoint);
    if Assigned(FPopupMenu) then
      FPopupMenu.PopUp(ScreenPoint.X, ScreenPoint.Y);
  end;
end;

// copy link methods

procedure TFormMain.HtmlPanelHotURL(Sender: TObject; const URL: string);
begin
  FCurrentURL := URL;  // Track the URL under mouse
end;

procedure TFormMain.HtmlPanelContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin
  // Enable "Copy Link" only if over a link
  if Assigned(FHtmlPopup) and (FHtmlPopup.Items.Count > 0) then
    FHtmlPopup.Items[0].Enabled := (FCurrentURL <> '');
end;

procedure TFormMain.MenuCopyLinkClick(Sender: TObject);
begin
  if FCurrentURL <> '' then
    Clipboard.AsText := FCurrentURL;
end;

procedure TFormMain.WatchButtonClick(Sender: TObject);
begin
  if FCurrentVideoURL = '' then
    Exit;

  if Assigned(FMpvProcess) and FMpvProcess.Running then
    Exit;

  FreeAndNil(FMpvProcess);

  FMpvProcess := TProcess.Create(nil);
  try
    FMpvProcess.Executable := 'mpv';
    FMpvProcess.Parameters.Add('--ytdl=yes');
    FMpvProcess.Parameters.Add('--ytdl-format=18/best[height<=360]');
    FMpvProcess.Parameters.Add('--ytdl-raw-options=cookies-from-browser=firefox:/home/inky/.librewolf/vifu5p28.default-release::youtube');
    FMpvProcess.Parameters.Add('--cache=yes');
    FMpvProcess.Parameters.Add('--cache-secs=60');
    FMpvProcess.Parameters.Add(FCurrentVideoURL);
    FMpvProcess.Options := [];

    FWatchButton.Enabled := False;
    FMpvProcess.Execute;
    FMpvTimer.Enabled := True;
  except
    on E: Exception do
    begin
      FreeAndNil(FMpvProcess);
      FWatchButton.Enabled := True;
      ShowMessage('Could not start mpv: ' + E.Message);
    end;
  end;
end;

procedure TFormMain.MpvTimerTick(Sender: TObject);
begin
  if not Assigned(FMpvProcess) then
  begin
    FMpvTimer.Enabled := False;
    if Assigned(FWatchButton) then
      FWatchButton.Enabled := True;
    Exit;
  end;

  if not FMpvProcess.Running then
  begin
    FMpvTimer.Enabled := False;
    FreeAndNil(FMpvProcess);
    if Assigned(FWatchButton) then
      FWatchButton.Enabled := True;
  end;
end;

// end of copy link


function TFormMain.IsExecutableInPath(const ProgName: string): Boolean;
var
  PathEnv, Candidate: string;
  Parts: TStringList;
  I: Integer;
  AccessOK: Boolean;
begin
  Result := False;

  PathEnv := GetEnvironmentVariable('PATH');
  Parts := TStringList.Create;
  try
    ExtractStrings([':'], [], PChar(PathEnv), Parts);

    for I := 0 to Parts.Count - 1 do
    begin
      Candidate := IncludeTrailingPathDelimiter(Parts[I]) + ProgName;

      AccessOK := fpAccess(Candidate, X_OK) = 0;

      if FileExists(Candidate) and AccessOK then
      begin
        Result := True;
        Exit;
      end;
    end;

  finally
    Parts.Free;
  end;
end;

function TFormMain.CanWatchYouTubeLocally: Boolean;
begin
  Result := IsExecutableInPath('mpv') and IsExecutableInPath('yt-dlp');
end;

procedure TFormMain.SetWatchButtonURL(const AURL: string);
begin
  FCurrentVideoURL := AURL;
  if Assigned(FWatchPanel) then
    FWatchPanel.Visible := FYoutubePlayerAvailable and (FCurrentVideoURL <> '');

  if Assigned(FWatchButton) then
    FWatchButton.Enabled := not (Assigned(FMpvProcess) and FMpvProcess.Running);
end;

procedure TFormMain.ListViewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Item: TListItem;
  Content: string;
  VideoId: string;
  HtmlContent: string;
  PosStart, PosEnd: Integer;
  FeedURL, ItemLink, ItemKey: string;
  NodeData: TFeedNodeData;
begin
  // Only handle left mouse button
  if Button <> mbLeft then
    Exit;

  // Get the item at the mouse position
  Item := FListView.GetItemAt(X, Y);

  if Item = nil then
    Exit;

  // prevent double-trigger cascades
  if FInSelectItem then Exit;
  FInSelectItem := True;
  try
    // Manually select the item that was actually clicked
    FListView.Selected := Item;
    FListView.ItemFocused := Item;

    // Do not mark as read while loading feed
    if not FLoadingFeed then
    begin
      // Only process if item is currently marked as unread
      if Item.Data = Pointer(1) then
      begin
        // Mark item as read
        NodeData := GetSelectedNodeData;
        if Assigned(NodeData) and not NodeData.IsFolder then
        begin
          FeedURL := NodeData.FeedURL;
          if Item.SubItems.Count > 2 then
          begin
            ItemLink := Item.SubItems[2]; // The link is in SubItems[2]
            if Item.SubItems.Count > 3 then
              ItemKey := Item.SubItems[3]
            else
              ItemKey := ItemLink;
            MarkItemAsRead(FeedURL, ItemKey, ItemLink);
            Item.Data := nil; // Mark as read in ListView
              FListView.Invalidate;
              FListView.Update;
            // Update tree node text
            FeedTreeUtils.UpdateFeedNodeText(FTreeView.Selected, FFeedItemsDb, FReadStatusDb);
          end;
        end;
      end;
    end;

    // Item.SubItems[1] contains the content (description or YouTube URL)
    if Item.SubItems.Count > 1 then
    begin
      Content := Item.SubItems[1];

      // Determine current feed URL (needed for YouTube special-casing)
      NodeData := GetSelectedNodeData;
      if Assigned(NodeData) and not NodeData.IsFolder then
        FeedURL := NodeData.FeedURL
      else
        FeedURL := '';


      // Check if this is a YouTube URL in a YouTube feed (avoid special-casing blog posts that just mention YouTube)
      if IsYouTubeFeedURL(FeedURL) and (
         (Pos('youtube.com/watch?v=', Content) > 0) or
          (Pos('youtu.be/', Content) > 0) or
          (Pos('youtube.com/shorts/', Content) > 0)) then
      begin
        // Extract video ID
        VideoId := '';
        if Pos('youtube.com/watch?v=', Content) > 0 then
        begin
          PosStart := Pos('v=', Content) + 2;
          PosEnd := Pos('&', Content);
          if PosEnd = 0 then
            VideoId := Copy(Content, PosStart, Length(Content) - PosStart + 1)
          else
            VideoId := Copy(Content, PosStart, PosEnd - PosStart);
        end
        else if Pos('youtube.com/shorts/', Content) > 0 then
        begin
          PosStart := Pos('youtube.com/shorts/', Content) + 19;
          PosEnd := Pos('?', Content);
          if PosEnd = 0 then
            VideoId := Copy(Content, PosStart, Length(Content) - PosStart + 1)
          else
            VideoId := Copy(Content, PosStart, PosEnd - PosStart);
        end
        else if Pos('youtu.be/', Content) > 0 then
        begin
          PosStart := Pos('youtu.be/', Content) + 9;
          PosEnd := Pos('?', Content);
          if PosEnd = 0 then
            VideoId := Copy(Content, PosStart, Length(Content) - PosStart + 1)
          else
            VideoId := Copy(Content, PosStart, PosEnd - PosStart);
        end;

        // Create HTML with embedded YouTube video
        HtmlContent := '<html><body style="margin:0;padding:20px;font-family:sans-serif;">' +
                       '<h3>' + Item.Caption + '</h3>' +
                       '<p><img src="https://i.ytimg.com/vi/' + VideoId + '/hqdefault.jpg" ' +
                       'style="max-width:100%;height:auto;border:1px solid #ccc;" ' +
                       'alt="Video thumbnail"></p>' +
                       '<p><a href="' + Content + '" target="_blank">' + Content + '</a></p>' +
                       //'<p><iframe width="640" height="360" ' +
                       //'src="https://www.youtube.com/embed/' + VideoId + '" ' +
                       //'frameborder="0" allowfullscreen></iframe></p>' +
                       '</body></html>';
        SetWatchButtonURL(Content);
        try
          FHtmlPanel.SetHTMLFromStr(HtmlContent);
        except
          on E: Exception do
            ShowMessage('Error displaying HTML: ' + E.Message);
        end;
      end
      else
      begin
        // Regular RSS feed content
        SetWatchButtonURL('');
        try
          FHtmlPanel.SetHTMLFromStr(Content);
        except
          on E: Exception do
            ShowMessage('Error displaying HTML: ' + E.Message);
        end;
      end;
    end;

  finally
    FInSelectItem := False
  end;
end;

procedure TFormMain.TreeViewSelectionChanged(Sender: TObject);
var
  NodeData: TFeedNodeData;
  IsYouTubeFeed: Boolean;
begin
  if FTreeView.Selected = nil then
    Exit;

  NodeData := GetSelectedNodeData;
  if Assigned(NodeData) and not NodeData.IsFolder then
  begin
    IsYouTubeFeed := IsYouTubeFeedURL(NodeData.FeedURL);
    LoadFeedItemsFromDb(NodeData.FeedURL, IsYouTubeFeed);

    if FListView.Items.Count = 0 then
      FHtmlPanel.SetHTMLFromStr('<html><body><p>No cached items for this feed yet.</p></body></html>')
    else
      FHtmlPanel.SetHTMLFromStr('<html><body><p>Select an item to view its content.</p></body></html>');
  end;
end;

procedure TFormMain.ListViewSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  Content: string;
  VideoId: string;
  HtmlContent: string;
  PosStart, PosEnd: Integer;
  FeedURL, ItemLink, ItemKey: string;
  NodeData: TFeedNodeData;
  ShouldMarkAsRead: Boolean;
begin
{$IFDEF RSSREADER_DEBUG}
  ShowMessage('entered listviewselectitem');
{$ENDIF}
  // Only process when an item is SELECTED (not deselected)
  if not Selected or (Item = nil) then
    Exit;

  // prevent double-trigger cascades
  if FInSelectItem then Exit;
       FInSelectItem := True;
  try
    ShouldMarkAsRead := False;

    // Don't mark as read while loading feed
    if not FLoadingFeed then
    begin
      // Only process if item is currently marked as unread
      if Item.Data = Pointer(1) then
      begin
        // Mark item as read
        NodeData := GetSelectedNodeData;
        if Assigned(NodeData) and not NodeData.IsFolder then
        begin
          FeedURL := NodeData.FeedURL;
          if Item.SubItems.Count > 2 then
          begin
            ItemLink := Item.SubItems[2]; // The link is in SubItems[2]
            if Item.SubItems.Count > 3 then
              ItemKey := Item.SubItems[3]
            else
              ItemKey := ItemLink;
            ShouldMarkAsRead := True;
          end;
        end;
      end;
    end;

    // Item.SubItems[1] contains the content (description or YouTube URL)
    if Item.SubItems.Count > 1 then
    begin
      Content := Item.SubItems[1];

      // Determine current feed URL (needed for YouTube special-casing)
      NodeData := GetSelectedNodeData;
      if Assigned(NodeData) and not NodeData.IsFolder then
        FeedURL := NodeData.FeedURL
      else
        FeedURL := '';


      // Check if this is a YouTube feed AND contains a YouTube URL
      // Only create embeds for actual YouTube feeds, not blog posts that mention YouTube
      if (Pos('youtube.com/feeds/videos.xml', FeedURL) > 0) and
         ((Pos('youtube.com/watch?v=', Content) > 0) or
          (Pos('youtu.be/', Content) > 0) or
          (Pos('youtube.com/shorts/', Content) > 0)) then
      begin
        // Extract video ID
        VideoId := '';
        if Pos('youtube.com/watch?v=', Content) > 0 then
        begin
          PosStart := Pos('v=', Content) + 2;
          PosEnd := Pos('&', Content);
          if PosEnd = 0 then
            VideoId := Copy(Content, PosStart, Length(Content)- PosStart + 1)
          else
            VideoId := Copy(Content, PosStart, PosEnd - PosStart);
        end
        else if Pos('youtube.com/shorts/', Content) > 0 then
        begin
          PosStart := Pos('youtube.com/shorts/', Content) + 19;
          PosEnd := Pos('?', Content);
          if PosEnd = 0 then
            VideoId := Copy(Content, PosStart, Length(Content) - PosStart + 1)
          else
            VideoId := Copy(Content, PosStart, PosEnd - PosStart);
        end
        else if Pos('youtu.be/', Content) > 0 then
        begin
          PosStart := Pos('youtu.be/', Content) + 9;
          PosEnd := Pos('?', Content);
          if PosEnd = 0 then
            VideoId := Copy(Content, PosStart, Length(Content)- PosStart + 1)
          else
            VideoId := Copy(Content, PosStart, PosEnd - PosStart);
        end;

        // Create HTML with embedded YouTube video
        HtmlContent := '<html><body style="margin:0;padding:20px;font-family:sans-serif;">' +
                       '<h3>' + Item.Caption + '</h3>' +
                       '<p><a href="' + Content + '" target="_blank">' + Content + '</a></p>' +
                       '<p><iframe width="640" height="360" ' +
                       'src="https://www.youtube.com/embed/' + VideoId + '" ' +
                       'frameborder="0" allowfullscreen></iframe></p>' +
                       '</body></html>';
        try
          FHtmlPanel.SetHTMLFromStr(HtmlContent);
        except
          on E: Exception do
            ShowMessage('Error displaying HTML: ' + E.Message);
        end;
      end
      else
      begin
        // Regular RSS feed content
        try
          FHtmlPanel.SetHTMLFromStr(Content);
        except
          on E: Exception do
            ShowMessage('Error displaying HTML: ' + E.Message);
        end;
      end;
    end;

    // NOW mark the item as read and update the count
    if ShouldMarkAsRead then
    begin
      MarkItemAsRead(FeedURL, ItemKey, ItemLink);
      Item.Data := nil; // Mark as read in ListView
      // Update tree node text
      FeedTreeUtils.UpdateFeedNodeText(FTreeView.Selected, FFeedItemsDb, FReadStatusDb);
    end;

  finally
    FInSelectItem := False
  end;
end;

procedure TFormMain.ListViewCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  // Draw unread items in bold font
  if Item.Data = Pointer(1) then  // Unread item
  begin
    Sender.Canvas.Font.Name := 'Arial';
    Sender.Canvas.Font.Size := 11;
    Sender.Canvas.Font.Style := [fsBold, fsUnderline];
    Sender.Canvas.Font.Color := clBlack;
    //Sender.Canvas.Font.StrikeThrough := True;
  end
  else
  begin
    Sender.Canvas.Font.Name := 'Courier New';
    Sender.Canvas.Font.Size := 9;
    Sender.Canvas.Font.Style := [fsItalic];
    Sender.Canvas.Font.Color := clGray;
  end;
  DefaultDraw := True;
end;

procedure TFormMain.TreeViewPopup(Sender: TObject);
var
  NodeData: TFeedNodeData;
begin
  // Enable/disable menu items based on selection
  NodeData := GetSelectedNodeData;

  // Refresh Feed (index 3) - only available for feeds, not folders
  FPopupMenu.Items[3].Enabled := Assigned(NodeData) and not NodeData.IsFolder;

  // Refresh Feeds (index 4) - only available for folders, not feeds
  FPopupMenu.Items[4].Enabled := Assigned(NodeData) and NodeData.IsFolder;

  // Refresh All Feeds (index 5) - always available
  FPopupMenu.Items[5].Enabled := True;

  // Mark All Read (index 6) - available for both feeds and folders
  FPopupMenu.Items[6].Enabled := Assigned(NodeData);
end;

procedure TFormMain.MenuAddFolderClick(Sender: TObject);
var
  FolderName: string;
  NodeData: TFeedNodeData;
begin
  FolderName := InputBox('Add Folder', 'Folder name:', '');
  if FolderName = '' then
    Exit;

  NodeData := TFeedNodeData.Create;
  NodeData.IsFolder := True;
  NodeData.FeedURL := '';

  if FTreeView.Selected = nil then
    FTreeView.Items.AddObject(nil, FolderName, NodeData)
  else
    FTreeView.Items.AddChildObject(FTreeView.Selected, FolderName, NodeData);

  SaveFeedList;
end;

procedure TFormMain.MenuAddFeedClick(Sender: TObject);
var
  FeedName, FeedURL: string;
  NodeData: TFeedNodeData;
  ParentNode: TTreeNode;
begin
  FeedName := InputBox('Add Feed', 'Feed name:', '');
  if FeedName = '' then
    Exit;

  FeedURL := InputBox('Add Feed', 'Feed URL:', '');
  if FeedURL = '' then
    Exit;

  FeedURL := ConvertYouTubeURLToFeed(FeedURL);

  NodeData := TFeedNodeData.Create;
  NodeData.IsFolder := False;
  NodeData.FeedURL := FeedURL;

  ParentNode := FTreeView.Selected;

  // If selected node is a feed (not folder), add to its parent
  if Assigned(ParentNode) then
  begin
    if Assigned(ParentNode.Data) and not TFeedNodeData(ParentNode.Data).IsFolder then
      ParentNode := ParentNode.Parent;
  end;

  if ParentNode = nil then
    FTreeView.Items.AddObject(nil, FeedName, NodeData)
  else
    FTreeView.Items.AddChildObject(ParentNode, FeedName, NodeData);

  SaveFeedList;
end;

procedure TFormMain.MenuDeleteClick(Sender: TObject);
var
  Node: TTreeNode;
  NodeData: TFeedNodeData;
  FeedURL: string;
begin
  Node := FTreeView.Selected;
  if Node = nil then
    Exit;

  if MessageDlg('Delete', 'Delete "' + Node.Text + '"?',
                 mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    // If it is a feed (not folder), delete its database entries
    NodeData := TFeedNodeData(Node.Data);
    if Assigned(NodeData) and not NodeData.IsFolder then
    begin
      FeedURL := NodeData.FeedURL;

      // Delete all read status entries for this feed
      if Assigned(FReadStatusDb) and FReadStatusDb.Active then
      begin
        FReadStatusDb.First;
        while not FReadStatusDb.EOF do
        begin
          if FReadStatusDb.FieldByName('FEEDURL').AsString = FeedURL then
            FReadStatusDb.Delete
          else
            FReadStatusDb.Next;
        end;
      end;
    end;

    FreeNodeData(Node);
    FTreeView.Items.Delete(Node);
    SaveFeedList;
  end;
end;

procedure TFormMain.RefreshFeedAction(const AFeedURL: string);
begin
  LoadRSSFeed(AFeedURL);
end;

procedure TFormMain.MarkFeedReadAction(const AFeedURL: string);
begin
  LoadRSSFeed(AFeedURL);
  MarkAllItemsAsRead(AFeedURL);
end;

procedure TFormMain.MenuRefreshClick(Sender: TObject);
var
  NodeData: TFeedNodeData;
begin
  NodeData := GetSelectedNodeData;
  if Assigned(NodeData) and not NodeData.IsFolder then
    LoadRSSFeed(NodeData.FeedURL);
end;

procedure TFormMain.MenuRefreshFeedsClick(Sender: TObject);
var
  NodeData: TFeedNodeData;
  RefreshCount: Integer;
begin
  NodeData := GetSelectedNodeData;
  if not Assigned(NodeData) or not NodeData.IsFolder then
    Exit;

  RefreshCount := FeedActionUtils.RefreshFeedsInSubtree(FTreeView.Selected, @RefreshFeedAction);

  if RefreshCount > 0 then
    ShowMessage('Refreshed ' + IntToStr(RefreshCount) + ' feeds in folder.')
  else
    ShowMessage('No feeds found in folder.');
end;

procedure TFormMain.MenuRefreshAllClick(Sender: TObject);
var
  RefreshCount: Integer;
begin
  if not Assigned(FTreeView) or (FTreeView.Items.Count = 0) then
  begin
    ShowMessage('No feeds to refresh.');
    Exit;
  end;

  RefreshCount := FeedActionUtils.RefreshAllFeeds(FTreeView, @RefreshFeedAction);

  if RefreshCount > 0 then
    ShowMessage('Refreshed ' + IntToStr(RefreshCount) + ' feeds.')
  else
    ShowMessage('No feeds to refresh.');
end;

procedure TFormMain.LoadFeedList;
begin
  try
    LoadFeedTreeFromConfig(FTreeView);
  except
    on E: Exception do
      ShowMessage('Error loading feeds: ' + E.Message);
  end;
end;

procedure TFormMain.SaveFeedList;
begin
  SaveFeedTreeToConfig(FTreeView);
end;

procedure TFormMain.LoadRSSFeed(const AURL: string);
var
  Response: string;
  IsYouTubeFeed: Boolean;
begin
{$IFDEF RSSREADER_DEBUG}
  FDebugLog.Clear;
{$ENDIF}
  DebugLog('=== Loading Feed ===');
  DebugLog(Format('AURL parameter: "%s"', [AURL]));
  DebugLog('');

  FLoadingFeed := True;
  FListView.Items.Clear;
  FHtmlPanel.SetHTMLFromStr('<html><body><p>Loading...</p></body></html>');
  Application.ProcessMessages;

  try
    try
      Response := FHttpClient.Get(AURL);
    except
      on E: Exception do
      begin
        FLoadingFeed := False;
        ShowMessage('Failed to connect to feed:' + LineEnding + LineEnding +
                    AURL + LineEnding + LineEnding +
                    'Error: ' + E.Message);
        FHtmlPanel.SetHTMLFromStr('<html><body><p style="color:red;">Connection failed: ' +
                                  E.Message + '</p></body></html>');
        Exit;
      end;
    end;

    try
      ParseFeedResponse(AURL, Response, @MarkFeedItemsAsNotSeen, @SaveOrUpdateFeedItem, IsYouTubeFeed);

      LoadFeedItemsFromDb(AURL, IsYouTubeFeed);

      if FListView.Items.Count = 0 then
        FHtmlPanel.SetHTMLFromStr('<html><body><p>No items found in feed.</p></body></html>')
      else
        FHtmlPanel.SetHTMLFromStr('<html><body><p>Select an item to view its content.</p></body></html>');

      FeedTreeUtils.UpdateAllFeedNodeTexts(FTreeView, FFeedItemsDb, FReadStatusDb);

      FLoadingFeed := False;

      DebugLog('');
      DebugLog('=== Load Complete ===');
      DebugLog('Total items loaded from DB: ' + IntToStr(FListView.Items.Count));
      DebugLog('Unread items: ' + IntToStr(GetUnreadCount(AURL)));
    except
      on E: Exception do
      begin
        FLoadingFeed := False;
        ShowMessage('Error loading feed: ' + E.Message);
        FHtmlPanel.SetHTMLFromStr('<html><body><p style="color:red;">Error loading feed: ' +
                                  E.Message + '</p></body></html>');
      end;
    end;
  finally
  end;
end;

procedure TFormMain.LoadFeedItemsFromDb(const AURL: string; IsYouTubeFeed: Boolean);
begin
  FeedListUtils.LoadFeedItemsIntoList(FListView, FFeedItemsDb, FReadStatusDb, AURL, IsYouTubeFeed, @DebugLog);
end;

procedure TFormMain.MarkFeedItemsAsNotSeen(const AURL: string);
begin
  FeedDbUtils.MarkFeedItemsAsNotSeen(FFeedItemsDb, AURL);
end;

procedure TFormMain.SaveOrUpdateFeedItem(const AFeedURL, AItemKey, ATitle, APubDate, AContent, ALink: string);
begin
  FeedDbUtils.SaveOrUpdateFeedItem(FFeedItemsDb, AFeedURL, AItemKey, ATitle, APubDate, AContent, ALink);
end;

function TFormMain.MakeItemKey(const AGuid, AId, ALink, ATitle, APubDate: string): string;
begin
  Result := FeedFetchUtils.MakeItemKey(AGuid, AId, ALink, ATitle, APubDate);
end;

function TFormMain.GetSelectedNodeData: TFeedNodeData;
begin
  Result := nil;
  if Assigned(FTreeView.Selected) and Assigned(FTreeView.Selected.Data) then
    Result := TFeedNodeData(FTreeView.Selected.Data);
end;

procedure TFormMain.FreeNodeData(Node: TTreeNode);
var
  Child: TTreeNode;
begin
  if not Assigned(Node) then
    Exit;

  // Free children first
  Child := Node.GetFirstChild;
  while Assigned(Child) do
  begin
    FreeNodeData(Child);
    Child := Child.GetNextSibling;
  end;

  // Free this nodes data
  if Assigned(Node.Data) then
  begin
    TFeedNodeData(Node.Data).Free;
    Node.Data := nil;
  end;
end;

procedure TFormMain.InitializeDatabase;
begin
  FeedDbUtils.InitializeDatabases(FReadStatusDb, FFeedItemsDb, TomarDataDir);
end;

procedure TFormMain.DebugLog(const S: string);
begin
{$IFDEF RSSREADER_DEBUG}
  if Assigned(FDebugLog) then
    FDebugLog.Add(S);
{$ENDIF}
end;

function TFormMain.ComputeItemHash(const AFeedURL, AItemKey: string): string;
begin
  Result := MD5Print(MD5String(AFeedURL + AItemKey));
end;

function TFormMain.IsItemRead(const AFeedURL, AItemKey: string; const ALegacyLink: string = ''): Boolean;
begin
  Result := FeedDbUtils.IsItemRead(FReadStatusDb, AFeedURL, AItemKey, ALegacyLink, @DebugLog);
end;

procedure TFormMain.MarkItemAsRead(const AFeedURL, AItemKey: string; const ALegacyLink: string = '');
begin
  try
    FeedDbUtils.MarkItemAsRead(FReadStatusDb, AFeedURL, AItemKey, ALegacyLink, @DebugLog);
  except
    on E: Exception do
    begin
      DebugLog('EXCEPTION during Post: ' + E.ClassName + ' - ' + E.Message);
      ShowMessage('Error marking item as read: ' + AItemKey + #13#10 + E.Message);
    end;
  end;
end;

procedure TFormMain.MarkAllItemsAsRead(const AFeedURL: string);
begin
{$IFDEF RSSREADER_DEBUG}
  FDebugLog.Clear;
{$ENDIF}
  FeedListUtils.MarkAllListItemsAsRead(FListView, FReadStatusDb, AFeedURL, @DebugLog);

{$IFDEF RSSREADER_DEBUG}
  ShowMessage(FDebugLog.Text);
{$ENDIF}

  FeedTreeUtils.UpdateAllFeedNodeTexts(FTreeView, FFeedItemsDb, FReadStatusDb);
end;

function TFormMain.GetUnreadCount(const AFeedURL: string): Integer;
begin
  Result := FeedDbUtils.GetUnreadCount(FFeedItemsDb, FReadStatusDb, AFeedURL);
end;

procedure TFormMain.MenuMarkAllReadClick(Sender: TObject);
var
  NodeData: TFeedNodeData;
  MarkedCount: Integer;
begin
  NodeData := GetSelectedNodeData;
  if not Assigned(NodeData) then
    Exit;

  if NodeData.IsFolder then
  begin
    MarkedCount := FeedActionUtils.ProcessFeedsInSubtree(FTreeView.Selected, @MarkFeedReadAction);
    if MarkedCount > 0 then
      ShowMessage('All items in folder marked as read.')
    else
      ShowMessage('No feeds found in folder.');
  end
  else
  begin
    MarkAllItemsAsRead(NodeData.FeedURL);
    ShowMessage('All items marked as read.');
  end;
end;

procedure TFormMain.MenuShowDebugLogClick(Sender: TObject);
begin
{$IFDEF RSSREADER_DEBUG}
  if FDebugLog.Count = 0 then
    ShowMessage('Debug log is empty.')
  else
    ShowMessage(FDebugLog.Text);
{$ELSE}
  ShowMessage('Debug UI is disabled (compile with RSSREADER_DEBUG).');
{$ENDIF}
end;

function TFormMain.ConvertYouTubeURLToFeed(const AUrl: string): string;
begin
  Result := FeedFetchUtils.ConvertYouTubeURLToFeed(FHttpClient, AUrl);
end;


end.
