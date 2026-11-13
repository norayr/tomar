unit MainForm;

{$mode objfpc}{$H+}

{ Uncomment to enable developer/debug UI & logging }
{.$DEFINE RSSREADER_DEBUG}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, Menus, Process, DOM, XMLRead, XMLWrite, fphttpclient, IpHtml, ipmsg, opensslsockets,
  FPImage, FPReadPNG, FPReadJPEG, FPReadGIF, db, dbf, md5, Clipbrd, DateUtils, HtmlProvider, FeedFetchUtils, FeedModel, FeedConfigUtils, FeedListUtils, FeedRefreshThread, FeedDbUtils, ArticleCollections
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
    FWatchHDButton: TButton;
    FFavoriteButton: TButton;
    FCollections: TArticleCollections;
    FInstallYtDlpButton: TButton;
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
    FLastTreeTapNode: TTreeNode;
    FLastTreeTapTime: TDateTime;

    FSplitter1, FSplitter2: TSplitter;
    FPopupMenu: TPopupMenu;
    FHttpClient: TFPHTTPClient;
    FDataProvider: TCustomHtmlDataProvider;
    FReadStatusDb: TDbf;
    FFeedItemsDb: TDbf;
    FLoadingFeed: Boolean; // Flag to prevent selection during loading
    FInSelectItem: Boolean;
    FAddFeedButton, FAddFolderButton, FRefreshAllButton: TButton;
    FRefreshStatus: TLabel;
    FRefreshTimer: TTimer;
    FRefreshThread: TFeedRefreshThread;
    FRefreshing, FClosing: Boolean;
    FRefreshSucceeded, FRefreshFailed: Integer;
    FRefreshErrors: TStringList;
    FFeedRecords: TFeedRecordIndex;
    FApplyingResult: TFeedRefreshResult;
    FApplyKeys: TStringList;
    FApplyExistingCount, FApplyExistingPosition, FApplyItemPosition: Integer;
{$IFDEF RSSREADER_DEBUG}
    FDebugLog: TStringList;  // Debug log (optional)
{$ENDIF}

    procedure CreateControls;
    procedure UpdateLayout;
    procedure UpdateTreeViewTouchMetrics(const AIsPortrait: Boolean);
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
    procedure TreeViewDblClick(Sender: TObject);
    procedure LongPressTimerTick(Sender: TObject);

    procedure HtmlPanelHotURL(Sender: TObject; const URL: string);
    procedure HtmlPanelContextPopup(Sender: TObject; MousePos: TPoint;
       var Handled: Boolean);
    procedure MenuCopyLinkClick(Sender: TObject);
    procedure FavoriteButtonClick(Sender: TObject);
    procedure WatchButtonClick(Sender: TObject);
    procedure WatchHDButtonClick(Sender: TObject);
    procedure InstallYtDlpButtonClick(Sender: TObject);
    procedure MpvTimerTick(Sender: TObject);

    procedure MenuAddFolderClick(Sender: TObject);
    procedure MenuAddFeedClick(Sender: TObject);
    procedure MenuDeleteClick(Sender: TObject);
    procedure MenuMoveUpClick(Sender: TObject);
    procedure MenuMoveToClick(Sender: TObject);
    procedure MenuRefreshClick(Sender: TObject);
    procedure MenuRefreshFeedsClick(Sender: TObject);
    procedure MenuRefreshAllClick(Sender: TObject);
    procedure MenuMarkAllReadClick(Sender: TObject);
    procedure MenuShowDebugLogClick(Sender: TObject);
    procedure ListViewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MarkFeedReadAction(const AFeedURL: string);
    procedure UpdateRefreshActions(AUpdateMenus: Boolean = True);
    procedure StartFeedRefresh(ARootNode: TTreeNode);
    procedure RefreshTimerTick(Sender: TObject);
    procedure BeginApplyFeedRefreshResult(AResult: TFeedRefreshResult);
    function ApplyFeedRefreshResult: Boolean;

    procedure LoadFeedList;
    procedure SaveFeedList;
    procedure AddCollectionNodes;
    procedure LoadCollection(AKind: Integer);
    function GetItemFeedURL(AItem: TListItem): string;
    function SelectedArticle: TStoredArticle;
    procedure UpdateFavoriteButton;
    procedure RecordSelectedAsWatched;
    procedure RefreshActiveCollection(AKind: Integer);
    procedure LoadFeedItemsFromDb(const AURL: string; IsYouTubeFeed: Boolean;
      APreserveSelection: Boolean = False);
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
    function UserYtDlpPath: string;
    function FindYtDlpForPlayback: string;
    function CanWatchYouTubeLocally: Boolean;
    procedure SetWatchButtonURL(const AURL: string);
    procedure SetWatchButtonsEnabled(AEnabled: Boolean);
    procedure StartMpvWithFormat(const AFormat: string);
  public

  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

uses
  LCLType, LCLIntf, BaseUnix, RssUtils, FeedTreeUtils, FeedActionUtils;
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
  FLastTreeTapNode := nil;
  FLastTreeTapTime := 0;
  OnActivate := @FormActivate;
  OnResize := @FormResize;

  FHttpClient := TFPHTTPClient.Create(nil);
  FHttpClient.AllowRedirect := True;

  FDataProvider := TCustomHtmlDataProvider.Create(Self);
  FLoadingFeed := False;
  FRefreshing := False;
  FClosing := False;
  FRefreshErrors := TStringList.Create;
  FCollections := TArticleCollections.Create(TomarDataDir);
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
  AddCollectionNodes;
  LoadFeedList;
  FeedTreeUtils.UpdateAllFeedNodeTexts(FTreeView, FFeedItemsDb, FReadStatusDb);
end;

procedure TFormMain.FormDestroy(Sender: TObject);
var
  i: Integer;
  Node: TTreeNode;
begin
  FClosing := True;
  FreeAndNil(FRefreshTimer);
  // The worker never accesses the form or DBF objects. Join it before cleanup.
  FreeAndNil(FRefreshThread);
  FreeAndNil(FApplyingResult);
  FreeAndNil(FApplyKeys);
  FreeAndNil(FFeedRecords);
  FreeAndNil(FRefreshErrors);
  FreeAndNil(FCollections);

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

  FAddFeedButton := TButton.Create(Self);
  FAddFeedButton.Parent := LeftToolbar;
  FAddFeedButton.Align := alLeft;
  FAddFeedButton.Width := 58;
  FAddFeedButton.Caption := '＋📰';
  FAddFeedButton.Hint := 'Add feed';
  FAddFeedButton.ShowHint := True;
  FAddFeedButton.OnClick := @MenuAddFeedClick;

  FAddFolderButton := TButton.Create(Self);
  FAddFolderButton.Parent := LeftToolbar;
  FAddFolderButton.Align := alLeft;
  FAddFolderButton.Width := 58;
  FAddFolderButton.Caption := '＋📁';
  FAddFolderButton.Hint := 'Add folder';
  FAddFolderButton.ShowHint := True;
  FAddFolderButton.OnClick := @MenuAddFolderClick;

  FRefreshAllButton := TButton.Create(Self);
  FRefreshAllButton.Parent := LeftToolbar;
  FRefreshAllButton.Align := alLeft;
  FRefreshAllButton.Width := 48;
  FRefreshAllButton.Caption := '⟳';
  FRefreshAllButton.Hint := 'Refresh all feeds';
  FRefreshAllButton.ShowHint := True;
  FRefreshAllButton.OnClick := @MenuRefreshAllClick;

  FRefreshStatus := TLabel.Create(Self);
  FRefreshStatus.Parent := FLeftPanel;
  FRefreshStatus.Align := alBottom;
  FRefreshStatus.AutoSize := False;
  FRefreshStatus.Height := 36;
  FRefreshStatus.WordWrap := True;
  FRefreshStatus.Caption := '';
  FRefreshStatus.ShowHint := True;

  FRefreshTimer := TTimer.Create(Self);
  FRefreshTimer.Enabled := False;
  FRefreshTimer.Interval := 20;
  FRefreshTimer.OnTimer := @RefreshTimerTick;

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
  FTreeView.OnDblClick := @TreeViewDblClick;

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
  MenuItem.Caption := 'Move Up';
  MenuItem.OnClick := @MenuMoveUpClick;
  FPopupMenu.Items.Add(MenuItem);

  MenuItem := TMenuItem.Create(FPopupMenu);
  MenuItem.Caption := 'Move to';
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
  FSplitter1.Width := 18;
  // Force the left/right splitter to sit after the tree pane, not before it.
  // With Align=alLeft, LCL uses Left to decide the order among left-aligned controls.
  FSplitter1.Left := FLeftPanel.Left + FLeftPanel.Width;
  FSplitter1.ResizeStyle := rsUpdate;
  FSplitter1.AutoSnap := False;
  FSplitter1.MinSize := 120;
  FSplitter1.Beveled := True;
  FSplitter1.Cursor := crHSplit;
  FSplitter1.Color := clGray;

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
  FSplitter2.Height := 18;
  // Force the vertical splitter to sit below the post list / above the post view.
  // With Align=alTop, LCL uses Top to decide the order among top-aligned controls.
  FSplitter2.Top := FTopPanel.Top + FTopPanel.Height;
  FSplitter2.ResizeStyle := rsUpdate;
  FSplitter2.AutoSnap := False;
  FSplitter2.MinSize := 120;
  FSplitter2.Beveled := True;
  FSplitter2.Cursor := crVSplit;
  FSplitter2.Color := clGray;

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
  FWatchButton.Width := 48;
  FWatchButton.Left := 0;
  FWatchButton.Caption := '📺';
  FWatchButton.Hint := 'Watch, low quality / format 18';
  FWatchButton.ShowHint := True;
  FWatchButton.OnClick := @WatchButtonClick;

  FWatchHDButton := TButton.Create(Self);
  FWatchHDButton.Parent := FWatchPanel;
  FWatchHDButton.Align := alLeft;
  FWatchHDButton.Width := 48;
  FWatchHDButton.Left := FWatchButton.Left + FWatchButton.Width;
  FWatchHDButton.Caption := '📽️';
  FWatchHDButton.Hint := 'Watch, better quality / up to 720p';
  FWatchHDButton.ShowHint := True;
  FWatchHDButton.OnClick := @WatchHDButtonClick;

  // A single compact favorite control for videos and ordinary RSS items.
  FFavoriteButton := TButton.Create(Self);
  FFavoriteButton.Parent := FWatchPanel;
  FFavoriteButton.Align := alLeft;
  FFavoriteButton.Width := 48;
  FFavoriteButton.Caption := '☆';
  FFavoriteButton.Hint := 'Add article to Favorites';
  FFavoriteButton.ShowHint := True;
  FFavoriteButton.Enabled := False;
  FFavoriteButton.OnClick := @FavoriteButtonClick;

  FInstallYtDlpButton := TButton.Create(Self);
  FInstallYtDlpButton.Parent := FWatchPanel;
  FInstallYtDlpButton.Align := alRight;
  FInstallYtDlpButton.Width := 104;
  FInstallYtDlpButton.Caption := '⬇ yt-dlp';
  FInstallYtDlpButton.Hint := 'Install or update yt-dlp in ~/bin';
  FInstallYtDlpButton.ShowHint := True;
  FInstallYtDlpButton.OnClick := @InstallYtDlpButtonClick;

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


procedure TFormMain.UpdateTreeViewTouchMetrics(const AIsPortrait: Boolean);
var
  ShortSide: Integer;
begin
  if not Assigned(FTreeView) then Exit;

  ShortSide := ClientWidth;
  if ClientHeight < ShortSide then
    ShortSide := ClientHeight;

  if AIsPortrait or (ShortSide <= 600) then
  begin
    FTreeView.Font.Size := 22;
    FTreeView.Indent := 48;
  end
  else if ShortSide <= 900 then
  begin
    FTreeView.Font.Size := 20;
    FTreeView.Indent := 42;
  end
  else
  begin
    FTreeView.Font.Size := 18;
    FTreeView.Indent := 34;
  end;
end;

procedure TFormMain.UpdateLayout;
const
  ToolbarH = 42;
var
  IsPortrait: Boolean;
  TreeSize, ListSize, ContentSize: Integer;
  SplitSize: Integer;

  function ClampInt(AValue, AMin, AMax: Integer): Integer;
  begin
    Result := AValue;
    if Result < AMin then Result := AMin;
    if Result > AMax then Result := AMax;
  end;

  function DesiredSplitterSize: Integer;
  var
    ShortSide: Integer;
  begin
    ShortSide := ClientWidth;
    if ClientHeight < ShortSide then
      ShortSide := ClientHeight;

    { On Maemo / small touch screens the default 8-12 px splitter is
      difficult to see and grab.  Make it thicker only on small windows. }
    if ShortSide <= 480 then
      Result := 24
    else if ShortSide <= 720 then
      Result := 18
    else
      Result := 12;
  end;

begin
  if (FLeftPanel = nil) or (FRightPanel = nil) or
     (FTopPanel = nil) or (FBottomPanel = nil) or
     (FSplitter1 = nil) or (FSplitter2 = nil) then Exit;

  if (ClientWidth < 20) or (ClientHeight < 20) then Exit;

  SplitSize := DesiredSplitterSize;

  IsPortrait := ClientHeight >= ClientWidth;
  UpdateTreeViewTouchMetrics(IsPortrait);

  { Keep splitters visible/grabbable even on ordinary same-orientation resizes. }
  if FSplitter1.Align in [alLeft, alRight] then
    FSplitter1.Width := SplitSize
  else
    FSplitter1.Height := SplitSize;

  if FSplitter2.Align in [alLeft, alRight] then
    FSplitter2.Width := SplitSize
  else
    FSplitter2.Height := SplitSize;

  FSplitter1.Color := clGray;
  FSplitter2.Color := clGray;
  FSplitter1.Beveled := True;
  FSplitter2.Beveled := True;

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
  if Button = mbRight then
  begin
    FLongPressTimer.Enabled := False;
    FTreeView.Selected := FTreeView.GetNodeAt(X, Y);
    Exit;
  end;

  if Button = mbLeft then
  begin
    FLongPressPoint := Point(X, Y);
    FLongPressControl := FTreeView;
    FLongPressTimer.Enabled := True;
  end;
end;

procedure TFormMain.TreeViewMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Node, UpNode: TTreeNode;
  NodeData: TFeedNodeData;
  TextRect: TRect;
  NowTime: TDateTime;
begin
  FLongPressTimer.Enabled := False;

  if (Button <> mbLeft) or not Assigned(FTreeView) then
    Exit;

  { Treat a tap on the folder name as expand/collapse.  This is much
    easier on touch screens than hitting the tiny expander mark. }
  Node := FTreeView.GetNodeAt(FLongPressPoint.X, FLongPressPoint.Y);
  UpNode := FTreeView.GetNodeAt(X, Y);
  if (Node = nil) or (Node <> UpNode) then
    Exit;

  NodeData := nil;
  if Assigned(Node.Data) then
    NodeData := TFeedNodeData(Node.Data);

  if not (Assigned(NodeData) and NodeData.IsFolder and (Node.Count > 0)) then
    Exit;

  { Only toggle when the tap is on/near the text label, not merely on empty
    space in the row.  The small margin helps with fingers/stylus taps. }
  TextRect := Node.DisplayRect(True);
  if not ((FLongPressPoint.X >= TextRect.Left - 12) and
          (FLongPressPoint.X <= TextRect.Right + 24) and
          (FLongPressPoint.Y >= TextRect.Top - 4) and
          (FLongPressPoint.Y <= TextRect.Bottom + 4)) then
    Exit;

  { Ignore the second event of a quick double-tap, otherwise a double-tap
    would expand and immediately collapse the same folder again. }
  NowTime := Now;
  if (FLastTreeTapNode = Node) and
     (FLastTreeTapTime > 0) and
     (MilliSecondsBetween(NowTime, FLastTreeTapTime) < 650) then
    Exit;

  FLastTreeTapNode := Node;
  FLastTreeTapTime := NowTime;

  FTreeView.Selected := Node;
  if Node.Expanded then
    Node.Collapse(False)
  else
    Node.Expand(False);
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


procedure TFormMain.TreeViewDblClick(Sender: TObject);
begin
  { Folder expand/collapse is handled in TreeViewMouseUp.  Keeping this empty
    so a double-tap does not toggle the same folder twice. }
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

function EscapeHtmlLink(const S: string): string;
begin
  Result := StringReplace(S, '&', '&amp;', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '&quot;', [rfReplaceAll]);
  Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
end;

{ Append the item's RSS/Atom link after its HTML content.  For complete
  HTML documents insert before </body>, not after </html>. }
function ArticleWithOriginalLink(const AHtml, AUrl: string): string;
var
  URL, LinkHtml: string;
  InsertAt: SizeInt;
begin
  Result := AHtml;
  URL := Trim(AUrl);
  if (Pos('https://', LowerCase(URL)) <> 1) and
     (Pos('http://', LowerCase(URL)) <> 1) then
    Exit;

  LinkHtml := '<p><a href="' + EscapeHtmlLink(URL) + '">' +
              EscapeHtmlLink(URL) + '</a></p>';
  InsertAt := Pos('</body>', LowerCase(Result));
  if InsertAt = 0 then
    InsertAt := Pos('</html>', LowerCase(Result));
  if InsertAt <> 0 then
    Insert(LinkHtml, Result, InsertAt)
  else
    Result := Result + LinkHtml;
end;

procedure TFormMain.SetWatchButtonsEnabled(AEnabled: Boolean);
begin
  if Assigned(FWatchButton) then
    FWatchButton.Enabled := AEnabled;
  if Assigned(FWatchHDButton) then
    FWatchHDButton.Enabled := AEnabled;
  if Assigned(FInstallYtDlpButton) then
    FInstallYtDlpButton.Enabled := AEnabled;
end;

procedure TFormMain.StartMpvWithFormat(const AFormat: string);
var
  YtDlpPath: string;
begin
  if FCurrentVideoURL = '' then
    Exit;

  if Assigned(FMpvProcess) and FMpvProcess.Running then
    Exit;

  YtDlpPath := FindYtDlpForPlayback;
  if YtDlpPath = '' then
  begin
    ShowMessage('yt-dlp was not found. Use the ⬇ yt-dlp button to install it into ~/bin.');
    Exit;
  end;

  FreeAndNil(FMpvProcess);

  FMpvProcess := TProcess.Create(nil);
  try
    FMpvProcess.Executable := 'mpv';
    FMpvProcess.Parameters.Add('--ytdl=yes');
    FMpvProcess.Parameters.Add('--script-opts=ytdl_hook-ytdl_path=' + YtDlpPath);
    FMpvProcess.Parameters.Add('--ytdl-format=' + AFormat);
    //FMpvProcess.Parameters.Add('--ytdl-raw-options=cookies-from-browser=firefox:/home/inky/.librewolf/vifu5p28.default-release::youtube');
    FMpvProcess.Parameters.Add('--cache=yes');
    FMpvProcess.Parameters.Add('--cache-secs=60');
    FMpvProcess.Parameters.Add('--cache-pause=yes');
    FMpvProcess.Parameters.Add('--cache-pause-initial=yes');
    FMpvProcess.Parameters.Add('--cache-pause-wait=10');
    FMpvProcess.Parameters.Add('--demuxer-max-bytes=512M');
    FMpvProcess.Parameters.Add(FCurrentVideoURL);
    FMpvProcess.Options := [];

    SetWatchButtonsEnabled(False);
    FMpvProcess.Execute;
    // A launch is a watch event; don't record failed attempts.
    try
      RecordSelectedAsWatched;
    except
      on E: Exception do
        ShowMessage('mpv started, but Watched could not be saved: ' + E.Message);
    end;
    FMpvTimer.Enabled := True;
  except
    on E: Exception do
    begin
      FreeAndNil(FMpvProcess);
      SetWatchButtonsEnabled(True);
      ShowMessage('Could not start mpv: ' + E.Message);
    end;
  end;
end;

procedure TFormMain.WatchButtonClick(Sender: TObject);
begin
  { Low quality button: progressive format 18, any format up to 360p, then
    separate video and audio streams up to 360p. }
  StartMpvWithFormat('18/b[height<=360]/(bv*[height<=360]+ba)');
end;

procedure TFormMain.WatchHDButtonClick(Sender: TObject);
begin
  { Better-quality button: prefer H.264 up to 720p with M4A audio.
    Old phones may still struggle, but the user explicitly chose this. }
  StartMpvWithFormat('bestvideo[height<=720][vcodec^=avc1]+bestaudio[ext=m4a]/best[height<=720][vcodec^=avc1]/best[height<=720]/18/best');
end;

procedure TFormMain.InstallYtDlpButtonClick(Sender: TObject);
const
  YtDlpURL = 'https://github.com/yt-dlp/yt-dlp/releases/latest/download/yt-dlp';
var
  HomeDir, BinDir, DestFile, TmpFile: string;
  Client: TFPHTTPClient;
  Stream: TFileStream;
begin
  HomeDir := GetEnvironmentVariable('HOME');
  if HomeDir = '' then
  begin
    ShowMessage('Cannot find HOME, so I do not know where ~/bin is.');
    Exit;
  end;

  BinDir := IncludeTrailingPathDelimiter(HomeDir) + 'bin';
  DestFile := IncludeTrailingPathDelimiter(BinDir) + 'yt-dlp';
  TmpFile := DestFile + '.tmp';

  if not ForceDirectories(BinDir) then
  begin
    ShowMessage('Could not create ' + BinDir);
    Exit;
  end;

  if Assigned(FInstallYtDlpButton) then
    FInstallYtDlpButton.Enabled := False;
  try
    if FileExists(TmpFile) then
      DeleteFile(TmpFile);

    Client := TFPHTTPClient.Create(nil);
    try
      Client.AllowRedirect := True;
      Stream := TFileStream.Create(TmpFile, fmCreate);
      try
        Client.Get(YtDlpURL, Stream);
      finally
        Stream.Free;
      end;
    finally
      Client.Free;
    end;

    if fpChmod(PChar(TmpFile), S_IRUSR or S_IWUSR or S_IXUSR or
      S_IRGRP or S_IXGRP or S_IROTH or S_IXOTH) <> 0 then
      raise Exception.Create('Could not chmod ' + TmpFile);

    if FileExists(DestFile) then
      DeleteFile(DestFile);
    if not RenameFile(TmpFile, DestFile) then
      raise Exception.Create('Could not move ' + TmpFile + ' to ' + DestFile);

    FYoutubePlayerAvailable := CanWatchYouTubeLocally;
    SetWatchButtonURL(FCurrentVideoURL);
    ShowMessage('Installed yt-dlp to ' + DestFile);
  except
    on E: Exception do
    begin
      if FileExists(TmpFile) then
        DeleteFile(TmpFile);
      ShowMessage('Could not install yt-dlp: ' + E.Message);
    end;
  end;

  if Assigned(FInstallYtDlpButton) then
    FInstallYtDlpButton.Enabled := True;
end;

procedure TFormMain.MpvTimerTick(Sender: TObject);
begin
  if not Assigned(FMpvProcess) then
  begin
    FMpvTimer.Enabled := False;
    SetWatchButtonsEnabled(True);
    Exit;
  end;

  if not FMpvProcess.Running then
  begin
    FMpvTimer.Enabled := False;
    FreeAndNil(FMpvProcess);
    SetWatchButtonsEnabled(True);
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

function TFormMain.UserYtDlpPath: string;
var
  HomeDir: string;
begin
  HomeDir := GetEnvironmentVariable('HOME');
  if HomeDir = '' then
    Result := ''
  else
    Result := IncludeTrailingPathDelimiter(HomeDir) + 'bin/yt-dlp';
end;

function TFormMain.FindYtDlpForPlayback: string;
var
  UserPath: string;
begin
  Result := '';

  UserPath := UserYtDlpPath;
  if (UserPath <> '') and FileExists(UserPath) and (fpAccess(UserPath, X_OK) = 0) then
  begin
    Result := UserPath;
    Exit;
  end;

  { Let mpv's ytdl_hook resolve yt-dlp from PATH if there is no ~/bin copy. }
  if IsExecutableInPath('yt-dlp') then
    Result := 'yt-dlp';
end;

function TFormMain.CanWatchYouTubeLocally: Boolean;
begin
  Result := IsExecutableInPath('mpv') and (FindYtDlpForPlayback <> '');
end;

procedure TFormMain.SetWatchButtonURL(const AURL: string);
begin
  FCurrentVideoURL := AURL;
  FYoutubePlayerAvailable := CanWatchYouTubeLocally;

  if Assigned(FWatchButton) then
    FWatchButton.Visible := FYoutubePlayerAvailable and (FCurrentVideoURL <> '');
  if Assigned(FWatchHDButton) then
    FWatchHDButton.Visible := FYoutubePlayerAvailable and (FCurrentVideoURL <> '');
  if Assigned(FInstallYtDlpButton) then
    FInstallYtDlpButton.Visible := FCurrentVideoURL <> '';

  UpdateFavoriteButton;
  SetWatchButtonsEnabled(not (Assigned(FMpvProcess) and FMpvProcess.Running));
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
        if Assigned(NodeData) and
           ((not NodeData.IsFolder) or (NodeData.CollectionKind <> 0)) then
        begin
          FeedURL := GetItemFeedURL(Item);
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
            if Assigned(NodeData) and (NodeData.CollectionKind <> 0) then
              FeedTreeUtils.UpdateAllFeedNodeTexts(FTreeView, FFeedItemsDb, FReadStatusDb)
            else
              FeedTreeUtils.UpdateFeedNodeText(FTreeView.Selected, FFeedItemsDb, FReadStatusDb, FFeedRecords);
          end;
        end;
      end;
    end;

    // Item.SubItems[1] contains the content (description or YouTube URL)
    if Item.SubItems.Count > 1 then
    begin
      Content := Item.SubItems[1];
      if Item.SubItems.Count > 2 then
        ItemLink := Item.SubItems[2]
      else
        ItemLink := '';

      // Determine current feed URL (needed for YouTube special-casing)
      NodeData := GetSelectedNodeData;
      FeedURL := GetItemFeedURL(Item);


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
                       //'<p><iframe width="640" height="360" ' +
                       //'src="https://www.youtube.com/embed/' + VideoId + '" ' +
                       //'frameborder="0" allowfullscreen></iframe></p>' +
                       '</body></html>';
        SetWatchButtonURL(Content);
        try
          FHtmlPanel.SetHTMLFromStr(ArticleWithOriginalLink(HtmlContent, ItemLink));
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
          FHtmlPanel.SetHTMLFromStr(ArticleWithOriginalLink(Content, ItemLink));
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

  SetWatchButtonURL('');
  NodeData := GetSelectedNodeData;
  if Assigned(NodeData) and (NodeData.CollectionKind <> 0) then
  begin
    LoadCollection(NodeData.CollectionKind);
    if FListView.Items.Count = 0 then
      FHtmlPanel.SetHTMLFromStr('<html><body><p>No articles in this collection yet.</p></body></html>')
    else
      FHtmlPanel.SetHTMLFromStr('<html><body><p>Select an article to view it.</p></body></html>');
  end
  else if Assigned(NodeData) and not NodeData.IsFolder then
  begin
    IsYouTubeFeed := IsYouTubeFeedURL(NodeData.FeedURL);
    LoadFeedItemsFromDb(NodeData.FeedURL, IsYouTubeFeed);

    if FListView.Items.Count = 0 then
      FHtmlPanel.SetHTMLFromStr('<html><body><p>No cached items for this feed yet.</p></body></html>')
    else
      FHtmlPanel.SetHTMLFromStr('<html><body><p>Select an item to view its content.</p></body></html>');
  end
  else
  begin
    FListView.Items.Clear;
    FHtmlPanel.SetHTMLFromStr('<html><body><p>Select a feed.</p></body></html>');
  end;
  UpdateFavoriteButton;
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
        if Assigned(NodeData) and
           ((not NodeData.IsFolder) or (NodeData.CollectionKind <> 0)) then
        begin
          FeedURL := GetItemFeedURL(Item);
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
      if Item.SubItems.Count > 2 then
        ItemLink := Item.SubItems[2]
      else
        ItemLink := '';

      // Determine current feed URL (needed for YouTube special-casing)
      NodeData := GetSelectedNodeData;
      FeedURL := GetItemFeedURL(Item);


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
        SetWatchButtonURL(Content);
        HtmlContent := '<html><body style="margin:0;padding:20px;font-family:sans-serif;">' +
                       '<h3>' + Item.Caption + '</h3>' +
                       '<p><iframe width="640" height="360" ' +
                       'src="https://www.youtube.com/embed/' + VideoId + '" ' +
                       'frameborder="0" allowfullscreen></iframe></p>' +
                       '</body></html>';
        try
          FHtmlPanel.SetHTMLFromStr(ArticleWithOriginalLink(HtmlContent, ItemLink));
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
          FHtmlPanel.SetHTMLFromStr(ArticleWithOriginalLink(Content, ItemLink));
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
      if Assigned(NodeData) and (NodeData.CollectionKind <> 0) then
        FeedTreeUtils.UpdateAllFeedNodeTexts(FTreeView, FFeedItemsDb, FReadStatusDb)
      else
        FeedTreeUtils.UpdateFeedNodeText(FTreeView.Selected, FFeedItemsDb, FReadStatusDb, FFeedRecords);
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
  Node: TTreeNode;
  MoveMenu, Item: TMenuItem;

  procedure AddDestination(const ACaption: string; ADestination: TTreeNode);
  begin
    Item := TMenuItem.Create(MoveMenu);
    Item.Caption := ACaption;
    if Assigned(ADestination) then
      Item.Tag := PtrInt(ADestination)
    else
      Item.Tag := -1;
    Item.OnClick := @MenuMoveToClick;
    MoveMenu.Add(Item);
  end;

  procedure AddFolderDestinations(ANode: TTreeNode);
  var
    Data: TFeedNodeData;
  begin
    while Assigned(ANode) do
    begin
      Data := TFeedNodeData(ANode.Data);
      if Assigned(Data) and Data.IsFolder and (Data.CollectionKind = 0) and
         (ANode <> FTreeView.Selected) and
         not FTreeView.Selected.HasAsParent(ANode) then
        AddDestination(ANode.Text, ANode);
      if ANode.HasChildren then
        AddFolderDestinations(ANode.GetFirstChild);
      ANode := ANode.GetNextSibling;
    end;
  end;
begin
  UpdateRefreshActions;
  Node := FTreeView.Selected;
  MoveMenu := FPopupMenu.Items[9];
  MoveMenu.Clear;
  if Assigned(Node) and not FRefreshing and Assigned(Node.Data) and
     (TFeedNodeData(Node.Data).CollectionKind = 0) then
  begin
    AddDestination('Top level', nil);
    AddFolderDestinations(FTreeView.Items.GetFirstNode);
    MoveMenu.Enabled := MoveMenu.Count > 0;
  end
  else
    MoveMenu.Enabled := False;
end;

procedure TFormMain.MenuMoveUpClick(Sender: TObject);
var
  Node, NewParent: TTreeNode;
begin
  if FRefreshing or FClosing then Exit;
  Node := FTreeView.Selected;
  if not Assigned(Node) or not Assigned(Node.Parent) then Exit;
  if Assigned(Node.Data) and (TFeedNodeData(Node.Data).CollectionKind <> 0) then Exit;
  NewParent := Node.Parent.Parent;
  if Assigned(NewParent) then
    Node.MoveTo(NewParent, naAddChild)
  else
    Node.MoveTo(nil, naAdd);
  FTreeView.Selected := Node;
  SaveFeedList;
end;

procedure TFormMain.MenuMoveToClick(Sender: TObject);
var
  Node, Destination: TTreeNode;
  DestinationTag: PtrInt;
begin
  if FRefreshing or FClosing then Exit;
  Node := FTreeView.Selected;
  if not Assigned(Node) or not (Sender is TMenuItem) then Exit;
  if Assigned(Node.Data) and (TFeedNodeData(Node.Data).CollectionKind <> 0) then Exit;
  DestinationTag := TMenuItem(Sender).Tag;
  if DestinationTag = -1 then
    Destination := nil
  else
    Destination := TTreeNode(Pointer(DestinationTag));
  if (Destination = Node) or (Assigned(Destination) and Destination.HasAsParent(Node)) then
    Exit;
  if Assigned(Destination) then
    Node.MoveTo(Destination, naAddChild)
  else
    Node.MoveTo(nil, naAdd);
  FTreeView.Selected := Node;
  SaveFeedList;
end;

procedure TFormMain.MenuAddFolderClick(Sender: TObject);
var
  FolderName: string;
  NodeData: TFeedNodeData;
begin
  if FRefreshing or FClosing then Exit;
  FolderName := InputBox('Add Folder', 'Folder name:', '');
  if FolderName = '' then
    Exit;

  NodeData := TFeedNodeData.Create;
  NodeData.IsFolder := True;
  NodeData.FeedURL := '';

  if (FTreeView.Selected = nil) or
     (Assigned(FTreeView.Selected.Data) and
      (TFeedNodeData(FTreeView.Selected.Data).CollectionKind <> 0)) then
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
  if FRefreshing or FClosing then Exit;
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
  if Assigned(ParentNode) and Assigned(ParentNode.Data) and
     (TFeedNodeData(ParentNode.Data).CollectionKind <> 0) then
    ParentNode := nil;

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
  if FRefreshing or FClosing then Exit;
  Node := FTreeView.Selected;
  if Node = nil then
    Exit;
  if Assigned(Node.Data) and (TFeedNodeData(Node.Data).CollectionKind <> 0) then
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

procedure TFormMain.MarkFeedReadAction(const AFeedURL: string);
begin
  FeedDbUtils.MarkAllFeedItemsAsRead(FFeedItemsDb, FReadStatusDb, AFeedURL);
end;

procedure TFormMain.UpdateRefreshActions(AUpdateMenus: Boolean);
var
  Node: TTreeNode;
  NodeData: TFeedNodeData;
  CanChange: Boolean;
begin
  CanChange := not FRefreshing and not FClosing;
  FAddFeedButton.Enabled := CanChange;
  FAddFolderButton.Enabled := CanChange;
  FRefreshAllButton.Enabled := CanChange;
  if not AUpdateMenus then Exit;
  Node := FTreeView.Selected;
  NodeData := GetSelectedNodeData;
  FPopupMenu.Items[0].Enabled := CanChange;
  FPopupMenu.Items[1].Enabled := CanChange;
  FPopupMenu.Items[3].Enabled := CanChange and Assigned(NodeData) and
    (NodeData.CollectionKind = 0) and not NodeData.IsFolder;
  FPopupMenu.Items[4].Enabled := CanChange and Assigned(NodeData) and
    (NodeData.CollectionKind = 0) and NodeData.IsFolder;
  FPopupMenu.Items[5].Enabled := CanChange;
  FPopupMenu.Items[6].Enabled := CanChange and Assigned(NodeData) and
    (NodeData.CollectionKind = 0);
  FPopupMenu.Items[7].Enabled := CanChange and Assigned(NodeData) and
    (NodeData.CollectionKind = 0);
  FPopupMenu.Items[8].Enabled := CanChange and Assigned(Node) and
    Assigned(Node.Parent) and Assigned(NodeData) and (NodeData.CollectionKind = 0);
  FPopupMenu.Items[9].Enabled := CanChange and Assigned(NodeData) and
    (NodeData.CollectionKind = 0);
end;

procedure TFormMain.StartFeedRefresh(ARootNode: TTreeNode);
var
  URLs: TStringList;
  I: Integer;
  Node: TTreeNode;
  NodeData: TFeedNodeData;
begin
  if FRefreshing or FClosing then Exit;
  URLs := TStringList.Create;
  try
    // Snapshot URLs on the UI thread; the worker never holds tree-node pointers.
    for I := 0 to FTreeView.Items.Count - 1 do
    begin
      Node := FTreeView.Items[I];
      if Assigned(ARootNode) and (Node <> ARootNode) and
         not Node.HasAsParent(ARootNode) then
        Continue;
      NodeData := TFeedNodeData(Node.Data);
      if Assigned(NodeData) and not NodeData.IsFolder and
         (NodeData.CollectionKind = 0) then
        URLs.Add(NodeData.FeedURL);
    end;
    if URLs.Count = 0 then
    begin
      FRefreshStatus.Caption := 'No feeds to refresh.';
      FRefreshStatus.Hint := '';
      Exit;
    end;

    try
      FRefreshThread := TFeedRefreshThread.Create(URLs);
      FRefreshSucceeded := 0;
      FRefreshFailed := 0;
      FRefreshErrors.Clear;
      FRefreshing := True;
      // Don't change the activating GTK popup while it is dispatching OnClick.
      // Handlers are already guarded; menu state updates on the next timer tick.
      UpdateRefreshActions(False);
      FRefreshStatus.Caption := Format('Refreshing 0/%d feeds...', [FRefreshThread.TotalFeeds]);
      FRefreshStatus.Hint := '';
      FRefreshThread.Start;
      FRefreshTimer.Enabled := True;
    except
      on E: Exception do
      begin
        FreeAndNil(FRefreshThread);
        FRefreshing := False;
        UpdateRefreshActions;
        FRefreshStatus.Caption := 'Could not start refresh.';
        FRefreshStatus.Hint := E.Message;
        ShowMessage('Could not start feed refresh: ' + E.Message);
      end;
    end;
  finally
    URLs.Free;
  end;
end;

procedure TFormMain.BeginApplyFeedRefreshResult(AResult: TFeedRefreshResult);
var
  Records: TFeedRecordList;
begin
  FApplyingResult := AResult;
  FApplyKeys := TStringList.Create;
  FApplyKeys.CaseSensitive := True;
  FApplyKeys.Sorted := True;
  FApplyKeys.Duplicates := dupIgnore;
  FApplyExistingPosition := 0;
  FApplyItemPosition := 0;
  Records := FFeedRecords.RecordsForFeed(AResult.FeedURL);
  if Assigned(Records) then FApplyExistingCount := Records.Count
  else FApplyExistingCount := 0;
end;

function TFormMain.ApplyFeedRefreshResult: Boolean;
const
  MaxOperations = 64;
  TimeBudgetMs = 8;
var
  I, KeyIndex, RecordNo, Operations: Integer;
  Started: QWord;
  Entry: TFeedListEntry;
  NodeData: TFeedNodeData;
  Records: TFeedRecordList;
begin
  Result := False;
  Started := GetTickCount64;
  Operations := 0;
  Records := FFeedRecords.RecordsForFeed(FApplyingResult.FeedURL);
  // Visit existing records once, not the entire database once per new item.
  // Every slice sets RecNo explicitly: browsing can move the DBF cursor between ticks.
  while FApplyExistingPosition < FApplyExistingCount do
  begin
    FFeedItemsDb.RecNo := Records[FApplyExistingPosition];
    FApplyKeys.AddObject(FFeedItemsDb.FieldByName('ITEMKEY').AsString,
      TObject(Pointer(PtrInt(FFeedItemsDb.RecNo))));
    if FFeedItemsDb.FieldByName('INLASTFETCH').AsBoolean then
    begin
      FFeedItemsDb.Edit;
      FFeedItemsDb.FieldByName('INLASTFETCH').AsBoolean := False;
      FFeedItemsDb.Post;
    end;
    Inc(FApplyExistingPosition);
    Inc(Operations);
    if (Operations >= MaxOperations) or (GetTickCount64 - Started >= TimeBudgetMs) then Exit;
  end;

  while FApplyItemPosition < FApplyingResult.Items.Count do
  begin
    Entry := TFeedListEntry(FApplyingResult.Items[FApplyItemPosition]);
    KeyIndex := FApplyKeys.IndexOf(Entry.ItemKeyValue);
    if KeyIndex >= 0 then RecordNo := PtrInt(Pointer(FApplyKeys.Objects[KeyIndex]))
    else RecordNo := 0;
    FeedDbUtils.SaveOrUpdateFeedItem(FFeedItemsDb, FApplyingResult.FeedURL,
      Entry.ItemKeyValue, Entry.Title, Entry.PubDate, Entry.ContentValue,
      Entry.LinkValue, RecordNo);
    if RecordNo = 0 then
    begin
      FFeedRecords.AddRecord(FApplyingResult.FeedURL, FFeedItemsDb.RecNo);
      FApplyKeys.AddObject(Entry.ItemKeyValue, TObject(Pointer(PtrInt(FFeedItemsDb.RecNo))));
    end;
    Inc(FApplyItemPosition);
    Inc(Operations);
    if (Operations >= MaxOperations) or (GetTickCount64 - Started >= TimeBudgetMs) then Exit;
  end;

  // Only this feed's counts changed; don't rescan every feed after each result.
  for I := 0 to FTreeView.Items.Count - 1 do
  begin
    NodeData := TFeedNodeData(FTreeView.Items[I].Data);
    if Assigned(NodeData) and not NodeData.IsFolder and
       (NodeData.FeedURL = FApplyingResult.FeedURL) then
      FeedTreeUtils.UpdateFeedNodeText(FTreeView.Items[I], FFeedItemsDb, FReadStatusDb, FFeedRecords);
  end;
  NodeData := GetSelectedNodeData;
  if Assigned(NodeData) and not NodeData.IsFolder and
     (NodeData.FeedURL = FApplyingResult.FeedURL) then
    LoadFeedItemsFromDb(FApplyingResult.FeedURL, FApplyingResult.IsYouTubeFeed, True);
  Result := True;
end;

procedure TFormMain.RefreshTimerTick(Sender: TObject);
var
  FeedResult: TFeedRefreshResult;
  WorkerDone: Boolean;
  WorkerError: string;
  ApplyDone: Boolean;
begin
  if FClosing or not Assigned(FRefreshThread) then Exit;
  UpdateRefreshActions;

  // Read Done first: observing completion then an empty queue guarantees that
  // no last result can arrive between TakeResult and finishing the refresh.
  WorkerDone := FRefreshThread.Done;
  if not Assigned(FApplyingResult) then
  begin
    FeedResult := FRefreshThread.TakeResult;
    if Assigned(FeedResult) then
      BeginApplyFeedRefreshResult(FeedResult);
  end;
  FeedResult := FApplyingResult;
  if Assigned(FeedResult) then
  begin
    ApplyDone := FeedResult.ErrorMessage <> '';
    if not ApplyDone then
    begin
      try
        ApplyDone := ApplyFeedRefreshResult;
      except
        on E: Exception do
        begin
          if FFeedItemsDb.State in [dsEdit, dsInsert] then
            FFeedItemsDb.Cancel;
          FeedResult.ErrorMessage := E.Message;
          ApplyDone := True;
        end;
      end;
    end;
    if not ApplyDone then
    begin
      FRefreshStatus.Caption := Format('Saving %d/%d items...',
        [FApplyItemPosition, FeedResult.Items.Count]);
      Exit;
    end;
    try
      if FeedResult.ErrorMessage = '' then
      begin
        Inc(FRefreshSucceeded);
      end
      else
      begin
        Inc(FRefreshFailed);
        FRefreshErrors.Add(FeedResult.FeedURL + ': ' + FeedResult.ErrorMessage);
        DebugLog(FRefreshErrors[FRefreshErrors.Count - 1]);
      end;
    finally
      FreeAndNil(FApplyingResult);
      FreeAndNil(FApplyKeys);
    end;
    // Apply at most one feed per tick so UI events can run between feeds.
    FRefreshStatus.Caption := Format('Refreshing %d/%d feeds...',
      [FRefreshSucceeded + FRefreshFailed, FRefreshThread.TotalFeeds]);
    Exit;
  end;

  if not WorkerDone then Exit;
  FRefreshTimer.Enabled := False;
  WorkerError := FRefreshThread.ErrorMessage;
  if WorkerError <> '' then
  begin
    FRefreshErrors.Add(WorkerError);
    FRefreshFailed := FRefreshThread.TotalFeeds - FRefreshSucceeded;
  end;
  FreeAndNil(FRefreshThread);
  FRefreshing := False;
  UpdateRefreshActions;
  FRefreshStatus.Caption := Format('Refreshed %d feeds.', [FRefreshSucceeded]);
  if FRefreshFailed > 0 then
    FRefreshStatus.Caption := FRefreshStatus.Caption + Format(' %d failed.', [FRefreshFailed]);
  FRefreshStatus.Hint := FRefreshErrors.Text;
end;

procedure TFormMain.MenuRefreshClick(Sender: TObject);
var
  NodeData: TFeedNodeData;
begin
  if FRefreshing or FClosing then Exit;
  NodeData := GetSelectedNodeData;
  if Assigned(NodeData) and not NodeData.IsFolder then
    StartFeedRefresh(FTreeView.Selected);
end;

procedure TFormMain.MenuRefreshFeedsClick(Sender: TObject);
var
  NodeData: TFeedNodeData;
begin
  if FRefreshing or FClosing then Exit;
  NodeData := GetSelectedNodeData;
  if not Assigned(NodeData) or not NodeData.IsFolder or
     (NodeData.CollectionKind <> 0) then
    Exit;

  StartFeedRefresh(FTreeView.Selected);
end;

procedure TFormMain.MenuRefreshAllClick(Sender: TObject);
begin
  StartFeedRefresh(nil);
end;

procedure TFormMain.AddCollectionNodes;
var
  Data: TFeedNodeData;
begin
  // Created before loading feeds.xml, so the normal subscriptions follow them.
  Data := TFeedNodeData.Create;
  Data.IsFolder := True;
  Data.CollectionKind := COLLECTION_WATCHED;
  FTreeView.Items.AddObject(nil, 'Watched', Data);

  Data := TFeedNodeData.Create;
  Data.IsFolder := True;
  Data.CollectionKind := COLLECTION_FAVORITES;
  FTreeView.Items.AddObject(nil, 'Favorites', Data);
end;

function TFormMain.GetItemFeedURL(AItem: TListItem): string;
var
  NodeData: TFeedNodeData;
begin
  Result := '';
  if not Assigned(AItem) then Exit;
  if AItem.SubItems.Count > 4 then
  begin
    Result := AItem.SubItems[4];
    Exit;
  end;
  NodeData := GetSelectedNodeData;
  if Assigned(NodeData) and (NodeData.CollectionKind = 0) and
     not NodeData.IsFolder then
    Result := NodeData.FeedURL;
end;

function TFormMain.SelectedArticle: TStoredArticle;
var
  Item: TListItem;
begin
  Result := nil;
  Item := FListView.Selected;
  if not Assigned(Item) or (Item.SubItems.Count < 4) or
     (GetItemFeedURL(Item) = '') or (Item.SubItems[3] = '') then Exit;
  Result := TStoredArticle.Create;
  Result.FeedURL := GetItemFeedURL(Item);
  Result.ItemKey := Item.SubItems[3];
  Result.Title := Item.Caption;
  Result.PubDate := Item.SubItems[0];
  Result.Content := Item.SubItems[1];
  Result.Link := Item.SubItems[2];
end;

procedure TFormMain.UpdateFavoriteButton;
var
  Item: TListItem;
  IsFavorite, HaveItem: Boolean;
begin
  if not Assigned(FFavoriteButton) then Exit;
  Item := FListView.Selected;
  HaveItem := Assigned(Item) and (Item.SubItems.Count > 3);
  if HaveItem then
    HaveItem := (GetItemFeedURL(Item) <> '') and (Item.SubItems[3] <> '');
  FFavoriteButton.Enabled := HaveItem;
  FFavoriteButton.Visible := HaveItem;
  IsFavorite := False;
  if HaveItem then
    IsFavorite := FCollections.IsFavorite(GetItemFeedURL(Item), Item.SubItems[3]);
  if IsFavorite then
  begin
    FFavoriteButton.Caption := '⭐';
    FFavoriteButton.Hint := 'Remove article from Favorites';
  end
  else
  begin
    FFavoriteButton.Caption := '☆';
    FFavoriteButton.Hint := 'Add article to Favorites';
  end;
  if Assigned(FWatchPanel) then
    FWatchPanel.Visible := HaveItem or (FCurrentVideoURL <> '');
end;

procedure TFormMain.LoadCollection(AKind: Integer);
var
  I: Integer;
  Article: TStoredArticle;
  Item: TListItem;
  WasLoading, WasSelecting: Boolean;
begin
  WasLoading := FLoadingFeed;
  WasSelecting := FInSelectItem;
  FLoadingFeed := True;
  FInSelectItem := True;
  FListView.Items.BeginUpdate;
  try
    FListView.Items.Clear;
    for I := 0 to FCollections.Count(AKind) - 1 do
    begin
      Article := FCollections.ArticleAt(AKind, I);
      Item := FListView.Items.Add;
      Item.Caption := Article.Title;
      Item.SubItems.Add(Article.PubDate);
      Item.SubItems.Add(Article.Content);
      Item.SubItems.Add(Article.Link);
      Item.SubItems.Add(Article.ItemKey);
      Item.SubItems.Add(Article.FeedURL); // original feed in mixed collection
      if not IsItemRead(Article.FeedURL, Article.ItemKey, Article.Link) then
        Item.Data := Pointer(1)
      else
        Item.Data := nil;
    end;
  finally
    FListView.Items.EndUpdate;
    FInSelectItem := WasSelecting;
    FLoadingFeed := WasLoading;
  end;
  SetWatchButtonURL('');
end;

procedure TFormMain.RefreshActiveCollection(AKind: Integer);
var
  Data: TFeedNodeData;
begin
  Data := GetSelectedNodeData;
  if Assigned(Data) and (Data.CollectionKind = AKind) then
  begin
    LoadCollection(AKind);
    FHtmlPanel.SetHTMLFromStr('<html><body><p>Select an article to view it.</p></body></html>');
  end;
end;

procedure TFormMain.FavoriteButtonClick(Sender: TObject);
var
  Article: TStoredArticle;
begin
  Article := SelectedArticle;
  if not Assigned(Article) then Exit;
  FCollections.ToggleFavorite(Article); // takes ownership
  RefreshActiveCollection(COLLECTION_FAVORITES);
  UpdateFavoriteButton;
end;

procedure TFormMain.RecordSelectedAsWatched;
var
  Article: TStoredArticle;
begin
  Article := SelectedArticle;
  if not Assigned(Article) then Exit;
  FCollections.MarkWatched(Article); // takes ownership
  // Preserve current video selection and playback while in Watched.
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

procedure TFormMain.LoadFeedItemsFromDb(const AURL: string; IsYouTubeFeed: Boolean;
  APreserveSelection: Boolean);
var
  I: Integer;
  SelectedKey: string;
  WasLoading, WasSelecting: Boolean;
begin
  SelectedKey := '';
  if APreserveSelection and Assigned(FListView.Selected) and
     (FListView.Selected.SubItems.Count > 3) then
    SelectedKey := FListView.Selected.SubItems[3];
  WasLoading := FLoadingFeed;
  WasSelecting := FInSelectItem;
  FLoadingFeed := True;
  FInSelectItem := True;
  try
    FeedListUtils.LoadFeedItemsIntoList(FListView, FFeedItemsDb, FReadStatusDb,
      AURL, IsYouTubeFeed, @DebugLog, FFeedRecords);
    // Rebuilding the list must not mark an item read or replace its open content.
    if SelectedKey <> '' then
      for I := 0 to FListView.Items.Count - 1 do
        if FListView.Items[I].SubItems[3] = SelectedKey then
        begin
          FListView.Selected := FListView.Items[I];
          FListView.ItemFocused := FListView.Items[I];
          Break;
        end;
  finally
    FInSelectItem := WasSelecting;
    FLoadingFeed := WasLoading;
  end;
  // A refresh can replace or clear the selected row.
  if not Assigned(FListView.Selected) then
    SetWatchButtonURL('')
  else
    UpdateFavoriteButton;
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
  FFeedRecords := TFeedRecordIndex.Create(FFeedItemsDb);
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
  Result := FeedDbUtils.GetUnreadCount(FFeedItemsDb, FReadStatusDb, AFeedURL, FFeedRecords);
end;

procedure TFormMain.MenuMarkAllReadClick(Sender: TObject);
var
  NodeData: TFeedNodeData;
  MarkedCount: Integer;
begin
  if FRefreshing or FClosing then Exit;
  NodeData := GetSelectedNodeData;
  if not Assigned(NodeData) or (NodeData.CollectionKind <> 0) then
    Exit;

  if NodeData.IsFolder then
  begin
    MarkedCount := FeedActionUtils.ProcessFeedsInSubtree(FTreeView.Selected, @MarkFeedReadAction);
    FeedTreeUtils.UpdateAllFeedNodeTexts(FTreeView, FFeedItemsDb, FReadStatusDb);
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
