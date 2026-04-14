unit MainForm;

{$mode objfpc}{$H+}

{ Uncomment to enable developer/debug UI & logging }
{.$DEFINE RSSREADER_DEBUG}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, Menus, DOM, XMLRead, XMLWrite, fphttpclient, IpHtml, ipmsg, opensslsockets,
  FPImage, FPReadPNG, FPReadJPEG, FPReadGIF, db, dbf, md5, Clipbrd, DateUtils;

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

  { TCustomHtmlDataProvider - Simple data provider for image loading }

  TCustomHtmlDataProvider = class(TIpAbstractHtmlDataProvider)
  private
    FHttpClient: TFPHTTPClient;
  protected
    function DoGetHtmlStream(const URL: string; PostData: TIpFormDataEntity): TStream; override;
    function DoCheckURL(const URL: string; var ContentType: string): Boolean; override;
    procedure DoLeave(Html: TIpHtml); override;
    procedure DoReference(const URL: string); override;
    procedure DoGetImage(Sender: TIpHtmlNode; const URL: string; var Picture: TPicture); override;
    function CanHandle(const URL: string): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function BuildURL(const OldURL, NewURL: string): string; override;
    function DoGetStream(const URL: string): TStream; override;
  end;

  { TFormMain }

  TFormMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FTreeView: TTreeView;
    FListView: TListView;
    FHtmlPanel: TIpHtmlPanel;
    FHtmlPopup: TPopupMenu;
    FCurrentURL: string; // to track the link under mouse

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

    procedure MenuAddFolderClick(Sender: TObject);
    procedure MenuAddFeedClick(Sender: TObject);
    procedure MenuDeleteClick(Sender: TObject);
    procedure MenuRefreshClick(Sender: TObject);
    procedure MenuRefreshFeedsClick(Sender: TObject);
    procedure MenuRefreshAllClick(Sender: TObject);
    procedure MenuMarkAllReadClick(Sender: TObject);
    procedure MenuShowDebugLogClick(Sender: TObject);
    procedure ListViewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

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
    procedure UpdateFeedNodeText(Node: TTreeNode);
    procedure UpdateAllFeedNodeTexts;
    function BuildReadKeyCache: TStringList;
    function FindCountIndex(ACounts: TStringList; const AFeedURL: string): Integer;
    function GetCountValue(ACounts: TStringList; const AFeedURL: string): Integer;
    procedure IncrementCountValue(ACounts: TStringList; const AFeedURL: string);

    function ConvertYouTubeURLToFeed(const AUrl: string): string;
  public

  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

uses
  LCLType, LCLIntf;
  //FeedDBCleanup;

const
  CONFIG_FILE_NAME = 'feeds.xml';




function TomarBaseDir: string;
begin
  // ~/.tomar/  directory
  Result := IncludeTrailingPathDelimiter(GetUserDir) + '.tomar' + DirectorySeparator;
  ForceDirectories(Result);
end;

function TomarDataDir: string;
begin
  // ~/.tomar/data/
  Result := TomarBaseDir + 'data' + DirectorySeparator;
  ForceDirectories(Result);
end;

function TomarConfigFile: string;
begin
  Result := TomarBaseDir + CONFIG_FILE_NAME;
end;

function IsYouTubeFeedURL(const AFeedURL: string): Boolean;
begin
  Result := Pos('youtube.com/feeds/videos.xml', LowerCase(AFeedURL)) > 0;
end;


function MonthNameToNumber(const S: string): Integer;
const
  Months: array[1..12] of string =
    ('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN',
     'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC');
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to 12 do
    if Months[I] = UpperCase(S) then
      Exit(I);
end;

function ExtractDigits(const S: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(S) do
    if S[I] in ['0'..'9'] then
      Result := Result + S[I];
end;

function FeedDateSortKey(const APubDate: string): string;
var
  S, TimePart, Digits: string;
  Y, M, D, H, N, Sec: Integer;
  Parts, TParts: TStringList;
  Work: string;
begin
  Result := '00000000000000';
  S := Trim(APubDate);
  if S = '' then
    Exit;

  if (Length(S) >= 10) and (S[5] = '-') and (S[8] = '-') then
  begin
    Y := StrToIntDef(Copy(S, 1, 4), 0);
    M := StrToIntDef(Copy(S, 6, 2), 0);
    D := StrToIntDef(Copy(S, 9, 2), 0);
    H := 0;
    N := 0;
    Sec := 0;

    if Length(S) >= 19 then
    begin
      H := StrToIntDef(Copy(S, 12, 2), 0);
      N := StrToIntDef(Copy(S, 15, 2), 0);
      Sec := StrToIntDef(Copy(S, 18, 2), 0);
    end;

    Result := Format('%.4d%.2d%.2d%.2d%.2d%.2d', [Y, M, D, H, N, Sec]);
    Exit;
  end;

  Work := StringReplace(S, ',', ' ', [rfReplaceAll]);
  while Pos('  ', Work) > 0 do
    Work := StringReplace(Work, '  ', ' ', [rfReplaceAll]);
  Work := Trim(Work);

  Parts := TStringList.Create;
  TParts := TStringList.Create;
  try
    ExtractStrings([' '], [], PChar(Work), Parts);

    if Parts.Count >= 4 then
    begin
      if Length(Parts[0]) = 3 then
      begin
        D := StrToIntDef(Parts[1], 0);
        M := MonthNameToNumber(Parts[2]);
        Y := StrToIntDef(Parts[3], 0);
        if Parts.Count >= 5 then
          TimePart := Parts[4]
        else
          TimePart := '00:00:00';
      end
      else
      begin
        D := StrToIntDef(Parts[0], 0);
        M := MonthNameToNumber(Parts[1]);
        Y := StrToIntDef(Parts[2], 0);
        if Parts.Count >= 4 then
          TimePart := Parts[3]
        else
          TimePart := '00:00:00';
      end;

      ExtractStrings([':'], [], PChar(TimePart), TParts);
      if TParts.Count >= 1 then H := StrToIntDef(ExtractDigits(TParts[0]), 0) else H := 0;
      if TParts.Count >= 2 then N := StrToIntDef(ExtractDigits(TParts[1]), 0) else N := 0;
      if TParts.Count >= 3 then Sec := StrToIntDef(ExtractDigits(TParts[2]), 0) else Sec := 0;

      if (Y > 0) and (M > 0) and (D > 0) then
      begin
        Result := Format('%.4d%.2d%.2d%.2d%.2d%.2d', [Y, M, D, H, N, Sec]);
        Exit;
      end;
    end;
  finally
    TParts.Free;
    Parts.Free;
  end;

  Digits := ExtractDigits(S);
  if Length(Digits) >= 8 then
  begin
    while Length(Digits) < 14 do
      Digits := Digits + '0';
    Result := Copy(Digits, 1, 14);
  end;
end;

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

{ TCustomHtmlDataProvider }

constructor TCustomHtmlDataProvider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHttpClient := TFPHTTPClient.Create(nil);
  FHttpClient.AllowRedirect := True;
end;

destructor TCustomHtmlDataProvider.Destroy;
begin
  FHttpClient.Free;
  inherited Destroy;
end;

function TCustomHtmlDataProvider.DoGetHtmlStream(const URL: string;
  PostData: TIpFormDataEntity): TStream;
begin
  Result := nil; // We do not handle HTML loading through data provider
end;

function TCustomHtmlDataProvider.DoCheckURL(const URL: string;
  var ContentType: string): Boolean;
begin
  Result := True; // Accept all URLs
end;

procedure TCustomHtmlDataProvider.DoLeave(Html: TIpHtml);
begin
  // Nothing to do
end;

procedure TCustomHtmlDataProvider.DoReference(const URL: string);
begin
  // Nothing to do
end;

procedure TCustomHtmlDataProvider.DoGetImage(Sender: TIpHtmlNode;
  const URL: string; var Picture: TPicture);
var
  Stream: TMemoryStream;
  ImageURL: string;
begin
  // Handle relative URLs
  ImageURL := URL;
  if (Pos('://', ImageURL) = 0) and (Length(ImageURL) > 0) then
  begin
    // This is a relative URL - skip it for now
    Exit;
  end;

  // Skip incomplete ytimg URLs (must have a file extension like .jpg)
  {if (Pos('i.ytimg.com', ImageURL) > 0) and
     (Pos('.jpg', LowerCase(ImageURL)) = 0) and
     (Pos('.jpeg', LowerCase(ImageURL)) = 0) and
     (Pos('.png', LowerCase(ImageURL)) = 0) and
     (Pos('.webp', LowerCase(ImageURL)) = 0) then
  begin
    Exit;
  end;
   }
  Stream := TMemoryStream.Create;
  try
    try
      // Download the image
      FHttpClient.Get(ImageURL, Stream);
      Stream.Position := 0;

      // Create picture if not assigned
      if Picture = nil then
        Picture := TPicture.Create;

      // Load the image from stream
      Picture.LoadFromStream(Stream);
    except
      on E: Exception do
      begin
        // Silently fail - image loading errors should not break the view
      end;
    end;
  finally
    Stream.Free;
  end;
end;

function TCustomHtmlDataProvider.CanHandle(const URL: string): Boolean;
begin
  Result := True;
end;

function TCustomHtmlDataProvider.BuildURL(const OldURL, NewURL: string): string;
begin
  // Simple implementation - just return the new URL
  if Pos('://', NewURL) > 0 then
    Result := NewURL
  else
    Result := OldURL + NewURL;
end;

function TCustomHtmlDataProvider.DoGetStream(const URL: string): TStream;
begin
  Result := nil; // We do not handle stream loading
end;

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
var
  Duplicates: TStringList;
begin
  Caption := 'Տոմար';
  Width := 1000;
  Height := 700;
  Position := poScreenCenter;

  FHttpClient := TFPHTTPClient.Create(nil);
  FHttpClient.AllowRedirect := True;

  FDataProvider := TCustomHtmlDataProvider.Create(Self);
  FLoadingFeed := False;

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
  UpdateAllFeedNodeTexts;
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

  FHttpClient.Free;
end;

procedure TFormMain.CreateControls;
var
  LeftPanel, RightPanel, TopPanel, BottomPanel: TPanel;
  MenuItem: TMenuItem;
begin
  // Left Panel with TreeView
  LeftPanel := TPanel.Create(Self);
  LeftPanel.Parent := Self;
  LeftPanel.Align := alLeft;
  LeftPanel.Width := 250;
  LeftPanel.Caption := '';
  LeftPanel.BevelOuter := bvNone;
  LeftPanel.Constraints.MinWidth := 120;

  FTreeView := TTreeView.Create(Self);
  FTreeView.Parent := LeftPanel;
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
  FSplitter1.Width := 8;
  FSplitter1.ResizeStyle := rsUpdate;
  FSplitter1.AutoSnap := False;
  FSplitter1.MinSize := 120;
  FSplitter1.Beveled := True;
  FSplitter1.Cursor := crHSplit;
  FSplitter1.Color := clMedGray;

  // Right Panel
  RightPanel := TPanel.Create(Self);
  RightPanel.Parent := Self;
  RightPanel.Align := alClient;
  RightPanel.Caption := '';
  RightPanel.BevelOuter := bvNone;

  // Top panel with ListView
  TopPanel := TPanel.Create(Self);
  TopPanel.Parent := RightPanel;
  TopPanel.Align := alTop;
  TopPanel.Height := 250;
  TopPanel.Caption := '';
  TopPanel.BevelOuter := bvNone;
  TopPanel.Constraints.MinHeight := 120;

  FListView := TListView.Create(Self);
  FListView.Parent := TopPanel;
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
  FSplitter2.Parent := RightPanel;
  FSplitter2.Align := alTop;
  FSplitter2.Height := 8;
  FSplitter2.ResizeStyle := rsUpdate;
  FSplitter2.AutoSnap := False;
  FSplitter2.MinSize := 120;
  FSplitter2.Beveled := True;
  FSplitter2.Cursor := crVSplit;
  FSplitter2.Color := clMedGray;

  // Bottom panel with HTML viewer
  BottomPanel := TPanel.Create(Self);
  BottomPanel.Parent := RightPanel;
  BottomPanel.Align := alClient;
  BottomPanel.Caption := '';
  BottomPanel.BevelOuter := bvNone;
  BottomPanel.Constraints.MinHeight := 120;

  FHtmlPanel := TIpHtmlPanel.Create(Self);
  FHtmlPanel.Parent := BottomPanel;
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

// end of copy link

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
            UpdateFeedNodeText(FTreeView.Selected);
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
      UpdateFeedNodeText(FTreeView.Selected);
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

  procedure RefreshFeedsInNode(Node: TTreeNode; var Count: Integer);
  var
    ChildNode: TTreeNode;
    ChildData: TFeedNodeData;
  begin
    if not Assigned(Node) then
      Exit;

    if Assigned(Node.Data) then
    begin
      ChildData := TFeedNodeData(Node.Data);

      if ChildData.IsFolder then
      begin
        // Recursively process children
        ChildNode := Node.GetFirstChild;
        while Assigned(ChildNode) do
        begin
          RefreshFeedsInNode(ChildNode, Count);
          ChildNode := ChildNode.GetNextSibling;
        end;
      end
      else
      begin
        // It is a feed - refresh it
        FTreeView.Selected := Node;
        LoadRSSFeed(ChildData.FeedURL);
        Inc(Count);
        Application.ProcessMessages;
      end;
    end;
  end;

var
  RefreshCount: Integer;
begin
  NodeData := GetSelectedNodeData;
  if not Assigned(NodeData) or not NodeData.IsFolder then
    Exit;

  RefreshCount := 0;
  RefreshFeedsInNode(FTreeView.Selected, RefreshCount);

  if RefreshCount > 0 then
    ShowMessage('Refreshed ' + IntToStr(RefreshCount) + ' feeds in folder.')
  else
    ShowMessage('No feeds found in folder.');
end;

procedure TFormMain.MenuRefreshAllClick(Sender: TObject);
var
  i: Integer;
  Node: TTreeNode;
  NodeData: TFeedNodeData;
  RefreshCount: Integer;
begin
  if not Assigned(FTreeView) or (FTreeView.Items.Count = 0) then
  begin
    ShowMessage('No feeds to refresh.');
    Exit;
  end;

  RefreshCount := 0;

  // Loop through all tree nodes and reload each feed
  for i := 0 to FTreeView.Items.Count - 1 do
  begin
    Node := FTreeView.Items[i];
    if Assigned(Node) and Assigned(Node.Data) then
    begin
      NodeData := TFeedNodeData(Node.Data);
      if not NodeData.IsFolder then
      begin
        // Select this node and load its feed
        FTreeView.Selected := Node;
        LoadRSSFeed(NodeData.FeedURL);
        Inc(RefreshCount);
        Application.ProcessMessages; // Allow UI to update
      end;
    end;
  end;

  if RefreshCount > 0 then
    ShowMessage('Refreshed ' + IntToStr(RefreshCount) + ' feeds.')
  else
    ShowMessage('No feeds to refresh.');
end;

procedure TFormMain.LoadFeedList;
var
  Doc: TXMLDocument;
  Root, ChildNode: TDOMNode;

  function StripTrailingCount(const S: string): string;
  var
    i, j: Integer;
  begin
    Result := Trim(S);

    // Remove a trailing unread count like "My Feed (23)"
    i := Length(Result);
    if (i < 4) or (Result[i] <> ')') then
      Exit;

    j := i - 1;
    while (j > 0) and (Result[j] in ['0'..'9']) do
      Dec(j);

    if (j > 1) and (Result[j] = '(') and (Result[j - 1] = ' ') then
      Result := Copy(Result, 1, j - 2);
  end;

  procedure LoadNode(ParentTreeNode: TTreeNode; XMLNode: TDOMNode);
  var
    Child: TDOMNode;
    NodeData: TFeedNodeData;
    TreeNode: TTreeNode;
    Name, URL, NodeType: string;
  begin
    Child := XMLNode.FirstChild;
    while Assigned(Child) do
    begin
      if Child.NodeName = 'item' then
      begin
        Name := '';
        URL := '';
        NodeType := 'folder';

        ChildNode := Child.FirstChild;
        while Assigned(ChildNode) do
        begin
          if ChildNode.NodeName = 'name' then
            Name := ChildNode.TextContent
          else if ChildNode.NodeName = 'url' then
            URL := ChildNode.TextContent
          else if ChildNode.NodeName = 'type' then
            NodeType := ChildNode.TextContent;
          ChildNode := ChildNode.NextSibling;
        end;
        Name := StripTrailingCount(Name);
        NodeData := TFeedNodeData.Create;
        NodeData.IsFolder := (NodeType = 'folder');
        NodeData.FeedURL := URL;

        if ParentTreeNode = nil then
          TreeNode := FTreeView.Items.AddObject(nil, Name, NodeData)
        else
          TreeNode := FTreeView.Items.AddChildObject(ParentTreeNode, Name, NodeData);

        // Recursively load children
        LoadNode(TreeNode, Child);
      end;

      Child := Child.NextSibling;
    end;
  end;

begin
  if not FileExists(TomarConfigFile) then
    Exit;

  try
    ReadXMLFile(Doc, TomarConfigFile);
    try
      Root := Doc.DocumentElement;
      if Assigned(Root) then
        LoadNode(nil, Root);
    finally
      Doc.Free;
    end;
  except
    on E: Exception do
      ShowMessage('Error loading feeds: ' + E.Message);
  end;
end;

procedure TFormMain.SaveFeedList;
var
  Doc: TXMLDocument;
  Root: TDOMElement;

  procedure SaveNode(ParentXMLNode: TDOMElement; TreeNode: TTreeNode);
  var
    ItemNode: TDOMElement;
    NodeData: TFeedNodeData;
    NodeName: string;
    ParenPos: Integer;
  begin
    while Assigned(TreeNode) do
    begin
      ItemNode := Doc.CreateElement('item');
      ParentXMLNode.AppendChild(ItemNode);

      // Strip unread count from node name before saving
      NodeName := TreeNode.Text;
      ParenPos := Pos(' (', NodeName);
      if ParenPos > 0 then
        NodeName := Copy(NodeName, 1, ParenPos - 1);

      ItemNode.AppendChild(Doc.CreateElement('name'));
      ItemNode.LastChild.AppendChild(Doc.CreateTextNode(NodeName));

      NodeData := TFeedNodeData(TreeNode.Data);
      if Assigned(NodeData) then
      begin
        ItemNode.AppendChild(Doc.CreateElement('type'));
        if NodeData.IsFolder then
          ItemNode.LastChild.AppendChild(Doc.CreateTextNode('folder'))
        else
          ItemNode.LastChild.AppendChild(Doc.CreateTextNode('feed'));

        ItemNode.AppendChild(Doc.CreateElement('url'));
        ItemNode.LastChild.AppendChild(Doc.CreateTextNode(NodeData.FeedURL));
      end;

      // Save children
      if TreeNode.HasChildren then
        SaveNode(ItemNode, TreeNode.GetFirstChild);

      TreeNode := TreeNode.GetNextSibling;
    end;
  end;

begin
  Doc := TXMLDocument.Create;
  try
    Root := Doc.CreateElement('feeds');
    Doc.AppendChild(Root);

    if FTreeView.Items.Count > 0 then
      SaveNode(Root, FTreeView.Items.GetFirstNode);

    WriteXMLFile(Doc, TomarConfigFile);
  finally
    Doc.Free;
  end;
end;

procedure TFormMain.LoadRSSFeed(const AURL: string);
var
  Response: string;
  Doc: TXMLDocument;
  ItemNode, ChildNode: TDOMNode;
  Title, Link, Description, PubDate, VideoId, GuidText, IdText, ItemKey: string;
  Stream: TStringStream;
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

        MarkFeedItemsAsNotSeen(AURL);

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

            if IsYouTubeFeed then
              SaveOrUpdateFeedItem(AURL, ItemKey, Title, PubDate, Link, Link)
            else
              SaveOrUpdateFeedItem(AURL, ItemKey, Title, PubDate, Description, Link);
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

            if IsYouTubeFeed then
              SaveOrUpdateFeedItem(AURL, ItemKey, Title, PubDate, Link, Link)
            else
              SaveOrUpdateFeedItem(AURL, ItemKey, Title, PubDate, Description, Link);
          end;

          ItemNode := ItemNode.NextSibling;
        end;

        LoadFeedItemsFromDb(AURL, IsYouTubeFeed);

        if FListView.Items.Count = 0 then
          FHtmlPanel.SetHTMLFromStr('<html><body><p>No items found in feed.</p></body></html>')
        else
          FHtmlPanel.SetHTMLFromStr('<html><body><p>Select an item to view its content.</p></body></html>');

        UpdateAllFeedNodeTexts;

        FLoadingFeed := False;

        DebugLog('');
        DebugLog('=== Load Complete ===');
        DebugLog('Total items loaded from DB: ' + IntToStr(FListView.Items.Count));
        DebugLog('Unread items: ' + IntToStr(GetUnreadCount(AURL)));

      finally
        Doc.Free;
      end;
    finally
      Stream.Free;
    end;
  except
    on E: Exception do
    begin
      FLoadingFeed := False;
      ShowMessage('Error loading feed: ' + E.Message);
      FHtmlPanel.SetHTMLFromStr('<html><body><p style="color:red;">Error loading feed: ' +
                                E.Message + '</p></body></html>');
    end;
  end;
end;

procedure TFormMain.LoadFeedItemsFromDb(const AURL: string; IsYouTubeFeed: Boolean);
var
  ListItem: TListItem;
  ContentValue, LinkValue, ItemKeyValue: string;
  Entries: TList;
  Entry: TFeedListEntry;
  I: Integer;
begin
  Entries := TList.Create;
  FListView.Items.BeginUpdate;
  try
    FListView.Items.Clear;

    if not Assigned(FFeedItemsDb) or not FFeedItemsDb.Active then
      Exit;

    if FFeedItemsDb.RecordCount = 0 then
      Exit;

    FFeedItemsDb.First;
    while not FFeedItemsDb.EOF do
    begin
      if FFeedItemsDb.FieldByName('FEEDURL').AsString = AURL then
      begin
        Entry := TFeedListEntry.Create;
        Entry.Title := FFeedItemsDb.FieldByName('TITLE').AsString;
        Entry.PubDate := FFeedItemsDb.FieldByName('PUBDATE').AsString;
        Entry.ContentValue := FFeedItemsDb.FieldByName('CONTENT').AsString;
        Entry.LinkValue := FFeedItemsDb.FieldByName('LINK').AsString;
        Entry.ItemKeyValue := FFeedItemsDb.FieldByName('ITEMKEY').AsString;
        Entry.Unread := not IsItemRead(AURL, Entry.ItemKeyValue, Entry.LinkValue);
        Entry.SortKey := FeedDateSortKey(Entry.PubDate);
        Entries.Add(Entry);
      end;

      FFeedItemsDb.Next;
    end;

    Entries.Sort(@CompareFeedEntriesDescending);

    for I := 0 to Entries.Count - 1 do
    begin
      Entry := TFeedListEntry(Entries[I]);
      ListItem := FListView.Items.Add;
      ListItem.Caption := Entry.Title;
      ListItem.SubItems.Add(Entry.PubDate);

      ContentValue := Entry.ContentValue;
      LinkValue := Entry.LinkValue;
      ItemKeyValue := Entry.ItemKeyValue;

      if IsYouTubeFeed then
        ListItem.SubItems.Add(LinkValue)
      else
        ListItem.SubItems.Add(ContentValue);

      ListItem.SubItems.Add(LinkValue);
      ListItem.SubItems.Add(ItemKeyValue);

      if Entry.Unread then
        ListItem.Data := Pointer(1)
      else
        ListItem.Data := nil;
    end;
  finally
    for I := 0 to Entries.Count - 1 do
      TObject(Entries[I]).Free;
    Entries.Free;
    FListView.Items.EndUpdate;
  end;
end;

procedure TFormMain.MarkFeedItemsAsNotSeen(const AURL: string);
begin
  if not Assigned(FFeedItemsDb) or not FFeedItemsDb.Active then
    Exit;

  FFeedItemsDb.First;
  while not FFeedItemsDb.EOF do
  begin
    if FFeedItemsDb.FieldByName('FEEDURL').AsString = AURL then
    begin
      FFeedItemsDb.Edit;
      FFeedItemsDb.FieldByName('INLASTFETCH').AsBoolean := False;
      FFeedItemsDb.Post;
    end;
    FFeedItemsDb.Next;
  end;
end;

procedure TFormMain.SaveOrUpdateFeedItem(const AFeedURL, AItemKey, ATitle, APubDate, AContent, ALink: string);
var
  Found: Boolean;
begin
  if not Assigned(FFeedItemsDb) or not FFeedItemsDb.Active then
    Exit;

  Found := False;
  FFeedItemsDb.First;
  while not FFeedItemsDb.EOF do
  begin
    if (FFeedItemsDb.FieldByName('FEEDURL').AsString = AFeedURL) and
       (FFeedItemsDb.FieldByName('ITEMKEY').AsString = AItemKey) then
    begin
      Found := True;
      Break;
    end;
    FFeedItemsDb.Next;
  end;

  if Found then
    FFeedItemsDb.Edit
  else
  begin
    FFeedItemsDb.Append;
    FFeedItemsDb.FieldByName('FEEDURL').AsString := AFeedURL;
    FFeedItemsDb.FieldByName('ITEMKEY').AsString := AItemKey;
    FFeedItemsDb.FieldByName('FIRSTSEEN').AsDateTime := Now;
  end;

  FFeedItemsDb.FieldByName('TITLE').AsString := Copy(ATitle, 1, 255);
  FFeedItemsDb.FieldByName('PUBDATE').AsString := Copy(APubDate, 1, 127);
  FFeedItemsDb.FieldByName('CONTENT').AsString := AContent;
  FFeedItemsDb.FieldByName('LINK').AsString := Copy(ALink, 1, 255);
  FFeedItemsDb.FieldByName('LASTSEEN').AsDateTime := Now;
  FFeedItemsDb.FieldByName('INLASTFETCH').AsBoolean := True;
  FFeedItemsDb.Post;
end;

function TFormMain.MakeItemKey(const AGuid, AId, ALink, ATitle, APubDate: string): string;
begin
  if Trim(AGuid) <> '' then
    Exit(Copy(Trim(AGuid), 1, 255));

  if Trim(AId) <> '' then
    Exit(Copy(Trim(AId), 1, 255));

  if Trim(ALink) <> '' then
    Exit(Copy(Trim(ALink), 1, 255));

  Result := Copy(Trim(ATitle) + '|' + Trim(APubDate), 1, 255);
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
var
  DbPath: string;
begin
  DbPath := TomarDataDir;
  if not DirectoryExists(DbPath) then
    CreateDir(DbPath);

  FReadStatusDb := TDbf.Create(nil);
  FReadStatusDb.FilePathFull := DbPath;
  FReadStatusDb.TableName := 'readstatus.dbf';
  FReadStatusDb.TableLevel := 7;

  if not FileExists(DbPath + 'readstatus.dbf') then
  begin
    with FReadStatusDb.FieldDefs do
    begin
      Clear;
      Add('ID', ftAutoInc, 0, False);
      Add('FEEDURL', ftString, 255, True);
      Add('ITEMLINK', ftString, 255, True);
      Add('ITEMHASH', ftString, 32, True);
      Add('ISREAD', ftBoolean, 0, True);
      Add('DATEREAD', ftDateTime, 0, False);
    end;
    FReadStatusDb.CreateTable;
  end;

  FReadStatusDb.Open;
  try
    FReadStatusDb.RegenerateIndexes;
  finally
  end;

  FFeedItemsDb := TDbf.Create(nil);
  FFeedItemsDb.FilePathFull := DbPath;
  FFeedItemsDb.TableName := 'feeditems.dbf';
  FFeedItemsDb.TableLevel := 7;

  if not FileExists(DbPath + 'feeditems.dbf') then
  begin
    with FFeedItemsDb.FieldDefs do
    begin
      Clear;
      Add('ID', ftAutoInc, 0, False);
      Add('FEEDURL', ftString, 255, True);
      Add('ITEMKEY', ftString, 255, True);
      Add('TITLE', ftString, 255, False);
      Add('PUBDATE', ftString, 127, False);
      Add('CONTENT', ftMemo, 0, False);
      Add('LINK', ftString, 255, False);
      Add('FIRSTSEEN', ftDateTime, 0, False);
      Add('LASTSEEN', ftDateTime, 0, False);
      Add('INLASTFETCH', ftBoolean, 0, False);
    end;
    FFeedItemsDb.CreateTable;
  end;

  FFeedItemsDb.Open;
end;

procedure TFormMain.DebugLog(const S: string);
begin
{$IFDEF RSSREADER_DEBUG}
  if Assigned(FDebugLog) then
    FDebugLog.Add(S);
{$ENDIF}
end;

function TFormMain.ComputeItemHash(const AFeedURL, AItemKey: string): string;
var
  Combined: string;
begin
  Combined := AFeedURL + AItemKey;
  Result := MD5Print(MD5String(Combined));
end;

function TFormMain.IsItemRead(const AFeedURL, AItemKey: string; const ALegacyLink: string = ''): Boolean;
var
  StoredKey: string;
begin
  Result := False;
  if not Assigned(FReadStatusDb) or not FReadStatusDb.Active then
  begin
    DebugLog('IsItemRead: Database not active for ' + AItemKey);
    Exit;
  end;

  try
    FReadStatusDb.First;
    while not FReadStatusDb.EOF do
    begin
      StoredKey := FReadStatusDb.FieldByName('ITEMLINK').AsString;
      if (FReadStatusDb.FieldByName('FEEDURL').AsString = AFeedURL) and
         ((StoredKey = AItemKey) or ((ALegacyLink <> '') and (StoredKey = ALegacyLink))) then
      begin
        Result := True;
        DebugLog('IsItemRead: FOUND');
        Break;
      end;
      FReadStatusDb.Next;
    end;

    if not Result then
      DebugLog('IsItemRead: NOT FOUND');
  except
    on E: Exception do
    begin
      DebugLog('IsItemRead: ERROR - ' + E.Message + ' for ' + AItemKey);
      Result := False;
    end;
  end;
end;

procedure TFormMain.MarkItemAsRead(const AFeedURL, AItemKey: string; const ALegacyLink: string = '');
var
  ItemHash: string;
  OldRecordCount, NewRecordCount: Integer;
  OldState: TDataSetState;
begin
  if not Assigned(FReadStatusDb) or not FReadStatusDb.Active then
  begin
    DebugLog('ERROR: Database not active for: ' + AItemKey);
    Exit;
  end;

  if IsItemRead(AFeedURL, AItemKey, ALegacyLink) then
  begin
    DebugLog('SKIP: Already marked as read: ' + AItemKey);
    Exit;
  end;

  try
    ItemHash := ComputeItemHash(AFeedURL, AItemKey);
    OldRecordCount := FReadStatusDb.RecordCount;

    DebugLog(Format('  Attempting to add: Hash=%s, RecordCount before=%d', [ItemHash, OldRecordCount]));

    FReadStatusDb.Append;
    OldState := FReadStatusDb.State;
    DebugLog(Format('  After Append: State=%d (dsInsert=%d)', [Ord(OldState), Ord(dsInsert)]));

    FReadStatusDb.FieldByName('FEEDURL').AsString := AFeedURL;
    FReadStatusDb.FieldByName('ITEMLINK').AsString := Copy(AItemKey, 1, 255);
    FReadStatusDb.FieldByName('ITEMHASH').AsString := ItemHash;
    FReadStatusDb.FieldByName('ISREAD').AsBoolean := True;
    FReadStatusDb.FieldByName('DATEREAD').AsDateTime := Now;

    DebugLog(Format('  Fields set. FEEDURL len=%d, ITEMKEY len=%d, ITEMHASH len=%d',
                         [Length(AFeedURL), Length(AItemKey), Length(ItemHash)]));

    FReadStatusDb.Post;
    FReadStatusDb.CheckBrowseMode;

    if not IsItemRead(AFeedURL, AItemKey, ALegacyLink) then
      DebugLog('*** Insert failed: still not found: ' + AItemKey);

    OldState := FReadStatusDb.State;
    NewRecordCount := FReadStatusDb.RecordCount;

    DebugLog(Format('  After Post: State=%d (dsBrowse=%d), RecordCount=%d (was %d)',
                         [Ord(OldState), Ord(dsBrowse), NewRecordCount, OldRecordCount]));

    if NewRecordCount > OldRecordCount then
      DebugLog('OK: Marked (RecordCount increased): ' + AItemKey)
    else
      DebugLog('NOTE: RecordCount did not increase (may be normal); rely on IsItemRead verification above');
  except
    on E: Exception do
    begin
      DebugLog('EXCEPTION during Post: ' + E.ClassName + ' - ' + E.Message);
      ShowMessage('Error marking item as read: ' + AItemKey + #13#10 + E.Message);
    end;
  end;
end;

procedure TFormMain.MarkAllItemsAsRead(const AFeedURL: string);
var
  i: Integer;
  ItemLink, ItemKey: string;
begin
{$IFDEF RSSREADER_DEBUG}
  FDebugLog.Clear;
{$ENDIF}
  DebugLog('=== Mark All As Read Debug ===');
  DebugLog(Format('Feed URL parameter: "%s"', [AFeedURL]));
  DebugLog('Total items: ' + IntToStr(FListView.Items.Count));
  DebugLog('');

  // Mark all items in the current ListView as read
  for i := 0 to FListView.Items.Count - 1 do
  begin
    if FListView.Items[i].SubItems.Count > 2 then
    begin
      ItemLink := FListView.Items[i].SubItems[2];
      if FListView.Items[i].SubItems.Count > 3 then
        ItemKey := FListView.Items[i].SubItems[3]
      else
        ItemKey := ItemLink;
      DebugLog(Format('[%d] Key: %s', [i, ItemKey]));
      MarkItemAsRead(AFeedURL, ItemKey, ItemLink);
      FListView.Items[i].Data := nil; // Mark as read visually
    end
    else
    begin
      DebugLog(Format('[%d] SKIPPED: SubItems.Count=%d (need >2)',
                           [i, FListView.Items[i].SubItems.Count]));
    end;
  end;

  // Show debug log
{$IFDEF RSSREADER_DEBUG}

  ShowMessage(FDebugLog.Text);
{$ENDIF}

  UpdateAllFeedNodeTexts;
end;

function TFormMain.BuildReadKeyCache: TStringList;
var
  CacheKey: string;
begin
  Result := TStringList.Create;
  Result.Sorted := True;
  Result.Duplicates := dupIgnore;

  if not Assigned(FReadStatusDb) or not FReadStatusDb.Active then
    Exit;

  FReadStatusDb.First;
  while not FReadStatusDb.EOF do
  begin
    CacheKey := FReadStatusDb.FieldByName('FEEDURL').AsString + #9 +
                FReadStatusDb.FieldByName('ITEMLINK').AsString;
    if Result.IndexOf(CacheKey) < 0 then
      Result.Add(CacheKey);
    FReadStatusDb.Next;
  end;
end;

function TFormMain.FindCountIndex(ACounts: TStringList; const AFeedURL: string): Integer;
var
  i: Integer;
  Prefix: string;
begin
  Result := -1;
  if not Assigned(ACounts) then
    Exit;

  Prefix := AFeedURL + #9;
  for i := 0 to ACounts.Count - 1 do
    if Copy(ACounts[i], 1, Length(Prefix)) = Prefix then
      Exit(i);
end;

function TFormMain.GetCountValue(ACounts: TStringList; const AFeedURL: string): Integer;
var
  Idx: Integer;
  S: string;
begin
  Result := 0;
  Idx := FindCountIndex(ACounts, AFeedURL);
  if Idx < 0 then
    Exit;

  S := Copy(ACounts[Idx], Length(AFeedURL) + 2, MaxInt);
  Result := StrToIntDef(S, 0);
end;

procedure TFormMain.IncrementCountValue(ACounts: TStringList; const AFeedURL: string);
var
  Idx, N: Integer;
begin
  if not Assigned(ACounts) then
    Exit;

  Idx := FindCountIndex(ACounts, AFeedURL);
  if Idx < 0 then
    ACounts.Add(AFeedURL + #9 + '1')
  else
  begin
    N := GetCountValue(ACounts, AFeedURL);
    ACounts[Idx] := AFeedURL + #9 + IntToStr(N + 1);
  end;
end;

function TFormMain.GetUnreadCount(const AFeedURL: string): Integer;
var
  ReadKeys: TStringList;
  FeedURL, LinkValue, ItemKeyValue: string;
  ReadKey1, ReadKey2: string;
begin
  Result := 0;

  if not Assigned(FFeedItemsDb) or not FFeedItemsDb.Active then
    Exit;

  if FFeedItemsDb.RecordCount = 0 then
    Exit;

  ReadKeys := BuildReadKeyCache;
  try
    FFeedItemsDb.First;
    while not FFeedItemsDb.EOF do
    begin
      FeedURL := FFeedItemsDb.FieldByName('FEEDURL').AsString;
      if FeedURL = AFeedURL then
      begin
        LinkValue := FFeedItemsDb.FieldByName('LINK').AsString;
        ItemKeyValue := FFeedItemsDb.FieldByName('ITEMKEY').AsString;
        ReadKey1 := AFeedURL + #9 + ItemKeyValue;
        ReadKey2 := AFeedURL + #9 + LinkValue;
        if (ReadKeys.IndexOf(ReadKey1) < 0) and
           ((LinkValue = '') or (ReadKeys.IndexOf(ReadKey2) < 0)) then
          Inc(Result);
      end;
      FFeedItemsDb.Next;
    end;
  finally
    ReadKeys.Free;
  end;
end;

procedure TFormMain.UpdateAllFeedNodeTexts;
var
  i, UnreadCount: Integer;
  Node: TTreeNode;
  NodeData: TFeedNodeData;
  BaseText, FeedURL, LinkValue, ItemKeyValue, ReadKey1, ReadKey2: string;
  ReadKeys, Counts: TStringList;
begin
  if not Assigned(FTreeView) then
    Exit;
  if not Assigned(FFeedItemsDb) or not FFeedItemsDb.Active then
    Exit;

  ReadKeys := BuildReadKeyCache;
  Counts := TStringList.Create;
  try
    { Feed URLs can contain '=' and other characters, and we update entries in-place,
      so keep this list unsorted and do our own lookup. }
    Counts.Sorted := False;
    Counts.Duplicates := dupAccept;

    if FFeedItemsDb.RecordCount > 0 then
    begin
      FFeedItemsDb.First;
      while not FFeedItemsDb.EOF do
      begin
        FeedURL := FFeedItemsDb.FieldByName('FEEDURL').AsString;
        LinkValue := FFeedItemsDb.FieldByName('LINK').AsString;
        ItemKeyValue := FFeedItemsDb.FieldByName('ITEMKEY').AsString;

        ReadKey1 := FeedURL + #9 + ItemKeyValue;
        ReadKey2 := FeedURL + #9 + LinkValue;
        if (ReadKeys.IndexOf(ReadKey1) < 0) and
           ((LinkValue = '') or (ReadKeys.IndexOf(ReadKey2) < 0)) then
        begin
          IncrementCountValue(Counts, FeedURL);
        end;

        FFeedItemsDb.Next;
      end;
    end;

    FTreeView.Items.BeginUpdate;
    try
      for i := 0 to FTreeView.Items.Count - 1 do
      begin
        Node := FTreeView.Items[i];
        if Assigned(Node) and Assigned(Node.Data) then
        begin
          NodeData := TFeedNodeData(Node.Data);
          if Assigned(NodeData) and (not NodeData.IsFolder) then
          begin
            BaseText := Node.Text;
            if Pos(' (', BaseText) > 0 then
              BaseText := Copy(BaseText, 1, Pos(' (', BaseText) - 1);

            UnreadCount := GetCountValue(Counts, NodeData.FeedURL);
            if UnreadCount > 0 then
              Node.Text := BaseText + ' (' + IntToStr(UnreadCount) + ')'
            else
              Node.Text := BaseText;
          end;
        end;
      end;
    finally
      FTreeView.Items.EndUpdate;
    end;

    FTreeView.Invalidate;
  finally
    Counts.Free;
    ReadKeys.Free;
  end;
end;

procedure TFormMain.UpdateFeedNodeText(Node: TTreeNode);
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

  UnreadCount := GetUnreadCount(NodeData.FeedURL);

  BaseText := Node.Text;
  if Pos(' (', BaseText) > 0 then
    BaseText := Copy(BaseText, 1, Pos(' (', BaseText) - 1);

  if UnreadCount > 0 then
    Node.Text := BaseText + ' (' + IntToStr(UnreadCount) + ')'
  else
    Node.Text := BaseText;
end;

procedure TFormMain.MenuMarkAllReadClick(Sender: TObject);
var
  NodeData: TFeedNodeData;

  procedure MarkFeedsInNode(Node: TTreeNode);
  var
    ChildNode: TTreeNode;
    ChildData: TFeedNodeData;
    OldSelected: TTreeNode;
  begin
    if not Assigned(Node) then
      Exit;

    if Assigned(Node.Data) then
    begin
      ChildData := TFeedNodeData(Node.Data);

      if ChildData.IsFolder then
      begin
        // Recursively process children
        ChildNode := Node.GetFirstChild;
        while Assigned(ChildNode) do
        begin
          MarkFeedsInNode(ChildNode);
          ChildNode := ChildNode.GetNextSibling;
        end;
      end
      else
      begin
        // It is a feed - mark all its items as read
        // We need to load the feed first to mark items
        OldSelected := FTreeView.Selected;
        try
          FTreeView.Selected := Node;
          LoadRSSFeed(ChildData.FeedURL);
          MarkAllItemsAsRead(ChildData.FeedURL);
          //Application.ProcessMessages;
        finally
          FTreeView.Selected := OldSelected;
        end;
      end;
    end;
  end;

var
  MarkedCount: Integer;
begin
  NodeData := GetSelectedNodeData;
  if not Assigned(NodeData) then
    Exit;

  if NodeData.IsFolder then
  begin
    // Mark all feeds in this folder and subfolders
    MarkFeedsInNode(FTreeView.Selected);
    ShowMessage('All items in folder marked as read.');
  end
  else
  begin
    // Single feed
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
var
  ChannelId: string;
  PageSource: string;
  Stream: TStringStream;
  PosStart, PosEnd: Integer;
  CleanUrl: string;
begin
  Result := AUrl;

  // If it's already a feed URL, return as-is
  if Pos('feeds/videos.xml', AUrl) > 0 then
    Exit;

  // Check if it's a YouTube URL
  if (Pos('youtube.com', LowerCase(AUrl)) = 0) and
     (Pos('youtu.be', LowerCase(AUrl)) = 0) then
    Exit;

  // Clean URL - remove query parameters and trailing slashes for parsing
  CleanUrl := AUrl;
  PosStart := Pos('?', CleanUrl);   // yt feeds contain '?'
  if PosStart > 0 then
    CleanUrl := Copy(CleanUrl, 1, PosStart - 1);
  while (Length(CleanUrl) > 0) and (CleanUrl[Length(CleanUrl)] = '/') do
    Delete(CleanUrl, Length(CleanUrl), 1);

  // Case 1: /channel/CHANNEL_ID format
  PosStart := Pos('/channel/', CleanUrl);
  if PosStart > 0 then
  begin
    ChannelId := Copy(CleanUrl, PosStart + 9, Length(CleanUrl));
    // Extract just the channel ID (stop at next slash if any)
    PosEnd := Pos('/', ChannelId);
    if PosEnd > 0 then
      ChannelId := Copy(ChannelId, 1, PosEnd - 1);

    if ChannelId <> '' then
    begin
      Result := 'https://www.youtube.com/feeds/videos.xml?channel_id=' + ChannelId;
      Exit;
    end;
  end;

  // Case 2: /@handle or /user/USERNAME format - need to fetch page
  if (Pos('/@', CleanUrl) > 0) or (Pos('/user/', CleanUrl) > 0) or
     (Pos('/c/', CleanUrl) > 0) then
  begin
    Stream := TStringStream.Create('');
    try
      try
        // Fetch the channel page
        FHttpClient.Get(AUrl, Stream);
        PageSource := Stream.DataString;

        // PRIORITY 1: Look for RSS feed link in <head> section (most reliable)
        // Pattern: <link rel="alternate" type="application/rss+xml" ... href="https://www.youtube.com/feeds/videos.xml?channel_id=CHANNEL_ID">
        PosStart := Pos('rel="alternate" type="application/rss+xml"', PageSource);
        if PosStart > 0 then
        begin
          // Find the href attribute after this
          PosStart := Pos('href="', PageSource, PosStart);
          if PosStart > 0 then
          begin
            PosStart := PosStart + 6; // Length of 'href="'
            PosEnd := PosStart;
            while (PosEnd <= Length(PageSource)) and (PageSource[PosEnd] <> '"') do
              Inc(PosEnd);
            Result := Copy(PageSource, PosStart, PosEnd - PosStart);
            // We already have the complete feed URL, so we're done
            if Pos('feeds/videos.xml?channel_id=', Result) > 0 then
              Exit;
          end;
        end;

        // PRIORITY 2: Look for canonical URL in <head> section (also very reliable)
        // Pattern: <link rel="canonical" href="https://www.youtube.com/channel/CHANNEL_ID">
        PosStart := Pos('rel="canonical"', PageSource);
        if PosStart > 0 then
        begin
          PosStart := Pos('href="', PageSource, PosStart);
          if PosStart > 0 then
          begin
            PosStart := PosStart + 6; // Length of 'href="'
            PosEnd := PosStart;
            while (PosEnd <= Length(PageSource)) and (PageSource[PosEnd] <> '"') do
              Inc(PosEnd);
            ChannelId := Copy(PageSource, PosStart, PosEnd - PosStart);
            // Extract channel ID from the canonical URL
            PosStart := Pos('/channel/', ChannelId);
            if PosStart > 0 then
            begin
              ChannelId := Copy(ChannelId, PosStart + 9, Length(ChannelId));
              Result := 'https://www.youtube.com/feeds/videos.xml?channel_id=' + ChannelId;
              Exit;
            end;
          end;
        end;

        // FALLBACK: Search for channel_id in page source (less reliable, kept for backwards compatibility)
        // Only use this if the more reliable methods above failed
        PosStart := Pos('"channelId":"', PageSource);
        if PosStart > 0 then
        begin
          PosStart := PosStart + 13; // Length of '"channelId":"'
          PosEnd := PosStart;
          while (PosEnd <= Length(PageSource)) and (PageSource[PosEnd] <> '"') do
            Inc(PosEnd);
          ChannelId := Copy(PageSource, PosStart, PosEnd - PosStart);
        end
        else
        begin
          // Try alternate pattern
          PosStart := Pos('channel_id=', PageSource);
          if PosStart > 0 then
          begin
            PosStart := PosStart + 11; // Length of 'channel_id='
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
        begin
          // If fetching fails, return original URL
          // User will get an error when trying to load it as a feed
          Result := AUrl;
        end;
      end;
    finally
      Stream.Free;
    end;
  end;
end;


end.
