unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, Menus, DOM, XMLRead, XMLWrite, fphttpclient, IpHtml, ipmsg, opensslsockets,
  FPImage, FPReadPNG, FPReadJPEG, FPReadGIF, db, dbf, md5, Clipbrd;

type
  TFeedNodeData = class
    FeedURL: string;
    IsFolder: Boolean;
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
    FLoadingFeed: Boolean; // Flag to prevent selection during loading
    FInSelectItem: Boolean;

    procedure CreateControls;
    procedure InitializeDatabase;
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
    procedure ListViewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    procedure LoadFeedList;
    procedure SaveFeedList;
    procedure LoadRSSFeed(const AURL: string);
    function GetSelectedNodeData: TFeedNodeData;
    procedure FreeNodeData(Node: TTreeNode);
    
    function ComputeItemHash(const AFeedURL, AItemLink: string): string;
    function IsItemRead(const AFeedURL, AItemLink: string): Boolean;
    procedure MarkItemAsRead(const AFeedURL, AItemLink: string);
    procedure MarkAllItemsAsRead(const AFeedURL: string);
    function GetUnreadCount(const AFeedURL: string): Integer;
    procedure UpdateFeedNodeText(Node: TTreeNode);

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
  CONFIG_FILE = 'feeds.xml';

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

  FTreeView.PopupMenu.OnPopup := @TreeViewPopup;

  // Splitter
  FSplitter1 := TSplitter.Create(Self);
  FSplitter1.Parent := Self;
  FSplitter1.Align := alLeft;
  FSplitter1.Width := 5;

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
  FSplitter2.Height := 5;

  // Bottom panel with HTML viewer
  BottomPanel := TPanel.Create(Self);
  BottomPanel.Parent := RightPanel;
  BottomPanel.Align := alClient;
  BottomPanel.Caption := '';
  BottomPanel.BevelOuter := bvNone;

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
  FeedURL, ItemLink: string;
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
            MarkItemAsRead(FeedURL, ItemLink);
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

      // Check if this is a YouTube URL
      if (Pos('youtube.com/watch?v=', Content) > 0) or
         (Pos('youtu.be/', Content) > 0) or
         (Pos('youtube.com/shorts/', Content) > 0) then
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
begin
  //showmessage ('entered treeviewselectionchanged');
  if FTreeView.Selected = nil then
    Exit;

  NodeData := GetSelectedNodeData;
  if Assigned(NodeData) and not NodeData.IsFolder then
    LoadRSSFeed(NodeData.FeedURL);
  //showmessage ('exiting treeviewselectionchanged');
end;

procedure TFormMain.ListViewSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  Content: string;
  VideoId: string;
  HtmlContent: string;
  PosStart, PosEnd: Integer;
  FeedURL, ItemLink: string;
  NodeData: TFeedNodeData;
  ShouldMarkAsRead: Boolean;
begin
  ShowMessage('entered listviewselectitem');
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
            ShouldMarkAsRead := True;
          end;
        end;
      end;
    end;

    // Item.SubItems[1] contains the content (description or YouTube URL)
    if Item.SubItems.Count > 1 then
    begin
      Content := Item.SubItems[1];

      // Check if this is a YouTube URL
      if (Pos('youtube.com/watch?v=', Content) > 0) or
         (Pos('youtu.be/', Content) > 0) or
         (Pos('youtube.com/shorts/', Content) > 0) then
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
      MarkItemAsRead(FeedURL, ItemLink);
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
    // If it's a feed (not folder), delete its database entries
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
        // It's a feed - refresh it
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
  if not FileExists(CONFIG_FILE) then
    Exit;

  try
    ReadXMLFile(Doc, CONFIG_FILE);
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

    WriteXMLFile(Doc, CONFIG_FILE);
  finally
    Doc.Free;
  end;
end;

procedure TFormMain.LoadRSSFeed(const AURL: string);
var
  Response: string;
  Doc: TXMLDocument;
  ItemNode, ChildNode: TDOMNode;
  Title, Link, Description, PubDate, VideoId: string;
  ListItem: TListItem;
  Stream: TStringStream;
  IsYouTubeFeed: Boolean;
begin
  //showmessage ('entered loadrssfeed');
  FLoadingFeed := True; // Prevent selection events during loading
  FListView.Items.Clear;
  FHtmlPanel.SetHTMLFromStr('<html><body><p>Loading...</p></body></html>');
  Application.ProcessMessages;

  try
    //Response := FHttpClient.Get(AURL);
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
        Exit; // Exit the procedure
      end;
    end;


    Stream := TStringStream.Create(Response);
    try
      ReadXMLFile(Doc, Stream);
      try
        // Check if this is a YouTube feed
        IsYouTubeFeed := False;
        if Assigned(Doc.DocumentElement) then
        begin
          // Check for YouTube namespace or yt:channelId
          if (Doc.DocumentElement.FindNode('yt:channelId') <> nil) or
             (Pos('youtube.com', AURL) > 0) then
            IsYouTubeFeed := True;
        end;

        // Try RSS format first
        ItemNode := Doc.DocumentElement.FindNode('channel');
        if not Assigned(ItemNode) then
          ItemNode := Doc.DocumentElement; // Might be Atom

        ItemNode := ItemNode.FirstChild;
        while Assigned(ItemNode) do
        begin
          if ItemNode.NodeName = 'item' then
          begin
            Title := '';
            Link := '';
            Description := '';
            PubDate := '';

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
                PubDate := ChildNode.TextContent;

              ChildNode := ChildNode.NextSibling;
            end;

            ListItem := FListView.Items.Add;
            ListItem.Caption := Title;
            ListItem.SubItems.Add(PubDate);
            if IsYouTubeFeed then
              ListItem.SubItems.Add(Link)  // For YouTube, only show the video URL
            else
              ListItem.SubItems.Add(Description);
            ListItem.SubItems.Add(Link);
            
            // Mark unread items with bold font
            if not IsItemRead(AURL, Link) then
            begin
              ListItem.Data := Pointer(1); // Mark as unread
            end
            else
            begin
              ListItem.Data := nil; // Mark as read
            end;
          end
          else if ItemNode.NodeName = 'entry' then // Atom format
          begin
            Title := '';
            Link := '';
            Description := '';
            PubDate := '';
            VideoId := '';

            ChildNode := ItemNode.FirstChild;
            while Assigned(ChildNode) do
            begin
              if ChildNode.NodeName = 'title' then
                Title := ChildNode.TextContent
              else if ChildNode.NodeName = 'link' then
              begin
                // For YouTube, prefer link with rel="alternate"
                if ChildNode.Attributes.GetNamedItem('rel') <> nil then
                begin
                  if ChildNode.Attributes.GetNamedItem('rel').NodeValue = 'alternate' then
                  begin
                    if ChildNode.Attributes.GetNamedItem('href') <> nil then
                      Link := ChildNode.Attributes.GetNamedItem('href').NodeValue;
                  end;
                end
                else if ChildNode.Attributes.GetNamedItem('href') <> nil then
                  Link := ChildNode.Attributes.GetNamedItem('href').NodeValue;
              end
              else if ChildNode.NodeName = 'yt:videoId' then
                VideoId := ChildNode.TextContent
              else if (ChildNode.NodeName = 'summary') or
                      (ChildNode.NodeName = 'content') then
                Description := ChildNode.TextContent
              else if (ChildNode.NodeName = 'published') or
                      (ChildNode.NodeName = 'updated') then
                PubDate := ChildNode.TextContent;

              ChildNode := ChildNode.NextSibling;
            end;

            // If we have videoId but no link, construct YouTube URL
            if IsYouTubeFeed and (Link = '') and (VideoId <> '') then
              Link := 'https://www.youtube.com/watch?v=' + VideoId;

            ListItem := FListView.Items.Add;
            ListItem.Caption := Title;
            ListItem.SubItems.Add(PubDate);
            if IsYouTubeFeed then
              ListItem.SubItems.Add(Link)  // For YouTube, only show the video URL
            else
              ListItem.SubItems.Add(Description);
            ListItem.SubItems.Add(Link);
            
            // Mark unread items with bold font
            if not IsItemRead(AURL, Link) then
            begin
              ListItem.Data := Pointer(1); // Mark as unread
            end
            else
            begin
              ListItem.Data := nil; // Mark as read
            end;
          end;

          ItemNode := ItemNode.NextSibling;
        end;

        if FListView.Items.Count = 0 then
          FHtmlPanel.SetHTMLFromStr('<html><body><p>No items found in feed.</p></body></html>')
        else
        begin
          FHtmlPanel.SetHTMLFromStr('<html><body><p>Select an item to view its content.</p></body></html>');
          // Update tree node text with unread count
          if Assigned(FTreeView.Selected) then
            UpdateFeedNodeText(FTreeView.Selected);
        end;

        // Prevent auto-selection from marking first item as read later
        if Assigned(FListView) then
        begin
          FListView.Selected := nil;
          FListView.ItemFocused := nil;
          FListView.ItemIndex := -1;
          // Also clear any internal selection state by calling ClearSelection
          FListView.ClearSelection;

          // Force a repaint to ensure visual state is cleared
          FListView.Invalidate;
        end;

        FLoadingFeed := False; // Re-enable selection events

      finally
        Doc.Free;
      end;
    finally
      Stream.Free;
    end;
  except
    on E: Exception do
    begin
      FLoadingFeed := False; // Re-enable selection events
      ShowMessage('Error loading feed: ' + E.Message);
      FHtmlPanel.SetHTMLFromStr('<html><body><p style="color:red;">Error loading feed: ' +
                                E.Message + '</p></body></html>');
    end;
  end;
  //showmessage ('exiting loadrssfeed');
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
  NeedRecreate: Boolean;
  i: Integer;
begin
  // Create database directory if it doesn't exist
  DbPath := 'data' + DirectorySeparator;
  if not DirectoryExists(DbPath) then
    CreateDir(DbPath);

  FReadStatusDb := TDbf.Create(nil);
  FReadStatusDb.FilePathFull := DbPath;
  FReadStatusDb.TableName := 'readstatus.dbf';
  FReadStatusDb.TableLevel := 7; // Visual dBase VII (supports ftAutoInc)

  // Check if we need to recreate the table (old structure without ITEMHASH)
  NeedRecreate := False;
  if FileExists(DbPath + 'readstatus.dbf') then
  begin
    try
      FReadStatusDb.Open;
      // Check if ITEMHASH field exists
      NeedRecreate := FReadStatusDb.FindField('ITEMHASH') = nil;
      FReadStatusDb.Close;
      
      if NeedRecreate then
      begin
        // Delete old database files
        DeleteFile(DbPath + 'readstatus.dbf');
        DeleteFile(DbPath + 'readstatus.dbt');
        DeleteFile(DbPath + 'readstatus.mdx');
        // Delete any index files
        for i := 0 to 9 do
        begin
          DeleteFile(DbPath + 'readstatus.cdx');
          DeleteFile(DbPath + 'readstatus.ndx');
          DeleteFile(DbPath + 'readstatus.id' + IntToStr(i));
        end;
      end;
    except
      // If there's an error opening, recreate anyway
      NeedRecreate := True;
    end;
  end;

  // Create table if it doesn't exist or needs recreation
  if not FileExists(DbPath + 'readstatus.dbf') then
  begin
    with FReadStatusDb.FieldDefs do
    begin
      Add('ID', ftAutoInc, 0, False);
      Add('FEEDURL', ftString, 255, True);
      Add('ITEMLINK', ftString, 255, True);
      Add('ITEMHASH', ftString, 32, True); // MD5 hash of FEEDURL+ITEMLINK for indexing
      Add('ISREAD', ftBoolean, 0, True);
      Add('DATEREAD', ftDateTime, 0, False);
    end;
    FReadStatusDb.CreateTable;
    
    // Open with exclusive access to add index
    FReadStatusDb.Exclusive := True;
    FReadStatusDb.Open;
    
    // Add index only when creating the table
    FReadStatusDb.AddIndex('idxItemHash', 'ITEMHASH', [ixUnique]);
    
    FReadStatusDb.Close;
    FReadStatusDb.Exclusive := False;
  end;

  // Open the database normally
  FReadStatusDb.Open;
  
  // Set the index as active for fast lookups
  try
    FReadStatusDb.IndexName := 'idxItemHash';
  except
    // Ignore if index doesn't exist yet
  end;
end;

function TFormMain.ComputeItemHash(const AFeedURL, AItemLink: string): string;
begin
  Result := MD5Print(MD5String(AFeedURL + AItemLink));
end;

function TFormMain.IsItemRead(const AFeedURL, AItemLink: string): Boolean;
var
  ItemHash: string;
begin
  Result := False;
  if not Assigned(FReadStatusDb) or not FReadStatusDb.Active then
    Exit;

  try
    ItemHash := ComputeItemHash(AFeedURL, AItemLink);
    
    // Use Locate for indexed lookup
    Result := FReadStatusDb.Locate('ITEMHASH', ItemHash, []);
  except
    Result := False;
  end;
end;

procedure TFormMain.MarkItemAsRead(const AFeedURL, AItemLink: string);
var
  ItemHash: string;
begin
  //showmessage ('entered markitemasread');
  if not Assigned(FReadStatusDb) or not FReadStatusDb.Active then
    Exit;

  // Check if already marked as read
  if IsItemRead(AFeedURL, AItemLink) then
    Exit;

  try
    ItemHash := ComputeItemHash(AFeedURL, AItemLink);
    
    FReadStatusDb.Append;
    FReadStatusDb.FieldByName('FEEDURL').AsString := AFeedURL;
    FReadStatusDb.FieldByName('ITEMLINK').AsString := AItemLink;
    FReadStatusDb.FieldByName('ITEMHASH').AsString := ItemHash;
    FReadStatusDb.FieldByName('ISREAD').AsBoolean := True;
    FReadStatusDb.FieldByName('DATEREAD').AsDateTime := Now;
    FReadStatusDb.Post;
  except
    on E: Exception do
      ShowMessage('Error marking item as read: ' + E.Message);
  end;
  //showmessage ('exiting markitemasread');
end;

procedure TFormMain.MarkAllItemsAsRead(const AFeedURL: string);
var
  i: Integer;
  ItemLink: string;
begin
  // Mark all items in the current ListView as read
  for i := 0 to FListView.Items.Count - 1 do
  begin
    if FListView.Items[i].SubItems.Count > 2 then
    begin
      ItemLink := FListView.Items[i].SubItems[2];
      MarkItemAsRead(AFeedURL, ItemLink);
      FListView.Items[i].Data := nil; // Mark as read visually
    end;
  end;
  
  // Update the tree node text
  if Assigned(FTreeView.Selected) then
    UpdateFeedNodeText(FTreeView.Selected);
end;

function TFormMain.GetUnreadCount(const AFeedURL: string): Integer;
var
  i: Integer;
  CurrentNodeData: TFeedNodeData;
begin
  Result := 0;
  
  // Only count if this feed is currently loaded in the ListView
  if Assigned(FTreeView.Selected) and Assigned(FTreeView.Selected.Data) then
  begin
    CurrentNodeData := TFeedNodeData(FTreeView.Selected.Data);
    if Assigned(CurrentNodeData) and (CurrentNodeData.FeedURL = AFeedURL) then
    begin
      // Count unread items in the ListView
      for i := 0 to FListView.Items.Count - 1 do
      begin
        if FListView.Items[i].Data = Pointer(1) then
          Inc(Result);
      end;
    end;
  end;
  // If feed is not loaded, return 0 (no count shown)
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
  
  // Get base name without unread count
  BaseText := Node.Text;
  if Pos(' (', BaseText) > 0 then
    BaseText := Copy(BaseText, 1, Pos(' (', BaseText) - 1);
  
  // Update node text with unread count
  if UnreadCount > 0 then
    Node.Text := BaseText + ' (' + IntToStr(UnreadCount) + ')'
  else
    Node.Text := BaseText;

  FTreeView.Invalidate;
  FTreeView.Update;
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
        // It's a feed - mark all its items as read
        // We need to load the feed first to mark items
        OldSelected := FTreeView.Selected;
        try
          FTreeView.Selected := Node;
          LoadRSSFeed(ChildData.FeedURL);
          MarkAllItemsAsRead(ChildData.FeedURL);
          Application.ProcessMessages;
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
