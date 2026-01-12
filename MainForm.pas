unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, Menus, DOM, XMLRead, XMLWrite, fphttpclient, IpHtml, ipmsg, opensslsockets,
  FPImage, FPReadPNG, FPReadJPEG, FPReadGIF;

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
    FSplitter1, FSplitter2: TSplitter;
    FPopupMenu: TPopupMenu;
    FHttpClient: TFPHTTPClient;
    FDataProvider: TCustomHtmlDataProvider;

    procedure CreateControls;
    procedure TreeViewSelectionChanged(Sender: TObject);
    procedure ListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure TreeViewPopup(Sender: TObject);

    procedure MenuAddFolderClick(Sender: TObject);
    procedure MenuAddFeedClick(Sender: TObject);
    procedure MenuDeleteClick(Sender: TObject);
    procedure MenuRefreshClick(Sender: TObject);

    procedure LoadFeedList;
    procedure SaveFeedList;
    procedure LoadRSSFeed(const AURL: string);
    function GetSelectedNodeData: TFeedNodeData;
    procedure FreeNodeData(Node: TTreeNode);
  public

  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

uses
  LCLType, LCLIntf;

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
begin
  Caption := 'Տոմար';
  Width := 1000;
  Height := 700;
  Position := poScreenCenter;

  FHttpClient := TFPHTTPClient.Create(nil);
  FHttpClient.AllowRedirect := True;

  FDataProvider := TCustomHtmlDataProvider.Create(Self);

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
  FListView.ViewStyle := vsReport;
  FListView.RowSelect := True;
  FListView.ReadOnly := True;
  FListView.OnSelectItem := @ListViewSelectItem;

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
end;

procedure TFormMain.TreeViewSelectionChanged(Sender: TObject);
var
  NodeData: TFeedNodeData;
begin
  if FTreeView.Selected = nil then
    Exit;

  NodeData := GetSelectedNodeData;
  if Assigned(NodeData) and not NodeData.IsFolder then
    LoadRSSFeed(NodeData.FeedURL);
end;

procedure TFormMain.ListViewSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  Content: string;
  VideoId: string;
  HtmlContent: string;
  PosStart, PosEnd: Integer;
begin
  if not Selected or (Item = nil) then
    Exit;

  // Item.SubItems[1] contains the content (description or YouTube URL)
  if Item.SubItems.Count > 1 then
  begin
    Content := Item.SubItems[1];
    
    // Check if this is a YouTube URL
    if (Pos('youtube.com/watch?v=', Content) > 0) or
       (Pos('youtu.be/', Content) > 0) then
    begin
      // Extract video ID
      VideoId := '';
      if Pos('youtube.com/watch?v=', Content) > 0 then
      begin
        PosStart := Pos('v=', Content) + 2;
        PosEnd := Pos('&', Content);
        if PosEnd = 0 then
          VideoId := Copy(Content, PosStart, Length(Content))
        else
          VideoId := Copy(Content, PosStart, PosEnd - PosStart);
      end
      else if Pos('youtu.be/', Content) > 0 then
      begin
        PosStart := Pos('youtu.be/', Content) + 9;
        PosEnd := Pos('?', Content);
        if PosEnd = 0 then
          VideoId := Copy(Content, PosStart, Length(Content))
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
      FHtmlPanel.SetHTMLFromStr(HtmlContent);
    end
    else
    begin
      // Regular RSS feed content
      FHtmlPanel.SetHTMLFromStr(Content);
    end;
  end;
end;

procedure TFormMain.TreeViewPopup(Sender: TObject);
var
  NodeData: TFeedNodeData;
begin
  // Enable/disable menu items based on selection
  NodeData := GetSelectedNodeData;

  // Refresh only available for feeds, not folders
  FPopupMenu.Items[3].Enabled := Assigned(NodeData) and not NodeData.IsFolder;
end;

procedure TFormMain.MenuAddFolderClick(Sender: TObject);
var
  FolderName: string;
  NewNode: TTreeNode;
  NodeData: TFeedNodeData;
begin
  FolderName := InputBox('Add Folder', 'Folder name:', '');
  if FolderName = '' then
    Exit;

  NodeData := TFeedNodeData.Create;
  NodeData.IsFolder := True;
  NodeData.FeedURL := '';

  if FTreeView.Selected = nil then
    NewNode := FTreeView.Items.AddObject(nil, FolderName, NodeData)
  else
    NewNode := FTreeView.Items.AddChildObject(FTreeView.Selected, FolderName, NodeData);

  SaveFeedList;
end;

procedure TFormMain.MenuAddFeedClick(Sender: TObject);
var
  FeedName, FeedURL: string;
  NewNode: TTreeNode;
  NodeData: TFeedNodeData;
  ParentNode: TTreeNode;
begin
  FeedName := InputBox('Add Feed', 'Feed name:', '');
  if FeedName = '' then
    Exit;

  FeedURL := InputBox('Add Feed', 'Feed URL:', '');
  if FeedURL = '' then
    Exit;

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
    NewNode := FTreeView.Items.AddObject(nil, FeedName, NodeData)
  else
    NewNode := FTreeView.Items.AddChildObject(ParentNode, FeedName, NodeData);

  SaveFeedList;
end;

procedure TFormMain.MenuDeleteClick(Sender: TObject);
var
  Node: TTreeNode;
begin
  Node := FTreeView.Selected;
  if Node = nil then
    Exit;

  if MessageDlg('Delete', 'Delete "' + Node.Text + '"?',
                 mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
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

procedure TFormMain.LoadFeedList;
var
  Doc: TXMLDocument;
  Root, ItemNode, ChildNode: TDOMNode;

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
    i: Integer;
  begin
    while Assigned(TreeNode) do
    begin
      ItemNode := Doc.CreateElement('item');
      ParentXMLNode.AppendChild(ItemNode);

      ItemNode.AppendChild(Doc.CreateElement('name'));
      ItemNode.LastChild.AppendChild(Doc.CreateTextNode(TreeNode.Text));

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
  FListView.Items.Clear;
  FHtmlPanel.SetHTMLFromStr('<html><body><p>Loading...</p></body></html>');
  Application.ProcessMessages;

  try
    Response := FHttpClient.Get(AURL);

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
          end;

          ItemNode := ItemNode.NextSibling;
        end;

        if FListView.Items.Count = 0 then
          FHtmlPanel.SetHTMLFromStr('<html><body><p>No items found in feed.</p></body></html>')
        else
          FHtmlPanel.SetHTMLFromStr('<html><body><p>Select an item to view its content.</p></body></html>');

      finally
        Doc.Free;
      end;
    finally
      Stream.Free;
    end;
  except
    on E: Exception do
    begin
      ShowMessage('Error loading feed: ' + E.Message);
      FHtmlPanel.SetHTMLFromStr('<html><body><p style="color:red;">Error loading feed: ' +
                                E.Message + '</p></body></html>');
    end;
  end;
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

end.
