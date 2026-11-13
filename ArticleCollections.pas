unit ArticleCollections;

{ Per-article Watched/Favorites snapshots in ~/.tomar/data/collections.xml. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  COLLECTION_WATCHED = 1;
  COLLECTION_FAVORITES = 2;

type
  TStoredArticle = class
    FeedURL, ItemKey, Title, PubDate, Content, Link: string;
  end;

  TArticleCollections = class
  private
    FWatched, FFavorites: TList;
    FPath: string;
    function Items(AKind: Integer): TList;
    function Find(AList: TList; const AFeedURL, AItemKey: string): Integer;
    procedure Load;
    procedure Save;
  public
    constructor Create(const ADataDir: string);
    destructor Destroy; override;
    function Count(AKind: Integer): Integer;
    function ArticleAt(AKind, AIndex: Integer): TStoredArticle;
    function IsFavorite(const AFeedURL, AItemKey: string): Boolean;
    function ToggleFavorite(AArticle: TStoredArticle): Boolean; // owns AArticle
    procedure MarkWatched(AArticle: TStoredArticle); // owns AArticle
  end;

implementation

uses DOM, XMLRead, XMLWrite;

procedure FreeArticles(AList: TList);
var
  I: Integer;
begin
  for I := 0 to AList.Count - 1 do TObject(AList[I]).Free;
  AList.Clear;
end;

constructor TArticleCollections.Create(const ADataDir: string);
begin
  inherited Create;
  FWatched := TList.Create;
  FFavorites := TList.Create;
  ForceDirectories(ADataDir);
  FPath := IncludeTrailingPathDelimiter(ADataDir) + 'collections.xml';
  Load;
end;

destructor TArticleCollections.Destroy;
begin
  FreeArticles(FWatched);
  FreeArticles(FFavorites);
  FWatched.Free;
  FFavorites.Free;
  inherited Destroy;
end;

function TArticleCollections.Items(AKind: Integer): TList;
begin
  if AKind = COLLECTION_WATCHED then Result := FWatched
  else if AKind = COLLECTION_FAVORITES then Result := FFavorites
  else raise Exception.Create('Unknown collection');
end;

function TArticleCollections.Count(AKind: Integer): Integer;
begin
  Result := Items(AKind).Count;
end;

function TArticleCollections.ArticleAt(AKind, AIndex: Integer): TStoredArticle;
begin
  Result := TStoredArticle(Items(AKind)[AIndex]);
end;

function TArticleCollections.Find(AList: TList; const AFeedURL, AItemKey: string): Integer;
var
  I: Integer;
  Article: TStoredArticle;
begin
  Result := -1;
  for I := 0 to AList.Count - 1 do
  begin
    Article := TStoredArticle(AList[I]);
    if (Article.FeedURL = AFeedURL) and (Article.ItemKey = AItemKey) then
      Exit(I);
  end;
end;

function TArticleCollections.IsFavorite(const AFeedURL, AItemKey: string): Boolean;
begin
  Result := Find(FFavorites, AFeedURL, AItemKey) >= 0;
end;

function TArticleCollections.ToggleFavorite(AArticle: TStoredArticle): Boolean;
var
  I: Integer;
begin
  I := Find(FFavorites, AArticle.FeedURL, AArticle.ItemKey);
  if I >= 0 then
  begin
    TStoredArticle(FFavorites[I]).Free;
    FFavorites.Delete(I);
    AArticle.Free;
    Result := False;
  end
  else
  begin
    FFavorites.Insert(0, AArticle);
    Result := True;
  end;
  Save;
end;

procedure TArticleCollections.MarkWatched(AArticle: TStoredArticle);
var
  I: Integer;
begin
  I := Find(FWatched, AArticle.FeedURL, AArticle.ItemKey);
  if I >= 0 then
  begin
    TStoredArticle(FWatched[I]).Free;
    FWatched.Delete(I);
  end;
  FWatched.Insert(0, AArticle); // most recently watched first
  Save;
end;

procedure TArticleCollections.Load;
var
  Doc: TXMLDocument;
  Root, Section, N, F: TDOMNode;
  L: TList;
  Article: TStoredArticle;
begin
  if not FileExists(FPath) then Exit;
  ReadXMLFile(Doc, FPath);
  try
    Root := Doc.DocumentElement;
    if not Assigned(Root) then Exit;
    Section := Root.FirstChild;
    while Assigned(Section) do
    begin
      L := nil;
      if Section.NodeName = 'watched' then L := FWatched
      else if Section.NodeName = 'favorites' then L := FFavorites;
      if Assigned(L) then
      begin
        N := Section.FirstChild;
        while Assigned(N) do
        begin
          if N.NodeName = 'article' then
          begin
            Article := TStoredArticle.Create;
            F := N.FirstChild;
            while Assigned(F) do
            begin
              if F.NodeName = 'feedurl' then Article.FeedURL := F.TextContent
              else if F.NodeName = 'itemkey' then Article.ItemKey := F.TextContent
              else if F.NodeName = 'title' then Article.Title := F.TextContent
              else if F.NodeName = 'pubdate' then Article.PubDate := F.TextContent
              else if F.NodeName = 'content' then Article.Content := F.TextContent
              else if F.NodeName = 'link' then Article.Link := F.TextContent;
              F := F.NextSibling;
            end;
            if (Article.FeedURL <> '') and (Article.ItemKey <> '') then L.Add(Article)
            else Article.Free;
          end;
          N := N.NextSibling;
        end;
      end;
      Section := Section.NextSibling;
    end;
  finally
    Doc.Free;
  end;
end;

procedure TArticleCollections.Save;
var
  Doc: TXMLDocument;
  Root, Section, Item: TDOMElement;
  I, Kind: Integer;
  A: TStoredArticle;
  L: TList;
  TempPath: string;

  procedure AddText(AParent: TDOMElement; const AName, AValue: string);
  var E: TDOMElement;
  begin
    E := Doc.CreateElement(AName);
    E.AppendChild(Doc.CreateTextNode(AValue));
    AParent.AppendChild(E);
  end;
begin
  Doc := TXMLDocument.Create;
  TempPath := FPath + '.tmp';
  try
    Root := Doc.CreateElement('collections');
    Doc.AppendChild(Root);
    for Kind := COLLECTION_WATCHED to COLLECTION_FAVORITES do
    begin
      if Kind = COLLECTION_WATCHED then Section := Doc.CreateElement('watched')
      else Section := Doc.CreateElement('favorites');
      Root.AppendChild(Section);
      L := Items(Kind);
      for I := 0 to L.Count - 1 do
      begin
        A := TStoredArticle(L[I]);
        Item := Doc.CreateElement('article');
        Section.AppendChild(Item);
        AddText(Item, 'feedurl', A.FeedURL);
        AddText(Item, 'itemkey', A.ItemKey);
        AddText(Item, 'title', A.Title);
        AddText(Item, 'pubdate', A.PubDate);
        AddText(Item, 'content', A.Content);
        AddText(Item, 'link', A.Link);
      end;
    end;
    WriteXMLFile(Doc, TempPath);
    if not RenameFile(TempPath, FPath) then
      raise Exception.Create('Cannot save article collections to ' + FPath);
  finally
    Doc.Free;
  end;
end;

end.
