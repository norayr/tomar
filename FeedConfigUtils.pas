unit FeedConfigUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, DOM, XMLRead, XMLWrite, FeedModel, RssUtils;

procedure LoadFeedTreeFromConfig(ATreeView: TTreeView);
procedure SaveFeedTreeToConfig(ATreeView: TTreeView);

implementation

function StripTrailingCount(const S: string): string;
var
  i, j: Integer;
begin
  Result := Trim(S);

  i := Length(Result);
  if (i < 4) or (Result[i] <> ')') then
    Exit;

  j := i - 1;
  while (j > 0) and (Result[j] in ['0'..'9']) do
    Dec(j);

  if (j > 1) and (Result[j] = '(') and (Result[j - 1] = ' ') then
    Result := Copy(Result, 1, j - 2);
end;

procedure LoadFeedTreeFromConfig(ATreeView: TTreeView);
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

        Name := StripTrailingCount(Name);
        NodeData := TFeedNodeData.Create;
        NodeData.IsFolder := (NodeType = 'folder');
        NodeData.FeedURL := URL;

        if ParentTreeNode = nil then
          TreeNode := ATreeView.Items.AddObject(nil, Name, NodeData)
        else
          TreeNode := ATreeView.Items.AddChildObject(ParentTreeNode, Name, NodeData);

        LoadNode(TreeNode, Child);
      end;

      Child := Child.NextSibling;
    end;
  end;

begin
  if not Assigned(ATreeView) then
    Exit;

  if not FileExists(TomarConfigFile) then
    Exit;

  ReadXMLFile(Doc, TomarConfigFile);
  try
    Root := Doc.DocumentElement;
    if Assigned(Root) then
      LoadNode(nil, Root);
  finally
    Doc.Free;
  end;
end;

procedure SaveFeedTreeToConfig(ATreeView: TTreeView);
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

      if TreeNode.HasChildren then
        SaveNode(ItemNode, TreeNode.GetFirstChild);

      TreeNode := TreeNode.GetNextSibling;
    end;
  end;

begin
  if not Assigned(ATreeView) then
    Exit;

  Doc := TXMLDocument.Create;
  try
    Root := Doc.CreateElement('feeds');
    Doc.AppendChild(Root);

    if ATreeView.Items.Count > 0 then
      SaveNode(Root, ATreeView.Items.GetFirstNode);

    WriteXMLFile(Doc, TomarConfigFile);
  finally
    Doc.Free;
  end;
end;

end.
