unit FeedActionUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, Forms, FeedModel;

type
  TFeedActionProc = procedure(const AFeedURL: string) of object;

function ProcessFeedsInSubtree(ARootNode: TTreeNode; const AAction: TFeedActionProc): Integer;
function RefreshFeedsInSubtree(ARootNode: TTreeNode; const AAction: TFeedActionProc): Integer;
function RefreshAllFeeds(ATreeView: TTreeView; const AAction: TFeedActionProc): Integer;

implementation

function ProcessFeedsInSubtree(ARootNode: TTreeNode; const AAction: TFeedActionProc): Integer;
var
  ChildNode: TTreeNode;
  NodeData: TFeedNodeData;
begin
  Result := 0;
  if not Assigned(ARootNode) then
    Exit;

  if Assigned(ARootNode.Data) then
  begin
    NodeData := TFeedNodeData(ARootNode.Data);

    if NodeData.IsFolder then
    begin
      ChildNode := ARootNode.GetFirstChild;
      while Assigned(ChildNode) do
      begin
        Inc(Result, ProcessFeedsInSubtree(ChildNode, AAction));
        ChildNode := ChildNode.GetNextSibling;
      end;
    end
    else
    begin
      if Assigned(AAction) then
        AAction(NodeData.FeedURL);
      Inc(Result);
      Application.ProcessMessages;
    end;
  end;
end;

function RefreshFeedsInSubtree(ARootNode: TTreeNode; const AAction: TFeedActionProc): Integer;
begin
  Result := ProcessFeedsInSubtree(ARootNode, AAction);
end;

function RefreshAllFeeds(ATreeView: TTreeView; const AAction: TFeedActionProc): Integer;
var
  I: Integer;
  Node: TTreeNode;
  NodeData: TFeedNodeData;
begin
  Result := 0;
  if not Assigned(ATreeView) then
    Exit;

  for I := 0 to ATreeView.Items.Count - 1 do
  begin
    Node := ATreeView.Items[I];
    if Assigned(Node) and Assigned(Node.Data) then
    begin
      NodeData := TFeedNodeData(Node.Data);
      if not NodeData.IsFolder then
      begin
        if Assigned(AAction) then
          AAction(NodeData.FeedURL);
        Inc(Result);
        Application.ProcessMessages;
      end;
    end;
  end;
end;

end.
