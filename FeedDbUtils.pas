unit FeedDbUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, dbf;

type
  TLogProc = procedure(const S: string) of object;

procedure InitializeDatabases(out AReadStatusDb, AFeedItemsDb: TDbf; const ADataPath: string);
procedure MarkFeedItemsAsNotSeen(AFeedItemsDb: TDbf; const AURL: string);
procedure SaveOrUpdateFeedItem(AFeedItemsDb: TDbf;
  const AFeedURL, AItemKey, ATitle, APubDate, AContent, ALink: string);
function IsItemRead(AReadStatusDb: TDbf; const AFeedURL, AItemKey: string;
  const ALegacyLink: string = ''; ALogProc: TLogProc = nil): Boolean;
procedure MarkItemAsRead(AReadStatusDb: TDbf; const AFeedURL, AItemKey: string;
  const ALegacyLink: string = ''; ALogProc: TLogProc = nil);
function BuildReadKeyCache(AReadStatusDb: TDbf): TStringList;
function FindCountIndex(ACounts: TStringList; const AFeedURL: string): Integer;
function GetCountValue(ACounts: TStringList; const AFeedURL: string): Integer;
procedure IncrementCountValue(ACounts: TStringList; const AFeedURL: string);
function GetUnreadCount(AFeedItemsDb, AReadStatusDb: TDbf; const AFeedURL: string): Integer;

implementation

uses
  md5;

procedure LogMessage(ALogProc: TLogProc; const S: string);
begin
  if Assigned(ALogProc) then
    ALogProc(S);
end;

function ComputeItemHash(const AFeedURL, AItemKey: string): string;
var
  Combined: string;
begin
  Combined := AFeedURL + AItemKey;
  Result := MD5Print(MD5String(Combined));
end;

procedure InitializeDatabases(out AReadStatusDb, AFeedItemsDb: TDbf; const ADataPath: string);
begin
  if not DirectoryExists(ADataPath) then
    CreateDir(ADataPath);

  AReadStatusDb := TDbf.Create(nil);
  AReadStatusDb.FilePathFull := ADataPath;
  AReadStatusDb.TableName := 'readstatus.dbf';
  AReadStatusDb.TableLevel := 7;

  if not FileExists(ADataPath + 'readstatus.dbf') then
  begin
    with AReadStatusDb.FieldDefs do
    begin
      Clear;
      Add('ID', ftAutoInc, 0, False);
      Add('FEEDURL', ftString, 255, True);
      Add('ITEMLINK', ftString, 255, True);
      Add('ITEMHASH', ftString, 32, True);
      Add('ISREAD', ftBoolean, 0, True);
      Add('DATEREAD', ftDateTime, 0, False);
    end;
    AReadStatusDb.CreateTable;
  end;

  AReadStatusDb.Open;
  AReadStatusDb.RegenerateIndexes;

  AFeedItemsDb := TDbf.Create(nil);
  AFeedItemsDb.FilePathFull := ADataPath;
  AFeedItemsDb.TableName := 'feeditems.dbf';
  AFeedItemsDb.TableLevel := 7;

  if not FileExists(ADataPath + 'feeditems.dbf') then
  begin
    with AFeedItemsDb.FieldDefs do
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
    AFeedItemsDb.CreateTable;
  end;

  AFeedItemsDb.Open;
end;

procedure MarkFeedItemsAsNotSeen(AFeedItemsDb: TDbf; const AURL: string);
begin
  if not Assigned(AFeedItemsDb) or not AFeedItemsDb.Active then
    Exit;

  AFeedItemsDb.First;
  while not AFeedItemsDb.EOF do
  begin
    if AFeedItemsDb.FieldByName('FEEDURL').AsString = AURL then
    begin
      AFeedItemsDb.Edit;
      AFeedItemsDb.FieldByName('INLASTFETCH').AsBoolean := False;
      AFeedItemsDb.Post;
    end;
    AFeedItemsDb.Next;
  end;
end;

procedure SaveOrUpdateFeedItem(AFeedItemsDb: TDbf;
  const AFeedURL, AItemKey, ATitle, APubDate, AContent, ALink: string);
var
  Found: Boolean;
begin
  if not Assigned(AFeedItemsDb) or not AFeedItemsDb.Active then
    Exit;

  Found := False;
  AFeedItemsDb.First;
  while not AFeedItemsDb.EOF do
  begin
    if (AFeedItemsDb.FieldByName('FEEDURL').AsString = AFeedURL) and
       (AFeedItemsDb.FieldByName('ITEMKEY').AsString = AItemKey) then
    begin
      Found := True;
      Break;
    end;
    AFeedItemsDb.Next;
  end;

  if Found then
    AFeedItemsDb.Edit
  else
  begin
    AFeedItemsDb.Append;
    AFeedItemsDb.FieldByName('FEEDURL').AsString := AFeedURL;
    AFeedItemsDb.FieldByName('ITEMKEY').AsString := AItemKey;
    AFeedItemsDb.FieldByName('FIRSTSEEN').AsDateTime := Now;
  end;

  AFeedItemsDb.FieldByName('TITLE').AsString := Copy(ATitle, 1, 255);
  AFeedItemsDb.FieldByName('PUBDATE').AsString := Copy(APubDate, 1, 127);
  AFeedItemsDb.FieldByName('CONTENT').AsString := AContent;
  AFeedItemsDb.FieldByName('LINK').AsString := Copy(ALink, 1, 255);
  AFeedItemsDb.FieldByName('LASTSEEN').AsDateTime := Now;
  AFeedItemsDb.FieldByName('INLASTFETCH').AsBoolean := True;
  AFeedItemsDb.Post;
end;

function IsItemRead(AReadStatusDb: TDbf; const AFeedURL, AItemKey: string;
  const ALegacyLink: string = ''; ALogProc: TLogProc = nil): Boolean;
var
  StoredKey: string;
begin
  Result := False;
  if not Assigned(AReadStatusDb) or not AReadStatusDb.Active then
  begin
    LogMessage(ALogProc, 'IsItemRead: Database not active for ' + AItemKey);
    Exit;
  end;

  try
    AReadStatusDb.First;
    while not AReadStatusDb.EOF do
    begin
      StoredKey := AReadStatusDb.FieldByName('ITEMLINK').AsString;
      if (AReadStatusDb.FieldByName('FEEDURL').AsString = AFeedURL) and
         ((StoredKey = AItemKey) or ((ALegacyLink <> '') and (StoredKey = ALegacyLink))) then
      begin
        Result := True;
        LogMessage(ALogProc, 'IsItemRead: FOUND');
        Break;
      end;
      AReadStatusDb.Next;
    end;

    if not Result then
      LogMessage(ALogProc, 'IsItemRead: NOT FOUND');
  except
    on E: Exception do
    begin
      LogMessage(ALogProc, 'IsItemRead: ERROR - ' + E.Message + ' for ' + AItemKey);
      Result := False;
    end;
  end;
end;

procedure MarkItemAsRead(AReadStatusDb: TDbf; const AFeedURL, AItemKey: string;
  const ALegacyLink: string = ''; ALogProc: TLogProc = nil);
var
  ItemHash: string;
  OldRecordCount, NewRecordCount: Integer;
  OldState: TDataSetState;
begin
  if not Assigned(AReadStatusDb) or not AReadStatusDb.Active then
  begin
    LogMessage(ALogProc, 'ERROR: Database not active for: ' + AItemKey);
    Exit;
  end;

  if IsItemRead(AReadStatusDb, AFeedURL, AItemKey, ALegacyLink, ALogProc) then
  begin
    LogMessage(ALogProc, 'SKIP: Already marked as read: ' + AItemKey);
    Exit;
  end;

  ItemHash := ComputeItemHash(AFeedURL, AItemKey);
  OldRecordCount := AReadStatusDb.RecordCount;

  LogMessage(ALogProc, Format('  Attempting to add: Hash=%s, RecordCount before=%d',
    [ItemHash, OldRecordCount]));

  AReadStatusDb.Append;
  OldState := AReadStatusDb.State;
  LogMessage(ALogProc, Format('  After Append: State=%d (dsInsert=%d)',
    [Ord(OldState), Ord(dsInsert)]));

  AReadStatusDb.FieldByName('FEEDURL').AsString := AFeedURL;
  AReadStatusDb.FieldByName('ITEMLINK').AsString := Copy(AItemKey, 1, 255);
  AReadStatusDb.FieldByName('ITEMHASH').AsString := ItemHash;
  AReadStatusDb.FieldByName('ISREAD').AsBoolean := True;
  AReadStatusDb.FieldByName('DATEREAD').AsDateTime := Now;

  LogMessage(ALogProc, Format('  Fields set. FEEDURL len=%d, ITEMKEY len=%d, ITEMHASH len=%d',
    [Length(AFeedURL), Length(AItemKey), Length(ItemHash)]));

  AReadStatusDb.Post;
  AReadStatusDb.CheckBrowseMode;

  if not IsItemRead(AReadStatusDb, AFeedURL, AItemKey, ALegacyLink, ALogProc) then
    LogMessage(ALogProc, '*** Insert failed: still not found: ' + AItemKey);

  OldState := AReadStatusDb.State;
  NewRecordCount := AReadStatusDb.RecordCount;

  LogMessage(ALogProc, Format('  After Post: State=%d (dsBrowse=%d), RecordCount=%d (was %d)',
    [Ord(OldState), Ord(dsBrowse), NewRecordCount, OldRecordCount]));

  if NewRecordCount > OldRecordCount then
    LogMessage(ALogProc, 'OK: Marked (RecordCount increased): ' + AItemKey)
  else
    LogMessage(ALogProc,
      'NOTE: RecordCount did not increase (may be normal); rely on IsItemRead verification above');
end;

function BuildReadKeyCache(AReadStatusDb: TDbf): TStringList;
var
  CacheKey: string;
begin
  Result := TStringList.Create;
  Result.Sorted := True;
  Result.Duplicates := dupIgnore;

  if not Assigned(AReadStatusDb) or not AReadStatusDb.Active then
    Exit;

  AReadStatusDb.First;
  while not AReadStatusDb.EOF do
  begin
    CacheKey := AReadStatusDb.FieldByName('FEEDURL').AsString + #9 +
                AReadStatusDb.FieldByName('ITEMLINK').AsString;
    if Result.IndexOf(CacheKey) < 0 then
      Result.Add(CacheKey);
    AReadStatusDb.Next;
  end;
end;

function FindCountIndex(ACounts: TStringList; const AFeedURL: string): Integer;
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

function GetCountValue(ACounts: TStringList; const AFeedURL: string): Integer;
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

procedure IncrementCountValue(ACounts: TStringList; const AFeedURL: string);
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

function GetUnreadCount(AFeedItemsDb, AReadStatusDb: TDbf; const AFeedURL: string): Integer;
var
  ReadKeys: TStringList;
  FeedURL, LinkValue, ItemKeyValue: string;
  ReadKey1, ReadKey2: string;
begin
  Result := 0;

  if not Assigned(AFeedItemsDb) or not AFeedItemsDb.Active then
    Exit;

  if AFeedItemsDb.RecordCount = 0 then
    Exit;

  ReadKeys := BuildReadKeyCache(AReadStatusDb);
  try
    AFeedItemsDb.First;
    while not AFeedItemsDb.EOF do
    begin
      FeedURL := AFeedItemsDb.FieldByName('FEEDURL').AsString;
      if FeedURL = AFeedURL then
      begin
        LinkValue := AFeedItemsDb.FieldByName('LINK').AsString;
        ItemKeyValue := AFeedItemsDb.FieldByName('ITEMKEY').AsString;
        ReadKey1 := AFeedURL + #9 + ItemKeyValue;
        ReadKey2 := AFeedURL + #9 + LinkValue;
        if (ReadKeys.IndexOf(ReadKey1) < 0) and
           ((LinkValue = '') or (ReadKeys.IndexOf(ReadKey2) < 0)) then
          Inc(Result);
      end;
      AFeedItemsDb.Next;
    end;
  finally
    ReadKeys.Free;
  end;
end;

end.
