unit RssUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function TomarBaseDir: string;
function TomarDataDir: string;
function TomarConfigFile: string;
function IsYouTubeFeedURL(const AFeedURL: string): Boolean;
function FeedDateSortKey(const APubDate: string): string;

implementation

const
  CONFIG_FILE_NAME = 'feeds.xml';

function TomarBaseDir: string;
begin
  Result := IncludeTrailingPathDelimiter(GetUserDir) + '.tomar' + DirectorySeparator;
  ForceDirectories(Result);
end;

function TomarDataDir: string;
begin
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
  S, TimePart, Digits, ISOTime: string;
  Y, M, D, H, N, Sec: Integer;
  Parts, TParts: TStringList;
  Work: string;
  TZPos, P: Integer;
begin
  Result := '00000000000000';
  S := Trim(APubDate);
  if S = '' then
    Exit;

  Parts := TStringList.Create;
  TParts := TStringList.Create;
  try
    if (Length(S) >= 10) and (S[5] = '-') and (S[8] = '-') then
    begin
      Y := StrToIntDef(Copy(S, 1, 4), 0);
      M := StrToIntDef(Copy(S, 6, 2), 0);
      D := StrToIntDef(Copy(S, 9, 2), 0);
      H := 0;
      N := 0;
      Sec := 0;

      if Length(S) >= 16 then
      begin
        ISOTime := Copy(S, 12, MaxInt);
        TZPos := Pos('Z', UpperCase(ISOTime));
        if TZPos > 0 then
          ISOTime := Copy(ISOTime, 1, TZPos - 1);

        if Length(ISOTime) > 0 then
        begin
          P := 1;
          while P <= Length(ISOTime) do
          begin
            if (ISOTime[P] in ['0'..'9', ':', ' ']) then
              Inc(P)
            else
              Break;
          end;
          ISOTime := Trim(Copy(ISOTime, 1, P - 1));
        end;

        TParts.Clear;
        ExtractStrings([':'], [], PChar(ISOTime), TParts);
        if TParts.Count >= 1 then H := StrToIntDef(ExtractDigits(TParts[0]), 0) else H := 0;
        if TParts.Count >= 2 then N := StrToIntDef(ExtractDigits(TParts[1]), 0) else N := 0;
        if TParts.Count >= 3 then Sec := StrToIntDef(ExtractDigits(TParts[2]), 0) else Sec := 0;
      end;

      if (Y > 0) and (M > 0) and (D > 0) then
      begin
        Result := Format('%.4d%.2d%.2d%.2d%.2d%.2d', [Y, M, D, H, N, Sec]);
        Exit;
      end;
    end;

    Work := StringReplace(S, ',', ' ', [rfReplaceAll]);
    while Pos('  ', Work) > 0 do
      Work := StringReplace(Work, '  ', ' ', [rfReplaceAll]);
    Work := Trim(Work);

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

end.
