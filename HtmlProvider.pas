unit HtmlProvider;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, fphttpclient, IpHtml, ipmsg;

type
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

implementation

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

end.
