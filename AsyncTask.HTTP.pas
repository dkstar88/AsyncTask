unit AsyncTask.HTTP;

interface

uses SysUtils, Classes, Rtti, Types, AsyncTask, IdHTTP, FMX.Types;

type


  IHttpResponse = interface
    ['{E8D3A6E5-284E-4C3F-BC66-24ECA806C1A0}']
    procedure SetHttp(const Value: TIdHttp);
    procedure SetResponseStream(const Value: TStream);
    function GetHttp: TIdHttp;
    function GetResponseStream: TStream;

    property ResponseStream: TStream read GetResponseStream write SetResponseStream;
    property Http: TIdHttp read GetHttp write SetHttp;
  end;

  THttpAsyncTask = class(TAsyncTask, IHttpResponse)
  private
    FHTTP: TIdHTTP;
    FURL: String;
    FMethod: String;
    FResponseStream: TStream;
    procedure SetHttp(const Value: TIdHttp);
    procedure SetResponseStream(const Value: TStream);
    function GetHttp: TIdHttp;
    function GetResponseStream: TStream;
  protected
    procedure Execute; override;
  public
    constructor Create(AURL: String); virtual;
    destructor Destroy; override;

    property ResponseStream: TStream read GetResponseStream write SetResponseStream;
    property Http: TIdHttp read GetHttp write SetHttp;
  end;

  IHttpTextResponse = interface
    ['{CF02650B-AD48-446B-BB23-08EEF115F427}']
    function GetText: String;
    property Text: String read GetText;
  end;

  IHttpBitmapResponse = interface
    ['{4B890635-EAD8-4497-98F1-8F89C1256C71}']
    function GetBitmap: TBitmap;
    property Bitmap: TBitmap read GetBitmap;
  end;

  THttpAsyncTaskText = class(THttpAsyncTask, IHttpTextResponse)
  private
    function GetText: String;
  protected
    procedure Execute; override;
  public
    constructor Create(AURL: String); override;
    destructor Destroy; override;
    property Text: String read GetText;
  end;

  THttpAsyncTaskBitmap = class(THttpAsyncTask, IHttpBitmapResponse)
  private
    FBitmap: TBitmap;
    function GetBitmap: TBitmap;
  protected
    procedure Execute; override;
  public
    constructor Create(AURL: String); override;
    destructor Destroy; override;

    property Bitmap: TBitmap read GetBitmap;
  end;

implementation

{ THttpAsyncTask }

constructor THttpAsyncTask.Create(AURL: string);
begin
  inherited Create;
  FResponseStream := TMemoryStream.Create;
  FHTTP := TIdHTTP.Create;
  FHTTP.ConnectTimeout := 300;
  FHTTP.ReadTimeout := 500;
  Params.AddOrSetValue('URL', AURL);
end;

destructor THttpAsyncTask.Destroy;
begin
  FResponseStream.Free;
  FHTTP.Free;
  inherited;
end;

procedure THttpAsyncTask.Execute;
begin
  inherited;
  Data := FResponseStream;
  FHTTP.Get(Params['URL'].AsString, FResponseStream);
end;


function THttpAsyncTask.GetHttp: TIdHttp;
begin
  Result := FHTTP;
end;

function THttpAsyncTask.GetResponseStream: TStream;
begin
  Result := FResponseStream;
end;

procedure THttpAsyncTask.SetHttp(const Value: TIdHttp);
begin
  FHttp := Value;
end;

procedure THttpAsyncTask.SetResponseStream(const Value: TStream);
begin
  FResponseStream := Value;
end;

{ THttpAsyncTaskText }

constructor THttpAsyncTaskText.Create(AURL: String);
begin
  inherited;
end;

destructor THttpAsyncTaskText.Destroy;
begin
  inherited;
end;

procedure THttpAsyncTaskText.Execute;
begin
  inherited;
end;

function THttpAsyncTaskText.GetText: String;
begin
  Result := FHTTP.ResponseText;
end;

{ THttpAsyncTaskBitmap }

constructor THttpAsyncTaskBitmap.Create(AURL: String);
begin
  inherited;
  FBitmap := TBitmap.Create(0, 0);
end;

destructor THttpAsyncTaskBitmap.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

procedure THttpAsyncTaskBitmap.Execute;
begin
  inherited;
  if FHTTP.ResponseCode = 200 then
  begin
    FResponseStream.Seek(0, 0);
    FBitmap.LoadFromStream(FResponseStream);
  end;
end;

function THttpAsyncTaskBitmap.GetBitmap: TBitmap;
begin
  Result := FBitmap;
end;

end.
