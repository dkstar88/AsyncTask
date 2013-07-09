unit AsyncTask;

interface

uses SysUtils, Classes, Rtti, Types, FMX.Types, System.Generics.Collections;

type
  IAsyncTask = interface;
  TTaskStatus = (tsNone, tsRunning, tsComplete);
  TTaskEvent = reference to procedure (ATask: IAsyncTask);
  TNamedParams = TDictionary<String, TValue>;
  IAsyncTask = interface
    ['{4CF30AFF-C915-48DB-9878-8995E052700B}']
    procedure SetData(const Value: TValue);
    procedure SetOnFinish(const Value: TTaskEvent);
    procedure SetOnStart(const Value: TTaskEvent);
    procedure SetResult(const Value: Integer);
    procedure SetTaskName(const Value: String);
    function GetData: TValue;
    function GetOnFinish: TTaskEvent;
    function GetOnStart: TTaskEvent;
    function GetResult: Integer;
    function GetTaskName: String;
    procedure SetStatus(const Value: TTaskStatus);
    function GetStatus: TTaskStatus;
    function GetNamedParams: TNamedParams;
    procedure SetOnExecute(const Value: TTaskEvent);
    function GetOnExecute: TTaskEvent;

    procedure Run;

    property Params: TNamedParams read GetNamedParams;
    property TaskName: String read GetTaskName write SetTaskName;
    property Data: TValue read GetData write SetData;
    property Result: Integer read GetResult write SetResult;
    property OnStart: TTaskEvent read GetOnStart write SetOnStart;
    property OnExecute: TTaskEvent read GetOnExecute write SetOnExecute;
    property OnFinish: TTaskEvent read GetOnFinish write SetOnFinish;
    property Status: TTaskStatus read GetStatus write SetStatus;
  end;

  TAsyncTask = class(TInterfacedObject, IAsyncTask)
  private
    FTaskName: String;
    FNamedParams: TNamedParams;
    FOnFinish: TTaskEvent;
    FOnStart: TTaskEvent;
    FResult: Integer;
    FData: TValue;
    FStatus: TTaskStatus;
    FOnExecute: TTaskEvent;
    procedure SetData(const Value: TValue);
    procedure SetOnFinish(const Value: TTaskEvent);
    procedure SetOnStart(const Value: TTaskEvent);
    procedure SetResult(const Value: Integer);
    procedure SetTaskName(const Value: String);
    function GetData: TValue;
    function GetOnFinish: TTaskEvent;
    function GetOnStart: TTaskEvent;
    function GetResult: Integer;
    function GetTaskName: String;
    procedure SetStatus(const Value: TTaskStatus);
    function GetStatus: TTaskStatus;
    function GetNamedParams: TNamedParams;
    procedure SetOnExecute(const Value: TTaskEvent);
    function GetOnExecute: TTaskEvent;
  protected
    procedure Execute; virtual;
    procedure DoStart; virtual;
    procedure DoFinish; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Run;

    property Params: TNamedParams read GetNamedParams;
    property TaskName: String read GetTaskName write SetTaskName;
    property Data: TValue read GetData write SetData;
    property Result: Integer read GetResult write SetResult;
    property OnStart: TTaskEvent read GetOnStart write SetOnStart;
    property OnExecute: TTaskEvent read GetOnExecute write SetOnExecute;
    property OnFinish: TTaskEvent read GetOnFinish write SetOnFinish;
    property Status: TTaskStatus read GetStatus write SetStatus;

  end;

  TTaskRunner = class(TThread)
  protected
    fTask: IAsyncTask;
    procedure Execute; override;
  public
    constructor Create(ATask: IAsyncTask);
    destructor Destroy; override;
  end;

  TAsyncTaskClass = class of TAsyncTask;

  TTaskMonitor = class(TObject)
  private
    fRunners: TList<TTaskRunner>;
    fTimer: TTimer;
    procedure MonitorTimer(ASender: TObject);

  public
    constructor Create;
    destructor Destroy; override;

    procedure RunTask( ATask: IAsyncTask;
      AOnFinish: TTaskEvent); overload;

    procedure RunTask( ATaskClass: TAsyncTaskClass;
      AOnFinish: TTaskEvent); overload;

    procedure RunTask( ATask: IAsyncTask;
      AOnExecute, AOnFinish: TTaskEvent); overload;

    procedure RunTask( ATaskClass: TAsyncTaskClass;
      AOnExecute, AOnFinish: TTaskEvent); overload;

    procedure RunTask( ATask: IAsyncTask;
      AOnStart, AOnExecute, AOnFinish: TTaskEvent); overload;

    procedure RunTask( ATaskClass: TAsyncTaskClass;
      AOnStart, AOnExecute, AOnFinish: TTaskEvent); overload;

  end;

  procedure Run( ATask: IAsyncTask; AOnStart, AOnExecute,
    AOnFinish: TTaskEvent); overload;

  procedure Run( ATask: TAsyncTaskClass; AOnStart, AOnExecute,
    AOnFinish: TTaskEvent); overload;

  procedure Run( ATask: IAsyncTask; AOnExecute, AOnFinish: TTaskEvent); overload;
  procedure Run( ATaskClass: TAsyncTaskClass; AOnExecute,
    AOnFinish: TTaskEvent); overload;

  procedure Run( ATask: IAsyncTask; AOnFinish: TTaskEvent); overload;
  procedure Run( ATaskClass: TAsyncTaskClass;
    AOnFinish: TTaskEvent); overload;

var
  DefaultTaskMonitor: TTaskMonitor;

implementation

procedure Run( ATask: IAsyncTask; AOnStart, AOnExecute,
  AOnFinish: TTaskEvent);
begin
  if DefaultTaskMonitor <> nil then
    DefaultTaskMonitor.RunTask(ATask, AOnStart, AOnExecute, AOnFinish);

end;

procedure Run( ATask: TAsyncTaskClass; AOnStart, AOnExecute,
  AOnFinish: TTaskEvent);
begin
  if DefaultTaskMonitor <> nil then
    DefaultTaskMonitor.RunTask(ATask, AOnStart, AOnExecute, AOnFinish);

end;

procedure Run( ATask: IAsyncTask; AOnExecute: TTaskEvent;
    AOnFinish: TTaskEvent);
begin
  if DefaultTaskMonitor <> nil then
    DefaultTaskMonitor.RunTask(ATask, AOnExecute, AOnFinish);
end;

procedure Run( ATaskClass: TAsyncTaskClass;
    AOnExecute: TTaskEvent;
    AOnFinish: TTaskEvent);
begin
  if DefaultTaskMonitor <> nil then
    DefaultTaskMonitor.RunTask(ATaskClass, AOnExecute, AOnFinish);

end;

procedure Run( ATask: IAsyncTask; AOnFinish: TTaskEvent);
begin
  if DefaultTaskMonitor <> nil then
    DefaultTaskMonitor.RunTask(ATask, AOnFinish);
end;

procedure Run( ATaskClass: TAsyncTaskClass; AOnFinish: TTaskEvent);
begin
  Run(ATaskClass.Create, AOnFinish);
end;
{ TAsyncTask }

constructor TAsyncTask.Create;
begin
  inherited Create;
  FNamedParams := TNamedParams.Create();
  fStatus := tsNone;
end;

destructor TAsyncTask.Destroy;
begin
  FNamedParams.Free;
  inherited;
end;

procedure TAsyncTask.DoFinish;
begin
  if Assigned(FOnFinish) then
    FOnFinish(Self)
end;

procedure TAsyncTask.DoStart;
begin
  if Assigned(FOnStart) then
    FOnStart(Self)
end;

procedure TAsyncTask.Execute;
begin
  if Assigned(FOnExecute) then
    FOnExecute(Self);
end;

function TAsyncTask.GetData: TValue;
begin
  Result := fData;
end;

function TAsyncTask.GetNamedParams: TNamedParams;
begin
  Result := FNamedParams;
end;

function TAsyncTask.GetOnExecute: TTaskEvent;
begin
  Result := FOnExecute;
end;

function TAsyncTask.GetOnFinish: TTaskEvent;
begin
  Result := FOnFinish;
end;

function TAsyncTask.GetOnStart: TTaskEvent;
begin
  Result := FOnStart;
end;

function TAsyncTask.GetResult: Integer;
begin
  Result := fResult;
end;

function TAsyncTask.GetStatus: TTaskStatus;
begin
  Result := fStatus;
end;

function TAsyncTask.GetTaskName: String;
begin
  Result := FTaskName;
end;

procedure TAsyncTask.Run;
begin
//  DoStart;
  Execute;
//  DoFinish;
end;

procedure TAsyncTask.SetData(const Value: TValue);
begin
  FData := Value;
end;

procedure TAsyncTask.SetOnExecute(const Value: TTaskEvent);
begin
  FOnExecute := Value;
end;

procedure TAsyncTask.SetOnFinish(const Value: TTaskEvent);
begin
  FOnFinish := Value;
end;

procedure TAsyncTask.SetOnStart(const Value: TTaskEvent);
begin
  FOnStart := Value;
end;

procedure TAsyncTask.SetResult(const Value: Integer);
begin
  FResult := Value;
end;

procedure TAsyncTask.SetStatus(const Value: TTaskStatus);
begin
  FStatus := Value;
end;

procedure TAsyncTask.SetTaskName(const Value: String);
begin
  FTaskName := Value;
end;

{ TTaskRunner }

constructor TTaskRunner.Create(ATask: IAsyncTask);
begin
  FTask := ATask;
  inherited Create(False);
  FreeOnTerminate := False;
end;

destructor TTaskRunner.Destroy;
begin
  FTask := nil;
  inherited;
end;

procedure TTaskRunner.Execute;
begin
  inherited;
  FTask.Status := tsRunning;
  FTask.Run;
  FTask.Status := tsComplete;
end;

{ TTaskMonitor }

constructor TTaskMonitor.Create;
begin
  fRunners := TList<TTaskRunner>.Create;
  fTimer := TTimer.Create(nil);
  fTimer.OnTimer := MonitorTimer;
  fTimer.Interval := 50;
  fTimer.Enabled := True;
end;

destructor TTaskMonitor.Destroy;
begin
  fRunners.Free;
  fTimer.Free;
  inherited;
end;

procedure TTaskMonitor.MonitorTimer(ASender: TObject);
var
  runner: TTaskRunner;
  deletelist: TList<TTaskRunner>;
  i: Integer;
begin
  fTimer.Enabled := False;
  try
    deletelist := TList<TTaskRunner>.Create;
    for runner in fRunners do
    begin
      if runner.fTask.Status = tsComplete then
      begin
        runner.fTask.OnFinish(runner.fTask);
        deletelist.Add(runner);
      end;
    end;
//    for runner in deletelist do
//    begin
//      fRunners.Remove(runner);
//      FreeAndNil(runner);
//    end;
    for i := 0 to deletelist.Count-1 do
    begin
      runner := deletelist[i];
      fRunners.Remove(runner);
      FreeAndNil(runner);
    end;
    deletelist.Free;
  finally
    fTimer.Enabled := True;
  end;
end;

procedure TTaskMonitor.RunTask(ATask: IAsyncTask; AOnStart, AOnExecute,
  AOnFinish: TTaskEvent);
begin
  ATask.OnStart := AOnStart;
  ATask.OnExecute := AOnExecute;
  ATask.OnFinish := AOnFinish;
  fRunners.Add(TTaskRunner.Create(ATask));
end;

procedure TTaskMonitor.RunTask(ATaskClass: TAsyncTaskClass; AOnStart,
  AOnExecute, AOnFinish: TTaskEvent);
begin
  RunTask(ATaskClass.Create, AOnStart, AOnExecute, AOnFinish);
end;

procedure TTaskMonitor.RunTask(ATask: IAsyncTask; AOnExecute,
  AOnFinish: TTaskEvent);
begin
  ATask.OnExecute := AOnExecute;
  ATask.OnFinish := AOnFinish;
  fRunners.Add(TTaskRunner.Create(ATask));
end;

procedure TTaskMonitor.RunTask(ATaskClass: TAsyncTaskClass; AOnExecute,
  AOnFinish: TTaskEvent);
begin
  RunTask(ATaskClass.Create, AOnExecute, AOnFinish);
end;

procedure TTaskMonitor.RunTask(ATaskClass: TAsyncTaskClass;
  AOnFinish: TTaskEvent);
begin
  RunTask(ATaskClass.Create, AOnFinish);
end;

procedure TTaskMonitor.RunTask(ATask: IAsyncTask; AOnFinish: TTaskEvent);
begin
  ATask.OnFinish := AOnFinish;
  fRunners.Add(TTaskRunner.Create(ATask));
end;


initialization
  DefaultTaskMonitor := TTaskMonitor.Create;

finalization
  DefaultTaskMonitor.Free;

end.
