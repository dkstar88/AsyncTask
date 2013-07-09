unit AsynctaskDemoForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, FMX.Objects, FMX.Layouts, FMX.Memo;

type
  TForm2 = class(TForm)
    Text1: TText;
    Panel1: TPanel;
    Text2: TText;
    Image1: TImage;
    Text3: TText;
    Button1: TButton;
    Panel2: TPanel;
    Memo1: TMemo;
    Text4: TText;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    AniIndicator1: TAniIndicator;
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    procedure MemoLog(ALog: string);
    procedure RunSleepTask(ASeconds: Integer);
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses AsyncTask, AsyncTask.HTTP;

{$R *.fmx}

procedure TForm2.Button1Click(Sender: TObject);
begin
  AniIndicator1.Visible := True;
  Run(THttpAsyncTaskBitmap.Create('http://www.google.com/images/nav_logo129.png'),
    // Finish
    procedure (ATask: IAsyncTask)
    begin
      Image1.Bitmap := (ATask as IHttpBitmapResponse).Bitmap;
      AniIndicator1.Visible := False;
    end
  );
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  RunSleepTask(5);
end;

procedure TForm2.Button3Click(Sender: TObject);
begin
  RunSleepTask(10);
end;

procedure TForm2.Button4Click(Sender: TObject);
begin
  RunSleepTask(20);
end;

procedure TForm2.MemoLog(ALog: string);
begin
  Memo1.Lines.Add(
    FormatDateTime('hh:nn:ss.sss', Now) + ': ' + ALog
    );
end;

procedure TForm2.RunSleepTask(ASeconds: Integer);
begin
  MemoLog(Format('(%dS) Start', [ASeconds]));
  // Sleep for <ASeconds>
  Run(TAsyncTask,
    // Execute
    procedure (ATask: IAsyncTask)
    begin
      Sleep(ASeconds*1000);
    end,
    // Finish
    procedure (ATask: IAsyncTask)
    begin
        MemoLog(Format('(%dS) End', [ASeconds]));
    end
  );
end;

end.
