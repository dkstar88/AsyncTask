program AsyncTaskDemo;

uses
  FMX.Forms,
  AsynctaskDemoForm in 'AsynctaskDemoForm.pas' {Form2},
  AsyncTask in '..\AsyncTask.pas',
  AsyncTask.HTTP in 'AsyncTask.HTTP.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
