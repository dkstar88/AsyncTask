AsyncTask
=========

Easy to use Asynchronous Task creation library


Features
========
* Create threading task with single function call
* Utilises anoymous procedure
* Ready to use AsyncHTTPTask, AsyncHTTPTaskBitmap


Demo Codes
=======


This code, sleeps for 5 seconds, and shows message box.

    Run(TAsyncTask,
      // Execute
      procedure (ATask: IAsyncTask)
      begin
        Sleep(5*1000);
      end,
      // Finish
      procedure (ATask: IAsyncTask)
      begin
          ShowMessage('Finished');
      end
    );
  
  
This code downloads google's logo png file, and displays in TImage component

    AniIndicator1.Visible := True;
    Run(THttpAsyncTaskBitmap.Create('http://www.google.com/images/nav_logo129.png'),
      // Finish
      procedure (ATask: IAsyncTask)
      begin
        Image1.Bitmap := (ATask as IHttpBitmapResponse).Bitmap;
        AniIndicator1.Visible := False;
      end
    );
  
  
