function FolderExists(fn)
{
   var fso = new ActiveXObject("Scripting.FileSystemObject");
   return fso.FolderExists(fn);
}

function FileExists(fn)
{
  var fso = new ActiveXObject("Scripting.FileSystemObject");
  return fso.FileExists(fn);
}

function parser(fn)
{
  var X, Y, sl, a, ra, link;
  ra = /:/;
  a = location.href.search(ra);
  if (a == 2)
    X = 14;
  else
    X = 7;
  sl = "\\";
  Y = location.href.lastIndexOf(sl) + 1;
  lfn = location.href.substring(X, Y) + fn;
  re = /%20/g;
  lfn = lfn.replace(re, " ");
  re = /\//g;
  lfn = lfn.replace(re, "\\");
  var objShell = new ActiveXObject("Shell.Application");
  if (FolderExists(lfn))
  {
    objShell.Open(lfn);
  }
  else if (FileExists(lfn))
  {
    htmlre = /\.html?$/;
    txtre = /\.txt$/;
    if (!("ShellExecute" in objShell) || lfn.match(htmlre) || lfn.match(txtre))
    {
      re = /\\/g;
      lfn = lfn.replace(re, "/");
      re = /\ /g;
      lfn = lfn.replace(re, "%20");
      location.href = 'file:///' + lfn;
    }
    else
    {
      objShell.ShellExecute(lfn, "", "", "open", 1);
    }
  }
  else if (fn.substring(0, 3) == "../")
  {
    parser(fn.substring(3));
  }
  else
  {
    alert(fn + " doesn't exist");
  }
}