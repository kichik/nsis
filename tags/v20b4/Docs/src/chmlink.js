function parser(fn) {
  var X, Y, sl, a, ra, link;
  ra = /:/;
  a = location.href.search(ra);
  if (a == 2)
    X = 14;
  else
    X = 7;
  sl = "\\";
  Y = location.href.lastIndexOf(sl) + 1;
  fso = new ActiveXObject("Scripting.FileSystemObject");
  lfn = location.href.substring(X, Y) + fn;
  re = /%20/g;
  lfn = lfn.replace(re, " ");
  re = /\//g;
  lfn = lfn.replace(re, "\\");
  if (fso.FolderExists(lfn))
  {
    var objShell = new ActiveXObject("Shell.Application");      
    objShell.Open(lfn);
  }
  else if (fso.FileExists(lfn))
  {
    htmlre = /\.html?$/;
    txtre = /\.txt$/;
    if (lfn.match(htmlre) || lfn.match(txtre))
    {
      re = /\\/g;
      lfn = lfn.replace(re, "/");
      re = /\ /g;
      lfn = lfn.replace(re, "%20");
      location.href = 'file:///' + lfn;
    }
  }
  else if (fn.substring(0, 3) == "../")
  {
    parser(fn.substring(3));
  }
  else
  {
    alert(fn.substring(0, 3));
    alert(fn + " doesn't exist");
  }
}