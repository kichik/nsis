function FolderExists(fn)
{
/*@cc_on @*/
/*@if (@_jscript_version >= 5)
  try
  {
    var objShell = new ActiveXObject("Shell.Application");
    var fso = new ActiveXObject("Scripting.FileSystemObject");
    return fso.FolderExists(fn);
  }
  catch(e) { }
/*@end @*/
  return false;
}

function FileExists(fn)
{
/*@cc_on @*/
/*@if (@_jscript_version >= 5)
  try
  {
    var fso = new ActiveXObject("Scripting.FileSystemObject");
    return fso.FileExists(fn);
  }
  catch(e) { }
/*@end @*/
  return true;
}

function TryShellExec(fn)
{
/*@cc_on @*/
/*@if (@_jscript_version >= 5)
  try
  {
    var objShell = new ActiveXObject("Shell.Application");
    objShell.ShellExecute(fn, "", "", "open", 1);
    return true;
  }
  catch(e) { }
/*@end @*/
  return false;
}

function parser(fn)
{
  var X, Y, sl, a, ra, re;
  ra = /:/;
  a = location.href.search(ra);
  if (a == 2)
    X = 14;
  else
    X = 7;
  sl = "\\";
  Y = location.href.lastIndexOf(sl) + 1;
/*@cc_on @*/
/*@if (@_jscript_version >= 5)
  try
  {
    var fso = new ActiveXObject("Scripting.FileSystemObject");
  }
  catch(e)
  {
    if (fn.substring(0, 3) == "../")
    {
      fn = fn.substring(3);
    }
  }
  @else @*/
  if (fn.substring(0, 3) == "../")
  {
    fn = fn.substring(3);
  }
/*@end @*/
  lfn = location.href.substring(X, Y) + fn;
  re = /%20/g;
  lfn = lfn.replace(re, " ");
  re = /\//g;
  lfn = lfn.replace(re, "\\");

  if (FolderExists(lfn))
  {
    if (!TryShellExec(lfn))
    {
      var objShell = new ActiveXObject("Shell.Application");
      objShell.Open(lfn);
    }
  }
  else if (FileExists(lfn))
  {
    execed = false;
    htmlre = /\.html?$/;
    txtre = /\.txt$/;
    if (!lfn.match(htmlre) && !lfn.match(txtre))
    {
      execed = TryShellExec(lfn);
    }
    if (!execed)
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
    alert(fn + " doesn't exist");
  }
}