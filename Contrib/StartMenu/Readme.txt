StartMenu.dll shows a custom page that lets the user select a start menu program 
folder to put shortcuts in.

To show the dialog use the Select function. This function has one required parameter 
which is the program group default name, and some more optional parameters:
  /autoadd - automatically adds the program name to the selected folder
  /noicon - doesn't show the icon in the top left corner
  /text [please select...] - sets the top text to something else than "Select 
                             the Start Menu folder in which..."
  /cancelconfirm [text] [caption] [flags] - displays a cancel confirmation 
              message box when the user click on the cancel button

The function pushes the folder selection back to the stack. It does not push the 
full path but only the selected sub-folder. It's up to you to decide if to put 
it in the current user or all users start menu.

Look at Example.nsi for an example.

Created by Amir Szekely (aka KiCHiK)