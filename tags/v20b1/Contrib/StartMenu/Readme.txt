StartMenu.dll shows a custom page that lets the user select a start menu program 
folder to put shortcuts in.

To show the dialog use the Select function. This function has one required parameter 
which is the program group default name, and some more optional parameters:
  /autoadd - automatically adds the program name to the selected folder
  /noicon - doesn't show the icon in the top left corner
  /text [please select...] - sets the top text to something else than
                             "Select the Start Menu folder in which..."
  /lastused [folder] - sets the edit box to a specific value folder.
                       Use this to make this plug-in remember the last
                       folder selected by the user
  /checknoshortcuts text - Shows a check box with the text "text". If
                           the user checks this box, the return value
                           will have > as its first character and you
                           should not create the program group.

The function pushes "success", "cancel" or an error to the stack. If there was no
error and the user didn't press on cancel it will push the selected folder name
after "success". If the user checked the no shortcuts checkbox the '>' will be
appended to the folder name. The function does not push the full path but only the
selected sub-folder. It's up to you to decide if to put it in the current user or
all users start menu.

Look at Example.nsi for an example.

Created by Amir Szekely (aka KiCHiK)