Banner.dll shows a banner with customizable text.

There are only two functions, show and destroy. Show must be called
with the /NOUNLOAD flag or else it won't work. Show takes one argument
which is the text to show and destroy takes no arguments.

To use with the MUI use:

Banner::show /NOUNLOAD /set 76 "text to replace Please wait while Setup is loading..." "other text as normal"

You can use multiple /SETs to change the text of multiple labels. For example:

Banner::show /NOUNLOAD /set 76 "bah #1" /set 54 "bah #2" "other text as normal"

The second parameter for /set is the ID of the control that its text should be replaced in the dialog.

Look at Example.nsi for an example.

Created by Amir Szekely (aka KiCHiK)