// Use this script to run Double Commander without console debug window

// run as logged-in user:

// First get the path of this script
var script_path = WScript.ScriptFullName;
script_path = script_path.substring(0, script_path.lastIndexOf('\\'));
var shell = WScript.CreateObject("WScript.Shell");
// Then launch dc(DoubleCommander directory is supposed to be placed besides this script)
shell.run("\"" + script_path + "\\doublecmd.exe\"", 0, false);

/*

// or this one asks for the desired user:

var script_path = WScript.ScriptFullName;
script_path = script_path.substring(0, script_path.lastIndexOf('\\'));
var shell = WScript.CreateObject("WScript.Shell");
var app = WScript.CreateObject("Shell.Application");
app.ShellExecute(script_path + "\\doublecmd.exe", "", "", "runas", 0);

*/