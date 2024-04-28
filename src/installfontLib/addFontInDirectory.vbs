Set objFSO = CreateObject("Scripting.FileSystemObject")
objStartFolder = CStr(WScript.Arguments.Item(0))
Const FONTS = &H14&

Set objShell = CreateObject("Shell.Application")
Set objFontFolder = objShell.Namespace(FONTS)
Set objFolder = objFSO.GetFolder(objStartFolder)

Set colFiles = objFolder.Files
For Each objFile in colFiles
	objFontFolder.CopyHere objFile.Path, 1044
    Wscript.Echo objFile.Name
Next
