Set objFSO = CreateObject("Scripting.FileSystemObject")
objStartFilePath = CStr(WScript.Arguments.Item(0))
Const FONTS = &H14&

Set objShell = CreateObject("Shell.Application")
Set objFontFolder = objShell.Namespace(FONTS)
Set objFile = objFSO.GetFile(objStartFilePath)

objFontFolder.CopyHere objFile.Path, 1044
Wscript.Echo objFile.Name
