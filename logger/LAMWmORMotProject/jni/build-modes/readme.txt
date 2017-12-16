How To Get More Builds:
 
   :: Warning: Your system [Laz4Android ?] needs to be prepared [cross-compile] for the various builds!
 
1. Edit Lazarus project file "*.lpi": [use notepad like editor]
 
   > Open the "*.lpi" project file
 
       -If needed replace the line <Libraries ..... /> in the "*.lpi" by line from "build_*.txt"
       -If needed replace the line <TargetCPU ..... /> in the "*.lpi" by line from "build_*.txt"
       -If needed replace the line <CustomOptions ..... /> in the "*.lpi" by line from "build_*.txt"
       -If needed replace the line <TargetProcessor...../> in the "*.lpi" by line from "build_*.txt"
 
   > Save the modified "*.lpi" project file 
 
2. From Lazarus/Laz4Android IDE
 
   >Reopen the Project
 
   > Run -> Build
 
3. Repeat for others "build_*.txt" if needed...
 
4. Execute [double click] the "build.bat" [or .sh] file to get the Apk !
 
 
      Thank you!
      By  ___jmpessoa_hotmail.com_____
