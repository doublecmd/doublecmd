
Project notes

 Conversion of the UniHighlighter to FPC/Lazarus
 Source homepage is:
   http://www.delphist.com/UniHighlighter.html

 Initial conversion started on 11-6-2003 by Tom Lisjac
 
 Status
 
 As of 17-6-2003, the highlighter itself has been converted and lightly
 tested. I also converted the designer and registration unit I wasn't
 able to get them to install. I haven't touched the Syntax Editor app
 although it would definitely be a useful one to convert!

 There are more than 300 highlighters available. See the project homepage.


 Changes for FPC

 Most of the common changes that were needed for FPC conversion were:

   - Added GraphType to the uses statement to resolve TFontStyles
   - In Designer, added Buttons for TButton and LCLType vk_* key declarations
   - In Designer, changed TPageControl to TNotebook and TTabset to TPage. There
     are possible problems that should be reviewed. They are flagged with a TL!!.
   - In SynUniReg, no TDefaultEditor class was found as a base class for
     TSynUniEditor. Used TDefaultComponentEditor instead. Flagged:needs testing!
   - In Designer, removed "out" prefix from method parameter declarations.
   - Added the Lazarus specific method GetTokenEx and conditional to SetLine
   - Prefixed addresses and function parameters with an "@"
   - FPC claimed duplicate IDs in a lot of method declarations that Delphi/Kylix
     obviously didn't worry about. I approached this, for better or worse, by
     adding a 1 suffix to the identifier in the declarations... and then fixing
     up the implementations. All are flagged for and should be reviewed.
   - Removed optional default parameter declarations and explicitly stated
     them in the calls.
  
 Help Wanted!
 
 The testuni project provided will compile all files. You'll have to point
 the units path to (LazarusDir)/designer and includes to
 (LazarusDir)/components/synedit to have the required support references.
 
 The highlighter component needs testing. I checked it out with bash scripts
 and Pascal highlighting and it worked ok. That makes 298 languages to go! :)
 
 The designer has been converted but is untested because I couldn't get it
 installed into Lazarus... which is probably due to my lack of familiarity
 with the process. There is a Syntax Editor included in the release that would
 probably be *very* useful to convert. Unfortunately I've run out of time
 to work on it right now. 
 
 Please send me any questions or comments... especially if you see something
 that could have been done better. I'm planning to do a lot more FPC
 code conversion and I'd like to get better at it as quickly as possible! :)
 
 Tom Lisjac
 vlx@users.sourceforge.net
 http://theseus.sourceforge.net
 
