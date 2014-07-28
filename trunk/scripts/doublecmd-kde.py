#!/usr/bin/env python

#
# Double Commander
# -------------------------------------------------------------------------
# KDE integration module
#
# Copyright (C) 2013-2014 Alexander Koblov (alexx2000@mail.ru)
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#

import sys
from PyKDE4.kdeui import KApplication
from PyKDE4.kdecore import KService, KLocale, KGlobal
from PyKDE4.kdecore import ki18n, KAboutData, KCmdLineArgs, KUrl, KCmdLineOptions
from PyKDE4.kio import  KFileItem, KFileItemList, KPropertiesDialog, KRun, KOpenWithDialog

# Show help message
def Help():
  print("\nSyntax:\n")
  print("  doublecmd-kde properties <URL(s)>\n"
        "            # Opens a file properties dialog\n\n")
  print("  doublecmd-kde openwith <URL(s)>\n"
        "            # Display the Open-With dialog for those URLs, and run the chosen application\n\n")
  exit(0)

# Check argument count
def CheckArguments(argc):
  if args.count() < argc:
    Help()
  
#--------------- main ------------------
if __name__ == '__main__':

    appName     = "doublecmd-kde"
    catalog     = "kio4"
    programName = ki18n ("Double Commander")
    version     = "1.0"
    description = ki18n ("Double Commander KDE helper")
    license     = KAboutData.License_GPL
    copyright   = ki18n ("(C) 2013-2014 Alexander Koblov")
    text        = ki18n ("none")
    homePage    = "doublecmd.sourceforge.net"
    bugEmail    = "alexx2000@users.sourceforge.net"
    
    aboutData   = KAboutData (appName, catalog, programName, version, description,
                              license, copyright, text, homePage, bugEmail)
    
        
    KCmdLineArgs.init (sys.argv, aboutData, 0)

    options = KCmdLineOptions()
    options.add("+command", ki18n("Command"))
    options.add("+[URL(s)]", ki18n("Arguments for command"))
    KCmdLineArgs.addCmdLineOptions(options)
    
    args = KCmdLineArgs.parsedArgs()
    CheckArguments(1)

    app = KApplication()    
    command = args.arg(0).toLocal8Bit();
    if command == "properties":
      CheckArguments(2)      
      fileList = []  
      for index in range(1, args.count()):
	fileList.append(KFileItem(args.url(index), "", 0))
    
      propertiesDialog = KPropertiesDialog(KFileItemList(fileList));
      propertiesDialog.exec_()   
    elif command == "openwith":
      CheckArguments(2)
      fileList = []  
      for index in range(1, args.count()):
	fileList.append(args.url(index))
    
      fileList = KUrl.List(fileList)
      propertiesDialog = KOpenWithDialog(fileList)
      if propertiesDialog.exec_():
        service = propertiesDialog.service()
        if service == None:
  	  print("No service set, running " + propertiesDialog.text() + "\n")
  	  service = KService(propertiesDialog.text(), propertiesDialog.text(), "")
        KRun.run(service, fileList, None)
    else:
      Help()
