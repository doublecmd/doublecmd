Double Commander
--------------------------------------------------------------------------------
WFX plugin for working with GVFS

Copyright (C) 2009  Koblov Alexander (Alexx2000@mail.ru)

Based on:
  GVFS plugin for Tux Commander
  Copyright (C) 2008 Tomas Bzatek <tbzatek@users.sourceforge.net>
  http://tuxcmd.sourceforge.net

Requirements:
 - glib2 library at least version 2.16.0
 - gvfs daemon and backends available in the system
   (check your distribution for gvfs packages)

This is the GVFS plugin for Double Commander file manager. It's built on top of
the GVFS daemon, which is an integral part of Gnome Desktop since version 2.22.
It provides access to many type of resources such as network shares 
(FTP, SSH/SFTP, SMB, WebDAV), bluetooth devices (ObexFTP), cameras and portable
players (gphoto2) and others.

Feature highlights:
  *  read/write access to network resources (FTP, SSH/SFTP, SMB, WebDAV)

For successful compilation you will need working gcc compiler and glib2
library installed with development files.

Compilation has been tested with gcc compiler v4.3.1
