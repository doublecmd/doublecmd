#
# This is an extension to the Double Commander to allow
# integration with the version control systems.
#
# Copyright (C) 2009 Jason Heeris <jason.heeris@gmail.com>
# Copyright (C) 2009 Bruce van der Kooij <brucevdkooij@gmail.com>
# Copyright (C) 2009 Adam Plumb <adamplumb@gmail.com>
# Copyright (C) 2014 Alexander Koblov <alexx2000@mail.ru>
#
# RabbitVCS is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# RabbitVCS is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with RabbitVCS;  If not, see <http://www.gnu.org/licenses/>.
#

import os, os.path
import sys

try:
  from rabbitvcs.util.contextmenuitems import *
  from rabbitvcs.util.contextmenu import MenuBuilder, MainContextMenu, MainContextMenuCallbacks
  from rabbitvcs.services.checkerservice import StatusCheckerStub
except:
  exit(1)

class DCSender:
  """Double Commander sender class"""
  def rescan_after_process_exit(self, proc, paths):
    print "rescan_after_process_exit"
    return

class DCMenuItem:
  """Double Commander menu item class"""

  identifier = None
  label = None
  icon = None
  menu = []

  def connect(self, signal, *callback):
    return

class DCContextMenu(MenuBuilder):
  """Double Commander context menu class"""

  signal = "activate"

  def make_menu_item(self, item, id_magic):

    menuitem = DCMenuItem()

    if type(item) is MenuSeparator:
      menuitem.label = "-"
    else:
      menuitem.icon = item.icon
      menuitem.label = item.make_label()
      menuitem.identifier = item.callback_name

    return menuitem

  def attach_submenu(self, menu_node, submenu_list):
    menu_node.menu = []
    menu_node.identifier = ""
    for item in submenu_list:
      menu_node.menu.append(item)

  def top_level_menu(self, items):
    return items

class DCMainContextMenu(MainContextMenu):
  """Double Commander main context menu class"""

  def Execute(self, identifier):
    # Try to find and execute callback function
    if hasattr(self.callbacks, identifier):
      function = getattr(self.callbacks, identifier)
      if callable(function):
        function(self, None)

  def GetMenu(self):
    return DCContextMenu(self.structure, self.conditions, self.callbacks).menu

def GetContextMenu(paths):
  upaths = []
  for path in paths:
    upaths.append(unicode(path))

  sender = DCSender()
  base_dir = os.path.dirname(upaths[0])
  return DCMainContextMenu(sender, base_dir, upaths, None)

if __name__ == "__main__":

  status_checker = StatusCheckerStub()

