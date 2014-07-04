#!/usr/bin/env python

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

from rabbitvcs.util.contextmenuitems import *
from rabbitvcs.util.contextmenu import MenuBuilder, MainContextMenu, MainContextMenuCallbacks
from rabbitvcs.services.checkerservice import StatusCheckerStub

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
  menu = None

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
      if item.callback_name != None:
        menuitem.identifier = "RabbitVCS::" + item.callback_name
      else:
        menuitem.identifier = item.identifier
      menuitem.label = item.make_label()
      menuitem.icon = item.icon

    return menuitem

  def attach_submenu(self, menu_node, submenu_list):
    menu_node.menu = []
    menu_node.identifier = None
    for item in submenu_list:
      menu_node.menu.append(item)

  def top_level_menu(self, items):
    return items

class DCMainContextMenu(MainContextMenu):
  """Double Commander main context menu class"""

  def BuildMenu(self, menu):
    result = ""
    for item in menu:
      result += "<item caption=\"%s\">\n" % item.label
      if item.identifier != None:
        result += "<command>%s</command>\n" % item.identifier
      if item.icon != None:
        result += "<icon>" + item.icon + "</icon>\n"
      if item.menu != None:
        result += self.BuildMenu(item.menu)
      result += "</item>\n"
    return result

  def GetXmlMenu(self):
    menu = DCContextMenu(self.structure, self.conditions, self.callbacks).menu
    return "<menu>\n" + self.BuildMenu(menu) + "</menu>"

def GetContextMenu(paths):
  upaths = []
  for path in paths:
    upaths.append(unicode(path))

  sender = DCSender()
  base_dir = os.path.dirname(upaths[0])
  return DCMainContextMenu(sender, base_dir, upaths, None).GetXmlMenu()

def Execute(identifier, paths):
  sender = DCSender()
  base_dir = os.path.dirname(paths[0])
  vcs_client = rabbitvcs.vcs.create_vcs_instance()
  action = MenuItem.make_default_name(identifier)
  callbacks = MainContextMenuCallbacks(sender, base_dir, vcs_client, paths)
  # Try to find and execute callback function
  if hasattr(callbacks, action):
    function = getattr(callbacks, action)
    if callable(function):
      function(sender, None)

if __name__ == "__main__":

    args = sys.argv
    argc = len(args)

    if argc < 2:
      status_checker = StatusCheckerStub()
    elif (argc > 2):
      args.pop(0)
      identifier = args.pop(0)
      Execute(identifier, args)
