#!/bin/bash
LazDebDir=$(pwd)/debian/lazarus/

#Copy Lazarus to /usr/share
	mkdir -p $LazDebDir/usr/share/lazarus
	cp -R $(pwd)/* $LazDebDir/usr/share/lazarus

#Make icons and menu-entries for the gnome menu
	mkdir -p $LazDebDir/usr/share/pixmaps/
	mkdir -p $LazDebDir/usr/share/gnome/apps/Development/
	mkdir -p $LazDebDir/usr/bin/
	install -m 644 $(pwd)/images/ide_icon48x48.png $LazDebDir/usr/share/pixmaps/lazarus.png
	install -m 644 $(pwd)/gnome.ide.desktop $LazDebDir/usr/share/gnome/apps/Development/lazarus.desktop

#Make a symlink to /usr/bin
	cd $LazDebDir
	ln usr/share/lazarus/lazarus usr/bin/lazarus

