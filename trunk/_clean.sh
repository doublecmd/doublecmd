#!/bin/sh
rm -f units/i386-linux-gtk2/*
rm -f units/i386-linux-qt/*
rm -f units/x86_64-linux-gtk2/*
rm -f units/x86_64-linux-qt/*

# Clean up components output directories
rm -rf components/CmdLine/lib/*
rm -rf components/KASToolBar/lib/*
rm -rf components/viewer/lib/*
