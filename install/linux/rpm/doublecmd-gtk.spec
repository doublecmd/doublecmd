# norootforbuild

%define doublecmd doublecmd

Name:		doublecmd-gtk
Summary:	Twin-panel (commander-style) file manager (GTK2)
Version:	0.4.6
Release:	1
URL:		http://doublecmd.sourceforge.net
Source0:	%{doublecmd}-%{version}.tar.gz
License:	GPL
Group:		Applications/File
BuildRequires:	fpc >= 2.4.0 fpc-src glib2-devel gtk2-devel lazarus >= 0.9.29
%if 0%{?mandriva_version}
BuildRequires:  libncurses-devel libdbus-1-devel
%endif
%if 0%{?fedora_version} >= 11
BuildRequires:  xorg-x11-devel gdk-pixbuf-devel ncurses-devel dbus-devel
%endif
Provides:  doublecmd
Obsoletes: doublecmd, doublecmd-qt
BuildRoot:	%{_tmppath}/%{doublecmd}-%{version}-build

%description
Double Commander is a cross platform open source file manager with two panels side by side.
It is inspired by Total Commander and features some new ideas.

%prep
%setup -q -n %{doublecmd}-%{version}

%build
./build.sh all gtk2

%install
install/linux/install.sh --install-prefix=%{buildroot}

%clean
[ %{buildroot} != "/" ] && ( rm -rf %{buildroot} )

%files
%defattr(-,root,root)
%{_libdir}/%{doublecmd}
%{_bindir}/%{doublecmd}
%{_datadir}/%{doublecmd}
%{_datadir}/pixmaps/%{doublecmd}.png
%{_datadir}/applications/%{doublecmd}.desktop

%changelog
* Fri Jun 11 2010 - Alexander Koblov <Alexx2000@mail.ru>
- Initial package, version 0.4.6
