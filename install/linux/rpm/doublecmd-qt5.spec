# norootforbuild

%define doublecmd doublecmd

Name:		doublecmd-qt5
Summary:	Twin-panel (commander-style) file manager (Qt5)
Version:	1.0.4
Release:	1
URL:		http://doublecmd.sourceforge.net
Source0:	%{doublecmd}-%{version}.tar.gz
License:	GPL
Group:		Applications/File
BuildRequires:	fpc >= 3.0.0 fpc-src glib2-devel libQt5Pas6-devel >= 2.6 lazarus >= 1.7.0
%if 0%{?mandriva_version}
BuildRequires:  libncurses-devel libdbus-1-devel libbzip2-devel
%endif
%if 0%{?fedora_version} || 0%{?rhel}
BuildRequires:  dbus-devel bzip2-devel
%endif
%if 0%{?suse_version} >= 1110
BuildRequires: ncurses-devel dbus-1-devel libbz2-devel
%endif
Provides:	doublecmd
Conflicts:	doublecmd-gtk doublecmd-qt
BuildRoot:	%{_tmppath}/%{doublecmd}-%{version}-build

%define debug_package %{nil}

%description
Double Commander is a cross platform open source file manager with two panels side by side.
It is inspired by Total Commander and features some new ideas.

%prep
%setup -q -n %{doublecmd}-%{version}

%build
./build.sh release qt5

%install
install/linux/install.sh --install-prefix=%{buildroot}

%clean
[ %{buildroot} != "/" ] && ( rm -rf %{buildroot} )

%files
%defattr(-,root,root)
%{_libdir}/%{doublecmd}
%{_bindir}/%{doublecmd}
%{_datadir}/%{doublecmd}
%{_datadir}/man/man1/%{doublecmd}.*
%{_datadir}/pixmaps/%{doublecmd}.*
%{_datadir}/applications/%{doublecmd}.desktop
%{_datadir}/icons/hicolor/scalable/apps/%{doublecmd}.svg

%changelog
* Sun Jan 01 2017 - Alexander Koblov <Alexx2000@mail.ru> - 0.8.0
- Initial package, version 0.8.0
