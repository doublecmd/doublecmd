Name:		doublecmd-gtk
Summary:	Twin-panel (commander-style) file manager (GTK2)
Version:	1.0.3
Release:	1
URL:		http://doublecmd.sourceforge.net
Source0:	doublecmd_%{version}.orig.tar.gz
License:	GPL
Group:		Applications/File
BuildRequires:	fpc >= 3.0.0 fpc-src glib2-devel gtk2-devel lazarus >= 1.8.0
%if 0%{?mandriva_version}
BuildRequires:  libncurses-devel libdbus-1-devel libbzip2-devel
%endif
%if 0%{?fedora_version} || 0%{?rhel}
BuildRequires:  dbus-devel bzip2-devel
%endif
%if 0%{?suse_version} >= 1110
BuildRequires: ncurses-devel dbus-1-devel libbz2-devel polkit
%endif 
Provides:	doublecmd
Conflicts:	doublecmd-qt
BuildRoot:	%{_tmppath}/doublecmd-%{version}-build

%define debug_package %{nil}

%description
Double Commander is a cross platform open source file manager with two panels side by side.
It is inspired by Total Commander and features some new ideas.

%prep
%setup -q -n doublecmd-%{version}

%build
./build.sh release gtk2

%install
install/linux/install.sh --install-prefix=%{buildroot}

%clean
[ %{buildroot} != "/" ] && ( rm -rf %{buildroot} )

%files
%defattr(-,root,root)
%{_libdir}/doublecmd
%{_bindir}/doublecmd
%{_datadir}/doublecmd
%{_datadir}/man/man1/doublecmd.*
%{_datadir}/pixmaps/doublecmd.*
%{_datadir}/applications/doublecmd.desktop
%{_datadir}/icons/hicolor/scalable/apps/doublecmd.svg
%{_datadir}/polkit-1/actions/org.doublecmd.root.policy

%changelog
* Fri Jun 11 2010 - Alexander Koblov <Alexx2000@mail.ru>
- Initial package, version 0.4.6
