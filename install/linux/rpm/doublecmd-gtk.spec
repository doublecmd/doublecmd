# norootforbuild

Name:		doublecmd-gtk
Summary:	Twin-panel (commander-style) file manager
Version:	0.4.6
Release:	1
URL:		http://doublecmd.sourceforge.net
Source0:	%{name}-%{version}.tar.gz
License:	GPL
Group:		Applications/File
BuildRequires:	fpc >= 2.4.0 fpc-src glib2-devel gtk2-devel lazarus >= 0.9.29
%if 0%{?mandriva_version}
BuildRequires:  libncurses-devel
%else
BuildRequires:  xorg-x11-devel gdk-pixbuf-devel ncurses-devel
%endif
BuildRoot:	%{_tmppath}/%{name}-%{version}-build

%description
Double Commander is a cross platform open source file manager with two panels side by side.
It is inspired by Total Commander and features some new ideas.

%prep
%setup -q

%build
./build.sh all gtk2

%install
install/linux/install.sh --install-prefix=%{buildroot}

%clean
[ %{buildroot} != "/" ] && ( rm -rf %{buildroot} )

%files
%defattr(-,root,root)
%{_libdir}/%{name}
%{_bindir}/%{name}
%{_datadir}/%{name}
%{_datadir}/pixmaps/%{name}.png
%{_datadir}/applications/%{name}.desktop

%changelog
* Fri Jun 11 2010 - Alexander Koblov <Alexx2000@mail.ru>
- Initial package, version 0.4.6
