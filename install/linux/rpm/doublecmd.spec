# norootforbuild

Name:		doublecmd
Summary:	Double Commander is a cross platform open source file manager with two panels side by side.
Version:	0.4.6
Release:	1
URL:		http://doublecmd.sourceforge.net
Source0:	%{name}-%{version}.tar.gz
License:	GPL
Group:		Applications/File
Requires:	glib2
BuildRequires:	fpc >= 2.4.0 fpc-src gdk-pixbuf-devel glib2-devel gtk2-devel lazarus >= 0.9.29 xorg-x11-devel hal-devel
BuildRoot:	%{_tmppath}/%{name}-%{version}-build

%description
Double Commander is a cross platform open source file manager with two panels side by side.
It is inspired by Total Commander and features some new ideas.

%prep
%setup -q

%build
./build.sh all

%install
install/linux/install.sh --install-prefix=%{buildroot}

%clean
[ %{buildroot} != "/" ] && ( rm -rf %{buildroot} )

%files
%defattr(-,root,root)
/

%changelog
* Fri Jun 11 2010 - Alexander Koblov <Alexx2000@mail.ru>
- Initial package, version 0.4.6
