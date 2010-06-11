# norootforbuild

Name:		doublecmd
Summary:	Double Commander is a cross platform open source file manager with two panels side by side.
Version:	0.4.6
Release:	1
URL:		http://doublecmd.sourceforge.net
Source0:	doublecmd-%{version}.tar.gz
License:	GPL
Group:		Applications/File
Requires:	glib2
BuildRequires:	fpc fpc-src gdk-pixbuf-devel glib2-devel gtk2-devel lazarus xorg-x11-devel
BuildRoot:	%{_tmppath}/doublecmd-%{version}-build
Prefix:		/

%description
Double Commander is a cross platform open source file manager with two panels side by side.
It is inspired by Total Commander and features some new ideas.

%prep
%setup -q -n trunk

%build
chmod +x build.sh
chmod +x components/build.sh
chmod +x plugins/build.sh
./build.sh all

%install
install/linux/install.sh --install-prefix=$RPM_BUILD_ROOT

%clean
./clean.sh

%files
%defattr(-,root,root)
/

%changelog
* Fri Jun 11 2010 - Alexander Koblov <Alexx2000@mail.ru>
- Initial package, version 0.4.6
