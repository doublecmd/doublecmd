# norootforbuild

Name:		doublecmd-help-en
Summary:	Documentation for the Double Commander (English)
Version:	0.4.6
Release:	1
Url:		http://doublecmd.sourceforge.net/
License:	GPL
Source0:	%{name}-%{version}.tar.gz
Group:		Documentation

%description
This package contains the documentation files for the Double Commander
designed for use with the external web browsers.

This package contains the documentation for the DC in English.

%package -n doublecmd-help-ru
Summary:	Documentation for the Double Commander (Russian)
Group:		Documentation

%description -n doublecmd-help-ru
This package contains the documentation files for the Double Commander
designed for use with the external web browsers.

This package contains the documentation for the DC in Russian.

%prep
%setup -q

%build

%install
install -d %{_datadir}/%{name}/doc
cp -r doc/en   %{_datadir}/%{name}/doc/
cp -r doc/ru   %{_datadir}/%{name}/doc/

%clean
[ %{buildroot} != "/" ] && ( rm -rf %{buildroot} )

%files
%defattr(-,root,root)
%{_datadir}/%{name}/doc/en

%files -n doublecmd-help-ru
%defattr(-,root,root)
%{_datadir}/%{name}/doc/ru

%changelog
* Fri Jar 21 2011 - Alexander Koblov <Alexx2000@mail.ru>
- Initial package, version 0.4.6
