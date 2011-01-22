# norootforbuild

%define doublecmd_help doublecmd-help

Name:		doublecmd-help-en
Summary:	Documentation for the Double Commander (English)
Version:	0.4.6
Release:	1
Url:		http://doublecmd.sourceforge.net/
License:	GPL
Source0:	%{doublecmd_help}-%{version}.tar.gz
Group:		Documentation
Requires:	doublecmd

%description
This package contains the documentation files for the Double Commander
designed for use with the external web browsers.

This package contains the documentation for the DC in English.

%package -n doublecmd-help-ru
Summary:	Documentation for the Double Commander (Russian)
Group:		Documentation
Requires:	doublecmd

%description -n doublecmd-help-ru
This package contains the documentation files for the Double Commander
designed for use with the external web browsers.

This package contains the documentation for the DC in Russian.

%prep
%setup -q -n %{doublecmd_help}

%build

%install
install -d     %{buildroot}/%{_datadir}/doublecmd/doc
cp -r en   %{buildroot}/%{_datadir}/doublecmd/doc
cp -r ru   %{buildroot}/%{_datadir}/doublecmd/doc

%clean
[ %{buildroot} != "/" ] && ( rm -rf %{buildroot} )

%files
%defattr(-,root,root)
%doc %{_datadir}/doublecmd/doc/en

%files -n doublecmd-help-ru
%defattr(-,root,root)
%doc %{_datadir}/doublecmd/doc/ru

%changelog
* Fri Jan 21 2011 - Alexander Koblov <Alexx2000@mail.ru>
- Initial package, version 0.4.6
