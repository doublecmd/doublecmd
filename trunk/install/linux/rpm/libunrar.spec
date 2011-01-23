Name:		libunrar
Version:	4.0.4
Release:	1
Summary:	Library for extract *.rar format archives
Source:		http://www.rarlab.com/rar/unrarsrc-%{version}.tar.gz
Url:		http://www.rarlab.com/rar_add.htm
License:	Freeware
Group:		System/Libraries
BuildRoot:	%{_tmppath}/%{name}-%{version}-root

%description
This library can be used by software developers to extract *.rar format archives.

%prep
%setup -q -n unrar

%post -p /sbin/ldconfig
%postun -p /sbin/ldconfig

%build
make lib -f makefile.unix CXXFLAGS="%{optflags} -fPIC -DSILENT" LDFLAGS="%{ldflags}" STRIP=true

%install
install -d -m 755 %{buildroot}/%{_libdir}
install -m 644 libunrar.so %{buildroot}/%{_libdir}

%clean
[ %{buildroot} != "/" ] && ( rm -rf %{buildroot} )

%files
%defattr(-,root,root)
%doc license.txt readme.txt
%{_libdir}/libunrar.so

%changelog
* Sun Jan 23 2011 Alexander Koblov <alexx2000@mail.ru>
- Initial release of library

* Sat Jul 10 2010 GÃ¶tz Waschk <waschk@mandriva.org> 3.93-1mdv2011.0
+ Revision: 550262
- new version

* Mon Dec 21 2009 Funda Wang <fwang@mandriva.org> 3.91-1mdv2010.1
+ Revision: 480547
- new version 3.91

* Thu Aug 20 2009 GÃ¶tz Waschk <waschk@mandriva.org> 3.90-2mdv2010.0
+ Revision: 418576
- rebuild for broken build system

* Thu Aug 20 2009 GÃ¶tz Waschk <waschk@mandriva.org> 3.90-1mdv2010.0
+ Revision: 418546
- new version

* Mon Jul 13 2009 GÃ¶tz Waschk <waschk@mandriva.org> 3.90-0.beta4.1mdv2010.0
+ Revision: 395466
- new version

* Sat Jun 20 2009 GÃ¶tz Waschk <waschk@mandriva.org> 3.90-0.beta3.1mdv2010.0
+ Revision: 387518
- new version

* Mon Jun 08 2009 GÃ¶tz Waschk <waschk@mandriva.org> 3.90-0.beta2.1mdv2010.0
+ Revision: 383871
- new version

* Thu May 07 2009 GÃ¶tz Waschk <waschk@mandriva.org> 3.90-0.beta1.1mdv2010.0
+ Revision: 372937
- new version

* Tue Feb 03 2009 Guillaume Rousse <guillomovitch@mandriva.org> 3.80-2mdv2009.1
+ Revision: 337120
- keep bash completion in its own package

* Wed Jan 07 2009 GÃ¶tz Waschk <waschk@mandriva.org> 3.80-1mdv2009.1
+ Revision: 326892
- new version

* Sun Oct 12 2008 Funda Wang <fwang@mandriva.org> 3.80-0.beta4.1mdv2009.1
+ Revision: 292676
- 3.8.4
- New version 3.8 beta3

* Wed Jun 25 2008 Funda Wang <fwang@mandriva.org> 3.80-0.beta2.1mdv2009.0
+ Revision: 228841
- New version 3.80 beta 2

* Tue Jun 10 2008 GÃ¶tz Waschk <waschk@mandriva.org> 3.80-0.beta1.1mdv2009.0
+ Revision: 217391
- new version

  + Funda Wang <fwang@mandriva.org>
    - Revert to actual version of program
    - fix real version

* Fri Feb 01 2008 Anssi Hannula <anssi@mandriva.org> 3.71-0.beta1.2mdv2008.1
+ Revision: 161265
- move to Mandriva non-free from PLF (license is no longer unclear,
  it now allows redistribution explicitely)
- drop pre-MDK10.0 support
- fix license tag capitalization
- import unrar


* Wed Dec 12 2007 Götz Waschk <goetz@zarb.org> 3.71-0.beta1.1plf2008.1
- new version

* Tue Nov  6 2007 Götz Waschk <goetz@zarb.org> 3.70-1plf2008.1
- new version

* Mon Apr 16 2007 Götz Waschk <goetz@zarb.org> 3.70-0.beta7.1plf2007.1
- new version

* Tue Mar  6 2007 Götz Waschk <goetz@zarb.org> 3.70-0.beta4.1plf2007.1
- new version

* Tue Feb  6 2007 Götz Waschk <goetz@zarb.org> 3.70-0.beta3.1plf2007.1
- drop patch
- new version

* Wed Jan 24 2007 Götz Waschk <goetz@zarb.org> 3.70-0.beta1.1plf2007.1
- don't strip the binary at build stage
- new version

* Thu Nov  2 2006 Götz Waschk <goetz@zarb.org> 3.60-1plf2007.1
- update description
- new version

* Mon Jul 24 2006 Götz Waschk <goetz@zarb.org> 3.60-0.beta7.1plf2007.0
- new version

* Wed Jun 28 2006 Götz Waschk <goetz@zarb.org> 3.60-0.beta5.1plf2007.0
- new version

* Tue Oct 11 2005 Götz Waschk <goetz@zarb.org> 3.51-1plf
- new version

* Tue Aug  9 2005 Götz Waschk <goetz@zarb.org> 3.50-1plf
- new version

* Tue Apr 19 2005 Götz Waschk <goetz@zarb.org> 3.50-0.beta1.1plf
- mkrel
- new version

* Mon Sep 20 2004 Götz Waschk <goetz@zarb.org> 3.40-1plf
- fix description
- new version

* Sun Jun  6 2004 Götz Waschk <goetz@plf.zarb.org> 3.30-2plf
- rebuild for new g++

* Wed Apr 21 2004 Götz Waschk <goetz@plf.zarb.org> 3.30-1plf
- new version

* Tue Dec 30 2003 Götz Waschk <goetz@plf.zarb.org> 3.20-2plf
- add unrar bash-completion for Cooker builds

* Fri Jun 20 2003 Götz Waschk <goetz@plf.zarb.org> 3.20-1plf
- small build patch for gcc 3.3
- new version

* Sun Mar 30 2003 Götz Waschk <goetz@plf.zarb.org> 3.20-0.beta2.1plf
- arrgh, the displayed version is 3.20 beta 2

* Sat Mar 29 2003 Götz Waschk <goetz@plf.zarb.org> 3.2.0-0.beta2.1plf
- new version

* Fri Feb 14 2003 Götz Waschk <goetz@plf.zarb.org> 3.10-2plf
- use default optimization flags (Francisco Javier Felix)

* Wed Jan  8 2003 Götz Waschk <waschk@informatik.uni-rostock.de> 3.10-1plf


* Tue Dec  3 2002 Götz Waschk <waschk@informatik.uni-rostock.de> 3.10-0.beta3.1plf
- 3.10 beta 3

* Tue Oct 22 2002 Götz Waschk <waschk@informatik.uni-rostock.de> 3.10-0.beta1.1plf
- fix url
- add some docs
- drop patch
- quiet tar
- set version to 3.10 beta 1, that's the output of the program
- new version

* Mon Aug 26 2002 Guillaume Rousse <guillomovitch@plf.zarb.org> 3.0-1plf 
- first PLF release, with patch from Pascal Terjan <pascal.terjan@free.fr>

