%define _altdata_dir %_datadir/alterator

Name: alterator-net-tcp
Version: 0.6
Release: alt8

Packager: Stanislav Ievlev <inger@altlinux.org>

Source:%name-%version.tar

Summary: alterator module for tcp/ip connections configuration
License: GPL
Group: System/Configuration/Other
Requires: alterator >= 2.9 gettext

BuildPreReq: alterator >= 2.9-alt0.10, alterator-standalone >= 2.5-alt0.3, alterator-fbi >= 0.7-alt1

Provides: alterator-network = %version
Obsoletes: alterator-network

Provides: alterator-backend-simple_etcnet = %version
Obsoletes: alterator-backend-simple_etcnet

# Automatically added by buildreq on Mon Jul 11 2005 (-bi)
BuildRequires: alterator

%description
alterator module for tcp/ip connections configuration

%prep
%setup -q

%build
%make_build libdir=%_libdir

%install
%makeinstall HTMLROOT=%buildroot%_var/www/
%find_lang %name

%post
%update_menus
%postun
%clean_menus


%files -f %name.lang
%_sysconfdir/alterator/profile.d/*
%_bindir/*
%_altdata_dir/maps/*
%_altdata_dir/ui/*/
%_var/www/html/*
%_desktopdir/*
%_alterator_backend3dir/*


%changelog
* Mon Mar 05 2007 Stanislav Ievlev <inger@altlinux.org> 0.6-alt8
- prepare for tabbox

* Tue Feb 27 2007 Stanislav Ievlev <inger@altlinux.org> 0.6-alt7
- another improvements from legion
  (Auto save network settings on select another interface,
   Restart network interfaces on exit.)

* Thu Feb 22 2007 Stanislav Ievlev <inger@altlinux.org> 0.6-alt6
- improvements from legion
  (Return default parameters in read_iface() if iface does not exist.
   Add some checks.)

* Wed Feb 21 2007 Stanislav Ievlev <inger@altlinux.org> 0.6-alt5
- automatically fill iftab

* Mon Feb 19 2007 Stanislav Ievlev <inger@altlinux.org> 0.6-alt4
- simplify backend: separate interface name and interface label
- html: add link to top-level menu

* Fri Feb 16 2007 Stanislav Ievlev <inger@altlinux.org> 0.6-alt3
- improve wireless detection

* Wed Feb 14 2007 Stanislav Ievlev <inger@altlinux.org> 0.6-alt2
- fix ui layout
- add hrefs to wireless settings and general settings
- improve interface listing algo

* Thu Feb 08 2007 Stanislav Ievlev <inger@altlinux.org> 0.6-alt1
- add fbi data
- obsolete alterator-network and alterator-backend-simple_etcnet

* Wed Feb 07 2007 Stanislav Ievlev <inger@altlinux.org> 0.5-alt2
- clean environment before ifup/ifdown start (etcnet has problems)

* Mon Feb 05 2007 Stanislav Ievlev <inger@altlinux.org> 0.5-alt1
- add card description to interface list
- fix wrong grep (/eth0/eth/)

* Mon Jan 29 2007 Stanislav Ievlev <inger@altlinux.org> 0.4-alt3
- add support for autoinstall

* Mon Jan 15 2007 Stanislav Ievlev <inger@altlinux.org> 0.4-alt2
- require gettext
- fix path to ifup/ifdown tools

* Fri Jan 12 2007 Stanislav Ievlev <inger@altlinux.org> 0.4-alt1
- add label constraints
- add translations
- use ifup/ifdown instead of full network restart

* Thu Dec 14 2006 Stanislav Ievlev <inger@altlinux.org> 0.3-alt3
- special behaviour for installer

* Wed Dec 13 2006 Stanislav Ievlev <inger@altlinux.org> 0.3-alt2
- improve backend: add default values, don't fail on read /

* Tue Dec 05 2006 Stanislav Ievlev <inger@altlinux.org> 0.3-alt1
- add tool to converting masks
- generate mask names in backend

* Fri Dec 01 2006 Stanislav Ievlev <inger@altlinux.org> 0.2-alt6
- show splash message during network restart

* Thu Nov 30 2006 Stanislav Ievlev <inger@altlinux.org> 0.2-alt5
- tunings for installer

* Wed Nov 22 2006 Stanislav Ievlev <inger@altlinux.org> 0.2-alt4
- add "exclude" constraints

* Thu Nov 16 2006 Stanislav Ievlev <inger@altlinux.org> 0.2-alt3
- enable constraints

* Wed Nov 01 2006 Stanislav Ievlev <inger@altlinux.org> 0.2-alt2
- minor bugfixes

* Mon Oct 16 2006 Stanislav Ievlev <inger@altlinux.org> 0.2-alt1
- replace command-arg-ref with modern woo-get-option

* Mon Oct 02 2006 Stanislav Ievlev <inger@altlinux.org> 0.1-alt1
- initial release

