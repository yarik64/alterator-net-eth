%define _altdata_dir %_datadir/alterator

Name: alterator-net-eth
Version: 4.3
Release: alt2

Packager: Stanislav Ievlev <inger@altlinux.org>

Source:%name-%version.tar

Summary: alterator module for tcp/ip connections configuration
License: GPL
Group: System/Configuration/Other
Requires: alterator >= 4.7-alt3 alterator-sh-functions >= 0.6-alt5 libshell >= 0.0.1-alt4
Requires: alterator-l10n >= 2.1-alt5
Requires: alterator-net-common >= 0.7-alt1
Requires: alterator-net-wifi >= 0.5-alt1
Requires: etcnet openresolv avahi-autoipd startup >= 0.9.8.21-alt1

Conflicts: alterator-lookout < 1.6-alt6
Conflicts: alterator-fbi < 5.9-alt1
Conflicts: alterator-browser-qt < 2.9.76-alt1
Conflicts: filesystem < 2.3.4-alt1


%add_findreq_skiplist %_datadir/install2/preinstall.d/*

BuildPreReq: alterator >= 4.7-alt3

BuildArch: noarch

#old names
Provides: alterator-network = %version
Obsoletes: alterator-network

Provides: alterator-net-tcp = %version
Obsoletes: alterator-net-tcp

Provides: alterator-backend-simple_etcnet = %version
Obsoletes: alterator-backend-simple_etcnet

Provides: alterator-net-general = %version
Obsoletes: alterator-net-general

# Automatically added by buildreq on Mon Jul 11 2005 (-bi)
BuildRequires: alterator

%description
alterator module for tcp/ip connections configuration

%prep
%setup -q

%build
%make_build

%install
%makeinstall

%files
%_altdata_dir/applications/*
%_altdata_dir/ui/*/
%_alterator_backend3dir/*
%_datadir/install2/preinstall.d/*
%attr(700,root,root) %dir %_libexecdir/alterator/hooks/net-eth.d

%changelog
* Mon Mar 23 2009 Stanislav Ievlev <inger@altlinux.org> 4.3-alt2
- stop interface with old configuration, start with new one.

* Thu Mar 19 2009 Stanislav Ievlev <inger@altlinux.org> 4.3-alt1
- edit resolv.conf for each interface

* Mon Mar 16 2009 Stanislav Ievlev <inger@altlinux.org> 4.2-alt4
- don't stop network interfaces during installation process

* Fri Mar 13 2009 Mikhail Efremov <sem@altlinux.org> 4.2-alt3
- use 'controlled' parameter in preinstall.d script
- removed unused variable

* Thu Mar 12 2009 Mikhail Efremov <sem@altlinux.org> 4.2-alt2
- updated UI for installer

* Thu Mar 12 2009 Mikhail Efremov <sem@altlinux.org> 4.2-alt1
- added 'Controlled by' combobox
- allow configuration when interface controlled by NetworkManager

* Tue Mar 10 2009 Stanislav Ievlev <inger@altlinux.org> 4.1-alt3
- more powerfull "on update" hook

* Thu Mar 05 2009 Stanislav Ievlev <inger@altlinux.org> 4.1-alt2
- minimize number of layers to work with cache
- restart only "cached" interfaces (TODO: restart only changed interfaces)
- add hooks if ip was changed

* Wed Mar 04 2009 Stanislav Ievlev <inger@altlinux.org> 4.1-alt1
- edit computer name instead of full name
- improve hostname.d hooks calling

* Wed Feb 25 2009 Stanislav Ievlev <inger@altlinux.org> 4.0-alt3
- run /etc/hooks/hostname.d instead of hostname binding to 127.0.0.1
- improve UI horizontal size

* Wed Feb 18 2009 Stanislav Ievlev <inger@altlinux.org> 4.0-alt2
- fix work in wizardface

* Mon Feb 16 2009 Stanislav Ievlev <inger@altlinux.org> 4.0-alt1
- new ajax based html interface, use new form API
- improve UI: add network adaptor name
- use translations directly from alterator-l10n

* Thu Jan 15 2009 Stanislav Ievlev <inger@altlinux.org> 3.3-alt1
- don't setup /etc/HOSTNAME
- use help directly from alterator-l10n

* Tue Dec 09 2008 Vladislav Zavjalov <slazav@altlinux.org> 3.2-alt9
- rebuild with new alterator-l10n (fix help)

* Fri Dec 05 2008 Vladislav Zavjalov <slazav@altlinux.org> 3.2-alt8
- use help from l10n
- rebuild

* Thu Nov 13 2008 Vladislav Zavjalov <slazav@altlinux.org> 3.2-alt7
- hide "Wireless settings" ref for non-w/l ifaces
- printf -> write_bool_param in backend

* Wed Nov 05 2008 Vladislav Zavjalov <slazav@altlinux.org> 3.2-alt6
- fix help html

* Wed Sep 17 2008 Stanislav Ievlev <inger@altlinux.org> 3.2-alt5
- rebuild with alterator-l10n

* Wed Sep 17 2008 Vladislav Zavjalov <slazav@altlinux.org> 3.2-alt4
- fix help and desktop-file (by mike@)

* Tue Sep 16 2008 Vladislav Zavjalov <slazav@altlinux.org> 3.2-alt3
- add Requires: alterator-sh-functions >= 0.6-alt5 (write_type_item)

* Tue Sep 16 2008 Vladislav Zavjalov <slazav@altlinux.org> 3.2-alt2
- don't show NM item when NM isn't installed

* Mon Sep 15 2008 Stanislav Ievlev <inger@altlinux.org> 3.2-alt1
- replace constraints with types

* Mon Sep 08 2008 Stanislav Ievlev <inger@altlinux.org> 3.1-alt6
- new configuration 'Managed by user (NetworkManager)'

* Fri Sep 05 2008 Stanislav Ievlev <inger@altlinux.org> 3.1-alt5
- add ability to skip interface restart on commit (parameter restart)

* Fri Aug 29 2008 Stanislav Ievlev <inger@altlinux.org> 3.1-alt4
- workaround: shutdown interface if we disable it

* Tue Jul 01 2008 Stanislav Ievlev <inger@altlinux.org> 3.1-alt3
- simplify i18n declarations
- replace effect-enable with effect-disable

* Mon Jun 23 2008 Stanislav Ievlev <inger@altlinux.org> 3.1-alt2
- rename: effect-update to update-effect
          effect-init to init-effect

* Fri Jun 20 2008 Stanislav Ievlev <inger@altlinux.org> 3.1-alt1
- use effect-disable

* Tue Jun 17 2008 Stanislav Ievlev <inger@altlinux.org> 3.0-alt1
- more functions from alterator-net-common

* Mon Jun 09 2008 Stanislav Ievlev <inger@altlinux.org> 2.2-alt12
- remove translations for desktop file
- remove po-file
- use common module.mak

* Tue Jun 03 2008 Vladislav Zavjalov <slazav@altlinux.org> 2.2-alt11
- fix label

* Mon Jun 02 2008 Vladislav Zavjalov <slazav@altlinux.org> 2.2-alt10
- add zeroconf
- Requires: avahi-autoipd

* Wed May 28 2008 Vladislav Zavjalov <slazav@altlinux.org> 2.1-alt10
- remove  "hrefarg" attr in href to net-wifi

* Wed May 28 2008 Vladislav Zavjalov <slazav@altlinux.org> 2.1-alt8
- change "target" attr to "hrefarg" in href to net-wifi 
  (see alterator-fbi-2.4-alt6)

* Mon May 26 2008 Vladislav Zavjalov <slazav@altlinux.org> 2.1-alt7
- change href to net-wifi for new alterator-net-wifi module

* Fri May 23 2008 Stanislav Ievlev <inger@altlinux.org> 2.1-alt6
- remove autoinstall backend usage

* Thu May 22 2008 Vladislav Zavjalov <slazav@altlinux.org> 2.1-alt5
- fix prev change
- add help from alterator-net-general

* Mon May 19 2008 Vladislav Zavjalov <slazav@altlinux.org> 2.1-alt4
- don't restart interface used in installer

* Thu May 15 2008 Stanislav Ievlev <inger@altlinux.org> 2.1-alt3
- fix cache initialization

* Tue May 13 2008 Stanislav Ievlev <inger@altlinux.org> 2.1-alt2
- use write_string_param
- remove po files
- use common select.js from alterator-fbi

* Thu May 08 2008 Stanislav Ievlev <inger@altlinux.org> 2.1-alt1
- use enumref, fix constraints

* Wed Apr 30 2008 Stanislav Ievlev <inger@altlinux.org> 2.0-alt3
- join to common translation database
- call update_chrooted on commit

* Tue Apr 29 2008 Stanislav Ievlev <inger@altlinux.org> 2.0-alt2
- update html UI for latest changes

* Mon Apr 28 2008 Stanislav Ievlev <inger@altlinux.org> 2.0-alt1
- merge with alterator-net-general

* Sat Apr 26 2008 Stanislav Ievlev <inger@altlinux.org> 1.0-alt6
- html UI: remove template-*, sync with qt UI
- backend: don't use ifvar utility, simplify list_iface result

* Fri Apr 25 2008 Stanislav Ievlev <inger@altlinux.org> 1.0-alt5
- improve qt UI

* Thu Apr 24 2008 Stanislav Ievlev <inger@altlinux.org> 1.0-alt4
- remove html-messages
- create common behaviour both for installer and acc

* Mon Mar 24 2008 Vladislav Zavjalov <slazav@altlinux.org> 1.0-alt3
- remove layout-policy attribute (bug #14945)
- remove quit button from UI

* Fri Feb 29 2008 Vladislav Zavjalov <slazav@altlinux.org> 1.0-alt2
- removed () in ifaceinfo

* Mon Feb 18 2008 Stanislav Ievlev <inger@altlinux.org> 1.0-alt1
- little interface improvements
- use alterator-sh-functions and libshell

* Mon Jan 14 2008 Stanislav Ievlev <inger@altlinux.org> 0.9-alt1
- update to new help system
- new feature: replace stupid iftabupdate with interface binding to hardware

* Wed Nov 14 2007 Stanislav Ievlev <inger@altlinux.org> 0.8-alt7
- use new ifcheckwireless common script

* Tue Oct 30 2007 Stanislav Ievlev <inger@altlinux.org> 0.8-alt6
- fix iftab generation
- move tools to alterator-net-common

* Tue Sep 25 2007 Stanislav Ievlev <inger@altlinux.org> 0.8-alt5
- replace C tool with shell script

* Wed Aug 15 2007 Stanislav Ievlev <inger@altlinux.org> 0.8-alt4
- updated Ukrainian translation

* Thu Aug 02 2007 Stanislav Ievlev <inger@altlinux.org> 0.8-alt3
- try to fix behaviour in installer

* Wed Aug 01 2007 Stanislav Ievlev <inger@altlinux.org> 0.8-alt2
- fix iftab creating

* Thu Jul 19 2007 Stanislav Ievlev <inger@altlinux.org> 0.8-alt1
- move all common functions to common place (alterator-net-common),
- replace ifdump with ifvar (ifdump was too slow)

* Wed Jul 18 2007 Stanislav Ievlev <inger@altlinux.org> 0.7-alt7
- avoid wrong mac addresses from broken drivers (like rt61)
- don't use constraints (was conflict in tab widget)

* Wed Jun 13 2007 Stanislav Ievlev <inger@altlinux.org> 0.7-alt6
- add desktop file
- use std woo-list/name+label function

* Thu May 31 2007 Stanislav Ievlev <inger@altlinux.org> 0.7-alt5
- add interface status information
- remove 'enabled' parameter

* Thu May 31 2007 Stanislav Ievlev <inger@altlinux.org> 0.7-alt4
- rebuild with latest standalone
- add interface status information

* Thu May 10 2007 Stanislav Ievlev <inger@altlinux.org> 0.7-alt3
- fix information about non-configured devices

* Mon May 07 2007 Stanislav Ievlev <inger@altlinux.org> 0.7-alt2
- return apply,cancel buttons to old places
- update call of /net-wifi sub-dialog

* Fri May 04 2007 Stanislav Ievlev <inger@altlinux.org> 0.7-alt1
- rename module to net-eth
- bugfix: enforce eth type in etcnet configs

* Thu May 03 2007 Stanislav Ievlev <inger@altlinux.org> 0.6-alt22
- shutup ifup (in verbose mode)

* Fri Apr 27 2007 Stanislav Ievlev <inger@altlinux.org> 0.6-alt21
- add link to wireless settings

* Mon Apr 23 2007 Stanislav Ievlev <inger@altlinux.org> 0.6-alt20
- update Ukrainian translation

* Thu Apr 19 2007 Stanislav Ievlev <inger@altlinux.org> 0.6-alt19
- little CSS optimizations

* Mon Apr 16 2007 Stanislav Ievlev <inger@altlinux.org> 0.6-alt18
- fix work with empty interface list

* Wed Apr 11 2007 Stanislav Ievlev <inger@altlinux.org> 0.6-alt16
- backend: start loopback device at start

* Thu Apr 05 2007 Stanislav Ievlev <inger@altlinux.org> 0.6-alt15
- temporary remove config-* scripts

* Wed Apr 04 2007 Alexey Gladkov <legion@altlinux.ru> 0.6-alt14.1
- Add netmask default value.

* Tue Apr 03 2007 Stanislav Ievlev <inger@altlinux.org> 0.6-alt14
- usability improvements (reverse mask list)

* Fri Mar 30 2007 Stanislav Ievlev <inger@altlinux.org> 0.6-alt13
- add Ukrainian translation
- help improvements from kirill@
- fix titles


* Wed Mar 28 2007 Stanislav Ievlev <inger@altlinux.org> 0.6-alt12
- improve po template generation
- add documentation

* Mon Mar 26 2007 Stanislav Ievlev <inger@altlinux.org> 0.6-alt11
- assign weight

* Wed Mar 14 2007 Stanislav Ievlev <inger@altlinux.org> 0.6-alt10
- assign 'Network' group

* Wed Mar 07 2007 Stanislav Ievlev <inger@altlinux.org> 0.6-alt9
- more translations
- rename to 'IP Interfaces'

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

