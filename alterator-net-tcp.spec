%define _altdata_dir %_datadir/alterator

Name: alterator-net-tcp
Version: 0.2
Release: alt1

Packager: Stanislav Ievlev <inger@altlinux.org>

BuildArch:	noarch

Source:%name-%version.tar

Summary: alterator module for tcp/ip connections configuration
License: GPL
Group: System/Configuration/Other
Requires: alterator >= 2.9

BuildPreReq: alterator >= 2.9-alt0.10, alterator-standalone >= 2.5-alt0.3

# Automatically added by buildreq on Mon Jul 11 2005 (-bi)
BuildRequires: alterator

%description
alterator module for tcp/ip connections configuration

%prep
%setup -q

%build
%make_build libdir=%_libdir

%install
%makeinstall DESTDIR=%buildroot
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
%_alterator_backend3dir/*
%_desktopdir/*


%changelog
* Mon Oct 16 2006 Stanislav Ievlev <inger@altlinux.org> 0.2-alt1
- replace command-arg-ref with modern woo-get-option

* Mon Oct 02 2006 Stanislav Ievlev <inger@altlinux.org> 0.1-alt1
- initial release

