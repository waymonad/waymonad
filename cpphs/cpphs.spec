#
# rpm spec for cpphs
#
# ======================================================================

Summary: Liberalized C pre-processor re-implementation in Haskell
Name: cpphs
Version: 1.4
Release: 1%{?_distver:.%{_distver}}
License: LGPL 2.1
Group: Development/Tools
Source: http://www.cs.york.ac.uk/fp/cpphs/cpphs-1.4.tar.gz
URL: http://www.cs.york.ac.uk/fp/cpphs/
Packager: Paul Heinlein <heinlein@galois.com>
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)
BuildRequires: ghc

%description
cpphs is a liberalised re-implementation of cpp, the C pre-processor,
in Haskell. Why re-implement cpp?

  * For some Haskell systems, notably Hugs on Windows, a true cpp is not
    available by default.
  * Even for the other Haskell systems, the common cpp provided by the gcc
    3.x and 4.x series is changing subtly in ways that are incompatible with
    Haskell's syntax. There have always been problems with, for instance,
    string gaps, and prime characters in identifiers. These problems are
    only going to get worse.

So, it seemed right to provide an alternative to cpp, both more compatible
with Haskell, and itself written in Haskell so that it can be distributed
with compilers.


%prep
%setup -q


%build
ghc --make cpphs -o cpphs
( cd tests && ./runtests )


%install
rm -rf ${RPM_BUILD_ROOT}
install -d ${RPM_BUILD_ROOT}%{_bindir}
install cpphs ${RPM_BUILD_ROOT}%{_bindir}
install -d ${RPM_BUILD_ROOT}%{_mandir}/man1
install -m 0644 docs/cpphs.1 ${RPM_BUILD_ROOT}%{_mandir}/man1


%clean
rm -rf ${RPM_BUILD_ROOT}


%files
%defattr(-,root,root)
%doc CHANGELOG README docs/design docs/index.html
%{_bindir}/cpphs
%doc %{_mandir}/man1/cpphs.*


%changelog
* Tue Apr 11 2006 Paul Heinlein <heinlein@galois.com> 1.1-1
- initial release

#
# eof
#
