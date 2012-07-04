Summary: java source to bytecode compiler
License: IBM Public License, http://ibm.com/developerworks/oss/license10.html
Group: Development/Languages
Name: jikes
Prefix: /usr
Release: 1
Source: %{name}-%{version}.tar.gz
URL: http://ibm.com/developerworks/opensource/jikes
Buildroot: /tmp/jikesrpm
Packager: %{_gpg_name}

%description
The IBM Jikes compiler translates Java source files to bytecode. It
also supports incremental compilation and automatic makefile generation,
and is maintained by the Jikes Project:
http://ibm.com/developerworks/opensource/jikes/

%prep
%setup -q

%build
./configure CXXFLAGS=-O3 --prefix=$RPM_BUILD_ROOT/%{_prefix} \
  --mandir=$RPM_BUILD_ROOT/%{_mandir} --datadir=$RPM_BUILD_ROOT/%{_datadir}
make

%install
make install

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root)
%doc AUTHORS COPYING ChangeLog NEWS README TODO
%doc %{_datadir}/doc/%{name}-%{version}/license.htm
%doc %{_mandir}/man1/%{name}.1*
%{_bindir}/%{name}
%{_includedir}/%{name}api.h
