Summary: java source to bytecode compiler
%define version 1.06
Buildroot: /var/tmp/jikes-%{version}-root
Copyright: IBM Public License, http://ibm.com/developerworks/opensource
Group: Development/Languages
Name: jikes
Packager: David Shields (shields@watson.ibm.com)
Prefix: /usr
Provides: jikes
Release: 1
Source: http://www10.software.ibm.com/developerworks/opensource/jikes/project/pub/jikes-%{version}.tar.gz 
URL: http://ibm.com/developerworks/opensource
Version: %{version}


%description
The IBM Jikes compiler translates Java source files to bytecode. It
also supports incremental compilation and automatic makefile generation,
and is maintained by the Jikes Project:
  http://ibm.com/developerworks/opensource
%prep
%setup -q -n jikes
cd src

%build
cd src
./configure --prefix=$RPM_BUILD_ROOT
make

%install
rm -fr $RPM_BUILD_ROOT
cd src
make prefix=$RPM_BUILD_ROOT/usr install

%clean
rm -fr $RPM_BUILD_ROOT

%files
%defattr(-,-,root)
%doc README contrib.htm jikes.htm license.htm news.htm
%doc /usr/man/man1/jikes.1
/usr/bin/jikes
