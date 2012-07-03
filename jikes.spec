Summary: Java source to bytecode compiler
%define version 1.04
Name: jikes
Provides: jikes
Version: %{version}
Release: 1
Copyright: IBM Public License Version 1.0 - Jikes Compiler, http://ibm.com/research/jikes/license/license3.htm
Group: Development/Languages
Url: http://www.ibm.com/research/jikes 
Packager: David Shields (shields@watson.ibm.com)
Source: http://research.ibm.com/jikes/download/jikes-%{version}_tar.gz 
Buildroot: /var/tmp/jikes-%{version}-root

Prefix: /usr

%description
The IBM Jikes compiler translates Java source files to bytecode.
Features include strict adherence to the language specification,
extremely fast compile speed, and a sophisticated dependence analysis 
that allows incremental compilation and automatic makefile generation.

%prep
%setup -q -n jikes
cd src

%build
cd src
./configure --prefix=$RPM_BUILD_ROOT
make

%install
rm -rf $RPM_BUILD_ROOT
cd src
make prefix=$RPM_BUILD_ROOT/usr install

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,-,root)
%doc README contrib.htm jikes.htm license.htm news.htm
%doc /usr/man/man1/jikes.1
/usr/bin/jikes

