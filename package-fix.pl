#!/usr/bin/perl -i
#
my $olduser="in-package 'user";
my $newuser="in-package :user";
my $oldafl="in-package 'afl";
my $newafl="in-package :afl";
while ( <>) {
  s/$olduser/$newuser/go;
  s/$oldafl/$newafl/go;
  print;
}
