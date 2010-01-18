#!/usr/bin/perl -i
#fix package decl
my $old=qq|in-package 'afl|;
my $new=qq|in-package :afl|;

while  (<>) {
s/$old/$new/go;
print;
}
