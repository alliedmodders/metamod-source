#!/usr/bin/perl
# vim: set ts=2 sw=2 tw=99 noet: 

use File::Basename;

my ($myself, $path) = fileparse($0);
chdir($path);

require 'helpers.pm';

chdir('../../../OUTPUT');

system("ambuild --no-color 2>&1");

if ($? != 0)
{
	die "Build failed: $!\n";
}
else
{
	exit(0);
}

