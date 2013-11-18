#!/usr/bin/perl
# vim: set ts=2 sw=2 tw=99 noet: 

use strict;
use Cwd;
use File::Basename;
use File::Path;

my ($myself, $path) = fileparse($0);
chdir($path);

require 'helpers.pm';

#Go back above build dir
chdir(Build::PathFormat('../../..'));

#Get the source path.
our ($root) = getcwd();

my $reconf = 0;

if (!(-f 'OUTPUT/.ambuild2/graph') || !(-f 'OUTPUT/.ambuild2/vars')) {
	rmtree('OUTPUT');
	mkdir('OUTPUT') or die("Failed to create output folder: $!\n");
	chdir('OUTPUT');
	my ($result, $argn);
	$argn = $#ARGV + 1;
	print "Attempting to reconfigure...\n";
	if ($argn > 0 && $^O !~ /MSWin/) {
		$result = `CC=$ARGV[0] CXX=$ARGV[0] python ../build/configure.py --enable-optimize`;
	} else {
		if ($^O eq "linux") {
			$result = `CC=gcc-4.4 CXX=gcc-4.4 python ../build/configure.py --enable-optimize`;
		} elsif ($^O eq "darwin") {
			$result = `CC=clang CXX=clang python ../build/configure.py --enable-optimize`;
		} else {
			$result = `C:\\Python27\\Python.exe ..\\build\\configure.py --enable-optimize`;
		}
	}
	print "$result\n";
	if ($? != 0) {
		die('Could not configure!');
	}
	open(FILE, '>sentinel');
	print FILE "this is nothing.\n";
	close(FILE);
}

sub IsNewer
{
	my ($file, $time) = (@_);

	my @s = stat($file);
	my $mtime = $s[9];
	return $mtime > $time;
}

exit(0);


