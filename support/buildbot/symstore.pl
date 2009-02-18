#!/usr/bin/perl

use File::Basename;

my ($myself, $path) = fileparse($0);
chdir($path);

require 'helpers.pm';

chdir('..');
chdir('..');

our $SSH = 'ssh -i ../../mmspvkey';

#Sync us up with the main symbol store
rsync('sourcemm@alliedmods.net:~/public_html/symbols/', '..\\..\\symstore');

#Get version info
my ($version);
$version = Build::ProductVersion(Build::PathFormat('product.version'));
$version .= '-hg' . Build::HgRevNum('.');

symstore("loader\\msvc9\\server.*", $version);
symstore("core-legacy\\msvc9\\Release\\metamod.1.ep1.*", $version);
symstore("core\\msvc9\\Release - Orange Box\\metamod.2.ep2.*", $version);
symstore("core\\msvc9\\Release - Left 4 Dead\\metamod.2.l4d.*", $version);
symstore("core\\msvc9\\Release - Dark Messiah\\metamod.2.darkm.*", $version);

#Lowercase DLLs.  Sigh.
my (@files);
opendir(DIR, "..\\..\\symstore");
@files = readdir(DIR);
closedir(DIR);

my ($i, $j, $file, @subdirs);
for ($i = 0; $i <= $#files; $i++)
{
	$file = $files[$i];
	next unless ($file =~ /\.dll$/);
	next unless (-d "..\\..\\symstore\\$file");
	opendir(DIR, "..\\..\\symstore\\$file");
	@subdirs = readdir(DIR);
	closedir(DIR);
	for ($j = 0; $j <= $#subdirs; $j++)
	{
		next unless ($subdirs[$j] =~ /[A-Z]/);
		Build::Command("rename ..\\..\\symstore\\$file\\" . $subdirs[$j] . " " . lc($subdirs[$j]));
	}	
}

#Now that we're done, rsync back.
rsync('../../symstore/', 'sourcemm@alliedmods.net:~/public_html/symbols');

sub symstore
{
	my ($line, $version) = (@_);
	Build::Command("symstore add /r /f \"$line\" /s ..\\..\\symstore /t \"Metamod:Source\" /v \"$version\" /c \"buildbot\"");
}

sub rsync
{
	my ($from, $to) = (@_);
	
	Build::Command('rsync -av --delete -e="' . $SSH . '" ' . $from . ' ' . $to);
}
