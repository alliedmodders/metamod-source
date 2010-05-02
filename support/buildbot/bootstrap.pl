#!/usr/bin/perl

use strict;
use Cwd;
use File::Basename;

my ($myself, $path) = fileparse($0);
chdir($path);

require 'helpers.pm';

#Go to main source dir
chdir(Build::PathFormat('../..'));

#Do the annoying revision bumping.
#Linux needs some help here.
if ($^O eq "linux")
{
	Build::Command("flip -u modules.versions");
	Build::Command("flip -u support/versionchanger.pl");
	Build::Command("chmod +x support/versionchanger.pl");
}
#Build::Command(Build::PathFormat('support/versionchanger.pl') . ' --buildstring="-dev"');

