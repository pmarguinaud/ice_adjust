#!/usr/bin/perl -w
#
use strict;
use FileHandle;
use Data::Dumper;

use FindBin qw ($Bin);
use lib $Bin;
use Fxtran;
use Inline;

my ($f) = @ARGV;

my $d = &Fxtran::fxtran (location => $f);

&Inline::inlineContainedSubroutines ($d);

'FileHandle'->new (">$f.new")->print ($d->textContent ());

