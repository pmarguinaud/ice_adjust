#!/usr/bin/perl -w
#
use strict;
use FileHandle;
use Data::Dumper;

use FindBin qw ($Bin);
use lib $Bin;
use Fxtran;
use Associate;

my ($f) = @ARGV;

my $d = &Fxtran::fxtran (location => $f);

&Associate::resolveAssociates ($d);

'FileHandle'->new (">$f.new")->print ($d1->textContent);


