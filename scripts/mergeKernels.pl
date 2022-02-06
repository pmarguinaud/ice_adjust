#!/usr/bin/perl -w
#
use strict;
use FileHandle;
use Data::Dumper;

use FindBin qw ($Bin);
use lib $Bin;
use Block;

my $f = shift;

my $d = &Fxtran::fxtran (location => $f);

&Blocks::mergeKernels ($d);

'FileHandle'->new (">$f.new")->print ($doc->textContent);
