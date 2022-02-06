#!/usr/bin/perl -w
#
use strict;
use FileHandle;
use Data::Dumper;

use FindBin qw ($Bin);
use lib $Bin;
use Fxtran;
use ReDim;

my $f = shift;

my $d = &Fxtran::fxtran (location => $f);

&ReDim::reDim ($d);

'FileHandle'->new (">$f.new")->print ($d->textContent);
