#!/usr/bin/perl -w
#
use strict;
use FileHandle;
use Data::Dumper;

use FindBin qw ($Bin);
use lib $Bin;
use Fxtran;

my $f = shift;

my $d = &Fxtran::fxtran (location => $f);

my @n = &F ($ARGV[0], $d);

for my $n (@n)
  {
    print $n->textContent, "\n" x 2;
  }


