#!/home/gmap/mrpm/marguina/install/perl-5.32.0/bin/perl -w
#
use strict;
use FileHandle;
use Data::Dumper;

use FindBin qw ($Bin);
use lib $Bin;
use Fxtran;

my $F90 = shift;

my $d = &Fxtran::fxtran (location => $F90);

my @en_decl = (&F ('.//T-decl-stmt[./attribute//shape-spec[string(upper-bound)="D%NJT"]]//EN-decl', $d),
               &F ('.//EN-decl[.//shape-spec[string(upper-bound)="D%NJT"]]', $d));

for my $en_decl (@en_decl)
  {
    my ($N) = &F ('./EN-N', $en_decl, 1);
    my @expr = &F ('.//named-E[string(N)="?"]', $N, $d);
    for my $expr (@expr)
      {
        my ($r) = &F ('./R-LT/ANY-R', $expr);
        next unless ($r);

        if ($r->nodeName eq 'parens-R')
          {
            my @e = &F ('./element-LT/element', $r);
            &Fxtran::removeListElement ($e[1]);
          }
        elsif ($r->nodeName eq 'array-R')
          {
            my @e = &F ('./section-subscript-LT/section-subscript', $r);
            &Fxtran::removeListElement ($e[1]);
          }
        else
          {
            die $r;
          }

      }
  }

my @dim = &F ('.//shape-spec[string(upper-bound)="D%NJT"]', $d);

for my $dim (@dim)
  {
    &Fxtran::removeListElement ($dim);
  }

my @do = &F ('.//do-construct[./do-stmt[string(./do-V)="JJ"]]', $d);

for my $do (@do)
  {
    my $C = &n ('<C/>');
    $do->parentNode->insertAfter ($C, $do);
    $do->firstChild->unbindNode ();
    $do->lastChild->unbindNode ();
    for my $c ($do->childNodes ())
      {
        $do->parentNode->insertBefore ($c, $C);
      }
    $do->unbindNode ();
    $C->unbindNode ();
  }

'FileHandle'->new (">$F90.new")->print ($d->textContent);

