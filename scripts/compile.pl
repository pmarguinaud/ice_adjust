#!/home/gmap/mrpm/marguina/install/perl-5.32.0/bin/perl -w

use strict;
use FindBin qw ($Bin);
use lib $Bin;
use FileHandle;
use File::Copy;
use File::Basename;
use File::stat;
use File::Path;
use Getopt::Long;
use Data::Dumper;

my %opts;

sub newer
{
  my ($f1, $f2)  = @_;
  die unless (-f $f1);
  return 1 unless (-f $f2);
  return stat ($f1)->mtime > stat ($f2)->mtime;
}

sub prune
{
  my $d = shift;

  for my $mod (qw (MODE_TIWMX MODE_ICECLOUD MODD_BUDGET MODE_BUDGET))
    {
      my @use = &F ('.//use-stmt[string(module-N)="?"]', $mod, $d);
      $_->unbindNode for (@use);
    }

  my @call = &F ('.//call-stmt[string(procedure-designator)="ICECLOUD"]', $d);

  $_->unbindNode for (@call);

  my @stmt = &F ('.//ANY-stmt[.//named-E[string(N)="ESATW" or string(N)="ESATI"]]', $d);

  $_->unbindNode for (@stmt);

  my @if = &F ('.//if-stmt[./condition-E/named-E[string(N)="BUCONF"]]', $d);

  $_->unbindNode for (@if);

  my @args = qw (BUCONF TBUDGETS KBUDGETS);

  for my $arg (@args)
    {
      my ($decl) = &F ('.//T-decl-stmt[.//EN-decl[string(EN-N)="?"]]', $arg, $d);
      $decl->unbindNode () if ($decl);
      my ($dum) = &F ('.//dummy-arg-LT/arg-N[string(N)="?"]', $arg, $d);
      &Fxtran::removeListElement ($dum) if ($dum);
    }

}

sub copyIfNewer
{
  my ($f1, $f2) = @_;

  if (&newer ($f1, $f2))
    {
      print "Copy $f1 to $f2\n"; 
      &copy ($f1, $f2); 
    }
}

sub saveToFile
{
  my ($x, $f) = @_;

  unless (-d (my $d = &dirname ($f)))
    {
      &mkpath ($d);
    }

  'FileHandle'->new (">$f")->print ($x->textContent ());
  'FileHandle'->new (">$f.xml")->print ($x->toString ());
}

sub addSeqDirective
{
  my $d = shift;
  my ($pu) = &F ('./object/file/program-unit', $d);
  my ($N) = &F ('./subroutine-stmt/subroutine-N', $pu, 1);
  $pu->insertAfter (&n ("<C>!\$acc routine ($N) seq</C>"), $pu->firstChild);
  $pu->insertAfter (&t ("\n"), $pu->firstChild);
}

sub preProcessIfNewerCPU
{
  use Inline;
  use Associate;
  use Expr;
  use Fxtran;
  use Dimension;
  use Construct;
  use ReDim;
  use DrHook;

  my ($f1, $f2) = @_;

  if (&newer ($f1, $f2))
    {
      print "Preprocess $f1\n";

      my $d = &Fxtran::fxtran (location => $f1);
      &saveToFile ($d, "tmp/$f2");

      &prune ($d);
      &saveToFile ($d, "tmp/prune/$f2");

      &Inline::inlineContainedSubroutines ($d);
      &saveToFile ($d, "tmp/inlineContainedSubroutines/$f2");

      &Dimension::attachArraySpecToEntity ($d);
      &saveToFile ($d, "tmp/attachArraySpecToEntity/$f2");

      &Construct::changeIfStatementsInIfConstructs ($d);
      &saveToFile ($d, "tmp/changeIfStatementsInIfConstructs/$f2");

      &Loop::arraySyntaxLoop ($d);
      &saveToFile ($d, "tmp/arraySyntaxRange/$f2");

      &Loop::removeJlonLoops ($d);
      &saveToFile ($d, "tmp/removeJlonLoops/$f2");

      &ReDim::reDim ($d);
      &saveToFile ($d, "tmp/reDim/$f2");

      &addSeqDirective ($d);

      &Stack::addStack ($d);
      &saveToFile ($d, "tmp/addStack/$f2");

      &DrHook::remove ($d);
      &saveToFile ($d, "tmp/drhook/$f2");

      'FileHandle'->new (">$f2")->print ($d->textContent ());

#     &Fxtran::intfb ($f2);
    }
}

sub preProcessIfNewerGPU
{
  use Inline;
  use Associate;
  use Fxtran;
  use Blocks;
  use SingleBlock;
  use Vector;
  use Stack;
  use Loop;
  use Expr;
  use ReDim;
  use DrHook;

  my ($f1, $f2) = @_;

  if (&newer ($f1, $f2))
    {
      print "Preprocess $f1\n";


      my $d = &Fxtran::fxtran (location => $f1);
      &saveToFile ($d, "tmp/$f2");

      &prune ($d);
      &saveToFile ($d, "tmp/prune/$f2");

      &Inline::inlineContainedSubroutines ($d);
      &saveToFile ($d, "tmp/inlineContainedSubroutines/$f2");

      &Dimension::attachArraySpecToEntity ($d);
      &saveToFile ($d, "tmp/attachArraySpecToEntity/$f2");

      &Construct::changeIfStatementsInIfConstructs ($d);
      &saveToFile ($d, "tmp/changeIfStatementsInIfConstructs/$f2");

      &Loop::arraySyntaxLoop ($d);
      &saveToFile ($d, "tmp/arraySyntaxRange/$f2");

      &Loop::removeJlonLoops ($d);
      &saveToFile ($d, "tmp/removeJlonLoops/$f2");

      &ReDim::reDim ($d);
      &saveToFile ($d, "tmp/reDim/$f2");

      &addSeqDirective ($d);

      &Stack::addStack ($d);
      &saveToFile ($d, "tmp/addStack/$f2");

      &DrHook::remove ($d);
      &saveToFile ($d, "tmp/drhook/$f2");

      'FileHandle'->new (">$f2")->print ($d->textContent ());

#     &Fxtran::intfb ($f2);
    }
}

my @opts_f = qw (update compile);
my @opts_s = qw (arch);

&GetOptions
(
  map ({ ($_,     \$opts{$_}) } @opts_f),
  map ({ ("$_=s", \$opts{$_}) } @opts_s),
);

my @compute = map { &basename ($_) } <compute/*.F90>;
my @support = map { &basename ($_) } <support/*>;

&mkpath ("compile.$opts{arch}");

chdir ("compile.$opts{arch}");

if ($opts{update})
  {
    for my $f (@support)
      {
        &copyIfNewer ("../support/$f", $f);
      }
    
    if ($opts{arch} =~ m/^gpu/o)
      {
        for my $f (@compute)
          {
            &preProcessIfNewerGPU ("../compute/$f", $f);
          }
      }
   else
     {
        for my $f (@compute)
          {
            &preProcessIfNewerCPU ("../compute/$f", $f);
          }
     }

    &copy ("../Makefile.$opts{arch}", "Makefile.inc");

    system ("$Bin/Makefile.PL") and die;
  }

if ($opts{compile})
  {
    system ('make -j4 main.x') and die;
  }





