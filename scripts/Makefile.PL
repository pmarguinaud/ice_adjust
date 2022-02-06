#!/home/gmap/mrpm/marguina/install/perl-5.32.0/bin/perl -w
#

use FileHandle;
use Data::Dumper;
use FindBin qw ($Bin);
use lib $Bin;
use Fxtran;

use strict;

my @sys = qw (OMP_LIB CUDAFOR ISO_C_BINDING);

sub slurp
{
  my $f = shift;
  do { local $/ = undef; my $fh = 'FileHandle'->new ("<$f"); <$fh> };
}

my %deps;

sub deps
{
  my $f = shift;

  my %sys = map { ($_, 1) } @sys;

  unless ($deps{$f})
    {
      my $code = &slurp ($f);
      my @inc = ($code =~ m/\n\s*#include\s+"(.*?)"/goms);
      my @use = map { uc ($_) } ($code =~ m/\n\s*use(?:\s*,\s*intrinsic)?\s+(\w+)/gomis);
      @use = grep { ! $sys{$_} } @use;
      my @mod = map { lc ($_) . '.o' } @use;
      $deps{$f} = [@mod, @inc];
   }

  return @{ $deps{$f} };
}

sub kind
{
  my $f = shift;
  my $code = &slurp ($f);
  my ($kind) = ($code =~ m/^\s*(PROGRAM|SUBROUTINE|MODULE)/igoms);
  $kind or die $f;
  return uc ($kind);
}

my $fh = 'FileHandle'->new (">Makefile");

$fh->print (<< "EOF");
include Makefile.inc

EOF

my (@obj, @prg);


for my $cpp (<*.cpp>)
  {
    (my $obj = $cpp) =~ s/\.cpp$/.o/o;
    $fh->print (<< "EOF");
$obj: $cpp 
	\$(CXX) -c $cpp

EOF

    push @obj, $obj,

  }

for my $C (<*.c>)
  {
    (my $obj = $C) =~ s/\.c$/.o/o;
    $fh->print (<< "EOF");
$obj: $C 
	\$(CC) -c $C

EOF

    push @obj, $obj,

  }



for my $F90 (<*.F90>)
  {
    my $kind = &kind ($F90);
    if ($kind eq 'SUBROUTINE')
      {
        my $intfb = &Fxtran::intfb ($F90);
        (my $ok = $intfb) =~ s/\.h$/.ok/o;
        my @deps = &deps ($intfb);
        $fh->print (<< "EOF");
$ok: $intfb @deps
	touch $ok

EOF
      }
    elsif ($kind eq 'PROGRAM')
      {
        push @prg, $F90;
      }
  }

for my $h (grep { ! m/\.intfb\.h$/o } <*.h>)
  {
    (my $ok = $h) =~ s/\.h$/.ok/o;
    $fh->print (<< "EOF");
$ok: $h
	touch $ok
EOF
  }

my @subobj;

for my $F90 (<*.F90>)
  {
    my @deps = &deps ($F90);
    for (@deps)
      {
        s/\.h$/.ok/o;
      }
    (my $obj = $F90) =~ s/\.F90$/.o/o;
    $fh->print (<< "EOF");
$obj: $F90 @deps
	\$(FC) -c $F90

EOF
    my $kind = &kind ($F90);
    
    push @obj, $obj if ($kind ne 'PROGRAM');

    push @subobj, $obj if ($kind ne 'MODULE');
  }

for my $prg (@prg)
  {
    (my $exe = $prg) =~ s/\.F90$/.x/o;
    (my $obj = $prg) =~ s/\.F90$/.o/o;
    $fh->print (<< "EOF");
$exe: $obj @obj
	\$(FC) -o $exe $obj @obj \$(LIBS)

EOF
  }

$fh->print (<< "EOF");

subclean:
	\\rm -f @subobj

clean:
	\\rm -f *.o *.xml *.a *.x *.mod *.optrpt

tidy:
	\\rm -f *.xml *.optrpt
EOF

