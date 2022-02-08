#!/home/gmap/mrpm/marguina/install/perl-5.32.0/bin/perl -w
#

use FileHandle;
use Data::Dumper;
use FindBin qw ($Bin);
use lib $Bin;
use Fxtran;

use strict;

my @sys = qw (OMP_LIB CUDAFOR ISO_C_BINDING);


my %slurp;
sub slurp
{
  my $f = shift;
  $slurp{$f} ||= do { local $/ = undef; my $fh = 'FileHandle'->new ("<$f"); <$fh> };
  $slurp{$f};
}

my %deps;
my %mod2obj;

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
      my @mod = map { $mod2obj{$_} or die $_; } @use;
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

sub name
{
  my $f = shift;
  my $code = &slurp ($f);
  my ($name) = ($code =~ m/^\s*(?:PROGRAM|SUBROUTINE|MODULE)\s+(\w+)/igoms);
  $name or die $f;
  return uc ($name);
}

my $fh = 'FileHandle'->new (">Makefile");

$fh->print (<< "EOF");
include Makefile.inc

EOF

for my $F90 (<*.F90>)
  {
    my $kind = &kind ($F90);
    next unless ($kind eq 'SUBROUTINE');
    my $name = &name ($F90);

    my $doc = &Fxtran::intf ($F90);

    my @inc = &F ('.//include-stmt', $doc);
    $_->unbindNode for (@inc);

    my $intf = $doc->textContent;
    $intf =~ s/\s+\n$//o;

    my $fh = 'FileHandle'->new ('>modi_' . lc ($name) . '.F90');
    $fh->print (<< "EOF");
MODULE MODI_$name

INTERFACE
$intf
END INTERFACE

END MODULE
EOF
  }


my (@obj, @prg);

for my $F90 (<*.F90>)
  {
    my $kind = &kind ($F90);
    next unless ($kind eq 'MODULE');
    my $name = &name ($F90);
    ($mod2obj{$name} = $F90) =~ s/\.F90$/.o/o;
  }


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
    if ($kind eq 'PROGRAM')
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

