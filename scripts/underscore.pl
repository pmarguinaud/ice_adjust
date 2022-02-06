#!/home/ms/fr/sor/install/perl-5.32.1/bin/perl -w
#
use strict;
use FileHandle;
use Data::Dumper;

use FindBin qw ($Bin);
use lib $Bin;
use Fxtran;

my ($f1, @n2) = @ARGV;

my $d1 = &Fxtran::fxtran (location => $f1);

for my $n2 (@n2)
  {
    my ($d2) = &f ('.//f:program-unit[./f:subroutine-stmt[./f:subroutine-N/f:N/f:n/text ()="?"]]', $n2, $d1);
    
    # Suffix local variables with a _
    
    my @da = map { $_->textContent } 
      &f ('descendant-or-self::f:subroutine-stmt/f:dummy-arg-LT/f:arg-N/f:N/f:n/text ()', $d2);
    
    my @N = &f ('.//f:EN-decl/f:EN-N/f:N/f:n/text ()', $d2);
    
    for my $N (@N)
      {
        my $n = $N->textContent;
        next if (grep { $_ eq $N } @da);
        my @e = &f ('.//f:named-E/f:N/f:n/text()[.="?"]', $N, $d2);
        for my $e (@e)
          {
            $e->replaceNode (&t ("${n}_"));
          }
        $N->replaceNode (&t ("${n}_"));
      }
  }


'FileHandle'->new (">$f1.new")->print ($d1->textContent);


