package Loop;
#
use strict;
use FileHandle;
use Data::Dumper;

use FindBin qw ($Bin);
use lib $Bin;
use Fxtran;


sub removeJlonLoops
{
  my $d = shift;
  
  my ($noexec) = do  
  {
    my ($exec) = grep { &Fxtran::stmt_is_executable ($_) } &F ('.//ANY-stmt', $d);
    my @prev = &F ('preceding::*', $exec);
  
    my $prev;
    for my $p (reverse (@prev))
      {   
        next if ($p->nodeName eq '#text');
        next if ($p->nodeName eq 'C');
        $prev = $p; 
        last;
      }   
  
    my @anc = &F ('ancestor::*', $prev);
  
    for my $anc (reverse (@anc))
      {   
        if (($anc->nodeName =~ m/-(?:construct|stmt)$/o) || ($anc->nodeName eq 'include'))
          {
            $prev = $anc;
          }
      }   
  
    $prev
  };  
  
  
  $noexec->parentNode->insertAfter (&Fxtran::fxtran (statement => "JLON = KIDIA\n"), $noexec);
  $noexec->parentNode->insertAfter (&t ("\n"), $noexec);
  $noexec->parentNode->insertAfter (&t ("\n"), $noexec);
  
  
  my @do = &F ('.//do-construct[./do-stmt[string(do-V)="JLON"]]', $d);
  
  for my $do (@do)
    {
      $do->firstChild->unbindNode;
      $do->lastChild->unbindNode;
      my @nodes = &F ('./node()', $do);
      for (@nodes)
        {
          $do->parentNode->insertBefore ($_, $do);
        }
      $do->unbindNode ();
    }
  
}

1;
