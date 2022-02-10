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
  
  
  $noexec->parentNode->insertAfter (&t ("\n"), $noexec);
  for my $x (qw (I J))
    {
      my ($init) = &Fxtran::fxtran (statement => "J${x} = D%N${x}B\n");
      $noexec->parentNode->insertAfter ($init, $noexec);
      $noexec->parentNode->insertAfter (&t ("\n"), $noexec);
    }
  $noexec->parentNode->insertAfter (&t ("\n"), $noexec);
  
  for my $x (qw (I J))
    {
      my @do = &F ('.//do-construct[./do-stmt[string(do-V)="?"]]', "J$x", $d);
      
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
}

sub arraySyntaxLoop
{
  my $d = shift;

  # Collect dimensions

  my @en_decl = &F ('.//EN-decl[./array-spec/shape-spec-LT[string(shape-spec)="D%NIT"]]', $d);

  my @N;

  my %as;

  for my $en_decl (@en_decl)
    {
      my ($N) = &F ('./EN-N', $en_decl, 1);
      push @N, $N;
      my @ss = &F ('./array-spec/shape-spec-LT/shape-spec', $en_decl);
      $as{$N} = \@ss;
    }

  my %N = map { ($_, 1) } @N;

  # Transform simple array syntax in loops
  
  my @stmt = &F ('.//a-stmt[./E-1/named-E[not(R-LT)]]|.//a-stmt[./E-1/named-E/R-LT/array-R]', $d);

  for my $stmt (@stmt)
    {
      my ($N1) = &F ('./E-1/named-E/N', $stmt, 1);
      next unless ($N{$N1});

      my @ind = ('JI', 'JJ', 'JK');
      my %ind;
      my %dim2ind = ('D%NIT' => 'JI', 'D%NJT' => 'JJ', 'D%NKT' => 'JK');
      my %ind2bnd = ('JI' => ['D%NIB', 'D%NIE'], 'JJ' => ['D%NJB', 'D%NJE'], 'JK' => ['D%NKTB', 'D%NKTE']);

      # For all expressions in this statement, involving arrays whose first dimension if NIT, replace subscript sections by indices

      my @expr = &F ('.//named-E', $stmt);
      for my $expr (@expr)
        {
          my ($N) = &F ('./N', $expr, 1);
          next unless ($N{$N});
          my @ss1 = @{ $as{$N} };

          my ($r) = &F ('././R-LT/array-R', $expr);

          # The array reference does not exist; add it
          
          unless ($r)
            {
              my $rlt = &n ('<R-LT/>');
              $expr->appendChild ($rlt);
              $r = &n ('<array-R/>');
              $rlt->appendChild ($r);
              my $sslt = &n ('<section-subscript-LT/>');
              $r->appendChild (&t ('('));
              $r->appendChild ($sslt);
              $r->appendChild (&t (')'));
              for my $i (0 .. $#ss1)
                {
                  $sslt->appendChild (&n ('<section-subscript>:</section-subscript>'));
                  $sslt->appendChild (&t (', ')) if ($i != $#ss1);
                }
            }

          $r->setNodeName ('parens-R');
          my ($sslt) = &F ('./section-subscript-LT', $r);
          $sslt->setNodeName ('element-LT');

          my @ss2 = &F ('./section-subscript', $sslt);

          die &Dumper ([\@ss1, \@ss2]) unless (scalar (@ss1) == scalar (@ss2));

          for my $i (0 .. $#ss1)
            {
              $ss2[$i]->setNodeName ('element');
              if ($ss2[$i]->textContent eq ':')
                {
                  my $ind = $dim2ind{$ss1[$i]->textContent};
                  die unless ($ind);
                  $ind{$ind}++;
                  $ss2[$i]->firstChild->replaceNode (&n ("<named-E><N><n>$ind</n></N></named-E>"));
                }
              elsif ($ss2[$i]->textContent =~ m/:/o)
                {
                  die;
                }
            }
          

        }

      @ind = grep { $ind{$_} } @ind;

      my $indent = &Fxtran::getIndent ($stmt);

      # Generate the loop and inserts the assignment statement 

      my $loop = join ('', 
                        map ({ my $i = $_; my $ind = $ind[$#ind-$i]; ('  ' x $i) . "DO $ind = $ind2bnd{$ind}[0], $ind2bnd{$ind}[1]\n" } (0 .. $#ind)),
                        ('  ' x scalar (@ind)) . "!\n",
                        reverse (map ({ my $i = $_; ('  ' x $i) . "ENDDO\n" } (0 .. $#ind)))
                      );

      ($loop) = &Fxtran::fxtran (fragment => $loop);

      &Fxtran::reIndent ($loop, $indent);

      my ($C) = &F ('.//C', $loop);
      $C->replaceNode ($stmt->cloneNode (1));

      $stmt->replaceNode ($loop);

    }

}

1;
