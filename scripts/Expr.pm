package Expr;
#
use strict;

use Fxtran;

sub replacePowByMultiplyOrExp
{
  my $d = shift;
  
  my @expr = &F ('.//op-E[string(./op)="**"]', $d);
  
  for my $expr (@expr) 
    {
      my ($x, $a) = &F ('./ANY-E', $expr);
  
      my $Expr;
  
      if (($a->nodeName eq 'literal-E') && ($a->textContent =~ m/^(\d+)$/goms))
        {
          $a = $1;
          $Expr = &n ('<op-E/>');
          for my $i (1 .. $a)
            {
              $Expr->appendChild ($x->cloneNode (1));
              $Expr->appendChild (&n ('<op><o>*</o></op>')) if ($i < $a);
            }
        }
      else
        {
          $Expr = &Fxtran::fxtran (expr => 'EXP(A*LOG(X))');
          my ($X) = &F ('.//named-E[string(N)="X"]', $Expr);
          my ($A) = &F ('.//named-E[string(N)="A"]', $Expr);
          $X->replaceNode ($x->cloneNode (1));
          $A->replaceNode ($a->cloneNode (1));
        }

      $expr->replaceNode ($Expr);
    }
}

1;
