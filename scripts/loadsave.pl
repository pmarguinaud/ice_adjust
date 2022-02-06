#!/home/gmap/mrpm/marguina/install/perl-5.32.0/bin/perl -w
#
use strict;
use FindBin qw ($Bin);
use Data::Dumper;
use lib $Bin;
use Fxtran;
use Getopt::Long;
use File::Path;

my %opts = ('dir' => 'src/local/ifsaux/module', skip => [], types => 0, vars => 0);

sub process_decl
{
  my ($en_decl, $sname, $prefix, $BODY_SAVE, $BODY_LOAD, $U, $J, $L, $B, $Z) = @_;

  my (@BODY_SAVE, @BODY_LOAD);
  my (%U, %J, %L, %B, %Z);

  my $stmt = &Fxtran::stmt ($en_decl);
  my %attr = map { ($_, 1) } &f ('.//f:attribute/f:attribute-N/text ()', $stmt);

  return if ($attr{PARAMETER});

  return if (&F ('./_T-spec_/procedure-T-spec', $stmt));
  
  my ($name) = &f ('.//f:EN-N/f:N/f:n/text ()', $en_decl, 1);
  
  my $skip = grep { "$sname$name" eq $_ } @{ $opts{skip} };

  if ($skip)
    {
      if ($attr{POINTER})
        {
          push @BODY_LOAD, "NULLIFY ($prefix$name)";
        }
      goto RETURN;
    }
  
  
  my ($tspec) = &f ('./f:_T-spec_', $stmt);
  my ($intrinsic) = &f ('./f:intrinsic-T-spec', $tspec);
  my ($tname) = &f ('./f:derived-T-spec/f:T-N/f:N/f:n/text ()', $tspec);
  my ($real) = &F ('./intrinsic-T-spec[string(T-N)="REAL"]', $tspec);
  
  $tname && ($U{$tname} = 1);
  
  my @ss = &f ('./f:array-spec/f:shape-spec-LT/f:shape-spec', $en_decl, 1);
  
  my $r = scalar (@ss);
  
  if ($attr{POINTER} || $attr{ALLOCATABLE})
    {
      my $func = $attr{POINTER} ? 'ASSOCIATED' : 'ALLOCATED';
      push @BODY_SAVE, "L$name = $func ($prefix$name)\n";
      push @BODY_SAVE, "WRITE (KLUN) L$name\n";
      push @BODY_LOAD, "READ (KLUN) L$name\n";
      $L{$name} = 1;
      push @BODY_SAVE, "IF (L$name) THEN\n";
      push @BODY_LOAD, "IF (L$name) THEN\n";
      if (@ss)
        {
          push @BODY_SAVE, "WRITE (KLUN) LBOUND ($prefix$name)\n";
          push @BODY_SAVE, "WRITE (KLUN) UBOUND ($prefix$name)\n";
          $B{$r} = 1;
          push @BODY_LOAD, "READ (KLUN) IL" . scalar (@ss) . "\n";
          push @BODY_LOAD, "READ (KLUN) IU" . scalar (@ss) . "\n";
          push @BODY_LOAD, "ALLOCATE ($prefix$name (" . join (', ', map { "IL$r($_):IU$r($_)" } (1 .. $#ss+1) ) . "))\n";
        }
      else
        {
          push @BODY_LOAD, "ALLOCATE ($prefix$name)\n";
        }
    }
  
  if ($intrinsic)
    {
      push @BODY_SAVE, "WRITE (KLUN) $prefix$name\n";
      if ($real)
        {
          if ($r)
            {
              push @BODY_LOAD, "ALLOCATE (ZTEMP$r (" . join (', ', map { "LBOUND ($prefix$name, $_):UBOUND ($prefix$name, $_)" } (1 .. $r) ) . "))\n";
            }
          push @BODY_LOAD, "READ (KLUN) ZTEMP$r\n";
          push @BODY_LOAD, "$prefix$name = ZTEMP$r\n";
          push @BODY_LOAD, "DEALLOCATE (ZTEMP$r)\n" if ($r);
          $Z{$r} = 1;
        }
      else
        {
          push @BODY_LOAD, "READ (KLUN) $prefix$name\n";
        }
    }
  else 
    {
      
      for (my $i = $#ss+1; $i >= 1; $i--)
        {
          $J{"J$i"} = 1;
          my $do = "DO J$i = LBOUND ($prefix$name, $i), UBOUND ($prefix$name, $i)\n";
          push @BODY_SAVE, $do;
          push @BODY_LOAD, $do;
        }
      push @BODY_SAVE, ('  ' x scalar (@ss)) 
                   . "CALL SAVE (KLUN, $prefix$name" . (@ss ? " (" . join (', ', map { "J$_"  } (1 .. $#ss+1)) . ")" : '' ) . ")\n";
      push @BODY_LOAD, ('  ' x scalar (@ss)) 
                   . "CALL LOAD (KLUN, $prefix$name" . (@ss ? " (" . join (', ', map { "J$_"  } (1 .. $#ss+1)) . ")" : '' ) . ")\n";
      for (my $i = $#ss; $i >= 0; $i--)
        {
          push @BODY_SAVE, "ENDDO\n";
          push @BODY_LOAD, "ENDDO\n";
        }
    }
  
  if ($attr{POINTER} || $attr{ALLOCATABLE})
    {
      push @BODY_SAVE, "ENDIF\n";
      if ($attr{POINTER})
        {
          push @BODY_LOAD, "ELSE\n", "NULLIFY ($prefix$name)\n";
        }
      push @BODY_LOAD, "ENDIF\n";
    }

RETURN:
  
  push @$BODY_SAVE, @BODY_SAVE;
  push @$BODY_LOAD, @BODY_LOAD;

  %$U = (%$U, %U); %$J = (%$J, %J); 
  %$L = (%$L, %L); %$B = (%$B, %B); 
  %$Z = (%$Z, %Z);

}

sub indent
{
  my $n = 0;
  for (@_)
    {
      chomp;
      s/^\s*//o;
      $n-- if (m/^\s*(?:ELSE|ENDIF|ENDDO)\b/o);
      $_ = ('  ' x $n) . $_;
      $n++ if (m/^\s*(?:ELSE|IF|DO)\b/o);
    }
}

sub r
{
  my $f = shift;
  return '' unless (-f $f);
  return do { local $/ = undef; my $fh = 'FileHandle'->new ("<$f"); <$fh> };
}

sub w
{
  my $f = shift;
  my $t = &r ($f);
  return if ($t eq $_[0]);
  'FileHandle'->new (">$f")->print ($_[0]);
}

sub process_types
{
  my $doc = shift;

  my ($mod) = &f ('.//f:module-stmt/f:module-N/f:N/f:n/text ()', $doc);
  
  my @tconst = &f ('.//f:T-construct', $doc);
  
  for my $tconst (@tconst)
    {
      my ($INTERFACE_SAVE, $CONTAINS_SAVE) = ('', '');
      my ($INTERFACE_LOAD, $CONTAINS_LOAD) = ('', '');
  
      my ($name) = &f ('.//f:T-stmt/f:T-N/f:N/f:n/text ()', $tconst, 1);
      my $tname = $name;
  
  
      $INTERFACE_SAVE .= "MODULE PROCEDURE SAVE_$name\n";
      $INTERFACE_LOAD .= "MODULE PROCEDURE LOAD_$name\n";
  
      my (@BODY_SAVE, @BODY_LOAD);
      my (%U, %J, %L, %B, %Z);
  
      my @en_decl = &f ('.//f:EN-decl', $tconst);
      for my $en_decl (@en_decl)
        {
          &process_decl ($en_decl, "$tname%", 'YD%', \@BODY_SAVE, \@BODY_LOAD, \%U, \%J, \%L, \%B, \%Z);
        }
  
      my $DECL_SAVE = '';
      my $DECL_LOAD = '';
  
      for my $r (sort keys (%Z))
        {
          if ($r)
            {
              $DECL_LOAD .= "REAL(KIND=JPRD), ALLOCATABLE :: ZTEMP$r (" . join (', ', (':') x $r) . ")\n";
            }
          else
            {
              $DECL_LOAD .= "REAL(KIND=JPRD) :: ZTEMP$r\n";
            }
        }
      if (%J)
        {
          $DECL_SAVE .= "INTEGER :: " . join (', ', sort keys (%J)) . "\n";
          $DECL_LOAD .= "INTEGER :: " . join (', ', sort keys (%J)) . "\n";
        }
      if (%B)
        {
          $DECL_LOAD .= "INTEGER :: " . join (', ', map  { ("IL$_($_)", "IU$_($_)") } sort keys (%B)) . "\n";
        }
      if (%L)
        {
          $DECL_SAVE .= "LOGICAL :: " . join (', ', map { "L$_" } sort keys (%L)) . "\n";
          $DECL_LOAD .= "LOGICAL :: " . join (', ', map { "L$_" } sort keys (%L)) . "\n";
        }
  
      my $USE_SAVE = join ('', map { "USE SAVE_${_}_MOD\n" } grep { $_ ne $name } sort keys (%U));
      my $USE_LOAD = join ('', map { "USE LOAD_${_}_MOD\n" } grep { $_ ne $name } sort keys (%U));
  
      for ($USE_SAVE, $USE_SAVE, $DECL_SAVE, $DECL_LOAD)
        {
          chomp ($_);
        }
  
      $CONTAINS_SAVE .= << "EOF";
SUBROUTINE SAVE_$name (KLUN, YD)
$USE_SAVE
IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE ($name), INTENT (IN) :: YD
EOF

      $CONTAINS_LOAD .= << "EOF";
SUBROUTINE LOAD_$name (KLUN, YD)
$USE_LOAD
IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE ($name), INTENT (OUT) :: YD
EOF

      &indent (@BODY_SAVE);
      &indent (@BODY_LOAD);


      $CONTAINS_SAVE .= $DECL_SAVE . "\n" . join ("\n", @BODY_SAVE, '') . "END SUBROUTINE\n";
      $CONTAINS_LOAD .= $DECL_LOAD . "\n" . join ("\n", @BODY_LOAD, '') . "END SUBROUTINE\n";

      for ($CONTAINS_SAVE, $CONTAINS_SAVE, $INTERFACE_SAVE, $INTERFACE_LOAD)
        {
          chomp ($_);
        }
  
      my $n = lc ($name);

      &w ("$opts{dir}/save_${n}_mod.F90", << "EOF");
MODULE SAVE_${name}_MOD

USE $mod, ONLY : $name

INTERFACE SAVE
$INTERFACE_SAVE
END INTERFACE

CONTAINS

$CONTAINS_SAVE

END MODULE
EOF

      &w ("$opts{dir}/load_${n}_mod.F90", << "EOF");
MODULE LOAD_${name}_MOD

USE $mod, ONLY : $name
USE PARKIND1, ONLY : JPRB, JPRD

INTERFACE LOAD
$INTERFACE_LOAD
END INTERFACE

CONTAINS

$CONTAINS_LOAD

END MODULE
EOF


    }
}

sub process_vars
{
  my $doc = shift;

  my ($mod) = &f ('.//f:module-stmt/f:module-N/f:N/f:n/text ()', $doc);
  
  my $stmt = &Fxtran::stmt ($mod);
  my $pu = $stmt->parentNode ();
  
  my @en_decl = &f ('./f:T-decl-stmt//f:EN-decl', $pu);
  
  my (@BODY_SAVE, @BODY_LOAD);
  my (%U, %J, %L, %B, %Z);
  
  for my $en_decl (@en_decl)
    {
      &process_decl ($en_decl, '', '', \@BODY_SAVE, \@BODY_LOAD, \%U, \%J, \%L, \%B, \%Z);
    }

  my ($CONTAINS_SAVE) = ('');
  my ($CONTAINS_LOAD) = ('');
  
  my $DECL_SAVE = '';
  my $DECL_LOAD = '';
  
  for my $r (sort keys (%Z))
    {
      if ($r)
        {
          $DECL_LOAD .= "REAL(KIND=JPRD), ALLOCATABLE :: ZTEMP$r (" . join (', ', (':') x $r) . ")\n";
        }
      else
        {
          $DECL_LOAD .= "REAL(KIND=JPRD) :: ZTEMP$r\n";
        }
    }
  if (%J)
    {
      $DECL_SAVE .= "INTEGER :: " . join (', ', sort keys (%J)) . "\n";
      $DECL_LOAD .= "INTEGER :: " . join (', ', sort keys (%J)) . "\n";
    }
  if (%B)
    {
      $DECL_LOAD .= "INTEGER :: " . join (', ', map  { ("IL$_($_)", "IU$_($_)") } sort keys (%B)) . "\n";
    }
  if (%L)
    {
      $DECL_SAVE .= "LOGICAL :: " . join (', ', map { "L$_" } sort keys (%L)) . "\n";
      $DECL_LOAD .= "LOGICAL :: " . join (', ', map { "L$_" } sort keys (%L)) . "\n";
    }
  
  my $USE_SAVE = join ('', map { "USE SAVE_${_}_MOD\n" } grep { $_ ne $mod } sort keys (%U));
  my $USE_LOAD = join ('', map { "USE LOAD_${_}_MOD\n" } grep { $_ ne $mod } sort keys (%U));
  
  for ($USE_SAVE, $USE_SAVE, $DECL_SAVE, $DECL_LOAD)
    {
      chomp ($_);
    }
  
  $CONTAINS_SAVE .= << "EOF";
SUBROUTINE SAVE_$mod (KLUN)
$USE_SAVE
IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
EOF

      $CONTAINS_LOAD .= << "EOF";
SUBROUTINE LOAD_$mod (KLUN)
$USE_LOAD
IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
EOF

  &indent (@BODY_SAVE);
  &indent (@BODY_LOAD);

  $CONTAINS_SAVE .= $DECL_SAVE . "\n" . join ("\n", @BODY_SAVE, '') . "END SUBROUTINE\n";
  $CONTAINS_LOAD .= $DECL_LOAD . "\n" . join ("\n", @BODY_LOAD, '') . "END SUBROUTINE\n";

  for ($CONTAINS_SAVE, $CONTAINS_SAVE)
    {
      chomp ($_);
    }
  
  my $n = lc ($mod);

  &w ("$opts{dir}/save_${n}_mod.F90", << "EOF");
MODULE SAVE_${mod}_MOD

USE $mod

CONTAINS

$CONTAINS_SAVE

END MODULE
EOF

  &w ("$opts{dir}/load_${n}_mod.F90", << "EOF");
MODULE LOAD_${mod}_MOD
USE PARKIND1, ONLY : JPRB, JPRD

USE $mod

CONTAINS

$CONTAINS_LOAD

END MODULE
EOF

}

&GetOptions
(
  'skip=s@' => \$opts{skip}, types => \$opts{types}, vars => \$opts{vars}, 'dir=s' => \$opts{dir},
);

( -d $opts{dir}) or &mkpath ($opts{dir});

my $F90 = shift;

my $doc = &Fxtran::fxtran (location => $F90);

if ($opts{types})
  {
    &process_types ($doc);
  }

if ($opts{vars})
  {
    &process_vars ($doc);
  }

