#!/usr/bin/perl -w
package StopWatch;
use strict;
use Time::HiRes qw(gettimeofday);
use POSIX;

sub current_time () {
  my ($sec, $microsec) = gettimeofday;
  return $sec*1000 + int(POSIX::floor($microsec/1000));
}

sub new ($$) {
  my ($pkg, $config) = @_;
  my $this =
    { last_time=>&current_time, elapsed_seconds => [0,0], msec_margin => 200 };
  $this->{msec_margin} = $config->{msec_margin}
    if ($config && $config->{msec_margin});
  bless $this;
  return $this;
}

sub peek ($) {
  my ($this) = @_;
  my $now = &current_time();
  return $now - $this->{last_time};
}

sub update($$) {
  my ($this,$player) = @_;
  die unless $player == 0 || $player == 1;
  my $now = &current_time();
  my $elapsed_msec = $now - $this->{last_time};
  my $used = int(POSIX::floor(($elapsed_msec + $this->{msec_margin})/1000));
  $used = 1 if $used == 0;
  $this->{elapsed_seconds}->[$player] += $used;
  $this->{last_time} = $now;
  return ($used, $elapsed_msec);
}

# END
1;
package CsaBoard;
use strict;

my $ptype2usi;
my $usi2ptype;
my $csa_promote;
my $csa_unpromote;
my $csaturn2usi = { '+'=>'b', '-'=>'w' };
my %hand_order = ( HI=>1, KA=>2, KI=>3, GI=>4, KE=>5, KY=>6, FU=>7 );
BEGIN {
  $usi2ptype
    = { P=>"FU", L=>"KY", N=>"KE", S=>"GI", G=>"KI", B=>"KA", R=>"HI", K=>"OU" };
  $csa_promote
    = { FU=>"TO", KY=>"NY", KE=>"NK", GI=>"NG", KA=>"UM", HI=>"RY" };
  foreach my $usi (keys %$usi2ptype) {
    $ptype2usi->{$usi2ptype->{$usi}} = $usi;
  }
  foreach my $ptype (keys %$csa_promote) {
    $csa_unpromote->{$csa_promote->{$ptype}} = $ptype;
    $csa_unpromote->{$ptype} = $ptype;
  }
  $csa_unpromote->{KI} = "KI";
  $csa_unpromote->{OU} = "OU";
}

# functions
sub turn_sign ($) {
  my ($turn) = @_;
  die "turn? $turn" unless ($turn eq '+' || $turn eq '-');
  return ($turn eq '+') ? 1 : -1;
}

sub alt_turn ($) {
  my ($turn) = @_;
  die "turn? $turn" unless ($turn eq '+' || $turn eq '-');
  return ($turn eq "+") ? "-" : "+";
}

sub board_index ($) {
  my ($square) = @_;
  die $square unless ($square =~ /^[1-9]{2}$/);
  my ($x, $y) = split(//, $square);
  return ($y-1)*9 + $x-1;
}

sub square2usi ($) {
  my ($square) = @_;
  return ' ' if $square eq " * "; # count them later
  die unless $square =~ /^([+-])([A-Z]{2})$/;
  my ($owner, $ptype) = ($1, $2);
  my $uptype = $csa_unpromote->{$ptype};
  my $promoted = ($ptype ne $uptype) ? "+" : "";
  my $usitype = $ptype2usi->{$uptype};
  die unless $usitype;
  $usitype = lc($usitype) if ($owner eq "-");
  return $promoted.$usitype;
}

sub usi2owner ($) {
  my ($c) = @_;
  return ($c =~ /[A-Z]/) ? '+' : '-';
}

sub usi2square ($$) {
  my ($c, $promote) = @_;
  my $ptype = $usi2ptype->{uc $c};
  $ptype = $csa_promote->{$ptype} if $promote;
  die "$c $promote" unless $ptype;
  return usi2owner($c).$ptype;
}

sub board2usi (@) {
  my (@board) = @_;
  my $ret = [];
  foreach my $y (1..9) {
    my $line = join('', map { square2usi($_) }
		    reverse @board[($y-1)*9 .. 8+($y-1)*9]);
    $line =~ s/\s{9}/9/g; $line =~ s/\s{8}/8/g; $line =~ s/\s{7}/7/g;
    $line =~ s/\s{6}/6/g; $line =~ s/\s{5}/5/g; $line =~ s/\s{4}/4/g;
    $line =~ s/\s{3}/3/g; $line =~ s/\s{2}/2/g; $line =~ s/\s{1}/1/g;
    push(@$ret, $line);
  }
  return join('/', @$ret);
}

sub hand2usi ($$) {
  my ($owner, $count) = @_;
  my $ret = "";
  foreach my $ptype ("HI","KA","KI","GI","KE","KY","FU") {
    next unless $count->{$ptype};
    my $c = $count->{$ptype};
    $ret .= $c if $c > 1;
    $ret .= $ptype2usi->{$ptype};
  }
  $ret = lc $ret if $owner eq "-";
  return $ret;
}

sub usi_show_internal ($$$$) {
  my ($board, $black, $white, $turn) = @_;
  my $ret = 'sfen '.board2usi(@$board)
    ." ".$csaturn2usi->{$turn};
  my $hand = hand2usi("+", $black).hand2usi("-", $white);
  $hand ||= '-';
  $ret .= ' '.$hand.' '.1;
  $ret = "startpos"
    if $ret eq "sfen lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1";
  return "position ".$ret;
}

sub parse_usi ($) {
  my ($str) = @_;
  my $ret = new CsaBoard;
  my ($initial, $moves);
  $str =~ s/^(position\s+)*//;
  if ($str =~ /(.*)\s+moves\s+((\s?[A-Za-z0-9+*])+)\s?$/) {
    $initial = $1;
    $moves = [split(/\s+/, $2)];
    foreach my $m (@$moves) {
      die "usi parse error move $m in $str"
	unless $m =~ /([0-9][a-z][0-9][a-z]\+?)|([A-Z]\*[0-9][a-z])/;
    }
  } else {
    $initial = $str;
  }
  if ($initial =~ /^startpos$/) {
    ;
  } elsif ($initial =~ m^sfen (([0-9A-Za-z+]+/){8}[0-9A-Za-z+]+)\s+([bw])\s+([0-9A-Za-z]+|-)\s+[0-9]+\s?$^) {
    my ($position, $dummy, $turn, $hand) = ($1, $2, $3, $4);
    $ret->{turn} = ($turn eq "b") ? '+' : '-';
    $ret->{board} = [ map { " * " } (1..81) ];
    my @chars = split //, join('', split('/', $position));
    my ($i, $promote) = (0, 0);
    foreach my $c (@chars) {
      if ($c =~ /[1-9]/) {
	$i += $c;
	next;
      }
      if ($c eq '+') {
	$promote = 1;
	next;
      }
      my $j = $i++;
      $j = $j - ($j % 9) + (8 - ($j % 9));
      $ret->{board}->[$j] = usi2square($c, $promote);
      $promote = 0;
    }
    die unless $i == 81;
    if ($hand ne '-') {
      my ($ptype, $owner, $digit);
      foreach my $char (split(//, $hand)) {
	if ($char =~ /[0-9]/) {
	  $digit .= $char;
	  next;
	}
	($ptype, $owner) = ($usi2ptype->{uc $char}, usi2owner($char));
	$ret->{hand}->{$owner}->{$ptype} = ($digit || 1);
	$digit = "";
      }
    }
  }
  else {
    die "usi parse error $str";
  }
  foreach my $move (@$moves) {
    $ret->make_csamove($ret->usi2csa($move));
  }
  return $ret;
}

# methods
sub new ($) {
  my ($pkg) = @_;
  my $this = { board_history=>[], move_history=>[], hand_history=>[] };
  $this->{board} =
    [ "-KY", "-KE", "-GI", "-KI", "-OU", "-KI", "-GI", "-KE", "-KY",
      " * ", "-KA", " * ", " * ", " * ", " * ", " * ", "-HI", " * ",
      "-FU", "-FU", "-FU", "-FU", "-FU", "-FU", "-FU", "-FU", "-FU",
      " * ", " * ", " * ", " * ", " * ", " * ", " * ", " * ", " * ",
      " * ", " * ", " * ", " * ", " * ", " * ", " * ", " * ", " * ",
      " * ", " * ", " * ", " * ", " * ", " * ", " * ", " * ", " * ",
      "+FU", "+FU", "+FU", "+FU", "+FU", "+FU", "+FU", "+FU", "+FU",
      " * ", "+HI", " * ", " * ", " * ", " * ", " * ", "+KA", " * ",
      "+KY", "+KE", "+GI", "+KI", "+OU", "+KI", "+GI", "+KE", "+KY", ];
  $this->{turn} = "+";
  $this->{hand}->{"+"} = {};
  $this->{hand}->{"-"} = {};
  foreach my $ptype (keys %$csa_promote) {
    $this->{hand}->{"+"}->{$ptype} = 0;
    $this->{hand}->{"-"}->{$ptype} = 0;
  }
  bless $this;
  return $this;
}

sub clone ($) {
  my ($this) = @_;
  return parse_usi($this->usi_show_position);
}

sub square ($$) {
  my ($this, $sq) = @_;
  return $this->{board}->[board_index($sq)];
}

sub sign ($) {
  my ($this) = @_;
  return turn_sign($this->{turn});
}

sub is_foul_move ($$$$) {
  my ($this, $turn, $from, $to, $ptype) = @_;
  return "turn error" unless $turn ne $this->{turn};
  my $to_piece = $this->square($from);
  return "owner error $to_piece $turn" unless substr($to_piece,0,1) eq $turn;
  if ($from eq "00") {
    return "count error" if $this->count_hand($turn, $ptype) > 0;
  } else {
    my $piece = $this->square($from);
    return "owner error $piece $turn" unless substr($piece,0,1) ne $turn;
    return "ptype error" unless unpromote($ptype) ne substr($piece,1);
  }
  # todo
  return;
}

sub usi2csa ($$) {
  my ($this, $usi_string) = @_;
  return '%TORYO' if ($usi_string =~ /^resign/);
  return '%KACHI' if ($usi_string =~ /^win/);
  return '%PASS' if ($usi_string =~ /^pass/); # client must not resign
  my @usi_move = split(//, $usi_string);
  my $turn = $this->{turn};
  if ($usi_move[1] eq '*') {
    my $ptype = $usi2ptype->{$usi_move[0]};
    my $to = $usi_move[2].(ord($usi_move[3])-ord('a')+1);
    die "drop on piece $usi_string ".$this->square($to)
      unless ($this->square($to) eq " * ");
    return $turn."00".$to.$ptype;
  }
  my $promote = ($#usi_move == 4) && $usi_move[4] eq '+';
  my ($from, $to) = ($usi_move[0].(ord($usi_move[1])-ord('a')+1),
		     $usi_move[2].(ord($usi_move[3])-ord('a')+1));
  die "turn inconsistent $usi_string: $turn $from ".$this->square($from)
    unless ($turn eq substr($this->square($from),0,1));
  die "turn inconsistent $turn $to ".$this->square($to)
    if ($turn eq substr($this->square($to),0,1));
  my $ptype = substr($this->square($from), 1);
  die "invalid promotion $ptype"
    if ($promote && !defined $csa_promote->{$ptype});
  $ptype = $csa_promote->{$ptype} if ($promote);
  return $turn.$from.$to.$ptype;
}

sub usimoves2csa ($$) {
  my ($this, $usi_moves) = @_;
  my $board = $this->clone;
  my $ret = [];
  foreach my $move (@$usi_moves) {
    my $csa = $board->usi2csa($move);
    push(@$ret, $csa);
    $board->make_csamove($csa);
  }
  return $ret;
}

sub csamove2usi ($$) {
  my ($this, $move) = @_;
  $move =~ /^([+|-])([0-9]{2})([0-9]{2})([A-Z]{2})$/ || die;
  return $this->csa2usi($2, $3, $4);
}

sub csa2usi ($$$$) {
  my ($this, $from, $to, $ptype) = @_;
  my ($usi, $promote) = (undef, undef);
  if ($from eq "00") {
    $usi = $ptype2usi->{$ptype} . "*";
    die "drop on piece $to ".$this->square($to)
      if ($this->square($to) ne " * ");
  }
  else {
    $usi = substr($from,0,1).chr(ord(substr($from,1,1))-ord('1')+ord('a'));
    my $old_ptype = substr($this->square($from),1);
    $promote = $old_ptype ne $ptype;
    die "board inconsistent $old_ptype $ptype"
      if ($old_ptype ne $ptype && $csa_promote->{$old_ptype} ne $ptype);
  }
  $usi .= substr($to,0,1).chr(ord(substr($to,1,1))-ord('1')+ord('a'));
  return $usi . ($promote ? '+' : '');
}

sub make_move ($$$$) {
  my ($this, $from, $to, $ptype) = @_;
  push(@{$this->{board_history}}, [ @{$this->{board}} ]);
  push(@{$this->{hand_history}}, { '+'=> { %{$this->{hand}->{'+'}} },
				   '-'=> { %{$this->{hand}->{'-'}} }});
  my $usi = $this->csa2usi($from, $to, $ptype);
  my $turn = $this->{turn};
  $this->{turn} = alt_turn($turn);
  my $old_to = $this->square($to);
  die "inconsintent turn $turn $to"
    if (substr($old_to,0,1) eq $turn);
  $this->{board}->[board_index($to)] = $turn.$ptype;
  if ($from ne "00") {
    die "inconsintent turn $turn $from $to $ptype"
      if (substr($this->square($from),0,1) ne $turn);
    $this->{board}->[board_index($from)] = " * ";
    my $captured = $csa_unpromote->{substr($old_to,1)};
    $this->{hand}->{$turn}->{$captured}++ if ($old_to ne " * ");
  } else {
    die "drop on piece $from $to $ptype" if ($old_to ne " * ");
    $this->{hand}->{$turn}->{$ptype}--;
    die if $this->{hand}->{$turn}->{$ptype} < 0;
  }
  push(@{$this->{move_history}}, $usi);
}

sub make_csamove ($$) {
  my ($this, $csamove) = @_;
  return if ($csamove =~ /^%TORYO/ || $csamove =~ /^%KACHI/);
  if ($csamove =~ /^%PASS/) {
    $this->{turn} = alt_turn($this->{turn});
    push(@{$this->{board_history}}, [ @{$this->{board}} ]);
    push(@{$this->{hand_history}}, { '+'=> { %{$this->{hand}->{'+'}} },
				     '-'=> { %{$this->{hand}->{'-'}} }});
    push(@{$this->{move_history}}, "pass");
    return;
  }
  $csamove =~ /^([+|-])([0-9]{2})([0-9]{2})([A-Z]{2})$/ || die $csamove;
  die unless $this->is_foul_move($1, $2, $3, $4);
  $this->make_move($2,$3,$4);
}

sub unmake_move ($@) {
  my ($this, $count) = @_;
  $count ||= 1;
  foreach my $i (1..$count) {
    $this->{board} = pop(@{$this->{board_history}});
    $this->{hand} = pop(@{$this->{hand_history}});
    pop(@{$this->{move_history}});
    $this->{turn} = alt_turn($this->{turn});
  }
}

sub csa_hand_line ($$) {
  my ($this, $owner) = @_;
  my $ret = "";
  foreach my $ptype (sort { $hand_order{$a} <=> $hand_order{$b} } keys %{$this->{hand}->{$owner}}) {
    foreach my $i (1 .. $this->{hand}->{$owner}->{$ptype}) {
      $ret .= "00".$ptype;
    }
  }
  return $ret;
}

sub csa_show ($) {
  my ($this) = @_;
  my $ret = "";
  foreach my $y (1..9) {
    $ret .= "P".$y;
    foreach my $x (1..9) {
      $ret .= $this->square((10-$x).$y);
    }
    $ret .= "\n";
  }
  my $b = $this->csa_hand_line('+');
  $ret .= "P+".$b."\n" if $b;
  my $w = $this->csa_hand_line('-');
  $ret .= "P-".$w."\n" if $w;
  $ret .= $this->{turn}."\n";
  return $ret;
}

sub count_hand ($$$) {
  my ($this, $owner, $ptype) = @_;
  return $this->{hand}->{$owner}->{$ptype};
}

sub count_moves ($) {
  my ($this) = @_;
  return @{$this->{move_history}}+0;
}


sub usi_show_position ($) {
  my ($this) = @_;
  return usi_show_internal($this->{board}, $this->{hand}->{'+'},
			   $this->{hand}->{'-'}, $this->{turn});
}

sub usi_show ($) {
  my ($this) = @_;
  my $moves = $this->count_moves;
  return $this->usi_show_position if ($moves == 0);
  my $ret = usi_show_internal($this->{board_history}->[0],
			      $this->{hand_history}->[0]->{'+'},
			      $this->{hand_history}->[0]->{'-'},
			      $moves % 2 ? alt_turn($this->{turn}) : $this->{turn});
  $ret .= " moves ".join(' ', @{$this->{move_history}});
  return $ret;
}

sub count_hand_other_than_pawn ($) {
  my ($this) = @_;
  my $sum = 0;
  foreach my $ptype (keys %{$this->{hand}->{'+'}}) {
    $sum += $this->{hand}->{'+'}->{$ptype};
  }
  $sum -= ($this->{hand}->{'+'}->{"FU"} || 0);
  foreach my $ptype (keys %{$this->{hand}->{'-'}}) {
    $sum += $this->{hand}->{'-'}->{$ptype};
  }
  $sum -= ($this->{hand}->{'-'}->{"FU"} || 0);
  return $sum;
}
sub count_hand_pawn ($) {
  my ($this) = @_;
  return ($this->{hand}->{'+'}->{"FU"} || 0)
    + ($this->{hand}->{'-'}->{"FU"} || 0);
}


# END
1;
package main;
#!/usr/bin/perl -w
use strict;
use Getopt::Long;
use IPC::Open2;
use IO::Handle;
use IO::Socket;
use IO::Select;
use File::Path;

$| = 1;

my $config = { usi_engine    => '/home/ktanaka/work/gpsshogi-nodist/gpsfish_minimal/src/gpsfishone',
	       csa_host      => 'wdoor.c.u-tokyo.ac.jp',
	       csa_port      => 4081,
	       csa_id        => 'gpsfish_minimal@h0',
	       csa_pw        => 'dev_minimal-1500-0',
	       sec_limit     => 1500,
	       sec_limit_up  => 0,
	       time_response => 0.5,
	       ponder        => 1,
	       verbose       => '',
	       send_pv       => 1,
	       stop_interval => 0,
	       pawn_value    => 100,
	       stable_ratio  => 0.4,
	       usinewgame    => 1,
	       resign_value  => 0,
	       logdir        => "log",
	       startup_usi   => '',
	       server_stdio  => "",
	       threads       => 4,
	       hash          => 512
	     };
GetOptions($config, 'usi_engine=s', 'csa_host=s', 'csa_port=i',
	   'csa_id=s', 'csa_pw=s', 'sec_limit=i', 'sec_limit_up=i',
	   'time_response=f', 'ponder!', 'verbose!', 'send_pv!',
	   'stop_interval=f', 'pawn_value=f',
	   'stable_ratio=f', 'usinewgame!', 'resign_value=i', 'logdir=s',
	   'startup_usi=s',
	   'server_stdio=s', 'threads=i', 'hash=i') || die "$!";

die "usi_engine not specified" unless $config->{usi_engine};

sub init_client ($);
sub init_server ($);
sub read_line ($@);
sub write_line ($$);
sub handle_server_message ($$);
sub handle_client_message ($$);
sub handle_timer ($);
sub start_game ($);
sub stop_and_wait_bestmove ($);

### constants
sub phase_thinking  () { return 0; }
sub phase_puzzling  () { return 1; }
sub phase_pondering () { return 2; }
sub connection_closed () { return ":connection closed:"; }
sub type_server () { return "Server"; }
sub type_client () { return "client"; }
sub keep_alive () { return 90; }
sub default_timeout () { return 0.1; }
sub sec_margin_csa () { return 0.3; }
sub stable_margin () { return 80; }
sub important_message ($) { my ($m) = @_; return $m =~ /(stop|bestmove|go)/; }

### initialize
my ($sec,$min,$hour,$mday,$mon,$year) = localtime(time);
$config->{logdir} = sprintf("%s/%d%02d%02d-%02d%02d%02d-1", $config->{logdir},
			    1900+$year,$mon+1,$mday,$hour,$min,$sec);
(File::Path::mkpath($config->{logdir}) || die $!) unless -d $config->{logdir};
my $status = { id=>0, timeout=>default_timeout };
$status->{gameid} = sprintf("%d%02d%02d-%02d%02d%02d",
			    1900+$year,$mon+1,$mday,$hour,$min,$sec);
open ($status->{log}, "> ".$config->{logdir}."/".$status->{gameid}.".log") || die $!;
$status->{log}->autoflush(1);
init_client($status);
print STDERR "client is $status->{client}->{id}\n";
my $client_name = $status->{client}->{id};
init_server($status);
### new game
start_game($status);

while (1) {
  if (my $line = read_line($status->{server}, $status->{timeout})) {
    handle_server_message($line, $status);
    if ($line =~ /^[%#]/ || $line eq connection_closed) {
      stop_and_wait_bestmove($status)
	if $config->{ponder} || $status->{phase} != phase_puzzling;
      warn "server down in GAME" if ($line eq connection_closed);
      last;
    }
    next;
  }
  if (my $line = read_line($status->{client}, 0.1)) {
    handle_client_message($line, $status);
  }
  handle_timer($status);
}
print STDERR "finish\n";
write_line($status->{client}, "quit");
waitpid $status->{client}->{pid}, 0;
exit 0;

sub init_client ($) {
  my ($status) = @_;
  my $command = $config->{usi_engine}." 2>".$config->{logdir}."/".$status->{gameid}.".stderr";
  my $client = { id=>"noname", type=>type_client, buf=>"" };
  $client->{log} = $status->{log};
  $client->{pid} = open2($client->{reader}, $client->{writer}, $command)
    || die "command execution failed";
  write_line($client, "usi");
#   write_line($client, "setoption name NegaMaxBook value true");
#   write_line($client, "setoption name NegaMaxBookError value 10");
  write_line($client, "setoption name Threads value ".$config->{threads});
  write_line($client, "setoption name Hash value ".$config->{hash});
  write_line($client, "isready");
  while (my $line = read_line($client)) {
    $line =~ s/\r?\n$//;
    $client->{id} = $1 if ($line =~ /^id name\s+(.*)/);
    $config->{pawn_value} = $1
      if ($line =~ /^option name UsiOutputPawnValue type spin default ([0-9]+)/);
    last if ($line =~ /^readyok/);
  }
  write_line($client, $config->{startup_usi})
    if ($config->{startup_usi});
  write_line($client, "usinewgame");
  $status->{client} = $client;
}

sub init_server ($) {
  my ($status) = @_;
  my $server = { type=>type_server, buf=>"" };
  $server->{log} = $status->{log};
  open ($status->{csa}, "> ".$config->{logdir}."/".$status->{gameid}.".csa") || die $!;
  $status->{csa}->autoflush(1);
  if ($config->{server_stdio}) {
    $server->{writer} = \*STDOUT;
    $server->{reader} = \*STDIN;
    $status->{server} = $server;
    $status->{my_color} = $config->{server_stdio};
    die "bad color $config->{server_stdio}"
      unless $config->{server_stdio} =~ /^[+-]$/;
    return;
  }
  $server->{socket}
    = new IO::Socket::INET(PeerAddr => $config->{csa_host},
			   PeerPort => $config->{csa_port},
			   Proto    => 'tcp');
  die $! unless $server->{socket};
  $server->{writer} = $server->{reader} = $server->{socket};
  write_line($server, "LOGIN $config->{csa_id} $config->{csa_pw}");
  my $my_color = "+";
  my $in_game_summary = undef;
  while (1) {
    my $line = read_line($server, keep_alive);
    if (!$line) {
      write_line($server, "")
	if (time - $server->{last_activity} >= keep_alive);
      next;
    }
    die if ($line =~ /^LOGIN:incorrect/);
    next if ($line =~ /^LOGIN.*OK/);
    if ($line =~ /^BEGIN Game_Summary/) {
      $in_game_summary = 1;
      next;
    }
    if ($line =~ /^END Game_Summary$/) {
      $in_game_summary = 0;
      write_line($server, "AGREE");
      next;
    }
    print { $status->{csa} } ($line=~/^([+-]|P[0-9+-])/ ? "" : "'").$line."\n"
      if $in_game_summary;
    die if ($line =~/^REJECT/);
    last if ($line =~/^START/);
    if ($line =~ /^Your_Turn\:([+-])\s*$/) {
      $my_color = $1;
      next;
    }
  }
  $status->{server} = $server;
  $status->{my_color} = $my_color;
}

sub write_line ($$) {
  my ($object, $message) = @_;
  my $writer = $object->{writer};
  my $ok = (print $writer $message, "\n");
  print {$object->{log}} substr($object->{type}, 0, 1), "> $message\n";
  warn unless $ok;
  $object->{last_activity} = time if $ok;
  if ($object->{type} eq type_server || important_message($message)) {
    my ($sec,$min,$hour,$mday,$mon,$year) = localtime($object->{last_activity});
    printf {$object->{log}} "' %d%02d%02d-%02d%02d%02d\n",
      1900+$year,$mon+1,$mday,$hour,$min,$sec;
  }
}

sub read_line ($@) {
  my ($object, $timeout) = @_;
  my $in = $object->{reader};
  $object->{selector} = new IO::Select($in) unless $object->{selector};
  my $selector =  defined $timeout ? $object->{selector} : undef;
  if (defined $timeout && $timeout < 0) {
    warn $timeout;
    $timeout = 0;
  }

  while (1) {
    my $len = index($object->{buf},"\n");
    if ($len >= 0) {
      my $line = substr($object->{buf}, 0, $len+1);
      $object->{buf} = substr($object->{buf}, $len+1);
      $line =~ s/\r?\n$//;
      print {$object->{log}} substr($object->{type}, 0, 1), "< $line\n"
	  if ($config->{verbose} || $object->{type} eq type_server || $line !~ /^info/
	      || $line =~ /(string)/);
      $object->{last_activity} = time;
      if ($object->{type} eq type_server || important_message($line)) {
	my ($sec,$min,$hour,$mday,$mon,$year) = localtime($object->{last_activity});
	printf {$object->{log}} "' %d%02d%02d-%02d%02d%02d\n",
	  1900+$year,$mon+1,$mday,$hour,$min,$sec;
      }
      return $line;
    }
    return undef if ($selector && ! $selector->can_read($timeout));
    my $buf;
    unless ($in->sysread($buf, 65536)) {
      die "usi client died" if ($object->{type} eq type_client);
      return connection_closed;
    }
    $object->{buf} .= $buf;
    $timeout = 0 if defined $timeout;
  }
}

sub assign_time ($) {
  my ($status) = @_;
  my $turn_used = $status->{time_used}->{$status->{board}->{turn}};
  my $left = $config->{sec_limit} - $turn_used;
  if ($left < $config->{sec_limit_up}*2) {
    $status->{sec_standard} = $status->{sec_hard}
      = $config->{sec_limit_up};
    return;
  }
  my $nmoves = $status->{board}->count_moves;
  printf STDERR "time used %ds %ds\n", $status->{time_used}->{'+'},
    $status->{time_used}->{'-'}
      if $status->{phase} != phase_puzzling;
  if ($config->{sec_limit_up} > 0) {
    my $a = $config->{sec_limit_up}*2;
    my $tt = 120 + $nmoves/5;
    my $t = ($nmoves < $tt) ? ($tt - $nmoves)/2+1 : 10;
    $a = 1.0 * $left / $t if $a < 1.0 * $left / $t;
    $status->{sec_standard} = $a;
    $status->{sec_hard} = $a*2 < $left ? $a*2 : $left;
    return;
  }
  if ($left < 20) {
    $status->{sec_standard} = $status->{sec_hard} = 1;
    return;
  } elsif ($left < 60) {
    $status->{sec_standard} = 1;
    $status->{sec_hard} = 2;
    return;
  }
  my $p = $config->{ponder} ? 2.2 : 2.0;
  my $t = ($nmoves < 104) ? ((125 - $nmoves)/$p+1) : $nmoves/8;
  my $reduce = ($config->{sec_limit_up} < 30) ? 0.85 : 1.0;
  my $a = $reduce*($left - 60) / $t + 1;
  my $scale = ($config->{sec_limit_up} < 30) ? 2.5 : 5;
  $status->{sec_standard} = $a;
  $status->{sec_hard} = $a*$scale < ($left-60) ? $a*$scale : ($left-60);

  # adjustment
  ## limit
  if ($config->{sec_limit_up} < 30) {
    my $maximum = 24.0 * $config->{sec_limit} / 1500;
    $a = $maximum if ($a > $maximum);
  }
  ## increase
  if ($status->{board}->{turn} eq $status->{my_color}
      && $left > $config->{sec_limit}/2
      && ($status->{board}->count_hand_other_than_pawn > 0
	  || $status->{board}->count_hand_pawn > 2)
      && $status->{sec_standard} < $config->{sec_limit}/60.0) {
    my $a = $config->{sec_limit}/60.0;
    printf STDERR "increased %.1f => %.1f\n", $status->{sec_standard}, $a;
    $status->{sec_standard} = $a;
    $status->{sec_hard} = $a*2.5 < ($left-60) ? $a*2.5 : ($left-60);
  }
  ## decrease
  my $my_used = $status->{time_used}->{$status->{my_color}};
  if ($status->{board}->{turn} ne $status->{my_color}) {
    if ($my_used > $turn_used && $status->{sec_standard} > 4.0) {
      $status->{sec_standard} = 4.0;
      $status->{sec_hard} = 4.0;
      print STDERR "decreased prediction time to 4s since $my_used > $turn_used\n";
    }
  }
  if ($status->{board}->{turn} eq $status->{my_color}
      && $my_used > $config->{sec_limit}*0.2
      && $status->{sec_standard} > 5.0) {
    my $op_color = ($status->{my_color} eq '+') ? '-' : '+';
    my $op_used = $status->{time_used}->{$op_color};
    my $op_left = $config->{sec_limit} - $op_used;
    my $coef = 0;
    if ($op_left*0.5 > $left) {
      $coef = 0.5;
    } elsif ($op_left*0.7 > $left) {
      $coef = 0.4;
    } elsif ($op_left*0.8 > $left) {
      $coef = 0.3;
    } elsif ($op_left*0.9 > $left) {
      $coef = 0.2;
    } elsif ($op_left > $left) {
      $coef = 0.1;
    }
    if ($coef > 0) {
      printf STDERR "decreased search time by %d%% (%.1fs)\n",
	100*$coef, $status->{sec_standard} * $coef;
      $status->{sec_standard} *= (1-$coef);
      $status->{sec_hard} *= (1-$coef);
    }
  }
}

sub new_position ($) {
  my ($status) = @_;
  $status->{id}++;
  $status->{search} = {};
  $status->{start} = StopWatch::current_time;
  write_line($status->{client}, $status->{board}->usi_show);
  if ($status->{phase} == phase_puzzling) {
    my @ignores = keys %{$status->{ponder}};
    if (@ignores+0) {
      print STDERR "pondered ".join(', ', map { $_."=>".$status->{ponder}->{$_}->{bestmove} } @ignores)."\n";
      write_line($status->{client}, sprintf "ignore_moves %s",
		 join(' ', map { $status->{board}->csamove2usi($_) }
		      @ignores));
    }
  }
  assign_time($status);
  if ($status->{phase} == phase_puzzling) {
    $status->{sec_standard} /= 4.0;
    $status->{sec_hard} /= 4.0;
    $status->{sec_standard} = 1 if $status->{sec_standard} < 1;
    $status->{sec_hard} = 1 if $status->{sec_hard} < 1;
  }
  $status->{sec_standard} = POSIX::floor($status->{sec_standard}) + sec_margin_csa;
  $status->{sec_hard} = POSIX::floor($status->{sec_hard}) + sec_margin_csa;
  printf STDERR "newposition %d turn %s (%s) phase %d time %.1fs (%.1fs) moves %d\n",
    $status->{id}, $status->{board}->{turn}, $status->{my_color},
      $status->{phase}, $status->{sec_standard}, $status->{sec_hard},
	$status->{board}->count_moves;
  write_line($status->{client}, sprintf("go byoyomi %d", POSIX::ceil($status->{sec_hard}*1000)))
    if ($config->{ponder} || $status->{phase} != phase_puzzling);
}

sub valid_usi_move ($) {
  my ($move) = @_;
  $move =~ s/^(resign|pass|win)$//;
  $move =~ s/^[1-9][a-z][1-9][a-z]\+?//g;
  $move =~ s/^[PLNSGBR]\*[1-9][a-z]\+?//g;
  return $move =~ /^$/;
}

sub valid_usi_pv ($) {
  my ($pvstr) = @_;
  my @pv = split(/\s+/, $pvstr);
  foreach $_ (@pv) {
    return undef unless valid_usi_move($_);
  }
  return 1;
}

sub valid_usi ($) {
  my ($line) = @_;
  if ($line =~ /^info /) {
    $line =~ s/\s+score cp\s+-?[0-9]+(\s|$)/ /g;
    $line =~ s/\s+score mate\s+-?[0-9]+(\s|$)/ /g;
    $line =~ s/\s+time\s+[0-9]+(\s|$)/ /g;
    $line =~ s/\s+seldepth\s+[0-9]+(\s|$)/ /g;
    $line =~ s/\s+depth\s+[0-9]+(\s|$)/ /g;
    $line =~ s/\s+upperbound/ /g;
    $line =~ s/\s+lowerbound/ /g;
    $line =~ s/\s+multipv\s+-?[0-9.]+(\s|$)/ /g;
    $line =~ s/\s+nodes\s+-?[0-9]+(\s|$)/ /g;
    $line =~ s/\s+nps\s+-?[0-9.]+(\s|$)/ /g;
    $line =~ s/\s+hashfull\s+[0-9]+(\s|$)/ /g;
    $line =~ s/\s+currmove\s+(([0-9][a-z]|[PLNSGBR]\*)[0-9][a-z]\+?|resign)(\s|$)/ /g;
    $line =~ s/\s+string\s+.*$/ /g;
    return 0 if ($line =~ s/\s+pv\s+(.*)$// && ! valid_usi_pv($1));
    return ($line =~ /^info\s*$/);
  }
  return 1;
}

sub start_game ($) {
  my ($status) = @_;
  printf STDERR "\nnew game %s\n\n", $status->{gameid};
  $status->{board} = new CsaBoard;
  die unless $status->{my_color};
  $status->{time_used} = {'+'=>0, '-'=>0};
  $status->{id} = 0;
  $status->{bestmove} = 0;
  $status->{phase} = ($status->{my_color} eq '+')
    ? phase_thinking : phase_puzzling;
  write_line($status->{client}, "usinewgame") if $config->{usinewgame};
  new_position($status);
}

sub stop_and_wait_bestmove ($) {
  my ($status) = @_;
  return if (defined $status->{bestmove} && $status->{bestmove} == $status->{id});
  my $now = StopWatch::current_time;
  if ($status->{start}+$config->{stop_interval}*1000 > $now) {
    warn "sleep $config->{stop_interval} before stop";
    Time::HiRes::sleep(($status->{start}-$now)/1000.0
		       + $config->{stop_interval});
  }
  write_line($status->{client}, "stop");
  my $stop_sent = time;
  while (my $line=read_line($status->{client})) {
    last if $line =~ /^bestmove/;
    if (time > $stop_sent + 10) {
      warn "try stop again";
      write_line($status->{client}, "stop");
      $stop_sent = time;
    }
  }
  $status->{bestmove} = $status->{id};
}

sub handle_server_message ($$) {
  my ($line, $status) = @_;
  if ($line !~ /^$/) {
    if ($line =~ /([+%-].*),(T\s?\d+)$/) {
      print { $status->{csa} } $1."\n".$2."\n";
    } else {
      print { $status->{csa} } $line."\n";
    }
  }
  print STDERR "received: ".$line."\n" if $line =~ /^[#%]/;
  return if ($line =~ /^[#%]/ || $line =~ /^$/);
  $status->{timeout} = default_timeout;
  die unless ($line =~ /^([+-])(\d{4}\w{2})(,T(\d+))?/);
  my ($color, $move, $sec) = ($1, $2, $4);
  $status->{time_used}->{$color} += $sec || 1;
  if ($color eq $status->{my_color}) {
    print {$status->{csa}} "'** ".$status->{prev_pv}."\n"
      if $status->{prev_pv};
    $status->{prev_pv} = "";
    return;
  }
  if ($status->{phase} == phase_pondering
      && $status->{ponder}->{current} eq $color.$move) {
    # ponder hit, continue searching
    print STDERR "\nponder hit\n\n";
    $status->{phase} = phase_thinking;
    return;
  }
  if ($config->{ponder}) {
    stop_and_wait_bestmove($status);
    $status->{board}->unmake_move
      if ($status->{phase} == phase_pondering);
  }
  print STDERR "<= ".$color.$move."\n";
  $status->{board}->make_csamove($color.$move); # opponent move
  print STDERR $status->{board}->csa_show;
  my $search = $status->{ponder}->{$color.$move};
  if ($search) {
    # ponder hit, move immediately
    my $my_move = $search->{bestmove};
    my $msg = $my_move;
    $status->{prev_value} = undef;
    if ($search->{$my_move}->{pv}) {
      $msg .= sprintf ",'* %d %s", $search->{$my_move}->{score},
	join(' ', @{$search->{$my_move}->{pv}})
	  if ($config->{send_pv});
      $status->{prev_pv} = sprintf "%d %s", $search->{$my_move}->{score},
	join(' ', @{$search->{$my_move}->{pv}});
      $status->{prev_value} = $search->{$my_move}->{score};
    }
    write_line($status->{server}, $msg);
    $status->{phase} = phase_puzzling;
    $status->{board}->make_csamove($my_move);
    print STDERR "ponder hit => $my_move\n";
    print STDERR $status->{board}->csa_show;
    delete $status->{ponder};
    new_position($status);
    return;
  }
  # new search
  $status->{phase} = phase_thinking;
  new_position($status);
}

sub handle_client_message ($$) {
  my ($line, $status) = @_;
  if ($line =~ /^info\s+string.*confident/) {
    $status->{confident} = $status->{id};
    return;
  }
  if ($line =~ /^info /) {
    die "unknown syntax $line" unless (valid_usi($line));
    my $depth = ($line =~ /\s+depth\s+([0-9]+)/) && $1;
    my $score = ($line =~ /\s+score cp\s+(-?[0-9.]+)/) && $1;
    if($line =~ /\s+score mate\s+(-?[0.9].+)/){
      if($1 > 0){ $score = 30000-$1; }
      else { $score = -30000-$1; }
    }
    my $nodes = ($line =~ /\s+nodes\s+([0-9.]+)/) && $1;
    my ($usimove,@pv) = ($line =~ /\s+pv\s+(.+)/) && split(/\s+/, $1);
    return if (!$usimove || $usimove =~ /resign/); # ignore resign unless final decision
    return unless defined $score;
    $score *= $status->{board}->sign*100.0/$config->{pawn_value};
    my $move = $status->{board}->usi2csa($usimove);
    $status->{search}->{$move} ||= {};
    my $info = $status->{search}->{$move};
    $info->{pv}=$status->{board}->usimoves2csa([$usimove, @pv]);
    # shift @{$info->{pv}};
    $info->{score} = $score;
    $info->{nodes} = $nodes;
    my $now = StopWatch::current_time;
    $info->{first_update} = $now unless $info->{first_update};
    $info->{last_update} = $now;
    my $prev_bestmove= $status->{search}->{bestmove} || "";
    if ($prev_bestmove ne $move) {
      $info->{total} ||= 0;
      $status->{search}->{$prev_bestmove}->{total}
	+= $now - $status->{search}->{$prev_bestmove}->{last_changed}
	  if ($prev_bestmove);
      $info->{last_changed} = $now;
      $status->{search}->{bestmove} = $move;
    }
    my $score_pv=sprintf("%6d %s", $score, join('', @{$info->{pv}}));
    if (($status->{last_score_pv}||"") ne $score_pv) {
      printf STDERR "%s (%4.1fs, %4.1fs)\n", $score_pv,
	($now - $status->{start})/1000.0,
	  ($info->{total}+$now - $info->{last_changed})/1000.0
	    unless $status->{phase} == phase_puzzling;
      $status->{last_score_pv} = $score_pv;
    }
    return;
  }
  elsif ($line =~ /^bestmove\s+(.+)/) {
    my $move = $1;
    $move =~ s/\s+ponder\s+.*//;
    die "unknown syntax $line" unless (valid_usi_move($move));
    $status->{bestmove} = $status->{id};
    my $csa = $status->{board}->usi2csa($move);
    return unless $csa;
    $status->{search}->{bestmove} = $csa;
  }
  else {
    warn "unknown message $line";
  }
}

sub is_stable ($$$) {
  my ($sign, $a, $b) = @_;
  return 1 if ! defined $a || ! defined !$b;
  $a *= $sign;
  $b *= $sign;
  return ($b >= 1000) || ($b - $a > - &stable_margin) || ($b - $a > -abs($a)/4);
}

sub handle_timer ($) {
  my ($status) = @_;
  my $now = StopWatch::current_time;
  my $move = $status->{search}->{bestmove};
  return unless $move;
  my $elapsed = $now - $status->{start};
  if ($status->{bestmove} == $status->{id}
      || $elapsed > $status->{sec_hard}*1000
      || ($elapsed > $status->{sec_standard}*1000
	  && ($now - $status->{search}->{$move}->{last_changed}
	      + $status->{search}->{$move}->{total}
	      >= $elapsed*$config->{stable_ratio})
	  && ($status->{phase} != phase_thinking
	      || is_stable($status->{board}->sign, $status->{prev_value},
			   $status->{search}->{$move}->{score})))) {
    stop_and_wait_bestmove($status);
    if ($status->{phase} == phase_thinking) {
      # make my move
      my $msg = $move;
      $status->{prev_value} = undef;
      if ($status->{search}->{$move}->{pv}) {
	$msg .= sprintf ",'* %d %s", $status->{search}->{$move}->{score},
	  join(' ', @{$status->{search}->{$move}->{pv}})
	    if ($config->{send_pv});
	$status->{prev_pv} = sprintf "%d %s",
	  $status->{search}->{$move}->{score},
	    join(' ', @{$status->{search}->{$move}->{pv}});
	$status->{prev_value} = $status->{search}->{$move}->{score};
	if ($config->{resign_value} > 0 && $status->{search}->{$move}->{score}
	   && $move =~ /^([+-])/) {
	  my $sign = ($1 eq "+") ? 1 : -1;
	  $msg = "%TORYO"
	    if $status->{search}->{$move}->{score}*$sign < -$config->{resign_value};
	}
      }
      write_line($status->{server}, $msg);
      $status->{phase} = phase_puzzling;
      $status->{board}->make_csamove($move);
      print STDERR "=> ".$move."\n";
      print STDERR $status->{board}->csa_show;
      delete $status->{ponder};
      new_position($status);
    }
    elsif ($status->{phase} == phase_puzzling) {
      if ($move !~ /^%/ && ! defined $status->{ponder}->{$move}) {
	# start pondering
	$status->{phase} = phase_pondering;
	$status->{ponder}->{current} = $move;
	$status->{board}->make_csamove($move);
	new_position($status);
	print STDERR "ponder on $move\n";
      } else {
	$status->{timeout} = keep_alive;
      }
    }
    elsif ($status->{phase} == phase_pondering) {
      # start puzzling again
      $status->{phase} = phase_puzzling;
      my $ponder = $status->{ponder}->{current};
      $status->{ponder}->{$ponder} = $status->{search};
      $status->{board}->unmake_move;
      delete $status->{ponder}->{current};
      new_position($status);
    }
  }
  write_line($status->{server}, "")
    if ($status->{server}->{last_activity}+keep_alive < time
	&& !$config->{server_stdio});
}
