#!/bin/perl
# simple testing script
# 
# The idea is that this is what an observer of a test run would see.
#
# The commands are
#  % cat > Foo.hs <<%n
#    .. contents ...
#  % $TIMBERC $TIMBERA ....
#
# Extensions:
#  <<% 		read a here style document from stdin
#  >% or 1>%	explicity look for *only* stdout
#  2>%		explicity look for *only* stderr
# 
# Flags:
#   --test      run the script, acknolage strange results (default)
#   --script	run the script, output results in script format
#   --date	normalize any dates before the diff

$mode = 'normal';
$DEBUG = 0;
@files = ();

foreach (@ARGV) {
    if (/--script/) {
	$mode  = 'script';
    } elsif (/--debug/) {
	$DEBUG = 1;
    } else {
	push(@files,$_);
    }
}

foreach my $test (@files) {
    print "$test\n";

    local @cmds = ();
    
    local $here = '';

    open(FILE,"$test");
    while(<FILE>) {
	local $mode = 'cli';
	if (/^%\s+(.*)$/) {
	    push(@cmds,$here);
	    $here = '';
	    push(@cmds,$1);
	} else {
	    s/^%% /% /;
	    $here .= $_;
	}
    }
    close(FILE);
    
    open(OUTFILE,">$test.actual");
    system("rm -Rf $test.dir");
    mkdir("$test.dir");
    chdir("$test.dir");
    local $header = shift(@cmds);
    print OUTFILE $header;
    while($#cmds > -1) {
	local $cmd = shift @cmds;
	local $here = shift @cmds;
	print OUTFILE "% $cmd\n";
	print "% $cmd\n" if ($DEBUG == 1);
	if ($cmd =~ /^\s*quit\s*$/) {
	    break;
	} elsif ($cmd =~ /\<\<\%/) {
	    # here doc
	    $cmd = $` . " " . $';
	    open(CMD,"| $cmd ");
	    print CMD $here;
	    print OUTFILE $here;
	    close(CMD);
	} else {
	    # need to check to see if stdout or stderr is redirected
	    if (open(CMD,"$cmd 2>&1 |")) {
		while(<CMD>) {
		    print OUTFILE $_;
		}
		close(CMD);
	    } else {
		print OUTFILE "Command not found: $cmd\n";
	    }
	}
    }
    chdir("..");
    close(OUTFILE);

    open(DIFF,"diff -c $test $test.actual|");
    while(<DIFF>) {
	print "$_";
    }
    close(DIFF);

}

exit 0;
