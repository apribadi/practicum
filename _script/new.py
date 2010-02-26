#!/usr/local/bin/python
#
# Script to test an ACM submission and see if it passes.  Should be
# run by the uid of the user doing the submission, not a priveleged
# user (as it runs the submitted code, and would otherwise be a big
# security hole).
#
# Usage: /cs/ACM/acmSubmit <submission name>

import glob

print "\nacmSubmit (Version 2010)\n"
root_dir = "/cs/ACM"
semester = "Spring09";
submission_dir = root_dir + "/Submissions2/" + se
TEST_DIR = "$ROOT/Tests/$SEMESTER";

# Path to various scripts
#$TEST_SCRIPT = "$ROOT/SubScripts/acmTest2";
$TEST_SCRIPT = "$ROOT/SubScripts/acmTestSpring2009";

# Where to mail test results
$MAILTO = 'dodds@cs.hmc.edu';
# Build a list of programs that can be submitted, by looking at the
# tests directory.
my $testdir = $TEST_DIR;
my %programs = ();		# Hash for quick lookup of available programs

opendir DIR, $testdir or die "Can't read tests in $testdir";
for ( grep { ! /^\./ && -d "$testdir/$_" } readdir(DIR) ) {
	$programs{$_} = 1;
}
closedir DIR;

sub usage {
	
    print "Usage: /cs/ACM/acmSubmit <filename>.$USAGE_EXT [additional files...]\n\n";
    print "Where <filename> is one of:\n";
    for ( sort keys %programs ) {
		print "    $_\n";
    }
}

# Check command line is valid
if ( $#ARGV < 0 ) {
	usage;
    exit 1;
}


#my $submission = $ARGV[0];
my $fullname = $ARGV[0];
my ($submission, $submitpath, $suffix) = fileparse($fullname, @EXTENSIONS);

# Check submission name is valid
if ( $submission =~ /^(\w+)$/ ) {
    $submission = $1;		# Untaint
} else {
    die "Invalid submission name: $submission";
}

unless ( exists $programs{$submission} ) {
	usage;
    exit 1;
}

print "  Testing $submission ...\n\n";

print "  Suffix is $suffix...   ";

# Detect language used
my $lang;
if ( $suffix eq ".c" ) {
    $lang = LANG_C;
} elsif ( $suffix eq ".cs" ) {
    $lang = LANG_CSHARP;
} elsif ( $suffix eq ".hs" ) {
    $lang = LANG_HASKELL;
} elsif ( $suffix eq ".cc" ) {
    $lang = LANG_CPP;
} elsif ( $suffix eq ".java" ) {
    $lang = LANG_JAVA;
} elsif ( $suffix eq ".py" ) {
    $lang = LANG_PYTH;
} elsif ( $suffix eq ".pl" ) {
    $lang = LANG_PERL;
} elsif ( $suffix eq ".php" ) {
    $lang = LANG_PHP;
} elsif ( $suffix eq ".ps" ) {
    $lang = LANG_PS;
} elsif ( $suffix eq ".rb" ) {
    $lang = LANG_RUBY;
} elsif ( $suffix eq ".lua" ) {
    $lang = LANG_LUA;
} elsif ( $suffix eq ".clj" ) {
    $lang = LANG_CLOJURE;
} else {
    die "Cannot detect language type";
}

print "  Language used: ", ("C","C++","Java","Python","Perl","PHP","Ruby","Haskell","C#","Postscript","Lua","Clojure")[$lang], "\n\n";

if ( -f $submission . $suffix ) {
    print "  Found file ", $submission . $suffix, "\n\n";
} else {
    die "  ** Cannot find that file! **\n\n\n";
}


# Compile the program and set a command that can be used to run it
# later
my $runcmd;
my $sourcefilename;

if ( $lang == LANG_C ) {
    system "gcc $submission.c -o $submission -lm";
    $runcmd = "./$submission";
    $sourcefilename = $submission . ".c"
} elsif ( $lang == LANG_CSHARP ) {
    system "gmcs $submission.cs";
    $runcmd = "mono ./$submission.exe";
    $sourcefilename = $submission . ".cs"
} elsif ( $lang == LANG_CPP ) {
    system "g++ $submission.cc -o $submission";
    $runcmd = "./$submission";
    $sourcefilename = $submission . ".cc"
} elsif ( $lang == LANG_JAVA ) {
    system "javac -cp . $submission.java";
    $runcmd = "java -cp . $submission";
    $sourcefilename = $submission . ".java"
} elsif ( $lang == LANG_HASKELL ) {
    system "ghc -O --make $submission.hs -o $submission";
    $runcmd = "./$submission";
    $sourcefilename = $submission . ".hs"
} elsif ( $lang == LANG_PYTH ) {
	$runcmd = "python $submission.py";
  $sourcefilename = $submission . ".py";
} elsif ( $lang == LANG_PERL ) {
	$runcmd = "perl ./$submission.pl";
  $sourcefilename = $submission . ".pl";
} elsif ( $lang == LANG_PHP ) {
	$runcmd = "php ./$submission.php";
  $sourcefilename = $submission . ".php";
} elsif ( $lang == LANG_PS ) {
	$runcmd = "gs -dNODISPLAY -dNOPROMPT -q ./$submission.ps";
  $sourcefilename = $submission . ".ps";
} elsif ( $lang == LANG_RUBY ) {
	$runcmd = "ruby ./$submission.rb";
  $sourcefilename = $submission . ".rb";
} elsif ( $lang == LANG_LUA ) {
	$runcmd = "lua ./$submission.lua";
  $sourcefilename = $submission . ".lua";
} elsif ( $lang == LANG_CLOJURE ) {
	my $clj_dir = "/home/dodds/public_html/ACM"
	my $classpath = "$clj_dir/clojure.jar:$clj_dir/clojure-contrib.jar"
	$runcmd = "java -cp $classpath clojure.main ./$submission.clj"
  $sourcefilename = $submission . ".clj";
} elsif ( $? >> 8 ) {
    print "  Could not compile $submission.\n\n";
    print "  Ending submission.\n\n";
    exit 1;
}

# Locate test files
opendir DIR, "$TEST_DIR/$submission"
    or die "Can't read tests directory: $!";

my @tests = ();
for ( sort grep { /\.in$/ && -f "$TEST_DIR/$submission/$_" } readdir DIR ) {
    /^(.*)\.in$/;
    push @tests, "$TEST_DIR/$submission/$1";
}

closedir DIR;

# Run the tests
my $testfile;
my $pass = 1;

foreach $testfile ( @tests ) {
    print "  Running test $testfile ...\n\n";

    open TESTOUTPUT, "$runcmd < $testfile.in |";
    open CORRECTOUTPUT, "$testfile.out";

    # Stolen from previous acmTest script, by Zach Dodds
    my $yourline;
    my $myline;

    # break on newlines
    while ( $myline = <CORRECTOUTPUT> )
    {
	$yourline = <TESTOUTPUT>;
	if ($yourline)
	{
	    # chop($yourline);
	    # chop($myline);

	    if ($yourline eq $myline)
	    { 
		# OK
		# print "OK line\n";
	    }
	    else 
	    {
		# lines differ
		print "  Your output line:\n\n";
		print $yourline;
		# print "length is ".length($yourline)."\n";
		print "\n\n  The solution line:\n\n";
		print $myline;
		# print "length is ".length($myline)."\n";
		print "\n\n";
		$pass = 0;
	    }
	}
	else 
	{
	    if ($myline =~ /^\s*$/)
	    {
		# OK if all whitespace
		# print "all whitespace extra line\n";
	    }
	    else
	    {
		# TESTOUTPUT (submission's output) was too short
		print "  Your output line:\n\n";
		print ((defined $yourline) ? $yourline : "<empty>\n");
		print "\n\n  The solution line:\n\n";
		print $myline;
		print "\n\n";
		print "  Your submission's output was too short.\n";
		$pass = 0;
	    }
	}
	if (!$pass)
	{
	    last;
	}
    }

    if (<TESTOUTPUT> && $pass)
    {
	# TESTOUTPUT was too long
	print "Submission output was too long.\n";
	$pass = 0;
    }
    else
    {
	# print "files were the same length\n";
    }

    close(CORRECTOUTPUT);
    close(TESTOUTPUT);
}

if ($pass)
{
    print "  -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+\n";
    print "  Congratulations! You've completed the $submission problem.\n";
    print "  -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+\n\n\n";

    # all the mail stuff seems to be broken... we'll
    # use python indtead...
    #
    my @args = ("/cs/ACM/SubScripts/pythonMail.py", $sourcefilename, $submission, $suffix);
    system(@args) == 0 or die "system @args failed: $?";

    #my $thecurrenttime = localtime(time);

    # email me
    #my $openfilename = "<$sourcefilename";
    #print "openfilename is ".$openfilename;
    #open(SOURCE, $openfilename) || die("Couldn's open the file!");
    #
    #my $student = getuser();
    #open(MAIL, "| mail dodds\@cs.hmc.edu");
    #print MAIL "From: $student\n";
    #open(MAIL, "| mail -s \"ACM\" dodds\@cs.hmc.edu");
    #print MAIL "Subject: ACM\n";
    #print MAIL "\n";
    #print MAIL "\n";
    #print MAIL "\n"; # $student passed "; #$submission at $thecurrenttime \n";
    #print MAIL "\n";
    #print MAIL "\n";

    #my $aline;
    #while ($aline = <SOURCE>) {
    #    print MAIL $aline;
    #}

    #print MAIL "\n";
    #print MAIL "\n";

    #close MAIL;
    #close(SOURCE);

    exit 0;
}
else
{
    print "\n\n  Your output differed from the expected output.\n";
    exit 1;
}
