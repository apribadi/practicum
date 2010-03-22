use strict;

my $Verbose = 1;

sub kmp_table {
    my ($word) = @_;

    my @table = (-1, 0);
    my $cnd = 0;

    for my $pos (2 .. length $word) {
        if (substr($word, $pos - 1, 1) eq substr($word, $cnd, 1)) {
            $table[$pos] = ++$cnd;
            next;
        }
        if ($cnd > 0) {
            $cnd = $table[$cnd];
            redo;
        }
        $table[$pos] = 0;
    }

    return @table;
}

sub kmp {
    my ($word, $text) = @_;

    print "String to find: $word\n" if $Verbose;
    print "Search string: $text\n"  if $Verbose;

    my @word = split '', $word;
    my @text = split '', $text;

    my $m = 0;
    my $i = 0;
    my @table = kmp_table($word);

    if ($Verbose) {
        local $" = ', ';
        print "Partial match table [@table]\n";
    }

    my @mismatch = (0) x @table;

    while ($m + $i < @text) {
        if ($word[$i] eq $text[$m + $i]) {
            $i++;
            if ($i == @word) {
                if ($Verbose) {
                    print "Match at m = $m\n";
                    local $" = ', ';
                    print "Mismatch totals = [@mismatch]\n";
                }
                return $m;
            }
        }
        else {
            $mismatch[$i]++;
            $m = $m + $i - $table[$i];
            $i = ($table[$i] > -1) ? $table[$i] : 0;
        }
    }

    if ($Verbose) {
        print "No match!\n";
        local $" = ', ';
        print "Mismatch totals = [@mismatch]\n";
    }
    return scalar @text;
}

# start script
my ($word, $text);

chomp($word = <>);
chomp($text = <>);

kmp $word, $text;

