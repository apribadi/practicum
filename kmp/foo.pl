use strict;

sub foo {
    my @bar = split '', shift;
    for (0 .. @bar - 1) {
        if ($_ == 2) { last; }
        print $_, "\n";
    }
}


my $foo = length "asdf";
print "$foo\n";
