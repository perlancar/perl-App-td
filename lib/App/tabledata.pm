package App::rmhere;

use 5.010001;
use strict;
use warnings;
#use experimental 'smartmatch';
use Log::Any '$log';

use File::chdir;

# for testing
use Time::HiRes qw(sleep);

#require Exporter;
#our @ISA       = qw(Exporter);
#our @EXPORT_OK = qw(rmhere);

# VERSION

our %SPEC;

$SPEC{rmhere} = {
    v             => 1.1,
    summary       => 'Delete files in current directory',
    args          => {
        estimate => {
            summary => 'Count files first before start deleting',
            schema  => 'bool*',
            description => <<'_',

With this opotion, the program will do an `opendir` and list the directory
first. This can take several minutes if the directory is large, so the program
will not start deleting after several minutes. But with this option, we know how
many files we want to delete, so the progress report will know when to reach
100%.

_
        },
        here => {
            summary => 'Override current directory',
            schema  => 'str*',
        },
        interactive => {
            summary => 'Whether to ask first before deleting each file',
            schema  => [bool => default=>1],
            cmdline_aliases => {
                i => {},
                force => {
                    summary => 'Equivalent to --nointeractive',
                    code => sub { shift->{interactive} = 0 },
                },
                f => {
                    summary => 'Equivalent to --nointeractive',
                    code => sub { shift->{interactive} = 0 },
                },
            },
        },
        progress => {
            summary => 'Show progress report',
            schema  => 'bool*',
            cmdline_aliases => {
                p => {},
                P => {
                    summary => 'Equivalent to --progress --estimate',
                    code => sub {
                        my $args = shift;
                        $args->{progress} = 1;
                        $args->{estimate} = 1;
                    },
                },
            },
        },
        # TODO: match option
        # TODO: dir option
        # TODO: recursive option
    },
    features => {
        progress => 1,
        dry_run  => 1,
    },
};
sub rmhere {
    my %args = @_;

    my $progress    = $args{-progress};
    my $dry_run     = $args{-dry_run};
    my $interactive = $args{interactive};

    # avoid output becomes too crowded/jumbled
    undef($progress) if $interactive;

    # by default we don't show progress, for performance
    undef($progress) unless $args{progress};

    local $CWD = $args{here} if defined $args{here};

    opendir my($dh), "." or return [500, "Can't opendir: $!"];
    my $get_next_file = sub {
        while (defined(my $e = readdir($dh))) {
            next if $e eq '.' || $e eq '..';
            next if (-d $e);
            return $e;
        }
        return undef;
    };
    my $files;
    my $num_files;

    $progress->pos(0) if $progress;
    if ($args{estimate}) {
        $files = [];
        while (defined(my $e = $get_next_file->())) {
            push @$files, $e;
        }
        $num_files = @$files;
        $progress->target($num_files) if $progress;
    } else {
        $progress->target(undef) if $progress;
    }

    my $i = 0;
  ENTRY:
    while (defined(my $e = $files ? shift(@$files) : $get_next_file->())) {
        $i++;
        if ($interactive) {
            while (1) {
                print "Delete $e (y/n)? ";
                my $ans = <STDIN>;
                if ($ans =~ /^[Nn]$/) {
                    next ENTRY;
                } elsif ($ans =~ /^[Yy]$/) {
                    last;
                } else {
                    print "Invalid answer. ";
                }
            }
        }
        if ($dry_run) {
            $log->info("DRY_RUN: Deleting $e ...");
            next;
        } else {
            unlink($e);
        }

        if ($progress) {
            $progress->update(
                message => "Deleted $i files".
                    ($files ? " (out of $num_files)" : ""));
        }
    }
    $progress->finish if $progress;
    [200, "OK"];
}

1;
# ABSTRACT: Delete files in current directory

=head1 SYNOPSIS

See L<rmhere> script.


=head1 DESCRIPTION


=cut
