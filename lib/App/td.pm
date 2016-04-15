package App::td;

# DATE
# VERSION

use 5.010001;
#IFUNBUILT
use strict;
use warnings;
#END IFUNBUILT

our %SPEC;

sub _get_table_spec_from_envres {
    my $envres = shift;
    my $ff = $envres->[3]{'table.fields'};
    return undef unless $ff;
    my $spec = {fields=>{}};
    my $i = 0;
    for (@$ff) {
        $spec->{fields}{$_} = {pos=>$i};
        $i++;
    }
    $spec;
}

sub _decode_json {
    require Cpanel::JSON::XS;

    state $json = Cpanel::JSON::XS->new->allow_nonref;
    $json->decode(shift);
}

$SPEC{td} = {
    v => 1.1,
    summary => 'Manipulate table data',
    description => <<'_',

*td* receives table data from standard input and performs an action on it. It
has functionality similar to some Unix commands like *head*, *tail*, *wc*,
*cut*, *sort* except that it operates on table rows/columns instead of
lines/characters. This is convenient to use with CLI scripts that output table
data.

A _table data_ is JSON-encoded data in the form of either: `hos` (hash of
scalars, which is viewed as a two-column table where the columns are `key` and
`value`), `aos` (array of scalars, which is viewed as a 1-column array where the
column is `elem`), `aoaos` (array of array of scalars), or `aohos` (array of
hash of scalars).

The input can also be an _enveloped_ table data, where the envelope is an array:
`[status, message, content, meta]` and `content` is the actual table data. This
kind of data is produced by `Perinci::CmdLine`-based scripts and can contain
more detailed table specification in the `meta` hash, which `td` can parse.

First you might want to the `info` action to see if the input is a table data:

    % osnames -l --json | td info

If input is not valid JSON, a JSON parse error will be displayed. If input is
valid JSON but not a table data, another error will be displayed. Otherwise,
information about the table will be displayed (form, number of columns, column
names, number of rows, and so on).

Next, you can use these actions:

    # count number of rows (equivalent to "wc -l" Unix command)
    % osnames -l --json | td rowcount

    # append a row containing rowcount
    % osnames -l --json | td rowcount-row

    # count number of columns
    % osnames -l --json | td colcount

    # select some columns
    % osnames -l --json | td select value description

    # sort by column(s) (add "-" prefix to for descending order)
    % osnames -l --json | td sort value tags
    % osnames -l --json | td sort -- -value

    # return sum of all numeric columns
    % list-files -l --json | td sum

    # append a sum row
    % list-files -l --json | td sum-row

    # return average of all numeric columns
    % list-files -l --json | td avg

    # append an average row
    % list-files -l --json | td avg-row

_
    args => {
        action => {
            summary => 'Action to perform on input table',
            schema => ['str*', in => [qw/
                                            avg
                                            avg-row
                                            colcount
                                            colcount-row
                                            head
                                            info
                                            rowcount
                                            rowcount-row
                                            select
                                            sort
                                            sum
                                            sum-row
                                            tail
                                            wc
                                            wc-row
                                        /]],
            req => 1,
            pos => 0,
            description => <<'_',

_
        },
        argv => {
            summary => 'Arguments',
            schema => ['array*', of=>'str*'],
            default => [],
            pos => 1,
            greedy => 1,
        },
    },
};
sub td {
    my %args = @_;
    my $action = $args{action};
    my $argv   = $args{argv};

    my ($input, $input_form, $input_obj);
  GET_INPUT:
    {
        require Data::Check::Structure;
        eval {
            local $/;
            $input = _decode_json(~~<STDIN>);
        };
        return [400, "Input is not valid JSON: $@"] if $@;

        # give envelope if not enveloped
        unless (ref($input) eq 'ARRAY' &&
                    @$input >= 2 && @$input <= 4 &&
                    $input->[0] =~ /\A[2-5]\d\d\z/ &&
                    !ref($input->[1])
                ) {
            $input = [200, "Envelope added by td", $input];
        }

        # detect table form
        if (ref($input->[2]) eq 'HASH') {
            $input_form = 'hash';
            require TableData::Object::hash;
            $input_obj = TableData::Object::hash->new($input->[2]);
        } elsif (Data::Check::Structure::is_aos($input->[2])) {
            $input_form = 'aos';
            require TableData::Object::aos;
            $input_obj = TableData::Object::aos->new($input->[2]);
        } elsif (Data::Check::Structure::is_aoaos($input->[2])) {
            $input_form = 'aoaos';
            my $spec = _get_table_spec_from_envres($input);
            require TableData::Object::aoaos;
            $input_obj = TableData::Object::aoaos->new($input->[2], $spec);
        } elsif (Data::Check::Structure::is_aohos($input->[2])) {
            $input_form = 'aohos';
            my $spec = _get_table_spec_from_envres($input);
            require TableData::Object::aohos;
            $input_obj = TableData::Object::aohos->new($input->[2], $spec);
        } else {
            return [400, "Input is not table data, please feed a hash/aos/aoaos/aohos"];
        }
    } # GET_INPUT

    my $output;
  PROCESS:
    {
        if ($action eq 'info') {
            my $form = ref($input_obj); $form =~ s/^TableData::Object:://;
            my $info = {
                form => $form,
                rowcount => $input_obj->row_count,
                colcount => $input_obj->col_count,
                cols => join(", ", @{ $input_obj->cols_by_idx }),
            };
            $output = [200, "OK", $info];
            last;
        }

        if ($action eq 'rowcount') {
            $output = [200, "OK", $input_obj->row_count];
            last;
        }

        # XXX return aohos as aohos again
        if ($action eq 'rowcount-row') {
            my $cols = $input_obj->cols_by_idx;
            my $rows = $input_obj->rows_as_aoaos;
            my $rowcount_row = [map {''} @$cols];
            $rowcount_row->[0] = $input_obj->row_count if @$rowcount_row;
            $output = [200, "OK", [@$rows, $rowcount_row],
                       {'table.fields' => $cols}];
            last;
        }

        if ($action eq 'colcount') {
            $output = [200, "OK", $input_obj->col_count];
            last;
        }

        if ($action =~ /\A(sum|sum-row|avg|avg-row)\z/) {
            require Scalar::Util;
            my $cols = $input_obj->cols_by_idx;
            my $rows = $input_obj->rows_as_aoaos;
            my $sum_row = [map {0} @$cols];
            for my $i (0..$#{$rows}) {
                my $row = $rows->[$i];
                for my $j (0..@$cols-1) {
                    $sum_row->[$j] += $row->[$j]
                        if Scalar::Util::looks_like_number($row->[$j]);
                }
            }
            my $avg_row;
            if ($action =~ /avg/) {
                if (@$rows) {
                    $avg_row = [map { $_ / @$rows } @$sum_row];
                } else {
                    $avg_row = [map {0} @$cols];
                }
            }
            # XXX return aohos if input is aohos
            if ($action eq 'sum') {
                $output = [200, "OK", [$sum_row],
                           {'table.fields' => $cols}];
            } elsif ($action eq 'sum-row') {
                $output = [200, "OK", [@$rows, $sum_row],
                           {'table.fields' => $cols}];
            } elsif ($action eq 'avg') {
                $output = [200, "OK", [$avg_row],
                           {'table.fields' => $cols}];
            } elsif ($action eq 'avg-row') {
                $output = [200, "OK", [@$rows, $avg_row],
                           {'table.fields' => $cols}];
            }
            last;
        }

        if ($action =~ /\A(sort|select)\z/) {
            return [400, "Please specify one or more columns"] unless @$argv;
            my $res;
            if ($action eq 'sort') {
                if ($input_form eq 'aohos') {
                    $res = $input_obj->select_as_aohos(undef, undef, $argv);
                } else {
                    $res = $input_obj->select_as_aoaos(undef, undef, $argv);
                }
            } elsif ($action eq 'select') {
                if ($input_form eq 'aohos') {
                    $res = $input_obj->select_as_aohos($argv);
                } else {
                    $res = $input_obj->select_as_aoaos($argv);
                }
            }

            my $resmeta = {};
            {
                my $ff = $res->{spec}{fields} or last;
                my $tff = [];
                for (keys %$ff) {
                    $tff->[$ff->{$_}{pos}] = $_;
                }
                $resmeta->{'table.fields'} = $tff;
            }
            $output = [200, "OK", $res->{data}, $resmeta];
            last;
        }

        return [400, "Unknown action '$action'"];
    } # PROCESS

  POSTPROCESS_OUTPUT:
    {
        require Pipe::Find;
        my $pipeinfo = Pipe::Find::get_stdout_pipe_process();
        last unless $pipeinfo;
        last unless
            $pipeinfo->{exe} =~ m![/\\]td\z! ||
            $pipeinfo->{cmdline} =~ m!\A([^\0]*[/\\])?perl\0([^\0]*[/\\])?td\0!;
        $output->[3]{'cmdline.default_format'} = 'json';
    }
    $output;
}

1;
# ABSTRACT:

=head1 SEE ALSO

L<Rinci::function> for a more detailed explanation on enveloped result.

L<TableDef> for more detailed explanation of table data definition, which can be
specified in enveloped result's `meta` hash in the `table` key (see
L<Perinci::Sub::Property::result::table>).

L<TableData::Object>

L<Perinci::CmdLine>
