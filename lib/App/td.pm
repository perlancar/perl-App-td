package App::td;

# AUTHORITY
# DATE
# DIST
# VERSION

use 5.010001;
#IFUNBUILT
use strict;
use warnings;
#END IFUNBUILT

use PerlX::Maybe;

our %SPEC;

our %actions = (
    'actions' => {summary=>'List available actions', req_input=>0},
    'as-aoaos' => {summary=>'Convert table data to aoaos form'},
    'as-aohos' => {summary=>'Convert table data to aohos form'},
    'as-csv' => {summary=>'Convert table data to CSV'},
    'avg-row' => {summary=>'Append an average row'},
    'avg' => {summary=>'Return average of all numeric columns'},
    'colcount-row' => {summary=>'Append a row containing number of columns'},
    'colcount' => {summary=>'Count number of columns'},
    'colnames' => {summary=>'Return only the row containing column names'},
    'colnames-row' => {summary=>'Append a row containing column names'},
    'head' => {summary=>'Only return the first N rows'},
    'info' => {summary=>'Check if input is table data and show information about the table'},
    'rowcount-row' => {summary=>'Count number of rows (equivalent to "wc -l" in Unix)'},
    'rowcount' => {summary=>'Append a row containing rowcount'},
    'rownum-col' => {summary=>'Add a column containing row number'},
    'select' => {summary=>'Select one or more columns'},
    'shuf' => {summary=>'Generate random permutations of rows'},
    'sort' => {summary=>'Sort rows'},
    'sum-row' => {summary=>'Append a row containing sums'},
    'sum' => {summary=>'Return a row containing sum of all numeric columns'},
    'tail' => {summary=>'Only return the last N rows'},
    'transpose' => {summary=>'Transpose table'},
    'wc-row' => {summary=>'Alias for rowcount-row'},
    'wc' => {summary=>'Alias for rowcount'},

    'grep' => {summary=>'Use Perl code to filter rows', tags=>['perl']},
    'map' => {summary=>'Use Perl code to transform row', tags=>['perl']},
    'psort' => {summary=>'Use Perl code to transform row', tags=>['perl']},
);

sub _get_table_spec_from_envres {
    my $envres = shift;
    my $tf  = $envres->[3]{'table.fields'};
    my $tfa = $envres->[3]{'table.field_aligns'};
    my $tff = $envres->[3]{'table.field_formats'};
    my $tfu = $envres->[3]{'table.field_units'};
    return undef unless $tf;
    my $spec = {fields=>{}};
    my $i = 0;
    for (@$tf) {
        $spec->{fields}{$_} = {
            pos=>$i,
            maybe _align  => $tfa->[$i],
            maybe _format => $tff->[$i],
            maybe _unit   => $tfu->[$i],
        };
        $i++;
    }
    $spec;
}

sub _decode_json {
    require Cpanel::JSON::XS;

    state $json = Cpanel::JSON::XS->new->allow_nonref;
    $json->decode(shift);
}

sub _get_td_obj {
    require Data::Check::Structure;

    my $input = shift;
    my ($input_form, $input_obj);
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
    }
    ($input_form, $input_obj);
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
column is `elem`), `aoaos` (array of arrays of scalars), or `aohos` (array of
hashes of scalars).

The input can also be an _enveloped_ table data, where the envelope is an array:
`[status, message, content, meta]` and `content` is the actual table data. This
kind of data is produced by `Perinci::CmdLine`-based scripts and can contain
more detailed table specification in the `meta` hash, which `td` can parse.

First you might want to use the `info` action to see if the input is a table
data:

    % osnames -l --json | td info

If input is not valid JSON, a JSON parse error will be displayed. If input is
valid JSON but not a table data, another error will be displayed. Otherwise,
information about the table will be displayed (form, number of columns, column
names, number of rows, and so on).

Next, you can use these actions:

 # List available actions
 % td actions

 # Convert table data (which might be hash, aos, or aohos) to aoaos form
 % list-files -l --json | td as-aoaos

 # Convert table data (which might be hash, aos, or aoaos) to aohos form
 % list-files -l --json | td as-aohos

 # Convert table data to CSV
 % list-files -l --json | td as-csv

 # Calculate arithmetic average of numeric columns
 % list-files -l --json | td avg

 # Append a row at the end containing arithmetic average of number columns
 % list-files -l --json | td avg-row

 # Count number of columns
 % osnames -l --json | td colcount

 # Append a single-column row at the end containing number of columns
 % osnames -l --json | td colcount-row

 # Return the column names only
 % lcpan related-mods Perinci::CmdLine | td colnames

 # append a row containing column names
 % lcpan related-mods Perinci::CmdLine | td colnames-row

 # Only show first 5 rows
 % osnames -l --json | td head -n5

 # Show all but the last 5 rows
 % osnames -l --json | td head -n -5

 # Check if input is table data and show information about the table
 % osnames -l --json | td info

 # Count number of rows
 % osnames -l --json | td rowcount
 % osnames -l --json | td wc            ;# shorter alias

 # Append a single-column row containing row count
 % osnames -l --json | td rowcount-row
 % osnames -l --json | td wc-row        ;# shorter alias

 # Add a row number column (1, 2, 3, ...)
 % list-files -l --json | td rownum-col

 # Select some columns
 % osnames -l --json | td select value description

 # Select all columns but some
 % osnames -l --json | td select '*' -e value -e description

 # Return the rows in a random order
 % osnames -l --json | td shuf

 # Pick 5 random rows from input
 % osnames -l --json | td shuf -n5

 # Sort by column(s) (add "-" prefix to for descending order)
 % osnames -l --json | td sort value tags
 % osnames -l --json | td sort -- -value

 # Return sum of all numeric columns
 % list-files -l --json | td sum

 # Append a sum row
 % list-files -l --json | td sum-row

 # Only show last 5 rows
 % osnames -l --json | td tail -n5

 # Show rows from the row 5 onwards
 % osnames -l --json | td tail -n +5

 # Transpose table (make first column of rows as column names in the transposed
 # table)
 % osnames -l --json | td transpose
 # Transpose table (make columns named 'row1', 'row2', 'row3', ... in the
 # transposed table)
 % osnames -l --json | td transpose --no-header-column

 # Use Perl code to filter rows. Perl code gets row in $ROW or $_
 # (scalar/aos/hos) or $ROW_HOS (always a hos) or $ROW_AOS (always aos). There
 # is also $ROW_NUM (integer, starts at 0). Perl code is eval'ed in the 'main'
 # package with strict/warnings turned off. The example below selects videos
 # that are larger than 480p.
 % media-info *.mp4 | td grep 'use List::Util qw(min); min($_->{video_height}, $_->{video_width}) > 480'

 # Use Perl code to transform row. Perl code gets row in $ROW or $_
 # (scalar/hash/array) and is supposed to return the new row. As in 'grep',
 # $ROW_HOS, $ROW_AOS, $ROW_NUM are also available as helper. The example below
 # adds a field called 'is_landscape'.
 % media-info *.jpg | td map '$_->{is_landscape} = $_->{video_height} < $_->{video_width} ? 1:0; $_'

 # Use perl code to sort rows. Perl sorter code gets row in $a & $b or $_[0] &
 # $_[1] (hash/array). Sorter code, like in Perl's standard sort(), is expected
 # to return -1/0/1. The example belows sort videos by height, descendingly then
 # by width, descendingly.
 % media-info *.mp4 | td psort '$b->{video_height} <=> $a->{video_height} || $b->{video_width} <=> $b->{video_width}'

_
    args => {
        action => {
            summary => 'Action to perform on input table',
            schema => ['str*', in => [sort keys %actions]],
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

        lines => {
            schema => ['str*', match=>qr/\A[+-]?[0-9]+\z/],
            cmdline_aliases => {n=>{}},
            tags => ['category:head-action', 'category:tail-action'],
        },

        detail => {
            schema => 'bool*',
            cmdline_aliases => {l=>{}},
            tags => ['category:actions-action'],
        },

        repeat => {
            summary => 'Allow duplicates',
            schema => 'bool*',
            cmdline_aliases => {r=>{}},
            tags => ['category:shuf-action'],
        },

        exclude_columns => {
            'x.name.is_plural' => 1,
            'x.name.singular' => 'exclude_column',
            schema => ['array*', of=>'str*'],
            cmdline_aliases => {e=>{}},
            tags => ['category:select-action'],
        },

        no_header_column => {
            summary => "Don't make the first column as column names of the transposed table; ".
                "instead create column named 'row1', 'row2', ...",
            schema => 'true*',
            tags => ['category:transpose-action'],
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
        last unless $actions{$action}{req_input} // 1;
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
        ($input_form, $input_obj) = _get_td_obj($input);
        return [400, "Input is not table data, please feed a hash/aos/aoaos/aohos"]
            unless $input_form;
    } # GET_INPUT

    my $output;
  PROCESS:
    {
        if ($action eq 'actions') {
            if ($args{detail}) {
                $output = [200, "OK", [map {+{name=>$_, summary=>$actions{$_}{summary}}} sort keys %actions]];
            } else {
                $output = [200, "OK", [sort keys %actions]];
            }
            last;
        }

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

        if ($action eq 'rowcount' || $action eq 'wc') {
            $output = [200, "OK", $input_obj->row_count];
            last;
        }

        if ($action eq 'as-aoaos') {
            my $cols = $input_obj->cols_by_idx;
            my $rows = $input_obj->rows_as_aoaos;
            $output = [200, "OK", $rows, {'table.fields' => $cols}];
            last;
        }

        if ($action eq 'as-csv') {
            require Text::CSV_XS;
            my $csv = Text::CSV_XS->new({binary=>1}) or die "Can't instantiate CSV parser";
            my $cols = $input_obj->cols_by_idx;
            my $res = "";
            $csv->combine(@$cols) or die "Can't combine header row to CSV: ".join(", ", @$cols);
            $res .= $csv->string . "\n";
            for my $row (@{ $input_obj->rows_as_aoaos }) {
                $csv->combine(@$row) or die "Can't combine data row to CSV: ".join(", ", @$row);
                $res .= $csv->string . "\n";
            }
            $output = [200, "OK", $res, {'cmdline.skip_format' => 1}];
            last;
        }

        if ($action eq 'as-aohos') {
            my $cols = $input_obj->cols_by_idx;
            my $rows = $input_obj->rows_as_aohos;
            $output = [200, "OK", $rows, {'table.fields' => $cols}];
            last;
        }

        if ($action eq 'rowcount-row' || $action eq 'wc-row') {
            my $cols = $input_obj->cols_by_idx;
            my $rows = $input_obj->rows;
            my $rowcount = $input_obj->row_count;
            my $rowcount_row;
            if (@$rows && ref $rows->[0] eq 'HASH') {
                $rowcount_row = {rowcount=>$rowcount};
            } elsif (@$rows && ref $rows->[0] eq 'ARRAY') {
                $rowcount_row = [$rowcount];
            } else {
                $rowcount_row = $rowcount;
            }
            $output = [200, "OK", [@$rows, $rowcount_row],
                       {'table.fields' => $cols}];
            last;
        }

        if ($action eq 'head' || $action eq 'tail') {
            my $cols = $input_obj->cols_by_idx;
            my $rows = $input_obj->rows;
            my $n = $args{lines} // 5;
            if ($action eq 'head') {
                if ($n =~ s/\A\+//) {
                    if ($n >= @$rows) {
                        $rows = [];
                    } else {
                        splice @$rows, @$rows - $n;
                    }
                } else {
                    splice @$rows, $n if $n < @$rows;
                }
            } else {
                if ($n < 0) {
                    $output = [400, "Cannot tail negative number of rows"];
                    last;
                } elsif ($n =~ s/\A\+//) {
                    splice @$rows, 0, $n-1 if $n >= 1;
                } else {
                    splice @$rows, 0, @$rows - $n if $n < @$rows;
                }
            }
            $output = [200, "OK", $rows, {'table.fields' => $cols}];
            last;
        }

        if ($action eq 'colcount') {
            $output = [200, "OK", $input_obj->col_count];
            last;
        }

        if ($action eq 'colnames') {
            my $cols = $input_obj->cols_by_idx;
            my $colnames_row = [map {$cols->[$_]} 0..$#{$cols}];
            $output = [200, "OK", $colnames_row];
            last;
        }

        if ($action eq 'colnames-row') {
            my $cols = $input_obj->cols_by_idx;
            my $rows = $input_obj->rows;
            my $colnames_row;
            if (@$rows && ref $rows->[0] eq 'HASH') {
                $colnames_row = $input_obj->cols_by_name;
            } elsif (@$rows && ref $rows->[0] eq 'ARRAY') {
                $colnames_row = [map {$cols->[$_]} 0..$#{$cols}];
            } else {
                $colnames_row = $cols->[0];
            }
            $output = [200, "OK", [@$rows, $colnames_row],
                       {'table.fields' => $cols}];
            last;
        }

        if ($action eq 'rownum-col') {
            $input_obj->add_col('_rownum', 0); # XXX if table already has that column, use _rownum2, ...
            $input_obj->set_col_val(_rownum => sub { my %a = @_; $a{row_idx}+1 });
            $output = [200, "OK", $input_obj->{data},
                       {'table.fields' => $input_obj->{cols_by_idx}}];
            last;
        }

        if ($action =~ /\A(sum|sum-row|avg|avg-row)\z/) {
            require Scalar::Util;
            my $cols = $input_obj->cols_by_idx;
            my $rows = $input_obj->rows;
            # XXX optimize by not producing two versions of rows
            my $rows_as_aoaos = $input_obj->rows_as_aoaos;
            my $sums = [map {0} @$cols];
            for my $i (0..$#{$rows_as_aoaos}) {
                my $row = $rows_as_aoaos->[$i];
                for my $j (0..@$cols-1) {
                    $sums->[$j] += $row->[$j]
                        if Scalar::Util::looks_like_number($row->[$j]);
                }
            }
            my $avgs;
            my $results;
            if ($action =~ /avg/) {
                if (@$rows) {
                    $avgs = [map { $_ / @$rows } @$sums];
                } else {
                    $avgs = [map {0} @$cols];
                }
                $results = $avgs;
            } else {
                $results = $sums;
            }

            my $result_row;
            if (@$rows && ref $rows->[0] eq 'HASH') {
                $result_row = {map {$cols->[$_] => $results->[$_]}
                                   0..$#{$cols}};
            } elsif (@$rows && ref $rows->[0] eq 'ARRAY') {
                $result_row = $results;
            } else {
                $result_row = $results->[0];
            }

            if ($action =~ /-row/) {
                $output = [200, "OK", [@$rows, $result_row],
                           {'table.fields' => $cols}];
            } else {
                $output = [200, "OK", $result_row,
                           {'table.fields' => $cols}];
            }
            last;
        }

        if ($action eq 'shuf') {
            my $cols = $input_obj->cols_by_idx;
            my $input_rows = $input_obj->rows;
            my @output_rows;
            if ($args{repeat}) {
                for my $i (1 .. ($args{lines} // scalar(@$input_rows))) {
                    $output_rows[$i-1] = $input_rows->[rand() * @$input_rows];
                }
            } else {
                require List::MoreUtils;
                @output_rows = List::MoreUtils::samples(
                    $args{lines} // scalar(@$input_rows),
                    @$input_rows);
            }
            $output = [200, "OK", \@output_rows, {'table.fields' => $cols}];
            last;
        }

        if ($action =~ /\A(sort|select)\z/) {
            return [400, "Please specify one or more columns"] unless @$argv;
            my $res;
            if ($action eq 'sort') {
                if ($input_form eq 'aohos') {
                    $res = $input_obj->select_as_aohos(undef, undef, undef, $argv);
                } else {
                    $res = $input_obj->select_as_aoaos(undef, undef, undef, $argv);
                }
            } elsif ($action eq 'select') {
                my $excl_cols = $args{exclude_columns} // [];
                if ($input_form eq 'aohos') {
                    $res = $input_obj->select_as_aohos($argv, $excl_cols);
                } else {
                    $res = $input_obj->select_as_aoaos($argv, $excl_cols);
                }
            }

            my $resmeta = {};
            {
                my $ff  = $res->{spec}{fields} or last;
                my $tf  = [];
                my $tfa = [];
                my $tff = [];
                my $tfu = [];
                for (keys %$ff) {
                    my $f = $ff->{$_};
                    my $i = $f->{pos};
                    $tf ->[$i] = $_;
                    $tfa->[$i] = $f->{_align};
                    $tff->[$i] = $f->{_format};
                    $tfu->[$i] = $f->{_unit};
               }
                $resmeta->{'table.fields'}        = $tf;
                $resmeta->{'table.field_aligns'}  = $tfa;
                $resmeta->{'table.field_formats'} = $tff;
                $resmeta->{'table.field_units'}   = $tfu;
            }
            $output = [200, "OK", $res->{data}, $resmeta];
            last;
        }

        if ($action =~ /\A(grep|map|psort)\z/) {
            return [400, "Usage: td $action <perl-code>"] unless @$argv == 1;
            my $code_str;
            if ($action eq 'psort') {
                $code_str = join(
                    "",
                    "package main; no strict; no warnings; sub {",
                    ' my ($a, $b) = @_;',
                    " $argv->[0] }",
                );
            } else {
                $code_str = "package main; no strict; no warnings; sub { $argv->[0] }";
            }
            my $code = eval $code_str; die if $@;

            my $input_rows     = $input_obj->rows;
            my $input_rows_aos = $input_obj->rows_as_aoaos;
            my $input_rows_hos = $input_obj->rows_as_aohos;

            my $output_rows;
            if ($action eq 'grep' || $action eq 'map') {
                for my $row_num (0 .. $#{ $input_rows }) {
                    my $code_res;
                    {
                        no warnings 'once';
                        local $main::ROW_NUM = $row_num;
                        local $_             = $input_rows->[$row_num];
                        local $main::ROW     = $input_rows->[$row_num];
                        local $main::ROW_AOS = $input_rows_aos->[$row_num];
                        local $main::ROW_HOS = $input_rows_hos->[$row_num];
                        $code_res = $code->($_);
                    }
                    if ($action eq 'grep') {
                        push @$output_rows, $input_rows->[$row_num] if $code_res;
                    } else { # map
                        push @$output_rows, $code_res;
                    }
                }
            } else { # psort
                $output_rows = [sort { $code->($a, $b) } @$input_rows];
            }

            $output = [200, "OK", $output_rows, $input->[3]];
            last;
        }

        if ($action eq 'transpose') {
            my $input_rows = $input_obj->rows_as_aoaos;
            my $input_cols = $input_obj->cols_by_idx;

            my @output_cols;
            if ($args{no_header_column} || !@$input_rows) {
                @output_cols = map {"row$_"} 1 .. @$input_rows;
            } else {
                @output_cols = map { $input_rows->[$_-1][0] } 1 .. @$input_rows;
            }

            my @output_rows;

            for my $inputrowidx (0..$#{ $input_rows }) {
                my $inputrow = $input_rows->[$inputrowidx];
                for my $inputcolidx (0..$#{ $input_cols }) {
                    $output_rows[$inputcolidx] //= [];
                    $output_rows[$inputcolidx][$inputrowidx] = $inputrow->[$inputcolidx];
                }
            }

            $output = [200, "OK", \@output_rows, {'table.fields'=>\@output_cols}];
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
