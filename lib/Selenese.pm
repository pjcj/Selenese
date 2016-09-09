package Selenese;

# Copyright 2016, Paul Johnson (paul@pjcj.net) http://www.pjcj.net

# This software is free.  It is licensed under the same terms as Perl itself.

use 5.18.2;
use warnings;
use utf8::all;

use Data::Dumper;
use Encode;
use JavaScript::V8;
use Test::More;
use Time::HiRes qw( usleep );
use Unicode::Normalize qw( NFC NFD NFKC NFKD );

$Data::Dumper::Indent   = 1;
$Data::Dumper::Purity   = 1;
$Data::Dumper::Sortkeys = 1;
$Data::Dumper::Deparse  = 1;

sub new {
    my $class = shift;
    my $self = bless { @_ }, $class;
    $self->init;
    $self
}

sub verbose       { shift->{verbose}                             }
sub driver        { shift->{driver} // die "No driver specified" }
sub d             { shift->{d}                                   }
sub error_capture { shift->{error_capture}                       }
sub error_html    { shift->{error_html}                          }
sub debug {
    my $self = shift;
    $self->{debug_sub} //= sub { say "@_" if $self->{debug} };
}
sub info {
    my $self = shift;
    $self->{info_sub} //= sub { say "@_" };
}

sub parse_loc {
    my $self = shift;
    my ($loc) = @_;
    $loc =~ /^(\w+)=(.*)/ ? ($1, $2) : ()
}

sub parse_loc_with_default {
    my $self = shift;
    my ($loc) = @_;
    $loc =~ /^(\w+)=(.*)/ ? ($1, $2) : ("xpath", $loc)
}

sub text_to_re {
    my $self = shift;
    my ($text) = @_;
    $text =~ /^regexp:(.*)/ ? qr/$1/ : qr/\Q$text/
}

sub init {
    my $self = shift;

    my $vars = $self->{vars} //= {};

    my $dvr = "Selenium::" . $self->driver;
    eval "require $dvr";
    my $opts = {
        accept_ssl_certs => 1,
        %{$self->{driver_options} // {}}
    };
    say "Using $dvr" if $self->verbose;
    my $d = $self->{d} = $dvr->new(%$opts);

    $self->{init}->($d) if $self->{init};

    my $v8 = $self->{v8} = JavaScript::V8::Context->new();
    $v8->bind_function(say => sub { say "js: ", Dumper(@_) if $self->verbose });

    $v8->eval("var storedVars = {}");
    for my $var (keys %$vars) {
        my $js = "storedVars['$var'] = '$vars->{$var}'";
        $v8->eval($js);
    }
    $v8->eval("say(storedVars)");

    my $cmds = $self->{cmds} = {
        storeEval => sub {
            my ($expr, $key) = @_;
            return if $expr =~ /^prompt\(/;
            $vars->{$key} = $v8->eval("storedVars['$key'] = $expr");
            note "Setting $key to $vars->{$key} from $expr" if $self->verbose;
        },
        _eval => sub {
            my ($script) = @_;
            my $res = $v8->eval($script);
            note "Eval of [$script] => [" . ($res // "*undef*") . "]"
                if $self->verbose;
            $res
        },
        getEval => sub {
            my ($script) = @_;
            $self->{cmds}{_eval}->($script);
        },
        storeText => sub {
            my ($loc, $key) = @_;
            $vars->{$key} = $d->get_text($loc);
            note "Setting $key to $vars->{$key} from $loc" if $self->verbose;
        },
        open => sub {
            my ($loc) = @_;
            if (my ($scheme, $target) = $self->parse_loc($loc)) {
                die "Can't handle", $loc;
            } else {
                $d->get($loc);
            }
        },
        click => sub {
            my ($loc) = @_;
            if (my ($scheme, $target) = $self->parse_loc_with_default($loc)) {
                $d->find_element($target, $scheme)->click;
            } else {
                die "Can't handle", $loc;
            }
        },
        type => sub {
            my ($loc, $text) = @_;
            if (my ($scheme, $target) = $self->parse_loc_with_default($loc)) {
                my $e = $d->find_element($target, $scheme);
                $e->clear;
                $e->send_keys($text);
            } else {
                die "Can't handle", $loc;
            }
        },
        select => sub {
            my ($select, $option) = @_;
            my ($sid, $s) = $self->parse_loc_with_default($select);
            my ($oid, $o) = $self->parse_loc_with_default($option);
            die "Can't handle", $select, $option
                unless grep defined, $sid, $s, $oid, $o;
            my $opt = ($oid eq "label" ? "normalize-space(.)" : "\@$oid");
            $self->debug->(
                select => [$select, $option, $sid, $s, $oid, $o, $opt]);
            if (my ($re) = $o =~ /^regexp:(.*)/) {
                $re          = qr|$re|;
                my $f        = "//select[\@$sid='$s']";
                $o           = "";
                my $sel      = $d->find_element($f);
                my @children = $d->find_child_elements($sel, "./option");
                $self->debug->(children => $f, \@children);
                GET: for my $get (qw( get_tag_name get_value )) {
                    for my $e (@children) {
                        my $val = ($oid eq "label") ? $e->get_text
                                                    : $e->$get;
                        $self->debug->(select => [$get, $e, $val, $re]);
                        next unless $val =~ /$re/;
                        $o = $val;
                        s/^\s+//, s/\s+$//, s/\s+/ / for $o;
                        last GET;
                    }
                }
                die "Can't find element matching", "[$re]" unless $o;
            }
            my $f;
            if ($oid eq "index") {
                $o++;  # XPath indices start at 1!
                $f = "(//select[\@$sid='$s']/option)[$o]";
            } else {
                $opt .= "='$o'";
                $f = "//select[\@$sid='$s']/option[$opt]";
            }
            $self->debug->(find => $f);
            my $e = $d->find_element($f);
            $e->click;
        },
        setTimeout => sub {
            my ($ms) = @_;
            $ms /= 10;  # hmmm
            $d->set_timeout("script",    $ms);
            $d->set_timeout("implicit",  $ms);
            $d->set_timeout("page load", $ms);
        },
        verifyTextPresent => sub {
            my ($text) = @_;
            my $re     = $self->text_to_re($text);
            my $body   = $d->get_body;
            $body =~ $re
        },
        verifyTextNotPresent => sub {
            !$self->{cmds}{verifyTextPresent}->(@_)
        },
        verifyText => sub {
            my ($select, $text) = @_;
            $self->debug->(verifyText => $select, $text);
            my ($sid, $s) = $self->parse_loc_with_default($select);
            die "Can't handle", $select, $text unless grep defined, $sid, $s;
            my $re  = $self->text_to_re($text);
            my $sel = $d->find_element($s);
            my $txt = $sel->get_text;
            $self->debug->("verifyText match" => $txt, $re);
            $txt =~ $re
        },
        pause => sub {
            my ($duration) = @_;
            usleep($duration * 1000);
        },
        deleteAllVisibleCookies => sub {
            $d->delete_all_cookies;
        },
        selectWindow => sub {
        },
        quit => sub {
            $d->quit;
            system "pgrep phantomjs 2> /dev/null | xargs kill 2> /dev/null";
        },
    };

    my $equalities = {
        clickAndWait      => "click",
        selectAndWait     => "select",
        addSelection      => "select",
        verifyBodyText    => "verifyTextPresent",
        verifyNotBodyText => "verifyTextNotPresent",
    };
    while (my ($to, $from) = each %$equalities) {
        $cmds->{$to} = $cmds->{$from};
    }
}

sub capture {
    my $self = shift;
    my ($filename) = @_;
    die "No filename for capture" unless $filename;  # no, don't use "0"!
    $self->d->maximize_window;
    $self->d->capture_screenshot($filename);
    say "Screen capture written to $filename";
}

sub write_page_source {
    my $self = shift;
    my ($filename) = @_;
    die "No filename for capture" unless $filename;  # no, don't use "0"!
    my $out = $self->d->get_page_source;
    open my $fh, ">", $filename or die "Can't open $filename: $!";
    binmode $fh, ":encoding(UTF-8)";
    print $fh $out;
    close $fh or die "Can't close $filename: $!";
    say "Page source written to $filename";
}

sub run_command {
    my $self = shift;
    my ($cmd, @params) = @_;

    say "command: $cmd(", join (", ", @params), ")" if $self->verbose;
    my $sub = $self->{cmds}{$cmd} or die "Can't handle $cmd @params";
    s/\$\{(\w+)\}/$self->{vars}{$1}/g for @params;
    my $ret = eval { $sub->(@params) };
    my $err = $@;
    ok !$err, "$cmd @params";
    if ($err) {
        $self->capture($self->error_capture)        if $self->error_capture;
        $self->write_page_source($self->error_html) if $self->error_html;
        $self->{cmds}{quit}->();
        $self->info->("Failed: $cmd(", join (", ", @params), ") -> $err");
        return 1;
    }

    $self->{post_sub}->(
        selenese => $self,
        ret      => $ret,
        err      => $err,
        cmd      => $cmd,
        params   => \@params,
    ) if $self->{post_sub};

    0
}

"
But tonight we'll be free
All the promises'll be broken
"

__END__

=head1 NAME

Selenese - Run Selenium IDE tests without the IDE

=head1 SYNOPSIS

 selenese

=head1 DESCRIPTION

This module allows you to run Selenium IDE tests without the IDE.

This makes your Selenium IDE tests useful for general testing purposes.  For
example, they could be used in a headless Continuous Integration environment.

=head1 REQUIREMENTS

=over

=item * Perl 5.18.2 or greater

This is probably not a hard requirement, but I've not thought much about earlier
versions.

=item * V8

On debian and ubuntu this requirement can be satisfied by installing the module
libv8-dev.

=back

=head1 OPTIONS

=head1 ENVIRONMENT

=head1 SEE ALSO

=head1 BUGS

=head1 LICENCE

Copyright 2016, Paul Johnson (paul@pjcj.net) http://www.pjcj.net

This software is free.  It is licensed under the same terms as Perl itself.

=cut
