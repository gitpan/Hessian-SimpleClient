package Hessian::Converter;

use warnings;
use strict;

use fields qw(
        out in outRefs inRefs inList inHeaders
        outClassRefs inClassRefs inTypeRefs
);
use Math::BigInt;

my $UB32 = Math::BigInt->new(0x7fffffff);
my $LB32 = Math::BigInt->new(-0x80000000);

=head1 NAME

Hessian::Converter - The great new Hessian::Converter!

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';



=head1 FUNCTIONS

=head2 new

=cut

sub new {
    my($class,@params) = @_;

    my %h = @params;
    my $self = \%h;

    #these will be used
    $self->{out} = '';            # call string
    $self->{in} = '';             # reply string
    $self->{outRefs} = [];   # ref list for writing call
    $self->{inRefs} = [];    # ref list for parsing reply

    $self->{inList} = [];    # reply parsed into perl structure
    $self->{inHeaders} = []; # reply headers, if used

    # Hessian 2.0 use only
    $self->{outClassRefs} = []; # class def, Hessian 2.0
    $self->{inClassRefs} = []; # class def, Hessian 2.0
    $self->{inTypeRefs} = []; # type ref, for list/map

    bless $self,$class;
    return $self;
}

=head2 pack_signed_int

=cut

sub pack_signed_int {
    my($self,$x,$n) = @_; #$n = bytes
    $n *= 2; # nybbles = 4-bit
    $x = Math::BigInt->new($x) unless ref($x) eq 'Math::BigInt';
    my $y = $x->as_hex();
    if($x->is_neg()){ #two's complement ...
        my $u = Math::BigInt->new('0x'.'f'x$n);
        $y = $x->bneg()->bxor($u)->binc()->as_hex();
    }
    $y = sprintf "%0${n}s",substr $y,2; #remove leading '0x' and pad with 0
    $y =~ s/([0-9a-f]{2})/pack('H2',$1)/gei;
    return $y;
}

=head2 unpack_int

=cut

sub unpack_int {
    my($self,$x,$i,$len,$signed) = @_; #len in bytes
    my $n = $len * 2;
    $x = substr $x,$i,$len;
    $x =~ s/(.)/sprintf('%02x',ord $1)/ge;
    $x = Math::BigInt->new("0x$x");

    my $y = Math::BigInt->new('0x7'.'f'x($n-1));
    if($signed && $signed !~ /^unsigned$/i && 0 < $x->bcmp($y)){ #signed & negative
        my $u = Math::BigInt->new('0x1'.'0'x$n);
        $x->bsub($u);
    }
    return $x;
}

=head2 is_between

=cut

sub is_between {
    my($self,$x,$l,$u) = @_;
    return $x >= $l && $x <= $u unless ref $x || ref $l || ref $u;
    $x = Math::BigInt->new($x) unless ref $x;
    $l = Math::BigInt->new($l) unless ref $l;
    $u = Math::BigInt->new($u) unless ref $u;
    return 0 <= $x->bcmp($l) && 0 >= $x->bcmp($u) if 'Math::BigInt' eq ref $x;
}

=head2 h2n

=cut

sub h2n { # host 2 network byte order, or n2h
    my($self,$bytes) = @_;
    use Config;
    return $Config{'byteorder'} =~ /^1234/ # little-endian
    ? scalar reverse $bytes
    : $bytes;
}

=head2 bin_debug

=cut

sub bin_debug {
    my($self, $octets) = @_;

    my($cnt,$out) = (1,'');
    for(split//,$octets){
        if( $_ =~ /^[a-zA-Z0-9]/ ){ #newer perl would take /\p{IsGraph}/ ...
            $out .= "$_ ";
        }elsif( $_ =~ /^[\`\~\!\@\#\$\%\^\&\*\(\)\-\_\=\+\[\{\]\}\;\:\'\"\,\<\.\>\/\?]/ ){ #newer perl would take /\p{IsGraph}/ ...
            $out .= "'$_' ";
        }else{
            $out .= sprintf 'x%02x ', ord($_);
        }
        $out .= "\n" if 0 == $cnt++ % 30;
    }
    $out .= "\n";
}

=head2 debug

=cut

sub debug {
    my($self, @msgs) = @_;
    print STDERR "@msgs\n" if $self->{debug};
}

=head2 dump_fault

=cut

sub dump_fault {
    my($self,$f) = @_;
    return unless 'Hessian::Fault' eq ref $f;
    my $stack = join "\n", map{
        join ' ',$_->{__type},$_->{lineNumber},$_->{fileName},"$_->{methodName}()",$_->{declaringClass}
    } grep{'HASH' eq ref $_}@{$f->{detail}->{stackTrace}};
    my $reply = <<EOFAULT;
Hessian::Fault code: $f->{code}
  message: $f->{message}
  detail: $f->{detail}->{__type}: $f->{detail}->{detailMessage}
  $stack
EOFAULT
    print STDERR Data::Dumper::Dumper($reply);
    return $reply;
}

=head2 mangle

=cut

sub mangle {
    my($self,$method,@params) = @_;
    my @pnames = map $self->type_of($_), @params;
    return join '_', $method, @pnames;
}

=head2 type_of

=cut

sub type_of {
    my($self,$x) = @_;
    for(ref $x){
        /^Hessian::Null/        && return 'null';
        /^Hessian::Double/      && return 'double';
        /^Hessian::Long/        && return 'long';
        /^DateTime/             && return 'date';
        /^Unicode::String/      && return 'string';
        /^Hessian::Binary/      && return 'binary';
        /^Hessian::XML/         && return 'xml'; # hessian 1.0 only ?
        /^Hessian::(True|False)/        && return 'boolean';
        /^(Hessian::List|ARRAY)/        && return $x->{type} ? $x->{type} : 'list';
        /^(Hessian::Map|HASH)$/ && return $x->{type} ? $x->{type} : 'map';
        /^Hessian::Remote/      && return 'remote';
        /^REF$/                 && return $self->type_of($$x); # perl ref
        /^$/                    && do {
                                    for($x){
                                        /^[\+-]?\d+$/
                                        && do { # int or long
                                            $x = Math::BigInt->new($x);
                                            return $self->is_between($x,$LB32,$UB32) ? 'int' : 'long';
                                        };
                                        /^[\+-]?(\d+\.|\.\d+)$/ && return 'double';
                                        m!<\?xml version="\d+" .*\?>! && return 'xml';
                                        # default
                                        return 'string';
                                    } # end for $x
                                };
        # DEFAULT
        #die "unkown type_of $x: ", ref $x;
        return 'Object'; # ??
    } # end for ref $x
}

=head1 AUTHOR

du ling, C<< <ling.du at gmail.com> >>

=head1 BUGS

Please report any bugs or feature requests to C<bug-hessian-translator at rt.cpan.org>, or through
the web interface at L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Hessian-SimpleClient>.  I will be notified, and then you'll
automatically be notified of progress on your bug as I make changes.




=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Hessian::SimpleClient


You can also look for information at:

=over 4

=item * RT: CPAN's request tracker

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Hessian-SimpleClient>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Hessian-SimpleClient>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/Hessian-SimpleClient>

=item * Search CPAN

L<http://search.cpan.org/dist/Hessian-SimpleClient>

=back


=head1 ACKNOWLEDGEMENTS


=head1 COPYRIGHT & LICENSE

Copyright 2009 du ling, all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.


=cut

1; # End of Hessian::Converter
