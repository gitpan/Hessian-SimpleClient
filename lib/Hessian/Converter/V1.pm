package Hessian::Converter::V1;

use warnings;
use strict;

use base qw(Hessian::Converter);

use DateTime;
use Unicode::String;
use Math::BigInt;

use Hessian::Converter;
use Hessian::Type;

my $UB32 = Math::BigInt->new(0x7fffffff);
my $LB32 = Math::BigInt->new(-0x80000000);

=head1 NAME

Hessian::Converter::V1 - The great new Hessian::Converter::V1!

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';


=head1 SYNOPSIS

Quick summary of what the module does.

Perhaps a little code snippet.

    use Hessian::Converter::V1;

    my $foo = Hessian::Converter::V1->new();
    ...

=head1 EXPORT

A list of functions that can be exported.  You can delete this section
if you don't export anything, such as for a purely object-oriented module.

=head1 FUNCTIONS

=head2 writeCall

=cut

sub writeCall {
    my ($self, $method, @params) = @_;

    $method = $self->mangle($method,@params) if $self->{mangle}; # not working well (nor clear in specs), so don't use

    my $x = join '', 'c'
    . pack('C2', 1, 0), # prefix for Hessian 1.0
    . 'm'
    . pack("n", length($method)) # length of method name (big endian)
    . $method
    . join('', map($self->typeformat($_),@params))
    . 'z';

    $self->{out} = $x; # save
#    $self->debug($self->bin_debug($x));
    return $x;
}


=head2 typeformat

=cut

sub typeformat {
    my($self, $x) = @_;
    return 'N' unless defined $x; # Hessian::Null
    push @{$self->{outRefs}}, $x if ref($x) =~ /^(ARRAY|HASH|Hessian::List|Hessian::Map)$/;
    for(ref $x){
        /^Hessian::Null/        && return 'N';
        /^Hessian::True/        && return 'T';
        /^Hessian::False/       && return 'F';
        /^DateTime$/            && do { # DateTime Object
                                    $x = Math::BigInt->new($x->epoch());
                                    $x->bmul(1000); # milliseconds
                                    return 'd' . $self->pack_signed_int($x,8);
                                };
        /^Math::BigInt/         && return 'L' . $self->pack_signed_int($x,8);
        /^Hessian::Double/      && do {
                                    my $rs = pack 'd', $x->{data};
                                    use Config;
                                    if($Config{'byteorder'} =~ /^1234/){ # little-endian
                                        $rs = join('',reverse(split//,$rs));
                                    } 
                                    return "D$rs";
                                };
        /^Unicode::String/      && return $self->writeChunks('s',$x);
        /^Hessian::Binary/      && return $self->writeChunks('b',$x->{data});
        /^Hessian::XML/         && return $self->writeChunks('x',$x->{data});
        /^Hessian::List/        && do {
                                    my $ar = $x->{data};
                                    return 'V'
                                    . ($x->{type} ? ('t' . pack('n',length $x->{type}) . $x->{type}) : '')
                                    . (defined $x->{len} ? ('l' . pack('N',$x->{len})) : '')
                                    . join('', map($self->typeformat($_), @$ar))
                                    . 'z';
                                };
        # untyped, fixed-length list, otherwise, use Hessian::List
        /^ARRAY$/               && return $self->typeformat(Hessian::List->new(len=>scalar(@$x),data=>$x));
        /^Hessian::Map/         && do {
                                    my @ar = 'HASH' eq ref $x->{data} ? %{$x->{data}} : @{$x->{data}};
                                    return 'M'
                                    . ($x->{type} ? ('t' . pack('n',length $x->{type}) . $x->{type}) : '')
                                    . join('', map($self->typeformat($_), @ar))
                                    . 'z';
                                };
        /^HASH$/                && do { # untyped map, otherwise, should use Hessian::Map
                                    #also perl hash key can only be perl string !
                                    my @ar;
                                    while(my($k,$v)=each(%$x)){
                                        push @ar, Unicode::String->new($k);
                                        push @ar, $v;
                                    }
                                    return 'M'
                                    . join('', map( $self->typeformat($_), @ar))
                                    . "z";
                                };
        /^REF$/                 && do { #perl ref
                                    my $ar = $self->{outRefs};
                                    if($#$ar >= 0){
                                        for(0 .. $#$ar){
                                            return 'R' . (pack 'N', unpack 'L', pack 'l', $_) if $$x == $ar->[$_];
                                        }
                                    }
                                    die 'Ref not found in argument list';
                                };
        /^Hessian::Header/      && do {
                                    return 'H'
                                    . pack('n',length $x->{name})
                                    . $x->{name}
                                    . $self->typeformat($x->{data});
                                };
        /^Hessian::Remote/      && do {
                                    return 'r'
                                    . ('t' . pack('n',length $x->{type}) . $x->{type})
                                    . ('S' . pack('n',length $x->{url}) . $x->{url});
                                };
        /^.+$/                  && die 'typeformat unknown type:', ref $x;
        /^$/                    && do { # primitives
                                    for($x){
                                        /^[\+-]?\d+$/ && do {  # signed int or long
                                                            $x = Math::BigInt->new($x);
                                                            return $self->is_between($x,$LB32,$UB32)
                                                            ? ('I' . pack('N', unpack 'L',(pack 'l',$x))) # 32-bit
                                                            : $self->typeformat(Math::BigInt->new($x)); # long
                                                        };
                                        /^[\+-]?(\d+\.|\.\d+)$/ && return $self->typeformat(Hessian::Double->new(data=>$x));
                                        m!<\?xml version="\d+" .*\?>! && return $self->writeChunks('x',Unicode::String->new($x));
                                        # treat everything else as a string
                                        return $self->writeChunks('s',Unicode::String->new($x));
                                    }
                                };
        # DEFAULT
        die "typeformat stuck with ($x):", ref $x;

    } # end of for-switch
}

=head2 writeChunks

=cut

sub writeChunks {
    my($self,$type,$str) = @_;
    $type = lc $type;

    $str = $str->as_string if 'Unicode::String' eq ref $str;
    if(length $str > 0x7fff){
        return $type
        . "\x7f\xff"
        . substr($str,0,0x7fff)
        . $self->writeChunks($type,substr($str,0x7fff));
    }else{
        $type = uc $type;
        return $type . pack('n',length($str)) . $str;
    }
}

=head2 parseResponse

=cut

sub parseResponse {
    my($self, $x) = @_;

    my $i=0;
    my $c = substr $x, $i,1;

    if($c eq 'r'){
        my($vmajor,$vminor) = unpack('C2',substr($x,$i+1,2));
        $self->debug("got \"$c\" with version $vmajor.$vminor");
        $i+=3;
    }else{
        die "reply not begin with r: $c";
    }

    $self->{in} = $x; # save
#    $self->debug($self->bin_debug($x));

    my($data,@list);
    while($i < length $x && 'z' ne substr $x,$i,1){
        ($data,$i) = $self->read_element($x,$i);
        push @list,$data;
    }
    die 'reply not end with z ',substr($x,$i,1) unless 'z' eq substr $x,$i,1;
    $self->{inList} = \@list;
    return $self->{inList}; # list should be: Header* (Fault | Object)
}

=head2 read_element

=cut

sub read_element {
    my($self,$x,$i) = @_;

    my $c = substr $x,$i,1;
    my $data;

    for($c){
        /^[NTFILdD]/ && return $self->read_primitive($x,$i);
        /^[SsXxBb]/  && return $self->read_chunks($x,$i);
        /^V/         && return $self->read_list($x,$i);
        /^M/         && return $self->read_map($x,$i);
        /^R/         && return $self->read_ref($x,$i);
        /^H/         && return $self->read_header($x,$i);
        /^r/         && return $self->read_remote($x,$i);
        /^f/         && return $self->read_fault($x,$i);
        /^z/         && die 'got z for element at ', $i;

        # default case error
        die "unrecognized element $c ($i) x",ord $c;
    }
}

=head2 read_primitive

=cut

sub read_primitive {
    my($self,$x,$i) = @_;
    for(substr $x,$i,1){
        /N/    && return Hessian::Null->new(), $i+1;
        /T/    && return Hessian::True->new(), $i+1;
        /F/    && return Hessian::False->new(), $i+1;
        /I/    && return unpack('l',pack 'L', unpack 'N', substr($x,$i+1,4)), $i+5;
        /[lR]/ && return unpack('N', substr($x,$i+1,4)), $i+5;
        /L/    && return $self->unpack_int($x,$i+1,8,'signed'), $i+9;
        /D/    && return Hessian::Double->new(data=>unpack('d',$self->h2n(substr $x,$i+1,8))), $i+9;
        /d/    && do { # date
                    my $v = $self->unpack_int($x,$i+1,8,'signed');
                    $v->bdiv(1000); # milli-senconds
                    my $d = DateTime->from_epoch(epoch=>$v->bstr(),time_zone=>'UTC');
                    return $d, $i+9;
                };

        # default
        die "not a primitive?($i): $_";
    } # end for
}

=head2 read_chunks

=cut

sub read_chunks {
    my($self,$x,$i) = @_;
    my($data,$type);
    do{
        $type = substr $x,$i,1;
        die "chunk not recognized($i)" unless $type =~ /^[SsXxBb]/;
        my $buf;
        ($buf,$i) = $self->read_string($x,$i+1);
        $data .= $buf;
    }while($type =~ /[sxb]/);

    $data = Unicode::String->new($data) if $type =~ /^[SX]/; # utf-8 for String/XML types
    $data = Hessian::XML->new(data=>$data) if $type =~ /^X/;
    $data = Hessian::Binary->new(data=>$data) if $type =~ /^B/;

    return $data, $i;
}

=head2 read_string

=cut

sub read_string { # get a string at pos. $i 
    my($self, $x, $i) = @_;
    my $len = unpack('n', substr($x,$i,2) );
    return substr($x, $i+2, $len), $i+$len+2;
}

=head2 read_map

=cut

sub read_map {
    my($self,$x,$i) = @_;

    my($type,$data,@list);
    my $thisMap = Hessian::Map->new();
    push @{$self->{inRefs}}, \$thisMap;
    $i++; #discard 'M'
    if('t' eq substr $x,$i,1) { # map with type
        ($type,$i) = $self->read_string($x,$i+1);
    }

    while('z' ne substr $x,$i,1 && $i < length $x){
        ($data,$i) = $self->read_element($x,$i); #hash key
        push @list,'Unicode::String' eq ref $data ? $data->as_string : $data; # for convenience
        ($data,$i) = $self->read_element($x,$i); #hash value
        push @list,$data;
    }
    die 'Map not end with z', substr($x,$i,1) unless 'z' eq substr $x,$i,1;
    $thisMap = Hessian::Map->new( type=> $type, data=> [@list] );
    return $thisMap, $i+1; # skip 'z'
}

=head2 read_list

=cut

sub read_list {
    my($self,$x,$i) = @_;

    my($data,@list,$len,$type);
    my $thisList = Hessian::List->new();
    push @{$self->{inRefs}}, \$thisList;
    $i++; #discard 'V'
    if('t' eq substr($x,$i,1) ) { # list with type
        ($type,$i) = $self->read_string($x,$i+1);
    }
    if('l' eq substr($x,$i,1) ){ # length of list
        ($len,$i) = $self->read_primitive($x,$i);
    }
    while('z' ne substr $x,$i,1 && $i < length $x){
        ($data,$i) = $self->read_element($x,$i);
        push @list,$data;
    }
    die 'List not end with z', substr($x,$i,1) unless 'z' eq substr $x,$i,1;
    $thisList = Hessian::List->new(type=>$type, len=>$len, data=>[@list] );
    return $thisList, $i+1; # skip 'z'
}

=head2 read_ref

=cut

sub read_ref {
    my($self,$x,$i) = @_;

    my($data);
    ($data,$i) = $self->read_primitive($x,$i);
    my $refref = $self->{inRefs};
    die "Ref out-of-bound: $data, $#$refref" if $data > $#$refref;
    return $refref->[$data], $i;
}

=head2 read_remote

=cut

sub read_remote {
    my($self,$x,$i) = @_;

    my($type,$url);
    die "Remote has no type" unless 't' eq substr $x,$i,$i+1;
    ($type,$i) = $self->read_string($x,$i+2);
    #followed by a string
    ($url,$i) = $self->read_primitive($x,$i);
    return Hessian::Remote->new(type => $type, url => $url->as_string), $i;
}

=head2 read_header

=cut

sub read_header {
    my($self,$x,$i) = @_;

    my($name,$data);
    ($name,$i) = $self->read_string($x,$i+1);
    ($data,$i) = $self->read_element($x,$i);
    my $hdr = Hessian::Header->new(name => $name, data => $data);
    push @{$self->{inHeaders}}, $hdr;
    return $hdr, $i;
}

=head2 read_fault

=cut

sub read_fault { # similiar to map, without type or Ref inside
    my($self,$x,$i) = @_;

    my($k,$data);
    my $h = {};
    $i++; # skip 'f'
    while($i < length $x && 'z' ne substr $x,$i,1){
        ($k,$i)    = $self->read_element($x,$i);
        ($data,$i) = $self->read_element($x,$i);
        $h->{$k} = $data;
    }
    die 'fault not end with z', substr($x,$i,1) unless 'z' eq substr $x,$i,1;
    return Hessian::Fault->new( %$h ), $i+1;
}

=head1 AUTHOR

du ling, C<< <ling.du at gmail.com> >>

=head1 BUGS

Please report any bugs or feature requests to C<bug-hessian-translator-v1 at rt.cpan.org>, or through
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

1; # End of Hessian::Converter::V1
