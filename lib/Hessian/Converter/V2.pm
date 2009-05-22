package Hessian::Converter::V2;

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

Hessian::Converter::V2 - The great new Hessian::Converter::V2!

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';


=head1 FUNCTIONS

=head2 writeCall

=cut

sub writeCall {
    my ($self, $method, @params) = @_;

    $method = $self->mangle($method,@params) if $self->{mangle}; # not working well (nor clear in specs), so don't use
    my $x = 'H'
    . pack('C2', 2, 0)
    . 'C'
    . $self->typeformat( Unicode::String->new($method) )
    . $self->typeformat( scalar @params )
    . join('', map($self->typeformat($_), @params));

    $self->{out} = $x; # save
#    $self->debug($self->bin_debug($x));
    return $x;
}

=head2 mangle

=cut

sub mangle {
    my($self,$method,@params) = @_;
    my @pnames = map $self->type_of($_), @params;
    return join '_', $method, @pnames;
}

=head2 typeformat

=cut

sub typeformat {
    my($self, $x) = @_;
    return 'N' unless defined $x; # Hessian::Null
    push @{$self->{outRefs}}, $x if ref($x) =~ /^(ARRAY|HASH|Hessian::(List|Map|Object))$/;
    for(ref $x){
        /^Hessian::Null/        && return 'N';
        /^Hessian::True/        && return 'T';
        /^Hessian::False/       && return 'F';
        /^DateTime$/            && return $self->writeDate($x);
        /^Math::BigInt/         && return $self->writeLong($x);
        /^Hessian::Double/      && return $self->writeDouble($x);
        /^Unicode::String/      && return $self->writeString($x);
        /^Hessian::Binary/      && return $self->writeBinary($x);
        /^Hessian::List/        && return $self->writeList($x);
        /^ARRAY$/               && return $self->writeList(Hessian::List->new(len=>scalar(@$x),data=>$x));
        /^Hessian::Map/         && return $self->writeMap($x);
        /^HASH$/                && return $self->writeMap($x);
        /^Hessian::Class/       && return $self->writeClass($x);
        /^Hessian::Object/      && return $self->writeObject($x);
        /^REF$/                 && return $self->writeRef($x);
        /^.+$/                  && die "typeformat unknown type $x:", ref $x;
        /^$/                    && do { # guess primitives
                                    for($x){
                                        /^[\+-]?\d+$/ && return $self->writeInt($x);
                                        /^[\+-]?(\d+\.|\.\d+)$/ && return $self->writeDouble($x);
                                        # treat everything else as a string
                                        return $self->writeString($x);
                                    }
                                };
        # DEFAULT
        die "typeformat stuck with ($x):", ref $x;

    } # end of for-switch
}

=head2 writeDate

=cut

sub writeDate {
    my($self,$x) = @_;
    $x = Math::BigInt->new($x->epoch());

    my($q,$r) = $x->copy()->bdiv(60);
    if($r->is_zero() && $self->is_between($q,$LB32,$UB32)){
        return 'K' . (pack 'N', unpack 'L', pack 'l', $q->bstr());
    }
    $x->bmul(1000); # milliseconds
    return 'J' . $self->pack_signed_int($x,8);
}

=head2 writeInt

=cut

sub writeInt {
    my($self,$x) = @_;

    my $b = Math::BigInt->new($x) unless 'Math::BigInt' eq ref $x;

    if   ($self->is_between($b,-16,47)){ return pack 'C', $x+0x90 }
    elsif($self->is_between($b,-0x800,0x7ff)){ return pack 'n', $x+0xc800 }
    elsif($self->is_between($b,-0x40000,0x3ffff)){ return substr((pack 'N', $x+0xd40000), -3) }
    elsif($self->is_between($b,$LB32,$UB32)){ return 'I' . pack 'N', unpack 'L', pack 'l', $x }
    return $self->writeLong($b);
}

=head2 writeLong

=cut

sub writeLong {
    my($self,$x) = @_;

    $x = Math::BigInt->new($x) unless 'Math::BigInt' eq ref $x;
    my $i = $x->bstr();

    if   ($self->is_between($x,-8,15)){ return pack 'C', $i+0xe0 }
    elsif($self->is_between($x,-0x800,0x7ff)){ return pack 'n', $i+0xf800 }
    elsif($self->is_between($x,-262144,262143)){ return substr((pack 'N', $i+0x3c0000), -3) }
    elsif($self->is_between($x,-0x80000000,0x7fffffff)){ return "Y" . pack 'N', unpack 'L', pack 'l', $i }
    return 'L' . $self->pack_signed_int($x,8);
}

=head2 writeDouble

=cut

sub writeDouble {
    my($self, $x) = @_;
    $x = $x->{data} if 'Hessian::Double' eq ref $x;

    my $milli = int($x * 1000);

    #not writing compact 4 octet-double, how to know if $x fits in a 32-bit float?
    if($x == int $x){ # probably not accurate, but doesn't matter
        if($x == 0){ return '[' }
        elsif($x == 1){ return "\\" }
        elsif($self->is_between($x, -128, 127)){ return ']' . (pack 'c', $x) }
        elsif($self->is_between($x, -0x8000, 0x7fff)){ return '^' . (pack 'n', unpack 'S', pack 's', $x) }
    }elsif($milli * 0.001 == $x){ # compact double: float
        return '_' . pack('N', unpack 'L', pack 'l', $milli);
    }
    return 'D' . $self->h2n(pack 'd',$x);
}

=head2 writeString

=cut

sub writeString {
    my($self,$x) = @_;
    $x = Unicode::String->new($x) unless 'Unicode::String' eq ref $x;

    my $len = $x->length;
    if($len < 32)         { return chr($len) . $x->as_string }
    elsif($len < 1024)    { return pack('n', $len+0x3000) . $x->as_string }
    elsif($len <= 0x7fff) { return 'S' . (pack 'n',$len) . $x->as_string }

    my($str1,$str2);
    $str1 = $x->substr(0,0x7fff);
    $str2 = $x->substr(0x7fff,$x->length() - 0x7fff);
    return "R\x7f\xff" . $str1->as_string() . $self->writeString($str2);
}

=head2 writeBinary

=cut

sub writeBinary {
    my($self,$x) = @_;
    $x = $x->{data} if 'Hessian::Binary' eq ref $x;

    my $len = length $x;
    if($len < 16){ return chr($len+0x20) . $x }
    elsif($len < 1024){ return pack('n', $len+0x3400) . $x }
    elsif($len <= 0x7fff){ return 'B' . pack('n', $len) . $x }

    return "A\x7f\xff" . substr($x,0,0x7fff) . $self->writeBinary(substr $x, 0x7fff);
}

=head2 writeList

=cut

sub writeList {
    my($self,$x) = @_;
    my $ar = $x->{data};
    if(defined $x->{len} && $x->{len} < 7){ # fixed-length compact
        return pack('C',$x->{len}+(defined $x->{type} ? 0x70 : 0x78))
        . (defined $x->{type} ? $self->writeString($x->{type}) : '')
        . join('',map $self->typeformat($_), @$ar);
    }elsif(defined $x->{len} && defined $x->{type}){
        return 'V'
        . $self->writeString($x->{type})
        . $self->writeInt($x->{len})
        . join('',map $self->typeformat($_), @$ar)
    }elsif(defined $x->{len}){
        return 'X'
        . $self->writeInt($x->{len})
        . join('',map $self->typeformat($_), @$ar)
    }elsif(defined $x->{type}){
        return 'U'
        . $self->writeInt($x->{len})
        . join('',map $self->typeformat($_), @$ar)
        . 'Z';
    }else{
        return 'W'
        . join('',map $self->typeformat($_), @$ar)
        . 'Z';
    }
}

=head2 writeMap

=cut

sub writeMap {
    my($self,$x) = @_;
    if('HASH' eq ref $x){
        my @ar;
        while(my($k,$v)=each(%$x)){
            push @ar, Unicode::String->new($k);
            push @ar, $v;
        }
        $x = Hessian::Map->new(data=>\@ar);
    }
    return (defined $x->{type} ? 'M' : 'H')
    . (defined $x->{type} ? $self->writeString($x->{type}) : '')
    . join('',map $self->typeformat($_), @{$x->{data}})
    . 'Z';
}

=head2 writeClass

=cut

sub writeClass {
    my($self,$x) = @_;

    my $cdar = $self->{outClassRefs};
    my $out;

    if('Hessian::Class' eq ref $x){
        push @$cdar, $x;
        return 'C'
        . $self->writeString($x->{name})
        . $self->writeInt(scalar @{$x->{fields}})
        . join('',map($self->writeString($_), @{$x->{fields}}))
        . ($#$cdar < 16
            ? pack('C',($#$cdar + 0x60))
            : ('O' . ($self->writeInt($#$cdar)))
          );
=comment
    }elsif('REF' eq ref $x){
        $x = $$x;
        die "REF to Hessian::Class needed $x" unless 'Hessian::Class' eq ref $x;
        die "Class def ref found before class def list populated $x" unless $#$cdar >= 0;
        for(my $j = 0; $j <= $#$cdar; $j++){
            if($cdar->[$j] == $x){
                return $j < 16
                ? pack('C',($#$cdar + 0x60))
                : ('O' . ($self->writeInt($#$cdar)));
            }
        }
        die "Class def ref not found in def list";
=cut
    }else{
        die "unknown class type $x:", ref $x;
    }


}

=head2 writeObject

=cut

sub writeObject {
    my($self,$x) = @_;

    return $self->writeClass($x->{class})
    . join('',map($self->typeformat($_), @{$x->{data}}));
}

=head2 writeRef

=cut

sub writeRef {
    my($self, $x) = @_;

    my $ar = $self->{outRefs};
    for(my $j=0; $j <= $#$ar; $j++){
        return 'Q' . $self->writeInt($j) if $$x == $ar->[$j];
    }
    die 'Ref not found in argument list';
}

=head2 parseResponse

=cut

sub parseResponse {
    my($self, $x) = @_;

    $self->{in} = $x; # save

    my $c = substr $x,3,1;
    die "V2 reply version not H 0x2 0x0: ", $self->bin_debug(substr $x,0,3) unless $x =~ /^H\x02\x00/;
    die "V2 reply not begin with R or F: $c, 0x", ord($c) unless $c eq 'R' or $c eq 'F';

    my($data,$i,@list);
    $i = 4;
    while($i < length $x){
        ($data,$i) = $self->readElement($x,$i);
        push @list,$data;
    }

    die "parsing not end at end of response string $i: ",length $x unless $i == 1 + length $x || $i == length $x;
    die "no element read from response" unless 0 < scalar @list;

    if($c eq 'F'){
        my $map = pop @list;
        my %h = @{$map->{data}};
        my $f = Hessian::Fault->new( %h );
        unshift @list, $f;
    }

    $self->{inList} = \@list;
    return $self->{inList}; # list should be: Header* (Fault | Object)
}

=head2 readElement

=cut

sub readElement { #deserialize
    my($self,$x,$i) = @_;

    my($data);
    for(substr $x,$i,1){
        if   (/N/)                               { return Hessian::Null->new(), $i+1 }
        elsif(/T/)                               { return Hessian::True->new(), $i+1 }
        elsif(/F/)                               { return Hessian::False->new(), $i+1 }
        elsif(/[I\x80-\xbf\xc0-\xcf\xd0-\xd7]/)  { ($data,$i) = $self->readInt($x,$i) }
        elsif(/[LY\x38-\x3f\xd8-\xef\xf0-\xff]/) { ($data,$i) = $self->readLong($x,$i) }
        elsif(/[JK]/)                            { ($data,$i) = $self->readDate($x,$i) }
        elsif(/[D\x5b-\x5f]/)                    { ($data,$i) = $self->readDouble($x,$i) }
        elsif(/[AB\x20-\x2f\x34-\x37]/)          { ($data,$i) = $self->readBinary($x,$i) }
        elsif(/[RS\x00-\x1f\x30-\x33]/)           { ($data,$i) = $self->readString($x,$i) }
        elsif(/[UVWX\x70-\x77\x78-\x7f]/)         { ($data,$i) = $self->readList($x,$i) }
        elsif(/[MH]/)                            { ($data,$i) = $self->readMap($x,$i) }
        elsif(/Q/)                               { ($data,$i) = $self->readRef($x,$i) }
        elsif(/C/)                               { ($data,$i) = $self->readClass($x,$i); return $self->readElement($x,$i) }
        elsif(/[O\x60-\x6f]/)                    { ($data,$i) = $self->readObject($x,$i) }
        # default
        else{ die sprintf('Unrecognized element: x%x',ord substr $self->{in},$i,1) }
    }

    return $data,$i;
}

=head2 readInt

=cut

sub readInt {
    my($self,$x,$i) = @_;

    for(substr $x,$i,1){
        /[\x80-\xbf]/   && return unpack('C',$_)-0x90, $i+1;
        /[\xc0-\xcf]/   && return unpack('n',substr $x,$i,2) - 0xc800, $i+2;
        /[\xd0-\xd7]/   && return unpack('N',("\x00". substr($x,$i,3))) - 0xd40000, $i+3;
        /I/             && return unpack('l',pack 'L',unpack 'N',(substr $x,$i+1,4)), $i+5;
        die "Wrong: Int $_";
    }
}

=head2 readLong

=cut

sub readLong {
    my($self,$x,$i) = @_;

    for(substr $x,$i,1){
        /[\xd8-\xef]/   && return Math::BigInt->new(unpack('C',$_)-0xe0), $i+1;
        /[\xf0-\xff]/   && return Math::BigInt->new(unpack('n',substr $x,$i,2) - 0xf800),$i+2;
        /[\x38-\x3f]/   && return Math::BigInt->new(unpack('N',("\x00". substr($x,$i,3))) - 0x3c0000), $i+3;
        /Y/             && return Math::BigInt->new(unpack 'l',pack 'L', unpack 'N', substr $x,$i+1,4), $i+5;
        /L/             && return $self->unpack_int($x,$i+1,8,'signed'), $i+9;
        die "Wrong: Long $_";
    }
}

=head2 readDate

=cut

sub readDate {
    my($self,$x,$i) = @_;

    die "Wrong: date ", substr($x,$i,1) unless substr($x,$i,1) =~ /[JK]/;

    my $epoch;
    if('J' eq substr $x,$i,1){
        $epoch = $self->unpack_int($x,$i+1,8,'signed');
        $epoch->bdiv(1000);
        $i+=9;
    }else{
        $epoch = unpack 'l', pack 'L', unpack 'N', (substr $x,$i+1,4);
        $epoch = Math::BigInt->new($epoch);
        $epoch->bmul(60);
        $i+=5;
    }
    return DateTime->from_epoch(epoch => $epoch->bstr(), time_zone => 'UTC'),$i;
}

=head2 readDouble

=cut

sub readDouble {
    my($self,$x,$i) = @_;

    for(substr $x,$i,1){
        /[\x5b]/ && return Hessian::Double->new(data=>'0.0'), $i+1;
        /[\x5c]/ && return Hessian::Double->new(data=>'1.0'), $i+1;
        /[\x5d]/ && return Hessian::Double->new(data=>unpack('c',(substr$x,$i+1,1))), $i+2;
        /[\x5e]/ && return Hessian::Double->new(data=>unpack('s', pack 'S',unpack 'n',(substr $x,$i+1,2))), $i+3;
        #/[\x5f]/ && return Hessian::Double->new(data=>unpack('f',$self->h2n(substr $x,$i+1,4))), $i+5;
        /[\x5f]/ && return Hessian::Double->new(data=>0.001 * unpack('l',pack 'L',unpack 'N', substr($x,$i+1,4))), $i+5;
        /D/      && return Hessian::Double->new(data=>unpack('d',$self->h2n(substr $x,$i+1,8))), $i+9;
        die "Wrong: double $_";
    }
}

=head2 readBinary

=cut

sub readBinary {
    my($self,$x,$i) = @_;

    my $len;
    for(substr $x,$i,1) {
        /[\x20-\x2f]/ && do {
            $len = unpack('C',$_)-0x20;
            return Hessian::Binary->new(data=>substr($x, $i+1, $len)), $i+1+$len;
        };
        /[\x34-\x37]/ && do {
            $len = unpack('n',substr($x,$i,2))-0x3400;
            return Hessian::Binary->new(data=>substr($x, $i+2, $len)), $i+2+$len;
        };
        /[B]/         && do {
            $len = unpack 'n',substr($x,$i+1,2);
            return Hessian::Binary->new(data=>substr($x, $i+3, $len)), $i+3+$len;
        };
        /[A]/         && do {
            $len = unpack 'n',substr($x,$i+1,2);
            my($y,$j) = $self->readBinary($x,$i+3+$len);
            $y->{data} = substr($x, $i+3, $len) . $y->{data};
            return $y, $j;
        };
        # default
        die "Wrong: Binary $_";

    }
}

=head2 readString

=cut

sub readString {
    my($self,$x,$i) = @_;

    my $len;
    for(substr $x,$i,1) {
        /[\x00-\x1f]/ && do {
            $len = unpack 'C', $_;
            return Unicode::String->new(substr $x,$i+1)->substr(0,$len), $i+1+$len;
        };
        /[\x30-\x33]/ && do {
            $len = unpack('n', substr($x,$i,2)) - 0x3000;
            return Unicode::String->new(substr $x,$i+2)->substr(0,$len), $i+2+$len;
        };
        /[S]/         && do {
            $len = unpack 'n', substr($x,$i+1,2);
            return Unicode::String->new(substr $x,$i+3)->substr(0,$len), $i+3+$len;
        };
        /[R]/         && do {
            $len = unpack 'n', substr($x,$i+1,2);
            my $thisChunk = Unicode::String->new(substr $x,$i+3)->substr(0,$len);
            $len = length $thisChunk->as_string();
            my $thatChunk = substr $x,$len+$i+3;
            my($rest,$j) = $self->readString($x,$len+$i+3);
            return $thisChunk->concat($rest), $j;
        };
        # default
        die "Wrong binary code $_";
    }
}

=head2 readType

=cut

sub readType {
    my($self,$x,$i) = @_;

    my($type,$j) = $self->readElement($x,$i);

    if('Unicode::String' eq ref $type){
        $type = $type->as_string;
        push @{$self->{inTypeRefs}}, $type;
    }elsif($type =~ /\d+/){
        $type = ${$self->{inTypeRefs}->[$type]};
        die "type ref idx not found $type" unless defined $type;
    }else{
        die "Wrong elemnent for type $type";
    }
    return ($type,$j);
}

=head2 readList

=cut

sub readList { # list, type discarded ?
    my($self,$x,$i) = @_;

    my $thisList = Hessian::List->new(); # put in ref first
    my $ar = [];
    push @{$self->{inRefs}}, \$thisList;
    
    my($type,$len,$data);
    for(substr $x,$i,1) {
        /U/ && do {
            ($type,$i) = $self->readType($x,$i+1);
            while('Z' ne substr($x,$i,1)){
                ($data,$i) = $self->readElement($x,$i);
                push @$ar, $data;
            }
            die "U list not end with Z ", substr($x,$i,1) unless 'Z' eq substr($x,$i,1);
            $thisList = Hessian::List->new(type=>$type,data=>$ar);
            return $thisList,$i+1;
        };
                
        /V/ && do {
            ($type,$i) = $self->readType($x,$i+1);
            ($len, $i) = $self->readInt($x,$i);
            for( my $j = 0; $j < $len; $j++ ){
                ($data, $i) = $self->readElement($x,$i);
                push @$ar, $data;
            }
            $thisList = Hessian::List->new(type=>$type,len=>$len,data=>$ar);
            return $thisList,$i;
        };
        /W/ && do {
            $i++;
            while('Z' ne substr $x,$i,1){
                ($data,$i) = $self->readElement($x,$i);
            }
            die "W list not end with Z ", substr($x,$i,1) unless 'Z' eq substr($x,$i,1);
            $thisList = Hessian::List->new(data=>$ar);
            return $thisList,$i+1;
        };
        /X/ && do {
            ($len, $i) = $self->readInt($x,$i+1);
            for( my $j = 0; $j < $len; $j++ ){
                ($data,$i) = $self->readElement($x,$i);
                push @$ar, $data;
            }
            $thisList = Hessian::List->new(len=>$len,data=>$ar);
            return $thisList,$i+1;
        };
        /[\x70-\x77]/ && do {
            $len = unpack('C', $_) - 0x70;
            ($type,$i) = $self->readType($x,$i+1);
            for( my $j = 0; $j < $len; $j++ ){
                ($data,$i) = $self->readElement($x,$i);
                push @$ar, $data;
            }
            $thisList = Hessian::List->new(len=>$len,type=>$type,data=>$ar);
            return $thisList, $i;
        };
        /[\x78-\x7f]/ && do {
            $i++;
            $len = unpack('C', $_) - 0x78;
            for( my $j = 0; $j < $len; $j++ ){
                ($data,$i) = $self->readElement($x,$i);
                push @$ar, $data;
            }
            $thisList = Hessian::List->new(len=>$len,type=>$type,data=>$ar);
            return $thisList, $i;
        };
        die "Wrong: List $_";
    } # end for
} #end readList

=head2 readMap

=cut

sub readMap { # hash, type discarded ?
    my($self,$x,$i) = @_;

    my ($type,$key,$val,$ar) = ('',[]);
    my $thisMap = Hessian::Map->new();
    push @{$self->{inRefs}}, \$thisMap;


    my $ch = substr $x,$i,1;
    $i++;
    die "Map not begin with M/H $ch" unless $ch =~ /[MH]/;
    ($type, $i) = $self->readType($x,$i) if $ch eq 'M';

    while('Z' ne substr $x,$i,1){ # key-value pair
        ($key,$i) = $self->readElement($x,$i);
        $key = $key->as_string() if 'Unicode::String' eq ref $key; # for convenience
        ($val,$i) = $self->readElement($x,$i);
        push @$ar, $key, $val;
    }
    die "Map not end with Z ", substr($x,$i,1) unless 'Z' eq substr($x,$i,1);
    $thisMap = Hessian::Map->new(type=>$type,data=>$ar);
    return $thisMap, $i+1; # skip 'Z'
}

=head2 readRef

=cut

sub readRef { # obj, list or map
    my($self,$x,$i) = @_;

    my($idx,$j) = $self->readInt($x,$i+1);
    die "failed to get int Ref idx $idx" unless defined $idx;
    die "ref idx out of bound $idx" unless defined $self->{inRefs}->[$idx];
    return $self->{inRefs}->[$idx],$j;
}

=head2 readClass

=cut

sub readClass {
    my($self,$x,$i) = @_;

    my $thisClass = Hessian::Class->new();
    my($name,$len,$data);
    my $ar = [];
    ($name, $i) = $self->readString($x,,$i+1);
    ($len, $i) = $self->readInt($x,$i);

    for(my $j=0; $j < $len; $j++){
        ($data, $i) = $self->readString($x,$i);
        push @$ar,$data;
    }
    $thisClass = Hessian::Class->new(name=>$name,len=>$len,fields=>$ar);
    push @{$self->{inClassRefs}}, $thisClass;
    return $thisClass, $i;
}

=head2 readObject

=cut

sub readObject {
    my($self,$x,$i) = @_;

    my $thisObject = {};
    my $ar = [];
    push @{$self->{inRefs}}, \$thisObject;

    my($idx,$data);
    my $ch = substr $x,$i,1;
    $i++;
    if($ch eq 'O'){
        ($idx,$i) = $self->readInt($x,$i);
    }elsif($ch =~ /[\x60-\x6f]/){
        $idx = unpack('C',$_) - 0x60;
    }

    my $class = $self->{inClassRefs}->[$idx];
    die "failed to get class for object" unless defined $class;

    for(my $j=0; $j < $class->{len}; $j++){
        ($data,$i) = $self->readElement($x,$i);
        push @$ar, $data;
    }
    $thisObject = Hessian::Object->new(class=>$class,data=>$ar);
    return $thisObject, $i;
}

=head1 AUTHOR

du ling, C<< <ling.du at gmail.com> >>

=head1 BUGS

Please report any bugs or feature requests to C<bug-hessian-translator-v2 at rt.cpan.org>, or through
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

1; # End of Hessian::Converter::V2
