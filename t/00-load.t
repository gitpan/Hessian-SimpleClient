#!perl -T

use Test::More tests => 5;

BEGIN {
	use_ok( 'Hessian::SimpleClient' );
	use_ok( 'Hessian::Converter' );
	use_ok( 'Hessian::Type' );
	use_ok( 'Hessian::Converter::V1' );
	use_ok( 'Hessian::Converter::V2' );
}

diag( "Testing Hessian::SimpleClient $Hessian::SimpleClient::VERSION, Perl $], $^X" );
