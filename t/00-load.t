#!perl -T

use Test::More tests => 5;

BEGIN {
	use_ok( 'Hessian::SimpleClient' );
	use_ok( 'Hessian::Translator' );
	use_ok( 'Hessian::Type' );
	use_ok( 'Hessian::Translator::V1' );
	use_ok( 'Hessian::Translator::V2' );
}

diag( "Testing Hessian::SimpleClient $Hessian::SimpleClient::VERSION, Perl $], $^X" );
