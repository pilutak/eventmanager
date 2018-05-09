#!/usr/bin/perl

use strict;
use Getopt::Long;
use LWP::UserAgent;

my $help;
my $host;
my $grp;

GetOptions(
    'help'     => \$help,
    'host=s'   => \$host,
    'grp=s'    => \$grp
);

help() if $help;

sub help {
    print <<EOH;
Usage: $0 [options]
    --host    HOST/IP ADDRESS IP of EM2 instance
    --grp     BROADWORKS group ID
EOH
    exit();
}


my $ua = LWP::UserAgent->new;

print "DELETING CORE DATA FOR BW GROUP: $grp\n";
my $response = $ua->delete( "http://$host:8080/groups/$grp");
die "Can't get http://$host:8080/groups -- ", $response->status_line
  unless $response->is_success;



