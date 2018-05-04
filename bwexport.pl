#!/usr/bin/perl

use strict;
use IO::Socket::INET;
use XML::Simple;
use Data::Dumper;
use Digest::SHA1 qw(sha1 sha1_hex sha1_base64);
use Digest::MD5 qw(md5 md5_hex md5_base64);
use Getopt::Long;

use LWP::UserAgent;

my $help;
my $host;
my $user;
my $password;
my $sp;
my $grp;
my $mode;
my $sessionId = get_session_id();

GetOptions(
    'help'     => \$help,
    'host=s'   => \$host,
    'user=s'   => \$user,
    'pass=s'   => \$password,
    'sp=s'     => \$sp,
    'grp=s'    => \$grp,
    'mode=s'   => \$mode
);

help() if $help;

sub help {
    print <<EOH;
Usage: $0 [options]
    --host    HOST/IP ADDRESS IP of Broadworks application server
    --user    USERNAME of Broadworks group administrator
    --pass    PASSWORD of Broadworks group administrator
    --sp      BROADWORKS service provider
    --grp     BROADWORKS group ID
EOH
    exit();
}


my $ua = LWP::UserAgent->new;
my $url = "http://localhost:8080/users";



# flush after every write
$| = 1;

my ( $socket, $client_socket );

$socket = new IO::Socket::INET(
    PeerHost => $host,
    PeerPort => '2208',
    Proto    => 'tcp',
) or die "ERROR in Socket Creation: $!\n";

my $xml = '<?xml version="1.0" encoding="ISO-8859-1"?>';
$xml .= '<BroadsoftDocument protocol = "OCI" xmlns="C" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">';
$xml .= "<sessionId xmlns=\"\">$sessionId</sessionId>";
$xml .= '<command xsi:type="AuthenticationRequest" xmlns="">';
$xml .= "<userId>$user</userId>";
$xml .= '</command></BroadsoftDocument>';

$socket->send($xml);

my $data;
$socket->recv( $data, 1024 );

#print "Received from Server : $data\n";

my $ref   = XMLin($data);
my $nonce = $ref->{command}->{nonce};

#Sign password with nonce
my $s1             = sha1_hex($password);
my $s2             = $nonce . ':' . $s1;
my $signedPassword = md5_hex($s2);

$xml = '<?xml version="1.0" encoding="ISO-8859-1"?>';
$xml .= '<BroadsoftDocument protocol = "OCI" xmlns="C" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">';
$xml .= "<sessionId xmlns=\"\">$sessionId</sessionId>";
$xml .= '<command xsi:type="LoginRequest14sp4" xmlns="">';
$xml .= "<userId>$user</userId>";
$xml .= "<signedPassword>$signedPassword</signedPassword>";
$xml .= '</command></BroadsoftDocument>';

$socket->send($xml);
$socket->recv( $data, 1024 );

#print "Received from Server : $data\n";

my $filename = 'users.txt';
my $filename1 = 'hg.txt';
my $filename2 = 'aa.txt';
my $filename3 = 'cc.txt';

#open(my $fh, '>>', $filename) or die "Could not open file '$filename' $!";
#open(my $fh1, '>>', $filename1) or die "Could not open file '$filename1' $!";
#open(my $fh2, '>>', $filename2) or die "Could not open file '$filename2' $!";
#open(my $fh3, '>>', $filename3) or die "Could not open file '$filename3' $!";


# WE ARE GETTING USERS FROM THE SOURCE GROUP
$xml = '<?xml version="1.0" encoding="ISO-8859-1"?>';
$xml .= '<BroadsoftDocument protocol = "OCI" xmlns="C" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">';
$xml .= "<sessionId xmlns=\"\">$sessionId</sessionId>";
$xml .= '<command xsi:type="UserGetListInGroupRequest" xmlns="">';
$xml .= "<serviceProviderId>$sp</serviceProviderId>";
$xml .= "<GroupId>$grp</GroupId>";
$xml .= "<responseSizeLimit>2000</responseSizeLimit>";
$xml .= '</command></BroadsoftDocument>';

$socket->send($xml);

# THIS DATA MIGHT BE REALLY BIG!!
my $tdata;
while (<$socket>) {

    $tdata .= $_;
    if ( $_ =~ m/<\/BroadsoftDocument>/ ) { last; }

}
print "$tdata\n";
my $userGetListInGroupResponse = XMLin( $tdata, ForceArray => 1 );

#print Dumper($userGetListInGroupResponse);

my $users_ref;
foreach my $user (@{ $userGetListInGroupResponse->{command}->[0]->{userTable}->[0]->{row} }){

    $users_ref->{ $user->{col}->[0] } = {

        userId     => $user->{col}->[0],
        lastName   => $user->{col}->[1],
        firstName  => $user->{col}->[2],
        phone      => $user->{col}->[4],
        istrunk    => $user->{col}->[9],
    };


}

while ( my ( $k, $v ) = each %$users_ref ) {
    

    $xml = '<?xml version="1.0" encoding="ISO-8859-1"?>';
    $xml .= '<BroadsoftDocument protocol = "OCI" xmlns="C" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">';
    $xml .= "<sessionId xmlns=\"\">$sessionId</sessionId>";
    $xml .= '<command xsi:type="UserGetRequest19" xmlns="">';
    $xml .= "<userId>$k</userId>";
    $xml .= '</command></BroadsoftDocument>';
    
    $socket->send($xml);

    # THIS DATA MIGHT BE REALLY BIG!!
    my $atdata;
    while (<$socket>) {
            $atdata .= $_;
            if ( $_ =~ m/<\/BroadsoftDocument>/ ) { last; }

    }

    #print "Received from Server : $atdata\n";

    my $userGetResponse19 = XMLin( $atdata, ForceArray => 1 );
    $users_ref->{$k}->{city} = $userGetResponse19->{command}->[0]->{address}->[0]->{stateOrProvince}->[0];
    $users_ref->{$k}->{ispilot} = $userGetResponse19->{command}->[0]->{trunkAddressing}->[0]->{trunkGroupDeviceEndpoint}->[0]->{isPilotUser}->[0];


    if ($v->{'city'} eq "") {
        $users_ref->{$k}->{city} = "Nuuk";
    }    

    if ($v->{'ispilot'} eq "") {
        $users_ref->{$k}->{ispilot} = "false";
    }    


    if ($v->{'istrunk'} eq "false") {
        if ($v->{'phone'} =~ m/HASH/) {
                    
            #print "User: $v->{'userId'} City: $v->{'city'} Phone: NODATA\n";
            #my $response = $ua->post( $url, { 'id' => $v->{'userId'}, 'group' => $grp, 'type' => 'user', 'city' =>  $v->{'city'} } );
            print "{ 'id' => $v->{'userId'}, 'group' => $grp, 'type' => 'user', 'city' =>  $v->{'city'} }\n";
            next;        
        };
        
        #print "User: $v->{'userId'} City: $v->{'city'} Phone: $v->{'phone'}\n";
        
        if ( $v->{'phone'} =~ m/(\+299-)(\d+)/ ) {        
            my $phone = $2;
            #my $response = $ua->post( $url, { 'id' => $v->{'userId'}, 'group' => $grp, 'phone' => $phone, 'type' => 'user', $v->{'city'} } );
            print "{ 'id' => $v->{'userId'}, 'group' => $grp, 'type' => 'user', 'city' =>  $v->{'city'}, 'phone' => $phone, }\n";
        
         }
        
        
    # This is an trunk user    
    } else {
        
        
        if ($v->{'ispilot'} eq "true") {
        
            print "{ 'id' => $v->{'userId'}, 'group' => $grp, 'type' => 'pilot', 'city' =>  $v->{'city'} }\n";

        } else {
            
            print "{ 'id' => $v->{'userId'}, 'group' => $grp, 'type' => 'trunk', 'city' =>  $v->{'city'} }\n";
                
            
        }
    };
    

}



# WE ARE GETTING HUNTGROUPS FROM THE GROUP
$xml = '<?xml version="1.0" encoding="ISO-8859-1"?>';
$xml .= '<BroadsoftDocument protocol = "OCI" xmlns="C" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">';
$xml .= "<sessionId xmlns=\"\">$sessionId</sessionId>";
$xml .= '<command xsi:type="GroupHuntGroupGetInstanceListRequest" xmlns="">';
$xml .= "<serviceProviderId>$sp</serviceProviderId>";
$xml .= "<groupId>$grp</groupId>";
$xml .= '</command></BroadsoftDocument>';

$socket->send($xml);

# THIS DATA MIGHT BE REALLY BIG!!
my $tdata1;
while (<$socket>) {

    $tdata1 .= $_;
    if ( $_ =~ m/<\/BroadsoftDocument>/ ) { last; }

}
#print "$tdata1\n";
my $GroupHuntGroupGetInstanceListResponse = XMLin( $tdata1, ForceArray => 1 );

#print Dumper($GroupHuntGroupGetInstanceListResponse);

my $hgs_ref;
foreach my $hg (@{ $GroupHuntGroupGetInstanceListResponse->{command}->[0]->{huntGroupTable}->[0]->{row} }){

    $hgs_ref->{ $hg->{col}->[0] } = {

        userId     => $hg->{col}->[0],
        phone      => $hg->{col}->[2]
    };


}


    while ( my ( $k, $v ) = each %$hgs_ref ) {
        
        if ($v->{'phone'} =~ m/HASH/) {
        
            print "$v->{'userId'} NODATA\n";
            next;
    
        
        };

        #print $fh1 "$v->{'userId'} $v->{'phone'}\n";
        print "$v->{'userId'} $v->{'phone'}\n";
        
    }
#close $fh1;



# WE ARE GETTING AUTOATTENDANTS FROM THE GROUP
$xml = '<?xml version="1.0" encoding="ISO-8859-1"?>';
$xml .= '<BroadsoftDocument protocol = "OCI" xmlns="C" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">';
$xml .= "<sessionId xmlns=\"\">$sessionId</sessionId>";
$xml .= '<command xsi:type="GroupAutoAttendantGetInstanceListRequest" xmlns="">';
$xml .= "<serviceProviderId>$sp</serviceProviderId>";
$xml .= "<groupId>$grp</groupId>";
$xml .= '</command></BroadsoftDocument>';

$socket->send($xml);

# THIS DATA MIGHT BE REALLY BIG!!
my $tdata2;
while (<$socket>) {

    $tdata2 .= $_;
    if ( $_ =~ m/<\/BroadsoftDocument>/ ) { last; }

}
#print "$tdata1\n";
my $GroupAutoAttendantGetInstanceListResponse = XMLin( $tdata2, ForceArray => 1 );

#print Dumper($GroupHuntGroupGetInstanceListResponse);

my $aas_ref;
foreach my $aa (@{ $GroupAutoAttendantGetInstanceListResponse->{command}->[0]->{autoAttendantTable}->[0]->{row} }){

    $aas_ref->{ $aa->{col}->[0] } = {

        userId     => $aa->{col}->[0],
        phone      => $aa->{col}->[3]
    };


}


  # WE ARE REMOVING PHONE FROM AA, GROUP AND SERVICEPROVIDER
while ( my ( $k, $v ) = each %$aas_ref ) {
    
    if ($v->{'phone'} =~ m/HASH/) {
        
        #print "$v->{'userId'} NODATA\n";
        #my $response = $ua->post( $url, { 'id' => $v->{'userId'}, 'group' => $grp, 'type' => 'virtual' } );
        next;
    
        
    };

    #print $fh2 "$v->{'userId'} $v->{'phone'}\n";
    print "$v->{'userId'} $v->{'phone'}\n";
    
    if ( $v->{'phone'} =~ m/(\+299-)(\d+)/ ) { 
        
        my $phone = $2;
        #my $response = $ua->post( $url, { 'id' => $v->{'userId'}, 'group' => $grp, 'phone' => $phone, 'type' => 'virtual' } );
    
    
     }
    
}
#close $fh2;


# WE ARE GETTING CALLCENTERS FROM THE GROUP
$xml = '<?xml version="1.0" encoding="ISO-8859-1"?>';
$xml .= '<BroadsoftDocument protocol = "OCI" xmlns="C" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">';
$xml .= "<sessionId xmlns=\"\">$sessionId</sessionId>";
$xml .= '<command xsi:type="GroupCallCenterGetInstanceListRequest" xmlns="">';
$xml .= "<serviceProviderId>$sp</serviceProviderId>";
$xml .= "<groupId>$grp</groupId>";
$xml .= '</command></BroadsoftDocument>';

$socket->send($xml);

# THIS DATA MIGHT BE REALLY BIG!!
my $tdata2;
while (<$socket>) {

    $tdata2 .= $_;
    if ( $_ =~ m/<\/BroadsoftDocument>/ ) { last; }

}
#print "$tdata1\n";
my $GroupCallCenterGetInstanceListResponse = XMLin( $tdata2, ForceArray => 1 );

#print Dumper($GroupHuntGroupGetInstanceListResponse);

my $ccs_ref;
foreach my $cc (@{ $GroupCallCenterGetInstanceListResponse->{command}->[0]->{callCenterTable}->[0]->{row} }){

    $ccs_ref->{ $cc->{col}->[0] } = {

        userId     => $cc->{col}->[0],
        phone      => $cc->{col}->[3]
    };


}


  # WE ARE REMOVING PHONE FROM AA, GROUP AND SERVICEPROVIDER
while ( my ( $k, $v ) = each %$ccs_ref ) {
    
    if ($v->{'phone'} =~ m/HASH/) {
        
        print "$v->{'userId'} NODATA\n";
        next;
    
        
    };

    #print $fh3 "$v->{'userId'} $v->{'phone'}\n";
    print "$v->{'userId'} $v->{'phone'}\n";

}
#close $fh2;

$xml = '<?xml version="1.0" encoding="ISO-8859-1"?>';
$xml .= '<BroadsoftDocument xmlns="C" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" protocol="OCI">';
$xml .= "<sessionId xmlns=\"\">$sessionId</sessionId>";
$xml .= '<command xmlns="" xsi:type="LogoutRequest">';
$xml .= "<userId>$user</userId>";
$xml .= '<reason>Client Logout</reason>';
$xml .= '</command></BroadsoftDocument>';

$socket->send($xml);
$socket->close();


sub get_session_id {

    my $sessionId = "";
    my $length    = 16;

    for ( my $i = 0; $i < $length; ) {
        my $j = chr( int( rand(127) ) );

        if ( $j =~ /[a-zA-Z0-9]/ ) {
            $sessionId .= $j;
            $i++;
        }
    }

    return $sessionId;
}



