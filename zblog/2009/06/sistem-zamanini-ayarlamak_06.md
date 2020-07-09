# Sistem Zamanini Ayarlamak


Sistem Zamanini Ayarlamak




Linux bilgisayarimizin sistem zamanini en dogru sekilde set etmek istiyorsak, Internet'te mevcut "zaman servislerinden" faydalanabiliriz. Bu servislere erisip, en dogru zamani oradan alip sistem saatini degistiren bir script altta bulunabilir.

Kullanim:

sudo perl time.pl -u pool.ntp.org


#!/usr/bin/perl -w
#
# sntp.pl
#
my $PgmName = "SNTP - Get time from an NTP time server";
my $VERSION = "0.2b, 2004-12-10";
#

=head1 SNTP - Get time from an NTP time server

 This program has been written just for fun to take a short look on the NTP stuff
 and to learn about the NTP/SNTP data :
 get time information from an NTP time server using SNTP (Simple Network Time
 Protocol, RFC-2030), analyze the protocol information, and display what we've got.
 Finally optionally set the local unix system clock, if you wish.

 For real time synchronization, please use a more professional client software.

 (c) 2001, Ralf D. Kloth <ralf at qrq.de>, QRQ.software.
 All rights reserved.
 This program is free software; you can redistribute it and/or modify it
 under the same terms as Perl itself.
 THIS PRGOGRAM IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESSED OR IMPLIED
 WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 You are using this program at own risk.

 Written and tested under Perl V 5.6.0 on Linux 2.4.

 Revision history:
   V0.1,  2000-09-30, first version
   V0.2,  2001-03-31, published version
   V0.2b, 2004-12-10, minor patch by Tim Braun

 usage: perl sntp.pl [-u (update the unix clock)] <timeserver>

 Attention: Year-2036 problem:
 This program will not work after the NTP timestamp rollover on 2036-02-07.

 Finally, RFC-2030 recommends the following on stratum usage:
 As the load on the hosts supporting NTP primary (stratum 1) time service is heavy
 and always increasing, clients should avoid using the primary servers whenever
 possible. In most cases the accuracy of the NTP secondary (stratum 2) servers is
 only slightly degraded relative to the primary servers and, as a group, the
 secondary servers may be just as reliable.
 List of stratum 2 servers: http://www.eecis.udel.edu/~mills/ntp/clock2.htm

=cut

use strict;
use IO::Socket;
use Time::HiRes qw(time);
use Getopt::Std;

sub usage { # print a short 'usage` message and exit
  print "usage: perl $0 <timeserver>\n";
  print "                      [-u (update the unix clock)]\n";
  exit;
}

use vars qw($opt_h $opt_u); # command line options

  print "$0, $PgmName, V $VERSION \n";
  # handle the commandline options
  getopts('hu');            # look for -h and -u
  $opt_h && usage;          # -h print usage message and exit
  (!$ARGV[0]) && usage;     # no server: print usage message and exit

  my $server = $ARGV[0];
  my $serverIPv4 ="";
  if (gethostbyname($server)) {
    $serverIPv4 = sprintf("%d.%d.%d.%d",unpack("C4",gethostbyname($server)));
  }

  my $timeout = 2;

  sub bin2frac { # convert a binary string to fraction
    my @bin = split '', shift;
    my $frac = 0;
    while (@bin) {
      $frac = ($frac + pop @bin)/2;
    }
    $frac;
  } # end sub bin2frac

  sub frac2bin { # convert a fraction to binary string (B32)
    my $frac = shift;
    my $bin ="";
    while (length($bin) < 32) {
      $bin = $bin . int($frac*2);
      $frac = $frac*2 - int($frac*2);
    }
    $bin;
  } # end sub frac2bin

  my ($LocalTime0, $LocalTime0F, $LocalTime0H, $LocalTime0FH, $LocalTime0FB);
  my ($LocalTime1, $LocalTime2);
  my ($LocalTime, $LocalTimeF, $LocalTimeT);
  my ($NetTime, $NetTime2, $Netfraction);
  my ($netround, $netdelay, $off);

  my ($Byte1, $Stratum, $Poll, $Precision,
      $RootDelay, $RootDelayFB, $RootDisp, $RootDispFB, $ReferenceIdent,
      $ReferenceTime, $ReferenceTimeFB, $OriginateTime, $OriginateTimeFB,
      $ReceiveTime, $ReceiveTimeFB, $TransmitTime, $TransmitTimeFB);
  my ($dummy, $RootDelayH, $RootDelayFH, $RootDispH, $RootDispFH, $ReferenceIdentT,
      $ReferenceTimeH, $ReferenceTimeFH, $OriginateTimeH, $OriginateTimeFH,
      $ReceiveTimeH, $ReceiveTimeFH, $TransmitTimeH, $TransmitTimeFH);
  my ($LI, $VN, $Mode, $sc, $PollT, $PrecisionV, $ReferenceT, $ReferenceIPv4);

  my $ntp_msg;  # NTP message according to NTP/SNTP protocol specification

  sub get_ntp_time {
  # open the connection to the ntp server,
  # prepare the ntp request packet
  # send and receive
  # take local timestamps before and after

    my ($remote);
    my ($rin, $rout, $eout) ="";
    my $ntp_msg;

    # open the connection to the ntp server
    $remote = IO::Socket::INET -> new(Proto => "udp", PeerAddr => $server,
                                      PeerPort => 123,
                                      Timeout => $timeout)
                                  or die "Can't connect to \"$server\"\n";

    # measure local time BEFORE timeserver query
    $LocalTime1 = time();
    # convert fm unix epoch time to NTP timestamp
    $LocalTime0 = $LocalTime1 + 2208988800;

    # prepare local timestamp for transmission in our request packet
    $LocalTime0F = $LocalTime0 - int($LocalTime0);
    $LocalTime0FB = frac2bin($LocalTime0F);
    $LocalTime0H = unpack("H8",(pack("N", int($LocalTime0))));
    $LocalTime0FH = unpack("H8",(pack("B32", $LocalTime0FB)));

    $ntp_msg = pack("B8 C3 N10 B32", '00011011', (0)x12, int($LocalTime0), $LocalTime0FB);
                   # LI=0, VN=3, Mode=3 (client), remainder msg is 12 nulls
                   # and the local TxTimestamp derived from $LocalTime1

    # send the ntp-request to the server
    $remote -> send($ntp_msg) or return undef;
    vec($rin, fileno($remote), 1) = 1;
    select($rout=$rin, undef, $eout=$rin, $timeout)
      or do {print "No answer from $server\n"; exit};

    # receive the ntp-message from the server
    $remote -> recv($ntp_msg, length($ntp_msg))
               or do {print "Receive error from $server ($!)\n"; exit};

    # measure local time AFTER timeserver query
    $LocalTime2 = time();

    $ntp_msg;

  } # end sub get_ntp_time

  sub interpret_ntp_data {
  # do some interpretations of the data

    my $ntp_msg = shift;

    # unpack the received ntp-message into long integer and binary values
    ( $Byte1, $Stratum, $Poll, $Precision,
      $RootDelay, $RootDelayFB, $RootDisp, $RootDispFB, $ReferenceIdent,
      $ReferenceTime, $ReferenceTimeFB, $OriginateTime, $OriginateTimeFB,
      $ReceiveTime, $ReceiveTimeFB, $TransmitTime, $TransmitTimeFB) =
      unpack ("a C3   n B16 n B16 H8   N B32 N B32   N B32 N B32", $ntp_msg);

    # again unpack the received ntp-message into hex and ASCII values
    ( $dummy, $dummy, $dummy, $dummy,
      $RootDelayH, $RootDelayFH, $RootDispH, $RootDispFH, $ReferenceIdentT,
      $ReferenceTimeH, $ReferenceTimeFH, $OriginateTimeH, $OriginateTimeFH,
      $ReceiveTimeH, $ReceiveTimeFH, $TransmitTimeH, $TransmitTimeFH) =
      unpack ("a C3   H4 H4 H4 H4 a4   H8 H8 H8 H8   H8 H8 H8 H8", $ntp_msg);

    $LI = unpack("C", $Byte1 & "\xC0") >> 6;
    $VN = unpack("C", $Byte1 & "\x38") >> 3;
    $Mode = unpack("C", $Byte1 & "\x07");
    if ($Stratum < 2) {$sc = $Stratum;}
    else {
      if ($Stratum > 1) {
        if ($Stratum < 16) {$sc = 2;} else {$sc = 16;}
      }
    }
    $PollT = 2**($Poll);
    if ($Precision > 127) {$Precision = $Precision - 255;}
    $PrecisionV = sprintf("%1.4e",2**$Precision);
    $RootDelay += bin2frac($RootDelayFB);
    $RootDelay = sprintf("%.4f", $RootDelay);
    $RootDisp += bin2frac($RootDispFB);
    $RootDisp = sprintf("%.4f", $RootDisp);
    $ReferenceT = "";
    if ($Stratum eq 1) {$ReferenceT = "[$ReferenceIdentT]";}
    else {
      if ($Stratum eq 2) {
        if ($VN eq 3) {
          $ReferenceIPv4 = sprintf("%d.%d.%d.%d",unpack("C4",$ReferenceIdentT));
          $ReferenceT = "[32bit IPv4 address $ReferenceIPv4 of the ref src]";
        }
        else {
          if ($VN eq 4) {$ReferenceT = "[low 32bits of latest TX timestamp of reference src]";}
        }
      }
    }

    $ReferenceTime += bin2frac($ReferenceTimeFB);
    $OriginateTime += bin2frac($OriginateTimeFB);
    $ReceiveTime += bin2frac($ReceiveTimeFB);
    $TransmitTime += bin2frac($TransmitTimeFB);

  } # end sub interpret_ntp_data

  sub calculate_time_data {
  # convert time stamps to unix epoch and do some calculations on the time data

    my ($sec, $min, $hr, $dy, $mo, $yr);

    $ReferenceTime -= 2208988800; # convert to unix epoch time stamp
    $OriginateTime -= 2208988800;
    $ReceiveTime -= 2208988800;
    $TransmitTime -= 2208988800;

    $NetTime = scalar(gmtime $TransmitTime);
    $Netfraction = sprintf("%03.f",1000*sprintf("%.3f", $TransmitTime - int($TransmitTime)));
    ($sec, $min, $hr, $dy, $mo, $yr) = gmtime($TransmitTime);
    $NetTime2 = sprintf("%04d-%02d-%02d %02d:%02d:%02d", $yr+1900, $mo+1, $dy, $hr, $min, $sec);

    # calculate delay and difference
    $netround = sprintf("%+.4f",($LocalTime1 - $LocalTime2));
    $netdelay = sprintf("%+.4f",(($LocalTime1 - $LocalTime2)/2) - ($TransmitTime - $ReceiveTime));
    $off = sprintf("%+.4f",(($ReceiveTime - $LocalTime1) + ($TransmitTime - $LocalTime2))/2);

    $LocalTime = ($LocalTime1 + $LocalTime2) /2;
    $LocalTimeF = sprintf("%03.f",1000*sprintf("%.3f", $LocalTime - int($LocalTime)));
    ($sec, $min, $hr, $dy, $mo, $yr) = gmtime($LocalTime);
    $LocalTimeT = sprintf("%04d-%02d-%02d %02d:%02d:%02d", $yr+1900, $mo+1, $dy, $hr, $min, $sec);

  } # end sub calculate_time_data

  sub output_ntp_data { # raw data from $ntp_msg
  # output the information we have

    my %LItext = (  "0" => "no warning",
                    "1" => "last minute of current day has 61 sec",
                    "2" => "last minute of current day has 59 sec",
                    "3" => "alarm condition (clock not synchronized)",
                 );
    my %Modetext = ("0" => "reserved",
                    "1" => "symmetric active",
                    "2" => "symmetric passive",
                    "3" => "client",
                    "4" => "server",
                    "5" => "broadcast",
                    "6" => "reserved for NTP control message",
                    "7" => "reserved for private use",
                   );

    my %Stratumtext = (
                    "0" => "unspecified or unavailable",
                    "1" => "primary reference (e.g. radio clock)",
                    "2" => "2...15: secondary reference (via NTP or SNTP)",
                    "16" => "16...255: reserved",
                       );

    print "Local Transmit Timestp : " . $LocalTime0 . "\n";
    print "The ntp server [$server $serverIPv4] sent the following data:\n";
    print "Byte1                  : " . ord($Byte1) . "\n";
    print "  Leap Indicator (LI)  : $LI [" . $LItext{$LI} . "]\n";
    print "  Version number (VN)  : $VN [NTP/SNTP version number]\n";
    print "  Mode                 : $Mode [" . $Modetext{$Mode} . "]\n";
    print "Stratum                : $Stratum [" . $Stratumtext{$sc} . "]\n";
    print "Poll Interval          : $Poll [2**$Poll = $PollT sec max interval between successive msgs]\n";
    print "Clock Precision        : $Precision [2**$Precision = $PrecisionV]\n";
    print "Root Delay             : $RootDelayH$RootDelayFH [$RootDelay sec]\n";
    print "Root Dispersion        : $RootDispH$RootDispFH [$RootDisp sec]\n";
    print "Reference Identifier   : $ReferenceIdent $ReferenceT \n";
    print "Reference Timestamp    : $ReferenceTimeH.$ReferenceTimeFH [" .
                                    sprintf("%10.5f",$ReferenceTime) . "]\n";
    print "Originate Timestamp    : $OriginateTimeH.$OriginateTimeFH [" .
                                    sprintf("%10.5f",$OriginateTime) . "]\n";
    print "Receive Timestamp      : $ReceiveTimeH.$ReceiveTimeFH [" .
                                    sprintf("%10.5f",$ReceiveTime) . "]\n";
    print "Transmit Timestamp     : $TransmitTimeH.$TransmitTimeFH [" .
                                    sprintf("%10.5f",$TransmitTime) . "]\n";
    print "\n";

  } # end sub output_ntp_date

  sub output_ntp_data2 { # interpreted time data
  # output the information we have

    print "Interpreted results, converted to unix epoch (sec since 1970-01-01 00:00:00):\n";
    print "Reference Timestamp    : " . sprintf("%10.5f",$ReferenceTime) .
          " [last sync of server clock with ref]\n";
    print "Originate Timestamp    : " . sprintf("%10.5f",$OriginateTime) .
          " [returned Local Transmit Timestamp]\n";

    print "Receive Timestamp      : " . sprintf("%10.5f",$ReceiveTime) .
          " [request packet arrived at server]\n";
    print "Transmit Timestamp     : " . sprintf("%10.5f",$TransmitTime) .
          " [this reply departed the server]\n";

    print "Net time UTC           : $NetTime +$Netfraction ms\n";
    print "                         $NetTime2.$Netfraction\n";

    # delay and difference
    print "Network roundtrip time : $netround sec";
    if (abs($netround) > 1) {print " <-- high roundtrip time, try another server closer to you";}
    print "\n";
    print "Network delay          : $netdelay sec";
    if (abs($netdelay) > 1) {print " <-- high delay time, try another server closer to you";}
    print "\n";

    print "Local Timestamp        : $LocalTime \n";
    print "Local time UTC         : $LocalTimeT.$LocalTimeF\n";

  } # end sub output_ntp_data2

  sub correct_localclock {
  # set the unix clock to the nearest second
    my $off = shift;
    $off = sprintf("%.f", $off);
    print "\nSet local system clock: ";
    #print ("date --set=\'$off seconds\'");
    system("date --set=\'$off seconds\'");
  } # end sub correct_localclock

  # main *********************************************************************

  print "Connecting to $server\n";

  $ntp_msg = get_ntp_time;

  interpret_ntp_data($ntp_msg);

  # Check if the received packet is the correct reply to our request:
  # it is correct, if our original transmit time we sent in the Transmit Time field
  # of our request packet shows up in the Originate Time field of the received reply.
  if (($LocalTime0H . $LocalTime0FH) ne ($OriginateTimeH . $OriginateTimeFH)) {
    print "*** The received reply seems to be faulty and NOT the reply to our request packet:\n";
    print "*** The OriginateTime stamp $OriginateTimeH.$OriginateTimeFH of the received packet does not \n";
    print "***  show our Transmit Time $LocalTime0H.$LocalTime0FH.\n";
    exit;
  }

  # comment this one out, if you don't want to see these data
  output_ntp_data;    # raw data from $ntp_msg

  calculate_time_data;

  # comment this one out, if you don't want to see these data
  output_ntp_data2;   # interpreted and calculated time data

  # the final result: the difference report
  print "Clock Difference       : $off sec off between $server and local";
  if (abs($off) > 11000) {print " <-- check this !";}
  print "\n";

  # if this program is executed with system privileges under unix,
  # the unix clock may now be set to the nearest second
  $opt_u && correct_localclock($off);

__END__







