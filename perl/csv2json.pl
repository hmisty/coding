#!/usr/bin/perl -w
use strict;
use Data::Dumper;
use Encode;

# A super simple tool to help convert csv to json for importing into mongodb eventually.

# the csv file
my $csvfile = "abc.csv";
my $jsonfile = "abc.json";

# csv col names
my @colnames = qw/member_id recommender name cellphone res_time_status guarantee_money accumulated_money doc_department hospital concern status num_docs place first_contact_time/;
print "@colnames\n";

# here deals with the ugly csv exported from the shitty excel file
open JSON, "> $jsonfile" or die "Cannot open file $jsonfile!\n";
open CSV, $csvfile or die "Cannot open file $csvfile!\n";
binmode CSV;

chomp(my $whole = <CSV>);
my @lines = split /\r/, $whole;
for (@lines) {
  my $utf8line = decode('gbk', $_);
  
  my @cols = split /;/, $utf8line; #FIXME ; or ,
  my @jsonfields;
  for my $i(0..$#colnames) {
    push @jsonfields, "\"$colnames[$i]\":\"$cols[$i]\"";
  }
  my $jsonstr = "{" . join(",", @jsonfields) . "}";
  print JSON "$jsonstr\n";
}

close CSV;
close JSON;
