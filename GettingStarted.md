# Introduction #

ip2country-erlang imports iptocountry database into mnesia and then performs lookups on it. It's meant to be used as a library. A small test wrapper is available that performs some benchmarking.


# Get Started #

  * Checkout the latest code by doing `svn checkout http://ip2country-erlang.googlecode.com/svn/trunk/ ip2country-erlang`
  * Get the IpToCountry.csv.gz file available [here](http://software77.net/cgi-bin/ip-country/geo-ip.pl?action=download) and gunzip it to the same location as the project files overwriting the IpToCountry.csv file in the repository. The default file is just a truncated placeholder.
  * Start an erlang shell and do `c(ip2country).` and `ip2country:init().`
  * Execute `./ip2country_test.erl` giving an IP address as parameter and everything should start working!
  * To use in your scripts, make sure the beam files are in path and then do `ip2country:reset_mnesia_database()` and `ip2country:lookup("valid.ip.add.ress")`. That's it!







