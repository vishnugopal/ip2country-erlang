#!/usr/bin/env escript

-export([main/1]).

init() ->
  ip2country:start(),
  ip2country:reset_mnesia_database().
  
main([X]) ->
    init(),
    {TimeForLookup, V} = timer:tc(ip2country, lookup, [X]),
    [H|_] = V,
    io:format("Found ~s in ~f seconds.~n", [H, TimeForLookup * 0.000010]).

