-module(ip2country).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

-record(ip_address_map, {range_from, range_to, country_code}).

for_each_line_in_file(Name, Proc, Mode, Accum0) ->
    {ok, Device} = file:open(Name, Mode),
    for_each_line(Device, Proc, Accum0).

for_each_line(Device, Proc, Accum) ->
    case io:get_line(Device, "") of
        eof  -> file:close(Device), Accum;
        Line -> NewAccum = Proc(Line, Accum),
                    for_each_line(Device, Proc, NewAccum)
    end.

% do this only once
init() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(ip_address_map, [{attributes, record_info(fields, ip_address_map)}]),
    mnesia:stop().

start() ->
    mnesia:start(),
    mnesia:wait_for_tables([ip_address_map], 20000).

stop() ->
    mnesia:stop().

load_data_from_csv_file(Filename) ->
  Lines = 
	  for_each_line_in_file(Filename,
    	fun(Line, Accum) -> 
    	  case string:left(Line, 1) of
    	    "\n"  -> %ignore blank lines
    	      Accum;
    	    "#" -> %ignore comments
    	      Accum;
  	      _ ->
  	        Params = string:tokens(Line, ","),
  	        Params2 = 
  	          lists:map(
  	            fun(Elem) -> string:strip(string:strip(Elem, both, $\n), both, $") end, 
  	            Params
  	          ), %strip quotes & newlines from both ends
  	          [Params2|Accum]
      end
    	end, [read], []),
  Lines.

extract_ip_ranges_and_country_code_from_csv_data(Data) ->
  lists:map(
    fun(Elem) -> 
      % see IpToCountry.csv for definitions of these
      [Ip_from, Ip_to, _Registry, _Assigned, Ctry, _Cntry, _Country] = Elem,
      {ip_address_map, list_to_integer(Ip_from), list_to_integer(Ip_to), Ctry}
    end, Data
  ).

do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

ip_address_as_integer_from_octet(Ip_address_as_octet) ->
    {Status, Address} = inet_parse:address(Ip_address_as_octet),
    case Status of
	    error ->
	      Ip_address_as_integer = {Status, Address};
	    ok ->
	      {A, B, C, D} = Address,
	      Ip_address_as_integer = D + C * 256 + B * 256 * 256 + A * 256 * 256 * 256
    end,
    Ip_address_as_integer.

dump() ->
  do(qlc:q([X|| X <- mnesia:table(ip_address_map)])).
			
lookup(Ip_address_as_octet) ->
  Ip_address_as_integer = ip_address_as_integer_from_octet(Ip_address_as_octet),
  do(qlc:q([X#ip_address_map.country_code || X <- mnesia:table(ip_address_map),
		     X#ip_address_map.range_from =< Ip_address_as_integer,
		     X#ip_address_map.range_to >= Ip_address_as_integer
			])).

reset_mnesia_database() ->
  mnesia:clear_table(ip_address_map),
   F = fun() ->
     lists:foreach(
	    fun mnesia:dirty_write/1, %transaction write is so slow!
	     extract_ip_ranges_and_country_code_from_csv_data(
	     load_data_from_csv_file("IpToCountry.csv")
	     ))
	   end,
	io:format("Finished loading data, writing to Mnesia~n"),
  F().
