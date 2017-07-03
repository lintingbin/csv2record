-module(map).

-include("map.hrl").

-export([get/1, get_all_keys/0]).

get(1) -> 
  #map{mapid = 1, name = "", type = 1, canattack = true, mons = ["1", "2", "3"]};
get(2) -> 
  #map{mapid = 2, name = "aa2", type = 1, canattack = false, mons = ["4", "5", "6"]};
get(3) -> 
  #map{mapid = 3, name = "aa3", type = 1, canattack = true, mons = ["8", "2", "3"]};
get(4) -> 
  #map{mapid = 4, name = "测试4", type = 2, canattack = false, mons = ["3", "2", "3", "1"]}.

get_all_keys() -> 
  [4, 3, 2, 1].



