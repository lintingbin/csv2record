csv2record
====
A conversion tool for csv to erlang record.

Introduction
----
Usually a large number of configurations in the game server are stored through the csv file. In order to read these configurations, we will convert csv format into record format, and then write to erl file. The above is the main purpose of the tool.

Csv file format
----
In order for the tool to work properly, we need to define the correct csv file format. We need to use the first line of the csv file to declare the type of data, and then use the second line to declare the name of the record field. For example:

KN|S|NI|B|AS
|:---:|:---:|:---:|:---:|:---:|
|**MapId**|**Name**|**Type**|**IsBool**|**Mons**|
|1|text1|1|True|1a;2;3|
|2|text2|1|False|4df;53f;6|
|3|text3|1|True|8;2;3asdf|
|4|text4|2|False|3;2;3fd;1|

In the above csv table, the first line is the type(Can be combined), the second line is the name of the record field. As shown below, all types are single uppercase letters:

|K|I|A|N|B|S|O|
|:---:|:---:|:---:|:---:|:---:|:---:|:---:|
Key|Index|Array|Number|Boolean|String|Omit

**Key**, **Index** and **Array** can be used with **Number**, **Boolean** and **String**. A csv table has at least one Key and zero or more Index. The above csv table will eventually be converted to the following .hrl file and .erl file:
``` erlang
-ifndef(CSV_MAP_HRL_FILE).
-define(CSV_MAP_HRL_FILE, csv_map_hrl_file).

-record(csv_map, {mapid = 0, name = "", type = 0, isbool = false, mons = ""}).

-endif.
```
``` erlang
-module(csv_map).

-include("csv_map.hrl").

-export([get/1, get_all_keys/0, get_type_keys/1]).

get(1) -> 
  #csv_map{mapid = 1, name = "text1", type = 1, isbool = true, mons = ["1a", "2", "3"]};
get(2) -> 
  #csv_map{mapid = 2, name = "text2", type = 1, isbool = false, mons = ["4df", "53f", "6"]};
get(3) -> 
  #csv_map{mapid = 3, name = "text3", type = 1, isbool = true, mons = ["8", "2", "3asdf"]};
get(4) -> 
  #csv_map{mapid = 4, name = "text4", type = 2, isbool = false, mons = ["3", "2", "3fd", "1"]};
get(_) -> not_found.

get_all_keys() -> 
  [1, 2, 3, 4].

get_type_keys(1) -> 
  [1, 2, 3];
get_type_keys(2) -> 
  [4];
get_type_keys(_) -> 
 not_found.
```
Usage
----
Compile the project, and then execute the following command:
``` erlang
csv2record:generate(File).  
csv2record:generate(File, Option).  
% Types 
%   File = CsvFilePath | CsvDir
%   Option = List = [{Atom, String}| List]
%   Atom = 
%     hrl_dir(.hrl file output directory. Default value: "csv_hrl_dir") |
%     src_dir(.erl file output directory. Default value: "csv_src_dir") |
%     ebin_dir(.beam file output directory. Default value: "ebin") |
%     record_prefix(The prefix of record name. Default: "csv")
```
Or create a escript executable file:
``` erlang
rebar3 escriptize
_build/default/bin/csv2record $csv_dir $csv_opt  
An example of $csv_opt: "ebin_dir=ebin;hrl_dir=include;src_dir=src;"
```

Discussing
----
- [Submit issue](https://github.com/lintingbin2009/csv2record/issues)
- Email: lintingbin31@gmail.com
