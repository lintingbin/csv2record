-module(csv2record).
-author("ltb<lintingbin31@gmail.com>").

-export([generate/1, generate/2, get_opt/2]).

generate(Path) ->
  generate(Path, []).

generate(Path, Opt) ->
  case filelib:is_dir(Path) of
    true ->
      generate_files(Path, Opt);
    false ->
      generate_file(Path, undefined, Opt)
  end.

generate_files(Dir, Opt) ->
  Pid = self(),
  AllCsvFiles = filelib:fold_files(Dir, ".csv$", false, fun(File, Acc) -> [File| Acc] end, []),
  [proc_lib:spawn_link(fun() -> generate_file(File, Pid, Opt) end) || File <- AllCsvFiles],
  loop(length(AllCsvFiles)).

loop(0) -> ok;
loop(N) ->
  receive
    {parse_success, File} ->
      io:format("file ~p parse success~n", [File]);
    Other ->
      io:format("unexcept msg ~p~n", [Other])
  end,
  loop(N - 1).

generate_file(Path, Pid, Opt) ->
  {Data, AttrList} = csv_parser:parse(Path, Opt),
  BaseName = filename:rootname(filename:basename(Path)),
  make_dir(Opt),
  write2file:write(BaseName, Data, AttrList, Opt),
  case Pid =:= undefined of
    true ->
      ok;
    false ->
      Pid ! {parse_success, Path}
  end.

make_dir(Opt) ->
  HrlDir = get_opt(hrl_dir, Opt),
  SrcDir = get_opt(src_dir, Opt),
  EbinDir = get_opt(ebin_dir, Opt),
  [make_dir_proc(X) || X <- [HrlDir, SrcDir, EbinDir]].

make_dir_proc(Dir) ->
  case filelib:is_dir(Dir) of
    true ->
      ok;
    false ->
      file:make_dir(Dir)
  end.

get_opt(Key, Opt) ->
  case lists:keyfind(Key, 1, Opt) of
    false ->
      get_default_opt(Key);
    {Key, Value} ->
      Value
  end.

get_default_opt(hrl_dir) ->
  "csv_hrl_dir";
get_default_opt(src_dir) ->
  "csv_source_dir";
get_default_opt(ebin_dir) ->
  "ebin";
get_default_opt(record_prefix) ->
  "".
