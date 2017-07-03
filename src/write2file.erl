-module(write2file).
-author("ltb<lintingbin31@gmail.com>").

-export([write/4]).

-include("record.hrl").

write(BaseName, ErlData, AttrList, Opt) ->
  Name = string:to_lower(BaseName),
  write_erl_file(Name, ErlData, AttrList, Opt),
  write_hrl_file(Name, AttrList).

write_erl_file(Name, ErlData, AttrList, Opt) ->
  {FunStr, IndexFunName} = build_function_str(ErlData, AttrList, [], [], Name),
  ErlStr = build_erl_file_str(Name, FunStr, IndexFunName),
  Dir = csv2record:get_opt(src_dir, Opt),
  FileName = filename:join([Dir, Name, ".erl"]),
  {ok, IoDevice} = file:open(FileName, [write]),
  io:format(IoDevice, ErlStr, []),
  file:close(IoDevice),
  erase().
  
build_function_str([], _, Funs, Keys, _) ->
  FunLists = lists:reverse(Funs),
  KeyFunStr = string:join(FunLists, ";\n"),
  KeysStr = string:join(Keys, ", "),
  GetAllKeysFunStr = lists:concat(["get_all_keys() -> \n  [", KeysStr, "].\n"]),
  {GetIndexFunStr, IndexFunName} = build_index_function_str(),
  FunStr = lists:concat([KeyFunStr, ".\n\n", GetAllKeysFunStr, "\n", GetIndexFunStr]),
  {FunStr, IndexFunName};
build_function_str([Line| Tail], AttrList, Funs, Keys, Name) ->
  {Key, Index, ValueStr} = build_value_str(Line, AttrList, [], [], []),
  FunStr = lists:concat(["get(", Key, ") -> \n  #", Name, ValueStr]),
  set_index(Index, Key),
  build_function_str(Tail, AttrList, [FunStr| Funs], [Key| Keys], Name).

build_index_function_str() ->
  DictList = [{IndexName, dict:to_list(Dict)} || {{dict, IndexName}, Dict} <- get()],
  build_index_function_str(DictList, [], []).

build_index_function_str([], Done, IndexFun) ->
  IndexFunStr = string:join(Done, "\n"),
  IndexFunStr1 = lists:concat([IndexFunStr, "\n\n"]),
  IndexFunName = string:join(IndexFun, ", "),
  IndexFunName1 = 
    case IndexFun of
      [] ->
        IndexFunName;
      _ ->
        lists:concat([", ", IndexFunName])
    end,
  {IndexFunStr1, IndexFunName1};
build_index_function_str([{IndexName, DictList}| Tail], Done, IndexFun) ->
  IndexFunName = lists:concat(["get_", IndexName, "_keys"]),
  FunStr = build_index_function_str_one(DictList, IndexFunName, []),
  build_index_function_str(Tail, [FunStr| Done], [lists:concat([IndexFunName, "/1"])| IndexFun]).

build_index_function_str_one([], _, Done) ->
  IndexFunStr = string:join(Done, ";\n"),
  lists:concat([IndexFunStr, ".\n\n"]);
build_index_function_str_one([{Key, Value}| Tail], IndexFunName, Done) ->
  RealValue = string:join(Value, ", "),
  FunStr = lists:concat([IndexFunName, "(", Key, ") -> \n  [", RealValue, "]"]),
  build_index_function_str_one(Tail, IndexFunName, [FunStr| Done]).

set_index([], _) -> ok;
set_index([{IndexName, IndexValue}| Tail], Key) ->
  case get({dict, IndexName}) of
    undefined ->
      Dict = dict:new(),
      Dict1 = dict:append(IndexValue, Key, Dict),
      put({dict, IndexName}, Dict1);
    Dict ->
      Dict1 = dict:append(IndexValue, Key, Dict),
      put({dict, IndexName}, Dict1)
  end,
  set_index(Tail, Key).

build_value_str(Line, Field, Done, Key, Index) when Line =:= []; Field =:= [] ->
  ReverseDone = lists:reverse(Done),
  Str = string:join(ReverseDone, ", "),
  RealKey = 
    case length(Key) =:= 1 of
      true ->
        io_lib:format("~w", [hd(Key)]);
      false ->
        io_lib:format("~w", [list_to_tuple(lists:reverse(Key))])
    end,
  {RealKey, Index, lists:concat(["{", Str, "}"])};
build_value_str([Value| VTail], [FieldAttr| CTail], Done, Key, Index) ->
  #field_attr{
    name = Name, 
    is_key = IsKey, 
    is_index = IsIndex, 
    is_array = IsArray
  } = FieldAttr,
  RealValue = 
    case IsArray of
      true ->
        build_array_str(Value, []);
      false ->
        Value
    end,
  NewOne = lists:concat([Name, " = ", RealValue]),
  NewKey = case IsKey of true -> [RealValue| Key]; false -> Key end,
  NewIndex = case IsIndex of true -> [{Name, RealValue}| Index]; false -> Index end,
  build_value_str(VTail, CTail, [NewOne| Done], NewKey, NewIndex).

build_array_str([], Done) ->
  ReverseDone = lists:reverse(Done),
  Str = string:join(ReverseDone, ", "),
  lists:concat(["[", Str, "]"]);
build_array_str([X| Tail], Done) ->
  Str = lists:concat([X]),
  build_array_str(Tail, [Str| Done]).

build_erl_file_str(Name, FunStr, IndexFunName) ->
  List = ["-module(", Name, ").\n\n"
  "-include(\"", Name, ".hrl\").\n\n"
  "-export([get/1, get_all_keys/0", IndexFunName, "]).\n\n",
  FunStr],
  lists:concat(List).

write_hrl_file(Name, AttrList) ->
  Macro = lists:concat([Name, "_hrl_file"]),
  UpperMacro = string:to_upper(Macro),
  Fields = build_record_field(AttrList, []),
  FileName = lists:concat([Name, ".hrl"]),
  {ok, IoDevice} = file:open(FileName, [write]),
  FileStr = build_record_file_str(UpperMacro, Macro, Name, Fields),
  io:format(IoDevice, FileStr, []),
  file:close(IoDevice).

build_record_field([], Done) -> 
  ReverseDone = lists:reverse(Done),
  Str = string:join(ReverseDone, ", "),
  lists:concat(["{", Str, "}"]);
build_record_field([#field_attr{name = Name, type = {_, Default}}| Tail], Done) ->
  NewOne = lists:concat([Name, " = ", Default]),
  build_record_field(Tail, [NewOne| Done]).

build_record_file_str(UpperMacro, Macro, Name, Fields) ->
  List = ["-ifndef(", UpperMacro, ").\n"
  "-define(", UpperMacro, ", ", Macro, ").\n\n"
  "-record(", Name, ", ", Fields, ").\n\n"
  "-endif."],
  lists:concat(List).