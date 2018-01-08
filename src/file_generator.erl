-module(file_generator).
-author("ltb<lintingbin31@gmail.com>").

-export([generate/4]).

-include("record.hrl").

generate(BaseName, Data, Attrs, Opt) ->
  BaseNameLower = string:to_lower(BaseName),
  Prefix = csv2record:get_opt(record_prefix, Opt),
  RecordName = Prefix ++ "_" ++ BaseNameLower,
  generate_hrl_file(RecordName, Attrs, Opt),
  generate_erl_file(RecordName, Data, Attrs, Opt).

generate_hrl_file(RecordName, Attrs, Opt) ->
  FileName = RecordName ++ ".hrl",
  Dir = csv2record:get_opt(hrl_dir, Opt),
  Path = filename:join([Dir, FileName]),
  Str = build_hrl_file_str(RecordName, Attrs),
  {ok, Fd} = file:open(Path, [write]),
  io:format(Fd, Str, []),
  file:close(Fd).

build_hrl_file_str(RecordName, Attrs) ->
  Macro = RecordName ++ "_hrl_file",
  UpperMacro = string:to_upper(Macro),
  Fields = build_record_field_str(Attrs, []),
  "-ifndef(" ++ UpperMacro ++ ").\n"
  "-define(" ++ UpperMacro ++ ", " ++ Macro ++ ").\n\n"
  "-record(" ++ RecordName ++ ", {" ++ Fields ++ "}).\n\n"
  "-endif.\n\n".

build_record_field_str([], Fields) -> 
  string:join(lists:reverse(Fields), ", ");
build_record_field_str([#column_attr{name = Name, type = {_, Default}}| Tail], Fields) ->
  Field = lists:concat([Name, " = ", Default]),
  build_record_field_str(Tail, [Field| Fields]).

generate_erl_file(RecordName, Data, Attrs, Opt) ->
  FileName = RecordName ++ ".erl",
  Dir = csv2record:get_opt(src_dir, Opt),
  Path = filename:join([Dir, FileName]),
  Str = build_erl_file_str(RecordName, Attrs, Data),
  {ok, Fd} = file:open(Path, [write]),
  io:format(Fd, Str, []),
  file:close(Fd),
  Path.

build_erl_file_str(RecordName, Attrs, Data) ->
  {GetFunStr, AllKey} = build_get_fun_str(Data, Attrs, [], [], RecordName),
  GetAllKeysFunStr = build_get_all_key_fun_str(AllKey),
  {GetIndexFunStr, IndexFunName} = build_index_fun_strs(),
  "-module(" ++ RecordName ++ ").\n\n"
  "-include(\"" ++ RecordName ++ ".hrl\").\n\n"
  "-export([get/1, get_all_keys/0" ++ IndexFunName ++ "]).\n\n" ++ 
  GetFunStr ++ 
  GetAllKeysFunStr ++ 
  GetIndexFunStr.
  
build_get_fun_str([], _, Funs, Keys, _) ->
  FunStr = string:join(lists:reverse(Funs), ";\n"),
  GetFunStr = FunStr ++ ";\nget(_) -> not_found.\n\n",
  {GetFunStr, lists:reverse(Keys)};
build_get_fun_str([Line| Tail], Attrs, Funs, Keys, Name) ->
  {Key, Index, ValueStr} = build_value_str(Line, Attrs, [], [], []),
  FunStr = "get(" ++ Key ++ ") -> \n  #" ++ Name ++ ValueStr,
  set_index(Index, Key),
  build_get_fun_str(Tail, Attrs, [FunStr| Funs], [Key| Keys], Name).

build_value_str(Line, Field, Done, KeyList, Index) when Line =:= []; Field =:= [] ->
  Str = string:join(lists:reverse(Done), ", "),
  RealKey = 
    case length(KeyList) =:= 1 of
      true ->
        hd(KeyList);
      false ->
        KeyStr = string:join(lists:reverse(KeyList), ", "),
        lists:concat(["{", KeyStr, "}"])
    end,
  {RealKey, Index, "{" ++ Str ++ "}"};
build_value_str([Value| VTail], [Attr| CTail], Done, Key, Index) ->
  #column_attr{
    name = Name, 
    is_key = IsKey, 
    is_index = IsIndex, 
    is_array = IsArray
  } = Attr,
  RealValue = 
    case IsArray of
      false ->
        Value;
      true ->
        build_array_str(Value, [])
    end,
  NewOne = lists:concat([Name, " = ", RealValue]),
  NewKey = case IsKey of true -> [lists:concat([RealValue])| Key]; false -> Key end,
  NewIndex = case IsIndex of true -> [{Name, RealValue}| Index]; false -> Index end,
  build_value_str(VTail, CTail, [NewOne| Done], NewKey, NewIndex).

build_array_str([], List) ->
  ListStr = string:join(lists:reverse(List), ", "),
  "[" ++ ListStr ++ "]";
build_array_str([X| Tail], List) ->
  build_array_str(Tail, [lists:concat([X])| List]).

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

build_get_all_key_fun_str(AllKey) ->
  KeysStr = string:join(AllKey, ", "),
  "get_all_keys() -> \n  [" ++ KeysStr ++ "].\n\n".

build_index_fun_strs() ->
  DictList = [begin
    erase({dict, IndexName}),
    {IndexName, dict:to_list(Dict)} 
  end || {{dict, IndexName}, Dict} <- get()],
  build_index_fun_strs(DictList, [], []).

build_index_fun_strs([], FunStrList, FunNameList) ->
  FunStr = string:join(FunStrList, "\n") ++ "\n\n",
  FunNameStr = 
    case FunNameList of
      [] ->
        "";
      _ ->
        ", " ++ string:join(FunNameList, ", ")
    end,
  {FunStr, FunNameStr};
build_index_fun_strs([{IndexName, DictList}| Tail], FunStrList, FunNameList) ->
  FunName = "get_" ++ IndexName ++ "_keys",
  FunStr = build_index_fun_str(DictList, FunName, []),
  build_index_fun_strs(Tail, [FunStr| FunStrList], [FunName ++ "/1"| FunNameList]).

build_index_fun_str([], FunName, FunStrList) ->
  FunStr = string:join(FunStrList, ";\n"),
  FunStr ++ ";\n" ++ FunName ++ "(_) -> \n not_found.\n";
build_index_fun_str([{Key, List}| Tail], FunName, FunStrList) ->
  ListStr = string:join(List, ", "),
  FunStr = lists:concat([FunName, "(", Key, ") -> \n  [", ListStr, "]"]),
  build_index_fun_str(Tail, FunName, [FunStr| FunStrList]).
