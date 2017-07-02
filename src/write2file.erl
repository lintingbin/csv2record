-module(write2file).
-author("ltb<lintingbin31@gmail.com>").

-export([write/3]).

-include("record.hrl").

write(BaseName, ErlData, AttrList) ->
  write_erl_file(BaseName, ErlData, AttrList),
  write_hrl_file(BaseName, AttrList).

write_erl_file(Name, ErlData, AttrList) ->
  FunStr = build_function_str(ErlData, AttrList, [], Name),
  ErlStr = build_erl_file_str(Name, FunStr),
  FileName = lists:concat([Name, ".erl"]),
  {ok, IoDevice} = file:open(FileName, [write]),
  io:format(IoDevice, ErlStr, []),
  file:close(IoDevice).
  
build_function_str([], _, Done, _) ->
  ReverseDone = lists:reverse(Done),
  string:join(ReverseDone, "\n");
build_function_str([Line| Tail], AttrList, Done, Name) ->
  {Key, Index, ValueStr} = build_value_str(Line, AttrList, [], undefined, undefined),
  FunStr = lists:concat(["get(", Key, ") -> \n  #", Name, ValueStr]),
  build_function_str(Tail, AttrList, [FunStr| Done], Name).

build_value_str([], [], Done, Key, Index) ->
  ReverseDone = lists:reverse(Done),
  Str = string:join(ReverseDone, ", "),
  {Key, Index, lists:concat(["{", Str, "}.\n"])};
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
  NewKey = case IsKey of true -> RealValue; false -> Key end,
  NewIndex = case IsIndex of true -> RealValue; false -> Index end,
  build_value_str(VTail, CTail, [NewOne| Done], NewKey, NewIndex).

build_array_str([], Done) ->
  ReverseDone = lists:reverse(Done),
  Str = string:join(ReverseDone, ", "),
  lists:concat(["[", Str, "]"]);
build_array_str([X| Tail], Done) ->
  Str = lists:concat([X]),
  build_array_str(Tail, [Str| Done]).

build_erl_file_str(Name, FunStr) ->
  List = ["-module(", Name, ").\n\n"
  "-include(", Name, ".hrl).\n\n"
  "-export([get/1, get_index_list/2, all/0]).\n\n",
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


