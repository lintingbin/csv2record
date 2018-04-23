-module(csv_parser).
-author("ltb<lintingbin31@gmail.com>").

-export([parse/1]).

-include("record.hrl").

-define(CSV_COMMA, ",").
-define(CSV_LINE_END, "\r\n|\n|\r").
-define(ARRAY_SEMICOLON, ";").

parse(File) ->
  put(file, File),
  case file:read_file(File) of
    {ok, BinData} ->
      Data = binary_to_list(BinData),
      AllLines = re:split(Data, ?CSV_LINE_END, [{return, list}, trim]),
      {Attrs, Lines} =
        case AllLines of
          [Types, Names| Tail] ->
            TypeList = re:split(Types, ?CSV_COMMA, [{return, list}]),
            NameList = re:split(Names, ?CSV_COMMA, [{return, list}]),
            {build_attr(NameList, TypeList), Tail};
          _ ->
            error_exit(type_and_name_undefined)
        end,
      ErlData = parse_lines(Lines, Attrs, []),
      {ErlData, Attrs};
    {error, Reason} ->
      error_exit(Reason)
  end.

build_attr(NameList, TypeList) ->
  build_attr(NameList, TypeList, [], 1).

build_attr([], [], Attrs, _) ->
  lists:reverse(Attrs);
build_attr([Name| Ntail], [Types| Ttail], Attrs, Column) ->
  NewColumn = Column + 1,
  Attr = build_column_attr(Types),
  case Attr#column_attr.type =:= undefined of
    true ->
      build_attr(Ntail, Ttail, Attrs, NewColumn);
    false ->
      LowerName = string:to_lower(Name),
      AtomName = io_lib:format("~w", [list_to_atom(LowerName)]),
      NewAttr = Attr#column_attr{col = Column, name = AtomName},
      build_attr(Ntail, Ttail, [NewAttr| Attrs], NewColumn)
  end.

build_column_attr(Attrs) ->
  build_column_attr(Attrs, #column_attr{}).

build_column_attr([], Attr) -> Attr;
build_column_attr([$O| _], Attr) -> Attr;
build_column_attr([$K| Tail], Attr) ->
  build_column_attr(Tail, Attr#column_attr{is_key = true});
build_column_attr([$I| Tail], Attr) ->
  build_column_attr(Tail, Attr#column_attr{is_index = true});
build_column_attr([$A| Tail], Attr) ->
  build_column_attr(Tail, Attr#column_attr{is_array = true});
build_column_attr([$N| Tail], Attr) ->
  build_column_attr(Tail, Attr#column_attr{type = {integer, 0}});
build_column_attr([$S| Tail], Attr) ->
  build_column_attr(Tail, Attr#column_attr{type = {string, [$", $"]}});
build_column_attr([$B| Tail], Attr) ->
  build_column_attr(Tail, Attr#column_attr{type = {bool, false}});
build_column_attr([_| Tail], Attr) ->
  build_column_attr(Tail, Attr).

parse_lines([], _, Res) -> 
  lists:reverse(Res);
parse_lines([Line| Tail], Attrs, Res) ->
  List = line_to_list(Line, []),
  Data = parse_line(List, Attrs),
  parse_lines(Tail, Attrs, [Data| Res]).

line_to_list(last_is_empty, Done) ->
  lists:reverse([[]| Done]);
line_to_list([], Done) -> 
  lists:reverse(Done);
line_to_list([$"| Tail], Done) ->
  {One, Tail1} = get_field_from_list(Tail, [], true),
  line_to_list(Tail1, [One| Done]);
line_to_list(List, Done) ->
  {One, Tail1} = get_field_from_list(List, [], false),
  line_to_list(Tail1, [One| Done]).

get_field_from_list([$,], Done, _IsOne) ->
  {lists:reverse(Done), last_is_empty};
get_field_from_list([], Done, _IsOne) ->
  {lists:reverse(Done), []};
get_field_from_list([$",$,| Tail], Done, _IsOne) ->
  {lists:reverse(Done), Tail};
get_field_from_list([$,| Tail], Done, true) ->
  get_field_from_list(Tail, [$,| Done], true);
get_field_from_list([$,| Tail], Done, false) ->
  {lists:reverse(Done), Tail};
get_field_from_list([C| Tail], Done, IsOne) -> 
  get_field_from_list(Tail, [C| Done], IsOne).

parse_line(List, Attrs) ->
  parse_line(List, Attrs, [], 1).

parse_line(List, Attrs, Done, _) when List =:= []; Attrs =:= [] ->
  lists:reverse(Done);
parse_line([Element| Ltail], [#column_attr{col = Col} = Attr| Atail], Done, Col) ->
  ErlData = parse_elment(Element, Attr),
  parse_line(Ltail, Atail, [ErlData| Done], Col + 1);
parse_line([_| Ltail], Atail, Done, Col) ->
  parse_line(Ltail, Atail, Done, Col + 1).

parse_elment(Element, #column_attr{is_array = IsArray, type = Type}) ->
  case IsArray of
    true when Element =:= [] ->
      [];
    true ->
      ElementList = re:split(Element, ?ARRAY_SEMICOLON, [{return, list}]),
      [trans2erlang_type(Type, X) || X <- ElementList];
    false ->
      trans2erlang_type(Type, Element)
  end.

trans2erlang_type({_ErlType, Defalut}, []) ->
  Defalut;
trans2erlang_type({string, _Defalut}, Data) ->
  [$"| Data] ++ [$"];
trans2erlang_type({integer, _Defalut}, Data) ->
  trans2erlang_integer(Data);
trans2erlang_type({bool, Defalut}, BoolStr) -> 
  trans2erlang_bool(Defalut, BoolStr);
trans2erlang_type(ErlType, Unkonw) ->
  error_exit({unexcept_type, ErlType, Unkonw}).

trans2erlang_integer(Data) ->
  try 
    list_to_integer(Data)
  catch _:_ ->
    try 
      list_to_float(Data)
    catch _:_ ->
      error_exit({error_integer, Data})
    end
  end.

trans2erlang_bool(Default, BoolStr) ->
  BoolAtom = 
    try
      string:lowercase(BoolStr)
    catch _:_ ->
      try 
        string:to_lower(BoolStr)
      catch _:_ ->
          error_exit({error_bool, BoolStr})
      end
    end,
  Bool = list_to_atom(BoolAtom),
  case Bool =:= false orelse Bool =:= true of
    true ->
      Bool;
    false ->
      error_exit({unexcept_type, {bool, Default}, BoolStr})
  end.

error_exit(Error) ->
  FileName = 
    case get(file) of
      undefined -> 
        unknown_file;
      Name ->
        Name
    end,
  exit({FileName, Error}).
