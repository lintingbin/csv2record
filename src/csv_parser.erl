-module(csv_parser).
-author("ltb<lintingbin31@gmail.com>").

-export([parse/2]).

-include("record.hrl").

-define(CSV_COMMA, ",").
-define(CSV_LINE_END, "\r\n|\n|\r").
-define(ARRAY_SEMICOLON, ";").

parse(File, Opt) ->
  put(file, File),
  case file:read_file(File) of
    {ok, BinData} ->
      Data = binary_to_list(BinData),
      AllLines = re:split(Data, ?CSV_LINE_END, [{return, list}, trim]),
      {AttrList, ContentLines} =
        case AllLines of
          [Types, Names| Tail] ->
            TypeList = re:split(Types, ?CSV_COMMA, [{return, list}]),
            NameList = re:split(Names, ?CSV_COMMA, [{return, list}]),
            {build_attr(NameList, TypeList), Tail};
          _ ->
            error_exit(type_and_name_undefined)
        end,
      ErlData = parse_lines(ContentLines, AttrList, []),
      {ErlData, AttrList};
    {error, Reason} ->
      error_exit(Reason)
  end.

build_attr(NameList, TypeList) ->
  build_attr(NameList, TypeList, [], 1).

build_attr([], [], Config, _) ->
  lists:reverse(Config);
build_attr([Name| NTail], [Attrs| TTail], Config, Cnt) ->
  NewCnt = Cnt + 1,
  FieldAttr = build_field_attr(Attrs),
  case FieldAttr#field_attr.type =:= undefined of
    true ->
      build_attr(NTail, TTail, Config, NewCnt);
    false ->
      LowerName = string:to_lower(Name),
      AtomName = io_lib:format("~w", [list_to_atom(LowerName)]),
      NewOne = FieldAttr#field_attr{line = Cnt, name = AtomName},
      build_attr(NTail, TTail, [NewOne| Config], NewCnt)
  end.

build_field_attr(Attrs) ->
  build_field_attr(Attrs, #field_attr{}).

build_field_attr([], Attr) -> Attr;
build_field_attr([$K| Tail], Attr) ->
  build_field_attr(Tail, Attr#field_attr{is_key = true});
build_field_attr([$I| Tail], Attr) ->
  build_field_attr(Tail, Attr#field_attr{is_index = true});
build_field_attr([$A| Tail], Attr) ->
  build_field_attr(Tail, Attr#field_attr{is_array = true});
build_field_attr([$N| Tail], Attr) ->
  build_field_attr(Tail, Attr#field_attr{type = {integer, 0}});
build_field_attr([$S| Tail], Attr) ->
  build_field_attr(Tail, Attr#field_attr{type = {string, [$", $"]}});
build_field_attr([$B| Tail], Attr) ->
  build_field_attr(Tail, Attr#field_attr{type = {bool, false}});
build_field_attr([_| Tail], Attr) ->
  build_field_attr(Tail, Attr).

parse_lines([], _, Res) -> 
  lists:reverse(Res);
parse_lines([Line| Tail], Attr, Res) ->
  LineList = re:split(Line, ?CSV_COMMA),
  LineData = build_line(LineList, Attr),
  parse_lines(Tail, Attr, [LineData| Res]).

build_line(LineList, Attr) ->
  build_line(LineList, Attr, [], 1).

build_line([], _, Done, _) ->
  lists:reverse(Done);
build_line(_, [], Done, _) ->
  lists:reverse(Done);
build_line([Data| LTail], [#field_attr{line = Cnt} = Attr| ATail], Done, Cnt) ->
  NewOne = build_line_elment(Data, Attr),
  build_line(LTail, ATail, [NewOne| Done], Cnt + 1);
build_line([_Data| LTail], ATail, Done, Cnt) ->
  build_line(LTail, ATail, Done, Cnt + 1).

build_line_elment(Data, #field_attr{is_array = IsArray, type = Type}) ->
  case IsArray of
    true ->
      DataList = re:split(Data, ?ARRAY_SEMICOLON),
      [trans2erlang_type(Type, X) || X <- DataList];
    false ->
      trans2erlang_type(Type, Data)
  end.

trans2erlang_type({_ErlType, Defalut}, <<>>) ->
  Defalut;
trans2erlang_type({string, _Defalut}, Data) ->
  [$"| binary_to_list(Data)] ++ [$"];
trans2erlang_type({integer, _Defalut}, Data) ->
  trans2erlang_integer(Data);
trans2erlang_type({bool, _Defalut}, <<"True">>) -> 
  true;
trans2erlang_type({bool, _Defalut}, <<"False">>) -> 
  false;
trans2erlang_type(_ErlType, Unkonw) ->
  error_exit({unexcept_type, Unkonw}).

trans2erlang_integer(Data) ->
  try 
    binary_to_integer(Data) 
  catch _:_ ->
    try 
      binary_to_float(Data)
    catch _:_ ->
      error_exit({error_integer, Data})
    end
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