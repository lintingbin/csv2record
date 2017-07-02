-module(csv_parser).
-author("ltb<lintingbin31@gmail.com>").

-export([parse/1]).

-include("record.hrl").

-define(CSV_COMMA, ",").
-define(CSV_LINE_END, "\r\n|\n|\r").
-define(ARRAY_SEMICOLON, ";").

parse(Name) ->
  case filelib:is_dir(Name) of
    true ->
      parse_csv_dir(Name);
    false ->
      parse_csv_file(Name)
  end.

parse_csv_dir(Dir) ->
  AllFiles = filelib:fold_files(Dir, ".csv$", false, fun(File, Acc) -> [File| Acc] end, []),
  [parse_csv_file(File) || File <- AllFiles].

parse_csv_file(File) ->
  case file:read_file(File) of
    {ok, BinData} ->
      Data = binary_to_list(BinData),
      AllLines = re:split(Data, ?CSV_LINE_END, [{return, list}, trim]),
      {AttrList, ContentLines} =
        case AllLines of
          [Types, Names| Tail] ->
            TypeList = re:split(Types, ?CSV_COMMA, [{return, list}]),
            NameList = re:split(Names, ?CSV_COMMA, [{return, list}]),
            {build_config(NameList, TypeList), Tail};
          _ ->
            exit({File, type_and_name_undefine})
        end,
      ErlData = parse_lines(ContentLines, AttrList, []),
      BaseName = filename:rootname(filename:basename(File)),
      write2file:write(BaseName, ErlData, AttrList);
    {error, Reason} ->
      exit({read_file, File, Reason})
  end.

build_config(NameList, TypeList) ->
  build_config(NameList, TypeList, [], 1).

build_config([], [], Config, _) ->
  lists:reverse(Config);
build_config([Name| NTail], [Attrs| TTail], Config, Cnt) ->
  NewCnt = Cnt + 1,
  FieldAttr = build_field_attr(Attrs),
  case FieldAttr#field_attr.type =:= undefined of
    true ->
      build_config(NTail, TTail, Config, NewCnt);
    false ->
      NewOne = FieldAttr#field_attr{line = Cnt, name = Name},
      build_config(NTail, TTail, [NewOne| Config], NewCnt)
  end.

build_field_attr(Attrs) ->
  build_field_attr(Attrs, #field_attr{}).

build_field_attr([], Attr) -> Attr;
build_field_attr([$K| Tail], Attr) ->
  build_field_attr(Tail, Attr#field_attr{is_key = true});
build_field_attr([$I| Tail], Attr) ->
  build_field_attr(Tail, Attr#field_attr{is_index = true});
build_field_attr([$N| Tail], Attr) ->
  build_field_attr(Tail, Attr#field_attr{type = {integer, 0}});
build_field_attr([$S| Tail], Attr) ->
  build_field_attr(Tail, Attr#field_attr{type = {string, "0"}});
build_field_attr([$B| Tail], Attr) ->
  build_field_attr(Tail, Attr#field_attr{type = {bool, false}});
build_field_attr([$A| Tail], Attr) ->
  build_field_attr(Tail, Attr#field_attr{is_array = true});
build_field_attr([_| Tail], Attr) ->
  build_field_attr(Tail, Attr).

parse_lines([], _, Res) -> 
  lists:reverse(Res);
parse_lines([Line| Tail], ConfList, Res) ->
  LineList = re:split(Line, ?CSV_COMMA),
  LineData = build_line(LineList, ConfList),
  parse_lines(Tail, ConfList, [LineData| Res]).

build_line(LineList, ConfList) ->
  build_line(LineList, ConfList, [], 1).

build_line([], _, Done, _Cnt) ->
  lists:reverse(Done);
build_line(_LineLists, [], Done, _Cnt) ->
  lists:reverse(Done);
build_line([Data| LTail], [#field_attr{line = Cnt} = Attr| CTail], Done, Cnt) ->
  NewOne = build_line_elment(Data, Attr),
  build_line(LTail, CTail, [NewOne| Done], Cnt + 1);
build_line([_Data| LTail], [_| CTail], Done, Cnt) ->
  build_line(LTail, CTail, Done, Cnt + 1).

build_line_elment(Data, #field_attr{is_array = IsArray, type = Type}) ->
  case IsArray of
    true ->
      DataList = re:split(Data, ?ARRAY_SEMICOLON),
      [trans2erlang_type(Type, X) || X <- DataList];
    false ->
      trans2erlang_type(Type, Data)
  end.

trans2erlang_type({_ErlType, Defalut}, "") ->
  Defalut;
trans2erlang_type({string, _Defalut}, Data) ->
  Str = [$"| binary_to_list(Data)] ++ [$"] ,
  io:format("Str ~w", [Str]),
  Str;
trans2erlang_type({integer, _Defalut}, Data) ->
  trans2erlang_integer(Data);
trans2erlang_type({bool, _Defalut}, <<"True">>) -> 
  true;
trans2erlang_type({bool, _Defalut}, <<"False">>) -> 
  false;
trans2erlang_type(_ErlType, Unkonw) ->
  exit({read_undefined_type, Unkonw}).

trans2erlang_integer(Data) ->
  try 
    binary_to_integer(Data) 
  catch _:_ ->
    exit({error_integer, Data})
  end.