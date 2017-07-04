-ifndef(RECORD_HRL).
-define(RECORD_HRL, record_hrl).

-record(column_attr, {col, name, is_key = false, is_index = false, is_array = false, type}).

-endif.