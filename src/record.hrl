-ifndef(RECORD_HRL).
-define(RECORD_HRL, record_hrl).

-record(field_attr, {line, name, is_key = false, is_index = false, is_array = false, type}).

-endif.