-ifdef(dbg).
-include_lib("eunit/include/eunit.hrl").
-else.
-define(debugFmt(X,Y),ok).
-endif.

-define(EncStateWindowLength, 1000).
