-ifndef(ph_hrl).
-define(ph_hrl, 1).


-record(ph_service_info, {
    port,
    module,
    workers_sup,
    options
}).


-endif.

