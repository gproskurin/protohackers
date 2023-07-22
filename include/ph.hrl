-ifndef(ph_hrl).
-define(ph_hrl, 1).


-record(ph_service_info, {
    proto :: tcp | udp,
    port,
    module,
    workers_sup,
    options
}).


-endif.

