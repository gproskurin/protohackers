-ifndef(ph_hrl).
-define(ph_hrl, 1).


-record(ph_service_info_tcp, {
    port,
    module,
    workers_sup,
    options
}).

-record(ph_service_info_udp, {
    port,
    module
}).


-endif.

