-module(ph_services_sup).

-behaviour(supervisor).

-include("include/ph.hrl").

-export([start_link/0]).
-export([init/1]).


start_link() ->
    supervisor:start_link(?MODULE, {}).


init(_) ->
    Children = [
        mk_child_spec(
            '0_tcp_echo', 
            #ph_service_info{
                proto = tcp,
                port = 50000,
                module = ph_handler_0_tcp_echo,
                workers_sup = ph_workers_sup_0,
                options = []
            }
        ),
        mk_child_spec(
            '1_prime_time', 
            #ph_service_info{
                proto = tcp,
                port = 50001,
                module = ph_handler_1_prime_time,
                workers_sup = ph_workers_sup_1,
                options = [{readline, true}]
            }
        ),
        mk_child_spec(
            '2_means_to_an_end', 
            #ph_service_info{
                proto = tcp,
                port = 50002,
                module = ph_handler_2_means_to_an_end,
                workers_sup = ph_workers_sup_2,
                options = []
            }
        ),
        mk_child_spec(
            '3_budget_chat',
            #ph_service_info{
                proto = tcp,
                port = 50003,
                module = ph_handler_3_budget_chat,
                workers_sup = ph_workers_sup_3,
                options = [{readline, true}]
            }
        ),
        mk_child_spec(
            '4_unusual_database',
            #ph_service_info{
                proto = udp,
                port = 50004,
                module = ph_handler_4_unusual_database,
                workers_sup = ph_workers_sup_4,
                options = []
            }
        )
    ],
    SupFlags = #{
        strategy => one_for_one
    },
    {ok, {SupFlags, Children}}.


mk_child_spec(Id, #ph_service_info{} = Si) ->
    #{
        id => Id,
        type => supervisor,
        start => {ph_service_sup, start_link, [Si]}
    }.

