{application, sockserv,
[{vsn, "1.0.0"},
{modules, [client, client_sup, sockserv_sup, sockserv_app, sockserv_serv, logon_lib, worldserv_sup, worldserv_serv, world_crypto]},
{registered, [sockserv_sup, sockserv_serv, worldserv_serv, worldserv_sup, client, client_sup]},
{applications, [stdlib, kernel]},
{mod, {sockserv_app, []}},
{env,
 [{realm_port, 3724},
 {world_port, 8640}
 ]}
]}.
