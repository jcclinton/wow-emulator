{application, emulator,
[{vsn, "1.0.0"},
{modules, [emulator_app, emulator_sup, client, login_server_sup, login_server, world_server_sup, world_socket_server_sup, world_socket_rcv_sup, world_socket_send_sup, world_socket_controller, world_socket_rcv, world_socket_send]},
{registered, [emulator_sup, client, login_server_sup, world_server_sup]},
{applications, [stdlib, kernel]},
{mod, {emulator_app, []}},
{env,
 [{realm_port, 3724},
 {world_port, 8899},
 {world_ip, "127.0.0.1"}
 ]}
]}.
