### Overview
This is a functional WoW emulator written in Erlang. Currently it supports authentication, character creation and world login.


### Usage
Start Erlang application using `Application:start(emulator).`. A testing account is created with username and password: `Alice` `password123`. Create your character and login. Characters are only stored in memory so will not show up if you stop the application. You can create new accounts in the shell by using `account:create("username", "password").`. Stop the application with `Application:stop(emulator).`. Currently the application uses port 3784 to run the login server and port 8899 for the world server.
