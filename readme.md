## Overview
This is a functional WoW emulator written in Erlang. Currently it supports authentication, character creation and world login. It has been tested on vanilla WoW; may work with TBC but won't work with newer versions.

The emulator is made up of two parts, the login server and the world server.


### Login Server
The login server spawns a gen\_server for every client that connects. It uses [SRP6](http://srp.stanford.edu/design.html) to authenticate the client. The logon process can be seen [here](http://arcemu.org/wiki/Logon_Process).


### World Server
The world server has two parts:

* the world
* player process structures


#### The World
Currently the world is mostly unimplemented. This is where the world state would go, for example an octtree with all the players.

#### Player Process Structures
When a player is authenticated, the client connects to the world server through a "player structure". This structure is supervised by a simple\_one\_to\_one supervisor and are spawned when needed. The structure consists of a player supervisor which itself has three children: a gen\_fsm which acts as a socket receive process; a gen\_fsm which acts as a socket send process; a gen\_server which acts as the player controller.

When an incoming packet comes in, it is received and decrypted in the receive process, then sent to the controller. This has a table of callbacks to be executed based on the packet opcode. The callback is executed (if it is implemented) and the player state is updated if necessary. Then if a packet is to be sent back to the server, it is done in the send process.




## Usage
Start Erlang application using `Application:start(emulator).`. A testing account is automatically created with username and password: `Alice` `password123`. Create your character and login. Characters are only stored in memory so will not show up if you stop the application. You can create new accounts in the shell by using `account:create("username", "password").`. Stop the application with `Application:stop(emulator).`. Currently the application uses port 3784 to run the login server and port 8899 for the world server.



License
-------
This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
Street, Fifth Floor, Boston, MA 02110-1301 USA.

The full license is included in the file `License.md`.
