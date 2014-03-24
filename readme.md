This is a bare bones implementation of an authentication server based on the MaNGOS. Currently it can authenticate one single user whose credentials are hardcoded. Once the user is authenticated, the authenticator does nothing more since there is no game server for it to hand the user off to.

This is called 'erlang-test' because it is very simple, it has minimal error testing, it does nothing upon successful authentication and the code is not organized very well. It is simply a test to see if the MaNGOS authentication process could be replicated in Erlang. It can.

Flow: user connects via port 3724. Authentication is done via SRP6a. If authentication fails, an error will occur on the server. If authentication is successful, the server just sits there. (I told you it was simple...)
