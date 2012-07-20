TicTacToe
=========

Simple tic-tac-toe game in Erlang.

License at the top of the module.

Modify the IP and PORT to play via network.

EXPORTS
=======

start_server/0 :

- starts the server

register_client/0 :

- registers a client<br/>
- returns the PID of the client

client/2 :

- implements the client API<br/>
- accepts PID and MESSAGE (in this order)<br/>
---> PID: the client PID as reported from register_client/0<br/>
---> MESSAGE: client message to be processed; can be:<br/>
------> an integer: 10*line+column<br/>
------> a string: sets name of the player (not used as ID in this version)<br/>
------> 'whoami': retrieves the name of the client<br/>
------> 'quit': removes the client<br/>
- returns the game status in human readable format

HOW TO PLAY
===========

1. Load the module an Erlang shell.<br/>
2. One player should start the server with tictactoe:start_server().<br/>
3. Use PID = tictactoe:register_client() to register a player and to hold his/her PID.<br/>
4. Use tictactoe:client(PID,Number) to define your move.

Enjoy! :)
