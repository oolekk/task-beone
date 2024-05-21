# Recruitment task Beone project

Your task is to create an application, that will enable the user to move chess pieces on a chess table, using REST API.

![chess.png](doc%2Fchess.png)

### Domain Requirements
1. Chess table is always 8x8 and each field can be described as a tuple e.g. (3,0), both values can be int, there is no need to keep the original chess position naming like "A4". 
1. On a single field there can be always only a single chess piece (pieces are of the same color so they can't interact with each other).
1. There are only two possible chess pieces: rook (The rook moves horizontally or vertically, through any number of unoccupied squares) and bishop (The bishop moves diagonally in any direction it wishes and as far as it wishes as long as the squares are free).
1. Each piece can be removed from the table, but we need to keep its last position, a piece that was removed can't be placed on the table again
1. There should be a possibility to put a new piece (of a given type) on an empty chess field and assign a unique ID (you can choose how ID should look like) to it during this action.
1. There should be a possibility to move a piece to a selected field given an id of the chess piece that is on the table.

### Technical Requirements:
1. Use this repository as a backbone to implement functionality under the '/application' directory.
1. To implement REST API, please use ZIO and Tapir (application should also host swagger)
1. You are free to choose how to persist the data, but the application in case of the unexpected crash should not lose the state of the chess board or any previously created chess piece
1. Each successful action should be emitted to Kafka "events" topic (adding piece, removing piece, moving piece)
1. Feel free to extend Dependencies.scala but keep in mind that using Scala, tapir, ZIO and kafka is mandatory!

For convenience and easier testing, you are provided with /client which should read and log all messages that were sent to
the configured topic. Also, you can use file under /docker-compose to set up Kafka for testing the implementation. If you need feel free to extend compose file.

# Solution and usage explanation

To start playing the game please run the `PlayReplApp`
It is supported to play with service offline, sync will occur on first re-connect.
Broken connectivity behaves in similar fashion, outstanding changes are pushed on reconnect.
However, for full experience it makes sense to run the `RestServiceApp`.

As a pre-requisite we need to spin up docker compose with `docker compose up`.
It is also needed to go to kafka container and create the `events` topic there.
`kafka-console-consumer --bootstrap-server localhost:9092 --topic events`
Redis DB will also be started, all is default, no further configuration needed there.
Each 5 seconds the in-memory backlog is pushed to persistent storage.

Once docker compose is started, we can also start the separate kafka client.
`ConsumerApp` can be run in separate tab, and will print board action events
as they are published by the producer. No special configuration was done to resume from
last offset, it will not replay previously missed events but those which occur while it
is running will be displayed.

`PlayReplApp` will accept commands via terminal to add pieces on board and to move them around.
There's more full worded commands which can be readily inspected in the `CmdParser`
I will quickly list the short versions which I used during testing. Location or ID can
be used to indicate which piece should be acted on.

All the numeric X Y args don't need to be separated by spaces, but can be.
Since it's always one digit numbers, 0-7 which are valid for X Y, each single digit is
treated as separate coordinate.

`r 01` will add rook on field (0, 1) `add rook` `new rook` `put rook` does the same

`b 10` will add bishop on field (1, 0) `add bishop` `new bishop` `put bishop` does the same

`rm 11` will remove any piece on that field, `take`, `rmv`, `remove`, `delete` works the same

`info 37` will print info of a piece at (3, 7)

`info` will print info and in-depth stats about all the pieces on and off the board

`mv 11 22` will attempt to move piece from (1,1) to (2,2) it is validated both locally and remotely

`rand` will randomly populate the board for a quick-start experience

`exit` will exit the repl

`restart` will start new repl with new empty game

`load` will load last saved state from the server, must be followed by gameId, which is printed
above the board  after each move, or after no-op, such as just pressing enter.

Each time there is a move, saving is attempted, but if it fails (server can be turned off to test it)
game continues unhindered, only informs that save did not succeed. On each move, all un-saved actions
are again pushed to server, and game will be in-sync on first reconnect. All events will be emitted.

We can switch to some previous game by running `load`.
Normally, when game is started, random game-id is assigned. We can also switch to a
random game by running `rand`.

There is variants of commands which work with rook or bishop id, as it was requested in the task
description. The id is simply the sequence number, each next piece has the next id, starting with 1.
So alternate text commands can be used like so:

`1 mv 34` will attempt to move piece with id 1 to the (3,4) location, `mov` and `move` also work

`2 info` will display in depth info about the piece with this id, inclusive where it started the game,
where it is currently, how many moves it made and distance covered, or where it finished the game if it is already removed.

`3 rm` will remove piece with this id `rmv`, `take`, `del` and similar also work

Client does the local validation and will not change the board if action is incorrect.
It will also not send such event to the server, but server still dose the check by itself.
Proper error information will be displayed by client, and exact same board will resume.

Incorrect actions or invalid states are detected on the server and will not be persisted. Server
returns current offset up to which all is saved, so if it is less than local game, it reveals
problem. However, this should not occur, as client validates it, but server does still check.
For the check to work with single trip to DB, client sends both the command and expected outcome
of it, namely hex-encoded board state after the move. This move is reverted and must then match
the last stored board state on the server. Using lua script with redis it is done atomically,
in single trip. Situation is always clear without need for back-and-forth between client and the
server. Only upon successful persistence events will be emitted by kafka producer.

There is just 3 endpoints. One for status, another for loading saved state, one more for
accepting incremental or bulk state changes - it supports resuming, where several actions
are accepted at once, all are still  validated.
The API can be inspected in swagger under `http://localhost:8090/docs/`

There is good test coverage for important logical parts, pertaining to move logic and
numeric representation of the board, as well as its serialization. For sure there could
be still more tests for other segments of the application, but it is this for now.

