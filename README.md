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

