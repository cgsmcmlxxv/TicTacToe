%% ===================================================================
%% Project: TicTacToe
%% File: tictactoe.erl
%% Description: A simple tic-tac-toe game in Erlang
%% Author: CGSMCMLXXV <cgsmcmlxxv@gmail.com>
%% Copyright: 2012 CGSMCMLXXV (for Erlang implementation)
%% License: GNU GPL3 (if something else is needed, drop an e-mail)
%% ===================================================================


-module(tictactoe).

-define(PORT,7000).
-define(IP,{127,0,0,1}).
-define(TCP_OPTIONS, [binary, {packet, 0}, {active, once}, {reuseaddr, true}, {nodelay, true}, {keepalive, true}]).
-define(MaxAttempts,5).
-define(SEPARATOR,"--SEP--").
-define(EMPTYSLOT,"-").
-define(X,"X").
-define(O,"0").

-export([start_server/0,register_client/0, client/2]).

-record(server_state, {port,loop,ip = any,lsocket = null}).
-record(game_state, {players,id,moves,next}).
-record(player_state, {socket,pid,name,game}).

start(Name, Port, Loop) ->
    State = #server_state{port = Port, loop = Loop},
    gen_server:start_link({local, Name}, ?MODULE, State, []).

init(State = #server_state{port=Port}) ->
    case gen_tcp:listen(Port, ?TCP_OPTIONS++[{ip,?IP}]) of
        {ok, LSocket} ->
            NewState = State#server_state{lsocket = LSocket},
            {ok, accept(NewState)};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_cast({accepted, _Pid}, State=#server_state{}) ->
    {noreply, accept(State)}.

accept_player({Server, LSocket, {M, F}}) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    inet:setopts(Socket, ?TCP_OPTIONS),
    gen_server:cast(Server, {accepted, self()}),
    games_control ! {pid,self()},
    M:F(#player_state{socket = Socket, pid = self(), game = "null"}),
    gen_tcp:close(Socket).

accept(State = #server_state{lsocket=LSocket, loop = Loop}) ->
    proc_lib:spawn(?MODULE, accept_player, [{self(), LSocket, Loop}]),
    State.

handle_call(_Msg, _Caller, State) -> {noreply, State}.
handle_info(_Msg, Library) -> {noreply, Library}.
terminate(_Reason, _Library) -> ok.
code_change(_OldVersion, Library, _Extra) -> {ok, Library}.

start_server() ->
    GCExists = whereis(games_control),
    if GCExists /= undefined -> unregister(games_control);
       true -> false
    end,
    register(games_control,proc_lib:spawn(?MODULE,games_control,[#game_state{players = [], id = 1}])),
    start(?MODULE, ?PORT, {?MODULE, listen_socket}).

listen_socket(PlayerState = #player_state{socket = Socket, pid = PID, name = Name, game = Game}) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp,Socket,Data} ->
              DataTuple = list_to_tuple(re:split(binary_to_list(Data),?SEPARATOR,[{return,list},trim])),
              case DataTuple of
                  {"gameid",GameID} -> NewPlayerState = PlayerState#player_state{game = GameID};
                  {"gameover","eoc"} -> NewPlayerState = PlayerState#player_state{game = "null"}, games_control ! {pid,self()};
                  {"name",PlayerName} -> NewPlayerState = PlayerState#player_state{name = PlayerName};
                  {"move",Move} -> NewPlayerState = PlayerState, case Game of "null" -> true; _G -> list_to_atom(_G) ! {move,PID,Move} end;
                  {"whoami","eoc"} -> NewPlayerState = PlayerState, self() ! {send,list_to_binary("whoami"++?SEPARATOR++Name)};
                  {"withdraw","eoc"} -> NewPlayerState = PlayerState, case Game of "null" -> true; _G -> list_to_atom(_G) ! {playerleft,PID} end;
                  _Other -> NewPlayerState = PlayerState
              end,
              listen_socket(NewPlayerState);
          {tcp_closed,Socket} -> case Game of "null" -> true; _G -> list_to_atom(_G) ! {playerleft,PID} end;
          {send,What} -> gen_tcp:send(Socket,What), listen_socket(PlayerState);
          _ELSE -> io:format("listen_socket error: ~p~n",[_ELSE])
    end.

games_control(GameState = #game_state{players = Players, id = ID}) ->
    receive
        {pid,Player} ->
            case length(GameState#game_state.players) of
                0 -> games_control(GameState#game_state{players = [Player]});
                1 ->
                    proc_lib:spawn(?MODULE,start_game,[#game_state{players = Players++[Player],
                                                                   id = ID}]),
                    games_control(GameState#game_state{players=[],id=ID+1})
            end
    end.

start_game(GameState = #game_state{id = ID}) ->
    SelfName = list_to_atom("sg"++integer_to_list(ID)),
    DoIExist = whereis(SelfName),
    if DoIExist /= undefined -> unregister(SelfName);
       true -> false
    end,
    register(SelfName,self()),
    erlang:send_after(500,SelfName,identify_yourself),
    new_game(GameState#game_state{moves = [-4,-4,-4,-4,-4,-4,-4,-4,-4], next = 1}).

new_game(GameState = #game_state{players = Players, id = ID, moves = Moves, next = Next}) ->
    PlayerX = lists:nth(1,Players), Player0 = lists:nth(2,Players),
    receive
        identify_yourself ->
            PlayerX ! {send,list_to_binary("gameid"++?SEPARATOR++integer_to_list(ID)++?SEPARATOR++"assign"++?SEPARATOR++?X)},
            Player0 ! {send,list_to_binary("gameid"++?SEPARATOR++integer_to_list(ID)++?SEPARATOR++"assign"++?SEPARATOR++?O)},
            new_game(GameState);
        {move,PID,Move} ->
            case Next of 1 -> PPlayer = PlayerX, NPlayer = Player0; 0 -> PPlayer = Player0, NPlayer = PlayerX end,
            case PID == PPlayer of
                false -> PID ! {send,list_to_binary("message"++?SEPARATOR++"Not your turn.")}, new_game(GameState);
                true ->
                    RC = list_to_integer(Move),
                    R = RC div 10, C = RC rem 10,
                    case lists:nth(3*(R-1)+C,Moves) == -4 of
                        false -> PID ! {send,list_to_binary("message"++?SEPARATOR++"Illegal move. Try again.")}, new_game(GameState);
                        true ->
                            NewMoves = lists:sublist(Moves,1,3*(R-1)+C-1)++[Next]++lists:sublist(Moves,3*(R-1)+C+1,9),
                            NML = lists:map(fun(E) -> case E of -4 -> ?EMPTYSLOT; 0 -> ?O; 1 -> ?X end end,NewMoves),
                            NPlayer ! {send,list_to_binary("message"++?SEPARATOR++"Your opponent moved at "++Move++"."++?SEPARATOR++"status"++?SEPARATOR++NML)},
                            PID ! {send,list_to_binary("status"++?SEPARATOR++NML)},
                            [NM1,NM2,NM3,NM4,NM5,NM6,NM7,NM8,NM9] = NewMoves,
                            case lists:any(fun(E) -> (E == 3*Next) end,[NM1+NM2+NM3,NM4+NM5+NM6,NM7+NM8+NM9,NM1+NM4+NM7,NM2+NM5+NM8,NM3+NM6+NM9,NM1+NM5+NM9,NM3+NM5+NM7]) of
                                true ->
                                    PID ! {send,list_to_binary(?SEPARATOR++"gameover"++?SEPARATOR++"Congratulations! You won! :)")},
                                    NPlayer ! {send,list_to_binary(?SEPARATOR++"gameover"++?SEPARATOR++"Sorry, you lost... :(")},
                                    SelfName = list_to_atom("sg"++integer_to_list(ID)),
                                    DoIExist = whereis(SelfName),
                                    if DoIExist /= undefined -> unregister(SelfName);
                                       true -> false
                                    end;
                                false ->
                                    case lists:any(fun(E1) -> (E1 == -4) end,NewMoves) of
                                        true -> new_game(GameState#game_state{moves = NewMoves, next = (Next+1) rem 2});
                                        false ->
                                            PID ! {send,list_to_binary(?SEPARATOR++"gameover"++?SEPARATOR++"It's a draw! :(")},
                                            NPlayer ! {send,list_to_binary(?SEPARATOR++"gameover"++?SEPARATOR++"It's a draw! :(")},
                                            SelfName = list_to_atom("sg"++integer_to_list(ID)),
                                            DoIExist = whereis(SelfName),
                                            if DoIExist /= undefined -> unregister(SelfName);
                                               true -> false
                                            end
                                    end
                            end
                    end
            end;
        {playerleft,PID} ->
            case PID of
                PlayerX -> Player0 ! {send,list_to_binary("gameover"++?SEPARATOR++"Your opponent left the game. You won! :)")};
                Player0 -> PlayerX ! {send,list_to_binary("gameover"++?SEPARATOR++"Your opponent left the game. You won! :)")}
            end,
            SelfName = list_to_atom("sg"++integer_to_list(ID)),
            DoIExist = whereis(SelfName),
            if DoIExist /= undefined -> unregister(SelfName);
               true -> false
            end
    end.

register_client() -> proc_lib:spawn(?MODULE,start_client,[]).

start_client() ->
    case gen_tcp:connect(?IP,?PORT,?TCP_OPTIONS) of
        {ok,Socket} -> client_messages(Socket);
        _ELSE -> io:format("Client ~p not connected.~n",[self()])
    end.

client_messages(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp,Socket,Data} ->
              DataTuple = list_to_tuple(re:split(binary_to_list(Data),?SEPARATOR,[{return,list},trim])),
              case DataTuple of
                  {"gameid",GameID,"assign",Type} -> io:format("For ~p: Game ID: ~s, where you play with ~s~n",[self(),GameID,Type]), self() ! {send,list_to_binary("gameid"++?SEPARATOR++"sg"++GameID)};
                  {"gameover",Message} -> io:format("For ~p: ~s~n",[self(),Message]), self() ! {send,list_to_binary("gameover"++?SEPARATOR++"eoc")};
                  {"message",What} -> io:format("For ~p: ~s~n",[self(),What]);
                  {"message",What,"status",Status} ->
                       io:format("For ~p: ~s~n",[self(),What]),
                       GameStatus = re:split(Status,"",[{return,list},trim]),
                       io:format("For ~p: Game status:~n    ~s ~s ~s~n    ~s ~s ~s~n    ~s ~s ~s~n",[self()]++GameStatus);
                  {"message",What,"status",Status,"gameover",Message} ->
                       io:format("For ~p: ~s~n",[self(),What]),
                       GameStatus = re:split(Status,"",[{return,list},trim]),
                       io:format("For ~p: Game status:~n    ~s ~s ~s~n    ~s ~s ~s~n    ~s ~s ~s~n",[self()]++GameStatus),
                       io:format("For ~p: ~s~n",[self(),Message]), self() ! {send,list_to_binary("gameover"++?SEPARATOR++"eoc")};
                  {"whoami",NameX} -> io:format("For ~p: Your name is: ~s~n",[self(),NameX]);
                  {"status",Status} ->
                       GameStatus = re:split(Status,"",[{return,list},trim]),
                       io:format("For ~p: Game status:~n    ~s ~s ~s~n    ~s ~s ~s~n    ~s ~s ~s~n",[self()]++GameStatus);
                  {"status",Status,"gameover",Message} ->
                       GameStatus = re:split(Status,"",[{return,list},trim]),
                       io:format("For ~p: Game status:~n    ~s ~s ~s~n    ~s ~s ~s~n    ~s ~s ~s~n",[self()]++GameStatus),
                       io:format("For ~p: ~s~n",[self(),Message]), self() ! {send,list_to_binary("gameover"++?SEPARATOR++"eoc")}
              end,
              client_messages(Socket);
        {tcp_closed,Socket} -> io:format("~p got tcp_closed signal.~n",[self()]);
        {send,What} -> gen_tcp:send(Socket,What), client_messages(Socket);
        quit -> gen_tcp:close(Socket);
        _ELSE -> io:format("Got something for ~p: ~p~n",[self(),_ELSE])
    end.

client(PID,Message) when is_list(Message) ->
    PID ! {send,list_to_binary("name"++?SEPARATOR++Message)},
    io:format("Name set to: ~s~n",[Message]);

client(PID,Message) when is_atom(Message) ->
    case Message of
        quit -> PID ! quit, io:format("Command quit sent (issued by ~p).~n",[PID]);
        _Other -> PID ! {send,list_to_binary(atom_to_list(Message)++?SEPARATOR++"eoc")}, io:format("Command ~p sent (issued by ~p).~n",[Message,PID])
    end;

client(PID,Message) when is_integer(Message) ->
    PID ! {send,list_to_binary("move"++?SEPARATOR++integer_to_list(Message))},
    io:format("Your last move, ~p, is being processed.~n",[Message]).

