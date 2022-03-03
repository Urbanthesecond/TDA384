-module(server).
-export([start/1,stop/1]).

-record(init_st, {
    nicks,      % list of nicks
    channels    % list of channels
}).

initial_state() ->
    #init_st{
        nicks = [],
        channels = []
    }.

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    genserver:start(ServerAtom, initial_state(), fun handle/2).
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    genserver:stop(ServerAtom).

%join channel, client request
handle(St, {join, CPid, CNick, Channel}) ->    
    ChannelList_New = 
        case lists:member(Channel, St#init_st.channels) of
            true ->
                St#init_st.channels;
            false ->
                channel:start(Channel),             % start channel
                [Channel | St#init_st.channels]     % update channel list
        end,
    NickList_New = 
        case lists:member(CNick, St#init_st.nicks) of
            true ->
                St#init_st.nicks;
            false ->
                [CNick | St#init_st.nicks]          % update nick list
        end,
    Response_Channel = genserver:request(list_to_atom(Channel), {join, CPid}),      %get response from channel
    {reply, Response_Channel, St#init_st{nicks=NickList_New, channels=ChannelList_New}};        

% assigning nicks
handle(St, {nick, Nick_Old, Nick_New}) ->
    case lists:member(Nick_New, St#init_st.nicks) of
        true when Nick_Old =:= Nick_New -> % automatically Ok if new nick is same as old nick
            {reply, ok, St};
        true -> % clash of nicknames
            {reply, {error, nick_taken, "Nickname " ++ Nick_New ++ " already taken"}, St};
        false -> % nick available
            NickList_New = [Nick_New | lists:delete(Nick_Old, St#init_st.nicks)],
            {reply, ok, St#init_st{nicks = NickList_New}}
    end.





