-module(channel).
-export([start/1,stop/1]).

-record(init_st, {
    members,    %member pid's
    name        %channel name
}).

initial_state(Name) ->
    #init_st {
        members = [],
        name = Name
    }.

% Joining
handle(St, {join, CPid}) ->
    case lists:member(CPid, St#init_st.members) of
        true -> %User already in channel
            {reply, {error, user_already_joined, "User already part of channel " ++ St#init_st.name}};
        false ->
            {reply, ok, St#init_st{members =[CPid | St#init_st.members]}}
    end;    

% Leaving
handle(St, {leave, CPid}) ->
        MembersList_New = lists:delete(CPid, St#init_st.members),
        {reply, ok, St#init_st{members = MembersList_New}};

% Sending message
handle(St, {message_send, CPid, CNick, Msg}) ->
        OtherMembers = lists:delete(CPid, St#init_st.members),
        Data = {request, self(), make_ref(), {message_receive, St#init_st.name, CNick, Msg}},
        lists:foreach((fun(Member) -> Member ! Data end), OtherMembers),
        {reply, ok, St}.

start(Name) ->
    genserver:start(list_to_atom(Name), initial_state(Name), fun handle/2).

stop(Name) ->
    genserver:stop(list_to_atom(Name)).