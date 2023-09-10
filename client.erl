-module(client).
-export([join/1, getuserslist/0, subscribe/2 , tweeting/2, joinuser/1,retweeting/3, simulate/1, listen/1, search/2, getsubscriptiondetails/3, register/1, retweet/2, subscribe_user/1, tweet/1]).



listen(Username) -> 
    receive 
        {tweet, Tweet, FromUser} -> 
            % {ok, Fd} = file:open("feed.txt", [append]),
            % io:format(Fd, "~p's Feed Update~n~p:~p ~n", [Username,FromUser,Tweet]);
            ok;
        {disconnect} -> disconnected
    end,
    listen(Username)
.


joinuser(Username) ->
    Pid = spawn(client, listen, [Username]),
    engine ! {register, Username,Pid},
    [Pid, Username]
.

join(Username) -> 
    spawn(client, joinuser, [Username])
.

subscribe(SubscriberName, SubscribeeName) ->
    engine ! {subscribe , SubscriberName, SubscribeeName}
.

search(Type, Term) ->
    engine ! {querytweet , Type, Term, self()},
    receive 
        {result, ResultsDict} -> 
            % {ok, Fd} = file:open("queryresults.txt", [append]),
            % io:format(Fd, "Results ~p~n",[ResultsDict])
            ok
end
.

tweeting(Username, Message) ->
    engine ! {sendtweet, Username, Message}
.

retweeting(Username, Usermain , Message_id) ->
    engine ! {retweet , Username , Usermain , Message_id}
.

getuserslist() -> 
    engine ! {getuserslist, self()},
    receive 
        {userlist, Users} -> io:format("~p", [Users])
end.





getrandomname() ->
    Length = 5,
    lists:foldl(fun(_, Acc) ->
                        [lists:nth(rand:uniform(length("ABCDEFGHIJKLMNOPQRSTUVWXYZ")),
                                   "ABCDEFGHIJKLMNOPQRSTUVWXYZ")]
                            ++ Acc
                end, [], lists:seq(1, Length)).


getrandommessage() ->
    Length = 15,
    lists:foldl(fun(_, Acc) ->
                        [lists:nth(rand:uniform(length("ABCDEFGHIJKLMNOPQRSTUVWXYZ   ")),
                                    "ABCDEFGHIJKLMNOPQRSTUVWXYZ   ")]
                            ++ Acc
                end, [], lists:seq(1, Length)).


createusers(0, UserList) -> UserList;
createusers(UserCount, UserList) ->
    UserDetails = joinuser(getrandomname()),
    createusers(UserCount - 1, lists:append(UserList, [UserDetails]))
.


makeusersubscriptions(_, []) -> ok;
makeusersubscriptions(User, SubUsers) -> 
    [First| Rest] = SubUsers,
    subscribe(lists:last(User), lists:last(First)),
    makeusersubscriptions(User, Rest)
.

getnusers(_, _, 0, _, SubList, _) -> SubList; 
getnusers(UserList, User, Count, UserCount, SubList, I) -> 
    U = lists:nth(I, UserList),
    getnusers(UserList, User, Count - 1,UserCount, lists:append(SubList, [U]), ((I rem UserCount) + 1))
.



makesubscriptions(UserCount, UserList, I) -> 
    if I > UserCount -> 
        ok;
        true ->
            User = lists:nth(I, UserList),
            SubUsers = getnusers(UserList, User, round(UserCount * (1 / I)), UserCount, [], (I rem UserCount) + 1),
            makeusersubscriptions(User, SubUsers),
            makesubscriptions(UserCount, UserList, I + 1)
    end
.

getrandomhandle() ->
    lists:foldl(fun(_, Acc) ->
                        [lists:nth(rand:uniform(length("#@")),
                                   "#@")]
                            ++ Acc
                end, [], lists:seq(1, 1)).


getrandomusername(Usernames) ->
    lists:nth(rand:uniform(length(Usernames)), Usernames)
    .


getsubscriptiondetails(UserCount, UserList, I) -> 
    if I > UserCount -> 
        ok;
        true -> 
            User = lists:nth(I, UserList),
            engine ! {getsubscriber, lists:last(lists:nth(I, UserList)), self()},
            receive 
                {subscriber, V} -> io:format("~p :::: ~p ~n", [User, V])
            end,
            getsubscriptiondetails(UserCount, UserList, I + 1)
    end
    .

getnmessages(0, _, Messages) -> Messages; 
getnmessages(Count, Usernames, Messages) ->
    getnmessages(Count - 1, Usernames, lists:append(Messages, [getrandommessage() ++ " "  ++ getrandomhandle() ++ lists:last(getrandomusername(Usernames))]))
    .

sendusertweets(_, []) -> ok;
sendusertweets(User, Tweets) ->
    [First|_] = Tweets,
    tweeting(lists:last(User), First)
    .



maketweets(UserCount, UserList, I) ->
    if I > UserCount -> 
        ok;
        true ->
            User = lists:nth(I, UserList),
            Tweets = getnmessages(round(UserCount * (1 / I)), UserList, []),
            sendusertweets(User, Tweets),
            maketweets(UserCount, UserList, I + 1)
    end
.

makequeries(_, 0) -> done;
makequeries(UserList, Count) ->
    U = getrandomusername(UserList),
    search(lists:nth(rand:uniform(2), ["Mentions", "Hashtags"]), lists:last(U)),
    makequeries(UserList, Count - 1)
.

simulate(UserCount) ->
    {ok, Fd} = file:open("stats.txt", [append]),
    io:format(Fd, "No of users ~p~n", [UserCount]),
    statistics(runtime),
    UserList = createusers(UserCount, []),
    {_, Time1} = statistics(runtime),
    io:format(Fd, "Time for create users ~p~n", [Time1]),
    % io:format("~p~n", [UserList]),

    statistics(runtime),
    makesubscriptions(UserCount, UserList, 1),
    {_, Time2} = statistics(runtime),
    io:format(Fd, "Time for subscribe ~p~n", [Time2]),
    % getsubscriptiondetails(UserCount, UserList, 1),

    statistics(runtime),
    maketweets(UserCount, UserList, 1),

    {_, Time3} = statistics(runtime),

    io:format(Fd, "Time for sendtweets ~p~n", [Time3]),

    statistics(runtime),
    makequeries(UserList, UserCount * 2),
    {_, Time4} = statistics(runtime),
    io:format(Fd, "Time for queries ~p~n", [Time4])
.



tweet(Tweet) ->
    tweeting(persistent_term:get(username), Tweet)
    .

subscribe_user(User) ->
    subscribe(persistent_term:get(username), User)
    .

retweet(Username, Message_id) ->
    retweeting(persistent_term:get(username), Username, Message_id)
    .

register(Username) -> 
    persistent_term:put(username, Username),
    join(Username)
.