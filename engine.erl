-module(engine).

-export([start/0, engine_start/1]).




register_username(Username,  Pid, State) -> 
    CurrentUsersDict = dict:fetch(users, State),
    UpdatedUserDict = dict:store(Username, Pid, CurrentUsersDict),
    UpdatedState = dict:store(users, UpdatedUserDict, State),
    
    CurrentSubscriberDict = dict:fetch(subscriberdict, State),
    CurrentSubscribeeDict = dict:fetch(subscribeedict, State),
    UpdatedSubscriberDict = dict:store(Username, [], CurrentSubscriberDict),
    UpdatedSubscribeeDict = dict:store(Username, [], CurrentSubscribeeDict),
    UpdatedState1 = dict:store(subscriberdict, UpdatedSubscriberDict, UpdatedState),
    UpdatedState2 = dict:store(subscribeedict, UpdatedSubscribeeDict, UpdatedState1),


    UserTweets = dict:fetch(usertweets, State),
    UpdatedTweetsDict = dict:store(Username, [], UserTweets),
    UpdatedState3 = dict:store(usertweets, UpdatedTweetsDict, UpdatedState2),
    UpdatedState3
.


subscribe(Subscriber, Subscribee, State) -> 
    CurrentSubscriberDict = dict:fetch(subscriberdict, State),
    CurrentSubscribeeDict = dict:fetch(subscribeedict, State),
    UpdatedSubscriberDict = dict:append(Subscriber, Subscribee, CurrentSubscriberDict),
    UpdatedSubscribeeDict = dict:append(Subscribee, Subscriber, CurrentSubscribeeDict),
    UpdatedState1 = dict:store(subscriberdict, UpdatedSubscriberDict, State),
    UpdatedState2 = dict:store(subscribeedict, UpdatedSubscribeeDict, UpdatedState1),
    UpdatedState2

.



send_tweet_to_subscriber([], _, _,_) -> ok;
send_tweet_to_subscriber(SubscriberList, Tweet, UsersDict, Username) ->
    [First | Rest] = SubscriberList,
    Pid = dict:fetch(First, UsersDict),
    Pid ! {tweet, Tweet, Username},
    send_tweet_to_subscriber(Rest, Tweet, UsersDict, Username)
.

size([], Length) -> Length;
size(Elements, Length) ->
    [_ | Rest] = Elements,
    size(Rest, Length + 1).


send_tweet(Username, Tweet, State) -> 
    UserTweets = dict:fetch(usertweets, State),
    User = dict:fetch(Username, UserTweets),
    Pos =  size(User, 0) + 1,
    UserList = lists:append(User, [[Pos, Tweet]]),
    CurrentUserTweets = dict:store(Username, UserList, UserTweets),
    CurrentSubscribeeDict = dict:fetch(subscribeedict, State),
    CurrentUserSubscriptionList = dict:fetch(Username, CurrentSubscribeeDict),

    UsersDict = dict:fetch(users, State),
    send_tweet_to_subscriber(CurrentUserSubscriptionList, [Pos, Tweet], UsersDict, Username),
    UpdatedState = dict:store(usertweets, CurrentUserTweets, State),
    UpdatedState
.


search([], _) -> useridnotfound;
search(MessageList, Id) ->
    [First|Rest] = MessageList,
    % io:format("~p", [First]),
    case lists:nth(1, First) == Id of
        true -> lists:last(First);
        false -> search(Rest, Id)
    end.


retweet(Username,Usermain, Message_id, State) ->
    Userlist = dict:fetch(usertweets , State),
    Tweetslist = dict:fetch(Usermain , Userlist),
    % io:format("~p", [Tweetslist]),
    Tweet = search(Tweetslist, Message_id),
    UpdatedState = send_tweet(Username, Tweet, State),
    UpdatedState
.

searchtweetsbyword([], _, ResultsList) -> ResultsList;
searchtweetsbyword(Tweets, Word, ResultsList) ->
    [First|Rest] = Tweets,
    % io:format("WOrd   ~p~p~p~n", [First, Word, string:str(lists:last(First), Word)]),
    case string:str(lists:last(First), Word)  > 0 of
        true -> searchtweetsbyword(Rest, Word, lists:append(ResultsList, [First]));
        false -> searchtweetsbyword(Rest, Word, ResultsList)
end.

searchinuserstweets([], _, _, ResultsDict) -> ResultsDict;
searchinuserstweets(Users, UserTweets, Word, ResultsDict) ->
    [First|Rest] = Users,
    Tweets = dict:fetch(First, UserTweets),
    TweetsWithWord = searchtweetsbyword(Tweets, Word, []),
    % io:format("~p~n", [TweetsWithWord]),
    case size(TweetsWithWord, 0) > 0 of
        true -> searchinuserstweets(Rest, UserTweets, Word, dict:store(First, TweetsWithWord, ResultsDict));
        false -> searchinuserstweets(Rest, UserTweets, Word, ResultsDict)
end.


gettweetscontainingstring(UserTweets, Word) ->
    Users = dict:fetch_keys(UserTweets),
    D = searchinuserstweets(Users, UserTweets, Word, dict:new()),
    D
.




query_tweet(Filter, Qury, Pid, State) -> 
    if Filter == "Mentions" ->
            Pid ! {result, gettweetscontainingstring(dict:fetch(usertweets, State), "@"++Qury)};
        true -> notcorrectfilter
    end,
    if Filter == "Hashtags" ->
            Pid ! {result, gettweetscontainingstring(dict:fetch(usertweets, State), "#"++Qury)};
        true -> notcorrectfilter
    end
    .


disconnect_user(Username, State) -> 
    CurrentUsersDict = dict:fetch(users, State),
    Pid = dict:fetch(Username, CurrentUsersDict),
    Pid ! {disconnect},
    dict:store(users, dict:erase(Username, CurrentUsersDict), State)
.

engine_start(State) ->
    receive
        {register, Username, Pid} -> 
            UpdatedState = register_username(Username, Pid, State),
            engine_start(UpdatedState);
        {getuserslist, Pid} -> 
            Pid ! {userlist, dict:fetch(users, State)},
            engine_start(State); 
        {subscribe, Subscriber, Subscribee} -> 
            UpdatedState = subscribe(Subscriber, Subscribee, State),
            engine_start(UpdatedState);
        {getsubscriber, Username, Pid} ->
            CurrentSubscriberDict = dict:fetch(subscriberdict, State),
            Pid ! {subscriber, dict:fetch(Username, CurrentSubscriberDict)},
            engine_start(State);
        {sendtweet, Username, Message} -> 
            UpdatedState = send_tweet(Username, Message, State),
            engine_start(UpdatedState);
        {querytweet, Filter, Qury, Pid} -> 
            query_tweet(Filter, Qury, Pid, State),
            engine_start(State);
        {retweet, Username, Usermain ,Message_id} -> 
            UpdatedState = retweet(Username,Usermain, Message_id, State),
            engine_start(UpdatedState);
        {disconnect, Username} -> 
            UpdatedState = disconnect_user(Username, State),
            engine_start(UpdatedState)
    end
.

start() ->
    State = dict:from_list([{users, dict:new()}, {subscriberdict,dict:new()}, {subscribeedict, dict:new()}, {usertweets, dict:new()}]),
    % io:format("~p", [State]),
    register(engine, spawn(engine, engine_start, [State]))
.