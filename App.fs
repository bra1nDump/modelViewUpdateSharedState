namespace App

[<AutoOpen>]
module Prelude =
    let flip (f: 'A -> 'B -> 'C) = fun b a -> f a b

module List =
    let apply (x: 'A) (fs: ('A -> 'B) list) : 'B list =
        [ for f in fs do f x ]

module Cmd =
    type Cmd<'Msg> = 
        | Sub of (('Msg -> unit) -> unit)
        | Msg of 'Msg

    /// takes closure that will later be ran with dispatch
    let ofSub (sub: ('Msg -> unit) -> unit): Cmd<'Msg> list = [ Sub sub ]
    let ofMsg (msg: 'Msg): Cmd<'Msg> list = [ Msg msg ]
    let batch (cmds: Cmd<'Msg> list list) = List.concat cmds
    let none: Cmd<_> list = []

[<AutoOpen>]
module DataModel =
    type MessageId = MessageId of string
    type ChatId = ChatId of string

    type Message = {
        Id: MessageId
        ChatId: ChatId
    }

    type Chat = {
        Id: ChatId
    }

module Api = 
    type Update =
        | NewMessage of Message
        | NewChat of Chat

    let randomUpdates = [
            NewChat { Id = ChatId "kirill-natasha" }
            NewMessage { Id = MessageId "SOME-UUID-4533"; ChatId = ChatId "kirill-natasha" }
        ]

    let listen (updatesHanlder: Update List -> unit) =
        async {
            while true do
                do! Async.Sleep(100)
                updatesHanlder randomUpdates
        }

module Store =
    open Api

    type Watcher<'Msg> =
        | Chat of (ChatId * (Message list -> 'Msg))
        | ChatList of (Chat list -> 'Msg)  

    type Msg<'Msg> = 
        | Watchers of Watcher<'Msg> list
        | ApiUpdates of Api.Update list
    
    type Model<'Msg> = {
        Messages: Map<MessageId, Message>
        Chats: Map<ChatId, Chat>
        Watchers: Watcher<'Msg> list
    }
    
    let init lift = 
        {
            Messages = Map.empty
            Chats = Map.empty
            Watchers = []
        }
        , Cmd.ofSub 
            (fun dispatch -> 
                Api.listen (ApiUpdates >> lift >> dispatch)
                |> Async.Start
            )
            
    let update msg model =
        match msg with
        | Watchers v ->
            { model with Watchers = v }, Cmd.none
        | ApiUpdates updates ->
            let newMessages, newChats =
                List.fold
                    (fun (ms, cs) -> function
                    | NewMessage m -> m::ms, cs
                    | NewChat c -> ms, c::cs)
                    ([], [])
                    updates

            let messages = 
                List.fold
                    (fun all (x: Message) -> Map.add x.Id x all)
                    model.Messages
                    newMessages
            let chats = 
                List.fold 
                    (fun all x -> Map.add x.Id x all)
                    model.Chats 
                    newChats

            let chatWatchers, chatListWatchers =
                List.fold 
                    (fun (w1, w2) -> function
                    | Watcher.Chat w -> w::w1, w2
                    | Watcher.ChatList w -> w1, w::w2)
                    ([], [])
                    model.Watchers

            { model with
                Messages = messages
                Chats = chats
            }
            , [
                if not newChats.IsEmpty then
                    let chats = chats |> Map.toList |> List.map snd
                    yield! List.apply chats chatListWatchers
                if not newMessages.IsEmpty then
                    let watched = List.map fst chatWatchers |> Set.ofList
                    let updated = [ for m in newMessages do m.ChatId ] |> Set.ofList
                    yield! [ 
                        for chatId in Set.intersect watched updated do
                        let watchers = 
                            chatWatchers
                            |> List.filter (fst >> (=) chatId)
                            |> List.map snd
                        let messages =
                            messages
                            |> Map.toList |> List.map snd
                            |> List.filter (fun m -> m.ChatId = chatId)
                        yield!
                            List.apply messages watchers
                    ]
            ]
            |> List.map Cmd.ofMsg
            |> Cmd.batch

module Chats =
    open Store

    type Msg =
        | Chats of Chat list
        
    type Model<'Msg> = {
        Chats: Chat list
        
        OpenChat: ChatId -> 'Msg
        Watchers: Watcher<'Msg> list
    }
    
    let init openChat lift = 
        { 
            Chats = []
            OpenChat = openChat
            Watchers = [
                ChatList (Chats >> lift)
            ]
        }
        , Cmd.none
        
    let update msg model : Model<_> * _ = 
        match msg with
        | Chats v -> 
            { model with Chats = v }, Cmd.none
    
module Chat =
    open Store

    type Msg =
        | Messages of Message list
    
    type Model<'Msg> = {
        Messages: Message list
        SendMessage: Message -> 'Msg
        // TODO: make watchers just one watcher for now?
        // merging logic will be simpler, but is it worth it?
        Watchers: Watcher<'Msg> list
    }
    
    let init chatId (sendMessage: Message -> 'Msg) lift  =
        {
            Messages = []
            SendMessage = sendMessage
            Watchers = [
                Watcher.Chat (chatId, Messages >> lift)
            ]
        } 
        , Cmd.none

    let update msg model : Model<_> * _ =
        match msg with 
        | Messages messages ->
            { model with Messages = messages }, Cmd.none
        
// in mvu deep nesting
// is pretty bad
// so its a good idea to keep
// all main screens on top
module App =
    type Msg = 
        | StoreMsg of Store.Msg<Msg>
        | ChatsMsg of Chats.Msg
        | OpenChat of ChatId
        | ChatMsg of Chat.Msg
        
    type Model = {
        Store: Store.Model<Msg>
        Chats: Chats.Model<Msg>
        Chat: Chat.Model<Msg> option
    }
    
    let watchers =
        Store.Msg.Watchers
        >> StoreMsg
        >> Cmd.ofMsg
    
    let init () =
        let store, cmd1 = Store.init StoreMsg
        let chats, cmd2 = Chats.init OpenChat ChatsMsg
        {
            Store = store
            Chats = chats
            Chat = None
        }
        , [
            cmd1; cmd2
            watchers chats.Watchers
        ]
        |> Cmd.batch
    
    // Idea: middleware not only for watchers, but also
    // for logging messages dispatched in an invalid state
    let update msg model =
        match msg with 
        | StoreMsg msg ->
            // Talk about it! vs mapFst
            let m, cmd = Store.update msg model.Store
            { model with Store = m }, cmd
        | ChatsMsg msg ->
            let chats, cmd = Chats.update msg model.Chats
            { model with Chats = chats }
            , [
                cmd
                watchers [
                    yield! chats.Watchers
                    match model.Chat with 
                    | None -> ()
                    | Some v -> yield! v.Watchers
                ]
            ]
            |> Cmd.batch
        | OpenChat chatId ->
            // let sendMessage = Store.Msg.SendMessage >> StoreMsg
            let chat, cmd = Chat.init chatId (fun _ -> failwith "not implemented") ChatMsg
            { model with Chat = Some chat }
            , [
                cmd
                watchers (chat.Watchers @ model.Chats.Watchers)
            ]
            |> Cmd.batch
        | ChatMsg msg ->
            match model.Chat with 
            | None -> model, Cmd.none
            | Some chat ->
                let chat, cmd = Chat.update msg chat
                { model with Chat = Some chat }
                , [ 
                    cmd
                    watchers (chat.Watchers @ model.Chats.Watchers)
                ]
                |> Cmd.batch
