namespace App

[<AutoOpen>]
module TestingHelpers =
    let inline undefined _ = failwith "not implemented"

    let inline expectTrue (x: bool) =
        if not x then failwith "expected true"

    let inline expectEqual (expected: 'A) (actual: 'A) =
        if expected <> actual then
            failwith (sprintf "%O not equal expected %O" actual expected)

module Map =
    let count (x: Map<_, _>) = x.Count

module StoreTests =
    open Api
    open Store

    // helpers
    let messages model = model.Messages

    let store() = init id |> fst 
    let chat1 = { Id = ChatId "kirill-natasha" }
    let chat2 = { Id = ChatId "kirill-kostas" }
    let message1 = 
        {   Id = MessageId "1"
            ChatId = chat1.Id }
    let message2 = 
        {   Id = MessageId "2"
            ChatId = chat2.Id }

    let chat1Watcher watcher = Watcher.Chat (chat1.Id, watcher)
    
    let ``can add messages``() = 
        store()
        |> update (ApiUpdates [ NewMessage message1; NewMessage message2 ]) |> fst 
        |> (fun store -> store.Messages)
        |> Map.count
        |> expectEqual 2

    let ``can add chats``() = 
        store()
        |> update (ApiUpdates [ NewChat chat1; NewChat chat2 ])
        |> fst |> (fun store -> store.Chats)
        |> Map.count
        |> expectEqual 2

    let ``can add watchers``() = 
        store()
        |> update (Watchers [ ChatList undefined ]) |> fst 
        |> (fun store -> store.Watchers)
        |> List.length
        |> expectEqual 1

    let ``old watchers get replaced``() = 
        store()
        |> update (Watchers [ ChatList undefined ]) |> fst
        |> update (Watchers [ ChatList undefined ]) |> fst 
        |> (fun store -> store.Watchers)
        |> List.length
        |> expectEqual 1

    exception Success

    let ``chat list watcher gets called when chat is added``() = 
        try
            let watcher chats =
                expectEqual [chat1] chats
                raise Success

            store()
            |> update (Watchers [ ChatList (watcher) ]) |> fst
            |> update (ApiUpdates [ NewChat chat1 ])
            |> ignore

            failwith "watcher never got called"
        with
        | Success -> ()
        | _ -> reraise()

    let ``all chat list watchers get called``() =
        ()

    let ``chat watcher gets called when message for that chat is recieved``() =
        let mutable callCount = 0
        let watcher messages =
            callCount <- callCount + 1
            expectEqual [ message1 ] messages
            // TODO: dirty hack!, even worse than with exceptions :)
            // we need to return some type of Store.Msg now
            Msg.ApiUpdates []

        store()
        |> update (Watchers [ chat1Watcher watcher ]) |> fst
        |> update (ApiUpdates [ NewMessage message1 ])
        |> ignore
        
        expectEqual 1 callCount

    let ``chat watcher does not get called when message for some other chat is recieved``() =
        let watcher _ = failwith "watcher for chat1 responded to chat2 message"
        store()
        |> update (Watchers [ chat1Watcher watcher ]) |> fst
        |> update (ApiUpdates [ NewMessage message2 ])
        |> ignore

    let run() =
        ``can add messages``()
        ``can add chats``()
        ``can add watchers``()
        ``old watchers get replaced``()

        // chat list
        ``chat list watcher gets called when chat is added``()
        ``all chat list watchers get called``()

        // chat watcher
        ``chat watcher gets called when message for that chat is recieved``()
        ``chat watcher does not get called when message for some other chat is recieved``()

module Main =
    [<EntryPoint>]
    let main _ =
        StoreTests.run()

        0