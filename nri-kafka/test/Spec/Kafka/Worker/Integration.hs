module Spec.Kafka.Worker.Integration (tests) where

import qualified Control.Concurrent.STM as STM
import qualified Dict
import qualified Expect
import qualified Helpers
import qualified Set
import qualified Test
import qualified Prelude

tests :: Test.Test
tests =
  Test.describe
    "Worker"
    [ Test.describe
        "Integration"
        [ Helpers.test "We receive what we send" <| \(topic, handler) -> do
            Helpers.sendSync handler topic 1 1
            msgsTVar <- atomically (STM.newTVar Set.empty)
            _ <- Helpers.spawnWorker handler topic (\msg -> STM.modifyTVar' msgsTVar (Set.insert msg))
            Helpers.sendSync handler topic 2 2
            msgs' <- waitFor msgsTVar (\items -> Set.size items == 2)
            msgs' |> Expect.equal (Set.fromList [1, 2]),
          Helpers.test "two workers process all messages once" <| \(topic, handler) -> do
            msgsTVar <- atomically (STM.newTVar [])
            _ <- Helpers.spawnWorker handler topic (\msg -> STM.modifyTVar' msgsTVar (\msgs -> msg : msgs))
            _ <- Helpers.spawnWorker handler topic (\msg -> STM.modifyTVar' msgsTVar (\msgs -> msg : msgs))
            Helpers.sendSync handler topic 1 (1, 1)
            Helpers.sendSync handler topic 1 (1, 2)
            Helpers.sendSync handler topic 2 (2, 3)
            msgs' <- waitFor msgsTVar (\items -> List.length items == 3)
            msgs' |> groupDictAndMap identity
              |> Expect.equal
                ( Dict.fromList
                    [ (1, [2, 1]),
                      (2, [3])
                    ]
                ),
          Helpers.test "second worker takes over after first worker gets stopped" <| \(topic, handler) -> do
            msgsTVar <- atomically (STM.newTVar [])
            worker1 <- Helpers.spawnWorker handler topic (\msg -> STM.modifyTVar' msgsTVar (\msgs -> msg : msgs))
            _ <- Helpers.spawnWorker handler topic (\msg -> STM.modifyTVar' msgsTVar (\msgs -> msg : msgs))
            Helpers.sendSync handler topic 1 (1, 1)
            Helpers.sendSync handler topic 2 (2, 1)
            _ <- waitFor msgsTVar (\items -> List.length items == 2)
            Helpers.stopWorker worker1
            Helpers.sendSync handler topic 1 (1, 2)
            Helpers.sendSync handler topic 2 (2, 2)
            msgsAfterStoppingWorker <- waitFor msgsTVar (\items -> List.length items == 4)
            msgsAfterStoppingWorker
              |> groupDictAndMap identity
              |> Expect.equal
                ( Dict.fromList
                    [ (1, [2, 1]),
                      (2, [2, 1])
                    ]
                )
        ]
    ]

atomically :: STM.STM a -> Expect.Expectation' a
atomically = STM.atomically >> Expect.fromIO

waitFor :: STM.TVar a -> (a -> Bool) -> Expect.Expectation' a
waitFor tVar pred =
  atomically <| do
    val <- STM.readTVar tVar
    if pred val
      then Prelude.pure val
      else STM.retry

groupDictAndMap :: Ord b => (a -> (b, c)) -> List a -> Dict.Dict b (List c)
groupDictAndMap f =
  List.foldr
    ( \x ->
        Dict.update (Tuple.first (f x)) <| \val ->
          case val of
            Nothing -> Just [Tuple.second (f x)]
            Just y -> Just (Tuple.second (f x) : y)
    )
    Dict.empty
