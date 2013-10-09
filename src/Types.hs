module Types where

data ForemanConsoleCommand = Exit
                            | E
                            | Quit
                            | Q
                            | Start
                            deriving (Show, Read, Eq)

data ForemanToWorkerCommand = StartWork JobId deriving (Show, Read, Eq)

data WorkerToForemanCommand = Result deriving (Show, Read, Eq)

data JobId = JobId String deriving (Show, Read, Eq)
