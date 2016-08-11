-- | A very simple executable that dumps all the data from the Posts file of a
-- stackoverflow xml dump into a postgresql database
--
--  Author: Christof Schramm
module Main where

import Thesis.Data.Stackoverflow.Dump.Export.Postgres
import Database.PostgreSQL.Simple

import System.Environment
import System.Directory

main = do
  args <- getArgs
  case args of
    [file, host, port, user, password, database] -> do
      let connectInfo = ConnectInfo{ connectHost = host
                                   , connectPort = read port
                                   , connectUser = user
                                   , connectPassword = password
                                   , connectDatabase = database
                                   }

      -- Check for existence of the XML dump file
      fileExists <- doesFileExist file
      if fileExists
        then do
          putStrLn "Read parameters, found dump file."
          putStrLn "Beginning transfer..."
          dumpToPostgres file connectInfo
          putStrLn "Done!"
        else putStrLn "Couldn't find dump file!"
    _ -> putStrLn msg
  
  where
    msg = unlines [ "Call with <dump file path> <postgre host> <postgre port> <postgre user> <postgre password> <postgre database>"
                  , ""
                  , "Dumps all posts from a stackoverflow XML dump into a PostgreSQL database."
                  ]

      
