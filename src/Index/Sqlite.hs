module Index.Sqlite
  ( -- * API
    SqliteIndex
  , new
  , S.insert
  , S.insertL
  , S.size
  , S.rewind
   -- * Observations
  , S.view
  , S.getHistory
  , S.getNotifications
  ) where

import           Database.SQLite.Simple (Connection, open)

import           Index.Split            (SplitIndex (..))
import qualified Index.Split            as S

type SqliteIndex e n q r = SplitIndex IO Connection e n q r

new
  :: (SqliteIndex e n q r -> q -> [e] -> IO r)
  -> (e -> SqliteIndex e n q r -> IO [n])
  -> (SqliteIndex e n q r -> IO ())
  -> Int
  -> FilePath
  -> IO (Maybe (SqliteIndex e n q r))
new fquery foninsert fstore depth db
  | depth <= 0 = pure Nothing
  | otherwise  = do
    connection <- open db
    pure . Just $ SplitIndex
      { siHandle        = connection
      , siEvents        = []
      , siBuffered      = []
      , siNotifications = []
      , siDepth         = depth
      , siStore         = fstore
      , siQuery         = fquery
      , siOnInsert      = foninsert
      }
