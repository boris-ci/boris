{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
module Boris.Http.Db.Session (
    newSession
  , tickSession
  , getSession
  , getSessionUser
  , getSessionOAuth
  ) where

import           Boris.Http.Data

import           P

import           Traction.Control (MonadDb)
import           Traction.Sql (sql)
import qualified Traction.Sql as Traction


newSession :: MonadDb m => Session -> UserId -> m ()
newSession session user = do
  void $ Traction.execute [sql|
      INSERT INTO session (id, account, oauth)
           VALUES (?, ?, ?)
    |] (getSessionId . sessionIdentifier $ session
      , getUserId user
      , githubOAuth . sessionOAuth $ session)

tickSession :: MonadDb m => SessionId -> m ()
tickSession session = do
  void $ Traction.execute [sql|
      UPDATE session
         SET updated = now()
       WHERE id = ?
    |] (Traction.Only . getSessionId $ session)

getSessionUser :: MonadDb m => SessionId -> m (Maybe UserId)
getSessionUser session = do
  (fmap . fmap) UserId . Traction.values $ Traction.unique [sql|
      SELECT account
        FROM session
       WHERE id = ?
         AND updated > now() - INTERVAL '1 day'
    |] (Traction.Only $ getSessionId $ session)

getSessionOAuth :: MonadDb m => SessionId -> m (Maybe GithubOAuth)
getSessionOAuth session = do
  (fmap . fmap) GithubOAuth . Traction.values $ Traction.unique [sql|
      SELECT oauth
        FROM session
       WHERE id = ?
         AND updated > now() - INTERVAL '1 day'
    |] (Traction.Only $ getSessionId $ session)


getSession :: MonadDb m => SessionId -> m (Maybe AuthenticatedUser)
getSession session = do
  x <- Traction.unique [sql|
      SELECT s.account, s.oauth, a.github_id, a.github_login, a.github_name, a.github_email
        FROM session s
        JOIN account a ON s.account = a.id
       WHERE s.id = ?
         AND s.updated > now() - INTERVAL '1 day'
    |] (Traction.Only $ getSessionId $ session)
  pure . with x $ \(i, oauth, guid, login, name, email) ->
    AuthenticatedUser
      (Identified
        (UserId i)
        (GithubUser
          (GithubId guid)
          (GithubLogin login)
          (GithubName <$> name)
          (GithubEmail <$> email)))
      (Session session $ GithubOAuth oauth)
