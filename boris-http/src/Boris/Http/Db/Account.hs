{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
module Boris.Http.Db.Account (
    userByGithubId
  , addUser
  , updateUser
  ) where


import           Boris.Http.Data

import           P

import           Traction.Control (MonadDb)
import           Traction.Sql (sql)
import qualified Traction.Sql as Traction


userByGithubId :: MonadDb m => GithubId -> m (Maybe (Identified GithubUser))
userByGithubId uid = do
  x <- Traction.unique [sql|
      SELECT id, github_id, github_login, github_name, github_email
        FROM account
       WHERE github_id = ?
    |] (Traction.Only $ githubId uid)
  pure . with x $ \(i, guid, login, name, email) ->
    Identified
      (UserId i)
      (GithubUser
        (GithubId guid)
        (GithubLogin login)
        (GithubName <$> name)
        (GithubEmail <$> email))

updateUser :: MonadDb m => Identified GithubUser -> m ()
updateUser user = do
  void $ Traction.execute [sql|
      UPDATE account
         SET github_id = ?
           , github_login = ?
           , github_name = ?
           , github_email = ?
           , updated = now()
       WHERE id = ?
    |] (githubId . githubUserId . userOf $ user
      , githubLogin . githubUserLogin . userOf $ user
      , fmap githubName . githubUserName. userOf $ user
      , fmap githubEmail . githubUserEmail . userOf $ user
      , getUserId . userIdOf $ user)

addUser :: MonadDb m => GithubUser -> m (Identified GithubUser)
addUser user = do
  i <- Traction.value $ Traction.mandatory [sql|
      INSERT INTO account (github_id, github_login, github_name, github_email)
           VALUES (?, ?, ?, ?)
        RETURNING id
    |] (githubId . githubUserId $ user
      , githubLogin . githubUserLogin $ user
      , fmap githubName . githubUserName $ user
      , fmap githubEmail . githubUserEmail $ user)
  pure $ Identified (UserId i) user
