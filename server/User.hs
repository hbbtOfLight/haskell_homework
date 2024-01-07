{-# LANGUAGE OverloadedStrings #-}
module User where
import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import Data.Aeson
import Control.Applicative
import Data.Time (UTCTime, parseTimeM, defaultTimeLocale, formatTime)
import Data.Time.Clock (getCurrentTime)
import Text.Regex.Posix
import qualified Data.Text as T

data User = User
  { userName :: String
  , email :: String
  } deriving (Show)


validateEmail :: String -> Bool
validateEmail emailString =
  emailString =~ ("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$" :: String)

instance FromJSON User where
  parseJSON = withObject "User" $ \obj -> do
    emailString <- obj .: "email"
    validEmail <- pure $ validateEmail emailString
    if validEmail
      then do
        name <- obj .: "userName"
        return $ User name emailString
      else
        fail "Invalid email format"


instance ToJSON User where
  toJSON (User name userEmail) =
    object [ "userName" .= name
           , "email" .= userEmail
           ]
