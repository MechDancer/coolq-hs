module Web.CoolQ.Data.Common.Builder where

import           Web.CoolQ.Data.Common

import           Control.Monad.State   (StateT, execStateT, modify)
import           Data.Functor.Identity (Identity, runIdentity)
import           Data.Text             (Text)

type MessageBuilderT = StateT Message

text :: (Monad m) => Text -> MessageBuilderT m ()
text t = modify (<> textMessage t)

image :: (Monad m) => Text -> MessageBuilderT m ()
image file = modify (<> imageMessage file)

face :: (Monad m) => Text -> MessageBuilderT m ()
face id = modify (<> faceMessage id)

buildMessageT :: (Monad m) => MessageBuilderT m a -> m Message
buildMessageT builder = execStateT builder (Message [])

buildMessage :: MessageBuilderT Identity a -> Message
buildMessage = runIdentity . buildMessageT
