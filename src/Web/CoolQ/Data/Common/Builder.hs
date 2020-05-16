module Web.CoolQ.Data.Common.Builder where

import           Web.CoolQ.Data.Common

import           Control.Monad.Writer  (WriterT, execWriterT, tell)
import           Data.Functor.Identity (Identity, runIdentity)
import           Data.Text             (Text)

type MessageBuilderT = WriterT Message

text :: (Monad m) => Text -> MessageBuilderT m ()
text t = tell $ textMessage t

image :: (Monad m) => Text -> MessageBuilderT m ()
image file = tell $ imageMessage file

face :: (Monad m) => Text -> MessageBuilderT m ()
face id = tell $ faceMessage id

buildMessageT :: (Monad m) => MessageBuilderT m a -> m Message
buildMessageT = execWriterT

buildMessage :: MessageBuilderT Identity a -> Message
buildMessage = runIdentity . buildMessageT
