{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Reading and writing data to and from a Postgres database.

This module simply contains conveniences for using the most excellent
__postgresql-simple__ library in concert with the rest of the packages in
this collection.
-}
module Core.Persistence.Postgres (
    ) where

import Core.Text (Rope, fromRope)
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import Database.PostgreSQL.Simple.ToField (Action (..), ToField, toField)

instance FromField Rope where
    fromField f dat = fromRope <$> fromField f dat

instance ToField Rope where
    toField r = Escape $ fromRope r
