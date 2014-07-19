{-# LANGUAGE TemplateHaskell   #-}
module DateTimePattern where

  import Database.Persist.TH

  data DateTimePattern = DateTimePattern { dateTimePattern :: ( Maybe Integer , Maybe Int , Maybe Int , Maybe Int , Maybe Int , Maybe Int ) } deriving ( Show , Read )
  derivePersistField "DateTimePattern"
