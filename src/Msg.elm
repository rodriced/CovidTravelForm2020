module Msg exposing (..)

import File exposing (File)
import Helper.Time exposing (..)
import Model.FormField exposing (..)
import Time


type Msg
    = NoOp
    | FieldEditionStart FieldRef
    | FieldInput FieldRef String
    | FieldEditionStop (Maybe FieldValidation) FieldRef
    | MotivationCheckboxToggle Int
    | SignatureRequested
    | SignatureSelected File
    | SignatureLoaded String
    | SignatureRemoveRequested
    | TimeInfoLoaded TimeInfo
    | ValidatedTimeLoaded (Maybe Time.Posix)
    | HelpDisplayStatusToggle
    | StateButtonToggle
    | PrintRequested
      -- | PrintStart
    | PrintingInProgress
    | ClearForm


type FieldValidation
    = Valid
    | ValidThenNext
    | ValidThenPrev
