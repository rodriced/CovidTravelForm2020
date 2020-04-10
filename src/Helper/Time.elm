module Helper.Time exposing (..)

import DateFormat
import Time


type alias TimeInfo =
    { time : Time.Posix
    , zone : Time.Zone
    }


formatDate : TimeInfo -> String
formatDate timeInfo =
    DateFormat.format "dd/MM/yyyy" timeInfo.zone timeInfo.time


formatTime : TimeInfo -> String
formatTime timeInfo =
    DateFormat.format "H" timeInfo.zone timeInfo.time
        ++ ":"
        ++ DateFormat.format "mm" timeInfo.zone timeInfo.time


posixToDate : Time.Zone -> Time.Posix -> String
posixToDate zone posix =
    DateFormat.format "dd/MM/yyyy" zone posix


posixToTime : Time.Zone -> Time.Posix -> String
posixToTime zone posix =
    DateFormat.format "H" zone posix
        ++ ":"
        ++ DateFormat.format "mm" zone posix
