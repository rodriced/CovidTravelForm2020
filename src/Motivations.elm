module Motivations exposing (..)


motivationsDescription : List String
motivationsDescription =
    [ """Déplacements entre le domicile et le lieu d’exercice de l’activité professionnelle, lorsqu’ils sont indispensables à l’exercice d’activités ne pouvant être organisées sous forme de télétravail ou déplacements professionnels ne pouvant être différés (2)."""
    , """Déplacements pour effectuer des achats de fournitures nécessaires à l’activité professionnelle et des achats de première nécessité (3) dans des établissements dont les activités demeurent autorisées (liste sur gouvernement.fr).
"""
    , """Consultations et soins ne pouvant être assurés à distance et ne pouvant être différés ; consultations et soins des patients atteints d'une affection de longue durée.
"""
    , """Déplacements pour motif familial impérieux, pour l’assistance aux personnes vulnérables ou la garde d’enfants."""
    , """Déplacements brefs, dans la limite d'une heure quotidienne et dans un rayon maximal d'un kilomètre autour du domicile, liés soit à l'activité physique individuelle des personnes, à l'exclusion de toute pratique sportive collective et de toute proximité avec d'autres personnes, soit à la promenade avec les seules personnes regroupées dans un même domicile, soit aux besoins des animaux de compagnie."""
    , """Convocation judiciaire ou administrative."""
    , """Participation à des missions d’intérêt général sur demande de l’autorité administrative."""
    ]


emptyMotivations : List Bool
emptyMotivations =
    List.repeat (List.length motivationsDescription) False
