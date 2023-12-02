
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta_TableNames.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' CCP Meta Data: Table names
#'
#' A tibble containing meta data about table names in Raw Data Model (RDM) and Harmonized Data Model (HDM)
#'
#' @format ## `Meta_TableNames`
#' Tibble
#' \describe{
#'   \item{TableName_Raw}{Table name in Raw Data Model}
#'   \item{TableName_Harmonized}{Corresponding table name in Harmonized Data Model}
#' }
#' @source <https://github.com/BastianReiter/CCPhos/blob/main/Development/MetaData>
"Meta_TableNames"



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta_FeatureNames.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' CCP Meta Data: Feature names
#'
#' A tibble containing meta data about feature names in Raw Data Model (RDM) and Harmonized Data Model (HDM)
#'
#' @format ## `Meta_FeatureNames`
#' Tibble
#' \describe{
#'   \item{TableName_Harmonized}{Table selector}
#'   \item{FeaturePosition}{Position of Feature in Table}
#'   \item{FeatureName_Raw}{Feature name in Raw Data Model}
#'   \item{FeatureName_Harmonized}{Feature name in Harmonized Data Model}
#' }
#' @source <https://github.com/BastianReiter/CCPhos/blob/main/Development/MetaData>
"Meta_FeatureNames"



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta_ValueSets.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' CCP Meta Data: Value sets
#'
#' A tibble containing meta data about eligible data values in Harmonized Data Model (HDM)
#'
#' @format ## `Meta_ValueSets`
#' Tibble
#' \describe{
#'   \item{TableName_Harmonized}{Table selector}
#'   \item{FeaturePosition}{Position of Feature in Table}
#'   \item{FeatureName}{Feature name}
#'   \item{ScaleLevel}{Scale level of feature}
#'   \item{Value_Raw}{Value in original / raw data}
#'   \item{Value_Harmonized}{Value as preferred}
#'   \item{Label_Harmonized}{Label for coded feature values}
#'   \item{Label_Raw}{Label in original data}
#'   \item{FactorRank}{Used to determine order in values}
#' }
#' @source <https://github.com/BastianReiter/CCPhos/blob/main/Development/MetaData>
"Meta_ValueSets"
