

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta_CancerGrouping.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' General meta data: ICD-10 cancer grouping
#'
#' A tibble containing meta data about grouping of ICD-10 cancer codes
#'
#' @format ## `Meta_CancerGrouping`
#' Tibble
#' \describe{
#'   \item{ICD10CodeShort}{Three digit ICD-10 code}
#'   \item{CancerTopographyGroup_ICD10}{Topography group as put forth by ICD-10}
#'   \item{CancerTopographyOrgan_ICD10}{Affected organ / topography as put forth by ICD-10}
#'   \item{CancerTopographyGroup_ZFKD}{Topography detail as put forth by ZFKD}
#'   \item{CancerTopographySpecification}{Additional information on topography}
#'   \item{CancerSpecification}{Specification of cancer entity where needed}
#'   \item{CancerIsLikelyToMetastasize}{}
#'   \item{CancerIsCarcinomaInSitu}{}
#'   \item{CancerIsNeoplasmOfUncertainBehavior}{}
#' }
#' @source <https://github.com/BastianReiter/TinkerLab/tree/main/Development/Data>
"Meta_CancerGrouping"



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta_TableNames.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' CCP meta data: Table names
#'
#' A tibble containing meta data about corresponding table names in Raw Data Model (RDM) and Curated Data Model (CDM)
#'
#' @format ## `Meta_TableNames`
#' Tibble
#' \describe{
#'   \item{TableName_Raw}{Table name in Raw Data Model}
#'   \item{TableName_Curated}{Corresponding table name in Curated Data Model}
#' }
#' @source <https://github.com/BastianReiter/dsCCPhos/blob/main/Development/MetaData>
"Meta_TableNames"



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta_FeatureNames.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' CCP meta data: Feature names
#'
#' A tibble containing meta data about corresponding feature names in Raw Data Model (RDM) and Curated Data Model (CDM)
#'
#' @format ## `Meta_FeatureNames`
#' Tibble
#' \describe{
#'   \item{TableName_Curated}{Table selector}
#'   \item{FeaturePosition}{Position of Feature in Table}
#'   \item{FeatureName_Raw}{Feature name in Raw Data Model}
#'   \item{FeatureName_Curated}{Corresponding feature name in Curated Data Model}
#' }
#' @source <https://github.com/BastianReiter/dsCCPhos/blob/main/Development/MetaData>
"Meta_FeatureNames"



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta_ValueSets.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' CCP meta data: Value sets
#'
#' A tibble containing meta data about eligible data values in Curated Data Model (CDM)
#'
#' @format ## `Meta_ValueSets`
#' Tibble
#' \describe{
#'   \item{TableName_Curated}{Table selector}
#'   \item{FeatureName}{Feature name}
#'   \item{ScaleLevel}{Scale level of feature}
#'   \item{Value_Raw}{Value in original / raw data}
#'   \item{Value_Curated}{Value as preferred}
#'   \item{Label_Curated}{Label for coded feature values}
#'   \item{Label_Raw}{Label in original data}
#'   \item{FactorRank}{Used to determine order in values}
#' }
#' @source <https://github.com/BastianReiter/dsCCPhos/blob/main/Development/MetaData>
"Meta_ValueSets"
