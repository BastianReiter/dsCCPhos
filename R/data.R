
# --- DOCUMENTATION of package data ---


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta_CancerGrouping.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' General meta data: ICD-10 cancer grouping
#'
#' A tibble containing meta data about grouping of ICD-10 cancer codes
#'
#' @format ## `Meta_CancerGrouping`
#' \code{tibble}
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
#' @author Bastian Reiter
"Meta_CancerGrouping"



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta_DataHarmonizationRules.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Meta data on rule-based transformation of raw data
#'
#' A tibble containing rules
#'
#' @format ## `Meta_DataHarmonizationRules`
#' \code{tibble}
#' \describe{
#'   \item{Profile}{}
#'   \item{Table}{}
#'   \item{Feature}{}
#'   \item{EvaluationOrder}{}
#'   \item{Operation}{}
#'   \item{LookupValue}{}
#'   \item{NewValue}{}
#'   \item{Comment}{}
#' }
#' @source <https://github.com/BastianReiter/dsCCPhos/blob/main/Development/MetaData>
#' @author Bastian Reiter
"Meta_DataHarmonizationRules"



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta_DiagnosisAssociation.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Meta data on rule-based identification of associations between diagnosis entries
#'
#' A tibble containing rules
#'
#' @format ## `Meta_DiagnosisAssociation`
#' \code{tibble}
#' \describe{
#'   \item{Profile}{}
#'   \item{Feature}{}
#'   \item{Value}{}
#'   \item{ValueRank}{}
#'   \item{EvaluationOrder}{}
#'   \item{Condition}{}
#' }
#' @source <https://github.com/BastianReiter/dsCCPhos/blob/main/Development/MetaData>
#' @author Bastian Reiter
"Meta_DiagnosisAssociation"



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta_DiagnosisRedundancy.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Meta data on rule-based identification of redundant diagnosis entries
#'
#' A tibble containing rules
#'
#' @format ## `Meta_DiagnosisRedundancy`
#' \code{tibble}
#' \describe{
#'   \item{Profile}{}
#'   \item{Feature}{}
#'   \item{Value}{}
#'   \item{ValueRank}{}
#'   \item{EvaluationOrder}{}
#'   \item{Condition}{}
#' }
#' @source <https://github.com/BastianReiter/dsCCPhos/blob/main/Development/MetaData>
#' @author Bastian Reiter
"Meta_DiagnosisRedundancy"



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta_EventFeatures.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Meta_EventFeatures
#'
#' Rules for engineering of informative features in event data
#'
#' @format ## `Meta_EventFeatures`
#' \code{tibble}
#' \describe{
#'   \item{Profile}{}
#'   \item{Feature}{}
#'   \item{Value}{}
#'   \item{ValueRank}{}
#'   \item{EvaluationOrder}{}
#'   \item{Condition}{}
#' }
#' @source <https://github.com/BastianReiter/dsCCPhos/blob/main/Development/MetaData>
#' @author Bastian Reiter
"Meta_EventFeatures"



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta_Features.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' CCP meta data: Features
#'
#' A tibble containing meta data about corresponding feature names in Raw Data Model (RDM) and Curated Data Model (CDM)
#'
#' @format ## `Meta_Features`
#' \code{tibble}
#' \describe{
#'   \item{FeatureID}{}
#'   \item{TableName_Raw}{Name of table in Opal}
#'   \item{TableName_Curated}{Name of table after loading into R session}
#'   \item{FeaturePosition}{Position of Feature in Table}
#'   \item{FeatureName_Raw}{Feature name in Raw Data Model}
#'   \item{FeatureName_Curated}{Corresponding feature name in Curated Data Model}
#'   \item{IsPrimaryKey}{Indicating whether feature serves as primary key for corresponding table}
#'   \item{Type}{Data type}
#'   \item{Scale}{Scale of measure}
#'   \item{HasEligibleValueSet}{Indicating whether values of feature are part of a finite, discrete eligible value set}
#'   \item{IsDiscriminatory}{Indicating whether feature is used to strictly discriminate between different entries. Used for discrimination of table entries in redundancy classification.}
#'   \item{IsEssential}{Indicating whether feature holds essential information. Used for discrimination of table entries in redundancy classification.}
#' }
#' @source <https://github.com/BastianReiter/dsCCPhos/blob/main/Development/MetaData>
#' @author Bastian Reiter
"Meta_Features"



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta_FeatureObligations.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Containing preferences on which features in CDS should be considered obligatory
#'
#' A tibble
#'
#' @format ## `Meta_FeatureObligations`
#' \code{tibble}
#' \describe{
#'   \item{Table}{}
#'   \item{Feature}{}
#'   \item{Default}{}
#' }
#' @source <https://github.com/BastianReiter/dsCCPhos/blob/main/Development/MetaData>
#' @author Bastian Reiter
"Meta_FeatureObligations"



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta_FeatureTracking.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Containing preferences on which features should be tracked/monitored during curation process
#'
#' A tibble
#'
#' @format ## `Meta_FeatureTracking`
#' \code{tibble}
#' \describe{
#'   \item{Table}{}
#'   \item{Feature}{}
#'   \item{HasEligibleValueSet}{Indicating whether values of feature are part of a finite, discrete eligible value set}
#'   \item{Default}{}
#' }
#' @source <https://github.com/BastianReiter/dsCCPhos/blob/main/Development/MetaData>
#' @author Bastian Reiter
"Meta_FeatureTracking"



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta_Tables.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' CCP meta data: Tables
#'
#' A tibble containing meta data about corresponding table names in Raw Data Model (RDM) and Curated Data Model (CDM)
#'
#' @format ## `Meta_Tables`
#' \code{tibble}
#' \describe{
#'   \item{TableName_Raw}{Table name in Raw Data Model}
#'   \item{TableName_Curated}{Corresponding table name in Curated Data Model}
#' }
#' @source <https://github.com/BastianReiter/dsCCPhos/blob/main/Development/MetaData>
#' @author Bastian Reiter
"Meta_Tables"



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta_Values.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' CCP meta data: Values
#'
#' A tibble containing meta data about data values
#'
#' @format ## `Meta_Values`
#' \code{tibble}
#' \describe{
#'   \item{FeatureID}{}
#'   \item{Table}{Table name}
#'   \item{Feature}{Feature name}
#'   \item{ScaleLevel}{Scale level of feature}
#'   \item{Value_Raw}{Value in original / raw data}
#'   \item{Value_Curated}{Value as preferred}
#'   \item{Label_Curated}{Label for coded feature values}
#'   \item{Label_Raw}{Label in original data}
#'   \item{FactorRank}{Used to determine order in values}
#'   \item{ComparatorCode}{Assignment of numeric value to certain non-numeric values to enable comparison operations}
#' }
#' @source <https://github.com/BastianReiter/dsCCPhos/blob/main/Development/MetaData>
#' @author Bastian Reiter
"Meta_Values"
