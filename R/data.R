
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# --- DOCUMENTATION of package data ---
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DisclosureSettings.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Disclosure settings when using CCPhos
#'
#' A \code{list} containing settings concerning data privacy and disclosure mitigation
#'
#' @format ## `DisclosureSettings`
#' \code{list}
#' \describe{
#'    \item{Profile}{Can be 'strict' or 'loose'}
#'    \item{NThreshold}{The minimum sample size required for transmission of aggregated data to client}
#' }
#' @source <https://github.com/BastianReiter/dsCCPhos/blob/main/Development/MetaData>
#' @author Bastian Reiter
"DisclosureSettings"



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta.Features.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' CCP meta data: Features
#'
#' A tibble containing meta data about corresponding feature names in Raw Data Model (RDM) and Curated Data Model (CDM)
#'
#' @format ## `Meta.Features`
#' \code{tibble}
#' \describe{
#'   \item{FeatureID}{}
#'   \item{TableName.Raw}{Name of table in Opal}
#'   \item{TableName.Curated}{Name of table after loading into R session}
#'   \item{FeaturePosition}{Position of Feature in Table}
#'   \item{FeatureName.Raw}{Feature name in Raw Data Model}
#'   \item{FeatureName.Curated}{Corresponding feature name in Curated Data Model}
#'   \item{IsPrimaryKey}{Indicating whether feature serves as primary key for corresponding table}
#'   \item{Type}{Data type}
#'   \item{Scale}{Scale of measure}
#'   \item{HasEligibleValueSet}{Indicating whether values of feature are part of a finite, discrete eligible value set}
#'   \item{IsDiscriminatory}{Indicating whether feature is used to strictly discriminate between different entries. Used for discrimination of table entries in redundancy classification.}
#'   \item{IsEssential}{Indicating whether feature holds essential information. Used for discrimination of table entries in redundancy classification.}
#' }
#' @source <https://github.com/BastianReiter/dsCCPhos/blob/main/Development/MetaData>
#' @author Bastian Reiter
"Meta.Features"



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta.Tables.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' CCP meta data: Tables
#'
#' A tibble containing meta data about corresponding table names in Raw Data Model (RDM) and Curated Data Model (CDM)
#'
#' @format ## `Meta.Tables`
#' \code{tibble}
#' \describe{
#'   \item{TableName.Raw}{Table name in Raw Data Model}
#'   \item{TableName.Curated}{Corresponding table name in Curated Data Model}
#' }
#' @source <https://github.com/BastianReiter/dsCCPhos/blob/main/Development/MetaData>
#' @author Bastian Reiter
"Meta.Tables"



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Meta.Values.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' CCP meta data: Values
#'
#' A tibble containing meta data about data values
#'
#' @format ## `Meta.Values`
#' \code{tibble}
#' \describe{
#'   \item{FeatureID}{}
#'   \item{Table}{Table name}
#'   \item{Feature}{Feature name}
#'   \item{ScaleLevel}{Scale level of feature}
#'   \item{Value.Raw}{Value in original / raw data}
#'   \item{Value.Curated}{Value as preferred}
#'   \item{Label.Curated}{Label for coded feature values}
#'   \item{Label.Raw}{Label in original data}
#'   \item{FactorRank}{Used to determine order in values}
#'   \item{ComparatorCode}{Assignment of numeric value to certain non-numeric values to enable comparison operations}
#' }
#' @source <https://github.com/BastianReiter/dsCCPhos/blob/main/Development/MetaData>
#' @author Bastian Reiter
"Meta.Values"



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Proc.EventFeatures.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Proc.EventFeatures
#'
#' Processing rules for engineering of informative features in event data
#'
#' @format ## `Proc.EventFeatures`
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
"Proc.EventFeatures"



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Proc.TableNormalization.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Processing preferences on table normalization operations
#'
#' A tibble
#'
#' @format ## `Proc.TableNormalization`
#' \code{tibble}
#' \describe{
#'   \item{Profile}{}
#'   \item{FeatureID}{}
#'   \item{Table}{}
#'   \item{Feature}{}
#'   \item{EvaluationOrder}{}
#'   \item{Operation}{}
#'   \item{Comment}{}
#' }
#' @source <https://github.com/BastianReiter/dsCCPhos/blob/main/Development/MetaData>
#' @author Bastian Reiter
"Proc.TableNormalization"



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Res.CancerGrouping.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Resource data on ICD-10 cancer grouping
#'
#' A tibble containing data about grouping of ICD-10 cancer codes
#'
#' @format ## `Res.CancerGrouping`
#' \code{tibble}
#' \describe{
#'   \item{ICD10Code.Short}{Three digit ICD-10 code}
#'   \item{CancerTopographyGroup.ICD10}{Topography group as put forth by ICD-10}
#'   \item{CancerTopographyOrgan.ICD10}{Affected organ / topography as put forth by ICD-10}
#'   \item{CancerTopographyGroup.ZFKD}{Topography detail as put forth by ZFKD}
#'   \item{CancerTopographySpecification}{Additional information on topography}
#'   \item{CancerSpecification}{Specification of cancer entity where needed}
#'   \item{CancerIsLikelyToMetastasize}{}
#'   \item{CancerIsCarcinomaInSitu}{}
#'   \item{CancerIsNeoplasmOfUncertainBehavior}{}
#' }
#' @source <https://github.com/BastianReiter/TinkerLab/tree/main/Development/Data>
#' @author Bastian Reiter
"Res.CancerGrouping"



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set.DataHarmonization.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Settings on which feature should be harmonized and which methods to use for each feature
#'
#' A tibble
#'
#' @format ## `Set.DataHarmonization`
#' \code{tibble}
#' \describe{
#'   \item{Profile}{}
#'   \item{FeatureID}{}
#'   \item{Table}{}
#'   \item{Feature}{}
#'   \item{HasEligibleValueSet}{}
#'   \item{RunHarmonization}{}
#'   \item{HarmonizationOrder}
#'   \item{Method.TransformativeExpressions}{}
#'   \item{Method.Dictionary}{}
#'   \item{Method.FuzzyStringMatching}{}
#'   \item{Method.NaiveBayes}{}
#' }
#' @source <https://github.com/BastianReiter/dsCCPhos/blob/main/Development/MetaData>
#' @author Bastian Reiter
"Set.DataHarmonization"



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set.DiagnosisAssociation.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Settings on rule-based identification of associations between diagnosis entries
#'
#' A tibble containing rules
#'
#' @format ## `Set.DiagnosisAssociation`
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
"Set.DiagnosisAssociation"



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set.DiagnosisRedundancy.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Settings on rule-based identification of redundant diagnosis entries
#'
#' A tibble containing rules
#'
#' @format ## `Set.DiagnosisRedundancy`
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
"Set.DiagnosisRedundancy"



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set.Dictionary.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Dictionary data used in Data Harmonization
#'
#' A tibble containing look-up values and corresponding replacements
#'
#' @format ## `Set.Dictionary`
#' \code{tibble}
#' \describe{
#'   \item{Profile}{}
#'   \item{FeatureID}{}
#'   \item{Table}{}
#'   \item{Feature}{}
#'   \item{LookupValue}{}
#'   \item{NewValue}{}
#'   \item{Comment}{}
#' }
#' @source <https://github.com/BastianReiter/dsCCPhos/blob/main/Development/MetaData>
#' @author Bastian Reiter
"Set.Dictionary"



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set.FeatureObligations.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Containing preferences on which features in CDS should be considered obligatory
#'
#' A tibble
#'
#' @format ## `Set.FeatureObligations`
#' \code{tibble}
#' \describe{
#'   \item{Table}{}
#'   \item{Feature}{}
#'   \item{Default}{}
#' }
#' @source <https://github.com/BastianReiter/dsCCPhos/blob/main/Development/MetaData>
#' @author Bastian Reiter
"Set.FeatureObligations"



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set.FeatureTracking.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Settings on which features should be tracked/monitored during curation process
#'
#' A tibble
#'
#' @format ## `Set.FeatureTracking`
#' \code{tibble}
#' \describe{
#'   \item{Table}{}
#'   \item{Feature}{}
#'   \item{HasEligibleValueSet}{Indicating whether values of feature are part of a finite, discrete eligible value set}
#'   \item{Default}{}
#' }
#' @source <https://github.com/BastianReiter/dsCCPhos/blob/main/Development/MetaData>
#' @author Bastian Reiter
"Set.FeatureTracking"



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set.FuzzyStringMatching.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Set.FuzzyStringMatching
#'
#' Feature-specific settings for Fuzzy String Matching
#'
#' @format ## `Set.FuzzyStringMatching`
#' \code{tibble}
#' \describe{
#'   \item{Profile}{}
#'   \item{FeatureID}{}
#'   \item{Table}{}
#'   \item{Feature}{}
#'   \item{HasEligibleValueSet}{}
#'   \item{PreferredMethod}{}
#'   \item{FindBestMethod}{}
#'   \item{Tolerance}{}
#'   \item{Preprocessing.FlattenCase}{}
#'   \item{Preprocessing.RemoveAllWhiteSpace}{}
#'   \item{Preprocessing.SquishWhiteSpace}{}
#'   \item{Stringdist.useBytes}{}
#'   \item{Stringdist.weight.d}{}
#'   \item{Stringdist.weight.i}{}
#'   \item{Stringdist.weight.s}{}
#'   \item{Stringdist.weight.t}{}
#'   \item{Stringdist.q}{}
#'   \item{Stringdist.p}{}
#'   \item{Stringdist.bt}{}
#' }
#' @source <https://github.com/BastianReiter/dsCCPhos/blob/main/Development/MetaData>
#' @author Bastian Reiter
"Set.FuzzyStringMatching"



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set.TransformativeExpressions.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Settings on transformative expressions used in Data Harmonization
#'
#' A tibble
#'
#' @format ## `Set.TransformativeExpressions`
#' \code{tibble}
#' \describe{
#'   \item{Profile}{}
#'   \item{FeatureID}{}
#'   \item{Table}{}
#'   \item{Feature}{}
#'   \item{EvaluationOrder}{}
#'   \item{Expression}{}
#'   \item{Comment}{}
#' }
#' @source <https://github.com/BastianReiter/dsCCPhos/blob/main/Development/MetaData>
#' @author Bastian Reiter
"Set.TransformativeExpressions"

