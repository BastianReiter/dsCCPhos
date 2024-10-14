#' ADS_PostprocessDS
#'
#' Takes Augmented Data Set (ADS) and secures congruency of Patients, Diagnoses and Events. The Events data frame is cleaned and standardized. The function also selects relevant features for further processing. The function is intended to be used after the ADS has been augmented with additional information.
#'
#' Server-side ASSIGN method
#'
#' @param data Augmented Data Set object (list) on server | Default: 'ADS'
#' @return An ADS list object containing the following objects:
#'         \itemize{\item Patients (data frame)
#'                  \item Diagnoses (data frame)
#'                  \item Events (data frame)
#'                 }
#' @export
#' @author Daniel Maier

ADS_PostprocessDS <- function(data = ADS){

  data <- eval(parse(text = data), envir = parent.frame())

  # 1. Congruency Transformation
  ADS_Congruent <- ADS_CongruencyTransformationDS(data)

  # 2. Standardization of events data
  ADS_Congruent$Events <- ADS_CongruentEventsCleanDS(ADS_Congruent)

  # 3. Select relevant features from Patients and Diagnoses
  ADS_Congruent$Patients <- ADS_Congruent$Patients %>%
    select(PatientID, DateOfBirth, Gender, LastVitalStatusDate, LastVitalStatus,
           DeathCancerRelated, CausesOfDeath, DiagnosisID, ICD10Code,
           InitialDiagnosisDate, DiagnosisLabel, PatientAgeAtDiagnosis,
           TimeDiagnosisToDeath, TimeFollowUp)

  ADS_Congruent$Diagnoses <- ADS_Congruent$Diagnoses %>%
    select(-PatientAgeAtDiagnosis, -TimeDiagnosisToDeath, -TimeFollowUp, -IsDocumentedDeceased)

  # 4. Secure congruency of Patients, Diagnoses and Events
  ADS_Postprocessed <- ADS_CongruencyTransformationDS(ADS_Congruent)

  return(ADS_Postprocessed)
}
