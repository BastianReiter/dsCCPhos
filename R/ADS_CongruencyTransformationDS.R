#' ADS_CongruencyTransformationDS
#'
#' Takes Augmented Data Set (ADS) and processes the data data frames. Most importantly, the function secures the congruency of Patients, Diagnoses and Events. Only entries are retained that refer to the same set of patients.
#'
#' Server-side ASSIGN method
#'
#' @param data Augmented Data Set object (list) on server | Default: 'ADS'
#' @return An ADS list-object containing the following objects:
#'         \itemize{
#'         \item Patients (data frame)
#'         \item Diagnoses (data frame)
#'         \item Events (data frame)
#'                 }
#' @export
#' @author Daniel Maier


ADS_CongruencyTransformationDS <- function (data = ADS){

  # auxfunction
  require(dplyr)
  `%notin%` <- Negate(`%in%`)

  # rename data object components
  P <- data$Patients
  E <- data$Events
  D <- data$Diagnoses

  # patient uniqueness check for data frame P
  if (length(P$PatientID) == length(unique(P$PatientID))){
    print("No duplicate PatientIDs in Table Patient.")
  }

  # intersect of patient ids in patient and diagnosis
  Inter1 <- dplyr::intersect(P$PatientID, D$PatientID)

  # intersect of inter1 and events
  vec_PatientIDsIntersect <- dplyr::intersect(Inter1, E$PatientID)

  # discard patient_ids not in intersect (either no entries in patient, events or diagnosis)
  # count patient ids
  n_PatDiscard <- length(which(P$PatientID %notin% vec_PatientIDsIntersect))

  # filter patient ids from P
  P <- P %>%
    filter(PatientID %in% vec_PatientIDsIntersect)

  # message
  print(paste0("Removing: ", n_PatDiscard," records from ADS Patients table."))

  # filter patient ids from D
  n_DiagDiscard <- length(which(D$PatientID %notin% vec_PatientIDsIntersect))
  D <- D %>%
    filter(PatientID %in% vec_PatientIDsIntersect)

  # message
  print(paste0("Removing: ", n_DiagDiscard, " records from ADS Diagnosis table."))

  # filter patient ids from E
  n_EventsDiscard <- length(which(E$PatientID %notin% vec_PatientIDsIntersect))
  E <- E %>%
    filter(PatientID %in% vec_PatientIDsIntersect)

  # message
  print(paste0("Removing: ", n_EventsDiscard," records from ADS Events table."))

  # if all patient ids from P in D and E
  if (all(P$PatientID %in% unique(D$PatientID)) &
      all(P$PatientID %in% unique(E$PatientID)) &
      all(unique(D$PatientID) %in% unique(E$PatientID))){

    # if TRUE message
    print("Congruency check passed: ADS tables contain data about the same set of patients.")

  }
  else {

    # else Warning message
    print("Warning in congruency check. Please re-investiage processing.")
  }

  # re-allocate
  data$Patients <- P
  data$Diagnoses <- D
  data$Events <- E

  # return
  return(data)
}
