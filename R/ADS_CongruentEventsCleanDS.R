#' ADS_CongruentEventsCleanDS
#'
#' Takes Augmented Data Set (ADS) and processes the Events data. Most importantly, the function cleans the Therapy Events (removal of redundant rows, etc.).
#'
#' Server-side ASSIGN method
#'
#' @param data Augmented Data Set object (list) on server | Default: 'ADS'
#' @return An ADS$Events object containing the following objects:
#'         \itemize{\item Events (data frame)
#'                 }
#' @export
#' @author Daniel Maier

ADS_CongruentEventsCleanDS <- function(ADS_Congruent){

  E <- ADS_Congruent$Events

  # Little helpers
  is.integer0 <- function(x){
    is.integer(x) && length(x) == 0L
  }

  `%notin%` <- Negate(`%in%`)

  # Retained Events
  EventRetained <- E %>%
    filter(EventClass %notin% "Therapy") %>%
    filter(EventSubclass %notin% c("Progress", "Metastasis")) %>%
    select(PatientID, DiagnosisID, EventDate, EventDateEnd, EventClass, EventSubclass, EventDetails)

  # Create systemic therapy specific events df includes all patients and diagnoses
  EventSystemic <- E %>%
    select(PatientID, DiagnosisID, EventDate, EventDateEnd, EventClass, EventSubclass, EventDetails) %>%
    filter(EventClass == "Therapy") %>%
    filter(EventSubclass %in% c("Bone Marrow Transplant", "Chemotherapy Mono",
                                "Hormone Therapy Mono", "Immunotherapy Mono",
                                "Other")) %>%
    tidyr::unnest(EventDetails)

  es0 <- nrow(EventSystemic)
  # create infomation count on details column specifying
  # what rows are more informative than others
  EventSystemic$InformativeDetails <- rowSums(!is.na(
    EventSystemic[, c("SystemicTherapyIntention",
                      "SystemicTherapySubstances", "SystemicTherapyRelationToSurgery")]))

  # keep only distinct rows in two steps
  # step 1: remove completely redundant rows
  # step 2: remove duplicated rows (Diagnosis, Dates, Substances)
  # order by informativity (!is.na) and keep first only

  EventSystemic <- EventSystemic %>%
    distinct() %>% # step 1
    mutate(tmpid = paste0(DiagnosisID, EventDate, EventDateEnd, SystemicTherapySubstances)) %>%
    group_by(tmpid) %>%
    arrange(desc(InformativeDetails)) %>%
    distinct(tmpid, .keep_all = TRUE) %>% # step 2
    ungroup() %>%
    select(-tmpid, -InformativeDetails)

  # Create therapy interruption period
  EventSystemic <- EventSystemic %>%
    group_by(PatientID, DiagnosisID) %>%
    arrange(lubridate::ymd(EventDate)) %>%
    mutate(TherapyInterruptionPeriod = lubridate::ymd(EventDate) -
             lag(lubridate::ymd(EventDateEnd))) %>%
    ungroup() %>%
    tidyr::nest(EventDetails = c(SystemicTherapyIntention,
                                 SystemicTherapySubstances,
                                 SystemicTherapyRelationToSurgery,
                                 TherapyInterruptionPeriod))
  es1 <- nrow(EventSystemic)

  print(paste0("Systemic therapy events intial: ", es0, " -> systemic therapy events cleaned: ", es1, "; difference: ", es0-es1))

  # Radiation
  EventRadiation <- E %>%
    select(PatientID, DiagnosisID, EventDate, EventDateEnd, EventClass, EventSubclass, EventDetails) %>%
    filter(EventClass == "Therapy") %>%
    filter(EventSubclass %in% c("Radiation Therapy")) %>%
    tidyr::unnest(EventDetails)

  er0 <- nrow(EventRadiation)

  EventRadiation$InformativeDetails <- rowSums(!is.na(
    EventRadiation[, c("RadiationTherapyIntention", "RadiationTherapyRelationToSurgery")]))

  EventRadiation <- EventRadiation %>%
    distinct() %>%
    mutate(tmpid = paste0(DiagnosisID, EventDate, EventDateEnd)) %>%
    group_by(tmpid) %>%
    arrange(desc(InformativeDetails)) %>%
    distinct(tmpid, .keep_all = TRUE) %>%
    ungroup() %>%
    select(-tmpid, -InformativeDetails) %>%
    tidyr::nest(EventDetails = c(RadiationTherapyIntention, RadiationTherapyRelationToSurgery))

  er1 <- nrow(EventRadiation)

  print(paste0("Radiation events intial: ", er0, " -> radiation events cleaned: ", er1, " ; difference: ", er0-er1))

  #Surgery
  EventSurgery <- E %>%
    select(PatientID, DiagnosisID, EventDate, EventDateEnd, EventClass, EventSubclass, EventDetails) %>%
    filter(EventClass == "Therapy") %>%
    filter(EventSubclass %in% c("Surgery")) %>%
    tidyr::unnest(EventDetails)

  esu0 <- nrow(EventSurgery)

  EventSurgery$InformativeDetails <- rowSums(!is.na(
    EventSurgery[, c("SurgeryIntention", "OPSCode", "ResidualAssessmentLocal",
                     "ResidualAssessmentTotal")]))

  EventSurgery <- EventSurgery %>%
    distinct() %>%
    mutate(tmpid = paste0(DiagnosisID, EventDate, OPSCode)) %>%
    group_by(tmpid) %>%
    arrange(desc(InformativeDetails)) %>%
    distinct(tmpid, .keep_all = TRUE) %>%
    ungroup() %>%
    select(-tmpid, -InformativeDetails) %>%
    tidyr::nest(EventDetails = c(SurgeryIntention, OPSCode, ResidualAssessmentLocal, ResidualAssessmentTotal))

  esu1 <- nrow(EventSurgery)

  print(paste0("Surgery events intial: ", esu0, " -> surgery events cleaned: ", esu1, "; difference: ", esu0-esu1))

  # Progression
  EventProgression <- E %>%
    select(PatientID, DiagnosisID, EventDate, EventDateEnd, EventClass, EventSubclass, EventDetails) %>%
    filter(EventSubclass %in% c("Progress")) %>%
    tidyr::unnest(EventDetails) %>%
    select(-LocalRelapseDate)

  ep0 <- nrow(EventProgression)

  EventProgression <- EventProgression %>%
    distinct() %>%
    tidyr::nest(EventDetails = c(GlobalStatus, LocalStatus, LymphnodalStatus, MetastasisStatus))

  ep1 <- nrow(EventProgression)

  print(paste0("Progression events intial: ", ep0, " -> progression events cleaned: ", ep1, "; difference: ", ep0-ep1))

  # Metastasis
  EventMetastasis <- E %>%
    select(PatientID, DiagnosisID, EventDate, EventDateEnd, EventClass, EventSubclass, EventDetails) %>%
    filter(EventSubclass %in% c("Metastasis")) %>%
    tidyr::unnest(EventDetails)

  em0 <- nrow(EventMetastasis)

  EventMetastasis <- EventMetastasis %>%
    distinct() %>%
    tidyr::nest(EventDetails = c(HasMetastasis, MetastasisLocalization))

  em1 <- nrow(EventMetastasis)

  print(paste0("Metastasis events intial: ", em0, " -> metastasis events cleaned: ", em1, "; difference: ", em0-em1))

  ADS_CongruentCleanedEvents <- rbind(EventRetained, EventSystemic, EventRadiation, EventSurgery, EventProgression, EventMetastasis) %>%
    select(PatientID, DiagnosisID, EventDate, EventDateEnd, EventClass, EventSubclass, EventDetails) %>%
    arrange(PatientID, DiagnosisID, EventDate)

  return(ADS_CongruentCleanedEvents)
}
