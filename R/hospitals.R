
#' Hospital admitted days-based fraud trigger
#' @description For each procedure there is a reasonable number of days a claimant could be admitted in the hospital.If the number of days actually admitted is higher than what is reasonable for that procedure this function will raise flag.
#' @param claims_file  data set containing claim-level information
#' @param no_of_days_stayed column in the claims_file that contains the number of days actually admitted in the hospital
#' @param triggers_file file containing the reasonable number of days a claimant could be admitted in the hospital for each procedure
#' @param procedure_code column in the triggers_file that contains the code/identifier for the medical procedure
#' @param admission_days_trigger column in the triggers_file that contains the reasonable number of days a claimant could be admitted in the hospital for each procedure
#'
#' @importFrom rlang sym
#' @import dplyr
#'
#' @return input claims_file with no_days_ratio and hosp_days_flag columns
#' @export
#'
#' @examples
#'
#' library(dplyr)
#'
#' data(claims_data_sample)
#' data(trigger_data)
#'
#' hosp_days_trigger_map(
#' claims_file = claims_data_sample,
#' no_of_days_stayed = "no_of_days_stayed",
#' triggers_file = trigger_data,
#' procedure_code = "primary_procedure_code",
#' admission_days_trigger = "admission_days_trigger") %>%
#' select(c("no_of_days_stayed", "admission_days_trigger", "hosp_days_flag"))
#'
hosp_days_trigger_map <- function(claims_file, no_of_days_stayed, triggers_file, procedure_code, admission_days_trigger){

  stopifnot("no_of_days_stayed doesn't exist in the claims_file"=no_of_days_stayed %in% colnames(claims_file))
  stopifnot("procedure_code doesn't exist in the claims_file"=procedure_code %in% colnames(claims_file))

  stopifnot("triggers file doesn't have the required columns"= c(procedure_code, admission_days_trigger) %in% colnames(triggers_file))

  stopifnot("no_of_days_stayed is not in numeric format"=is.numeric(claims_file[[no_of_days_stayed]]))
  stopifnot("admission_days_trigger is not in numeric format"=is.numeric(triggers_file[[admission_days_trigger]]))


  no_of_days_stayed <- sym(no_of_days_stayed)
  admission_days_trigger <- sym(admission_days_trigger)

  claims_file %>%
    left_join(triggers_file, by = procedure_code) %>%
    mutate(hosp_days_flag = case_when(!!no_of_days_stayed > !!admission_days_trigger ~ 1,
                                      TRUE ~ 0))

}







#' Empanelled hospitals(medical service providers)-based fraud trigger
#' @description The insurer generally empanells hospitals to service its policy holders. In the process of empanelment the insurer ensures that the hospital has the required facilities and also agrees the tarrif for each of the treatments. If the hospital mentioned is not a part of the empanelled list of hospitals it will raise a flag
#' @param claims_file  data set containing claim-level information
#' @param hospital_id_field column with unique identity number of the hospitals
#' @param empanelled_hospitals_list file containing the list of all the empanelled hospitals
#' @param empanelled_hospital_id column in the empanelled_hospital_list file that contains all the unique hospital ids
#'
#' @importFrom rlang sym
#' @import dplyr
#'
#' @return input claims_file with hospital_empanelled_flag column
#' @export
#'
#' @examples
#'
#' library(dplyr)
#'
#' data(claims_data_sample)
#' data(hospital_list)
#'
#' hospital_empanelled_trigger_map(
#' claims_file = claims_data_sample,
#' hospital_id_field = "medical_service_provider_id",
#' empanelled_hospitals_list = hospital_list,
#' empanelled_hospital_id = "hosp_id") %>%
#' group_by(medical_service_provider_id)%>%
#' summarise(hospital_empanelled_flag = mean(hospital_empanelled_flag))
#'
hospital_empanelled_trigger_map <- function(claims_file, hospital_id_field,empanelled_hospitals_list,empanelled_hospital_id){

  stopifnot("hospital_id_field doesn't exist in the claims_file"=hospital_id_field %in% colnames(claims_file))

  stopifnot("hospital empanellment file doesn't have the required columns"= c(empanelled_hospital_id) %in% colnames(empanelled_hospitals_list))

  stopifnot("hospital_id_field is not in correct format"=is.character(claims_file[[hospital_id_field]]))

  hospital_id_field_1 = sym(hospital_id_field)
  empanelled_hospital_id_1 = sym(empanelled_hospital_id)

  empanelled_hospitals_list <-
    empanelled_hospitals_list %>%
    mutate(hospital_empanelled_flag = 0) %>%
    rename(!!hospital_id_field_1 := !!empanelled_hospital_id_1)


  claims_file %>%
    left_join(empanelled_hospitals_list, by = hospital_id_field) %>%
    mutate(hospital_empanelled_flag = ifelse(is.na(hospital_empanelled_flag), 1 , hospital_empanelled_flag))


}


#' Hospital distance-based fraud trigger
#' @description It is reasonable to expect that the policyholder gets treated in the nearest hospital. If the distance between the residence location and the hospital location is more than a defined threshold, this function will raise a flag
#' @param claims_file data set containing claim-level information
#' @param residence_location_field column in the claims_file with the location of the policy holder
#' @param hospital_location_field column in the claims_file with the location of the hospital
#' @param hospital_distance_file file with reference table of distances between places in the data
#' @param residence_location_map column in the hospital_distance_file  with the list of all possible residence locations
#' @param hospital_location_map column in the hospital_distance_file with all the possible hospital locations
#' @param hospital_distance_field column in the hospital_distance_file that contains the distance between the hospital location and the residence location
#' @param distance_threshold  distance level beyond which claim is flagged for fraud
#'
#' @importFrom rlang sym
#' @importFrom stats setNames
#' @import dplyr lubridate
#'
#' @return input claims_file with hospital_distance_flag column
#' @export
#'
#' @examples
#'
#' library(dplyr)
#'
#' data(claims_data_sample)
#' data(location_distance)
#'
#' hospital_distance_trigger_map(
#' claims_file = claims_data_sample,
#' residence_location_field = "residence_location",
#' hospital_location_field = "hospital_location",
#' hospital_distance_file = location_distance,
#' residence_location_map = "location_1",
#' hospital_location_map = "location_2",
#' hospital_distance_field = "distance",
#' distance_threshold = 100) %>%
#' select(c("residence_location", "hospital_location", "hospital_distance_flag"))
#'
hospital_distance_trigger_map <- function(claims_file, residence_location_field, hospital_location_field, hospital_distance_file, residence_location_map, hospital_location_map,hospital_distance_field, distance_threshold){

  stopifnot("residence_location_field doesn't exist in the claims_file"= residence_location_field %in% colnames(claims_file))
  stopifnot("hospital_location_field doesn't exist in the claims_file"= hospital_location_field %in% colnames(claims_file))
  stopifnot("residence_location_map doesn't exist in the hospital_distance_file"= residence_location_map %in% colnames(hospital_distance_file))
  stopifnot("hospital_location_map doesn't exist in the hospital_distance_file"= hospital_location_map %in% colnames(hospital_distance_file))


  stopifnot("distance_threshold is not in correct format"=is.numeric(distance_threshold))

  hospital_distance <- sym(hospital_distance_field)

  claims_file %>%
    left_join( hospital_distance_file, by = setNames(c(residence_location_map, hospital_location_map),c(residence_location_field, hospital_location_field))) %>%
    mutate(hospital_distance_flag = ifelse(!!hospital_distance > distance_threshold,1,0))

}

