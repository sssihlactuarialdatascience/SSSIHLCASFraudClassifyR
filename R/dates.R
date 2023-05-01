#' Close proximity-based fraud trigger
#' @description a claim is in close proximity if the treatment start date is very close to the policy commencement date. This function  will raise a flag for such claims.
#'
#' @param claims_file data set containing claim-level information
#' @param treatment_start_date column in the claims_file that contains the starting date of the treatment
#' @param policy_commencement_date column in the claims_file that contains the commencement date of the policy
#' @param triggers_file file containing the number of days for each procedure within which we can call it as a close proximity claim
#' @param procedure_code column in the triggers_file that contains the code/identifier for the medical procedure
#' @param close_prox_days column in the triggers_file that contains the number of days for each procedure within which can call it as a close proximity claim
#'
#' @return input claims_file with close_prox_flag and claim_duration_days columns
#'
#' @importFrom rlang sym
#' @import dplyr lubridate
#'
#' @export
#'
#' @examples
#'
#' library(dplyr)
#'
#' data(claims_data_sample)
#' data(trigger_data)
#'
#'close_prox_trigger_map(
#'claims_file = claims_data_sample,
#'treatment_start_date = "treatment_start_date",
#'policy_commencement_date = "policy_commencement_date",
#'triggers_file = trigger_data,
#'procedure_code = "primary_procedure_code",
#'close_prox_days = "close_prox_days") %>%
#'select(c("treatment_start_date", "policy_commencement_date",
#'"claim_duration_days", "close_prox_flag"))
#'
close_prox_trigger_map <- function(claims_file, treatment_start_date, policy_commencement_date, triggers_file, procedure_code, close_prox_days){

  stopifnot("treatment_start_date doesn't exist in the claims_file"=treatment_start_date %in% colnames(claims_file))
  stopifnot("policy_commencement_date doesn't exist in the claims_file"=policy_commencement_date %in% colnames(claims_file))
  stopifnot("procedure_code doesn't exist in the claims_file"=procedure_code %in% colnames(claims_file))

  stopifnot("triggers file doesn't have the required columns"= c(procedure_code, close_prox_days) %in% colnames(triggers_file))

  stopifnot("treatment_start_date is not in date format"=is.Date(claims_file[[treatment_start_date]]))
  stopifnot("policy_commencement_date is not in date format"=is.Date(claims_file[[policy_commencement_date]]))
  stopifnot("close_prox_days is not in numeric format"=is.numeric(triggers_file[[close_prox_days]]))

  treatment_start_date <- sym(treatment_start_date)
  policy_commencement_date <- sym(policy_commencement_date)
  close_prox_days <- sym(close_prox_days)

  claims_file %>%
    left_join(triggers_file, by = procedure_code) %>%
    mutate(claim_duration_days = !!treatment_start_date - !!policy_commencement_date,
           close_prox_flag = case_when(claim_duration_days < !!close_prox_days ~ 1,
                                       TRUE ~ 0))

}


#' treatment date validity-based fraud trigger
#' @description For each policy there is a commencement and termination date within which the claim event(treatment) should occur. If the treatment date is outside these dates it will raise a flag
#' @param claims_file data set containing claim-level information
#' @param treatment_start_date_field column in the claims_file that contains the starting date of the treatment
#' @param policy_commencement_date_field column in the claims_file that contains the commencement date of the policy
#' @param policy_termination_date_field column in the claims_file that contains the termination date of the policy
#'
#' @importFrom rlang sym
#' @import dplyr lubridate
#'
#' @return input claims_file with treatment_date_validity_flag column
#' @export
#'
#' @examples
#'
#' library(dplyr)
#'
#' data(claims_data_sample)
#'
#' treatment_date_validity_trigger_map(
#' claims_file = claims_data_sample,
#' treatment_start_date_field = "treatment_start_date",
#' policy_commencement_date_field = "policy_commencement_date",
#' policy_termination_date_field = "policy_termination_date") %>%
#' select(c("treatment_start_date", "policy_commencement_date", "treatment_date_validity_flag"))
#'
treatment_date_validity_trigger_map <- function(claims_file, treatment_start_date_field, policy_commencement_date_field, policy_termination_date_field){

  stopifnot("treatment_start_date_field doesn't exist in the claims_file"=treatment_start_date_field %in% colnames(claims_file))

  stopifnot("policy_commencement_date_field doesn't exist in the claims_file"=policy_commencement_date_field %in% colnames(claims_file))

  stopifnot("policy_termination_date_field doesn't exist in the claims_file"=policy_termination_date_field %in% colnames(claims_file))

  stopifnot("treatment_start_date_field is not in correct format"=is.Date(claims_file[[treatment_start_date_field]]))
  stopifnot("policy_commencement_date_field is not in correct format"=is.Date(claims_file[[policy_commencement_date_field]]))
  stopifnot("policy_termination_date_field is not in correct format"=is.Date(claims_file[[policy_termination_date_field]]))


  treatment_start_date_field = sym(treatment_start_date_field)
  policy_commencement_date_field = sym(policy_commencement_date_field)
  policy_termination_date_field = sym(policy_termination_date_field)

  claims_file %>%
    mutate(treatment_date_validity_flag = case_when((as.numeric(!!treatment_start_date_field - !!policy_commencement_date_field) < 0) | (as.numeric(!!policy_termination_date_field- !!treatment_start_date_field) < 0) ~ 1,
                                                    TRUE ~ 0))

}


#' claim reporting delay based fraud trigger
#' @description For each policy there is a treatment start date and treatment end date. The claim should be reported within the permissible days after treatment end date(discharge). If the claim reported date is outside the permissible limit it will raise a flag
#' @param claims_file data set containing claim-level information
#' @param treatment_end_date_field column in the claims_file that contains the ending date of the treatment
#' @param claim_reported_date_field column in the claims_file that contains the claim reported date of the policy
#' @param claim_delay_days a user input that sets a permissible time period within which a claim should be reported
#'
#' @importFrom rlang sym
#' @import dplyr lubridate
#'
#' @return input claims_file with claims_delay_flag column
#' @export
#'
#' @examples
#'
#' library(dplyr)
#'
#' data(claims_data_sample)
#'
#' claim_reported_delay_trigger_map(
#' claims_file = claims_data_sample,
#' treatment_end_date_field = "treatment_end_date",
#' claim_reported_date_field = "claim_reported_date",
#' claim_delay_days = 15) %>%
#' select(c("treatment_end_date", "claim_reported_date", "claim_reported_delay_flag"))
#'
claim_reported_delay_trigger_map <- function(claims_file, treatment_end_date_field, claim_reported_date_field,claim_delay_days){

  stopifnot("treatment_end_date_field doesn't exist in the claims_file"=treatment_end_date_field %in% colnames(claims_file))

  stopifnot("claim_reported_date_field doesn't exist in the claims_file"=claim_reported_date_field %in% colnames(claims_file))


  stopifnot("treatment_end_date_field is not in correct format"=is.Date(claims_file[[treatment_end_date_field]]))
  stopifnot("claim_reported_date_field is not in correct format"=is.Date(claims_file[[claim_reported_date_field]]))
  stopifnot("claim_delay_days is not in correct format"=is.numeric(claim_delay_days))


  treatment_end_date_field = sym(treatment_end_date_field)
  claim_reported_date_field = sym(claim_reported_date_field)

  claims_file %>%
    mutate(claim_reported_delay_flag = case_when((as.numeric(!!claim_reported_date_field - !!treatment_end_date_field  ) > claim_delay_days)  ~ 1,
                                                 TRUE ~ 0))

}
