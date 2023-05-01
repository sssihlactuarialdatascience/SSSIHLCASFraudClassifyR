#' claim amount-based fraud trigger
#' @description For each procedure there is a specific amount that is generally agreed between the insurer and the medical service provider. This is especially the case for cashless claim processing. If the claim amount is higher than this agreed procedure specific amount it will raise a flag.
#'
#' @param claims_file data set containing claim-level information
#' @param claim_paid_field column in the claims_file that contains the claim amount paid by the insurer
#' @param triggers_file file containing the agreed specific amount for each procedure
#' @param procedure_code column in the triggers_file that contains the code/identifier for the medical procedure
#' @param procedure_amount_field column in the triggers_file that contains the agreed specific amount for each procedure
#'
#' @return input claims_file with claim_amount_ratio and claim_amount_flag columns
#'
#' @importFrom rlang sym
#' @import dplyr
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
#'claim_amount_trigger_map(
#'claims_file = claims_data_sample,
#'claim_paid_field = "approved_allowed_amount",
#'triggers_file = trigger_data,
#'procedure_code = "primary_procedure_code",
#'procedure_amount_field = "package_amount_trigger") %>%
#'select(c("approved_allowed_amount", "claim_amount_ratio", "claim_amount_flag"))
#'
claim_amount_trigger_map <- function(claims_file, claim_paid_field, triggers_file, procedure_code, procedure_amount_field){

  stopifnot("claim_paid_field doesn't exist in the claims_file"=claim_paid_field %in% colnames(claims_file))
  stopifnot("procedure_code doesn't exist in the claims_file"=procedure_code %in% colnames(claims_file))

  stopifnot("triggers file doesn't have the required columns"= c(procedure_code, procedure_amount_field) %in% colnames(triggers_file))

  stopifnot("claim_paid_field is not in numeric format"=is.numeric(claims_file[[claim_paid_field]]))
  stopifnot("procedure_amount_field is not in numeric format"=is.numeric(triggers_file[[procedure_amount_field]]))


  procedure_amount_field <- sym(procedure_amount_field)
  claim_paid_field <- sym(claim_paid_field)

  claims_file %>%
    left_join(triggers_file, by = procedure_code) %>%
    mutate(claim_amount_ratio = !!claim_paid_field / !!procedure_amount_field,
           claim_amount_flag = case_when(claim_amount_ratio != 1 ~ 1,
                                         TRUE ~ 0))

}



#' Claim count-based fraud trigger
#' @description We don't expect the policyholder to get a given medical treatment more than few times a year, depending on the nature of the treatment. If the policy holder has taken a given treatment unreasonably high number of times, it needs to be investigated for fraud. This the function identifies all such policyholders.
#' @param claim_count file that contains the total number of times a policy holder claimed for a given procedure in a given time period
#' @param claim_count_pa column that contains the total number of claims per year by a policy holder
#' @param claim_count_trigger_file file containing maximum number of  times a policy holder can claim for each procedure
#' @param procedure_code_field column in the claim_count_trigger_file that contains the code/identifier for each medical procedure
#' @param claim_count_trigger column in the claim_count_trigger_file that indicates the number of times a procedure can be claimed by a policyholder before it can be considered as potential fraud
#'
#' @importFrom rlang sym
#' @import dplyr
#'
#' @return input claims_file with claim_count_flag column
#' @export
#'
#' @examples
#'
#' library(dplyr)
#'
#' data(claims_data_sample)
#' data(trigger_data)
#'
#'claim_count_trigger_map(
#'claim_count = claims_data_sample,
#'claim_count_pa = "claim_count_pa",
#'claim_count_trigger_file = trigger_data,
#'procedure_code_field = "primary_procedure_code",
#'claim_count_trigger = "claim_count_trigger") %>%
#'group_by(claim_count_pa)%>%
#'summarise(claim_count_flag = mean(claim_count_flag))
#'
claim_count_trigger_map <- function(claim_count,claim_count_pa,claim_count_trigger_file,procedure_code_field, claim_count_trigger ){

  stopifnot("claim_count_pa doesn't exist in the claim_count file"= claim_count_pa %in% colnames(claim_count))


  stopifnot("claim_count_pa is not in correct format"=is.numeric(claim_count[[claim_count_pa]]))
  stopifnot("claim_count_trigger is not in correct format"=is.numeric(claim_count_trigger_file[[claim_count_trigger]]))
  claim_count_pa = sym(claim_count_pa)
  claim_count_trigger = sym(claim_count_trigger)



  claim_count %>%
    left_join(claim_count_trigger_file, by = procedure_code_field) %>%
    mutate(claim_count_flag = case_when(claim_count_pa > claim_count_trigger ~ 1,
                                        TRUE ~ 0))

}
