#' Age-based fraud trigger
#' @description The triggers file contains reasonable age ranges for getting a certain medical procedure done. This function creates a flag to identify all the claimants whose age is falls outside this range.
#'              The age should be specified consistently in the claims_file and the triggers_file for this function to work.

#' @param claims_file data set containing claim-level information
#' @param age_column_name column within the claims file that indicates the age of the policy holder
#' @param triggers_file file containing the age ranges for each procedure code
#' @param procedure_code column in the triggers_file that contains the code/identifier for the medical procedure
#' @param lower_age_limit column in the triggers_file that contains the lower age limit for getting the procedure done
#' @param upper_age_limit column in the triggers_file that contains the upper age limit for getting the procedure done
#'
#' @return input claims_file with age_flag column
#'
#' @importFrom rlang sym
#' @import dplyr
#'
#' @export
#'
#' @examples
#'
#' library(dplyr)
#' data(claims_data_sample)
#' data(trigger_data)
#'
#' age_trigger_map(
#' claims_file = claims_data_sample,
#' age_column_name = "patient_age",
#' triggers_file = trigger_data,
#' lower_age_limit = "lower_age_limit",
#' upper_age_limit = "upper_age_limit",
#' procedure_code = "primary_procedure_code") %>%
#' group_by(patient_age) %>%
#' summarise(age_flag = mean(age_flag))
#'

age_trigger_map <-
  function(claims_file, age_column_name, triggers_file, procedure_code, lower_age_limit, upper_age_limit){

    stopifnot("age_column_name doesn't exist in the claims_file"=age_column_name %in% colnames(claims_file))
    stopifnot("procedure_code doesn't exist in the claims_file"=procedure_code %in% colnames(claims_file))

    stopifnot("triggers file doesn't have the required columns"= c(procedure_code, lower_age_limit, upper_age_limit) %in% colnames(triggers_file))

    stopifnot("age_column_name has to be a numeric field"= is.numeric(claims_file[[age_column_name]]))
    stopifnot("lower_age_limit has to be a numeric field"= is.numeric(triggers_file[[lower_age_limit]]))
    stopifnot("upper_age_limit has to be a numeric field"= is.numeric(triggers_file[[upper_age_limit]]))

    age_column_name <- sym(age_column_name)
    lower_age_limit <- sym(lower_age_limit)
    upper_age_limit <- sym(upper_age_limit)

    claims_file %>%
      left_join(triggers_file, by = procedure_code) %>%
      mutate(age_flag = case_when((!!age_column_name < !!lower_age_limit) & (!!age_column_name > !!upper_age_limit) ~ 1,
                                  TRUE ~ 0))
  }


#' Gender-based fraud trigger
#' @description Some procedures are gender specific. This function creates a red flag if a procedure is performed for a claimant for a wrong gender. For example gynacologic procedure for a male claimant will raise a redflag.
#'              The gender should be specified consistently in the claims_file and the triggers_file for this function to work.
#' @param claims_file   data set containing claim-level information
#' @param gender_column_name column in the claims_file that contains the gender of the policy holder
#' @param triggers_file file containing the gender specification for each procedure code
#' @param procedure_code column in the triggers_file that contains the code/identifier for the medical procedure
#' @param gender_trigger column in the triggers_file that contains the specification of gender for each medical procedure
#'
#' @importFrom rlang sym
#' @import dplyr
#'
#' @return input claims_file with gender_flag column
#' @export
#'
#' @examples
#'
#' library(dplyr)
#'
#' data(claims_data_sample)
#' data(trigger_data)
#'
#' gender_trigger_map(
#' claims_file = claims_data_sample,
#' gender_column_name = "gender_code",
#' triggers_file = trigger_data,
#' procedure_code = "primary_procedure_code",
#' gender_trigger = "gender_trigger") %>%
#' group_by(gender_code)%>%
#' summarise(gender_flag = mean(gender_flag))
#'

gender_trigger_map <- function(claims_file, gender_column_name, triggers_file, procedure_code, gender_trigger){

  stopifnot("gender_column_name doesn't exist in the claims_file"=gender_column_name %in% colnames(claims_file))
  stopifnot("procedure_code doesn't exist in the claims_file"=procedure_code %in% colnames(claims_file))

  stopifnot("triggers file doesn't have the required columns"= c(procedure_code, gender_trigger) %in% colnames(triggers_file))

  stopifnot("gender_column_name must be Male or Female"= unique(claims_file[[gender_column_name]]) %in% c("Male", "Female"))

  gender_trigger <- sym(gender_trigger)
  gender_column_name <- sym(gender_column_name)

  claims_file %>%
    left_join(triggers_file, by = procedure_code) %>%
    mutate(gender_flag = case_when(!!gender_trigger == "Both" ~ 0,
                                   !!gender_trigger != !!gender_column_name ~ 1,
                                   TRUE ~ 0))

}
