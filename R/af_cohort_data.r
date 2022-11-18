#' Get Data from AppsFlyer Cohort API
#' @description Get Data from AppsFlyer Cohort API <https://support.appsflyer.com/hc/en-us/articles/360004799057-Cohort-API>
#' @importFrom httr add_headers content POST
#' @importFrom jsonlite toJSON
#' @importFrom retry retry
#' @importFrom stringr str_glue
#'
#' @param app_id AppsFlyer App ID
#' @param cohort_type Cohort attribution (conversion) type
#' @param min_cohort_size Minimum cohort size
#' @param from Lower bound of the LTV attribution date range
#' @param to Higher bound of the LTV attribution date range
#' @param granularity Data granularity
#' @param partial_data Partial data
#' @param filters Filters
#' @param groupings Groupings
#' @param kpis Selected KPI
#' @param preferred_currency KPI revenue currency
#' @param preferred_timezone Time zone of data ranges
#' @param aggregation_type Aggregation type
#' @param per_user Data per user
#' @param signature Signature
#'
#' @export

af_cohort_data <- function(app_id, cohort_type, min_cohort_size = 1, from, to, granularity = "day", partial_data = T, filters = NULL, groupings,
                           kpis, preferred_currency = F, preferred_timezone = F, aggregation_type, per_user = F, signature) {
  json_body <- toJSON(list(cohort_type = cohort_type, min_cohort_size = min_cohort_size,
                           from = from, to = to, granularity = granularity, partial_data = partial_data,
                           filters = filters, groupings = groupings, kpis = list(kpis),
                           preferred_currency = preferred_currency, preferred_timezone = preferred_timezone,
                           aggregation_type = aggregation_type, per_user = per_user),
                      auto_unbox = T)

  retry({

  cohort_data <- POST(str_glue("https://hq1.appsflyer.com/api/cohorts/v1/data/app/{app_id}?format=csv"),
                      body = json_body, encode = "raw",
                      add_headers("Authorization" = signature,
                                  "Content-Type" = "application/json",
                                  "Accept" = "application/json"))

  if (cohort_data$status_code != 200) {
    stop(paste0("Error code ", cohort_data$status_code, ": ", content(cohort_data)))
  }

  },

  when = "lexical error",
  interval  = 60,
  max_tries = 5)

  content(cohort_data, as = "parsed", type = "text/csv", encoding = "UTF-8")
}

