#' Get Data from AppsFlyer Master API
#' @description Get Data from AppsFlyer Master API <https://support.appsflyer.com/hc/en-us/articles/213223166-Master-API-user-acquisition-metrics-via-API>
#' @importFrom httr content GET
#' @importFrom stringr str_c
#'
#' @param api_token AppsFlyer API token
#' @param app_id AppsFlyer App ID
#' @param from Lower bound of the LTV attribution date range
#' @param to Higher bound of the LTV attribution date range
#' @param groupings Groupings
#' @param kpis List of KPIs
#' @param filters Filters
#' @param currency KPI revenue currency
#' @param timezone Time zone of data ranges
#' @param format CSV or JSON formatting
#'
#' @export

af_master_data <- function(api_token = api_token, app_id = app_id, from, to, groupings, kpis,
                           filters = NULL, currency = NULL, timezone = NULL, format = NULL) {
  master_data <- GET("https://hq.appsflyer.com/export/master_report/v4",
                     query = list(api_token = api_token,
                                  app_id = app_id,
                                  from = from,
                                  to = to,
                                  groupings = str_c(groupings, collapse = ","),
                                  kpis = str_c(kpis, collapse = ","),
                                  filters = filters,
                                  currency = currency,
                                  timezone = timezone,
                                  format = format))

  if (master_data$status_code != 200) {
    stop(paste0("Error code ", master_data$status_code, ": ", content(master_data)))
  }

  content(master_data, as = "parsed", type = "text/csv", encoding = "UTF-8")
}

