% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/af_master_data.r
\name{af_master_data}
\alias{af_master_data}
\title{Get Data from AppsFlyer Master API}
\usage{
af_master_data(
  api_token = api_token,
  app_id = app_id,
  from,
  to,
  groupings,
  kpis,
  filters = NULL,
  currency = NULL,
  timezone = NULL,
  format = NULL
)
}
\arguments{
\item{api_token}{AppsFlyer API token}

\item{app_id}{AppsFlyer App ID}

\item{from}{Lower bound of the LTV attribution date range}

\item{to}{Higher bound of the LTV attribution date range}

\item{groupings}{Groupings}

\item{kpis}{List of KPIs}

\item{filters}{Filters}

\item{currency}{KPI revenue currency}

\item{timezone}{Time zone of data ranges}

\item{format}{CSV or JSON formatting}
}
\description{
Get Data from AppsFlyer Master API <https://support.appsflyer.com/hc/en-us/articles/213223166-Master-API-user-acquisition-metrics-via-API>
}
