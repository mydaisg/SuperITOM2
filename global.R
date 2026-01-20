library(shiny)
library(shinythemes)
library(DT)
library(RSQLite)
library(DBI)
library(ggplot2)
library(plotly)

db_path <- file.path(getwd(), "DB", "GH_ITOM.db")

db_connect <- function() {
  con <- dbConnect(RSQLite::SQLite(), db_path)
  return(con)
}

db_disconnect <- function(con) {
  dbDisconnect(con)
}

check_database <- function() {
  if (!file.exists(db_path)) {
    stop("数据库文件不存在: ", db_path)
  }
}

check_database()
