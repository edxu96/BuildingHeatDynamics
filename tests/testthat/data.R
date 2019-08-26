
get_data <- function(){
  df_x_raw <- read.table(
    "./soenderborg_2day.csv", sep = ",", header = TRUE, as.is = TRUE
  )
  df_x_raw$t <- as.POSIXct(df_x_raw$t, tz="GMT")
  ihouse <- 3  # Set which house to use (there are 3 in the data)
  df_x_raw$P <- df_x_raw[ ,paste0("P",ihouse)]
  saveRDS(df_x_raw, "soenderborg_2day.RDS")
  df_x <- df_x_raw
  return(df_x)
}
