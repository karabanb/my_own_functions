
library(tidyverse)
load("./data/raw_data.Rdata")

x <- raw_data[raw_data$RodzajScore=="PierwszaZewn",]



data0 <- "Data0"
event_date <- "evv_EventDate"
cols <- c("evt_Code", "evs_Code", "evi_Code", "evr_Code")
id <- "cli_ID"
intervals <- c(1, 7, 14, 30)


tmp <- list()
tmp2 <- list()
comb_cols <- list()

## generating all possible combinations of event's names

for (i in 1:length(cols)) {
  if (i == length(cols))  ## zakombinaować coś z while
    break
  tmp[[i]] <- t(combn(cols, i+1))
  comb_cols[[i]] <- t(combn(cols, i+1))
    for (j in 1 : dim(tmp[[i]])[1]){
      tmp[[i]][j] <- paste(tmp[[i]][j,], collapse = "_")
    }
  tmp[[i]] <- tmp[[i]][,1]
}

tmp <- unlist(unlist(tmp))

## generating all possible combinations of columns

k <-1

for (i in 1:length(comb_cols)){
  for (j in 1:dim(comb_cols[[i]])[1]){
     tmp2[[k]] <- comb_cols[[i]][j,]
     k <- k+1
  }
}

## merging and renaming combinations of columns

for (i in 1:length(tmp)){
  x <- tidyr::unite(x, tmp[i], tmp2[[i]], remove = FALSE)
  names(x)[names(x) == 'tmp[i]'] <- tmp[i]
}

## roundind date

x$Data0_floor <- lubridate::floor_date(x[, data0], unit = "day")

## creating interval's start date for each interval 

interval_lenght <- c()

for (i in seq_along(intervals)) {
  interval_lenght[i] <- paste0("D", intervals[i])
  interval_start <- x$Data0_floor - lubridate::days(intervals[i])
  x <- cbind(x, interval_start)
  colnames(x)[colnames(x)=="interval_start"] <- interval_lenght[i]
}

tmp <- c(cols, tmp)

for (i in tmp) {
  for (j in interval_lenght)
    count_interval <- 'sum(x[,event_date] >  x[, j])'
    count_name <- paste(i, j, sep = "_")
 z <- head(x) %>% 
   group_by_(id, i) %>%  
   summarise_(
               .dots = setNames(count_interval, count_interval) # tu sie sypie
             )
  print(z)
}


tmp3 <- expand.grid(tmp, interval_lenght)
tmp4 <- paste(tmp3$Var1, tmp3$Var2, sep = "_")

count_interval <- paste0('sum(', event_date > interval_lenght[j], ')')

sum(x[,event_date] >  x[, "D1"])
