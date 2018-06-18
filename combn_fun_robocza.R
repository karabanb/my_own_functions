
library(tidyverse)
load("./data/raw_data.Rdata")

x <- raw_data[raw_data$RodzajScore=="PierwszaZewn",]


data0 <- "Data0"
event_date <- "evv_EventDate"
cols <- c("evt_Code", "evs_Code", "evi_Code", "evr_Code")
id <- "cli_ID"
intervals <- c(1, 7,180, 360)


tmp <- list()
comb_cols <- list()

#### generating all possible combinations of event's names ####

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

#### generating all possible combinations of columns ####

tmp2 <- list()

k <-1

for (i in 1:length(comb_cols)){
  for (j in 1:dim(comb_cols[[i]])[1]){
     tmp2[[k]] <- comb_cols[[i]][j,]
     k <- k+1
  }
}

#### merging and renaming combinations of columns ####

for (i in 1:length(tmp)){
  x <- tidyr::unite(x, tmp[i], tmp2[[i]], remove = FALSE)
  names(x)[names(x) == 'tmp[i]'] <- tmp[i]
}

#### roundind date ####

x$Data0_floor <- lubridate::floor_date(x[, data0], unit = "day")

#### creating interval's start date for each interval ####

interval_lenght <- c()

for (i in seq_along(intervals)) {
  interval_lenght[i] <- paste0("D", intervals[i])
  interval_start <- x$Data0_floor - lubridate::days(intervals[i])
  x <- cbind(x, interval_start)
  colnames(x)[colnames(x)=="interval_start"] <- interval_lenght[i]
}

### creating function for keeping columns with non unique values ####

keep_unique <- function(x){
  tmp <- sapply(x, function(x){length(unique(x))>1})
  names(tmp[tmp==TRUE])
}


### counting each combination of events for each interval for each id ####

tmp <- c(cols, tmp)
z <- data.frame()
df <- data.frame()
all <- list()

for (i in tmp) {
  count_name <- "N"
  count_interval <- 'n()'
  df<- x %>% 
    group_by_(id, i) %>%
    summarise_(.dots = setNames(count_interval, count_name))
      for (j in interval_lenght){
          count_interval <- 'sum(event_date > j)'
          renaming <- 'paste(.[, i], j, sep = "_")'
          z <- x %>%
                mutate_(.dots = set_names(renaming, i)) %>%
                group_by_(id, i) %>%
                summarise_(.dots = setNames(count_interval, count_name))
          df <- union(df, z)
      }
 # unq <- keep_unique(df)
  all[[i]] <- df#[,unq]
}

<<<<<<< HEAD
<<<<<<< HEAD
k <- 1

=======
>>>>>>> 650b6051dbb49f74c66866c43d2d895f72574351
=======
>>>>>>> 650b6051dbb49f74c66866c43d2d895f72574351
for (i in all){
  tmp <- as.data.frame(i)
  tmp <- spread(tmp, key = names(tmp)[2], value = names(tmp)[3])
  tmp[is.na(tmp)] <- 0
<<<<<<< HEAD
<<<<<<< HEAD
  all[[k]] <- tmp
  k <- k+1
=======
>>>>>>> 650b6051dbb49f74c66866c43d2d895f72574351
=======
>>>>>>> 650b6051dbb49f74c66866c43d2d895f72574351
}


