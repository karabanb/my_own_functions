
df <- data.frame(id = unique(x[,id]))
names(df)[names(df)=="id"] <- id
funs <- c("max", "min")

for (i in tmp){
  for (j in funs){
  renaming <- 'paste(.[, i], j, sep = "_")'
  z <- x %>%
    mutate_(.dots = setNames(renaming, i)) %>%
    group_by_(id, i) %>%
    summarise_at(vars(event_date, date0),funs(min)) %>%
    rename_(.dots = setNames(i, "code")) %>%
    mutate_(.dots = setNames(paste0("as.numeric(difftime(",date0,",",event_date,",units = \"days\"))"), "N")) %>%
    select_(id, "code", "N") %>%
    spread(., code, N)
  df <- left_join(df, z, by = id)
  }
}
