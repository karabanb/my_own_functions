
keep_unique <- function(x){
  tmp <- sapply(x, function(x){length(unique(x))>1})
  names(tmp[tmp==TRUE])
}

source("https://github.com/karabanb/my_own_functions/blob/master/keep_unique.R")
