
keep_unique <- function(x){
  tmp <- sapply(x, function(x){length(unique(x))>1})
  names(tmp[tmp==TRUE])
}