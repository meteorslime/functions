unscale <- function(data) {

for (i in 7:length(data)) {
  data[, i] <- data[, i]*attr(data[, i], 'scaled:scale') + attr(data[, i], 'scaled:center')
}
rm(i)

return(data)
}
