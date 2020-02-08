## Remember that for my datasets, the first 6 columns are just ID fields, so the habitat variables start at column 7. If that changes, remember to go and change the +6 modifier in the "Predicted values" section.

modpred <- function(data, model) {

## Coefficients
coef <- summary(model)$coefficients

## Variable names
p.vars <- paste("p.", tail(rownames(coef), -1), sep = "")

## Create a vector to contain predicted values
p.list <- vector(mode = "list", length = length(p.vars))
names(p.list) <- p.vars

## Replicate each element in list
for (i in 1:length(p.list)) {
  p.list[[i]] <- list()
}
rm(i)

## Predicted values
for (i in 1:length(p.list)) {
  p.list[[i]] <- with(data, exp(coef[i+1]*data[, i+6]) / (1+exp(coef[i+1]*data[, i+6])))
}
rm(i)

## Cleanup
rm(coef, p.vars)

## Return
return(p.list)
}