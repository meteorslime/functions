sample.aic.comparison <- function(table) {

## Add fields to table
aic.table$delta <- round((aic.table$AICc - min(aic.table$AICc)), digits = 4)
aic.table <- aic.table[order(aic.table$delta), ]

aic.table$var <- substring(rownames(aic.table), 1, 4)
aic.table$scale <- as.numeric(substring(rownames(aic.table), 5, 7))
aic.table$sample <- as.factor(attr(dataset$SampleID, "sample"))

varname <- aic.table$var[1]

## Assign AIC table name
aic.table.name <- paste(spp, ".aic.", varname, ".", sample, sep = "")

assign(aic.table.name, aic.table)

## Save
save(list = paste(aic.table.name), 
    file = paste("./Data/", spp, "-aic-", varname, "-", sample, ".Rdata", sep = ""))

## Write table
write.csv(aic.table, 
    file = paste("./Results/", spp, "-aic-", varname, "-", sample, ".csv", sep = ""))

## Generate keep table
#keep.table <- aic.table[which(aic.table$AIC == min(aic.table$AIC)), ]

## Assign keep table name
#keep.table.name <- paste(spp, ".keep.", varname, ".", sample, sep = "")

#assign(keep.table.name, keep.table)

## Save
#save(list = paste(keep.table.name), 
#    file = paste("./Data/", spp, "-keep-", varname, "-", sample, ".Rdata", sep = ""))

## Cleanup
rm(varname, aic.table.name, keep.table, keep.table.name)

}
