### Packages 
library(purrr)
library(tidyverse)

### Read in Data
v2panom_mid <- readRDS("C:/Users/ochoc/AppData/Local/Temp/Temp5634de69-9563-44a5-9a77-a346afd26bb6_vparty_data.zip/vparty_data/v2panom.rds")

### Is it a list?
is.list(v2panom_mid) ### it is.

### Put wdata in an data.frame
data <- v2panom_mid[["v2panom"]][["wdata"]]
data <- as.data.frame(data)

### Replacing data 
colna <- colnames(data)

for (col_name in colna) {
  data[, col_name] <- ifelse(data[, col_name] == 1, 3, data[, col_name])
}

for (col_name in colna) {
  data[, col_name] <- ifelse(data[, col_name] == 2, 1, data[, col_name])
}

for (col_name in colna) {
  data[, col_name] <- ifelse(data[, col_name] == 3, 2, data[, col_name])
}

### Replacing the original element with the new one.
class(v2panom_mid[["v2panom"]][["wdata"]])
class(data)
wdata <- as.matrix(data)
class(wdata)

v2panom_mid[["v2panom"]][["wdata"]] <- wdata


v2panom_end <- readRDS("C:/Users/ochoc/AppData/Local/Temp/Temp5634de69-9563-44a5-9a77-a346afd26bb6_vparty_data.zip/vparty_data/v2panom.rds")

### Put wdata in an data.frame
data1 <- v2panom_end[["v2panom"]][["wdata"]]
data1 <- as.data.frame(data1)

### Replacing data 
colna1 <- colnames(data1)

for (col_name in colna) {
  data1[, col_name] <- ifelse(data1[, col_name] == 1, 5, data1[, col_name])
}

for (col_name in colna1) {
  data1[, col_name] <- ifelse(data1[, col_name] == 2, 1, data1[, col_name])
}

for (col_name in colna1) {
  data1[, col_name] <- ifelse(data1[, col_name] == 3, 2, data1[, col_name])
}

for (col_name in colna1) {
  data1[, col_name] <- ifelse(data1[, col_name] == 4, 3, data1[, col_name])
}

for (col_name in colna1) {
  data1[, col_name] <- ifelse(data1[, col_name] == 5, 4, data1[, col_name])
}

### Replacing the original element with the new one.
class(v2panom_end[["v2panom"]][["wdata"]])
class(data1)
wdata <- as.matrix(data1)
class(wdata)

v2panom_end[["v2panom"]][["wdata"]] <- wdata

View(v2panom_mid[["v2panom"]][["wdata"]])
View(v2panom_end[["v2panom"]][["wdata"]])
