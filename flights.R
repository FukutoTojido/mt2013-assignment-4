#### INCLUDE
install.packages("data.table", dependencies=TRUE)
install.packages("dplyr", dependencies=TRUE)
library(data.table)
library(dplyr)

#### SECTION A: Import File
load(file='flights.rda')

#### SECTION B: Create a new data frame with desired data
newFlights <- flights[c("carrier","origin","dep_time","arr_time","dep_delay","arr_delay")]

#### SECTION C: Find the percentage of NA value per variable and suggest a replacement
colMeans(is.na(newFlights))*100

# The values should be (rounded up to 4 decimal places):
#   - carrier:    0.0000%
#   - origin:     0.0000%
#   - dep_time:   0.5289%
#   - arr_time:   0.6097%
#   - dep_delay:  0.5289%
#   - arr_delay:  0.8028%

# I suggest removing all flights that has NA value since those flights has no meaning in our statistic.
newFlights <- newFlights[rowSums(is.na(newFlights)) == 0,]

#### SECTION D: Calculate the size, mean, standard deviation, minimum, maximum and quartiles of dep_delay for each type of carriers and export it as a table.
table<-as.data.table(aggregate(dep_delay~carrier, data=newFlights, FUN=function(x) c(count=length(x),avg=mean(x),sd=sd(x),min=min(x),max=max(x),quantile(x)[2:4])), keep.rownames=TRUE)

#### SECTION E: Draw Boxplot of dep_delay for each type of carriers.
boxplot(newFlights$dep_delay ~ newFlights$carrier)

#### SECTION F: Remove Outliers using IQR

# Create a function to replace Outliers with NA
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs = c(.25, .75), na.rm = na.rm, ...)
  val <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - val)] <- NA
  y[x > (qnt[2] + val)] <- NA
  y
}

# Then create a data frame in which NA values have been filtered out
grp_dep_delay = newFlights %>% group_by(carrier) %>% mutate(dep_delay = remove_outliers(dep_delay)) %>% ungroup() %>% filter(!is.na(dep_delay))

# Draw the boxplot again
boxplot(grp_dep_delay)