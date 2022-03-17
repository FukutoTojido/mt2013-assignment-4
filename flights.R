load(file='flights.rda')
head(flights)
df <- flights[c("carrier","origin","dep_time","arr_time","dep_delay","arr_delay")]