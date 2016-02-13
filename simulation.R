options(stringsAsFactors = F)

#Incomes and populations, deprecated
n <- 220000
incomes <- rnorm(n, mean=63603, sd=1700)
scaled_incomes <- incomes / max(incomes)
hist(incomes)
hist(scaled_incomes)
low_cutoff <- 20000
high_cutoff <- 100000
pay_cutoff <- 0.5

#130K (MoPac)
#58K (71) 
#220K (I35) 
#60K (183)
optimal_volume <- 8000
max_toll <- 10
max_speed <- 75
standstill_speed <- 5

total_volume <- 60000
morning_rush <- rnorm(total_volume/5 * 2, mean=9, sd=1.5)
evening_rush <- rnorm(total_volume/5 * 2, mean=17, sd=1.5)
lunch_rush <- rnorm(total_volume/5 * 1, mean=13, sd=1.2)


#Function of current volume vs. time 
getVolume <- function(start_time){
  end_time <- start_time + .25
  return(total_volume/130 + 
         length(morning_rush[morning_rush >= start_time & morning_rush <= end_time])+
         length(evening_rush[evening_rush >= start_time & evening_rush <= end_time])+
         length(lunch_rush[lunch_rush >= start_time & lunch_rush <= end_time]))
}

volx <- sapply(seq(0, 23.75, .25), getVolume)
volx <- volx * (total_volume / sum(volx))

#Function of toll vs. current volume 
newToll <- function(volume){
  return(.75*max_toll*volume/max(volx))
}

#Function of resulting volume vs. toll
resultVolume <- function(toll, volume) {
  return (1.0 * volume*(max_toll - toll)/(max_toll + 1 ))
}

#revenue function 
revenue <- function (toll, volume){
  return(resultVolume(toll, volume)* toll)
}

#Function of speed vs. resulting volume 
getSpeed <- function(volume) {
  vol_diff <- 2*optimal_volume - volume
  vol_ratio <- vol_diff / (2*optimal_volume)
  return ((max_speed - standstill_speed)*vol_ratio)
}



times <- c()
init_volumes <- c()
final_volumes <- c()
speeds <- c()
tolls <- c()
revs <- c()

for (i in 1:95) {
  print(i)
  time <- i/4
  volume <- getVolume(time) * (total_volume / (total_volume + 95*total_volume/130))
  toll <- newToll(volume)
  res_volume <- resultVolume(toll, volume)
  rev <- revenue(toll, volume) 
  #speed <- getSpeed(volume)
  
  times <- c(times, time)
  init_volumes <- c(init_volumes, volume)
  final_volumes <- c(final_volumes, res_volume)
  #speeds <- c(speeds, speed)
  tolls <- c(tolls, toll)
  revs <- c(revs, rev)
}

sum(revs)
sum(final_volumes)

df <- data.frame(times, init_volumes, final_volumes, tolls, revs)
write.csv(df, "C:\\Users\\visha\\OneDrive\\Documents\\Spring 2016\\Other\\Traffic Case Comp\\183.csv")
plot(final_volumes)