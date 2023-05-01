#Start of Item 1
# Mean and Standard Deviation 
mean <- 200
sd <- sqrt(256)

#For 1A
#The likelihood that the signal will be stronger than 224 V
prob_exceed_224 <- 1 - pnorm(224, mean, sd)
prob_exceed_224
#End of 1A

#For 1B
#The likelihood that it will fall within the range of 186 and 224.
prob_between_186to224 <- pnorm(224, mean, sd) - pnorm(186, mean, sd)
prob_between_186to224
#End of 1B

#For 1C
#The microvoltage at which 25% of signals
microvolt_below_25p <- qnorm(0.25, mean, sd)
microvolt_below_25p
#End of 1C

#For 1D
#The likelihood that the signal will be smaller than 240 V if the signal is bigger than 210 V
prob_less240_greater210 <- (pnorm(240, mean, sd) - pnorm(210, mean, sd)) / (1 - pnorm(210, mean, sd))
prob_less240_greater210
#End of 1D

#For 1E
#The interquartile range
inv_normal_dist <- qnorm(0.75)
x <-(inv_normal_dist * sd) + 200 
x
interquartile_range <- x - microvolt_below_25p
interquartile_range
#End of 1E

#For 1F
#The likelihood that the signal will be smaller than 220 V if the signal is bigger than 210 V
z1 = (210-200) / sd
z2 = (220-200) / sd 

x_lessthan220_greater210 <- pnorm(220, mean, sd) - pnorm(210, mean, sd)
x_lessthan220_greater210

x_greaterthan_210 <- 1 - pnorm(210, mean, sd)
x_lessthan_220 <- 1 - pnorm(220, mean, sd)

ans<- 100*(x_lessthan220_greater210 * x_lessthan_220/x_greaterthan_210)
ans
#End of 1F


#For 1G
#The likelihood that it is actually higher than 220 V given that a received signal is higher than 200 V
px_greaterthan_220 <- 1 - 0.8944
px_greaterthan_220

px_greaterthan_200 <- 1 - 0.5
px_greaterthan_200

px_greaterthan_220_200 <- 1 * (px_greaterthan_220/px_greaterthan_200)
px_greaterthan_220_200

px_greaterthan_220_200 * 100
#End of 1G

#End of Item 1 



#Start of Item 2
upper_x <- 0.975
lower_x <- 0.025
bound_10 <- 0.9
min <- 25
sd <- sqrt(144)

#For 2A
#The lower and upper bound that will have 95% of the downtime of all customer
#Upperbound
upperbound <- qnorm(upper_x, min, sd)
upperbound

#Lowerbound
lowerbound <- qnorm(lower_x,  min, sd)
lowerbound
#End of 2A

#For 2B
#The threshold at which 10% of downtime is included
Bound_dt_10 <- qnorm(bound_10, min, sd)
Bound_dt_10
#End of 2B

#End of Item 2