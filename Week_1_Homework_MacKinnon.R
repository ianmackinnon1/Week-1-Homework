###### Question 6 ######

## Part B
x = seq(153,189)
sum(dpois(x,171))

qnorm(0.8430889) #gives z-value for part B


## Part C
std = sqrt(((171-18)^2)/(171))
print(std)

me = qnorm(0.975)*std
range = c(171-me,171+me)
print(range)


###### Question 11 ######

## Part A

shots_low = 20 #(1 minute with 20 shots per minute)
shots_high = 15 #(5 minutes with 3 shots per minute)

# Probability that a plane gets hit while flying low
p_low = 0.90*0.8*0.05

# Probability that a plane gets hit while flying high
p_high = 0.75*0.95*0.70

# Probability that at least one plane gets hit while flying low
x = seq(1,shots_low)
sum(dbinom(x,shots_low,p_low))

# Probability that at least one plane gets hit while flying high
y = seq(1,shots_high)
sum(dbinom(y,shots_high,p_high))



## Part B

# Chance of success if low
planes = seq(1,16)
expected_low = sum(1-dbinom(planes,shots_low,p_low))
planes_low = seq(1,15) #created based on line above expecting 15.4 planes to make it through
sum(dbinom(planes_low,16,0.7))

# Chance of success if high
expected_high = sum(1-dbinom(planes,shots_high,p_high))
planes_high = seq(1,14) #created based on line above expecting 15.4 planes to make it through
sum(dbinom(planes_high,16,0.7))


## Part C

# Chance of success if low
planes_sent = seq(1,20)
for(i in 1:length(planes_sent)){
  expected_low = sum(1-dbinom(planes_sent[i],shots_low,p_low)) #expected number of planes to make it through
  planes_low = round(expected_low) #rounds the expected value of planes to make it tan integer
  success = sum(dbinom(planes_low,16,0.7)) #prob of at least 1 plane hitting target
}
results = data.frame(planes_sent,expected_low,success)
print(results)


# I Could not get loop to run how I wanted. Spent way too long on it. I was trying to make a data frame where I could see when the probability of success surpassd 0.95 and would use the number of planes it took to get there to answer part c. 


## Part D

#For part D, I'd use the same loop from part c (if it had worked) but instead changed my value of "o.7" instead of the number of planes sent


## Part E

p_low = 0.88*0.8*0.05 # Took 0.02 off from 0.9
expected_low = sum(1-dbinom(planes,shots_low,p_low))
planes_low = seq(1,15) #created based on line above expecting 15.4 planes to make it through
sum(dbinom(planes_low,16,0.68)) # Took 0.02 off from 0.7

#I subtracted 0.02 from p_detect and the chance of the bombers succeeding and this value is higher than the original one, so the bombers gain the advantage