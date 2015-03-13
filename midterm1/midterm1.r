# Please load in the dataset included in the midterm1 directory. It will be
# required to perform the following tasks. The dataset includes data for houses
# in the city of Berkeley.
load("SFHousing-2.rda")

# calculate the mean and median bsqft of houses in Berkeley. Store these as the
# variables <mean.bsqft> and <med.bsqft> respectively.

# mean.bsft <- your code here
berkeley <- housing[housing$city == "Berkeley",]
mean.bsqft <- mean(berkeley$bsqft, na.rm = TRUE)
  
# med.bsqft <- your code here
med.bsqft <- median(berkeley$bsqft, na.rm = TRUE)

# For each house in the dataset, calculate the squared difference between its
# bsqft and the median bsqft of houses in Berkeley. Store this as the variable
# <bsqft.diffs>. Note that this should be a numeric vector with length equal to
# the number of observations in the dataset

# bsqft.diffs <- your code here
bsqft.diffs <- (berkeley$bsqft-med.bsqft)^2

# Please create two new data frames with the following two subsets
# and store them with the indicated names:
# 1) houses whose bsqft is strictly greater than <mean.bsqft>:  <bsft.greater>
# 2) houses whose bsqft is less than or equal to  <mean.bsqft>: <bsqft.less>

# bsqft.greater <- your code here
bsqft.greater <- berkeley[berkeley$bsqft > mean.bsqft,]
# bsqft.less <- your code here
bsqft.less <- berkeley[berkeley$bsqft <= mean.bsqft,]


# For each of your subsets, create a vector giving the price of each house. Name
# these variables <rooms.greater.price> and <rooms.less.price>.

# rooms.greater.price <- your code here
rooms.greater.price <- bsqft.greater$price
# rooms.less.price <- your code here
rooms.less.price <- bsqft.less$price

# Please implement the function priceByRooms. Your function should take the
# following arguments:
#
# <room.range>: a numeric vector of length 2 whose first and second observations
#   give the minimum and maximum number of rooms to consider
# <br>: a numeric vector giving the number of bedrooms for each observation
# <prices>: a numeric vector giving the price of each observation associated
#   with <br>
#
# Your function should return the average of <prices> for all observations with
# <br> in the range (inclusive) specified by <room.range>

priceByRooms <- function(room.range, br, prices) {
    houses <- data.frame(br,prices)
    houses_short <- houses[houses$br >= min(room.range) & houses$br <= max(room.range),]
    mean(houses_short$prices)

}

# Please create a plot of house price (y-axis) against br (x-axis). Your plot
# should include the following features:
# 1) a title "Housing price vs Number of Rooms"
# 2) axis labels: "price" and "number of rooms"
# 3) plotting character set to 20
plot(berkeley$br, berkeley$price, xlab = "price", ylab = "number of rooms", main= "Housing price vs Number of Rooms", pch= 20)
