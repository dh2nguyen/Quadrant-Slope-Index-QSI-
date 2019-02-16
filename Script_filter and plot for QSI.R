library(dplyr)

### This script requires two files. The first file contains the data that you want
# to sort into grid units. This first file is loaded as the object "the.data". 
   # This file should contain a column of x coordinates that is caled "X".
   # It should also contain a column of y coordinates that is called "Y".
# The second file contains the upper and lower bounds for each grid unit in the x 
# and y dimensions (i.e. the widths and heights). This second file is loaded as the 
# object named "the.param". 
   # This file should contain four columns. The column names should be "x.lower",
   # "x.upper", "y.lower", and "y.upper". These columns represent the upper and lower
   # bounds of each grid unit, in terms of x coordinates and y coordinates. 

# NOTE: You have to change the file names for the above two files TWICE EACH 
   # in the below code. Once for the positive slopes, once for the negative slopes.
# There are 6 things that need to be updated when you apply this script to 
   # a new data file (i.e. the data) or grid unit system (i.e. 11x11, 12x12, 13x13).
      # Use the search function to find the term "UpdateMe" to find each place
      # that needs updating.



### load data that you want to filter into grid units
# [UpdateMe - data file's name, 1st of 2 places]
the.data = read.csv("luminalB2_marked_data_edit.csv") 
#View(the.data)

### change the name of three columns of interest X, Y, and FeretAngle 
# should be xcoord, ycoord, and angle

colnames(the.data)[colnames(the.data)=="X"] <- "xcoord"
colnames(the.data)[colnames(the.data)=="Y"] <- "ycoord"
colnames(the.data)[colnames(the.data)=="FeretAngle"] <- "angle"

#View(the.data)

# angles between 0 and 90 (including 90) are "positive"
wanted = filter(the.data, angle > 0, angle <= 90)
slope = select(wanted, xcoord, ycoord, angle)
#View(slope)

# load the file that defines the widths and heights coordinates of each grid unit
# [UpdateMe - file name of grid parameters, 1st of 2 places]
the.param = read.csv("Grid Params 144 squares_12x12 2250x1439.csv")
#View(the.param)

# create a function called filtastic, which takes in four inputs
filtastic = function (n1, n2, n3, n4) {
  filter(slope, xcoord >= n1 , xcoord <= n2 , ycoord >= n3 , ycoord <= n4)
}

# apply the filtastic function from above to the four columns in the.param
megaman = mapply(filtastic, the.param$x.lower, the.param$x.upper, 
                 the.param$y.lower, the.param$y.upper)

# turn the matrix resulting from the mapply function into a dataframe
df = as.data.frame(megaman)
#View(df)

# transpose the dataframe from above so columns become rows
dftrans = as.data.frame(t(df))
#View(dftrans)

# isolate the column called angle
dftrans.angle = dftrans$angle

# count the number of items in each row of the column called angle
the.count.pos = lengths(dftrans.angle)
#View(the.count.pos)


############################## 
### REPEAT THE CODE FOR THE NEGATIVE SLOPES

### load scatter data
# [UpdateMe - data file's name, 2nd of 2 places]
the.data2 = read.csv("luminalB2_marked_data_edit.csv") 
#View(the.data2)

### change the name of three columns of interest X, Y, and FeretAngle 
# should be xcoord, ycoord, and angle

colnames(the.data2)[colnames(the.data2)=="X"] <- "xcoord"
colnames(the.data2)[colnames(the.data2)=="Y"] <- "ycoord"
colnames(the.data2)[colnames(the.data2)=="FeretAngle"] <- "angle"

#View(the.data2)

# angles between 90 and 180 are "negative"
wanted2 = filter(the.data2, angle > 90, angle < 180)
slope = select(wanted2, xcoord, ycoord, angle)
#View(slope)

# [UpdateMe - file name of grid parameters 2nd of 2 places]
the.param = read.csv("Grid Params 144 squares_12x12 2250x1439.csv")
#View(the.param)

filtastic = function (n1, n2, n3, n4) {
  filter(slope, xcoord >= n1 , xcoord <= n2 , ycoord >= n3 , ycoord <= n4)
}

megaman2 = mapply(filtastic, the.param$x.lower, the.param$x.upper, 
                 the.param$y.lower, the.param$y.upper)

# turn the matrix resulting from the mapply function into a dataframe
df = as.data.frame(megaman2)
#View(df)

# transpose the dataframe from above so counts become rows
df.t = as.data.frame(t(df))
#View(df.t)

# isolate the column called angle
df.t.angle = df.t$angle

# count the number of items in each row of the column called angle.
# since each item is itselft a list, you need to use the plural lengths() function
the.count.neg = lengths(df.t.angle)
#View(the.count.neg)

############################ 

melting.pot = cbind(the.count.pos, the.count.neg)

colnames(melting.pot)[colnames(melting.pot)=="the.count.neg"] = "neg.slopes"
colnames(melting.pot)[colnames(melting.pot)=="the.count.pos"] = "pos.slopes"
head(melting.pot)

# [UpdateMe - dimensions of scatter plot for ggplot. change for both x and y.]
x.location = rep(1:12, each = 12)
y.location = rep(1:12, times = 12)

oohlala = cbind(melting.pot, x.location, y.location)
oohlala = as.data.frame(oohlala)
head(oohlala)

# create a new column containing number of pos slopes divided by number of neg slopes
# name this new column pos_over_neg
oohlala = mutate(oohlala, pos_over_neg = pos.slopes/neg.slopes)
#View(oohlala)


# save the data in a new csv file
write.csv(oohlala, "luminalB2_11x11_plotme.csv")


############################## 

# Plot the data such that each grid unit is a large square instead of a small dot

library(ggplot2)

ggplot(oohlala, aes(x=x.location, y=y.location, color=pos_over_neg)) + 
  geom_point(size=25, shape=15) + scale_color_gradient(low="red", high="green") + 
  ggtitle("QSI Plot") + xlab(" ") + ylab(" ") + 
  theme(plot.title = element_text(hjust = 0.5))



