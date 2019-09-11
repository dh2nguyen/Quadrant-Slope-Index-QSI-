# By David H. Nguyen, PhD (www.TSG-Lab.org)
# Updated 9/05/19. Modified lines 20 and 242 to define "positive" angles to include 0 degrees (as in >=0 and <=90). 


### Start of ReadMe Info

                 ###### Quadrant Slope Index (QSI) Algorithm #####

                            #### Instructions ####

# 1. YOU NEED TO modify inputs in six places: Steps 1a, 1b, 1c, 1d, 2a, 2b. 
#     Then, you can just run the whole script and then save the plot image.
#     NOTE: Read the insructions at each of these six places in the below script carefully.
# 2. You will type in the name of your files in Step 2a and 2b. This script 
#     comes with two sample files for Step 2a and 2b: 
#     * "sample_input_positive_slopes.csv"  
#     * "sample_input_negative_slopes.csv"
# 3. For simplicity, I decided to require that you sort and separate the 
#     positive and negatives slopes into two different files before you run this script. 
#       * Slopes >=0 and >=90 degrees are considered positive. 
#       * Slopes between 90 and >= 180 are considered negative.
#     Your two files should contain at least three columns each:  
#       the x coordinate, the y coordinate, and the slope. 
#         * The x coordinate column MUST be called "X". 
#         * The y coordinate column MUST be called "Y". 
#         * The slope column MUST be called "FeretAngle". 
# 4. Each column should contain only numbers.
# 5. Each column should NOT have any missing values. 
# 6. NOTE: This script creates 2-3 .csv files, depending on your answer to Step 1d. 
#     First File. The data that is plotted as a graph will be created in your working directory as a file 
#         called "your qsi data.csv".
#     Second File. An important file containing descriptive statistics about 4 special circumstances in your data, 
#         and what two circumstances to avoid by determining the optimal grid dimensions.
#         The file is called "descriptive stats QSI.csv".         
#     3. This script also produces a .csv file called "parameters of your grid.csv" (Step 1d). 
#         This file contains the tail and head of both the x and y directions of 
#         every square in your plotted grid. 
              

# End of ReadMe Info

                 
##########################
##########################

# Required packages
library(dplyr)
library(ggplot2)
                            

################################
################################
                            
                            
# Step 1a: 
# What dimension do you want for your square grid? (e.g. 12 = 12x12 grid, 20 = 20x20 grid)
# Change "ii" to that number. 
#   NOTE: This is not a one-size-fits-all decision. You have to decide on how many grid 
#   units you want. You may have to try a range of them (e.g. 10x10, 15x15, 20x20, etc.) 
#   to see which works best for your research question. 

num.rows.cols = ii

# Step 1b: 
# How many pixels are in the horizontal axis of your image? 
# Change "jjjj" to that number. 
x.max = jjjj

# Step 1c: 
# How many pixels are in the vertical axis of your image? 
# Change "mmmm" to that number. 
y.max = mmmm

# Step 1d:
# Do you want a .csv file containing the upper and lower 
# bounds of each square in your unit? It will be called "parameters of your grid.csv"
# If yes, leave the T below as is. If no, change the T to F. 
want.grid.units = T


####################

# Step 2a:

# Load the file that contains only the POSITIVE slopes
the.data = read.csv("sample_input_positive_slopes.csv") 
#View(the.data)


# Step 2b:

# Load the file that contains only the NEGATIVE slopes
the.data2 = read.csv("sample_input_negative_slopes.csv") 
#View(the.data2)



#############################
#############################
#############################
#############################
#############################
#############################

# Section 1 
# Create the grid by which the data will be divided into. This grid will allow you 
# to plot the data as a uniformly distributed square dot plot. 


# The following code works off of the inputs that you gave in Steps 1a, 1b, 1c, and 1d.

# This is the pixel length of the horizontal dimension of each grid unit
x.unitlength = x.max/num.rows.cols 

# This is the pixel length of the vertical dimension of each grid unit
y.unitlength = y.max/num.rows.cols 

# This divides the max horizontal pixel length into equal units 
x.units = seq(0, x.max, by = x.unitlength)
x.units = round(x.units, digits = 1)  # round to the nearest tenth

# This divides the max vertical pixel length into equal units 
y.units = seq(0, y.max, by = y.unitlength)
y.units = round(y.units, digits = 1) # round to the nearest tenth


###############

# Instructions for what to do with "x.units": 
# You need to create a vector called "x.lows" that filters items 1 to 2nd-to-last. 
# You need to add 0.1 to each item in x.lows; call it x.lows.final
# You then need to create a vector called "x.upps" by filtering items 2 to last.

x.minusLast = length(x.units) - 1
x.lows = x.units[1:x.minusLast]

counter = c()

for (i in x.lows){ #This inadvertantly removes the first item of x.lows, which is a zero
  if (i > 0){ 
    new = i + 0.1
    counter = c(counter, new)
  } else {
    i == 0
  }
}

x.lows.final = c(0, counter) # add a zero back to start of the vector
x.lows.final

x.upps = x.units[2:length(x.units)]


# Instructions for what to do with "y.units": 
# You need to create a vector called "y.lows" that filters items 1 to 2nd-to-last. 
# You need to add 0.1 to each item in y.lows; call it y.lows.final
# You then need to create a vector called "y.upps" by filtering items 2 to -1.

y.minusLast = length(y.units) - 1
y.lows = y.units[1:y.minusLast]


y.counter = c()
for (i in y.lows){ #This inadvertantly removes the first item of y.lows, which is a zero
  if (i > 0){ 
    new = i + 0.1
    y.counter = c(y.counter, new)
  } else {
    i == 0
  }
}


y.lows.final = c(0, y.counter) # add a zero back to start of the vector


y.upps = y.units[2:length(y.units)]



##########
# Instructions for what to do with x.lows.final, x.upps, y.lows.final, and y.upps:

# For each item in x.lows.final, you need to repeat it times = num.rows.cols. 
#   Call this vector "x.lowers".
# For each item in x.upps, you need to repeat it times = num.rows.cols
#   Call this vector "x.uppers". 

x.lowers = rep(x.lows.final, each = num.rows.cols)


x.uppers = rep(x.upps, each = num.rows.cols)



# For the sequence in y.lows.final, you need to repeat THE WHOLE sequence 
# consecutively, times = num.rows.cols. Call this "y.lowers"
# For the sequence in y.upps, you need to repeat THE WHOLE sequence 
# consecutively, times = num.rows.cols. Call this "y.uppers"

y.lowers = rep(y.lows.final, times = num.rows.cols)


y.uppers = rep(y.upps, times = num.rows.cols)


######################

# Combine the four vectors into a data frame. Create a .csv file of this data frame.
grid.units = data.frame(x.lowers, x.uppers, y.lowers, y.uppers)

if (want.grid.units == T){
  write.csv(grid.units, "parameters of your grid.csv")  
} else {
  print("You chose not to create a .csv file of the parameters of your grid units.")
}




##############################
##############################
##############################
##############################
##############################
##############################

# Section 2
# This section works off of the file you loaded in Step 2a.
# Isolate the three columns of interest. 
  
# Change the name of three columns of interest: X, Y, and FeretAngle. 
# The new names will be: xcoord, ycoord, and angle.

colnames(the.data)[colnames(the.data)=="X"] <- "xcoord"
colnames(the.data)[colnames(the.data)=="Y"] <- "ycoord"
colnames(the.data)[colnames(the.data)=="FeretAngle"] <- "angle"

#View(the.data)

# Angles between 0 and 90 (including 90) are "positive"
wanted = filter(the.data, angle >= 0, angle <= 90)
slope = select(wanted, xcoord, ycoord, angle)
#View(slope)



##############################
##############################
##############################
##############################
##############################
##############################

# Section 3 
# Filter each slope into its appropriate grid unit based on its (x,y) coordinates.
# The code works off of your inputs in Steps 1a, 1b, 1c, and 1d.


# create a function called filtastic, which takes in four inputs
filtastic = function (n1, n2, n3, n4) {
  filter(slope, xcoord >= n1 , xcoord <= n2 , ycoord >= n3 , ycoord <= n4)
}

# apply the filtastic function from above to the four columns in the.param
megaman = mapply(filtastic, grid.units$x.lowers, grid.units$x.uppers, 
                 grid.units$y.lowers, grid.units$y.uppers)

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
##############################
##############################
##############################
##############################
##############################

# Section 4
# This section works off of the file you loaded in Step 2b.
# Isolate the three columns of interest. 

# Change the name of three columns of interest: X, Y, and FeretAngle. 
# The new names will be: xcoord, ycoord, and angle.

colnames(the.data2)[colnames(the.data2)=="X"] <- "xcoord"
colnames(the.data2)[colnames(the.data2)=="Y"] <- "ycoord"
colnames(the.data2)[colnames(the.data2)=="FeretAngle"] <- "angle"

#View(the.data2)

# angles between 90 and 180 are "negative"
wanted2 = filter(the.data2, angle > 90, angle < 180)
slope = select(wanted2, xcoord, ycoord, angle)
#View(slope)



##############################
##############################
##############################
##############################
##############################
##############################

# Section 5
# Filter each slope into its appropriate grid unit based on its (x,y) coordinates.
# The code works off of your inputs in Steps 1a, 1b, 1c, and 1d.

filtastic = function (n1, n2, n3, n4) {
  filter(slope, xcoord >= n1 , xcoord <= n2 , ycoord >= n3 , ycoord <= n4)
}

megaman2 = mapply(filtastic, grid.units$x.lowers, grid.units$x.uppers, 
                  grid.units$y.lowers, grid.units$y.uppers)

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



##############################
##############################
##############################
##############################
##############################
##############################

# Section 6 
# Combine the results from Sections 2 and 4. 

melting.pot = cbind(the.count.pos, the.count.neg)

colnames(melting.pot)[colnames(melting.pot)=="the.count.neg"] = "neg.slopes"
colnames(melting.pot)[colnames(melting.pot)=="the.count.pos"] = "pos.slopes"
#head(melting.pot)

# These two lines determine the dimensions of the dot plot based on your input in Step 1a.
x.location = rep(1:num.rows.cols, each = num.rows.cols)
y.location = rep(1:num.rows.cols, times = num.rows.cols)

oohlala = cbind(melting.pot, x.location, y.location)
oohlala = as.data.frame(oohlala)
#View(oohlala)


##############################
##############################
##############################
##############################

# Section 7 - Isolating 


# Section 7a - Prep the data for subsequent sections. 

# This line creates a new column called sum.posNneg, which contains 
#   the sum pos.slopes and neg.slopes per row.
oohlala = mutate(oohlala, sum.posNneg = pos.slopes+neg.slopes)

# Create a vector as long as # of rows in oohlala
permanent.rowNumb = seq(1:dim(oohlala)[1]) 

# Add the above vector as the first column of oohlala. It will serve as an permanent address for each row.
oohlala = cbind(permanent.rowNumb, oohlala) 

# Make a copy of the pos.slopes and neg.slopes columns at end of oohlala.
#   Call them converted.pos.slopes and converted.neg.slopes.
oohlala = mutate(oohlala, converted.pos.slopes = pos.slopes)
oohlala = mutate(oohlala, converted.neg.slopes = neg.slopes)

############

total.gridunits = dim(oohlala)[1] # This is the number of units in your grid system (i.e. 16x16 = 256)

############

# Section 7b - Isolate rows that are: pos.slopes==0/neg.slopes>0. 
#   Change the value of pos.slopes to -1, then muliply them by neg.slopes.
#   What's the point? 0/2 and 0/6 both = 0, so we are losing important info;  
#   but -1*2 = -2 and -1*6 = -6, both of which are more informative than 0.

# This filters all rows in which pos.slopes = 0 and neg.slopes > 1
pe0.ngt1 = filter(oohlala, pos.slopes==0, neg.slopes>1)
#View(pe0.ngt1)

####
minus.ones = rep(-1, times = dim(pe0.ngt1)[1]) # create a vector of repeating -1
pe0.ngt1$converted.pos.slopes = NULL
pe0.ngt1 = cbind(pe0.ngt1, minus.ones)
pe0.ngt1 = rename(pe0.ngt1, converted.pos.slopes = minus.ones) # Rename minus.one to coverted.pos.slopes
pe0.ngt1 = pe0.ngt1[, c(1,2,3,4,5,6,8,7)] # This switches order of last to columns to match initial ordering
pe0.ngt1 = mutate(pe0.ngt1, converted.PosOverNeg = converted.pos.slopes*converted.neg.slopes)



######################

# Section 7c - Isolate rows that are: pos.slopes==0/neg.slopes==1. 
#   Change the value of pos.slopes to -1, then muliply them by neg.slopes.
#   What's the point? 0/1, so we are losing info for this grid unit that has 1 neg slope;  
#   but -1*1 = -1, so -1 is more informative than 0.


# This filters all rows in which pos.slopes = 0 and neg.slopes = 1
pe0.ne1 = filter(oohlala, pos.slopes==0, neg.slopes==1)
#View(pe0.ne1)


######
pe0.ne1 = pe0.ne1[, -c(7,8,9)] # Remove the last three columns

minus.ones.2 = rep(-1, times = dim(pe0.ne1)[1]) # create a vector of repeating -1
pe0.ne1 = cbind(pe0.ne1, minus.ones.2)
pe0.ne1 = rename(pe0.ne1, converted.pos.slopes = minus.ones.2) # Rename minus.one to coverted.pos.slopes
pe0.ne1 = mutate(pe0.ne1, converted.neg.slopes = neg.slopes)
pe0.ne1 = mutate(pe0.ne1, converted.PosOverNeg = converted.pos.slopes*converted.neg.slopes)


######################

# Section 7d - Isolate rows that are: pos.slopes==1/neg.slopes==0. 
#   Change the value of neg.slopes to 1, then muliply them by pos.slopes.
#   What's the point? 1/0 is undefined, so we are losing info for this grid
#     unit that is shown as blank spot on graph;  
#     but 1*1 = 1, so 1 is more informative than 0.


# This filters all rows in which pos.slopes = 1 and neg.slopes = 0,
#   which would result in "NA" b/c you can't divide by 0
pe1.ne0 = filter(oohlala, pos.slopes==1, neg.slopes==0)
#View(pe1.ne0)

pe1.ne0 = pe1.ne0[, -c(7,8)] # Remove the last three columns

ones = rep(1, times = dim(pe1.ne0)[1]) # create a vector of repeating 1
pe1.ne0 = cbind(pe1.ne0, ones)
pe1.ne0 = rename(pe1.ne0, converted.neg.slopes = ones) # Rename ones to coverted.pos.slopes
pe1.ne0 = mutate(pe1.ne0, converted.pos.slopes = pos.slopes) # Duplate pos.slopes and rename it converted.pos.slopes
pe1.ne0 = pe1.ne0[, c(1,2,3,4,5,6,8,7)]
pe1.ne0 = mutate(pe1.ne0, converted.PosOverNeg = converted.pos.slopes*converted.neg.slopes)






#######################
# Section 7e - Isolate rows that are: pos.slopes>0/neg.slopes==0. 
#   Change the value of neg.slopes to 1, then muliply them by pos.slopes.
#   What's the point? 2/0=undefined; 8/0=undefined; so we are losing info for grid
#     units that have many positive slopes but are "undefined" because 0 in denominator.  
#     But, 2/1=2, and 8/1=8, so 2 and 8 are more informative than "undefined".

# This filters all rows in which pos.slopes > 0 and neg.slopes = 0, 
#   which would result in "NA" b/c you can't divide by 0
pgt0.ne0 = filter(oohlala, pos.slopes>0, neg.slopes==0)
#View(pgt0.ne0)

pgt0.ne0 = pgt0.ne0[, -c(7,8)] # Remove the last three columns

ones = rep(1, times = dim(pgt0.ne0)[1]) # create a vector of repeating 1
pgt0.ne0 = cbind(pgt0.ne0, ones)
pgt0.ne0 = rename(pgt0.ne0, converted.neg.slopes = ones) # Rename ones to coverted.pos.slopes
pgt0.ne0 = mutate(pgt0.ne0, converted.pos.slopes = pos.slopes) # Duplate pos.slopes and rename it converted.pos.slopes
pgt0.ne0 = pgt0.ne0[, c(1,2,3,4,5,6,8,7)]
pgt0.ne0 = mutate(pgt0.ne0, converted.PosOverNeg = converted.pos.slopes/converted.neg.slopes)


####################
# Section 7f - Combine results and Save the data as a .csv file

voila = rbind(pe0.ngt1, pe0.ne1, pe1.ne0, pgt0.ne0)
#View(voila)

# Create dataframe containing all items in oohlala except for the rows contained in voila
toDrop.voila = voila$permanent.rowNumb # list of what rows to remove
remaining = oohlala[ !(rownames(oohlala) %in% toDrop.voila), ] # remove the rows
remaining = mutate(remaining, converted.PosOverNeg = converted.pos.slopes/converted.neg.slopes)
#View(remaining)

shabbam = rbind(voila, remaining)
shabbam = arrange(shabbam, permanent.rowNumb)
#View(shabbam)


# save the data in a new .csv file
write.csv(shabbam, "your qsi data.csv", row.names = F)



############################
############################
############################
############################
############################

# Section 8

# Section 8a - Calculate percentages of each of the four special 
#   categories (from the previous section) with regard to the total number of grid units.


#####
# Calculate percentage of total for category: positive/negative = 0/(N>1)
num.pe0.ngt1 = dim(pe0.ngt1)[1]
percent_pe0.ngt1 = round(num.pe0.ngt1/total.gridunits, 3)


#####
# Calculate percentage of total for category: positive/negative = 0/1
num.pe0.ne1 = dim(pe0.ne1)[1]
percent_pe0.ne1 = round(num.pe0.ne1/total.gridunits, 3)


#####
# Calculate percentage of total for category: positive/negative = 1/0
num.pe1.ne0 = dim(pe1.ne0)[1]
percent_pe1.ne0 = round(num.pe1.ne0/total.gridunits, 3)


#####
# Calculate percentage of total for category: positive/negative = (N>1)/0
num.pgt0.ne0 = dim(pgt0.ne0)[1]
percent_pgt0.ne0 = round(num.pgt0.ne0/total.gridunits, 3)



##################

# Section 8b - Print the descriptive statistics and how to interpret them.

gather = list( "Category 1 - # of 0/(N>1)" = num.pe0.ngt1, "Category 1 - % is 0/(N>1)" = percent_pe0.ngt1, "Category 2 - # of (N>1)/0" = num.pgt0.ne0, "Category 2 - % is (N>1)/0" = percent_pgt0.ne0, 
              "Category 3 - # of 0/1" = num.pe0.ne1, "Category 3 - % is 0/1" = percent_pe0.ne1, "Category 4 - # of 1/0" = num.pe1.ne0, "Category 4 - % is 1/0" = percent_pe1.ne0)


export.me = as.matrix(gather)
export.me

write.csv(export.me, "descriptive stats QSI.csv")


##############################
##############################
##############################
##############################
##############################
##############################

# Section X
# Plot the Dot Plot!

# Plot the data as a dot plot, such that each grid unit is a large square instead of a small dot.


ggplot(shabbam, aes(x=x.location, y=y.location, color=converted.PosOverNeg)) + 
  geom_point(size=25, shape=15) + scale_color_gradient(low="red", high="green") + 
  ggtitle("QSI Plot") + xlab(" ") + ylab(" ") + 
  theme(plot.title = element_text(hjust = 0.5))


print("This script is finished running. Save the image of the graph. This is just a dot plot of uniformly spaced dots that are very large squares.")

