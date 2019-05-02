# Quadrant-Slope-Index-QSI
The R Script for automating the calculations and graphing for the QSI algorithm. 

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
#       * Slopes between 0 and >=90 degrees are considered positive. 
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
