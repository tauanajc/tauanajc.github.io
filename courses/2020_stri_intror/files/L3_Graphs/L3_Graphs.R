#' ---
#' title: "Tutorial 3 - Graphs"
#' author: "Tauana Cunha"
#' date: "May 2020"
#' ---

#' Plots for data exploration and to carefully design figures for papers and talks
#' Plan for today: 
#' basic built-in functions
#' package ggplot2


##############' Setup
#' 
#' First set your working directory:
setwd(dir = "/Users/tauanajc/TEACHING/2020_STRI_introR/L3_Graphs") # Type the path to your project folder


##############' Install and load packages
#' 
#' More tools are available through packages, sets of new R functions developed by the R community
#' 
install.packages("ggplot2")
#' The package is now in your computer and you can delete the installation command from your script.
#' 
#' Load it in your R session to make the new set of functions avalaible.
#' Keep the loading commands at the top of the script
library(ggplot2)
#'
#' If a package has never been installed, you will get an error trying to load it:
library(ggrepel)


##############' Import and manipulate a dataset
#' 
#' Same dataset from Lee's tutorial: leaves, roots, and soils from five plant species growing throughout a site contaminated with the heavy metals zinc, lead and cadmium.
#' Investigate relationships between the metal concentrations in the soil, root colonization by mycorrhizal fungi (AMF), and metal concentrations in the leaves.

#' Read data
heavy_metals <- read.csv(file = "../L2_FlowControl/Dietterich et al 2017 data for R course.csv")

#' Column names, first few rows and structure:
colnames(heavy_metals)
head(heavy_metals)
str(heavy_metals)

#' To make things more clear for us, let's change the column names:
colnames(heavy_metals) <- c("SampleID", "Species", "Mycorrhizal_Colonization",
                            "Leaves_Cadmium", "Leaves_Zinc", "Leaves_Lead",
                            "Soil_Cadmium", "Soil_Lead", "Soil_Zinc")
#' Check how they are now:
colnames(heavy_metals)

#' Change the species codes, replacing them with the genus name:
#' 
#' Create a vector with the genus names
genera <- c("Ageratina", "Deschampsia", "Eupatorium", "Minuartia", "Agrostis")
# Name the elements of this vector according to their code
names(genera) <- c("AA", "DF", "ES", "MP", "AP")
genera
#'
#' Let's try indexing the new genera object
genera[c("AA", "AA", "AA")] # We get 3 times the full genus name of code AA
#'
# Now let's give it the entire column of species
genera[heavy_metals$Species] # We get a new vector of all the genera names according to the code!
#'
#' Use the command above to overwrite the content of the column Species
heavy_metals$Species <- genera[heavy_metals$Species]
#' 
#' Check that this worked by looking at some random lines of the dataset
#' Use indexing to select just a few lines, and the first 5 columns:
heavy_metals[c(1,30,50,75,90), 1:5]
#' 
#' Before plotting, change the Species column from character to factor:
heavy_metals$Species <- as.factor(heavy_metals$Species)
str(heavy_metals)


##############' Basic plots with built-in functions
#' 
#' Some of the most common plot types with built-in functions in R:
#' 
#' ## Histogram
#' Frequencies of values in your data
#' Automatically bins data in ranges, but you can customize number of bins
#' 
#' Distribution of the zinc concentrations in the leaves of all plant species
hist(x = heavy_metals$Leaves_Zinc)
#' 
#' The names of the axis look terrible! Always change them to informative names
hist(x = heavy_metals$Leaves_Zinc,
     breaks = 20, # Specify number of bins
     main = "", # Plot title
     xlab = "Zinc concentration in plant leaves (mg/kg)", # Axes labels
     ylab = "Frequency")
#' 
#' Much better!
#' 
#' 
#' ## Boxplot
#' Summarizes data, lines of the box are the median, quartiles, minimum and maximum values of the dataset
#' Spacing between parts of the box reflect amount of spread and skewness
#' 
#' Boxplot for the same zinc concentrations
boxplot(x = heavy_metals$Leaves_Zinc,
        ylab = "Zinc concentration in leaves (mg/kg)")
#' 
#' More information on how to interpret boxplots are in the online tutorial
#' 
#' Distribution of zinc concentrations for each one of the individual plant species
#' Plot a separate box for each level of a factor:
boxplot(formula = Leaves_Zinc ~ Species,
        data = heavy_metals,
        ylab = "Zinc concentration in leaves (mg/kg)",
        xlab = "")
#' 
#' 
#' ## Scatter plot
#' Relationship between two variables
#' 
#' Soil and leaf concentrations of lead
plot(formula = Leaves_Lead ~ Soil_Lead,
     data = heavy_metals,
     ylab = "Lead concentration in leaves (mg/kg)",
     xlab = "Lead concentration in soil (mg/kg)")
#' 
#' What if we look at individual plant species?
#' Use a FOR LOOP to get a scatter plot for each of the plants
for(i in 1:5){ # 5 species of plant
  each_plant = subset(x = heavy_metals,
                      Species == levels(heavy_metals$Species)[i]) # subset for each species
  plot(formula = Leaves_Lead ~ Soil_Lead,
       data = each_plant, # plot variables
       main = levels(heavy_metals$Species)[i], # title indexed to get species name
       ylab = "Lead concentration in leaves (mg/kg)",
       xlab = "Lead concentration in soil (mg/kg)")}
#' 
#' Remember this is a small sample dataset, not the complete dataset from Lee's paper
#' Plots are merely illustrative and do not necessarily reflect real results


##############' Customization with base R
#' 
#' Many other types of plots and many more options for customization
#' 
colors() # existing colors in base R

?points # symbols

palette.pals() # several new palettes added as of R v4.0.0


#' PANEL WITH 4 DIFFERENT PLOTS: HIST, BOX, SCATTER, and BAR
#' ## Barplot
#' Data frame for the colors for each species
plant_colors <- data.frame(Species = factor(levels(heavy_metals$Species)),
                           Color = c("darkgreen", "darkorange",
                                     "deepskyblue", "firebrick", "darkviolet"),
                           Symbol = c(7,19,24,0,8))
plant_colors
#'
#'
#' Creating a function to calculate the standard error - same as last week
se <- function(x) {
  sd(x)/sqrt(length(x))}
#'
#' Calculate mean of zinc concentrations for each species
for_barplot <- with(data = heavy_metals,
                    aggregate(formula = Leaves_Zinc ~ Species,
                              FUN = mean))
#' Change column names
colnames(for_barplot) <- c("Species", "Mean_Zinc")
#'
#' Calculate standard error
for_barplot$SE_Zinc <- with(data = heavy_metals,
                            aggregate(formula = Leaves_Zinc ~ Species,
                                      FUN = se))[,2]
#' Check what's there now
for_barplot
#' 
#' 
##############' PANEL WITH MULTIPLE PLOTS
#' 
?par # graphic parameters

#pdf(file = "baseRpanel.pdf", width = 8, height = 8) # Explained below
#'
par(mfrow = c(2,2)) # mfrow: panel of 2x2 plots, ordered by row

# Histogram
hist(heavy_metals$Soil_Zinc,
     breaks = 20,
     main = "",
     xlab = "Zinc concentration in soil (mg/kg)")

# Boxplot
boxplot(formula = Leaves_Zinc ~ Species,
        data = heavy_metals,
        col = plant_colors$Color,
        xaxt = "n", # Suppress the x axis completely
        ylab = "Zinc concentration in leaves (mg/kg)",
        xlab = "",
        main = "Boxplots are better than barplots:")

text(x = 1:5, y = -23, # Position to add labels in x axis
     labels = for_barplot$Species, # Text to add
     xpd = TRUE, # Enable text outside the plot region
     srt = 15) # Angle

mtext(text = "GOOD INFO/INK RATIO",
      side = 3, # Side of the graph
      col = "firebrick",
      cex = .8) # Size of text

# Scatter plot
plot(formula = Mycorrhizal_Colonization*100 ~ Soil_Zinc,
     data = heavy_metals,
     col = palette.colors(n = 5, "Dark 2"),
     pch = plant_colors$Symbol,
     ylab = "Root colonization by fungi (%)",
     xlab = "Zinc concentration in soil (mg/kg)")

mtext(text = "Different color palette just to illustrate",
      side = 4,
      col = "firebrick",
      cex = .8)

# Barplot
barplot(formula = Mean_Zinc ~ Species,
        data = for_barplot,
        col = plant_colors$Color,
        xaxt = "n",
        ylab = "Zinc concentration in leaves (mg/kg)",
        xlab = "",
        ylim = c(0,80))

text(x = c(0.7,1.9,3.1,4.3,5.5),
     y = -5,
     labels = for_barplot$Species,
     xpd = TRUE,
     srt = 15)

arrows(y0 = for_barplot$Mean_Zinc-for_barplot$SE_Zinc,
       y1 = for_barplot$Mean_Zinc+for_barplot$SE_Zinc,
       x0 = c(0.7,1.9,3.1,4.3,5.5),
       x1 = c(0.7,1.9,3.1,4.3,5.5),
       angle = 90,
       code = 3,
       length = 0.1)

mtext(text = "BAD INFO/INK RATIO",
      side = 3,
      col = "firebrick",
      cex = .8)

par(mfrow = c(1,1)) # Set graphic device back to 1 plot only

#dev.off() # Explained below
##############' END OF PANEL


##############' Save basic plots to file
#' 
#' Functions like pdf(), png(), tiff(), jpeg()
#' Open an external graphics device to save
#' Graph will not show on RStudio window, it will be saved in your working directory
pdf(file = "baseRhist.pdf", width = 5, height = 5) # Open device

hist(heavy_metals$Soil_Zinc,
     breaks = 20,
     main = "Zinc concentrations (mg/kg)",
     xlab= "Soil")

dev.off() # Close device

#' Let's go back and save the 4-plot panel above


##############' ggplot2
#' 
#' Arguably the most used R tool for creating graphs nowadays
#' 
#' A major difference: syntax
#' Layers are put together to create graphs
#' More verbose for simple graphics, much less verbose for complex graphics
#' Data should always be in a data frame
#
#' Build same types of graph that we built with base R
#' 
#' ## Histogram
ggplot(data = heavy_metals) # Just an empty plot, but holds the data to be used
#'
#' Specify set of aesthetics that will be used throughout the layers (information is inherited)
#' Aesthetic (aes): mappings of visual element to a specific variable
ggplot(data = heavy_metals, # Dataset
       aes(x = Leaves_Zinc)) + # Position of data for the x axis
  geom_histogram(bins = 20, na.rm = TRUE) # Specify histogram type of plot

#' ## Default themes
#'
gghist <- ggplot(data = heavy_metals,
                 aes(x = Leaves_Zinc)) +
  geom_histogram(bins = 20, na.rm = TRUE) +
  theme_minimal() + # Various default themes
  labs(x = "Zinc concentration in plant leaves (mg/kg)", # Axes names
       y = "Frequency")
#'
gghist
#' 
#' ## Boxplot
#' 
#' Aesthetics can be also used in the geom_ functions, in which case they are not inherited by other layers
ggplot(data = heavy_metals) +
  geom_boxplot(aes(y = Leaves_Zinc, x = Species), na.rm = TRUE) + # boxplot with variables in aesthetics
  theme_minimal() +
  labs(x = "", y = "Zinc concentration in plant leaves (mg/kg)")
#' 
#' ## Mapping color aesthetics
#' 
#' Add color to the border and fillings of the boxes according to a variable
ggplot(data = heavy_metals) +
  geom_boxplot(aes(y = Leaves_Zinc, x = Species,
                   col = Species, fill = Species), na.rm = TRUE) + # new color aesthetics
  theme_minimal() +
  labs(x = "", y = "Zinc concentration in plant leaves (mg/kg)")
#' 
#' Fill hides the value of the median
#' Make it transparent in the geom_ function (fixed visual attributes are set outside of aes)
#' Change color schemes with scale_ functions
?scale_color_brewer # Color Brewer

ggplot(data = heavy_metals) +
  geom_boxplot(aes(y = Leaves_Zinc, x = Species,
                   col = Species, fill = Species),
               na.rm = TRUE,
               alpha = 0.3) + # Transparency
  theme_minimal() +
  labs(x = "", y = "Zinc concentration in plant leaves (mg/kg)") +
  scale_fill_brewer(palette = "Dark2") + # Color palette for the fill
  scale_color_brewer(palette = "Dark2") # and for the borders
#' 
#' ## Theme function: legends, fonts of axis, titles, background etc.
#' 
#' Remove legend
#' Italicize names of genera
#' Make them fit better by setting the words at an angle
ggbox <- ggplot(data = heavy_metals) +
  geom_boxplot(aes(y = Leaves_Zinc, x = Species,
                   col = Species, fill = Species),
               na.rm = TRUE,
               alpha = 0.6) +
  theme_minimal() +
  labs(x = "", y = "Zinc concentration in plant leaves (mg/kg)") +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none", # Remove legend
        axis.text.x = element_text(face = "italic", # Make names italic
                                   size = 12, # Increase font size for x labels
                                   angle = 15)) # Angle of x labels
#'
ggbox
#' 
#' ## Scatter plot
#' 
#' Use aesthetics to differentiate the points of each Species by color in the same plot
#' Label specific points of interest with geom_text
ggpoint <- ggplot(data = heavy_metals,
                  aes(x = Soil_Lead, y = Leaves_Lead)) +
  geom_point(aes(col = Species, fill = Species), # aestethics for geom_points
             na.rm = TRUE,
             pch = 22, # symbol
             size = 3) + # size of points
  theme_minimal() +
  theme(axis.title = element_text(size = 11),
        legend.text = element_text(face = "italic", size = 11), # Italics in the legend
        #legend.position = "top", # Position of legend
        legend.position = "none", # Position of legend
        legend.title = element_blank()) + # Remove title of legend
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") +
  geom_text(aes(label = SampleID),
            data = subset(heavy_metals, Leaves_Lead > 10000)) + # Label subset of points
  labs(x = "Lead concentration in soil (mg/kg)",
       y = "Lead concentration in leaves (mg/kg)")
#'
ggpoint
#' 
#' ## Barplot
#' 
ggbar <- ggplot(data = heavy_metals,
                aes(y = Leaves_Zinc, x = Species,
                    col = Species, fill = Species)) +
  geom_bar(stat = "summary", # Barplot with mean as summary statistics
           fun = "mean", na.rm = TRUE) +
  stat_summary(fun.data = mean_se,
               geom = "errorbar", na.rm = TRUE, # Add SE bars
               width = .5, col = "black") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12, face = "italic", angle = 15),
        legend.text = element_text(face = "italic", size = 12),
        legend.position = "none") +
  scale_color_brewer(palette = "Dark2") + 
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "", y = "Zinc concentration in leaves (mg/kg)")

ggbar
#' 
#' 
#' ## Assign ggplots to objects
#' 
zinc_box <- ggplot(data = heavy_metals) +
  geom_boxplot(aes(y = Leaves_Zinc, x = Species), na.rm = TRUE)

zinc_box

zinc_box +
  theme_minimal() +
  labs(x = "", y = "Zinc concentration in plant leaves (mg/kg)")
#' 
#' 
#' ## Facet plots
#' 
#' Display several plots of the same kind, each representing a level of a categorical variable
#' Zinc data, one plot per Species, all in the same figure
ggplot(data = heavy_metals) +
  geom_point(aes(x = Soil_Lead, y = Leaves_Lead, fill = Species),
             na.rm = TRUE, pch = 22, size = 3) +
  theme_minimal() +
  facet_wrap(~ Species, nrow = 3) + # Wrap plots by Species
  theme(axis.title = element_text(size = 12),
        legend.position = "none",
        strip.text = element_text(size = 12, face = "italic")) +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Lead concentration in leaves (mg/kg)",
       y = "Lead concentration in soil (mg/kg)")
#' 
#' 
#' Go back to the four ggplots and assign them to object names: gghist, ggbox, ggpoint, ggbar
#' 
#' 
#' ## Grid of independent plots
#' 
#' Package cowplot - move this command to top of the script
library(cowplot)

metal_plots <- plot_grid(gghist, ggbox, ggpoint, ggbar, # All plots
          nrow = 2, # Plots distributed in two rows
          align = "h", # Align plots horizontally
          axis = "l", # Side on which to align
          labels = "AUTO", # Place autmatic labels on each plot
          scale = 0.88) # Scale down the size of plots to increase white space in between

metal_plots
#' 
#' 
#' ## Save ggplots to file
ggsave(filename = "ggplot2panel.pdf",
       plot = metal_plots,
       device = "pdf",
       width = 8,
       height = 8,
       units = "in")
#' 
#' 
#' # Exercises!
#' 
#' Practice your skills with plotting on another dataset: your own or the attached dataset about Himalayan Climbing Expeditions (members.csv)
#' 
#' 
#' IMPORT AND EXPLORE YOUR DATA:
#'
#'
#' 1. Plot two types of graph (it's ok to try out other types that we did not see together!). Feel free to use built-in functions or ggplot2. The dataset has many interesting variables, many possible questions to explore. There is no correct answer, you can pick and choose basically anything you like. The only rule is that you have to change, in one or both of the graphs, at least the following:
#' 
#' * axes titles
#' * any font element in any part of the plot
#' * color scheme
#' * some element of the legend
#' 
#' PLOT 1:
#' 
#' 
#' PLOT 2:
#' 
#' 
#' 2. Now combine these two (or more) plots into one figure:
#' PANEL WITH MULTIPLE PLOTS"
#' 
#' 
#' 3. Save your plot(s)
#' SAVE: 
#' 
#' 
#' YOU ARE DONE =)