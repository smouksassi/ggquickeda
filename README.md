ggplotwithyourdata
========
R Shiny app as a handy inteface to ggplot2. It enables you to quickly explore your data to detect trends on the fly. You can do scatter plots, dotplots, boxplots, barplots, histograms and densities.
For a quick overview head to this <a href="https://www.youtube.com/watch?v=1rBBmJUIZhs" target="_blank"> Youtube Tutorial </a>.

**Export Plots** and **Plot Code** tabs contributed by <a href="https://github.com/daattali" target="_blank">Dean Attali</a>. Once a plot is saved in the **X/Y Plot** tab by providing a name and hitting the **Save plot** star button it will become available for exporting. You can export in portrait, landscape and multiple plots per page.  
**Plot Code** will let you look at the source code that generated the plot with the various options.

Quick summary statistics tables using  <a href="https://github.com/benjaminrich" target="_blank">Benjamin Rich </a> table1 package. 

### CSV Data Input 
* [read.csv("youruploadeddata.csv",na.strings = c("NA","."))]

### Data Manipulations 
* Change continuous variable to be treated as categorical 
* Change continuous variable to categories with a specified number of bins or by supplying values for the bins start/end
* Up to six sequential filters for categorical and continuous variables
* Renaming and reordering of the levels of categorical variable(s)
* Combining two categorical variables into one
* Rounding a numerical variable to a specified number of digits

### Summary/Regression functions 
* Quantile Regression 
* Smooth/Linear/Logistic Regressions
* Mean Confidence Intervals
* Median Prediction Intervals
* Kaplan-Meier
* Correlation Coefficients

### `ggplot2` built-in functionality
*`facet_grid` and `facet_wrap`
* Group, color, size, fill mappings
* Controlling y and x axis labels, legends and other commonly used theme options.
* Adding a title, subtitle and a caption

### Running Information
Make sure to have all required packages installed
shiny, shinyjs, ggplot2, ggrepel, scales, DT, tidyr, dplyr, Hmisc, quantreg, markdown, ggkm, colourpicker and table1.
The app will try to install ggkm and table1 (if not found) using the commands below:
```
install.packages(c("shiny","shinyjs","ggplot2","scales","DT","tidyr","dplyr","Hmisc","quantreg","markdown","colourpicker","devtools"))
devtools::install_github("sachsmc/ggkm")
devtools::install_github("benjaminrich/table1")
```
Make sure that you are using the this version of ggrepel 0.70 or later.
```
devtools::install_github("slowkow/ggrepel")

```


The main app contains several testing apps the main app can be run using this command:
```
shiny::runGitHub('ggplotwithyourdata', 'smouksassi', subdir = 'shinyapp')
```
Try to reproduce the steps below using the included sample_df.csv.
### Example 1
![Example use case 1 with the included sample_df.csv.](img/snapshot1.gif)

### Example 2
![Example use case 2 with the included sample_df.csv.](img/snapshot2.gif)

### Example 3
![Example use case 3 with the included sample_df.csv.](img/snapshot3.gif)

### Example 4
![Example use case 4 with the included sample_df.csv.](img/snapshot4.1.png)

