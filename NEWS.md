# ggquickeda 0.1.6.9999
* added gradientInput (broken, submitted an issue)
* fixed a bug in risktable when nothing was selected
* added controls for plot margins
* added controls for legend margins
* strip.position for facet_wrap and more labeller options
* added parsed x and y axis titles
* custom position for legends and specification of legend and items background fill
* updated intro vignette

# ggquickeda 0.1.5
* let ggpairs plots reflect custom color scales
* update the emax smoother to add e0
* apply a fix for the weight aesthetic for emax smoother
* update code to be compatible with breaking change from tidyr 1.0
* user defined shape and linetype scales
* user defined custom labels for custom x and y axes ticks
* user can divide by a constant not just a column


# ggquickeda 0.1.4
* custom labels with text, label, text_repel and label_repel
* user can now add more custom colors up to 20
* added user control to specify mid color of continuous color scales
* added user control to specify strip text colour and more colourpicker background color
* order stacked data by sd of value 
* added smooth emax model via nls and parameter values
* added parameter values for lm fit and made text size adjustable
* viridis and custom continuous color scale
* added possibility to divide multiple columns by a specified one (e.g. dose normalization)
* "Treat as Categories:" now accepts character variables in addition to numeric.
* "Treat as Numeric:" added to enable converting columns read as factors.
* updated the docs for geom and stat_km stat_kmband stat_kmticks.


# ggquickeda 0.1.3
* UI and options improvements for KM (line sizes, transparency, ignore group and color), linetype mappping
* changed default error in stat_kmbands, borrowed stairstepn from ggalt and applying zoo::na.locf to deal with na
* added the possibility to add median survival with or without CI
* Using stat_cor from ggpubr to compute correlations for more flexibility and margin in facets
* added ability to ignore grouping and or ignore color mapping for correlation coefficient
* added possibility to dodge means and medians
* added possibility to specify means and medians N and Values transparency and a default seed for text_repel
* geom text,label, withe repel variants and position dodge for median and mean labels
* added transparency control for densities and histograms as well as linetype mappping
* added binwidth control for histogram (user defined or auto), and position dodge
* added window adjustment control for densities
* linetypes for predefined quantiles
* added the possibility to add one more therapeutic window and fixed a bug with categorical x axis
* added possibility to remove x/y axis tick labels
* fixes for continuous scales logic
* Added initial support for ggpairs



# ggquickeda 0.1.2
* bug fixed where in some cases plotdata did not apply filtering
* updated and expanded merge multiple levels of a factor
* added mappings by shape for points and linetypes of lines and associated custom legends and possibility to ignore it
* added the possibility of choosing more linetypes for lines
* various UI improvements for points, lines, qr, smooth, median and mean
* added options to control size and transparency of qr, smooth, median and mean lines and added predefined qr 25 and 75%
* added options to force mean and median shapes
* fixed bug in median line not honoring transparency when Median\PI selected with some UI tweaks
* added more options for boxplot outliers (size and transparency)
* separate color control for major and minor gridlines with the possibility to remove them
* added annotation_logticks
* make faceting less prone to fail when user mistakenly choose the same variable in rows and columns
* make user defined color/fill scales

# ggquickeda 0.1.1
* added options to control facets strip background fill and placement
* added options to control panel spacing
* reworked barplots to be able to sort by frequency or reverse frequency
* histograms and densities gain more options and flexibility
* added possibility to reorder factors by length of of another variable
* added possibility to keep last row by id
* added more options to label x and y axes ticks e.g. using `prettyNum()`
* added styling options for horizontal and vertical reference lines
* added more methods for the correlation coefficient
* added p-values for slope(s) and adjusted Rsquare when a linear regression smoother is selected
* fixed a bug where x and or y axis zoom would not work in some situations
* added more stats from table1 as well as possibility to cut by quantiles `eqcut()`
* added possibility to merge multiple levels of a factor
* fixed bug where commas in label names broke up the factor levels when using "Recode/Reorder Categories"

# ggquickeda 0.1.0

Initial CRAN release
