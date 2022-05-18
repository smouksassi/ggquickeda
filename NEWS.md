# ggquickeda 0.2.9999
* prepare for next release

# ggquickeda 0.2.2
* fixed bug in linear model equation where slope and intercept values were switched
* fixed bug where flip barplot was not working (auto flipped barplot already supported)
* added group aesthetic and other enhancements for pairs plot
* bug fixes for expansion, zooming with user input (plots with only x variable(s) or only y variable(s))
* restored target window annotations to work with factor/character x axis
* added the possibility to change the size and linetype of density plots
* added the possibility to choose the shape of boxplot outlier points
* added options for x and y scale tick formatting are now visible when pairs plot are active
* added options for identity line and for placement of custom/target text at plot edges
* added checkbox to recode numeric variables with -99 to missing
* removed ggstance dependency by adding needed positions to global.r
* fixed typos in Visualizing Summary Data vignette data labels

# ggquickeda 0.2.1
* added a new method for factor reordering min/max difference
* added the mean +/- multiples of standard deviations summary using the `mean_sdl` function
* allowed to transform the label for mean and median to 10^ (useful when the x/y axis are logged)
* fixed a namespace bug with calling the `scales` package functions like `muted` or `trans_xxx` without the package being loaded, now using `::`.
* fixed a bug with `updateColourInput` which is now only available from the `colourpicker` package
* reactivating the colorgradient widget after `shinyjqui` bug was updated and fixed
* added distiller continuous Blues and brewer discrete palettes
* added possibility to specify palette for viridis discrete and continuous with possibility to reverse the scale
* added more support for `POSIXct` variables where previously they were being treated as character
* automatic zoom slider is now working with `POSIXct`
* automatic Slider or User zoom now is controllable when facets scales are free  
* discrete scale is no longer applied when the x and or y variable inherits from `POSIXct`
* modifying X/Y scale expansion is now optional (fixed auto expansion with free facets)

# ggquickeda 0.2.0
* Added Violin plots
* Added options and fixes for histograms, barplots and densities
* Added options for Median/Mean/N labels (N digits, justification, edge positions.)
* Added points `position_quasirandom` and `position_beeswarm`
* Added multiple enhancements for `theme` control like `panel.border`, `plot.tag.position` and X and Y Axes Titles formatting
* Added multiple enhancements and options for ggpairs
* logticks and rug marks can now be on the outside of plot panel
* coordinates cartesian clip can now be set to off
* x/y axis percent and comma labels format now works for univariate plots
* x/y zoom now works when more than one x and/or y variables are selected
* long x/y discrete axes labels can now be wrapped to specified length
* UI updates and auto hiding for univariate plots, pairs, km
* Boxplots UI rework
* Readme updated
* summary level data vignette updated

# ggquickeda 0.1.9
* Made errorbars for Median/PI and Mean/CI ignore mapped size
* fixed a bug when labels from Median/PI and Mean/CI and data are used
* table1 enhancements: na.is.category, render.missing, footnote and caption.
* table1 enhancements: added N missing and Sum to stats.
* added second field to order and recode variables after stacking e.g. for xvars and yvars
* added two new by variable factor sorting methods: 'N Unique' and 'Sum'
* fixed a bug when custom reorder ignored the current order as starting point
* added ggplot tag field
* fixed a bug when user was selecting the same variables for x and y now stacking x and y is separate.
* fixed a bug in x axis label and barplots in some special cases
* updated UI or barplot and Density and ability to suppress label legend
* UI updates to only show the relevant options for pairs plot and histogram/barplot
* Automatic color theme switching to avoid not enough color or fill errors
* switched to github actions

# ggquickeda 0.1.8
* Added possibility to use more than one x variable(s) and or no y variable(s)
* Added option for an explicit Missing Category with custom cuts
* Added option to compute the inverse of Numeric variables
* Moved the rounding to after division/inverse
* Added possibility to round data labels before plotting
* Added a blank shape type so we can do multiple y(s) when some layers do not have a point plotted.
* Added Docker configuration 

# ggquickeda 0.1.7
* Added support for semicolon separated files and added package version in App Title
* Added `geom_rug` support for x/y and for additional variable(s)
* Added `geom_ribbon` for mean/CI and `geom_errorbar` for median/PI (with UI updates)
* Enable dodge for mean/CI and median/PI to take into account the specified width
* Updated UI for facets options and added controls for bold, angle and justification
* Allowing specifying `multi_line=TRUE` for all facet labellers
* Added theme options for `strip.switch.pad.grid` and `strip.switch.pad.wrap`
* Added possibility to completely remove the facet strips background and outline
* Updated UI for legends options and added controls for legend/legend box, justification and margins
* Added control for `legend.spacing.y`
* Added continuous x/y scale(s) asymmetrical expansion values
* Added more position adjustments for points (e.g. dodge)
* Minor enhancements for correlation coefficients
* Updated UI for theme options
* Added support for the `break.time.by` argument for risk table, enhanced the K-M UI and added inputs for `conf.type` and `conf.lower`
* Fixed code generation issues by adding dependencies
* Bumped the minimum R version to 3.6.0 and the minimum ggplot version to 3.3.1
* Refreshed vignettes and readme screenshots

# ggquickeda 0.1.6
* withheld gradientInput addition (shinyjqui issue)
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
