Please read the How to to make sure you are using the app as intented.
Contact me @ samermouksassi@gmail.com for feedback or use the link to file bugs/features/pull requests!
<a href="https://github.com/smouksassi/ggquickeda/issues" target="_blank">Click Here to go to github</a>

1. Upload your data file in CSV format. R default options for `read.csv` will apply except for missing values where both (NA) and dot (.) are treated as missing. If your data has columns with other non-numeric missing value codes then they be treated as factors.

2. The UI is dynamic and changes depending on your choices of options, x ,y, filter, group and so on.

3. It is assumed that your data is tidy and ready for plotting (long format). Whether you select one or more y variable(s), the app will automatically stack the data using `tidyr::gather` and result in yvars and yvalues variables. The internal variable yvalues is used for y variable mapping and the app automatically split the plot by selecting additional row split as `yvars` and by setting Facet Scales to `free_y`. The user can override this by specifying different facets splits and Facet Scales options as he sees fit. To change Facet Scales and many other options go to **Graph Options tab**.

4. x and y variable(s) input allow numeric and factor variables.

5. If you select a mix of factor and continuous variables for the y variables, all selected variables will be transformed to factor.

6. The **Inputs** tab allow the user to do many data operations on the data before the plotting.
    + In the **Categorize/Rename** sub-tab:    
    Use ***Recode into Binned Categories*** field to select the numeric variable to change to categorical in the list and then choose a number of cuts (default is 2). This helps when you want to group or color by cuts of a continuous variable.  
    Use ***Treat as Categories*** field to change a numeric variable to factor without binning. This helps when you want to group or color by numerical variable that has few unique values e.g. 1,2,3.  
    Use ***Custom cuts of this variable...*** to cut a numeric variable to factor using specified cutoffs, by default a text input is created and  comma separated min, median, max are used. You can override with any values of your liking. An optional  ***Treat as Numeric*** checkbox can be selected to recode the created custom cuts variable to numeric values that start with 0. This can be useful to overlay a logistic smooth or to use a Kaplan-Meier curve. Below the check box the created categories and numeric codes pair( if applicable) are shown for reference.  
    The *Change labels of this variable* is now dynamic!. This field allows you to select as many factor/character variable(s) as you want in order to input in a comma separated list of new labels. By default, value1, value2,...valuen will be populated in the field. Make sure to edit/type keeping the correct number of new labels otherwise the plot will not be generated as the recoding will fail with this message `Error: number of levels differs`. You can combine levels by repeating a value with special attention to spaces e.g. 1,1,2. You can also include line breaks using `\n`.
    + In the **Combine Variables** sub-tab:   
    The ***Combine the categories of these two variables*** field allows you to select two categorical variables (other than those used for y variables). A new variable will then be generated with values of the two pasted together. e.g. you select variable `Sex` with values: Male and Female and variable `Treatment` with values: TRT1, TRT2 and TRT3), the new variable will be named `Sex_Treatment` with values: Male TRT1, Male TRT2, Male TRT3, Female TRT1, Female TRT2, Female TRT3. The plot will reset any color/fill/grouping mapping and the `Sex_Treatment` will  become available to color, group and  other mappings.  
    + In the **Filters** sub-tab:  
    There is six slots for Filter variables. ***Filter variable (1), (2),(3)*** and ***Filter continuous (1), (2),(3)***
    The Filters are applied sequentially. Values shown for ***Filter variable (2)*** will depend on your selected data exclusions using ***Filter variable (1)*** and so on. The first three filters accept numeric and non numeric columns while the last three are sliders and only work with numeric variables. To avoid potential performance issues, the first three filters only show variables with a default ***maximum number of levels*** of 500 and you can adjust it to your needs.  
    + In the **Simple Rounding** sub-tab:   
    The ***Round the Values to the Specified N Digits*** field allows you to round a numeric variable using the specified ***N digits*** (defaults to 0). This convert values like 1.11, 1.12, 2.1 to 1, 1, 2. Thic can be a quick and easy way to group/bin continuous values for better viewing in the plot.  
    + In the **Reorder Variables** sub-tab:  
    There is a group of options to allow you to reorder a categorical variable is several ways. ***Reorder This Variable:*** allows you to select and reorder a categorical variable ***By the:***  `Median`,`Mean`,`Minimum` or `Maximum` of ***Of this Variable:*** where you pick a numerical variable of your choice the default is to use the `Median` of `yvalues`. You can also check the ***Reverse Order ?*** box to have the order inverted. The ***Custom Reorder this variable*** will create an input field with unique values of the selected variable and then you can drag and drop the values to the order of your choice. Finally the  ***Change labels of this variable:*** is an additional place where you can change the names of the levels of a character variable like the one holding the names of the selected y variables `yvars` which is handy if you want to change the names in the facet labels.  
    + In the **NEW** **One Row by ID** sub-tab:  
    The ***Filter to One Row by ID ?*** field if checked will let you filter your data to the first row by your grouping ***ID*** variable. This is useful for variables that do not change across time by ID to make a boxplot or a table.


7. The **X/Y Plot** tab is the main tab where the plot is shown and where the user can control which features and layers to include.  
    + The ***Plot types, Points, Lines (?)*** sub-tab has options for `Points`, `Jitter` and `Lines` (optionally grouped) and with the user ability to control shapes, line types, transparency, and overriding mapped colors or sizes.  
    + The ***Color/Group/Split/Size/Fill Mappings (?)*** sub-tab has options for mapping `Color`, `Group` and `Split` (faceting in ggplot jargon),`Color` and `Fill`. Splits can be done up to two levels by column and/or by row. The user is encouraged to experiment to get familiar with the various options and capabilities.  
    + The ***Boxplots*** sub-tab brings limited boxplots support. Carefully choose grouping expecially when the x axis variable is continuous, you can change the **Group By:*** variable in the ***Color/Group/Split/Size/Fill Mappings (?)*** to better reflect your needs.  
    + The ***Histogram/Density*** sub-tab is relevant when no y variables are selected and when the x variable is continuous. It allows you to plot density and or histogram.  
    + The ***Quantile regression (?)*** sub-tab contains options pertaining to smoothed quantile regression.  
    + To include a trend line experiment with ***Smooth/Linear/Logistic Regressions (?)***. Make sure that data is compatible with the smoothing method used for example the ***Smoothing Method*** `logistic` expects a numerical 0/1 variable.  
    + The ***Mean CI (?)*** and ***Median PIs (?)*** sub-tabs contain options to add `Mean with confidence intervals` and `Median with percentiles intervals`. The ***Median PIs (?)*** also contains two checkboxes to add the median and or the number of data points to the blot as a label.  
    + It is  possible to join the boxplots with a line connecting medians percentiles or means, for this to work first uncheck the ***Ignore Mapped Group *** in the corresponding menu and make sure to have **Group By:*** and **Color By:*** both set to `None` or to the same variable in the ***Color/Group/Split/Size/Fill Mappings (?)*** sub-tab. 
    +  The ***Kaplan-Meier*** sub-tab adds support to enable Kaplan-Meier (K-M) plots with confidence intervals, censoring ticks and commonly used transformations. When K-M curves are added nothing else is shown on the plot.

8. The **Export Plots** tab has was initially based on Mason DeCamillis ggplotLive app and then completely re-written by <a href="https://github.com/daattali" target="_blank">Dean Attali</a>. Once a plot is saved in the **X/Y Plot** tab by providing a name and hitting the **Save plot** star button it will become available here. You can export in portrait, landscape and multiple plots per page.
9. The **Plot Code** tab added the ability to view the source code of the plot. This, in addition to refactoring a big portion of the initial code and adding on the fly insert UI  dynamic features, were also contributed by Dean Attali.

10. Look at the data used in the actual plot in the **Data** tab. You can reorder the columns, filter and much more.  

11. **NEW** <a href="https://github.com/benjaminrich" target="_blank">Benjamin Rich </a> table1 package powers the summary statitics tab. More on this soon.

12. Copy Paste Bonus ≤ ≥ µ m² ³
    HTML: &le; &ge; &mu; &plusmn; &deg; <sup>superscript</sup>

*Samer Mouksassi 2018*
