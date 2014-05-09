# STATXP

- **Author:** Guillaume T. Vallet, gtvallet@gmail.com, Université de Montréal, CRIUGM
- **Version:** 0.1
- **Date:** 2014/05/08

*Statxp* is R package developed for a personal use to improve some experimental analysis processing. 
*Statxp* offers a set of functions to clean data, export and format results into a [knitr](http://yihui.name/knitr/)/[sweave](http://www.stat.uni-muenchen.de/~leisch/Sweave/) document.


## Table of Contents

- [Licence](#licence)
- [Dependencies](#dependencies)
- [Installation](#install)
- [Functions](#functions)


## <a name='licence'></a>Licence

This package is released under the [Creative Common Attribution-NonCommercial-ShareAlike 4.0 International](http://creativecommons.org/licenses/by-nc-sa/4.0/) license.


## <a name='dependencies'></a>Dependencies

*Predata* depends on the ``plyr`` package. 
You can install *plyr* by typing ``install.packages('plyr')`` in your R console.


## <a name='install'></a>Installation

To install a R package from Github, you first need to install the devtools package.
In R, type ``install.packages('devtools')``. 
Then install *statxp* with the following command : ``install_github('Cogitos/statxp')``.
And now enjoy the package!


## <a name='functions'></a>Functions

### filtRT & outliers

These two functions are designed to filter reaction times (or other dependent variables) by thresholds (minimum and/or maximum values) and/or standard deviations from the mean. 
These functions return the cleaned data frame as well as how many data were exclude by kind of filter.

The difference between *filtRT* and *outliers* is that filtRT uses outliers to filter the data base on different conditions (e.g. by subject and independent variables).

### faov, ftt & fmsd

These functions allow to easily format for knitr/sweave results from an ANOVA (faov, computed with the [ez](http://cran.r-project.org/web/packages/ez/index.html) package), a *t* test of Student (ftt) and simple a mean with its related standard deviation (fmsd).

The formatting is done according to the [APA](http://www.apastyle.org/) guidelines.
Different options allow to add the*d* of Cohen, change the format, etc.

### writeAOV

This function was designed to easily export an ANOVA result (from the ez package) into a csv file.
The options allow to add descriptive statistics and/or posthoc analysis.


