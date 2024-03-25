# Mar 27/ Apr 3: Analysis
### Description, inference, and linear association

Step beyond data wrangling and visualization and into the world of statistical analysis. Can we use the data we cleaned, collected, and described to answer empirical questions? Part 1 covers analysis of numeric variables: summary stats, difference of means, ANOVA, and linear regression. Part 2 will cover interaction effects, categorical predictors, tabular analysis, and transformations.

## Prep for class
- In case you want a basic refresher, see Chs 5-7 of the [SIS 600 Survival Guide](https://austin-hart-pols.github.io/SurvivalGuide/)  
- See also Ch 7 of Winston Chang's [Cookbook for R](http://www.cookbook-r.com/)  
- Data for class: `qog23ex.rdata`  
- Our [750 Stats Crash Course](https://sis750.github.io/10-11-analysis/) for class these two weeks.
- See `index.Rmd` for example of `downcute` doc from the `rmdformats` package

Consider using the chunk below as the YAML for `downcute` docs:

```yaml 
---
title: "Stats crash course"
date: "`r Sys.Date()`"
author: "Austin Hart"
output:
  rmdformats::downcute:
    self_contained: true
    code_folding: show
    use_bookdown: true
    thumbnails: false
    default_style: "dark"
    downcute_theme: "default"
---
```

## Assessment
- Problem Set 6: Post to `github` by April 10.

