
<!-- README.md is generated from README.Rmd. Please edit that file -->

# atable

![travis](https://travis-ci.com/aghaynes/atable.svg?branch=master)
[![codecov](https://codecov.io/github/aghaynes/atable/branch/master/graphs/badge.svg)](https://codecov.io/github/aghaynes/atable)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/atable)](https://cran.r-project.org/package=atable)

The `atable` package supports the analysis and reporting of controlled
clinical trials. Reporting of clinical trials is such a frequent task
that guidelines have been written which recommend certain properties of
clinical trial reports (Moher et al. (2010)). In particular Item 17a of
CONSORT states that “*Trial results are often more clearly displayed in
a table rather than in the text*”. And Item 15 suggests: “*a table
showing baseline demographic and clinical characteristics for each
group*”. The `atable` package is specifically designed to comply with
these two items.

## Using `atable`

Load the package

``` r
library(atable)
# remotes::install_github("arminstroebel/atable") # development version
```

We will use the `arthritis` data set to demonstrate the features of
`atable`, but as all variables are numeric in `arthritis`, we add some
other variable types.

``` r
data(arthritis, package = "multgee")
arthritis <- within(arthritis, {
  score <- ordered(y)
  baselinescore <- ordered(baseline)
  time <- paste0("Month ", time)
  sex <- factor(sex, levels = c(1,2), labels = c("female", "male"))
  trt <- factor(trt, levels = c(1,2), labels = c("placebo", "drug"))
  date <- as.Date("2016-03-09") + runif(nrow(arthritis), -300, 300)
  })
```

To create a summary table of sex and age:

``` r
atable_options(format_to = "console") # more on this in a moment
atable(arthritis, target_cols = c("sex", "age"))
##   Group                value    
## 1 Observations                  
## 2                      906      
## 3 sex                           
## 4      female          27% (249)
## 5      male            73% (657)
## 6      missing         0% (0)   
## 7 age                           
## 8      Mean (SD)       50 (11)  
## 9      valid (missing) 906 (0)
```

We can also get statistics grouped by some variable (e.g. a treatment
indicator):

``` r
atable(arthritis, target_cols = c("sex", "age"), group_col = "trt")
## Warning in stats::ks.test(x, y, alternative = c("two.sided"), ...): p-value will be approximate in
## the presence of ties
##   Group                placebo   drug      p     stat  Effect Size (CI)    
## 1 Observations                                                             
## 2                      447       459                                       
## 3 sex                                                                      
## 4      female          29% (129) 26% (120) 0.4   0.71  1.1 (0.85; 1.6)     
## 5      male            71% (318) 74% (339)                                 
## 6      missing         0% (0)    0% (0)                                    
## 7 age                                                                      
## 8      Mean (SD)       51 (11)   50 (11)   0.043 0.092 0.058 (-0.072; 0.19)
## 9      valid (missing) 447 (0)   459 (0)
```

Furthermore, we can split by another variable
(e.g. timepoints):

``` r
atable(arthritis, target_cols = c("sex", "age"), group_col = "trt", split_cols = "time")
## Warning in stats::ks.test(x, y, alternative = c("two.sided"), ...): p-value will be approximate in
## the presence of ties

## Warning in stats::ks.test(x, y, alternative = c("two.sided"), ...): p-value will be approximate in
## the presence of ties

## Warning in stats::ks.test(x, y, alternative = c("two.sided"), ...): p-value will be approximate in
## the presence of ties
##    Group                     placebo   drug      p    stat  Effect Size (CI)   
## 1  Month 1                                                                     
## 2       Observations                                                           
## 3                            149       153                                     
## 4       sex                                                                    
## 5            female          29% (43)  26% (40)  0.69 0.16  1.1 (0.67; 2)      
## 6            male            71% (106) 74% (113)                               
## 7            missing         0% (0)    0% (0)                                  
## 8       age                                                                    
## 9            Mean (SD)       51 (11)   50 (11)   0.55 0.092 0.058 (-0.17; 0.28)
## 10           valid (missing) 149 (0)   153 (0)                                 
## 11 Month 3                                                                     
## 12      Observations                                                           
## 13                           149       153                                     
## 14      sex                                                                    
## 15           female          29% (43)  26% (40)  0.69 0.16  1.1 (0.67; 2)      
## 16           male            71% (106) 74% (113)                               
## 17           missing         0% (0)    0% (0)                                  
## 18      age                                                                    
## 19           Mean (SD)       51 (11)   50 (11)   0.55 0.092 0.058 (-0.17; 0.28)
## 20           valid (missing) 149 (0)   153 (0)                                 
## 21 Month 5                                                                     
## 22      Observations                                                           
## 23                           149       153                                     
## 24      sex                                                                    
## 25           female          29% (43)  26% (40)  0.69 0.16  1.1 (0.67; 2)      
## 26           male            71% (106) 74% (113)                               
## 27           missing         0% (0)    0% (0)                                  
## 28      age                                                                    
## 29           Mean (SD)       51 (11)   50 (11)   0.55 0.092 0.058 (-0.17; 0.28)
## 30           valid (missing) 149 (0)   153 (0)
```

The same can be achieved via the formula interface:

``` r
atable(sex + age ~ trt | time, arthritis)
```

Here, the left hand side represents the variables being summarized. The
right hand side gives the variables being used to group and split the
variables. The variables used to split come after a `|` character

### Output format

In an earlier code chunk, we set `atable_options(format_to =
"Console")`. `atable` is designed to return output in a format optimized
for LaTeX, the console, MS Word (via `flextable` and `officer`), HTML or
raw (i.e. no formatting). This is for easy use in manuscripts.

``` r
form <- sex + age ~ trt
atable(form, arthritis, format_to = "Latex")   # format to LaTeX
atable(form, arthritis, format_to = "Console") # format to console
atable(form, arthritis, format_to = "HTML")    # format to HTML
atable(form, arthritis, format_to = "Raw")     # no formatting
atable(form, arthritis, format_to = "Word")    # format to MS Word
```

## Modifying `atable`

If statistics other than the default ones are required, if it possible
to return others by changing the functions `atable` uses to create
summary statistics, tests, effect measures and formatting.

Here is an example to calculate median, MAD, mean and SD (requires
argument `x` and returns a named list with class `statistics_'class'`:

``` r
new_statistics_numeric <- function(x, ...){
  statistics_out <- list(Median = median(x, na.rm = TRUE), 
                         MAD = mad(x, na.rm = TRUE),
                         Mean = mean(x, na.rm = TRUE),
                         SD = sd(x, na.rm = TRUE))
  class(statistics_out) <- c("statistics_numeric", class(statistics_out))
  # We will need this new class later to specify the format
  return(statistics_out)
}
```

The suitable formatting function:

``` r
new_format_statistics_numeric <- function(x, ...){
  Median_MAD <- paste(round(c(x$Median, x$MAD), digits = 1), collapse = "; ")
  Mean_SD <- paste(round(c(x$Mean, x$SD), digits = 1), collapse = "; ")
  out <- data.frame(tag = factor(c("Median; MAD", "Mean; SD"), 
                                 levels = c("Median; MAD", "Mean; SD")),
                    # use levels to retain the order of the rows 
                    value = c(Median_MAD, Mean_SD),
                    stringsAsFactors = FALSE)
  return(out)
}
```

And a test function to compute both t-tests and Kolmogorov-Smirnov
tests:

``` r
new_two_sample_htest_numeric <- function(value, group, ...){
  d <- data.frame(value = value, group = group)
  group_levels <- levels(group)
  x <- subset(d, group %in% group_levels[1], select = "value", drop = TRUE)
  y <- subset(d, group %in% group_levels[2], select = "value", drop = TRUE)
  ks_test_out <- stats::ks.test(x, y)
  t_test_out <- stats::t.test(x, y)
  out <- list(p_ks = ks_test_out$p.value,
              p_t = t_test_out$p.value )
  return(out)
}
```

These can then be passed to `atable` for use in the table:

``` r
atable(sex + age ~ trt | time, arthritis,
       statistics.numeric = new_statistics_numeric,
       format_statistics.statistics_numeric = new_format_statistics_numeric,
       two_sample_htest.numeric = new_two_sample_htest_numeric)
## Warning in stats::ks.test(x, y): p-value will be approximate in the presence of ties

## Warning in stats::ks.test(x, y): p-value will be approximate in the presence of ties

## Warning in stats::ks.test(x, y): p-value will be approximate in the presence of ties
##    Group                  placebo    drug      p    stat Effect Size (CI) p_ks p_t 
## 1  Month 1                                                                         
## 2       Observations                                                               
## 3                         149        153                                           
## 4       sex                                                                        
## 5            female       29% (43)   26% (40)  0.69 0.16 1.1 (0.67; 2)             
## 6            male         71% (106)  74% (113)                                     
## 7            missing      0% (0)     0% (0)                                        
## 8       age                                                                        
## 9            Median; MAD  55; 10.4   53; 10.4                             0.55 0.61
## 10           Mean; SD     50.7; 11.2 50.1; 11                                      
## 11 Month 3                                                                         
## 12      Observations                                                               
## 13                        149        153                                           
## 14      sex                                                                        
## 15           female       29% (43)   26% (40)  0.69 0.16 1.1 (0.67; 2)             
## 16           male         71% (106)  74% (113)                                     
## 17           missing      0% (0)     0% (0)                                        
## 18      age                                                                        
## 19           Median; MAD  55; 10.4   53; 10.4                             0.55 0.61
## 20           Mean; SD     50.7; 11.2 50.1; 11                                      
## 21 Month 5                                                                         
## 22      Observations                                                               
## 23                        149        153                                           
## 24      sex                                                                        
## 25           female       29% (43)   26% (40)  0.69 0.16 1.1 (0.67; 2)             
## 26           male         71% (106)  74% (113)                                     
## 27           missing      0% (0)     0% (0)                                        
## 28      age                                                                        
## 29           Median; MAD  55; 10.4   53; 10.4                             0.55 0.61
## 30           Mean; SD     50.7; 11.2 50.1; 11
```

(See Ströbel (2019) for passing methods via `atable_options` and
changing `atable`’s namespace)

## Extending `atable`

`atable` only has methods for numeric, factor and ordered variables but
it possible to extend `atable`s functionality by defining methods for
other classes (e.g. `Date` or `surv`).

Here is an example for `surv` objects, while `vignette("atable_usage",
package = "atable")` contains an example with `Date`s.

Define the statistics and testing functions:

``` r
statistics.Surv <- function(x, ...){
  survfit_object <- survival::survfit(x ~ 1)
# copy from survival:::print.survfit:
  out <- survival:::survmean(survfit_object, rmean = "common")
  return(list(mean_survival_time = out$matrix["*rmean"],
              SE = out$matrix["*se(rmean)"]))
}

two_sample_htest.Surv <- function(value, group, ...){
  survdiff_result <- survival::survdiff(value~group, rho=0)
  # copy from survival:::print.survdiff:
  etmp <- survdiff_result$exp
  df <- (sum(1 * (etmp > 0))) - 1
  p <- 1 - stats::pchisq(survdiff_result$chisq, df)
  return(list(p = p,stat = survdiff_result$chisq))
}
```

The `ovarian` dataset in the `survival` package has a suitable example…

``` r
library(survival)
# set classes
ovarian <- within(survival::ovarian, 
                  {time_to_event = survival::Surv(futime, fustat)})
# create the table
atable(ovarian, target_cols = c("time_to_event"), group_col = "rx")
##   Group                   1   2   p   stat
## 1 Observations                            
## 2                         13  13          
## 3 time_to_event                           
## 4      mean_survival_time 650 889 0.3 1.1 
## 5      SE                 120 115
```

### References

<div id="refs" class="references">

<div id="ref-Moher2010">

Moher, D., S. Hopewell, K. F Schulz, V. Montori, P. C Gotzsche, P J
Devereaux, D. Elbourne, M. Egger, and D. G Altman. 2010. “CONSORT 2010
Explanation and Elaboration: Updated Guidelines for Reporting Parallel
Group Randomised Trials.” *BMJ* 340 (mar23 1): c869–c869.
<https://doi.org/10.1136/bmj.c869>.

</div>

<div id="ref-stroebel2019">

Ströbel, Armin. 2019. “Atable: Create Tables for Clinical Trialreports.”
*The R Journal* 11 (1): 137–48.
<https://journal.r-project.org/archive/2019/RJ-2019-001/RJ-2019-001.pdf>.

</div>

</div>
