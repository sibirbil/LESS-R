### Resubmission

> Please write full names in the "Authors" and "Maintainer" field.

  The maintainer's name is written explicitly as Burhan Ozer Cavdar
  
> Please add a few small executable examples in your Rd-files to
illustrate the use of the exported function but also enable automatic
testing.
  
  Small examples are added to those exported functions with no examples using roxygen2
  
> You write information messages to the console that cannot be easily
suppressed... e.g.:  R/helper-classes.R ; R/helper-functions.R
  
  All print/cat functions are removed in R/helper-classes.R and R/helper-functions.R
  
> Please make sure that you do not change the user's options, par or
working directory... e.g.: R/tutorial.R
  
  Functions with par() are corrected by using on.exit() function. So, user's options are reset
  when the function is exited.
  
  
### Resubmission

> License components with restrictions and base license permitting such:
  MIT + file LICENSE
File 'LICENSE':
  MIT License
  
  License files has been restructured

> Possibly misspelled words in DESCRIPTION:
  Regressor (2:8)
  
  Misspelling is corrected

> Found the following (possibly) invalid URLs:
  URL: http://archive.ics.uci.edu/ml (moved to http://archive.ics.uci.edu/ml/)
    From: man/abalone.Rd
    Status: 200
    Message: OK
  URL: http://hunch.net/~jl/projects/cover_tree/cover_tree.html (moved to https://hunch.net/~jl/projects/cover_tree/cover_tree.html)
    From: man/CoverTree.Rd
    Status: 200
    Message: OK
    
  URLs are corrected

> The Description field should not start with the package name,
  'This package' or similar.
  
  The beginning of the Description field is corrected
  
> The Description field contains
  in our manuscript at https://arxiv.org/abs/2112.06251.
Please write arXiv ids as <arXiv:YYMM.NNNNN>.

  arXiv style is corrected
  
> checking examples ... [31s] NOTE
Examples with CPU (user + system) or elapsed time > 10s
                user system elapsed
LESSClassifier 12.45   0.08   12.58

  The example of LESSClassifier is made smaller.
  

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 2 NOTES:

* checking dependencies in R code ... NOTE
  Namespace in Imports field not imported from: 'R6'

  R6 is a build-time dependency.
  
* checking CRAN incoming feasibility ... [3s/4s] NOTE
  Maintainer: ‘Burhan Ozer Cavdar <bcavdar17@ku.edu.tr>’

  New submission
