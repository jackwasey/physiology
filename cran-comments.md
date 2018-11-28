New functions and tests from contributor.

## Test environments
* local OS X install, R 3.5.1
* ubuntu 14.04 (on travis-ci), R 3.5.1, R-devel
* appveyor Windows Server 2012 R2 x64, R version 3.5.1 Patched
* r-hub Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* win-builder

## R CMD check results

Found the following (possibly) invalid URLs:
  URL: http://www.jstor.org/stable/1692001 (moved to https://www.jstor.org/stable/1692001)
    From: inst/doc/Everest.html
    Status: 403
    Message: Forbidden
  URL: http://www.jstor.org/stable/20511613
    From: inst/doc/Everest.html
    Status: 403
    Message: Forbidden
  URL: https://doi.org/10.2307/1782342
    From: inst/doc/Everest.html
    Status: 403
    Message: Forbidden
    
These URLs all work for me, on and off an academic network. The first JSTOR link
is automatically generated with 'http', but does work on redirect.

0 errors | 0 warnings | 0 notes
