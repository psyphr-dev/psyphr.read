
<!-- README.md is generated from README.Rmd. Please edit that file -->

# psyphr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/psyphr)](https://cran.r-project.org/package=psyphr)
<!-- badges: end -->

The `psyphr` R package tranforms
[psychophysiological](https://en.wikipedia.org/wiki/Psychophysiology)
data from commercial data collection and processing systems into a
[tidy](http://vita.had.co.nz/papers/tidy-data.pdf) form for downstream
analysis and visualization.

### WIP

The package is currently under active development.

Currently, the package supports data outputs from the the following
corresponding [MindWare Technologies](https://www.mindwaretech.com/)
applications in version 3.2:

  - [Electrodermal Activity
    (EDA)](https://support.mindwaretech.com/manuals/software/eda/3-2/)

  - [Heart Rate Variability
    (HRV)](https://support.mindwaretech.com/manuals/software/hrv/3-2/)

  - [Electromyography
    (EMG)](https://support.mindwaretech.com/manuals/software/emg/3-2/)

  - [Blood Pressure Variability
    (BPV)](https://support.mindwaretech.com/manuals/software/bpv/3-2/)

  - [Impedance Cardiography
    (IMP)](https://support.mindwaretech.com/manuals/software/imp/3-2/)

  - 
    
    <del>
    
    [Basic Signal Analysis
    (BSA)](https://support.mindwaretech.com/manuals/software/bsa/3-2/)
    
    </del>
    
    (No support currently)

## Installation

Install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("wendtke/psyphr")
```

## Usage

Load and attach `psyphr`.

``` r
require(psyphr)
```

### Read a Single File

### Compile a Study

#### Directory Structure

### Exploratory Analysis

## TODO

The development team considers the following for the package’s future:

  - Submit `psyphr` to [rOpenSci](https://ropensci.org/) for peer review
    and publication
  - Customizable data QA utilities with meaningful defaults
  - Common, publication-grade visualization schemes
  - Study compilation utility
  - Compatibility on data feeds from other popular data collection and
    processing systems (e.g., [BIOPAC](https://www.biopac.com/))

## Acknowledgements

`psyphr` was founded at the [Uncoast
Unconf 2019](https://github.com/uncoast-unconf/uu-2019) with the help of
[Amanda Gadrow](https://github.com/ajmcoqui) and [Audrey
McCombs](https://github.com/almccombs). Travel to the Uncoast Unconf was
supported by Colorado State University (Department of Human Development
and Family Studies and The Graduate School) and the Uncoast Unconf
organizing committee, particularly [Dr. Sam
Tyner](https://sctyner.github.io/).

This material is based upon work supported by the National Science
Foundation Graduate Research Fellowship Program under Grant
No. 006784-00002 \[to KEW\]. Any opinions, findings, and conclusions or
recommendations expressed in this material are those of the authors and
do not necessarily reflect the views of the National Science Foundation.
