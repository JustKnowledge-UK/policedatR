# policedatR

An R package for acquiring, augmenting and analysing stop and search data from the UK Police API.

## Installation

In its current form policedatR requires an RTools installation. The simplest way of installing RTools is to use the package 'installr':

```
# check if you already have installr installed and install if not.
packages <- c("installr")
pkg_not_install <- packages[!(packages %in% installed.packages()[,"Package"])]
lapply(pkg_not_install, install.packages, dependencies = TRUE)
lapply(packages, library, character.only = TRUE)

# run install.rtools. This will check for installation and update to latest
installr::install.Rtools()
```

Then use the devtools package to install policedatR

```
# check if you already have devtools installed and install if not.
packages <- c("devtools")
pkg_not_install <- packages[!(packages %in% installed.packages()[,"Package"])]
lapply(pkg_not_install, install.packages, dependencies = TRUE)
lapply(packages, library, character.only = TRUE)

# install policedatR
devtools::install_github("JustKnowledge-UK/policedatR")

# add policedatR to library
library(policedatR)
```

## Notes on licencing

Data acquired using policedatR is published by data.police.uk under [Open Government Licence v3.0](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/)
policedatR makes use of lookups and boundary data from the Office for National Statistics [Open Geography Portal](https://geoportal.statistics.gov.uk/). These data are also licenced [Open Government Licence v3.0](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/)
