
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BusinessObjects 📊

<!-- badges: start -->

<!-- badges: end -->

This packages is an API wrapper of the SAP Business Objects API for the
R language. It allows to connect R to SAP Business Objects to automate
and build multiple workflows from within R.

### Installation

You can install the latest release of this package from
[Github](https://github.com/matbmeijer/JirAgileR) with the following
commands in `R`:

``` r
if (!require("devtools")) install.packages("devtools")
devtools::install_github("matbmeijer/BusinessObjects")
```

### Current functionalities 🔧

  - Log-on to SAP Business Objects REST API: `log_on()`
  - Retrieve children elements from a CMS repository folder:
    `get_bo_folder_ressources()`
  - Retrieve details from a SAP Business Objects document ID:
    `get_bo_document_details()`
  - Retrieve schedules from a SAP Business Objects document ID:
    `get_bo_document_schedules()`
  - Retrieve details (frequency, parameters, etc.) of a specific
    schedule from a SAP Business Objects document ID:
    `get_bo_schedule_details()`
  - Upload Microsoft Excel to the SAP Business Objects’ CMS Repository:
    `upload_excel_to_bo()`
  - Update a Microsoft Excel located in te SAP Business Objects’ CMS
    Repository: `update_bo_excel_file()`
  - Retrieve SAP Business Objects connections: `get_bo_connections()`
  - Retrieve SAP Business Objects configurations:
    `get_bo_configuration()`
  - Retrieve SAP Business Objects universes: `get_bo_universes()`
  - obtain the details of a SAP Business Objects universe:
    `get_bo_universe_details()`
  - Log-off from the SAP Business Objects REST API: `log_off()`

### Documentation

  - Full R package documentation ca be found here:
    <https://matbmeijer.github.io/BusinessObjects>

  - Further documentation about the SAP BusinessObjects API can be found
    here: [Documentation for the WEB REST
    API](https://help.sap.com/viewer/58f583a7643e48cf944cf554eb961f5b/4.2/en-US/7da2e2d66f701014aaab767bb0e91070.html)

### Code of Conduct

Please note that the ‘BusinessObjects’ project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to
this project, you agree to abide by its terms.

### License

This package will probably never see CRAN as SAP Business Objects is not
open source. Nonetheless, all the code here follows the following
licensing: [MIT © Matthias
Brenninkmeijer](https://github.com/matbmeijer/BusinessObjects/blob/master/LICENSE.md)
