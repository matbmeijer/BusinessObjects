
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BusinessObjects

<!-- badges: start -->

<!-- badges: end -->

**🚧 UNDER CONSTRUCTION**

This packages is an API wrapper of the SAP Business Objects API for the
R language. This package will probably never see CRAN as SAP Business
Objects is not Open Source.

### Current functionalities

  - Log-on to SAP Business Objects REST API: `log_on()`
  - Retrieve schedules from a SAP Business Objects document ID:
    `get_bo_document_schedules()`
  - Retrieve details (frequency, parameters, etc.) of a specific
    schedule from a SAP Business Objects document ID:
    `get_bo_schedule_details()`
  - Upload Microsoft Excel to the SAP Business Objects CMS Repository:
    `upload_excel_to_bo()`
  - Retrieve SAP Business Objects connections: `get_bo_connections()`
  - Retrieve SAP Business Objects configurations:
    `get_bo_configuration()`
  - Retrieve SAP Business Objects universes: `get_bo_universes()`
  - Log-off from the SAP Business Objects REST API: `log_off()`

[Documentation for the WEB
API](https://help.sap.com/viewer/58f583a7643e48cf944cf554eb961f5b/4.2/en-US/7da2e2d66f701014aaab767bb0e91070.html)
