# EuroFlow Modelling paper (working title)
## Background
This study extends the work by T. Maavara ([2017](https://uwspace.uwaterloo.ca/bitstreams/913ea8ce-6189-48fa-9cfb-6dc0af6f3ccc/download), [2020](https://idp.nature.com/authorize/casa?redirect_uri=https://www.nature.com/articles/s43017-019-0019-0&casa_token=dpH48pIrrf0AAAAA:_HYPQfPccu38GWoLdLyiKPrFNCkc24pAU_XzmnmVhPW3PyZh72HsVTQ7x0jmbpaKf55WnLc-0zIiMbRr)).
The aim is to assess nutrient cycling in European dams under various environmental flow management scenarios. This repository shall contain all scripts necessary to preprocess input data, perform the modelling and the final analyses.

## Contents
The following details the contents of this repo and the modelling steps performed by individual scripts.
### Modelling scripts:
- routing_boxmodel_seasT_resOP.R: defines one function called routing() to route water <ins>and</ins> nutrients
- filter_catchment: defines one function called filter_catchment() for pulling and matching basins with subbasins/dams
### Analysis and processing scripts:
- main_analysis.R: pre-process all modelled variables for analyses and plotting
- wet_dry.R: creates a matrix with 0, 1, 2 indicating dry (1) and wet (2) days (wet_dry_season.RDS)

## Procedure
![Image](https://github.com/user-attachments/assets/bb5e5c0f-b375-4154-9475-3e77e65ba965)

