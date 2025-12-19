# The energy, material and carbon handprint of lithium-ion batteries in electric vehicles - A US fleet model

Folders:

* **Spatial Join**: Do spatial joins for spatial data at different resolutions: join made at county level.
* **Prepare-DataInputs**: Scripts to load and process original data sources into model inputs.
* **LCA Model**: Scripts to run the Life Cycle Assessment (LCA) model.

Scripts:

All scripts are numbered in order suggested to run to replicate full results. Code is to replicate main figures for the article.

* **00-Libraries.R**: Load required R libraries.
* **01-CommonVariables.R**: Load common variables and dimensions to use in the model and figures.
* **02-Load_Results.R**: Common script to load model results for the figure creations.
* **02b-Scenario_Load_Results.R**: Common script to load model results for the figure creations (different format).

Code to replicate Figures:

* **Fig2-Energy.R**: Primary energy consumption.
* **Fig3-Materials.R**: Materials requirements. 
* **Fig4-CO2.R**: Carbon emissions.
* **Fig5-Handprint.R**: Handprint (avoided emissions) for lithium-ion batteries and lithium.

Supplementary Information Figures:

* **FigSI_OtherImpacts.R**: Figure for other impacts: ozone depletion, acidification, human health (air particulate matter) and smog formation.
* **FigSI_Cambium.R**: Using Cambium Long Term Marginal Electricity Emissions factors.
* **FigSI_GREET.R**: Uisng GREET impact factors for upstream manufacturing impacts.
