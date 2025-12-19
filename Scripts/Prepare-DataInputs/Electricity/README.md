# The energy, material and carbon handprint of lithium-ion batteries in electric vehicles - A US fleet model

Scripts to process data regarding environmental impacts from the **Electricity Generation** stage.

All scripts are numbered in order suggested to run to replicate full results.

* **01-EIA_GetData_API.R**: Download EIA (Energy Information Administration) Electricity Data from the Annual Energy Outlook. 
* **02-EIA_Data.R**: Load EIA Electricity Data and convert to useful formats.
* **02b-Cambium.R**: Dowload and process Cambium  Electricity Dataset 2024 for sensitivity analysis.
* **03-Mix_AK_HI.R**: eGrid data for Hawai and Alaska, as these states are not present on EIA data.
* **04-ecoinventLCI.R**: Extract life cycle inventory flows from electricity datasets from ecoinvent 3.11. 
* **05-ElectricityEmissions.R**: Combines the forecast of electricity grid mix with the ecoinvent LCI flows to estimate the emissions factors (kg CO2e/kWh) for each year (2025-2050) and scenario at county level. 
