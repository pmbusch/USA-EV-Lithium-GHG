# The energy, material and carbon handprint of lithium-ion batteries in electric vehicles - A US fleet model

Estimate vehicle fleet, battery requirements and battery flows for recycling using survival curves and vehicle sales forecast. Then it estimates total energy (kWh or gasoline gallons) use from the vehicle fleet and average consumption.

All scripts are numbered in order suggested to run to replicate full results.

* **01-Sales.R**: Load and filter vehicle sales forecast from the ICCT Roadmap 2.6.
* **02-Dissagregate_EV.R**:  Dissagregate EVs at US level into state level based on 2023 registrations and population.
* **03-SurvivalFleet.R**: Stock, inflow and outflow vehicle and battery model based on vehicles sales and survival curves, using the product-component framework.
* **04-TemperatureEffect.R**: Temperature effect on driving efficiency (energy consumption).
* **05-Fleet_Model.R**: Compiles results for projection of fleet level at US state.
* **06-Fleet_Operation.R**: Based on Fleet size (with age), fuel consumption and vehicle miles traveled (VMT), calculates yearly fuel or electricity consumption.
* **07-LCI_Combustion.R**: Upstream impacts of gasoline production (not combustion).
