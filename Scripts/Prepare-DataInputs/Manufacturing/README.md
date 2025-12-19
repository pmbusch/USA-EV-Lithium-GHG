# The energy, material and carbon handprint of lithium-ion batteries in electric vehicles - A US fleet model

Scripts to process data regarding environmental impacts from the **Manufacturing** stage.

All scripts are numbered in order suggested to run to replicate full results.

* **0-BatScenario.R**: LIB Size scenario definition based on average and quantile values.
* **01-LCI_Upstream.R**: Combine all life cycle inventory data for manufacturing into a common file, from the ecoinvent 3.11 dataset. This file is later used in the Scripts "02" to "06".
* **02-Vehicle_Upstream.R**: Vehicle Upstream production impacts.
* **03-VehMaintenance.R**: Vehicle maintenance impacts.
* **04-LIB_Upstream.R**: Lithium-ion battery (LIB) manufacturing Upstream production impacts.
* **05-LIB_Recycling.R**:  LIB Recycling  impacts.
* **06-Veh_Recycling.R**: Vehicle Recycling  impacts.
