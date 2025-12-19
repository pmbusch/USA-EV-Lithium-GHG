# The energy, material and carbon handprint of lithium-ion batteries in electric vehicles - A US fleet model


Scripts usually load raw data to stored it in a processed format in the folder 'Inputs'. The processed data inputs are latter used for the Life Cycle Assessment Model.

* **Endpoints.R**: Script to combine all endpoints characterization factors into a **Master table** to translate all flows into relevant impact categories later.
* **Manufacturing**: Scripts to process data regarding environmental impacts from the **Manufacturing** stage.
* **Electricity**: Scripts to process data regarding environmental impacts from the **Electricity Generation** stage.
* **Fleet**: Estimate vehicle fleet, battery requirements and battery flows for recycling using survival curves and vehicle sales forecast. Then it estimates total energy (kWh or gasoline gallons) use from the vehicle fleet and average consumption.