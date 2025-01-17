# Reproducible research compendium

## About project 
This research addresses the problem of identifying rare anomalies in a high-dimensional, imbalanced dataset. The primary research question focuses on determining which unsupervised anomaly detection method—K-Nearest Neighbors or Isolation Forest—performs more effectively under these conditions. Both methods are evaluated using cross-validation and compared based on their F1-scores on a labeled validation set. 


## Data 
In this research we utilise a dataset gathered from one of India's leading semiconductor manufacturers, where each row represents an observation recorded every 10 milliseconds. These features are derived from sensor readings across multiple manufacturing equipment, reflecting a range of operational parameters and conditions at different stages of production. The dataset is anonymised, with feature names hidden. Data comes from the [Kaggle competition](https://www.kaggle.com/datasets/subham07/detecting-anomalies-in-water-manufacturing/data).

You can check further description in the report. 

## Folder structure 
   - `code`: code for 
      * `01_data_preprocessing.R`: EDA and data preprocessing
      * `02_analysis.R`: KNN and Isolation Forest application 
   - `raw_data`: 
      * `Train.csv`: initial dataset from the competition 
   - `processed_data`:
      * `test_data.csv`: test data after preprocessing
      * `train_data.csv`: train data after preprocessing
   - `report`
      * `bibliography.bib`: document with bibliography 
      * `report.qmd`: source report file
      * `report.pdf`: rendered report 

   
## How to use 
### Renv
To ensure use of exact package versions, isolation from other projects and reproducible workflow, virtual environment is used and needs to be activated. All required packages are in the `renv.lock`,  
  1. Install `renv` if needed: `install.packages("renv")`
  2. Load `renv`: `library(renv)`
  3. Activate it: `renv::activate()`
  4. Install packages in `renv.lock` by running `renv::restore()`

### Project
1. Run `code/02_analysis.R` to check results for comparison
2. Render `report/report.qdm` to obtain final report
3. Check the results

## Questions? 
If you still have any questions, please email me at lv(dot)pauline( @ )gmail( dot )com
