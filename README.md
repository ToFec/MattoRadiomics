# MattoRadiomics
radiomic feature elimination, clustering, model building and analysis

This repository contains code to analyze radiomic features extracted from MRI and PET data during the MATTO project. The provided code allows the user to build a fully automated feature processing, model building, and evaluation pipeline, and provides the following features:
- Analyze radiomic features
- Eliminate redundant and unimportant features
- Cluster features
- Feature sampling using different methods
- Cross-validation and nested cross-validation
- Model building (e.g. logistic, linear or cox models)
- Combine multiple models into an ensemble model
- Model evaluation (e.g. concordance index, accuracy, AUC)
- Model analysis (e.g. feature importance, frequency and coefficients)

To apply the feature elimination and model building routines you must run the `MattoRunModelTrainingWithCV.R` script. The script takes three arguments. The first one specifies the output folder, the second one defines the settings class to use and the last one is the class name of the csv parser to use.
```
./MattoRunModelTrainingWithCV.R /path/to/output/folder MattoSettingsCox MattoCSVParserOS
```
The class `MattoSettingsCox` is defined in `MattoSettings.R` and contains information about cross validation, which models to use, feature elimination pipeline and samplers. 
The class `MattoCSVParserOS` is defined in `MattoCSVParser.R`. In the csv parser the file containing the radiomic features and information about the file (e.g. columns to include or exclude, which columns contain the target variables) is given.

After successfully training a model or an ensemlbe of models you can evaluate the models using `EvaluateModels.R`. The script takes three arguments. The first one specifies the folder containing the trained model. The second and third argument define the settings class and csv parser class used during training. To evaluate the model created with the command above run:
```
./EvaluateModels.R /path/to/output/folder MattoSettingsCox MattoCSVParserOS
```
The script creates a file called info.txt in the defined output folder.

In case you want to have more information about the features used in the trained models, use `EvaluateFeatureCoefficients.R`. The script gives information about how often a feature was used for a model in cross-validation, the model coefficients and the cluster it is part of. The parameters are the same as for `./EvaluateModels.R`
```
./EvaluateFeatureCoefficients.R /path/to/output/folder MattoSettingsCox MattoCSVParserOS
```
The script creates a file called featureInfo.txt in the specified output folder.  

---

For developement and testing R version 4.1.2 was used. 
Dependencies: tba

---
This is an ongoing project. More code and information will follow.

If you have any questions or need more information about the code please feel free to contact me. 
