# -------------------------------------------------------------------------
# Title: Output- Classical models
# Date: April 2018
# -------------------------------------------------------------------------

source("modeling/test-models.R")


#------------------------------------------------------------------------

predict_model <- function ( model_name, validation_data ){
   output_predict = predict(model_name,newdata = validation_data)
   retun(output_predict)
}

classical_models = c(model_bayes,tree_based,model_lda,model_qda,random_forest,svm_gaussien,svm_poly3,model_multi)

for(i in 1:8){
  predict_models <- mapply(predict_model,classical_models[i],data_prepared[, -(c("split_group"))])
}



# save output -------------------------------------------------------------------
fwrite(predict_models, file = "data\predict_naive_bayes.csv")
fwrite(classical_models[1], file = "data\predict_tree_based.csv")
fwrite(classical_models[2], file = "data\predict_lda.csv")
fwrite(classical_models[3], file = "data\predict_qda.csv")
fwrite(classical_models[4], file = "data\predict_random_forest.csv")
fwrite(classical_models[5], file = "data\predict_svm_gaussien.csv")
fwrite(classical_models[6], file = "data\predict_svm_poly3.csv")
fwrite(classical_models[6], file = "data\predict_model_multi.csv")