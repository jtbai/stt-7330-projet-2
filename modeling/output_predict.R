# -------------------------------------------------------------------------
# Title: Output- Classical models
# Date: April 2018
# -------------------------------------------------------------------------

source("modeling/test-models.R")


#------------------------------------------------------------------------

predict_model <- function ( model_name, validation_data ){
   output_predict = predict(model_name,newdata = validation_data)
   return(output_predict)
}

classical_models = list(model_bayes,tree_based,model_lda,model_qda,random_forest,svm_gaussien,svm_poly3,model_multi)

list_predictions <- lapply(classical_models, function(model) {
  predict_model(model_name = model, validation_data = data_prepared[, -(c("split_group"))])
})



# save output -------------------------------------------------------------------

vec_models <- c("model_bayes","model_tree","model_lda","model_qda","model_rf","model_svm_gaussien","model_svm_poly3","model_multi")

for (preds in seq_along(list_predictions)) {
  fwrite(data.frame(list_predictions[[preds]]), file = paste0("data/predictions/predictions_", vec_models[preds], ".csv"))
}
