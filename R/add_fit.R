add_fit <- function(model,dd,...){
    pred <- bayes_predict(model,dd,...)
    out <- bind_cols(dd,pred)
    return(out)
}
