fit_model <- function(model_spec,recipe,data){
    wf <- workflows::workflow() |> workflows::add_model(model_spec) |> workflows::add_recipe(recipe)
    fitted_model <- parsnip::fit(wf,data=data)
    return(fitted_model)
}
