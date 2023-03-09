bayes_predict <- function(bayes_fit,dta,get_summary=TRUE,probs=c(0,0.25,0.5,0.75,1)){
    local_model <- bayes_fit |>
        hardhat::extract_fit_parsnip() |>
        purrr::pluck('fit')
    local_params <- local_model |> tidybayes::spread_draws(`b_.*`,regex=TRUE)
    var_names <- local_model |>
        tidybayes::get_variables() |>
        tibble::as_tibble() |>
        filter(grepl('b_',value)) |>
        mutate(vars=gsub('b_','',value))
    just_predictors <- dta |> mutate(Intercept=1,rn=row_number()) |> select(rn,any_of(var_names$vars))
    jp_split <- split(just_predictors,f=seq_along(just_predictors$Intercept))
    predict_params <- local_params[,var_names$value]
    logits <- lapply(jp_split,function(x,mm=predict_params){
        x_pred <- select(x,-rn)
        mat_prod <- as.matrix(mm) %*% t(as.matrix(x_pred))
        p <- exp(mat_prod) / (1+exp(mat_prod))
        results <- tibble(rn=unique(x$rn),p=p[,1])
        return(results)
    }) |> bind_rows()
    out <- logits
    if( get_summary ){
        quibble <- function(x, q) {
            tibble(x = quantile(x, q), q = q)
        }
        out <- logits |>
            group_by(rn) |>
            summarise(mean=mean(p),quibble(p,q=probs),.groups='drop') |>
            mutate(q=gsub('0\\.','_',paste0('p',q))) |>
            pivot_wider(names_from=q,values_from=x)
    }
    return(out)
}
