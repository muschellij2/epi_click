shiny_token <- gs_auth() # authenticate w/ your desired Google identity here
saveRDS(shiny_token, "shiny_app_token.rds")
