library(googlesheets)
library(readr)
googlesheets::gs_auth(token = "shiny_app_token.rds")

# drawn_data_url = "https://docs.google.com/spreadsheets/d/1weYOccg_e9DGN165Hr9zXO26yIT6zVO55irOj_2ddcs/2PACX-1vSG-Jz8R2Ga5T37Rc83nyS3bNMF4z_eckRdnLsmCryy0bhWZUeZgr6AmpeiqtgoUx9TxwLJemuYXu8X/pubhtml"
# users_data_url = "https://docs.google.com/spreadsheets/d/1KMAqTD8HmNmSjRHp-xRT8yy-hu46ZDA9iOO04EyOKpA/2PACX-1vSG-Jz8R2Ga5T37Rc83nyS3bNMF4z_eckRdnLsmCryy0bhWZUeZgr6AmpeiqtgoUx9TxwLJemuYXu8X/pubhtml"
# drawn_gs = drawn_data_url %>% gs_url()
# users_gs = users_data_url %>% gs_url()

# see https://stackoverflow.com/questions/32537882/adding-rows-to-a-google-sheet-using-the-r-package-googlesheets
drawn_gs = gs_key(x = "1weYOccg_e9DGN165Hr9zXO26yIT6zVO55irOj_2ddcs", 
                  lookup = FALSE,
                  visibility = "private")
users_gs = gs_key(x = "1KMAqTD8HmNmSjRHp-xRT8yy-hu46ZDA9iOO04EyOKpA",
                  lookup = FALSE,
                  visibility = "private")

# gs_drawn_data = gs_read(drawn_gs)
read_gs_coltypes = function(ss = drawn_gs,
                         col_types = readr::cols(      
                           x = col_integer(),
                           y = col_double(),
                           group = col_character(),
                           user = col_character(),
                           date = col_date(format = ""),
                           session_id = col_character(),
                           .default = col_character()
                         )) {
  x = gs_read(ss = ss)
  # need this for issue
  # https://github.com/jennybc/googlesheets/issues/330
  x = dplyr::mutate_all(x, as.character)
  x = readr::type_convert(
    x, 
    col_types = col_types
  )
  x
}
read_gs_drawn = function(...) {
  x = read_gs_coltypes(...)
  # need rounding for merge
  x$y = round(x$y, digits = 8)
  x
}

read_gs_user = function() {
  read_gs_coltypes(ss = users_gs,
                col_types = readr::cols(
                  user = col_character(),
                  email = col_character()
                ))
}
gs_drawn_data = read_gs_drawn()

gs_user_data = read_gs_user()

lookup_user = function(email) {
  have_email = email %in% gs_user_data$email
  msg = paste0("Email is ", email)
  shinyjs::logjs(msg)
  
  if (have_email) {
    shinyjs::logjs("Email is in the data set")
    print(head(gs_user_data))
    username = gs_user_data[ gs_user_data$email %in% email, ]
    username = username$user[1]
  } else {
    shinyjs::logjs("Email NOT in data set")
    # make it unique
    username = NA
    while (username %in% unique(gs_user_data$user)) {
      username = basename(tempfile(pattern = "user"))
    }
    
    vec = c(email = email, user = username)
    users_gs %>% gs_add_row(input = vec, verbose = TRUE)
  }
  return(username)
}

diff_data = function(drawn_data) {
  if (nrow(drawn_data) > 0) {
    gs_drawn_data = read_gs_drawn()
    drawn_data$group = as.character(drawn_data$group)
    dd = arrange(drawn_data, user, date, session_id, 
                 x, y, group)
    print(head(dd))
    print(dput(head(dd)))
    
    dd = arrange(gs_drawn_data, user, date, session_id, 
                 x, y, group)
    print(head(gs_drawn_data))    
    drawn_data = anti_join(drawn_data, gs_drawn_data)
    print(drawn_data)
  }
  # make sure order
  gs_drawn_names = colnames(gs_drawn_data)
  drawn_data = drawn_data[, gs_drawn_names]
  return(drawn_data)
}

