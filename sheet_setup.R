library(googlesheets)

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

drawn_data = gs_read(drawn_gs)
user_data = gs_read(users_gs)

lookup_user = function(email) {
  if (email %in% user_data$email) {
    username = user_data[ user_data$email %in% email, ]
    username = username$user
  } else {
    username = basename(tempfile(pattern = "user"))
    vec = c(email = email, user = username)
    users_gs %>% gs_add_row(input = vec, verbose = TRUE)
  }
  return(username)
}

