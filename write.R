

FileConnection <- file("energyschedule.R")
writeLines( paste0("#This is a test script. Run at: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
            , FileConnection
)
close(FileConnection)
rm(FileConnection)

add( repo = getwd()
     , path = "GitHub.R"
)

push( object = getwd()
      , credentials = cred_user_pass( username = "jwildish" # BE CAREFUL!!
                                      , password = "Hazel_1912!"  # BE CAREFUL!!
      )                           # NEVER EVER PUSH YOUR CREDENTIALS TO ANY REPOSITORY!!!!
)
