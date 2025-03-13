
caching_check <- function(reset = FALSE){
  if(reset){
    cat("Resetting caching option\n")
    Sys.unsetenv("caching")
  }
  caching_status <- Sys.getenv("caching")
  cache_dir <- rappdirs::user_cache_dir(appname = NULL, appauthor = "policedatR")
  if(caching_status == ""){
    if(!dir.exists(cache_dir)){
      # repeat check until either y or n has been pressed
      repeat{
        cat("We recommend caching the data acquired from geoportal.gov.uk\nso that repeat queries can run faster.\nDo you want to create a local cache to save queries?")
        check <- readline("(y/n)")
        if(check == "y"){
          dir.create(cache_dir, recursive = TRUE)
          caching_status <- TRUE
          cat(paste0("Creating cache at ", cache_dir))
          cat(paste0("Caching status set to ", caching_status))
          break
        }
        else if(check == "n"){
          print("Not caching data. Repeat queries won't be quicker.")
          caching_status <- FALSE
          break
        }
        else{
          print("Please type either 'y' or 'n' and press Enter.")
        }
      }
    }
    else {
      repeat{
        cat(paste0("Cache detected but caching option not set. Do you want to cache?"))
        check2 <- readline("(y/n)")
        if(check2 == "y"){
          cat(paste0("Caching at ", cache_dir))
          caching_status <- TRUE
          break
        }
        else if(check2 == "n"){
          print("Not caching data. Repeat queries won't be quicker.")
          caching_status <- FALSE
          break
        }
        else{
          print("Please type either 'y' or 'n' and press Enter.")
        }
      }
    }

  }
  else {
    if(!dir.exists(cache_dir)){
      cat(paste0("Caching set to ", caching_status, " but no cache directory detected. Use caching_check(reset = TRUE) to create one."))
      caching_status <- caching_status
      break
    }
    else{
      cat(paste0("Caching set to ", caching_status, "\nCache directory exists at ", cache_dir, "\nUse caching_check(reset = TRUE) to reset this option."))
      caching_status <- caching_status
    }

  }

  Sys.setenv(caching = caching_status)
  Sys.setenv(cache_dir = cache_dir)
}
