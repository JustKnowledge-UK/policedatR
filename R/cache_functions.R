#' Check the caching status of API requests
#'
#' caching_check() will suggest setting up a cache directory on local disk to
#' speed up API repeat API queries. It will first look for a cache directory using
#' rappdirs::user_cache_dir. If it finds none it will ask permission to create
#' one. If user agrees, API query responses will be cached here to be reused on
#' repeat queries. "caching" and "cache_dir" will be added to the global environment
#' using Sys.setenv() so that these options are available to other functions.
#'
#' If user opts out, the cache won't be created and queries won't be saved.
#' "caching" and "cache_dir" will not be added to global environment.
#'
#' It is possible to change your preference after creating the cache using the
#' reset = TRUE option. This will remove "caching" and "cache_dir" from global
#' environment. Note this doesn't remove the existing cache if it was previously made.
#'
#' Although not handled by this function, other policedatR functions use "caching"
#' and "cache_dir" to set their parameters for caching. These functions use
#' memoise::memoise() and cachem::cache_disk() to handle caching. The max age
#' for cache items is set to Inf. max_size of the cache is 1GB, after which cache
#' items are evicted according to the Least Recently Used (LRU) eviction policy.
#'
#'
#'
#' @param reset Boolean. If TRUE it will erase the previously set caching preference
#' and ask the user again. Note existing cache won't be deleted
#'
#' @returns No object, but sets "caching" and "cache_dir" in the global environment
#' to be used by other functions. Also prints to console the cache directory and
#' current chosen caching preference.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # This is interactive so don't run
#' caching_check()
#' }
#'
caching_check <- function(cache = TRUE){
  caching_status <- Sys.getenv("caching")
  cache_dir <- rappdirs::user_cache_dir(appname = NULL, appauthor = "policedatR")
  if(caching_status == ""){ # if caching_status not set
    caching_status <- cache # set caching_status
  }
  cat(paste0("\nCaching status set to ", caching_status))

  if(caching_status == TRUE){
    if(!dir.exists(cache_dir)){ # if caching directory doesn't exist
      dir.create(cache_dir, recursive = TRUE) # create directory
      cat(paste0("\nCreating cache at ", cache_dir))
    }
    else{
      cat(paste0("\nCache directory exists at ", cache_dir))
    }
  }

  Sys.setenv(caching = caching_status)
  Sys.setenv(cache_dir = cache_dir)
}

#' Flush the cache
#'
#' Delete all files in the cache directory. This may be helpful if `get_*` functions
#' are returning unexpected results. This may happen, for example, where requests
#' have failed but their responses were cached, and subsequent requests are
#' reloading this failed request.
#'
#'
#' Ideally this should never be necessary as failed requests should be handled by
#' the `get_*` functions. However, there have been a few instances during development
#' where it was necessary to flush the cache, so this function is provided as a
#' 'just in case you need it' function.
#'
#' Cache directory is specified by `rappdirs::user_cache_dir(appname = NULL, appauthor = "policedatR")`
#'
#' @returns Nothing. Simply deletes all files found in cache directory.
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # This is interactive so don't run
#' flush_cache()
#' }
flush_cache <- function(){

  # Specify cache directory
  cache_dir <- rappdirs::user_cache_dir(appname = NULL, appauthor = "policedatR")
  # Check if it exists.
  if(!dir.exists(cache_dir)){
    stop("No cache detected so nothing to flush. Aborting.")

  }
  # If it exists, check if there are files to delete.
  else if(dir.exists(cache_dir) & length(list.files(cache_dir)) == 0){
    cat(paste0("Cache detected at ", cache_dir, ", but cache is empty. No need to flush."))
  }
  # If there are files to delete, check the user is sure they want to delete them.
  else{
      repeat{
        cat(paste0("Cache detected at ", cache_dir, ". Are you sure you want to delete files in cache?\n"))
        check <- readline("(y/n)")
        if(check == "y"){
          cat("Deleting files in cache...")
          do.call(file.remove, list(list.files(cache_dir, full.names = TRUE)))
          break
        }
        else if(check == "n"){
          cat("Flush aborted.")
          break
        }
        else{
          cat("Please type either 'y' or 'n' and press Enter.\n")
        }
      }
  }
}

