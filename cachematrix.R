## Two functions that will either 
## a. Calculate the inverse of the matrix provided  
## b. If the inverse matrix has been calculated previously then retrieve the inverse matrix from cache


## makeCacheMatrix returns a list of functions that allows the user to cache the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
      inverse_matrix <- NULL                                      ## initialise inverse_matrix to null
      set <- function(y) {                                        ## Set function caches value of matrix x 
            x <<- y
            inverse_matrix <<- NULL
      }
      get <- function() x                                         ## get function returns value of matrix x from cache
      set_inverse <- function(solve) inverse_matrix <<- solve     ## set_inverse caches the inverse matrix
      get_inverse <- function() inverse_matrix                    ## get_inverse returns the inverse matrix from cache
      list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)  ## returns a list of the functions
}


## cacheSolve either calculates and returns the inverse matrix to x and caches the inverse matrix, or if the inverse matrix 
## has been calculated previously returns the cached inverse matrix

cacheSolve <- function(x, ...) {
      inverse_matrix <- x$get_inverse()   ## Return the cached inverse matrix
      if(!is.null(inverse_matrix)) {      ## If an inverse matrix has been cached previously return inverse matrix
            message("getting cached data")
            return(inverse_matrix)
      }
      data <- x$get()                     ## If inverse hasn't been cached previously then
      inverse_matrix <- solve(data)       ## calculate the inverse matrix
      x$set_inverse(inverse_matrix)       ## add the inverse matrix to the cache
      inverse_matrix                      ## return the inverse matrix
}

