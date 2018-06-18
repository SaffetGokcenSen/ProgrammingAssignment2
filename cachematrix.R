## These two functions make use of the lexical scoping, closure and deep 
## assignment arrow concepts of R language for using the cached matrix inverse
## insead of re-calculating it.

## makeCacheMatrix has a matrix input and outputs a list of four functions. set
## function sets the input x to its formal argument y and sets inv to NULL. get 
## function gets the value of x in the execution environment of makeCacheMatrix.
## set_inv function sets inv to its formal argument the_inv. get_inv function
## gets the value of inv in the execution environment of makeCacheMatrix. The 
## execution environment of the makeCacheMatrix is the enclosing environment for
## each of the functions set, get, set_inv and get_inv. Hence, the execution
## environment is captured after makeCacheMatrix is run. Hence, the value of inv
## and x are not lost. The use of deep assignment operator <<- in the functions
## set and set_inv enable the change of the variables inv and x in the execution
## environment. Once the inv is computed, its value can be kept and read from 
## cache.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      
      get <- function(){
            x
      }
      
      set_inv <- function(the_inv){
            inv <<- the_inv
      }
      
      get_inv <- function() {
            inv
      }
      
      list(set = set, get = get,
           set_inv = set_inv,
           get_inv = get_inv)
}


## cacheSolve gets x as input. x is a list like the output of makeCacheMatrix.
## Using this list, cacheSolve determines if the matrix inverse has already been
## computed. If it has been computed, then the inverse is read from cache, else
## it is computed and inv in the execution environment of makeCacheMatrix is set
## by means of the set_inv function. After this setting, the inverse is read 
## from cache unless the matrix is not changed.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$get_inv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$set_inv(inv)
      inv
}
