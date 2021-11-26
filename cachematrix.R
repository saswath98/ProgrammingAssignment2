## This function creates a special "matrix" object that can cache its inverse.
## 1 Sets the value of the matrix
## 2 Gets the value of the matrix
## 3 Sets the inverse of the matrix
## 4 Gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

        r <- NULL

        set <- function(y) {
                x <<- y
                r <<- NULL
        }

        get <- function(){ 
                   x
        }

        setinverse <- function(inverse) {
                r <<- inverse
        }
        
        getinverse <- function() {
          r
        }

        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix .
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {

        r <- x$getinverse()

        if(!is.null(r)) {
                message("getting cached data")
                return(r)
        }

        data <- x$get()
        
        r <- solve(data, ...)

        x$setinverse(r)
        
        r

}
