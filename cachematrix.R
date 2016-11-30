## Caches the inverse of a matrix to avoid repeated computation


## Creates a special "matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()){
        imat <- NULL
        set <-function(y){
                x <<- y
                imat <<- NULL
        }
        get <-function() x
        setsolve <- function(solution) imat<<-solution
        getsolve <- function() imat
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## Function that returns the inverse of the cached matrix x



cachesolve <- function(x, ...) {
        imat <- x$getsolve()
        if(!is.null(imat)) {
                message("getting cached data")
                return(imat)
        }
        data <- x$get()
        imat <-solve(data, ...)
        x$setsolve(imat)
        imat
        
        ##Returns the Inverse of x
}
