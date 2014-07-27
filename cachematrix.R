## These functions are used as a pair to calculate the inverse of a square matrix
## and to cache the inverse matrix in memory for quick retrieval if it is requested again

## makeCacheMatrix creates a list of functions used to store and retreieve a matrix
## and it's inverse 

makeCacheMatrix <- function(x = matrix()) {

        xinverse <- NULL
        set <- function(y) {
                x <<- y
                xinverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) xinverse <<- inverse
        getinverse <- function() xinverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}

## cacheSolve computes the inverse a square matrix and stores to a cache Matrix. 
## It will retrieve the inverse from the cache if it's already been calculted and stored. 

cacheSolve <- function(x, ...) {
        ## Check to see if CacheMatrix exists; create if it doesn't
        if (!exists("CacheMatrix")) {
                CacheMatrix <<- makeCacheMatrix(x) 
        } else {
                ## If cacheMatrix exists, check stored matrix to match this one
                matequal <- function(x, y) 
                        is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)  
                if (!matequal(CacheMatrix$get(), x)) {
                        CacheMatrix <<- makeCacheMatrix(x)
                }
        }
        
        ## Check CacheMatrix inverse value, display message and return it if not NULL
        xinverse <- CacheMatrix$getinverse()
        if(!is.null(xinverse)) {
                message("getting cached data")
                return(xinverse)
        }
                
        #Solve for inverse, if error display and return NULL
        #Solve will throw an error if matrix is singular, or not n x n
        xinverse <- tryCatch(solve(x, ...), 
                        error = function(e) {
                                message("Got the following error:")
                                message(geterrmessage())
                                message(" ")
                                message("Will return NULL")
                                xinverse <- NULL
                                return(xinverse)
                                }
                        )
        
        ## Store xinverse in CacheMatrix and return value
        CacheMatrix$setinverse(xinverse)
        xinverse
        
}
