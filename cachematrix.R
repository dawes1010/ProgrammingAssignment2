## Cache the inverse of a matrix. First create the matrix. Then use solve to create the invserse. 
# when retrieving the inverse, check if it exists first, otherwise create it. 

#The makeCachMatrix function creates a matrix object that can cache its inverse
#matrix is in x and inverse is in mx
makeCacheMatrix <- function(x = matrix()) {
        mx <- NULL
        set <- function(y) {
                x <<- y
                mx <<- NULL
        }
        get <- function() x                          #return matrix
        setinverse<- function(solve) mx <<-solve     #save solve value
        getinverse <- function() mx                  #retrieve solve value
        list(set = set, get = get,                   #return a list 
             setinverse = setinverse,
             getinverse = getinverse)
}



#The CacheSolve function returns the inverse of the matrix returned by makeCacheMatrix. If the inverse has 
#already been calculated then the cachesolve retrieves the inverse from the cache. If it has not been calculated
#cacheSolve calculates the inverse
cacheSolve <- function(x, ...) {
        mx <- x$getinverse()
        if (!is.null(mx)) {
                message("getting cached data")
                return(mx)
        } 
        xdata <- x$get()                        #get data from the matrix to calc the inverse on
        mx <- solve(xdata,...)                  #use solve to inverse
        x$setinverse(mx)                        #save solve value
        mx
        
}
