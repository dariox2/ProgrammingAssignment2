## Creates a special "matrix" object
## that can cache its inverse.
## The "constructor" returns a list with four
## function objects to handle the data.
makeCacheMatrix <- function(x = matrix()) {

        imcach <- NULL ## init inverse matrix cache
        
        ## function to assign a new matrix object
        set <- function(mtrx) {
        
                x <<- mtrx      ## set the value of the original
                                ## matrix (parent environment)
                
                imcach <<- NULL ## As this is a new matrix, we 
                                ## must also clean the inverse cache
                                ## (parent environment) from any previous 
                                ## result, which is at
                                ## this moment not solved yet
        }
        
        ## function to get the original matrix
        get <- function() {
                            return(x)
                          }
        
        ## function to save the solved inverse matrix
        ## into the cache
        setinverse <- function(mtrxinverse){
                                             imcach <<- mtrxinverse ## (parent environment)
                                           }
        
        ## function to get the inverse matrix
        getinverse <- function() {
                                   return(imcach)
                                 }
        
        
        ## return list with all 4 functions
        return ( list(set = set,
                      get = get,
                      setinverse = setinverse,
                      getinverse = getinverse))


}


## Creates a function that returns the inverse of
## the matrix object allocated using the function
## makeCacheMatrix() defined above.
## The inverse matrix is solved only the first time
## and stored in the object cache. Subsequent calls
## must obtain the result from the cache.
cacheSolve <- function(x, ...) {

         ## Check if exists previous result
         ## assigned to the cache
         m <- x$getinverse()
        if(!is.null(m)) {
                ## if the stored inverse is not null,
                ## then it is already solved; return
                ## the previous result
                message("getting cached data")
                return(m)
        }
        
        ## solve the new inverse and
        ## save it to the cache
        message("solving...")
        mtrx <- x$get()
        m <- solve(mtrx, ...)
        x$setinverse(m)
        return(m)
}


##
## NOTE: UNCOMMENT FOLLOWING SECTION FOR TESTING ONLY
##

# cm <- makeCacheMatrix()
#
# cm$set(matrix( c(1,3,2,4), nrow=2, ncol=2))
# print(cm$get())
#
# print(cacheSolve(cm)) 
# print(cacheSolve(cm)) ## test cache working
# print(cacheSolve(cm)) ## test cache working
#
# cm$set(matrix( c(4,3,3,2), nrow=2, ncol=2))
# print(cm$get())
#
# print(cacheSolve(cm)) ## test cache reset
#

##
## results:
##
##      [,1] [,2]
## [1,]    1    2
## [2,]    3    4
## solving...
##      [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5
## getting cached data
##      [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5
## getting cached data
##      [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5
## solving...
##      [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4
##
