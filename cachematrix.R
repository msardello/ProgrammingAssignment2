## Put comments here that give an overall description of what your
## functions do
## This is my version of Programming Assignment 2.  I have created two functions as required
## and have also tested that both functions work properly.  The functions work together to take an input
## which is in matrix format, and then calculate the Inverse of that Matrix.  There were lots of posts in the forum 
## on what an Inverse of a Matrix is... but many were wrong.  I have determined what that actually is and confirmed
## what the solve() function actually does and have confirmed proper results of these functions, even though that was 
## not really part of this exercise... per instructor, functions should not be run when graded.  But, this does work. 


## Write a short comment describing this function
## The cacheMatrix Function is a "container" function that essentially stores four additional functions.  In case, I have
## called them (1) setCachedMatrix, (2) getCachedMatrix, (3) setInvMatrix, and (4) getInvMatrix. makeCacheMatrix takes as 
## its input a matrix (i.e., a <- matrix(1:4, 2, 2) to create a 2 x 2 matrix, and then use makeCacheMatrix(a) to test)
## Once a matrix is "set" using this function, the other functions are called as necessary from the cacheSolve function later. 

makeCacheMatrix <- function(x = matrix()) {
     cm <- NULL
     setInputMatrix <- function(y) {  ##sets the the matrix as outlined above, i.e., makeCacheMatrix(a) where a is a 2 x 2 matrix
          x <<- y		       ## sets x as the input, y	
          cm <<- NULL		       ## restores the cached matrix to null value when creating the new special matrix	
     }
     getInputMatrix <- function() x   ## gets the current cached Matrix and returns it  
     setInvMatrix <- function(mat) cm <<- mat   ## sets the Inverse Matrix Object
     getInvMatrix <- function() cm              ## gets the Inverse Matrix Object
     list(setInputMatrix = setInputMatrix,    ## list() "stores" each of the four functions in the makeCacheMatrix function
          getInputMatrix = getInputMatrix,
          setInvMatrix = setInvMatrix,
          getInvMatrix = getInvMatrix)
}


## Write a short comment describing this function
## cacheSolve calculates the Inverse of a matrix as created in makeCacheMatrix.  It first looks to see if it is in memory, and if so, 
## the function simply returns what is in memory.  If it is not, it will calculate the inverse of the matrix using the solve() function. 
## for example the order to run these would be: 
## a <- matrix(1:4, 2, 2)  ## creates a 2 x 2 matrix (inverse matrices can only be calculated on a square matrix)
## b <- makeCacheMatrix(a) ## stores the matrix a (you can use b$getInputMatrix to pull this out and see the matrix)
## cacheSolve(b) ## this function looks at the stored matrix a, and then determines is the inverse of this matrix is in memory
## If it is in memory, the inverse matrix is returned, it is is not in memory, the solve() is used to calculate the inverse matrix
## and store it in memory.  Such that the next time it is run, if it is found in memory, a note indicating that the cached version is
## being returned is now part of the output.  

cacheSolve <- function(x, ...) {
     cm <- x$getInvMatrix()                  ## sets cm to the Inverse Matrix from he makeCacheMatrix function
     if(!is.null(cm)) {                      ## If cm is not null (i.e., it is in memory, then a message stating "getting cached matrix"
          message("getting cached matrix")   ## is returned, and then the cached version of the inverse matrix cm is returned
          return(cm)
     }
     specMatrix <- x$getInputMatrix()       ## If there is nothing in cached inverse is not present, the stored original matrix is returned
     cm <- solve(specMatrix, ...)            ## solve() is then used to calculate the inverse of the input matrix
     x$setInvMatrix(cm)                      ## the setInvMatrix function is then used to set the inverse of the input matrix
     cm                                      ## finally, the newly calculated inverse of the input matrix is returned
}
