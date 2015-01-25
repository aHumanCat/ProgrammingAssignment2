## makeCacheMatrix:
#    INPUTS:
#        matrixToCache - an invertable matrix
#    OUTPUT:
#        A LIST of function calls to manipulate makeCacheMatrix content
#
#    store a local/cached copy of the input matrix
#    create local functions to get & set the local matrix
#     and to set and get the inverse of the local matrix
#    return a list of the functions to the caller

makeCacheMatrix <- function( matrixToCache = matrix() ) {
  
  # Inverse cache variable
  inverse <- NULL
  
  # reset matrixToCache to a new value and reset the inverse atribute
  set <- function( y ) {
    
    matrixToCache <<- y
    inverse <<- NULL
    
  }
  
  # return the cacahed matrix
  get <- function() {  
    
    matrixToCache
    
  }
  
  # store the inverse of matrixToCache
  setInverse <- function( inv ) {
    
    inverse <<- inv

  }
  
  # return the inverse of matrixToCache
  getInverse <- function() {
   
    inverse
    
  }
  
  # list of getter and setter functions 
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## cacheSolve:
#    INPUTS: 
#       cacheFunctionList - object returned by makeCacheMatrix
#       inMatrix - initial matrix for comparison to the cached one
#    OUTPUT:
#       a MATRIX - Either the inverse of the matrix referenced by cacheFunctionList
#                   or inMatrix on error.
#
#    if the matrix has changed throw an error and return inMatrix
#       The caller is responsible for detecting and dealing with the return of inMatrix
#    if the inverse has already been computed, return it
#    otherwise, compute the inverse, cache it and return the newly calulated inverse to the caller
      
cacheSolve <- function( cacheFunctionList = list(), inMatrix = matrix(), ... ) {
  
  # grab the cached matrix
  cachedMatrix <- cacheFunctionList$get()
  
  # if the matrix has changed nothing more to do 
  if(!( identical( inMatrix, cachedMatrix )) ) {
    
    message("Error: cached matrix and submitted matrix are not identical")
    return( inMatrix )
    
  }
  
  # grab the invese from the matrix cache
  inverse <- cacheFunctionList$getInverse()
  
  # test for presence of a cached inverse
  if( !is.null( inverse ) ) {
   
    message("getting cached data")
    
    return( inverse )
    
  } else {
    
    # inverse has not been cached.
    #  compute the inverse , cache and return it
    
    inverse <- solve( cachedMatrix )
    
    cacheFunctionList$setInverse( inverse )
    
    inverse
    
  }
        
}
