makeCacheMatrix <- function(x = matrix()) {
  m<-NULL                     #  m will be our 'solve matrix' and it's reset to NULL every time makeCacheMatrix is called
  set<-function(y){           # takes an input Matrix
    x<<-y                   # saves the input Matrix 
    m<<-NULL                # resets the solve matrix to NULL, basically what happens when a new object is generated.
  }
  get<-function() x           # this function returns the value of the original Matrix
  setmatrix<-function(solve) m<<- solve  # this is called by cacheSolve() during the first cacheSolve()
  #  access and it will store the value using superassignment
  getmatrix<-function() m   # this will return the cached value to cacheSolve() on
  #  subsequent accesses
  
  list(set=set, get=get,      #  OK, this is accessed each time makeCacheMatrix() is called,       
       setmatrix=setmatrix,   #   that is, each time we make a new object.  This is a list of
       getmatrix=getmatrix)   #   the internal functions ('methods') so a calling function
  #   knows how to access those methods.  
}

cacheSolve <- function(x=matrix(), ...) {   # the input x is an object created by makeCacheMatrix
  m <- x$getmatrix()                      # accesses the object 'x' and gets the value of the solve matrix
  if(!is.null(m)){                        # if solve was already cached (not NULL) ...
    message("getting cached data")      # ... send this message to the console
    return(m)                           # ... and return the solve ... "return" ends
  }                                       #   the function cachemean(), note
  matrix<-x$get()                         # we reach this code only if x$getmatrix() returned NULL
  m<-solve(matrix, ...)                   # if m was NULL then we have to calculate the solve
  x$setmatrix(m)                          # store the calculated solve value in x (see setmatrix() in makeCacheMatrix
  m                                       # return the solve to the code that called this function
}