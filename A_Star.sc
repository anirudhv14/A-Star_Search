import scala.util.control._
import scala.collection.mutable.ArrayBuffer

object A_Star {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
	def AStar(r:Int, c:Int, diff:Double = 0.4, printBool:Boolean) = {
	  val cols = r
	  val rows = c
	  
	  var path:Array[Spot] = Array()
	  
	  val grid = Array.ofDim[Spot](rows,cols)
	  
	  
	  for(i <- 0 until cols){
	  	for(j <- 0 until rows){
	  		grid(i)(j) = new Spot(i, j, rows, cols)
	  	}
	  }
		
		def create_Grid(grid:Array[Array[Spot]]) = {
			for(i <- 0 until cols){
				for(j <- 0 until rows){
					var rd = math.random()
					if(rd < diff){
						grid(i)(j).data = 1
					}
				}
			}
			grid(0)(0).data = 0
			grid(rows-1)(cols-1).data = 0
		}
		
		def getNeighboursPos(grid:Array[Array[Spot]], i1:Int, j1:Int) = {
			var aaaa = grid(i1)(j1).neighbours
			for(ini <- aaaa){
				println(ini.i+ "  "+ini.j)
			}
		}
		
		def heuristic(a:Spot, b:Spot):Double = {
			var d = math.sqrt((a.i - a.j)*(a.i - a.j) + (b.i - b.j)*(b.i - b.j))
			return d
		}
		
		def printPath() = {
			for(i1 <- path.reverse){
		  	println(i1.i + "  " + i1.j)
		  }
		}
		
	  
		
		
		
		create_Grid(grid)
		var start = grid(0)(0)
	  var end = grid(cols-1)(rows-1)
		
		def printGrid() = {
			for(i <- 0 until cols){
		  	for(j <- 0 until rows){
		  		var cc = grid(i)(j)
		  		if(cc.i == start.i && cc.j == start.j){
		  			 print("S" + " " )
		  			//print("| S |" + " " )
		  		}
		  		else if(cc.i == end.i && cc.j == end.j){
		  			// print("| E |" + " " )
		  			print("E" + " " )
		  		}
		  		else if(path contains cc){
		  			// print("| @ |" + " " )
		  			print("@" + " " )
		  		}
		  		else{
		  			if(cc.data == 0){
		  				//print("|   |" + " " )
		  				print(" " + " " )
		  			}
		  			else{
		  				// print("| "+cc.data + " | " )
		  				print("X" + " " )
	  				}
		  		}
		  	}
		  	println("  ")
		  }
		}
		
		
		for(i <- 0 until cols){
			for(j <- 0 until rows){
					grid(i)(j).addNeighbours(grid)
			}
		}
		
		
		// Neighbours by (col. no, row no.)
		// getNeighboursPos(grid, 0,1)
	  
	         
		var openSet:ArrayBuffer[Spot] = ArrayBuffer()
	  var closedSet:Array[Spot] = Array()
	  
	  openSet = openSet ++ Array(start)
	  
	  val loop = new Breaks;
	  
	  loop.breakable{
	    
		  while(true){
			  if(openSet.length > 0){
			  	var winner = 0
			  	for(i <- 0 until openSet.length){
			  		if(openSet(i).f < openSet(winner).f){
			  			winner = i
			  		}
			  	}
			  	var current = openSet(winner)
			  	
				  if(current == end ){
				  	println("End")
				  	
				  	if(printBool){
						  var temp  = current
						  path = path ++ Array(temp)
						  while(temp.previous != null){
						  	path = path ++ Array(temp.previous)
						  	temp = temp.previous
						  }
						  printGrid
					  	println("")
					  }
				  	loop.break
				  }
				  
				  openSet -= current
				  closedSet = closedSet ++ Array(current)
				  
				  var neighbours = current.neighbours
				  for(i <- 0 until neighbours.length){
				  	var neighbour = neighbours(i)
				  	
				  	if(!(closedSet contains neighbour) && neighbour.data != 1){
				  		var tempG = current.g + heuristic(neighbour, current)
				  		
				  		var newPath = false
				  		if(openSet contains neighbour){
				  			if(tempG <  neighbour.g){
				  				neighbour.g = tempG
				  				newPath = true
				  			}
				  		}
				  		else{
				  			neighbour.g = tempG
				  			newPath = true
				  			openSet = openSet ++ Array(neighbour)
				  		}
				  		
				  		if(newPath){
				  			neighbour.h = heuristic(neighbour, end)
				  			neighbour.f = neighbour.g + neighbour.h
				  			neighbour.previous = current
				  		}
				  	}
				  }
				  
				  
			  }
			  else{
			  	println("No Solution")
			  	loop.break
			  }
		  }
	  }
  }                                               //> AStar: (r: Int, c: Int, diff: Double, printBool: Boolean)Unit
  
  /*
  for(i <- 1 to 20){
		var diff = 0.2
		if(i > 6 && i < 13){
			diff = 0.3
		}
		if(i > 12 && i < 20){
			diff = 0.4
		}
		
		// var t = System.nanoTime()
		AStar(i, i, diff, false)
		// println(i + " " + (System.nanoTime()-t))
		// println()
	} */
	
	AStar(20, 20, 0.4, true)                  //> End
                                                  //| S   X X     X       X X   X   X   X       
                                                  //|   @ X   X             X         X   X     
                                                  //| X @ X   X X   X     X   X X               
                                                  //|     @       X X         X   X X       X   
                                                  //| X @ X X   X X X X X   X X X   X X         
                                                  //|   X @ X         X X X               X X   
                                                  //|     @ X X X X X X       X X     X X   X   
                                                  //| X @ X X @ X @ X X X       X X X     X     
                                                  //| X X @ @ X @ X @           X         X     
                                                  //| X X X         X @ X         X X     X X   
                                                  //| X X X X     X X @ X X X       X     X     
                                                  //| X       X X       @ @ @   X   X           
                                                  //|       X         X X     @ X X   X X   X   
                                                  //|   X     X   X           @ X X   X     X   
                                                  //| X X       X     X X X   @ X X X           
                                                  //| X     X     X X   X     @ X X @   X X     
                                                  //|       X         X X X X X @ @ X @   X     
                                                  //|       X X X   X X X X   X     X X @ X     
                                                  //| X X       X X   X         X   X     @     
                                                  //|     X         X X         X   X   X   E   
                                                  //| 
  
  
}


class Spot(i1:Int, j1:Int, r:Int, c:Int){
	var i = i1
	var j = j1
	val cols = c
	val rows = r
	var data  = 0
	
	var f = 0.0
	var g = 0.0
	var h = 0.0
	
	var previous:Spot = null
	
	var neighbours:List[Spot] = List()
	
	
	def addNeighbours(grid:Array[Array[Spot]]) = {
		var i = this.i
		var j = this.j
		if (i < cols - 1) {
      neighbours = grid(i+1)(j) :: this.neighbours
    }
    if (i > 0) {
      neighbours = grid(i-1)(j) :: this.neighbours
    }
    if (j < rows - 1) {
      neighbours = grid(i)(j+1) :: this.neighbours
    }
    if (j > 0) {
      neighbours = grid(i)(j-1):: this.neighbours
    }
    
    if(i > 0 && j > 0){
      	neighbours = grid(i-1)(j-1) :: this.neighbours
      }
	  if(i < cols - 1 && j > 0){
	  	neighbours = grid(i+1)(j-1) :: this.neighbours
	  }
	  if(i > 0 && j < rows - 1){
	  	neighbours = grid(i-1)(j+1) :: this.neighbours
	  }
	  if(i < cols - 1 && j < rows - 1){
	  	neighbours = grid(i+1)(j+1) :: this.neighbours
	  }
	}

}