package org.opalaart.dots

import collection.mutable.ArrayBuffer

trait Color {
	override def toString: String = super.toString.split('.').last.split('$').head + ")"
}

object WHITE extends Color
object BLACK extends Color
object RED extends Color
object BLUE extends Color
object GRAY extends Color

case class Player(id:String) extends Color {
  override def toString: String = s"Player($id)"
}

case class Dot(h: Int, w: Int, private val board: DotsBoard) {
	var color: Color = WHITE

	lazy val index = board.indexOf(h, w)
	lazy val adjacent: Set[Dot] = board.adjacentOf(h, w)
	lazy val edges: Set[Edge] = board.targetsOf(h, w) map (board.createEdge(this, _))

	def vectorTo(target: Dot): Vector = (target.h-h,target.w-w)
	def dotAtOffset(vector: Vector): Option[Dot] = board(h+vector._1,w+vector._2)
	def edgeTo(target: Dot): Option[Edge] = edges.find(edge => edge.target == target)

	def adjustEdges {edges foreach (_.adjust)}

	override def toString: String = "Dot(" + w + "," + h + "," + color + ")"
}

case class Edge(source: Dot, target: Dot, var color: Color = WHITE, var taken: Boolean = false) {
    
	def adjust {
		if (color==WHITE || color==BLACK) {
			if (source.color == target.color) {
				color = source.color
			} else if (source.color != WHITE && source.color != BLACK && target.color != WHITE && target.color != BLACK) {
				if (source.color != target.color) color = GRAY
			}
		}
		adjustSymmetricEdge
	}

	def adjustSymmetricEdge {
		val symmetricEdge = target.edgeTo(source).get
		symmetricEdge.color = color
		symmetricEdge.taken = taken
	}
	
	def markCrossings {
	    val vector:Vector = source.vectorTo(target)
	    val crossings = DotsGameRules.crossings(vector)
	    crossings foreach {
	        case (v1,v2) => {
	           source.dotAtOffset(v1) match {
	                 case Some(dot1) => {
	                     dot1.dotAtOffset(v2) match {
	                         case Some(dot2) => {
	                           val edge = dot1.edgeTo(dot2).get
	                           if(edge.color==WHITE || edge.color==BLACK || (color!=GRAY && !taken)){
	                              edge.color = GRAY  
	                           } 	   
	                         }
	                         case None => Unit
	                     }
	                 }
	                 case None => Unit
	           }
	        }
	    }
	}
	
	def checkCrossings:Boolean = {
	    val vector:Vector = source.vectorTo(target)
	    val crossings = DotsGameRules.crossings(vector)
	    crossings forall {
	        case (v1,v2) => {
	           source.dotAtOffset(v1) match {
	                 case Some(dot1) => {
	                     dot1.dotAtOffset(v2) match {
	                         case Some(dot2) => {
	                           val edge = dot1.edgeTo(dot2).get
	                           !edge.taken	   
	                         }
	                         case None => true
	                     }
	                 }
	                 case None => true
	           }
	        }
	    }
	}
}

case class Polygon(vertices:Seq[Dot],color:Color)

class DotsBoard(height: Int, width: Int) extends Traversable[Dot] {

	private val dots = new Array[Array[Option[Dot]]](height)
  val polygons = new ArrayBuffer[Polygon]

	//board arrays initialization with None
	for (h <- 0 until height) {
		dots(h) = new Array[Option[Dot]](width)
		for (w <- 0 until width) dots(h)(w) = None
	}

	def apply(h: Int, w: Int): Option[Dot] = {
		if (h < 0 || h >= height || w < 0 || w >= width) None
		else dots(h)(w) orElse {
			val dotOpt = Some(createDot(h, w))
			dots(h)(w) = dotOpt
			dotOpt
		}
	}

	def apply(p: Vector): Option[Dot] = apply(p._1, p._2)

	def apply(index: Int): Option[Dot] = {
		if (index < 0 || index >= size) None
		else this(locate(index))
	}

	def dot(h: Int, w: Int) = this(h, w) getOrElse {
		throw new IllegalArgumentException("(" + h + "," + w + ")")
	}

	def indexOf(h: Int, w: Int) = h * width + w

	def locate(index: Int): Vector = (index / width, index % width)

	def adjacentOf(h: Int, w: Int): Set[Dot] = DotsGameRules.moves map {
		case (dh, dw) => this(h + dh, w + dw)
	} filter (_.isDefined) map (_.get)

	def targetsOf(h: Int, w: Int): Set[Dot] = DotsGameRules.edges map {
		case (dh, dw) => this(h + dh, w + dw)
	} filter (_.isDefined) map (_.get)

	def createDot(h: Int, w: Int): Dot = Dot(h, w, this)

	def createEdge(source: Dot, target: Dot): Edge = Edge(source, target)

	//extends trait Traversable
	override val size = width * height

	def foreach[U](f: Dot => U): Unit = {
		for (h <- 0 until height; w <- 0 until width) dots(h)(w).map(f)
	}

}

object DotsGameRules {

	val moves: Set[Vector] = Set(
		(-1, -1), (-2, 0), (0, 2), (2, -1),
		(-2, 1), (2, 0), (1, -2), (1, 1), (0, -2),
		(1, -1), (-1, 0), (-2, -1), (2, 2), (-1, 2),
		(0, 1), (-2, 2), (-1, 1), (1, 2), (-2, -2),
		(2, 1), (1, 0), (-1, -2), (0, -1), (2, -2)
	)

	val edges: Set[Vector] = Set(
		(-1, -1), (2, -1), (-1, 2), (-2, 1), (1, -2), (1, 1),
		(1, -1), (-1, 0), (-2, -1), (0, 1), (-1, 1), (1, 2),
		(2, 1), (1, 0), (-1, -2), (0, -1)
	)

	val doubles: Set[Vector] = Set(
		(-2, 0), ( 0, 2), ( 2, 0), ( 0,-2), ( 2, 2),(-2, 2), (-2,-2), ( 2,-2)
	)
	
	val crossings: Map[Vector,Set[(Vector,Vector)]] = Map(
	        (-1,-1) -> Set(((0,-1),(-2,1)), ((-1,0),(1,-2)), ((-1,0),(1,-1)), ((-1,0),(2,-1)), ((0,-1),(-1,2))), 
	        ( 2,-1) -> Set(((1,-1),(1,1)), ((2,0),(-1,-2)), ((1,0),(-1,-2)), ((1,-1),(2,1)), ((0,-1),(1,1)), ((1,0),(1,2)), ((0,-1),(2,1)), ((0,-1),(1,2)), ((1,-1),(1,2))), 
	        (-2, 1) -> Set(((-1,0),(-1,-2)), ((-1,1),(-2,-1)), ((0,1),(-2,-1)), ((-2,0),(1,2)), ((0,1),(-1,-2)), ((-1,0),(1,2)), ((-1,1),(-1,-1)), ((-1,1),(-1,-2)), ((0,1),(-1,-1))), 
	        ( 1,-2) -> Set(((1,0),(-1,-2)), ((1,-1),(-1,-2)), ((0,-1),(2,1)), ((1,0),(-1,-1)), ((1,-1),(-2,-1)), ((1,-1),(-1,-1)), ((0,-1),(-2,-1)), ((1,0),(-2,-1)), ((0,-2),(2,1))), 
	        ( 1, 1) -> Set(((1,0),(-1,1)), ((1,0),(-2,1)), ((0,1),(1,-2)), ((1,0),(-1,2)), ((0,1),(2,-1))), 
	        ( 1,-1) -> Set(((1,0),(-1,-2)), ((0,-1),(1,1)), ((0,-1),(2,1)), ((0,-1),(1,2)), ((1,0),(-2,-1))), 
	        (-1, 0) -> Set(((0,1),(-1,-2)), ((0,-1),(-1,2))), 
	        (-2,-1) -> Set(((0,-1),(-2,1)), ((-1,0),(-1,2)), ((-1,0),(1,-2)), ((-2,0),(1,-2)), ((-1,-1),(-2,1)), ((-1,-1),(-1,2)), ((-1,-1),(-1,1)), ((0,-1),(-1,2)), ((0,-1),(-1,1))), 
	        (-1, 2) -> Set(((-1,0),(1,1)), ((-1,0),(2,1)), ((0,2),(-2,-1)), ((0,1),(2,1)), ((0,1),(-2,-1)), ((-1,0),(1,2)), ((-1,1),(2,1)), ((-1,1),(1,2)), ((-1,1),(1,1))), 
	        ( 0, 1) -> Set(((1,0),(-2,1)), ((-1,0),(2,1))), 
	        (-1, 1) -> Set(((-1,0),(2,1)), ((0,1),(-2,-1)), ((0,1),(-1,-2)), ((-1,0),(1,2)), ((0,1),(-1,-1))), 
	        ( 1, 2) -> Set(((1,0),(-1,1)), ((0,2),(2,-1)), ((1,1),(-1,2)), ((1,1),(-1,1)), ((1,1),(-2,1)), ((0,1),(-2,1)), ((1,0),(-2,1)), ((1,0),(-1,2)), ((0,1),(2,-1))), 
	        ( 2, 1) -> Set(((2,0),(-1,2)), ((1,1),(1,-1)), ((0,1),(1,-1)), ((1,1),(1,-2)), ((0,1),(1,-2)), ((1,0),(-1,2)), ((1,1),(2,-1)), ((1,0),(1,-2)), ((0,1),(2,-1))), 
	        ( 1, 0) -> Set(((0,-1),(1,2)), ((0,1),(1,-2))), 
	        (-1,-2) -> Set(((-1,-1),(1,-2)), ((0,-1),(-2,1)), ((-1,0),(1,-2)), ((-1,0),(1,-1)), ((-1,0),(2,-1)), ((0,-2),(-2,1)), ((0,-1),(2,-1)), ((-1,-1),(1,-1)), ((-1,-1),(2,-1))), 
	        ( 0,-1) -> Set(((-1,0),(2,-1)), ((1,0),(-2,-1)))
	)
	
	private[dots] def r(v:Vector):Vector = (v._2,-v._1)
	
	/*
	private[dots] def rs(s:Set[Vector]):Set[Vector] = s map r
	private[dots] def rst(s:Set[(Vector,Vector)]):Set[(Vector,Vector)] = s map {case (a,b) => (r(a),r(b))}
	private[dots] def rmp(m:Map[Vector,Set[(Vector,Vector)]]):Map[Vector,Set[(Vector,Vector)]] = m map {case (k,v) => (r(k),rst(v))}
	private[dots] def m(v:Vector):Vector = (v._2,v._1)
	private[dots] def mst(s:Set[(Vector,Vector)]):Set[(Vector,Vector)] = s map {case (a,b) => (m(a),m(b))}
	private val cs1 = Set(((0,-1),(1,2)), ((0,1),(1,-2)))
	private val cs2 = Set(((1,0),(-1,1)), ((1,0),(-1,2)), ((1,0),(-2,1)), ((0,1),(1,-2)), ((0,1),(2,-1)))
	private val cs3 = Set(((0,1),(2,-1)), ((0,1),(1,-1)), ((1,1),(1,-1)), ((0,1),(1,-2)), ((1,0),(1,-2)), ((1,0),(-1,2)), ((1,1),(1,-2)), ((1,1),(2,-1)), ((2,0),(-1,2)))
	private val csm1 = Map((1,0) -> cs1, (1,1) -> cs2, (2,1) -> cs3, (1,2) -> mst(cs3))
	private val csm2 = rmp(csm1)
	private val csm3 = rmp(csm2)
	private val csm4 = rmp(csm3)
	val crossings: Map[Vector,Set[(Vector,Vector)]] = csm1 ++ csm2 ++ csm3 ++ csm4
	*/
}

class DotsGame(height: Int, width: Int) {

	val board = new DotsBoard(height, width)

	private var counter = 0;

	def take(h: Int, w: Int, color: Color): Dot = {
		val dot = board.dot(h, w)
		take(dot,color)
	}

	def take(dot: Dot, color: Color): Dot = {
		assert(counter == 0 || dot.color == BLACK || dot.color==color, s"$dot should be BLACK or $color")
		counter = counter + 1
		dot.color = color
		dot.adjacent filter (_.color == WHITE) foreach {dot =>
			dot.color = BLACK
			dot.adjustEdges
		}
		dot.adjustEdges
		dot
	}

	def takeRed(h: Int, w: Int) = take(h, w, RED)
	def takeBlue(h: Int, w: Int) = take(h, w, BLUE)

	def connect(h1: Int, w1: Int, h2: Int, w2: Int, color: Color): Edge = {
		val source = board.dot(h1, w1)
		assert(source.color == color, s"$source should be $color")
		val target = board.dot(h2, w2)
		assert(target.color == color, s"$target should be $color")
		connect(source, target, color)
	}

	def connect(source: Dot, target: Dot, color: Color): Edge = {
		val vector = source.vectorTo(target)
		if (DotsGameRules.doubles.contains(vector)){
			val middle = source.dotAtOffset((vector._1/2,vector._2/2)).get
			take(middle,color)
			connect(middle,target,color)
			connect(source,middle,color)
		} else {
			val edge = source.edgeTo(target).getOrElse(
				throw new IllegalArgumentException(s"$target not reachable from $source")
			)
			assert(edge.color == color, s"$edge should be $color")
			edge.taken = true
			edge.markCrossings
			edge.adjustSymmetricEdge
			edge
			
		}
	}

	def connectRed(source: Dot, target: Dot) = connect(source, target, RED)
	def connectBlue(source: Dot, target: Dot) = connect(source, target, BLUE)
  
  def connect(points:Seq[(Int,Int)],color:Color) {
    val vertices:Seq[Dot] = points map {case (h,w) => board.dot(h,w)}
    vertices sliding(2,1) foreach {
      dots => connect(dots(0),dots(1),color)
    }
    val polygon = Polygon(vertices,color)
    board.polygons += polygon
  }
  
  def chooseNextMoveFor(color:Color):Option[(Int,Int)] = {
    for (
      dot <- board find (dot => dot.color==BLACK)
    ) {
      take(dot,color)
      return Some((dot.h,dot.w))
    }
    None
  }

}