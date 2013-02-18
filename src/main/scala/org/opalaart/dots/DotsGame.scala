package org.opalaart.dots

trait Color {
  override def toString:String = super.toString.split('.').last.split('$').head+")"
}

object WHITE extends Color
object BLACK extends Color
object RED extends Color
object BLUE extends Color
object GRAY extends Color

case class Dot(h:Int, w:Int, private val board:DotsBoard){
  var color:Color = WHITE
  
  lazy val index = board.indexOf(h,w)
  lazy val adjacent:Set[Dot] = board.adjacentOf(h,w) 
  lazy val edges:Set[Edge] = board.adjacentOf(h,w) map (board.createEdge(this,_)) 
  
  def edgeTo(target:Dot):Option[Edge] = edges.find(edge => edge.target==target)

  def adjustEdges {edges foreach (_.adjust)}
  
  override def toString:String = "Dot("+w+","+h+","+color+")"
}

case class Edge(source:Dot, target:Dot, var color:Color = WHITE, var taken:Boolean = false){
  
  def adjust {
    if(color==WHITE){
      if(source.color==target.color) {
	      color = source.color
      } else if(source.color!=WHITE && source.color!=BLACK && target.color!=WHITE && target.color!=BLACK){
		  if(source.color!=target.color) color = GRAY
	  }
    }
	adjustSymmetricEdge
  }
	
  def adjustSymmetricEdge {
	  val symmetricEdge = target.edgeTo(source).get
	  symmetricEdge.color = color
	  symmetricEdge.taken = taken
  }
}

class DotsBoard(height:Int, width:Int) extends Traversable[Dot]{
  
  private val dots = new Array[Array[Option[Dot]]](height)
  private val edges = scala.collection.mutable.ArrayBuffer[Edge]()
  
  //board arrays initialization with None
  for(h <- 0 until height){
    dots(h) = new Array[Option[Dot]](width)
    for(w <- 0 until width) dots(h)(w) = None
  }
  
  def apply(h:Int,w:Int):Option[Dot] = {
    if(h<0 || h>=height || w<0 || w>=width) None
    else dots(h)(w) orElse {
    	val dotOpt = Some(createDot(h,w))
    	dots(h)(w) = dotOpt
    	dotOpt
    }
  }
  
  def apply(p:(Int,Int)):Option[Dot] = apply(p._1,p._2)
  
  def apply(index:Int):Option[Dot] = {
    if(index<0 || index>=size) None
    else this(locate(index))
  }
  
  def dot(h:Int,w:Int) = this(h,w) getOrElse {
    throw new IllegalArgumentException("("+h+","+w+")")
  }
  
  def indexOf(h:Int,w:Int) = h*width + w
  def locate(index:Int):(Int,Int) = (index/width,index%width)
  
  def adjacentOf(h:Int,w:Int):Set[Dot] = DotsGameRules.moves map {
    case (dh,dw) => this(h+dh,w+dw) 
  } filter (_.isDefined) map (_.get)
  
  def createDot(h:Int,w:Int):Dot = Dot(h,w,this)
  
  def createEdge(source:Dot,target:Dot):Edge = {
    val edge = Edge(source,target)
    edges += edge
    edge
  }
  
  //extends trait Traversable
  override val size = width*height
  def foreach[U](f: Dot => U): Unit = {
    for(h <- 0 until height; w <- 0 until width) dots(h)(w).map(f)
  }
  
}

object DotsGameRules {
  
  val moves:Set[(Int,Int)] = Set(
      (-1,-1), (-2,0), (0,2), (2,-1), 
      (-2,1), (2,0), (1,-2), (1,1), (0,-2), 
      (1,-1), (-1,0), (-2,-1), (2,2), (-1,2), 
      (0,1), (-2,2), (-1,1), (1,2), (-2,-2), 
      (2,1), (1,0), (-1,-2), (0,-1), (2,-2)
  )

}

class DotsGame(height:Int,width:Int) {
  
  val board = new DotsBoard(height,width)
  
  private var counter=0;
  
  def take(h:Int,w:Int,color:Color):Dot = {
    val dot = board.dot(h,w)
    assert(counter==0 || dot.color==BLACK, s"$dot should be BLACK")
    counter = counter+1
    dot.color = color
    dot.adjacent filter (_.color==WHITE) foreach (_.color=BLACK)
    dot.adjustEdges
    dot
  }
  
  def takeRed(h:Int,w:Int) = take(h,w,RED)
  def takeBlue(h:Int,w:Int) = take(h,w,BLUE)
  
  def connect(h1:Int,w1:Int,h2:Int,w2:Int,color:Color):Edge = {
    val source = board.dot(h1,w1)
    assert(source.color==color, s"$source should be $color")
    val target = board.dot(h2,w2)
    assert(target.color==color, s"target should be $color")
    connect(source,target,color)
  }
  
  def connect(source:Dot,target:Dot,color:Color):Edge = {
    val edge = source.edgeTo(target).getOrElse(
        throw new IllegalArgumentException(s"$target not reachable from $source")
    )
    assert(edge.color==color, s"$edge should be $color")
    //TODO check crossings
    edge.taken = true
	edge.adjustSymmetricEdge
    edge
  }
  
  def connectRed(source:Dot,target:Dot) = connect(source,target,RED)
  def connectBlue(source:Dot,target:Dot) = connect(source,target,BLUE)
  
}