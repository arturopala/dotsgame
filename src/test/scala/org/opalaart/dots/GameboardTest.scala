package org.opalaart.dots

import org.junit.Test
import org.junit.Assert._
import org.scalatest.FunSpec

class GameboardTest extends FunSpec {
  
  describe("A Board") {
    
    it ("should have size equal to width*height") {
      val board = new DotsBoard(40,30)
      assert(board.size==30*40)
    }
    it ("should have unique index for all dots") {
      val board = new DotsBoard(40,30)
      assert(board.indexOf(0,0)==0)
      assert(board.indexOf(10,10)==10*30+10)
      assert(board.indexOf(39,29)==1199)
    }
    it ("should locate all dots by unique index") {
      val board = new DotsBoard(40,30)
      assert(board.locate(0)==(0,0))
      assert(board.locate(10*30+10)==(10,10))
      assert(board.locate(1199)==(39,29))
    }
    it ("should have all dots None at start") {
      val board = new DotsBoard(40,30)
      assert(board.count(dot => true)==0)
    }
    it ("should return adjacent of dot") {
      val board = new DotsBoard(40,30)
      val dot = board.dot(2,2)
      val adjacent = dot.adjacent
      adjacent.foreach(dot => assert(dot.color == WHITE))
    }
    it ("should apply return None for location outside board") {
      val board = new DotsBoard(40,30)
      val dot = board(-2,32)
      assert(dot==None,dot)
    }
    it ("should apply return None for index outside board") {
      val board = new DotsBoard(40,30)
      val dot = board(-20)
      assert(dot==None,dot)
    }
    it ("should dot throw Exception for index outside board") {
      val board = new DotsBoard(40,30)
      intercept[IllegalArgumentException]{
    	  val dot = board.dot(-2,32)
      }
    }
  }
  
  describe("A Dot") {
    
    it ("should have adjacent dots") {
      val board = new DotsBoard(40,30)
      val dot = board.dot(10,10)
      assert(dot.adjacent.size==DotsGameRules.adjacent.size)
    }
    it ("should have traversable edges") {
      val board = new DotsBoard(40,30)
      val dot = board.dot(10,10)
      assert(dot.edges.size==DotsGameRules.edges.size)
    }
    it ("should find traversable edge") {
      val board = new DotsBoard(40,30)
      val dot1 = board.dot(10,10)
      val dot2 = board.dot(11,12)
      val dot3 = board.dot(15,15)
      assert(dot1.edgeTo(dot2).isDefined)
      assert(dot1.edgeTo(dot3)==None)
    }
  }
  
  describe("A DotsGameRules") {
    
    it ("should have adjacent set of size 24") {
      assert(DotsGameRules.adjacent.size==24)
    }
    it ("should have traversable set of size 16") {
      assert(DotsGameRules.edges.size==16)
    }
  }
  
  describe("A DotsGame") {
    
    it ("should take first dot with blue and set adjacent to black") {
      val game = new DotsGame(40,30)
      game.board.foreach(dot => assert(dot.color == WHITE))
      game.takeBlue(2,2)
      assert(game.board.dot(2,2).color==BLUE)
      val adjacent = game.board.dot(2,2).adjacent
      adjacent foreach (dot => assert(dot.color==BLACK,dot))
    }
    it ("should take next dot with red and set adjacent to black") {
      val game = new DotsGame(40,30)
      game.takeBlue(2,2)
      assert(game.board.dot(2,2).color==BLUE)
      val adjacent1 = game.board.dot(2,2).adjacent
      adjacent1 foreach (dot => assert(dot.color==BLACK,dot))
      game.takeRed(4,4)
      assert(game.board.dot(2,2).color==BLUE)
      assert(game.board.dot(4,4).color==RED)
      val adjacent2 = game.board.dot(4,4).adjacent
      adjacent2 foreach (dot => assert(dot.color==BLACK || dot.color==BLUE,dot))
    }
    it ("should connect two blue dots with blue edge") {
      val game = new DotsGame(40,30)
      val dot1 = game.takeBlue(2,2)
      dot1.adjacent foreach (dot => assert(dot.color==BLACK,s"$dot should become BLACK"))
      Console.println(dot1.adjacent)
      val dot2 = game.takeBlue(3,4)
      val edge = game.connectBlue(dot1, dot2)
      assert(edge.color==BLUE,"edge color should become blue")
    }
  }

}