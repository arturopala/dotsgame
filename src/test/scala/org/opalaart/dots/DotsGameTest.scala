package org.opalaart.dots

import org.scalatest.FunSpec
import java.net.ServerSocket
import java.io._
import collection.mutable

class DotsGameTest extends FunSpec {

	describe("A Board") {

		it("should have size equal to width*height") {
			val board = new DotsBoard(40, 30)
			assert(board.size == 30 * 40)
		}
		it("should have unique index for all dots") {
			val board = new DotsBoard(40, 30)
			assert(board.indexOf(0, 0) == 0)
			assert(board.indexOf(10, 10) == 10 * 30 + 10)
			assert(board.indexOf(39, 29) == 1199)
		}
		it("should locate all dots by unique index") {
			val board = new DotsBoard(40, 30)
			assert(board.locate(0) ==(0, 0))
			assert(board.locate(10 * 30 + 10) ==(10, 10))
			assert(board.locate(1199) ==(39, 29))
		}
		it("should have all dots None at start") {
			val board = new DotsBoard(40, 30)
			assert(board.count(dot => true) == 0)
		}
		it("should return adjacent of dot") {
			val board = new DotsBoard(40, 30)
			val dot = board.dot(2, 2)
			val adjacent = dot.adjacent
			adjacent.foreach(dot => assert(dot.color == WHITE))
		}
		it("should apply return None for location outside board") {
			val board = new DotsBoard(40, 30)
			val dot = board(-2, 32)
			assert(dot == None, dot)
		}
		it("should apply return None for index outside board") {
			val board = new DotsBoard(40, 30)
			val dot = board(-20)
			assert(dot == None, dot)
		}
		it("should dot throw Exception for index outside board") {
			val board = new DotsBoard(40, 30)
			intercept[IllegalArgumentException] {
				val dot = board.dot(-2, 32)
			}
		}
	}

	describe("A Dot") {

		it("should have adjacent dots") {
			val board = new DotsBoard(40, 30)
			val dot = board.dot(10, 10)
			assert(dot.adjacent.size == DotsGameRules.moves.size)
		}
		it("should have traversable edges") {
			val board = new DotsBoard(40, 30)
			val dot = board.dot(10, 10)
			assert(dot.edges.size == DotsGameRules.edges.size)
		}
		it("should find traversable edge") {
			val board = new DotsBoard(40, 30)
			val dot1 = board.dot(10, 10)
			val dot2 = board.dot(11, 12)
			val dot3 = board.dot(15, 15)
			assert(dot1.edgeTo(dot2).isDefined)
			assert(dot1.edgeTo(dot3) == None)
		}
	}

	describe("A DotsGameRules") {

		import DotsGameRules._

		it("should have moves set of size 24") {
			assert(moves.size == 24)
		}
		it("should have edges set of size 16") {
			assert(edges.size == 16)
		}
		it("should have crossings") {
			assert(crossings.size == 16)
			assert(edges.forall(edge => crossings.get(edge).isDefined))
			assert(crossings((1, 0)).size == 2)
			assert(crossings((1, 1)).size == 5)
			assert(crossings((1, 2)).size == 9)
			assert(crossings((2, 1)).size == 9)
			assert(crossings(r(1, 0)).size == 2)
			assert(crossings(r(1, 1)).size == 5)
			assert(crossings(r(1, 2)).size == 9)
			assert(crossings(r(2, 1)).size == 9)
		}
	}

	describe("A DotsGame") {

		it("should take first dot with blue and set adjacent to black") {
			val game = new DotsGame(40, 30)
			game.board.foreach(dot => assert(dot.color == WHITE))
			game.takeBlue(2, 2)
			assert(game.board.dot(2, 2).color == BLUE)
			val adjacent = game.board.dot(2, 2).adjacent
			adjacent foreach (dot => assert(dot.color == BLACK, dot))
		}
		it("should take next dot with red and set adjacent to black") {
			val game = new DotsGame(40, 30)
			game.takeBlue(2, 2)
			assert(game.board.dot(2, 2).color == BLUE)
			val adjacent1 = game.board.dot(2, 2).adjacent
			adjacent1 foreach (dot => assert(dot.color == BLACK, dot))
			game.takeRed(4, 4)
			assert(game.board.dot(2, 2).color == BLUE)
			assert(game.board.dot(4, 4).color == RED)
			val adjacent2 = game.board.dot(4, 4).adjacent
			adjacent2 foreach (dot => assert(dot.color == BLACK || dot.color == BLUE, dot))
		}
		it("should connect two blue dots with blue edge") {
			val game = new DotsGame(40, 30)
			val dot1 = game.takeBlue(2, 2)
			assert(dot1.color == BLUE, s"$dot1 should be BLUE")
			dot1.adjacent foreach (dot => assert(dot.color == BLACK, s"adjacent $dot should be BLACK"))
			val dot2 = game.takeBlue(4, 4)
			assert(dot2.color == BLUE, s"$dot2 should be BLUE")
			val edge1 = game.connectBlue(dot1, dot2)
			assert(edge1.taken == true, s"$edge1 should be taken")
			assert(edge1.color == BLUE, s"$edge1 should be BLUE")
			val dot3 = game.board.dot(3, 3)
			assert(dot3.color == BLUE, s"$dot3 should be BLUE")
			val edge2 = dot3.edgeTo(dot2).get
			assert(edge2.taken == true, s"$edge2 should be taken")
			assert(edge2.color == BLUE, s"$edge2 should be BLUE")
		}
		it("should not connect two red dots across blue edge") {
			val game = new DotsGame(40, 30)
			val dot1 = game.takeBlue(2, 2)
			val dot2 = game.takeBlue(4, 4)
			game.connectBlue(dot1, dot2)
			val dot3 = game.board.dot(3, 3)
			dot3.edgeTo(dot2).get
			val dot4 = game.takeRed(3, 2)
			val dot5 = game.takeRed(2, 3)
			intercept[AssertionError] {
				game.connectRed(dot4, dot5)
			}
		}
	}

	describe("A DotsGameRunner") {
		it("should create player with proper args") {
			val id = "12345"
			val host = "localhost"
			val port = 6060
			val filename = "data.txt"
			DotsGameRunner.main(Array("-g", id, "-h", host, "-p", port.toString, "-f", filename))
			val player = DotsGameRunner.player
			assert(player.id == id)
			assert(player.host == host)
			assert(player.port == port)
			assert(player.filename == filename)
		}
	}

	describe("A DotsGamePlayer") {
		val id = "12345"
		val host = "localhost"
		val port = 6060
		val question = "How do you do?"
		val answer = "How do you do."
		val runnable = new Runnable {
			override def run {
				val serverSocket = new ServerSocket(port)
				val socket = serverSocket.accept
				val writer: BufferedWriter = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream()))
				val reader: BufferedReader = new BufferedReader(new InputStreamReader(socket.getInputStream()))
				val line = reader.readLine()
				assert(line == question)
				writer.write(answer + "\r\n")
				writer.flush()
			}
		}
		it("should connect to the game server and exchange messages") {
			val server = new Thread(runnable)
			server.setDaemon(true)
			server.start
			Thread.sleep(200)
			val player = new DotsGamePlayer(id, host, port)
			val (writer, reader) = player.connect
			assert(writer != null)
			assert(reader != null)
			writer.write(question + "\r\n")
			writer.flush()
			val line = reader.readLine()
			assert(line == answer)
		}
		it("should parse dot move line") {
			val player = new DotsGamePlayer(id, host, port)
			val (x, y) = (12, 29)
			val dot = player.parseDot(s"$x $y $id")
			assert(dot.point.x == x)
			assert(dot.point.y == y)
			assert(dot.player == id)
		}
		it("should parse polygon move line") {
			val player = new DotsGamePlayer(id, host, port)
			val points = Set((12, 29), (23, 33), (18, 40), (13, 31))
			val line = s"B ${points.size} $id" + points.foldLeft("") {
				case (s, (x, y)) => s + "\t" + x + " " + y
			}
			val polygon = player.parsePolygon(line)
			assert(polygon.player == id)
			assert(polygon.points.size == points.size, s"$polygon size should be ${points.size}")
		}
		it("should read all moves") {
			val player = new DotsGamePlayer(id, host, port)
			val previous = mutable.HashSet[String]()
			val moves =
				"""4
				  |12 13 2
				  |14 15 1
				  |16 17 2
				  |B 3 2 12 13 14 15 16 17
				  |""".stripMargin
			val reader = new BufferedReader(new StringReader(moves))
			val newMoves = player.readMoves(reader, previous)
			assert(newMoves.size == 4, "new moves size should be 4")
			assert(newMoves(0) == MoveDot("2", MovePoint(12, 13)))
			assert(newMoves(1) == MoveDot("1", MovePoint(14, 15)))
			assert(newMoves(2) == MoveDot("2", MovePoint(16, 17)))
		}
		it("should read only new moves from other players") {
			val player = new DotsGamePlayer(id, host, port)
			val previous = mutable.HashSet[String]("12 13 2", "B 3 2 12 13 14 15 16 17")
			val moves =
        """4
          |12 13 2
          |14 15 2
          |16 17 1
          |20 20 12345
				  |B 3 2 12 13 14 15 16 17
        	|""".stripMargin
			val reader = new BufferedReader(new StringReader(moves))
			val newMoves = player.readMoves(reader, previous)
			assert(newMoves.size == 2, "new moves size should be 2")
			assert(newMoves(0) == MoveDot("2", MovePoint(14, 15)))
			assert(newMoves(1) == MoveDot("1", MovePoint(16, 17)))
		}
		it("should apply sequence of moves") {
			val player = new DotsGamePlayer(id, host, port)
			val game = new DotsGame(30, 40)
			val moves = Seq(
				MoveDot("2", MovePoint(10, 20)),
				MoveDot("1", MovePoint(11, 21)),
				MoveDot("2", MovePoint(9, 18)),
				MovePolygon("2", Seq(MovePoint(10, 20), MovePoint(9, 18)))
			)
			player.applyMoves(moves, game)
			val p1 = player.playerOf("1")
			val p2 = player.playerOf("2")
			val dot1 = game.board.dot(19, 9)
			val dot2 = game.board.dot(17, 8)
			assert(dot1.color == p2)
			assert(dot2.color == p2)
			assert(game.board.dot(20, 10).color == p1)
			val edge1 = dot1.edgeTo(dot2).get
			assert(edge1.color == p2)
			assert(edge1.taken)
		}
		it("should write move point to the stream") {
			val player = new DotsGamePlayer(id, host, port)
			val writer = new StringWriter
			player.writePoint(writer,7,13)
			assert(writer.getBuffer.toString=="14 8\n")
		}
		it("should write moves to the stream") {
			val player = new DotsGamePlayer(id, host, port)
			val writer = new StringWriter
			val moves = Seq(
				MoveDot("2", MovePoint(10, 20)),
				MovePolygon("2", Seq(MovePoint(10, 20), MovePoint(9, 18)))
			)
      val previous = mutable.HashSet[String]() 
			player.writeMoves(writer,moves,previous)
      val expected = """21 11
         |2
         |21 11
         |19 10
         |""".stripMargin
      val response = writer.getBuffer.toString
			assert(response == expected)
		}
    it("should listen and respond") {
      val player = new DotsGamePlayer(id, host, port)
      val writer = new StringWriter
      val moves =
        """4
          |12 13 2
          |14 15 2
          |16 17 2
          |B 3 2 12 13 14 15 16 17
      """.stripMargin
      val reader = new BufferedReader(new StringReader(moves))
      player.listen(writer,reader)
      val board = player.game.board
      val response = writer.getBuffer.toString
      assert(!response.isEmpty)
      assert(board.dot(12,11).color==player.playerOf("2"))
    }
	}

}