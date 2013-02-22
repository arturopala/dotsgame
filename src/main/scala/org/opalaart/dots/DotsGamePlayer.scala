package org.opalaart.dots

import java.net.Socket
import java.io._
import collection.mutable
import scala.Console

object DotsGameRunner extends App {

	val params: Map[String, String] = args.sliding(2) map {
		case a => (a(0), a(1))
	} toMap

	val host = params("-h")
	val port = params("-p").toInt
	val id = params("-g")
	val filename = params("-f")

	val player = new DotsGamePlayer(id, host, port, filename)

}

class DotsGamePlayer(
	                    /** nazwa_hosta, czyli adres docelowy silnika gry */
	                    val id: String,

	                    /** numer_portu, czyli numer portu na kt�rym silnik gry nas?uchuje */
	                    val host: String,

	                    /** numer_gracza, czyli nadany unikalny identyfikator dla gracza. 
	                      * Ka?dy gracz przez ca?y okres rozgrywek b?dzie posiada? w?asny numer identyfikacyjny, b?d?cy liczb? z zakresu <1,65535 > */
	                    val port: Int,

	                    /** nazwa_pliku, czyli plik do kt�rego program mo?e zapisywa? swoje dane w dowolnym formacie 
	                      * o nie przekraczalnym rozmiarze 5MB = 5*1024*1024 bajty, plik b?dzie przekazywany mi?dzy rozgrywkami */
	                    val filename: String = "data.txt"
	                    ) {

	assert(id != null, "id arg should be not null")
	assert(host != null, "host arg should be not null")

	val game = new DotsGame(30, 40)
	val players = mutable.HashMap[String, Color]()
	val previousMoves = mutable.HashSet[String]()

	def playerOf(id: String): Color = players.getOrElseUpdate(id, Player(id))

	def connect: (BufferedWriter, BufferedReader) = {
		try {
			val socket = new Socket(host, port)
			val writer: BufferedWriter = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream()))
			val reader: BufferedReader = new BufferedReader(new InputStreamReader(socket.getInputStream()))
			(writer, reader)
		}
		catch {
			case e: Throwable => {
				Console.println("Fatal error occurred. Cannot continue. Bye!")
				e.printStackTrace()
				System.exit(-1)
				throw e
			}
		}
	}

	def start {
		Console.println("Good luck!")
		val (writer, reader) = connect
		while(true){
			listen(writer, reader)
		}
		Console.println("Game over!")
	}

	def listen(writer: Writer, reader: BufferedReader) {
		try {
			val moves = readMoves(reader, previousMoves)
			applyMoves(moves, game)
			val nextMoves = Seq.newBuilder[Move]
			for (
				(h, w) <- game.chooseNextMoveFor(playerOf(id))
			) {
				nextMoves += MoveDot(id, MovePoint(w + 1, h + 1))
				writeMoves(writer, nextMoves.result, previousMoves)
			}
		}
		catch {
			case e: Throwable => {
				Console.println("Fatal error occurred:")
				e.printStackTrace()
				throw e
			}
		}
	}

	def readMoves(reader: BufferedReader, previousMoves: mutable.HashSet[String]): Seq[Move] = {
		val moves = Seq.newBuilder[Move]
		val size = reader.readLine.trim.toInt;
		for (i <- 0 until size) {
			val line = reader.readLine.trim
			if (line.length > 0 && !previousMoves.contains(line)) {
				previousMoves add line
				val move = (line.head match {
					case 'B' => parsePolygon(line)
					case _ => parseDot(line)
				})
		        if (move.player!=id){
		          moves += move
		        }
			}
		}
		moves.result
	}

	def parseDot(line: String): MoveDot = {
		val s = line.split(' ')
		assert(s.size == 3)
		MoveDot(s(2), MovePoint(s(0).toInt, s(1).toInt))
	}

	def parsePolygon(line: String): MovePolygon = {
		val s = line.split("\\s|\t")
		val size = s(1).toInt
		val points = Seq.newBuilder[MovePoint]
		for (i <- 0 until (size * 2) by 2) {
			points += MovePoint(s(i + 3).toInt, s(i + 4).toInt)
		}
		MovePolygon(s(2), points.result)
	}

	def applyMoves(moves: Seq[Move], game: DotsGame): Unit = {
		moves foreach {
			case MoveDot(id, MovePoint(x, y)) => game.take(y - 1, x - 1, playerOf(id));
			case MovePolygon(id, points) => game.connect(points.map{case MovePoint(x,y) => (y - 1,x - 1)}, playerOf(id))
		}
	}

	def writeMoves(writer: Writer, moves: Seq[Move], previousMoves: mutable.HashSet[String]): Unit = {
		moves foreach {
			case MoveDot(id, MovePoint(x, y)) => writePoint(writer, x, y)
			case MovePolygon(id, points) => {
				writer.write(points.size.toString)
				writer.write("\n")
				points foreach {
					case MovePoint(x, y) =>  writePoint(writer, x, y)
				}
			}
		}
		writer.flush()
	}


	def writePoint(writer: Writer, x: Int, y: Int) {
		writer.write((y + 1).toString)
		writer.write(" ")
		writer.write((x + 1).toString)
		writer.write("\n")
		writer.flush()
	}
}

trait Move {
  val player: String
}

case class MovePoint(x: Int, y: Int)

case class MoveDot(player: String, point: MovePoint) extends Move

case class MovePolygon(player: String, points: Seq[MovePoint]) extends Move
