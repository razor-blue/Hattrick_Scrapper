import com.github.tototoshi.csv.CSVWriter

import java.io.{File, FileNotFoundException}
import com.github.tototoshi.csv.*
import org.jsoup.nodes.{Document, Element}
import org.jsoup.select.Elements
import org.jsoup.{Connection, Jsoup}

import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import java.time.Duration
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.time.DayOfWeek
import java.text.SimpleDateFormat
import java.util.{Calendar, Date}
import scala.collection.{immutable, mutable}
import scala.io.BufferedSource
import scala.util.{Random, Success}
import scala.util.control.Breaks.*
import scala.concurrent.{Await, Future}
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

def JsoupConnection(url: String): Document = {
  val connection: Connection = Jsoup.connect(url).timeout(10000)
  connection.get()
}

//ok
def tryBufferedSource(pathToFile: String): Option[BufferedSource] = {

  try {
    Some(io.Source.fromFile(pathToFile))
  } catch {
    case _: FileNotFoundException => None
  }

}

//ok
def writeToFile(path: String, appendFlag: Boolean, headline: Seq[String], lines: Seq[String]): Unit = {

  val file = new File(path)
  val writer = CSVWriter.open(file, appendFlag)
  if(!appendFlag)writer.writeRow(headline)
  lines.foreach(x => if (x != null) writer.writeRow(Seq(x)))
  writer.close()

}

val teamPath = "https://www.hattrick.org/Club/?TeamID="
val databasePath: String = "src/data/"
val youthTeamPath = "https://www.hattrick.org/Club/Players/YouthPlayers.aspx?YouthTeamID="
val leaguePath = "https://www.hattrick.org/World/Series/?LeagueLevelUnitID="
val seniorPlayerPath = "https://www.hattrick.org/pl/Club/Players/Player.aspx?PlayerID="
val youthPlayerPath = "https://www.hattrick.org/pl/Club/Players/YouthPlayer.aspx?YouthPlayerID="

def headline: Seq[String] = Seq("Player,Player ID,Age,Speciality,Days in Academy,WC X,Stage N,Description,Last Match Date,Season,Week,B_GK,B_CD,B_WB,B_IM,B_W,B_F,L_GK,L_CD,L_WB,L_IM,L_W,L_F,Last Match Details,Country,Last update,Usposobienie(test)")

/*def read_config_db: Int = {

  val config_db_path: String = databasePath + "config_db.dat"

  val bufferedSource: Option[BufferedSource] = tryBufferedSource(config_db_path)

  val firstPlayerID = bufferedSource match {
    case Some(source) =>
        if(source.hasNext)
          val startingPlayerID: Int = source.getLines.toList.head.split(" ").head.toInt
          source.close()
          startingPlayerID
        else
          println(s"No player id in config file. Starting from default value: 320411270")
          320411270

    case None =>
      println(s"File $config_db_path does not exists.")
      println(s"Default starting player id: 320411270.")
      320411270

  }


  //println(s"Starting id: $firstPlayerID")

  firstPlayerID

}*/


object YouthDatabase {

  //ok
  val DatabasePath: Map[String, String] = Map(
    "test-TL'a" -> {databasePath + "TL_test.csv"},
    "678445" -> {databasePath + "FCB_youthPlayerDatabase.csv"},
    "2955119" -> {databasePath + "luka_w_systemie_youthPlayerDatabase.csv"},
    "Polska" -> {databasePath + "Polska_youthPlayerDatabase.csv"},
    "Kenia" -> {databasePath + "Kenia_youthPlayerDatabase1-4L.csv"},
    "World" -> {databasePath + "World_youthPlayerDatabase.csv"},
    "Ligi_1-4" -> {databasePath + "Polska_youthPlayerDatabase1-4L.csv"},
    "5 Liga" -> {databasePath + "Polska_youthPlayerDatabase_5L.csv"},
    "6 Liga 1-256" -> {databasePath + "Polska_youthPlayerDatabase_6L_1_256.csv"},
    "7 Liga 1-256" -> {databasePath + "Polska_youthPlayerDatabase_7L_1_256.csv"},
    "6 Liga 257-512" -> {databasePath + "Polska_youthPlayerDatabase_6L_257_512.csv"},
    "7 Liga 257-512" -> {databasePath + "Polska_youthPlayerDatabase_7L_257_512.csv"},
    "6 Liga 513-768" -> {databasePath + "Polska_youthPlayerDatabase_6L_513_768.csv"},
    "7 Liga 513-768" -> {databasePath + "Polska_youthPlayerDatabase_7L_513_768.csv"},
    "6 Liga 769-1024" -> {databasePath + "Polska_youthPlayerDatabase_6L_769_1024.csv"},
    "7 Liga 769-1024" -> {databasePath + "Polska_youthPlayerDatabase_7L_769_1024.csv"}
  )

  def doSthWithDatabase(pathToCsvFile: String, label: String): Any = {

    label match{

      case "clearWrongSpecialityStatus" =>

        val bufferedSource: Option[BufferedSource] = tryBufferedSource(pathToCsvFile)

        bufferedSource match {
          case Some(source) =>

            val updateRecords: mutable.Builder[String, Seq[String]] = Seq.newBuilder[String]
            var counter = 0

            val dataLines: Iterator[String] = source.getLines.drop(1)

            for (line <- dataLines) {

              val cols: Array[String] = line.split(",").map(_.trim)
              val lineBefore = cols.take(3).mkString(",")
              val lineAfter = cols.drop(4).mkString(",")
              val speciality: String = cols(3)
              val since: String = cols(4)

              val correctedSpeciality: String = if(speciality.equals(since)) "" else speciality
              val newRecord: String = lineBefore ++ "," ++ correctedSpeciality ++ "," ++ lineAfter


              updateRecords += newRecord.replaceAll("\"", "")

              counter += 1
              if (counter % 100 == 0) {
                val updatedRecords = updateRecords.result()
                writeToFile("src/data/tttest.csv", true, headline, updatedRecords)
                updateRecords.clear()
              }

            }

            val updatedRecords = updateRecords.result()
            println(counter)

            writeToFile("src/data/tttest.csv", true, headline, updatedRecords)

            updateRecords.clear()

            source.close()


          case None =>
            println(s"File $pathToCsvFile does not exists.")

        }

      case "removePlayersThatLeftAcademy" =>

        val bufferedSource: Option[BufferedSource] = tryBufferedSource(pathToCsvFile)

        bufferedSource match {
          case Some(source) =>

            val updateRecords: mutable.Builder[String, Seq[String]] = Seq.newBuilder[String]
            var counter = 0

            val dataLines: Iterator[String] = source.getLines.drop(1)

            for (line <- dataLines) {

              val cols: Array[String] = line.split(",").map(_.trim)
              val since: Int = cols(4).toInt

              val newRecord: String = if(since >= -1) line.replaceAll("\"", "") else null


              updateRecords += newRecord

              counter += 1
              if (counter % 100 == 0) {
                val updatedRecords = updateRecords.result()
                writeToFile("src/data/rptla.csv", true, headline, updatedRecords)
                updateRecords.clear()
              }

            }

            val updatedRecords = updateRecords.result()
            println(counter)

            writeToFile("src/data/rptla.csv", true, headline, updatedRecords)

            updateRecords.clear()

            source.close()


          case None =>
            println(s"File $pathToCsvFile does not exists.")

        }

      case "findMultipleRecords" =>

        val database: Seq[String] = Seq(
          databasePath + "Polska_youthPlayerDatabase1-4L.csv",
          databasePath + "Polska_youthPlayerDatabase_5L.csv",
          databasePath + "Polska_youthPlayerDatabase_6L_1_256.csv",
          databasePath + "Polska_youthPlayerDatabase_6L_257_512.csv",
          databasePath + "Polska_youthPlayerDatabase_6L_513_768.csv",
          databasePath + "Polska_youthPlayerDatabase_6L_769_1024.csv",
          databasePath + "Polska_youthPlayerDatabase_7L_1_256.csv",
          databasePath + "Polska_youthPlayerDatabase_7L_257_512.csv",
          databasePath + "Polska_youthPlayerDatabase_7L_513_768.csv",
          databasePath + "Polska_youthPlayerDatabase_7L_769_1024.csv"
        )

        val all: Seq[String] = database.flatMap(pathToCsvFile => {

          println(pathToCsvFile)

          val bufferedSource: Option[BufferedSource] = tryBufferedSource(pathToCsvFile)

          bufferedSource match {
            case Some(source) =>

              val updateRecords: mutable.Builder[String, Seq[String]] = Seq.newBuilder[String]

              val dataLines: Iterator[String] = source.getLines.drop(1)

              for (line <- dataLines) {

                val newRecord: String = line.replaceAll("\"", "")

                updateRecords += newRecord

              }

              val updatedRecords: Seq[String] = updateRecords.result()

              source.close()

              updatedRecords

            case None =>
              println(s"File $pathToCsvFile does not exists.")
              null

          }

        })

        val ids: Seq[String] = getYouthPlayerIDsFromPlayerRecordsString(all)

        val uniqueID: Seq[String] =
          ids.groupBy(identity).collect { case (x, occurence) if occurence.size == 1 => x }.toSeq

        val repeatedID: Map[String, Int] =
          ids.groupBy(identity).collect { case (x, occurence) if occurence.size > 1 => (x, occurence.length) }
        //ids.groupBy(identity).collect{case (x,occurence) if occurence.size > 1 => x}.toSeq

        uniqueID.foreach(println(_))

        val updateRecords: mutable.Builder[String, Seq[String]] = Seq.newBuilder[String]
        var counter = 1
        uniqueID.foreach(f = id => {

          breakable(all.foreach(f = line => {

            if (line.contains(id)) {
              println(s"$counter -> $line")
              counter += 1
              updateRecords += line
              break()
            }
          }))

        })

        val updatedRecords: Seq[String] = updateRecords.result()
        updateRecords.clear()

        writeToFile("src/data/allData.csv", true, headline, updatedRecords)


        repeatedID.foreach(println(_))
        repeatedID.map(x => {

          val updateRecords: mutable.Builder[String, Seq[String]] = Seq.newBuilder[String]

          val id: String = x._1
          val occurence: Int = x._2

          var counter = 0

          breakable {
            all.foreach(line => {

              if (line.contains(id)) {
                counter += 1
                updateRecords += line
              }

              if (counter == occurence) {
                val updatedRecords: Seq[String] = updateRecords.result()

                //id,l_gk,l_cd,l_wb,l_im,l_wg,l_fn
                val playerData
                : Seq[(Double, Double, Double, Double, Double, Double, Double)] =

                  updatedRecords.map(line => {
                    val cols: Array[String] = line.split(",").map(_.trim)

                    (cols(2).toDouble,
                      cols(17).toDouble, cols(18).toDouble, cols(19).toDouble,
                      cols(20).toDouble, cols(21).toDouble, cols(22).toDouble)

                  })

                val age: Seq[Double] = playerData.map(_._1)
                val primeIndex = age.indexOf(age.max)

                val l_gk = playerData.map(_._2).max.toString
                val l_cd = playerData.map(_._3).max.toString
                val l_wb = playerData.map(_._4).max.toString
                val l_im = playerData.map(_._5).max.toString
                val l_wg = playerData.map(_._6).max.toString
                val l_fn = playerData.map(_._7).max.toString

                val reducedLine: String = s"${
                  updatedRecords(primeIndex).split(",").map(_.trim).take(17).mkString(",")
                    ++ "," ++ Seq(l_gk, l_cd, l_wb, l_im, l_wg, l_fn).mkString(",")
                    ++ "," ++ updatedRecords(primeIndex).split(",").map(_.trim).drop(23).mkString(",")
                }"

                println(reducedLine)

                updateRecords.clear()
                writeToFile("src/data/allData.csv", true, headline, Seq(reducedLine))
                break()
              }

            }
            )


          }


        })


      case "databaseMinusTttestRecords" =>

        def removePlayerFromDatabase(dbLines: Iterator[String], idsToBeRemoved: Seq[String]): Unit = {

          println(s"${idsToBeRemoved.length}")

          for(line <- dbLines){

            println(line)

            val cols: Array[String] = line.split(",").map(_.trim)
            val id = cols(1)

            if(idsToBeRemoved.contains(id))
              {
                //val idToBeRemoved = idsToBeRemoved.indexOf(id)
                removePlayerFromDatabase(dbLines, idsToBeRemoved.filterNot(p => p == id))
              }
            else
              {
                writeToFile("src/data/new_db.csv", true, headline, Seq(line))
                removePlayerFromDatabase(dbLines, idsToBeRemoved)
              }

          }

        }


        val bufferedSource0: Option[BufferedSource] = tryBufferedSource("src/data/tttest.csv")
        val idsToBeRemoved: Seq[String] = bufferedSource0 match {

          case Some(source) =>

            val sourceGetLines = source.getLines

            //println(sourceGetLines.length) do not invoke this as this reveals source to the end and source is seen as empty

            val readRecords: mutable.Builder[String, Seq[String]] = Seq.newBuilder[String]

            if (sourceGetLines.nonEmpty) {
              //println("sourceGetLines.nonEmpty")
              for (line <- sourceGetLines) readRecords += line

              source.close()

              val readedRecord: Seq[String] = readRecords.result()

              getYouthPlayerIDsFromPlayerRecordsString(readedRecord)

            }
            else {
              //println("sourceGetLines.Empty")
              source.close()
              Seq.empty[String]
            }


          case None =>
            println(s"File src/data/tttest.csv does not exists.")
            Seq.empty[String]

        }

        println(idsToBeRemoved)
        println(idsToBeRemoved.isEmpty)


        val bufferedSource: Option[BufferedSource] = tryBufferedSource(pathToCsvFile)
        bufferedSource match {

          case Some(source) =>

            //val readRecords: mutable.Builder[String, Seq[String]] = Seq.newBuilder[String]

            val dbLines: Iterator[String] = source.getLines()

            removePlayerFromDatabase(dbLines.drop(1), idsToBeRemoved)

              source.close()





          case None =>
            println(s"File $pathToCsvFile does not exists.")
            Seq.empty[String]

        }


      case "removeDuplicateRecords" =>

        println(pathToCsvFile)

        val bufferedSource: Option[BufferedSource] = tryBufferedSource(pathToCsvFile)

        val lines: Seq[String] = bufferedSource match {
          case Some(source) =>

            val updateRecords: mutable.Builder[String, Seq[String]] = Seq.newBuilder[String]

            val dataLines: Iterator[String] = source.getLines.drop(1)

            for (line <- dataLines) {

              val newRecord: String = line.replaceAll("\"", "")

              updateRecords += newRecord

            }

            val updatedRecords: Seq[String] = updateRecords.result()

            source.close()

            updatedRecords

          case None =>
            println(s"File $pathToCsvFile does not exists.")
            null

        }

        println(lines.length)
        println(lines.distinct.length)

        writeToFile("src/data/allData.csv", true, headline, lines.distinct)




      case _ => None

    }

  }

  def gentlenessFromCharacter(characterLine: String): String = {

    val split: Array[String] = characterLine.split("-")
    val eyes = split(2).last
    val mouth = split(3).last

    println(s"$eyes, $mouth")
    println(s"${eyes.equals('b')}, ${mouth.equals('b')}")


    val gentleness = if(eyes.equals('a') && mouth.equals('a'))"zlosliwy"
    else if(eyes.equals('b') && mouth.equals('b'))"kontrowersyjny"
    else if(eyes.equals('a') && mouth.equals('c'))"kontrowersyjny"
    else if(eyes.equals('c') && mouth.equals('c'))"przyjemny+"
    else if(!eyes.equals(mouth) )"unknown"
    else "------"

    gentleness

  }

  //
  def cleanDatabaseByAge(pathToCsvFile: String, maxAge: Double): Unit = {

    val bufferedSource: Option[BufferedSource] = tryBufferedSource(pathToCsvFile)

    bufferedSource match {
      case Some(source) =>

        writeToFile("src/data/tttest.csv", false, headline, Seq.empty[String])

        val updateRecords: mutable.Builder[String, Seq[String]] = Seq.newBuilder[String]
        var counter = 0

        val dataLines: Iterator[String] = source.getLines.drop(1)

        for (line <- dataLines) {

          val cols: Array[String] = line.split(",").map(_.trim)
          val age: Double = cols(2).toDouble
          val since = cols(4).toInt
          val gentleness = if(cols.length <= 26) "------" else gentlenessFromCharacter(cols(26))
          println(s"$line")
          println(s"$age")
          println(s"${age <= maxAge }")

          val newRecord: String = if (age <= maxAge && since >= -1) cols.dropRight(4).mkString(",") ++ "," ++ gentleness ++ "," ++ cols.takeRight(3).mkString(",") replaceAll("\"", "") else null
          updateRecords += newRecord

          counter += 1
          if (counter % 100 == 0) {
            val updatedRecords = updateRecords.result()
            writeToFile("src/data/tttest.csv", true, headline, updatedRecords)
            updateRecords.clear()
          }

        }

        val updatedRecords = updateRecords.result()
        println(counter)

        writeToFile("src/data/tttest.csv", true, headline, updatedRecords)

        updateRecords.clear()

        source.close()


      case None =>
        println(s"File $pathToCsvFile does not exists.")

    }

  }

  //ok
  def getDatabasePathByDatabaseKey(key: String): String = DatabasePath(key)

  //ok
  def getDatabaseKeyByDatabasePath(path: String): String = DatabasePath.map(_.swap)(path)

  //ok
  def updateSeniorPlayer(id: String, line: String): String = {

    val sp = new Senior(Array(seniorPlayerPath,id))

    if (sp.exists)
      Senior.UpdateExistingPlayer(sp, line)
    else
      //Senior.UpdateNonExistingPlayer(line)
      null

  }

  //ok
  def updateYouthPlayer(id: String, b5p: Seq[String], l5p: Seq[Double], line: String): String = {

    val yp = new Youth(Array(youthPlayerPath,id))

    //println(yp.exists) to test
    
    if(yp.exists)

      if (line.isEmpty)
      {
        val nationality: String = yp.nationality.getOrElse("unknown")
        val scoutingHistory: Seq[String] = yp.scoutingHistory.getOrElse((Seq("------,------,0"),"------"))._1

        scoutingHistory.foreach(sh => if(!sh.contains("-")) writeToFile(databasePath + "scouting_history.csv", true, Seq.empty[String], Seq(s"$nationality,$sh")) )

        Youth.UpdateExistingPlayer(yp, b5p, l5p)
      }
      else
        Youth.UpdateExistingPlayer1(yp, b5p, l5p, line.replaceAll("\"", ""))
    else
      //Youth.UpdateNonExistingPlayer(line)
      null

    }

  //ok
  def createDatabase(pathToCsvFile: String, ids: Seq[String]): Unit = {

    val createRecords: mutable.Builder[String, Seq[String]] = Seq.newBuilder[String]
    for(id <- ids) createRecords += updateYouthPlayer(id,Seq.fill(6)("-1.0"),Seq.fill(6)(-1.0),/*Seq.fill(6)("").mkString(",")*/"")
    val createdRecords = createRecords.result()

    writeToFile(pathToCsvFile, false, headline, createdRecords)

  }

  /*def createDatabase(playerLine: Seq[String], pathToCsvFile: String, appendFlag: Boolean ): Unit = {

    writeToFile(pathToCsvFile, appendFlag, headline, playerLine)

  }*/

  //ok
  def updateDatabase(pathToCsvFile: String): Unit = {

    val bufferedSource0: Option[BufferedSource] = tryBufferedSource("src/data/tttest.csv")

    val lastID_int: Int = bufferedSource0 match {

      case Some(source) =>

        val sourceGetLines = source.getLines.drop(1)

        //println(sourceGetLines.length) do not invoke this as this reveals source to the end and source is seen as empty

        val updateRecords: mutable.Builder[String, Seq[String]] = Seq.newBuilder[String]

        if(sourceGetLines.nonEmpty) {
          //println("sourceGetLines.nonEmpty")
          for(line <- sourceGetLines) updateRecords += line

          source.close()

          val updatedRecord: Seq[String] = updateRecords.result()

          val lastPlayerRecord: String = updatedRecord.last
          getYouthPlayerIDFromPlayerRecordString(lastPlayerRecord).toInt

        }
        else {
          //println("sourceGetLines.Empty")
          source.close()
          0
        }


      case None =>
        println(s"File src/data/tttest.csv does not exists.")
        0

    }

    val lastID_String = lastID_int.toString

    println(lastID_String)

    /*val ids: Seq[String] = bufferedSource0 match {

      case Some(source) =>

        val sourceGetLines = source.getLines

        //println(sourceGetLines.length) do not invoke this as this reveals source to the end and source is seen as empty

        val readRecords: mutable.Builder[String, Seq[String]] = Seq.newBuilder[String]

        if (sourceGetLines.nonEmpty) {
          //println("sourceGetLines.nonEmpty")
          for (line <- sourceGetLines) readRecords += line

          source.close()

          val readedRecord: Seq[String] = readRecords.result()

          getYouthPlayerIDsFromPlayerRecordsString(readedRecord)

        }
        else {
          //println("sourceGetLines.Empty")
          source.close()
          Seq.empty[String]
        }


      case None =>
        println(s"File src/data/tttest.csv does not exists.")
        Seq.empty[String]

    }

    println(ids)
    println(ids.isEmpty)*/

    val bufferedSource1: Option[BufferedSource] = tryBufferedSource(pathToCsvFile)
    val db_ids: Seq[String] = bufferedSource1 match {

          case Some(source) =>

            val sourceGetLines = source.getLines

            //println(sourceGetLines.length) do not invoke this as this reveals source to the end and source is seen as empty

            val readRecords: mutable.Builder[String, Seq[String]] = Seq.newBuilder[String]

            if (sourceGetLines.nonEmpty) {
              //println("sourceGetLines.nonEmpty")
              for (line <- sourceGetLines) readRecords += line

              source.close()

              val readedRecord: Seq[String] = readRecords.result()

              getYouthPlayerIDsFromPlayerRecordsString(readedRecord)

            }
            else {
              //println("sourceGetLines.Empty")
              source.close()
              Seq.empty[String]
            }


          case None =>
            println(s"File src/data/tttest.csv does not exists.")
            Seq.empty[String]

        }

    //println(db_ids)
    println(db_ids.isEmpty)


    val ref_ID_index = if(!lastID_String.equals("0")) db_ids.indexOf(lastID_String) else 0

    println(ref_ID_index)







      val bufferedSource: Option[BufferedSource] = tryBufferedSource(pathToCsvFile)

      bufferedSource match {
        case Some(source) =>

          val updateRecords: mutable.Builder[String, Seq[String]] = Seq.newBuilder[String]
          var counter = 0

          val dataLines: Iterator[String] = source.getLines.drop(1+ref_ID_index)

          for (line <- dataLines) {

            val cols: Array[String] = line.split(",").map(_.trim)
            val id: String = cols(1)

            //println(s"Line to test: $line") to test


            //val id_index: Int = db_ids.indexOf(id)

            //if (ids.isEmpty || (ids.nonEmpty && !ids.contains(id))) {  //very slow!!
            //if (id_index > ref_ID_index) {
              val l5p: Seq[Double] = cols.slice(17, 23).toSeq.map(_.toDouble)
              val b5p: Seq[String] = cols.slice(11, 17).toSeq

              val since = cols(4).toInt

              val newRecord: String =
                if (since >= 0) updateYouthPlayer(cols(1), b5p, l5p, line)
                else if(since == -1) updateSeniorPlayer(cols(1), line)
                else null
              updateRecords += newRecord

              counter += 1
              if (counter % 100 == 0) {
                val updatedRecords = updateRecords.result()
                writeToFile("src/data/tttest.csv", true, headline, updatedRecords)
                updateRecords.clear()
              }

            //}
          }

          val updatedRecords = updateRecords.result()

          //val dataLinesLength = dataLines.length
          println(counter)

          if(counter < 70)
            writeToFile(pathToCsvFile, false, headline, updatedRecords)
          else
            writeToFile("src/data/tttest.csv", true, headline, updatedRecords)

          updateRecords.clear()

          source.close()
          /*println(s"File $pathToCsvFile exists.")

          println("--------------------------")
          println(updatedRecords)
          updatedRecords.foreach(println(_))
          println("--------------------------")*/



          /*val file = new File(pathToCsvFile)
          val writer = CSVWriter.open(file, append = false)
          writer.writeRow(headline)
          updatedRecords.foreach(x => writer.writeRow(Seq(x)))
          writer.close()*/

        case None =>
          println(s"File $pathToCsvFile does not exists.")
          println(s"Create file $pathToCsvFile.")

        val databaseKey: String = getDatabaseKeyByDatabasePath(pathToCsvFile)

        if (databaseKey.matches("\\d+"))
        {

          val youthClubID: Int = databaseKey.toInt

          println(s"Creating database for Youth Club ID = $youthClubID")

          val playerIDs: Array[String] =
            YouthDatabase.getYouthPlayerIDsFromYouthClubID(youthClubID)

          YouthDatabase.createDatabase(pathToCsvFile, playerIDs)

        }
        else {
          println(s"Cannot update $databaseKey. File does not exist.")

        }

      }

  }

  //ok
  def getYouthPlayerIDsFromYouthClubID(youthAcademyId: Int): Array[String] = {

    val url = youthTeamPath + s"$youthAcademyId"

    val document = JsoupConnection(url)

    val YourNYouthPlayers: String = document.select("head[id]").text()

    val pattern = "\\d+".r
    val nPlayers: Int = pattern.findAllIn(YourNYouthPlayers).map(_.toInt).toArray.head

    val playersID: Array[String] = document.select("td.hidden:not(.left)").text().split(" ")

    if(nPlayers > 0) playersID else Seq("0").toArray

  }

  def getYouthPlayerIDFromPlayerRecordString(line: String): String = {

    val cols: Array[String] = line.split(",").map(_.trim)
    val youthPlayerId: String = cols(1)

    youthPlayerId

  }

  def getYouthPlayerIDsFromPlayerRecordsString(playerRecords: Seq[String]): Seq[String] = {

    val IDsbuffer: mutable.Builder[String, Seq[String]] = Seq.newBuilder[String]

    for(line <- playerRecords){

      val youthPlayerId: String = getYouthPlayerIDFromPlayerRecordString(line)
      IDsbuffer += youthPlayerId

    }

    val youthPlayerIDs: Seq[String] = IDsbuffer.result()
    youthPlayerIDs

  }

  //ok
  def getYouthPlayerIDsFromYouthDatabase(pathToCsvFile: String): Seq[String] = {

    val bufferedSource: Option[BufferedSource] = tryBufferedSource(pathToCsvFile)

    bufferedSource match {
      case Some(source) =>
        val IDsbuffer: mutable.Builder[String, Seq[String]] = Seq.newBuilder[String]
        for (line <- source.getLines) {
          val youthPlayerId: String = getYouthPlayerIDFromPlayerRecordString(line)
          IDsbuffer += youthPlayerId
        }
        source.close
        val youthPlayerIDs: Seq[String] = IDsbuffer.result()
        youthPlayerIDs
      case None =>
        Seq.empty[String]
    }
  }

  //ok
  def addPlayerToDatabase(
                           pathToCsvFile: String,
                           currentPlayerIDs: Seq[String],
                           storedPlayerIDs: Seq[String]
                         ): Unit = {

    val newPlayers: Seq[String] = currentPlayerIDs.filterNot(p => p.equals("0")).diff(storedPlayerIDs)
    //val newPlayers: Seq[String] = currentPlayerIDs.filterNot(p => p.equals("0")).map(x => if(!storedPlayerIDs.contains(x)) x else null).filterNot(p => p == null)

    println(s"New players: $newPlayers")

    //////-----------------
    val createRecords: mutable.Builder[String, Seq[String]] = Seq.newBuilder[String]

    //val test = "new_text"

    for (id <- newPlayers) createRecords += updateYouthPlayer(id, Seq.fill(6)("-1.0"), Seq.fill(6)(-1.0), /*Seq.fill(6)("").mkString(",")*/"") //++ test

    val createdRecords = createRecords.result()

    writeToFile(pathToCsvFile, true, headline, createdRecords)
    writeToFile("src/data/new.csv", true, headline, createdRecords)

    /*val file = new File(pathToCsvFile)
    val writer = CSVWriter.open(file, append = true)
    createdRecords.foreach(x => writer.writeRow(Seq(x)))
    writer.close()*/

  }

}


/*object createCustomYouthClubDatabase extends App{

  //val youthAcademyId = 678445
  val youthAcademyId = 2955119

  val pathToDatabase: String = YouthDatabase.PlayerDatabasePathByYouthTeamID(youthAcademyId)

  val playerIDs: Array[String] = YouthDatabase.getYouthPlayerIDsFromYouthClubID(youthAcademyId)

  YouthDatabase.createDatabase(pathToDatabase, playerIDs)

}*/


class YouthAnalysis {

  def this(youthAcademyId: Seq[Int]) = {

    this()


  }


  def this(youthAcademyId: Int) = {

    this()

    val youthAcademyIdAsString = youthAcademyId.toString

    val pathToDatabase: String = YouthDatabase.getDatabasePathByDatabaseKey(youthAcademyIdAsString)

    val currentPlayerIDs: Array[String] = YouthDatabase.getYouthPlayerIDsFromYouthClubID(youthAcademyId)

    YouthDatabase.updateDatabase(pathToDatabase)

    val storedPlayerIDs: Seq[String] = YouthDatabase.getYouthPlayerIDsFromYouthDatabase(pathToDatabase)
    YouthDatabase.addPlayerToDatabase(pathToDatabase, currentPlayerIDs, storedPlayerIDs)

  }

  //ok
  def this(databaseName: String) = {

    this()

    if(databaseName eq "test"){



    }
    else {
      val pathToDatabase: String = YouthDatabase.getDatabasePathByDatabaseKey(databaseName)
      YouthDatabase.updateDatabase(pathToDatabase)
    }



  }

  def this(maxAgeLimit: Double, databaseName: String)= {

    this()

    val pathToDatabase: String = YouthDatabase.getDatabasePathByDatabaseKey(databaseName)
    YouthDatabase.cleanDatabaseByAge(pathToDatabase,maxAgeLimit)

  }

  def this(label: String, databaseName: String) = {

    this()

    val pathToDatabase: String = YouthDatabase.getDatabasePathByDatabaseKey(databaseName)
    YouthDatabase.doSthWithDatabase(pathToDatabase, label)

  }




}

object run extends App{

  //new YouthAnalysis("test-TL'a")
  //new YouthAnalysis(678445)
  //new YouthAnalysis(2955119)
  new YouthAnalysis("Polska")
  //new YouthAnalysis("Kenia")
  //new YouthAnalysis("Ligi_1-4")
  //new YouthAnalysis("5 Liga")
  //new YouthAnalysis("6 Liga 1-256")
  //new YouthAnalysis("6 Liga 257-512")            //tu aktualizuj
  //new YouthAnalysis("6 Liga 513-768")
  //new YouthAnalysis("6 Liga 769-1024")
  //new YouthAnalysis("7 Liga 1-256")
  //new YouthAnalysis("7 Liga 257-512")
  //new YouthAnalysis("7 Liga 513-768")
  //new YouthAnalysis("7 Liga 769-1024")
  //new YouthAnalysis("World")

}

//ok
def teamsIDFromLeagueID(leagueID: Int): Seq[String] = {

  val url = leaguePath + s"$leagueID"

  val document = JsoupConnection(url)

  val teamsID: Seq[String] = document.select("td.series-table-team").select("a").attr("href").split("&")(1).split("=")(1).split(",").toSeq

  println(s"$teamsID")

  teamsID

}

//ok
def youthClubIDFromTeamID(teamID: Int): Int = {

  val url = teamPath + s"$teamID"
  val document = JsoupConnection(url)

  val youthClubID: Int = {
    val youthClubLine: Elements = document.select("div.boxBody").select("a:contains(Overview)")
    if(!youthClubLine.isEmpty)
      youthClubLine.first.attr("href").split("=").last.toInt
    else
      0

  }

  youthClubID

}

object newTest extends App{

  val leagueIDsL1L4: Seq[Int] = Range(3620, 3704, 1)
  //val leagueIDsL5

  //val youthClubIds: Seq[Int] = teamsIDFromLeagueID(3672).map(x => youthClubIDFromTeamID(x.toInt)).filter(p => !p.equals(0))

  val youthClubIds: Seq[Int] = leagueIDsL1L4.flatMap(y => teamsIDFromLeagueID(y).map(x => youthClubIDFromTeamID(x.toInt)).filter(p => !p.equals(0)))

    println(youthClubIds)
  
  youthClubIds.foreach(x => {
    val currentPlayerIDs: Array[String] = YouthDatabase.getYouthPlayerIDsFromYouthClubID(x)
    val storedPlayerIDs: Seq[String] = YouthDatabase.getYouthPlayerIDsFromYouthDatabase(databasePath)
    YouthDatabase.addPlayerToDatabase(databasePath + "Polska_youthPlayerDatabase.csv", currentPlayerIDs, storedPlayerIDs)

  })

}

//ok
class leagueIDs_DatabasePath {

  def L1: (List[Int], String) = ((3620 to 3620).toList, databasePath + "Polska_youthPlayerDatabase_1L.csv")
  def L2: (List[Int], String) = ((3621 to 3624).toList, databasePath + "Polska_youthPlayerDatabase_2L.csv")
  def L3: (List[Int], String) = ((3641 to 3704).toList, databasePath + "Polska_youthPlayerDatabase_3L.csv")
  def L4: (List[Int], String) = ((3641 to 3704).toList, databasePath + "Polska_youthPlayerDatabase_4L.csv")

  def L1_4: (List[Int], String) = ((3620 to 3704).toList, databasePath + "Polska_youthPlayerDatabase1-4L.csv")

  def L5: (List[Int], String) = ((9383 to 9638).toList, databasePath + "Polska_youthPlayerDatabase_5L.csv")

  def L6_1_256: (List[Int], String) = ((32114 to 32369).toList, databasePath + "Polska_youthPlayerDatabase_6L_1_256.csv")
  def L6_257_512: (List[Int], String) = ((32370 to 32625).toList, databasePath + "Polska_youthPlayerDatabase_6L_257_512.csv")
  def L6_513_768: (List[Int], String) = ((32626 to 32881).toList, databasePath + "Polska_youthPlayerDatabase_6L_513_768.csv")
  def L6_769_1024: (List[Int], String) = ((32882 to 33137).toList, databasePath + "Polska_youthPlayerDatabase_6L_769_1024.csv")

  def L7_1_256: (List[Int], String) = ((58605 to 58860).toList, databasePath + "Polska_youthPlayerDatabase_7L_1_256.csv")
  def L7_257_512: (List[Int], String) = ((58861 to 59116).toList, databasePath + "Polska_youthPlayerDatabase_7L_257_512.csv")
  def L7_513_768: (List[Int], String) = ((59117 to 59372).toList, databasePath + "Polska_youthPlayerDatabase_7L_513_768.csv")
  def L7_769_1024: (List[Int], String) = ((59373 to 59628).toList, databasePath + "Polska_youthPlayerDatabase_7L_769_1024.csv")

}

class other_leagueIDs_DatabasePath {

  def Kenia_L1_4: (List[Int], String) = (
    List(
      //Range.inclusive(60149,60149),
      //Seq(60149),
      List(60149),                      //L1
      Range.inclusive(60164,60167),     //L2
      Range.inclusive(60208,60223),
      Range.inclusive(249625,249688)
    ).flatten,
    databasePath + "Kenia_youthPlayerDatabase1-4L.csv")

  def Polska_L1_7: (List[Int], String) = (
    List(
      //Range.inclusive(3620,3704),    //L1-L4
      //Range.inclusive(9383,9638),    //L5
      //Range.inclusive(32114,33137),  //L6
      Range.inclusive(32759,33137),  //L6
      Range.inclusive(58605,59628)   //L7
    ).flatten,
    databasePath + "Polska_youthPlayerDatabase.csv")


}

object test extends App{

  val t = new other_leagueIDs_DatabasePath().Kenia_L1_4
  t._1.foreach(println(_))
  println(t._2)

}

object addNewPlayersToDatabase extends App{

  //csv files have to have header, unless empty line is detected and no read is applied

  //val leagueIDs_Path: (List[Int], String) = new leagueIDs_DatabasePath().L1_4

  //val leagueIDs_Path: (List[Int], String) = new other_leagueIDs_DatabasePath().Kenia_L1_4
  val leagueIDs_Path: (List[Int], String) = new other_leagueIDs_DatabasePath().Polska_L1_7

  val leagueIDs: Seq[Int] = leagueIDs_Path._1
  val pathToCsvFile: String = leagueIDs_Path._2

  leagueIDs.foreach(l_id => {
    teamsIDFromLeagueID(l_id).map(x => youthClubIDFromTeamID(x.toInt)).filter(p => !p.equals(0)).foreach(yc => {

      val currentPlayerIDs: Array[String] = YouthDatabase.getYouthPlayerIDsFromYouthClubID(yc)
      val storedPlayerIDs: Seq[String] = YouthDatabase.getYouthPlayerIDsFromYouthDatabase(pathToCsvFile)
      YouthDatabase.addPlayerToDatabase(pathToCsvFile, currentPlayerIDs, storedPlayerIDs)

    })

    writeToFile(databasePath + "config1_db.dat", false, Seq.empty[String], Seq(l_id.toString))

    /*val file = new File(databasePath + "config1_db.dat")
    val writer = CSVWriter.open(file, append = false)
    writer.writeRow(Seq(l_id.toString))
    writer.close()*/

  })

}

object addNewPlayersToDatabase_withFutures extends App{

  def doF(leagueIDs_Path: (List[Int], String), fileName: String) = {

    val leagueIDs: Seq[Int] = leagueIDs_Path._1
    val pathToCsvFile: String = leagueIDs_Path._2

    leagueIDs.foreach(l_id => {
      teamsIDFromLeagueID(l_id).map(x => youthClubIDFromTeamID(x.toInt)).filter(p => !p.equals(0)).foreach(yc => {

        val currentPlayerIDs: Array[String] = YouthDatabase.getYouthPlayerIDsFromYouthClubID(yc)
        val storedPlayerIDs: Seq[String] = YouthDatabase.getYouthPlayerIDsFromYouthDatabase(pathToCsvFile)
        YouthDatabase.addPlayerToDatabase(pathToCsvFile, currentPlayerIDs, storedPlayerIDs)

      })

      writeToFile(databasePath + fileName, false, Seq.empty[String], Seq(l_id.toString))

    })

  }

  //csv files have to have header, unless empty line is detected and no read is applied

  //val f1 = Future { doF((Range.inclusive(32964,33137).toList,databasePath + "Polska_youthPlayerDatabase.csv"),"config2_db.dat") }
  val f1 = Future { doF((Range.inclusive(59361,59628).toList,databasePath + "Polska_youthPlayerDatabase.csv"),"config2_db.dat") }
  val f2 = Future { doF((Range.inclusive(59102,59360).toList,databasePath + "Polska_youthPlayerDatabase.csv"),"config3_db.dat") }

  //Future {for(i <- 1 to 100) {print("A");Thread.sleep(10)}}
  //Future {for(i <- 1 to 100) {print("B");Thread.sleep(10)}}

  Await.result(Future.sequence(Seq(f1, f2)), 1.day)


}

object prepareDatabaseForScouts extends App{

  val maxAgeLimit_Poland = 17.030
  val maxAgeLimit_Kenia = 21.000


  //new YouthAnalysis(maxAgeLimit,"Ligi_1-4")
  //new YouthAnalysis(maxAgeLimit,"5 Liga")
  //new YouthAnalysis(maxAgeLimit,"6 Liga 1-256")
  //new YouthAnalysis(maxAgeLimit,"6 Liga 257-512")
  //new YouthAnalysis(maxAgeLimit,"6 Liga 513-768")
  //new YouthAnalysis(maxAgeLimit,"6 Liga 769-1024")
  //new YouthAnalysis(maxAgeLimit,"7 Liga 1-256")
  //new YouthAnalysis(maxAgeLimit,"7 Liga 257-512")
  //new YouthAnalysis(maxAgeLimit,"7 Liga 513-768")
  //new YouthAnalysis(maxAgeLimit,"7 Liga 769-1024")
  new YouthAnalysis(maxAgeLimit_Poland,"Polska")
  //new YouthAnalysis(maxAgeLimit_Kenia,"Kenia")


}

object doSthWithDatabases extends App{

  //val label: String = "clearWrongSpecialityStatus"
  //val label: String = "removePlayersThatLeftAcademy"
  //val label: String = "findMultipleRecords"
  //val label: String = "databaseMinusTttestRecords"
  val label: String = "removeDuplicateRecords"


  new YouthAnalysis(label,"Polska")
  //new YouthAnalysis(label,"Ligi_1-4")
  //new YouthAnalysis(label,"5 Liga")
  //new YouthAnalysis(label,"6 Liga 1-256")
  //new YouthAnalysis(label,"6 Liga 257-512")
  //new YouthAnalysis(label,"6 Liga 513-768")
  //new YouthAnalysis(label,"6 Liga 769-1024")
  //new YouthAnalysis(label,"7 Liga 1-256")
  //new YouthAnalysis(label,"7 Liga 257-512")
  //new YouthAnalysis(label,"7 Liga 513-768")
  //new YouthAnalysis(label,"7 Liga 769-1024")


}

object readtttest extends App{

  def getYouthPlayerIDFromPlayerRecordString(line: String): String = {

    val cols = line.split(",").map(_.trim)
    val youthPlayerId: String = cols(1)

    youthPlayerId

  }


  val bufferedSource0: Option[BufferedSource] = tryBufferedSource("src/data/tttest.csv")

  bufferedSource0 match {
    case Some(source) =>

      val updateRecords: mutable.Builder[String, Seq[String]] = Seq.newBuilder[String]

      //println(source.getLines.length)

      for(line <- source.getLines) {
        updateRecords += line
      }

      val lastPlayerRecord: String = updateRecords.result().last
      val lastID: Any = getYouthPlayerIDFromPlayerRecordString(lastPlayerRecord)
      println(lastID)

      source.close()

    case None =>
      println(s"File src/data/tttest.csv does not exists.")

  }

}


object outlookCollector extends App{

  //val path = "https://www.hattrick.org/pl/Club/Players/YouthPlayer.aspx?YouthPlayerID="
  val path = "https://www.hattrick.org/pl/Club/Players/Player.aspx?PlayerID="
  //val id = "332699838"
  //val id = "332435756"
  //val id = "333148660"
  val id = "477758191"

  val url: String = path + id
  val connection: Connection = Jsoup.connect(url)
  val document: Document = connection.get()

  //val text: String = document.select("img[src]").attr("src")
  //val text: Seq[Element] = document.select("img[src^=/Img/Avatar/bodies/]").asScala.toSeq
  val bodies = document.select("img[src^=/Img/Avatar/bodies/]").attr("src").split("/")(4).split("\\.").head
  val faces = document.select("img[src^=/Img/Avatar/faces/]").attr("src").split("/")(4).split("\\.").head
  val eyes = document.select("img[src^=/Img/Avatar/eyes/]").attr("src").split("/")(4).split("\\.").head
  val mouths = document.select("img[src^=/Img/Avatar/mouths/]").attr("src").split("/")(4).split("\\.").head
  val noses = document.select("img[src^=/Img/Avatar/noses/]").attr("src").split("/")(4).split("\\.").head
  val hair = document.select("img[src^=/Img/Avatar/hair/]").attr("src").split("/")(4).split("\\.").head

  val outlook = Seq(bodies,faces,eyes,mouths,noses,hair).mkString(",")

  println(bodies)
  println(faces)
  println(eyes)
  println(mouths)
  println(noses)
  println(hair)

  println(outlook)

}

object characterCollector extends App{

  val path = "https://www.hattrick.org/pl/Club/Players/Player.aspx?PlayerID="
  val id = "478180611"

  val url: String = path + id
  val connection: Connection = Jsoup.connect(url)
  val document: Document = connection.get()

  val gentleness = document.select("a[href^=/pl/Help/Rules/AppDenominations.aspx?lt=gentleness]").text()
  val aggressiveness = document.select("a[href^=/pl/Help/Rules/AppDenominations.aspx?lt=aggressiveness]").text()
  val honesty = document.select("a[href^=/pl/Help/Rules/AppDenominations.aspx?lt=honesty]").text()
  val leadership = document.select("a[href^=/pl/Help/Rules/AppDenominations.aspx?lt=leadership]").text()


  println(s"$gentleness $aggressiveness $honesty $leadership")

}

object addLeadershipIntoCharacterDb extends App{

  val bufferedSource0: Option[BufferedSource] = tryBufferedSource("src/data/characters.csv")

  bufferedSource0 match {
    case Some(source) =>

      val updateRecords: mutable.Builder[String, Seq[String]] = Seq.newBuilder[String]

      //println(source.getLines.length)

      for (line <- source.getLines.drop(1)) {
        val cols: Array[String] = line.split(",")
        if(cols.length == 16)
          updateRecords += cols.dropRight(4) ++ cols.takeRight(1) mkString "," replaceAll("\"","")
        else if(cols.length > 12)
          updateRecords += line.replaceAll("\"","")
        else
          val id = cols(2)

          val sp = new Senior(Array(seniorPlayerPath, id))

          if (sp.exists)
            val leadership: String = sp.character.get
            updateRecords += cols.dropRight(3) ++ "," ++ leadership mkString "," replaceAll("\"","")


      }

      source.close()

      val updatedRecords: Seq[String] = updateRecords.result()
      updatedRecords.foreach(println(_))

      writeToFile("src/data/CandL.csv", false, Seq("player,since,id,body,face,eyes,mouth,nose,hair,gentleness,aggressiveness,honesty,leadership"), updatedRecords)


    case None =>
      println(s"File src/data/characters.csv does not exists.")

  }


}


object scanYouthPlayerHistory extends App{

  def dateToDayOfTheWeek(data: String) = {

    val dateFormatter = DateTimeFormatter.ofPattern("dd.MM.yyyy")
    val date = LocalDate.parse(data, dateFormatter)
    val dayOfWeek = date.getDayOfWeek
    val dayOfWeekText = dayOfWeek.toString

    val engPolDayNames = Map(
      "MONDAY" -> "poniedzialek",
      "TUESDAY" -> "wtorek",
      "WEDNESDAY" -> "sroda",
      "THURSDAY" -> "czwartek",
      "FRIDAY" -> "piatek",
      "SATURDAY" -> "sobota",
      "SUNDAY" -> "niedziela")

    engPolDayNames(dayOfWeekText)

  }

  def playerScoutingHistory(id: String): Unit = {

    val path = "https://www.hattrick.org/pl/Club/Players/YouthPlayerHistory.aspx?YouthPlayerID="

    val url: String = path + id
    val connection: Connection = Jsoup.connect(url)
    val document: Document = connection.get()

    val memoryDates: String = document.select("h3.feed").text()
    val memoryHours: String = document.select("span.date").text()
    val memoryItems: String = document.select(".feedItem").text()

    println(memoryDates)
    println(memoryHours)
    println(memoryItems)

    val tableMemoryBool: Array[Boolean] = memoryItems.split("\\.").map(
      mi => List("przyprowadził", "Nie otrzymał").exists(text => mi.contains(text))
    )

    tableMemoryBool.foreach(println(_))

    val memoryModified: Array[String] = (memoryDates.split(" ").map(x => dateToDayOfTheWeek(x)) zip memoryHours.split(" ") /*zip memoryHours.zipWithIndex.map(x => tableMemoryBool.length - x._2)*/) /*zip tableMemoryBool*/ mkString "-" replaceAll("[()]", "") split "-" zip tableMemoryBool filter(p => p._2) map(_._1)

    //memoryModified.foreach(println(_))

    val scoutingHistory: Array[String] = memoryModified.zipWithIndex.map(x => x._1 + "," + (memoryModified.length - x._2).toString)

    scoutingHistory.foreach(println(_))
    println(scoutingHistory.head)

  }

  //val id = "335941970"
  val id = "332230129"
  //val id = "323286624"

  playerScoutingHistory(id)


}


object tak_lub_nie extends App{

  import scala.io.StdIn

  def Ben_mowi(wynik_rzutu: Int): String =
    {

      if(wynik_rzutu <= 3) "TAK"
      else if(wynik_rzutu >3 && wynik_rzutu <=6) "NIE"
      else "HA-HA-HA"

    }

  private val r: Random.type = scala.util.Random





  for(i <- 0 to 1000)
  {

    StdIn.readLine()
    val wynik_rzutu = r.nextInt(6)+1
    println(s"$i -> $wynik_rzutu" -> Ben_mowi(wynik_rzutu))
    //println(Ben_mówi(wynik_rzutu))

  }


}








