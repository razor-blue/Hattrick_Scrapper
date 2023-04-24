import com.github.tototoshi.csv.CSVWriter

import java.io.{File, FileNotFoundException}
import com.github.tototoshi.csv.*
import org.jsoup.nodes.Document
import org.jsoup.select.Elements
import org.jsoup.{Connection, Jsoup}

import java.text.SimpleDateFormat
import java.util.{Calendar, Date}
import scala.collection.mutable
import scala.io.BufferedSource

def JsoupConnection(url: String): Document = {
  val connection: Connection = Jsoup.connect(url)
  connection.get()
}

def tryBufferedSource(pathToFile: String): Option[BufferedSource] = {

  try {
    Some(io.Source.fromFile(pathToFile))
  } catch {
    case _: FileNotFoundException => None
  }

}

val projectFolder = "src/data/"
val youthTeamPath = "https://www.hattrick.org/Club/Players/YouthPlayers.aspx?YouthTeamID="
val leaguePath = "https://www.hattrick.org/World/Series/?LeagueLevelUnitID="
val seniorPlayerPath = "https://www.hattrick.org/pl/Club/Players/Player.aspx?PlayerID="
val youthPlayerPath = "https://www.hattrick.org/pl/Club/Players/YouthPlayer.aspx?YouthPlayerID="

def headline: Seq[String] = Seq("Player,Player ID,Age,Speciality,Days in Academy,WC X,Stage N,Description,Last Match Date,Season,Week,B_GK,B_CD,B_WB,B_IM,B_W,B_F,L_GK,L_CD,L_WB,L_IM,L_W,L_F,Last Match Details,Country,Last update")

def read_config_db: Int = {

  val config_db_path: String = projectFolder + "config_db.dat"

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

}


object YouthDatabase {

  val DatabasePath: Map[String, String] = Map(
    "678445" -> {projectFolder + "FCB_youthPlayerDatabase.csv"},
    "2955119" -> {projectFolder + "luka_w_systemie_youthPlayerDatabase.csv"},
    "Polska" -> {projectFolder + "Polska_youthPlayerDatabase.csv"},
    "World" -> {projectFolder + "World_youthPlayerDatabase.csv"}
  )

  def getDatabasePathByDatabaseKey(key: String): String = DatabasePath(key)

  def getDatabaseKeyByDatabasePath(path: String): String = DatabasePath.map(_.swap)(path)

  def updateSeniorPlayer(id: String, line: String): String = {

    val sp = new Senior(Array(seniorPlayerPath,id))

    if (sp.exists)
      Senior.UpdateExistingPlayer(sp, line)
    else
      Senior.UpdateNonExistingPlayer(line)

  }

  def addYouthPlayer(yp: Youth/*, b5p: Seq[String], l5p: Seq[Double], line: String*/): String = {
    //320761510
    if (yp.exists)
      //Youth.UpdateExistingPlayer(yp, Seq.empty[String], Seq.empty[Double])
      Youth.UpdateExistingPlayer(yp, Seq.fill(6)("-1.0"),Seq.fill(6)(-1.0))
    else
      Youth.UpdateNonExistingPlayer("")

  }


  def updateYouthPlayer(id: String, b5p: Seq[String], l5p: Seq[Double], line: String): String = {

    val yp = new Youth(Array(youthPlayerPath,id))

    if(yp.exists)
      Youth.UpdateExistingPlayer(yp, b5p, l5p)
    else
      Youth.UpdateNonExistingPlayer(line)

    }

  def createDatabase(pathToCsvFile: String, ids: Seq[String]): Unit = {

    val createRecords: mutable.Builder[String, Seq[String]] = Seq.newBuilder[String]
    for(id <- ids) createRecords += updateYouthPlayer(id,Seq.fill(6)("-1.0"),Seq.fill(6)(-1.0),Seq.fill(6)("").mkString(","))
    val createdRecords = createRecords.result()

    val file = new File(pathToCsvFile)
    val writer = CSVWriter.open(file, append = false)
    writer.writeRow(headline)
    createdRecords.foreach(x => writer.writeRow(Seq(x)))
    writer.close()

  }

  def createDatabase(playerLine: Seq[String], pathToCsvFile: String, appendFlag: Boolean ): Unit = {

    val file = new File(pathToCsvFile)
    val writer = CSVWriter.open(file, appendFlag)
    if(!appendFlag)writer.writeRow(headline)
    playerLine.foreach(x => if (x != null) writer.writeRow(Seq(x)))
    writer.close()

  }

  def updateDatabase(pathToCsvFile: String): Unit = {

      val bufferedSource: Option[BufferedSource] = tryBufferedSource(pathToCsvFile)

      bufferedSource match {
        case Some(source) =>

          val updateRecords: mutable.Builder[String, Seq[String]] = Seq.newBuilder[String]
          for (line <- source.getLines.drop(1)) {
            val cols: Array[String] = line.split(",").map(_.trim)
            val l5p: Seq[Double] = cols.slice(17, 23).toSeq.map(_.toDouble)
            val b5p: Seq[String] = cols.slice(11, 17).toSeq
            val newRecord: String = if (cols(4).toInt >= 0) updateYouthPlayer(cols(1), b5p, l5p, line) else updateSeniorPlayer(cols(1), line)
            updateRecords += newRecord
          }
          val updatedRecords = updateRecords.result()
          source.close()
          println(s"File $pathToCsvFile exists.")

          println("--------------------------")
          println(updatedRecords)
          updatedRecords.foreach(println(_))
          println("--------------------------")

          val file = new File(pathToCsvFile)
          val writer = CSVWriter.open(file, append = false)
          writer.writeRow(headline)
          updatedRecords.foreach(x => writer.writeRow(Seq(x)))
          writer.close()

        case None =>
          println(s"File $pathToCsvFile does not exists.")
          println(s"Create file $pathToCsvFile.")

        val databaseKey: String = getDatabaseKeyByDatabasePath(pathToCsvFile)

        if (databaseKey.matches("\\d+")) {

          val youthClubID: Int = databaseKey.toInt

          println(s"Creating database for Youth Club ID = $youthClubID")

          val playerIDs: Array[String] =
            YouthDatabase.getYouthPlayerIDsFromYouthClubID(youthClubID)

          YouthDatabase.createDatabase(pathToCsvFile, playerIDs)

        } else {
          println(s"Creating database for $databaseKey")

          val startID = read_config_db

          for(i <- Range(0,10000,1)) {


            val YouthPlayerBuffer: mutable.Builder[String, Seq[String]] = Seq.newBuilder[String]
            for (j <- Range(1, 100, 1)) {

              val id = startID + i*100 + j

              val idString = id.toString
              val yp_scan = new QuickScanYouthPlayer(Array(youthPlayerPath, idString))
              val singleYouthPlayerLine: Option[String] = if yp_scan.exists && yp_scan.nationality.getOrElse("").equals("Polska") then
                val yp = new Youth(Array(youthPlayerPath, idString))
                Some(addYouthPlayer(yp))
              else
                None

              YouthPlayerBuffer += singleYouthPlayerLine.orNull
            }

            //println(read_config_db+20000)

            val youthPlayerLines: Seq[String] = YouthPlayerBuffer.result()

            YouthDatabase.createDatabase(youthPlayerLines, pathToCsvFile, true)

            val file = new File(projectFolder + "config_db.dat")
            val writer = CSVWriter.open(file, append = false)
            val lastID = startID + i*100 + 100
            writer.writeRow(Seq(lastID.toString))
            writer.close()

          }

//320412639

        }




      }


  }

  def getYouthPlayerIDsFromYouthClubID(youthAcademyId: Int): Array[String] = {

    val url = youthTeamPath + s"$youthAcademyId"

    val document = JsoupConnection(url)

    val YourNYouthPlayers: String = document.select("head[id]").text()
    val pattern = "\\d+".r
    val nPlayers: Int = pattern.findAllIn(YourNYouthPlayers).map(_.toInt).toArray.head

    val playersID: Array[String] = document.select("td.hidden:not(.left)").text().split(" ")

    if(nPlayers > 0) playersID else Seq("0").toArray

  }

  def getYouthPlayerIDsFromYouthDatabase(pathToCsvFile: String): Seq[String] = {

    val bufferedSource = io.Source.fromFile(pathToCsvFile)
    val IDsbuffer: mutable.Builder[String, Seq[String]] = Seq.newBuilder[String]
    for (line <- bufferedSource.getLines) {
      val cols = line.split(",").map(_.trim)
      val youthPlayerId: String = cols(1)
      IDsbuffer += youthPlayerId
    }
    bufferedSource.close

    val youthPlayerIDs: Seq[String] = IDsbuffer.result()

    youthPlayerIDs

  }

  def addPlayerToDatabase(pathToCsvFile: String, currentPlayerIDs: Seq[String]): Unit = {

    val storedPlayerIDs: Seq[String] = getYouthPlayerIDsFromYouthDatabase(pathToCsvFile)

    val newPlayers: Seq[String] = currentPlayerIDs.filterNot(p => p.equals("0")).diff(storedPlayerIDs)

    println(s"New players: $newPlayers")

    //////-----------------
    val createRecords: mutable.Builder[String, Seq[String]] = Seq.newBuilder[String]

    for (id <- newPlayers) createRecords += updateYouthPlayer(id, Seq.fill(6)("-1.0"), Seq.fill(6)(-1.0), Seq.fill(6)("").mkString(","))

    val createdRecords = createRecords.result()

    val file = new File(pathToCsvFile)
    val writer = CSVWriter.open(file, append = true)
    createdRecords.foreach(x => writer.writeRow(Seq(x)))
    writer.close()

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
    YouthDatabase.addPlayerToDatabase(pathToDatabase, currentPlayerIDs)

  }

  def this(databaseName: String) = {

    this()

    if(databaseName eq "test"){



    }
    else {
      val pathToDatabase: String = YouthDatabase.getDatabasePathByDatabaseKey(databaseName)
      YouthDatabase.updateDatabase(pathToDatabase)
    }



  }




}

object run extends App{

  new YouthAnalysis(678445)
  new YouthAnalysis(2955119)
  //new YouthAnalysis("Polska")
  //new YouthAnalysis("World")


}


def teamsIDFromLeagueID(leagueID: Int): Seq[String] = {

  val url = leaguePath + s"$leagueID"

  val document = JsoupConnection(url)

  val teamsID: Seq[String] = document.select("td.series-table-team").select("a").attr("href").split("&")(1).split("=")(1).split(",").toSeq

  println(s"$teamsID")

  teamsID

}

def youthClubIDFromTeamID(teamID: Int): Int = {

  val url = s"https://www.hattrick.org/Club/?TeamID=$teamID"
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
    val currentPlayerIDs = YouthDatabase.getYouthPlayerIDsFromYouthClubID(x)
    YouthDatabase.addPlayerToDatabase("C:\\Users\\Lukasz\\IdeaProjects\\Scrapper\\src\\data\\Polska_youthPlayerDatabase.csv", currentPlayerIDs)

  })

}

object newTest2 extends App{

  //val leagueIDs: Seq[Int] = Seq(3620)      // 1 liga
  //val leagueIDs: Seq[Int] = 3641 to 3704   //Range(3704, 3704, 1) // 4 liga
  //val leagueIDs: Seq[Int] = 9520 to 9638   // 5 liga
  //val leagueIDs: Seq[Int] = 32114 to 32369 // 6 liga, 1-256
  //val leagueIDs: Seq[Int] = 3230 to 32625  // 6 liga, 257-512
  //val leagueIDs: Seq[Int] = 32626 to 32881 // 6 liga, 513-768
  //val leagueIDs: Seq[Int] = 32882 to 33137 // 6 liga, 769-1024
  //val leagueIDs: Seq[Int] = 58605 to 58860 // 7 liga, 1-256
  //val leagueIDs: Seq[Int] = 59048 to 59116 // 7 liga, 257-512
  //val leagueIDs: Seq[Int] = 59296 to 59372   // 7 liga, 513-768
  val leagueIDs: Seq[Int] = 59373 to 59628   // 7 liga, 513-768

  val pathToCsvFile = 
    if(leagueIDs.head >= 32114 && leagueIDs.last <= 32369) "C:\\Users\\Lukasz\\IdeaProjects\\Scrapper\\src\\data\\Polska_youthPlayerDatabase_6L_1_256.csv"
    else if(leagueIDs.head >= 32370 && leagueIDs.last <= 32625) "C:\\Users\\Lukasz\\IdeaProjects\\Scrapper\\src\\data\\Polska_youthPlayerDatabase_6L_257_512.csv"
    else if(leagueIDs.head >= 32626 && leagueIDs.last <= 32881) "C:\\Users\\Lukasz\\IdeaProjects\\Scrapper\\src\\data\\Polska_youthPlayerDatabase_6L_513_768.csv"
    else if(leagueIDs.head >= 32882 && leagueIDs.last <= 33137) "C:\\Users\\Lukasz\\IdeaProjects\\Scrapper\\src\\data\\Polska_youthPlayerDatabase_6L_769_1024.csv"
    else if(leagueIDs.head >= 58605 && leagueIDs.last <= 58860) "C:\\Users\\Lukasz\\IdeaProjects\\Scrapper\\src\\data\\Polska_youthPlayerDatabase_7L_1_256.csv"
    else if(leagueIDs.head >= 58861 && leagueIDs.last <= 59116) "C:\\Users\\Lukasz\\IdeaProjects\\Scrapper\\src\\data\\Polska_youthPlayerDatabase_7L_257_512.csv"
    else if(leagueIDs.head >= 59117 && leagueIDs.last <= 59372) "C:\\Users\\Lukasz\\IdeaProjects\\Scrapper\\src\\data\\Polska_youthPlayerDatabase_7L_513_768.csv"
    else if(leagueIDs.head >= 59373 && leagueIDs.last <= 59628) "C:\\Users\\Lukasz\\IdeaProjects\\Scrapper\\src\\data\\Polska_youthPlayerDatabase_7L_769_1024.csv"
    else "C:\\Users\\Lukasz\\IdeaProjects\\Scrapper\\src\\data\\sthGoesWrong.csv"

  leagueIDs.foreach(l_id => {
    teamsIDFromLeagueID(l_id).map(x => youthClubIDFromTeamID(x.toInt)).filter(p => !p.equals(0)).foreach(yc => {

      val currentPlayerIDs = YouthDatabase.getYouthPlayerIDsFromYouthClubID(yc)
      YouthDatabase.addPlayerToDatabase(pathToCsvFile, currentPlayerIDs)

    })

    val file = new File("src/data/config1_db.dat")
    val writer = CSVWriter.open(file, append = false)
    writer.writeRow(Seq(l_id.toString))
    writer.close()

  })

}

object updateDatabase extends App{}



