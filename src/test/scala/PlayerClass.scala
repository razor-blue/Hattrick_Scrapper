
import org.jsoup.{Connection, Jsoup}
import org.jsoup.nodes.{Document, Element}
import org.jsoup.select.Elements

import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import java.text.SimpleDateFormat
import java.util.Date
import java.util.Calendar
import scala.annotation.tailrec
import java.text.Normalizer

def getTodayDate(pattern: String = "dd.MM.yyyy"): String = {

  lazy val today: Date = Calendar.getInstance().getTime
  lazy val dateFormat = new SimpleDateFormat(pattern)
  dateFormat.format(today)

}

object QuickScanYouthPlayer{

    def Nationality(document: Document): String = document.select("div.byline").select("img[title]").attr("title").split(": ")(1)


}

class QuickScanYouthPlayer(args: Array[String]) {

    val url: String = args(0) + args(1)
    val connection: Connection = Jsoup.connect(url)
    val document: Document = connection.get()

    lazy val exists: Boolean = if (document.title.split("»").length <= 2) false else true

    lazy val nationality: Option[String] = if (exists) {
        try {
            Some(Youth.Nationality(document))
        }
        catch
            case _: Throwable =>
                try {
                    Some(Senior.Nationality(document))
                } catch {
                    case _: Throwable => None
                }
    }
    else None
    
}

object PlayerClass{

    def Age(document: Document): (Double, Int, Int) = {

      val wiek = document.select("div.byline").text()
      val years = wiek.split(" ")(0).toInt
      val days = wiek.split(" ")(3).toInt

        (years.toFloat + days.toFloat / 1000.0, years, days)

    }

    def RemoveDiacritics(text: String): String = {
      val normalizedText = Normalizer.normalize(text, Normalizer.Form.NFD)
        normalizedText.replaceAll("ł", "l").replaceAll("Ł", "L").replaceAll("[^\\p{ASCII}]", "")
    }

    def UpdatePreparation(line: String): (Array[String], String) = {

      val cols: Array[String] = line.split(",").map(_.trim)
      val colsDrop5: String = cols.drop(5).mkString(",").replaceAll("\"", "")

        (cols, colsDrop5)

    }

    def UpdateLineForNonExistingPlayer(colscolsDrop5: (Array[String], String), isYouthPlayer: Boolean): String = {

      val (cols, colsDrop5) = colscolsDrop5

      val sign = if(isYouthPlayer) 1 else -1

        if(math.abs(cols(4).toInt).equals(10000)) ""
        else f"${cols.take(4).mkString(",").replaceAll("\"", "")},${sign * 10000}," + colsDrop5

    }

    def AgeFormatLine(age: Double): String = f"$age%2.3f".replace(',', '.')

    def Outlook(document: Document): String = {

      val bodiesNoKits = document.select("img[src^=/Img/Avatar/bodies/]").attr("src").split("/")//
      val bodiesFromKits = document.select("img[src^=//res.hattrick.org/kits/]").attr("src").split("/")//(4).split("\\.").head

      val bodies = if(bodiesNoKits.length > bodiesFromKits.length) bodiesNoKits.last.split("\\.").head.split("_").head
      else bodiesFromKits.last.split("\\.").head.filterNot(Set('o','y').contains)

      val faces: String = document.select("img[src^=/Img/Avatar/faces/]").attr("src").split("/")(4).split("\\.").head.take(2)
      val eyes = document.select("img[src^=/Img/Avatar/eyes/]").attr("src").split("/")(4).split("\\.").head
      val mouths = document.select("img[src^=/Img/Avatar/mouths/]").attr("src").split("/")(4).split("\\.").head
      val noses = document.select("img[src^=/Img/Avatar/noses/]").attr("src").split("/")(4).split("\\.").head
      val hair_tmp: String = document.select("img[src^=/Img/Avatar/hair/]").attr("src").split("/")(4).split("\\.").head.drop(2)
      val hair = if(hair_tmp.length > 2) hair_tmp.dropRight(1) else hair_tmp

      val outlook = Seq(bodies, faces, eyes, mouths, noses, hair).mkString("-")

      outlook

    }





}

class PlayerClass(args: Array[String]) {

  val url: String = args(0) + args(1)
  val connection: Connection = Jsoup.connect(url)
  val document: Document = connection.get()

  lazy val exists: Boolean = if (document.title.split("»").length <= 2) false else true
  lazy val has_club: Boolean = if (document.title.split("»").length == 4) true else false

  lazy val id: Option[Int] = if (exists) Some(document.select("span.idNumber").text().replaceAll("[()]", "").toInt) else None

  lazy val name: Option[String] = if (exists) {
        //document.title.split("»").head.trim
        Some(PlayerClass.RemoveDiacritics(document.title.split("»").head.replaceAll(",","").trim))
    } else None

  lazy val age: Option[(Double, Int, Int)] = if (exists) Some(PlayerClass.Age(document)) else None
  lazy val link: Option[String] = if (exists) Some(args(0) + args(1)) else None

  lazy val specialityMap: Map[String, String] = Map("gra głową" -> "H", "nieprzewidywalny" -> "U", "techniczny" -> "T", "atletyczny" -> "P", "szybki" -> "Q", "witalny" -> "R", "pomocny" -> "S", "" -> "")
  lazy val speciality: Option[String] = if (exists) Some(specialityMap(document.select("td[colspan]").select("i[title]").attr("title"))) else None

  lazy val outlook: Option[String] = if(exists) Some(PlayerClass.Outlook(document)) else None


}

object Youth{

    def Nationality(document: Document): String = document.select("div.byline").select("img[title]").attr("title").split(": ")(1)
    def Since(document: Document): Int = document.select("span.shy span[dir=ltr]").first.text().split(" ")(2).toInt

    def BestPerformances(document: Document): String = {

        val arrayString: Array[String] = document.select("table.youthPlayerPerformance").select("td.middle").
          text().replaceAll("-", "-1 -").replaceAll("boczny obrońca", "boczny_obrońca").split(" ")

        @tailrec
        def addString(str: String, stringToAdd: Array[String], index: Int): String =
            if (index >= stringToAdd.length) str
            else addString(str + "," + stringToAdd(index), stringToAdd, index + 3)

        addString(arrayString(1), arrayString, 4)

    }

    def Last5Performances(document: Document): (Seq[Double], String) = {

      val arrayString1: mutable.Buffer[String] = document.select("div.mainBox").select("td.middle").asScala.map(x => x.text().replaceAll("boczny obrońca", "boczny_obrońca"))
      val arrayString2 = document.select("div.mainBox").select("span.stars-full").asScala
      val arrayString3 = document.select("div.mainBox").select("td.top").asScala

      val nGames = arrayString2.length

      val dictionary = Seq("bramkarz", "stoper", "boczny_obrońca", "pomocnik", "skrzydłowy", "napastnik")
      val dictionaryMap = Map("bramkarz" -> 0, "stoper" -> 1, "boczny_obrońca" -> 2, "pomocnik" -> 3, "skrzydłowy" -> 4, "napastnik" -> 5)

      val lastMatchDetails: String =
            if(nGames > 0)
                PlayerClass.RemoveDiacritics(arrayString1.head.split(" ").head + " " + arrayString1(2))
            else
                "--------"

      val data: Seq[(Int, Double)] = (0 until nGames).map(i => {
        val nPositionsPlayed = dictionary.count(p => arrayString1(2 + i * 3).contains(p))
        val minutesPlayed: Int = if (nPositionsPlayed == 1) arrayString1(2 + i * 3).split(" ").last.replaceAll("[(')]", "").toInt else 0
        val positionPlayed: Int = if (nPositionsPlayed == 1  && minutesPlayed == 90) dictionaryMap(arrayString1(2 + i * 3).split(" ").head) else -1
        val starsPlayed: Double = if (arrayString3(i).text().nonEmpty) {arrayString3(i).text().toDouble} else -1.0

            (positionPlayed, starsPlayed)
        })

      val newRecord: Seq[Double] = (0 to 5).map(i => {
        val tmp = data.filter(p => p._1 == i).map(_._2)
        val tmp2 = if (tmp.isEmpty) -1 else tmp.max
            tmp2
        })

        (newRecord,lastMatchDetails)

    }

    def Read_td(worldCupNumber: Int): Elements = {

      val url: String = s"https://hattrickportal.pro/Tracker/U20/U21WC.aspx?WorldCup=$worldCupNumber"
      val connection: Connection = Jsoup.connect(url)
      val document: Document = connection.get()

        document.select("td")

    }

    def WorldCupAgeMinMax(worldCupNumber: Int): ((Double, Int, Int), (Double, Int, Int)) = {

      val td = Read_td(worldCupNumber: Int).asScala

      val maxAgeYears = td(3).text().split(" ")(0).toInt
      val maxAgeDays = td(3).text().split(" ")(3).toInt

      val minAgeYears = td(167).text().split(" ")(0).toInt
      val minAgeDays = td(167).text().split(" ")(3).toInt

      val ageMax = maxAgeYears + maxAgeDays / 1000.0
      val ageMin = minAgeYears + minAgeDays / 1000.0

        ((ageMin, minAgeYears, minAgeDays), (ageMax, maxAgeYears, maxAgeDays))

    }

    @tailrec
    def WhichWorldCup(playerAge: (Double, Int, Int), worldCupNumber: Int): Int = {

      val ageMin = WorldCupAgeMinMax(worldCupNumber)._1._1
        //val ageMax = WorldCupAgeMinMax(worldCupNumber)._2._1

      val ageCurrent = playerAge._1

        if (ageCurrent >= ageMin) worldCupNumber
        else WhichWorldCup(playerAge, worldCupNumber + 1)

    }

    @tailrec
    def f(td: mutable.Buffer[Element], worldCupNumber: Int, playerAge: Double, index: Int): String = {

      val age = td(index).text().split(" ")(0).toDouble + td(index).text().split(" ")(3).toDouble / 1000.0
      val round_id = ((index - 7) / 4.0 + 1.0).toInt
      val round = td(index - 5).text().replaceAll(",", " ")
      val date = td(index - 6).text()
      val week = td(index - 7).text().replaceAll("/", ",")

        if (playerAge > age) s"WC ${worldCupNumber - 1} --> $round_id --> $round --> $date --> $week"
        else f(td, worldCupNumber, playerAge, index + 4)

    }

    def WhichRoundOfWorldCup(playerAge: (Double, Int, Int), worldCupNumber: Int): String = {

      val td: mutable.Buffer[Element] = Read_td(worldCupNumber).asScala

      val age = WorldCupAgeMinMax(worldCupNumber)

      val ageMin = age._1._1
      val ageMax = age._2._1

        if (playerAge._1 > ageMax) s"WC ${worldCupNumber - 1} --> ,Final,,,"
        else if (playerAge._1 <= ageMin) s"WC $worldCupNumber --> ,Final,,,"
        else f(td, worldCupNumber, playerAge._1, 7)

    }

    def Availability(age: (Double, Int, Int)): String = {

      val i = 37

      val worldCupNumber = WhichWorldCup(age, i)
      val worldCupRound = WhichRoundOfWorldCup(age, worldCupNumber)

        worldCupRound

    }
    
    def UpdateNonExistingPlayer(line: String): String = {

      val cols_colsDrop5 = PlayerClass.UpdatePreparation(line)

        PlayerClass.UpdateLineForNonExistingPlayer(cols_colsDrop5, true)
        
    }


    def UpdateExistingPlayer(yp: Youth, b5p: Seq[String], l5p: Seq[Double]): String = {

      val last5Games: (Seq[Double], String) = yp.last5Performances.getOrElse((Seq(-1.0, -1.0, -1.0, -1.0, -1.0, -1.0), "-----"))
      val lastGame = last5Games._2
      val bestPerformances: String = yp.bestPerformances.getOrElse(b5p.mkString(","))

      val age: String = PlayerClass.AgeFormatLine(yp.age.get._1)
      val since = yp.since.get
      val specialityStatus = yp.speciality.getOrElse("")
        println(s"$specialityStatus")
      val speciality = if (!specialityStatus.equals("")) specialityStatus.concat(s"$since") else specialityStatus

      val outlook: String = yp.outlook.get

        println(f"${yp.name.get},${yp.id.get},$age,$speciality,$since,${yp.availability.get.replaceAll(" --> ", ",")},$bestPerformances,${last5Games._1.zip(l5p).map(x => math.max(x._1, x._2)).mkString(",")},$lastGame,${yp.nationality.get},${getTodayDate()},$outlook")
        f"${yp.name.get},${yp.id.get},$age,$speciality,${yp.since.get},${yp.availability.get.replaceAll(" --> ", ",")},$bestPerformances,${last5Games._1.zip(l5p).map(x => math.max(x._1, x._2)).mkString(",")},$lastGame,${yp.nationality.get},${getTodayDate()},$outlook"

    }

    def UpdateExistingPlayer1(yp: Youth, b5p: Seq[String], l5p: Seq[Double], line: String): String = {

      val last5Games: (Seq[Double], String) = yp.last5Performances.getOrElse((Seq(-1.0, -1.0, -1.0, -1.0, -1.0, -1.0), "-----"))
      val lastGame = last5Games._2
      val bestPerformances: String = yp.bestPerformances.getOrElse(b5p.mkString(","))

      val age: String = PlayerClass.AgeFormatLine(yp.age.get._1)

      val playerAtributes: Array[String] = line.split(",").map(_.trim)

      val outlook: String = if(playerAtributes.length <= 26) yp.outlook.get
      else playerAtributes(26)

      val (name, /*id, */ previousSpecialityStatus, availability_wc, availability_num, availability_descr, availability_lastM, availability_lSeason, availability_lWeek, nationality)
        = (playerAtributes(0)/*,playerAtributes(1)*/,playerAtributes(3),playerAtributes(5),playerAtributes(6),playerAtributes(7),playerAtributes(8),playerAtributes(9),playerAtributes(10),playerAtributes(24))

      val currentSpecialityStatus: String = yp.speciality.getOrElse("")
      val since = yp.since.get
      val id = yp.id.get.toString

      val speciality = if(currentSpecialityStatus.equals(previousSpecialityStatus)) currentSpecialityStatus else currentSpecialityStatus.concat(s"$since")

      val last5Games_updated = last5Games._1.zip(l5p).map(x => math.max(x._1, x._2)).mkString(",")

      val f: String = f"$name,$id,$age,$speciality,$since,$availability_wc,$availability_num,$availability_descr,$availability_lastM,$availability_lSeason,$availability_lWeek,$bestPerformances,$last5Games_updated,$lastGame,$nationality,${getTodayDate()},$outlook"

        if(!yp.stillInYouthAcademy) {

          val sp = new Senior(Array(seniorPlayerPath,id))

            if(sp.onTL.get) {
              val skills = sp.skills.get
                writeToFile(databasePath + "TL_listed.csv", true, Seq.empty[String], Seq(s"$name,${sp.daysInClub.get},$id,$last5Games_updated,${skills.productIterator.mkString(",")}"))
            }

            if(sp.exists){
              val character = sp.character.get
              writeToFile(databasePath + "characters.csv", true, Seq.empty[String], Seq(s"$name,${sp.daysInClub.get},$id,${outlook.replaceAll("-", ",")},$character"))

            }

        }


        println(f)
        //println(s"${currentSpecialityStatus.equals(previousSpecialityStatus)} $previousSpecialityStatus $currentSpecialityStatus $speciality")
        f

        //f"${name/*.replaceAll("\"","")*/},${id},$age,${yp.speciality.getOrElse("-")},${yp.since.get},$availability_wc,$availability_num,$availability_descr,$availability_lastM,$availability_lSeazon,$availability_lWeek,$bestPerformances,${last5Games._1.zip(l5p).map(x => math.max(x._1, x._2)).mkString(",")},$lastGame,$nationality,${getTodayDate()}"

        //println(f"${yp.name.get},${yp.id.get},$age,${yp.speciality.getOrElse("-")},${yp.since.get},${yp.availability.get.replaceAll(" --> ", ",")},$bestPerformances,${last5Games._1.zip(l5p).map(x => math.max(x._1, x._2)).mkString(",")},$lastGame,${yp.nationality.get},${getTodayDate()}")
        //f"${yp.name.get},${yp.id.get},$age,${yp.speciality.getOrElse("-")},${yp.since.get},${yp.availability.get.replaceAll(" --> ", ",")},$bestPerformances,${last5Games._1.zip(l5p).map(x => math.max(x._1, x._2)).mkString(",")},$lastGame,${yp.nationality.get},${getTodayDate()}"


    }

}

class Youth(args: Array[String]) extends PlayerClass(args){

  lazy val stillInYouthAcademy: Boolean = id.getOrElse(false).equals(args(1).toInt)

  //println(s"$stillInYouthAcademy")

  lazy val nationality: Option[String] = if(exists) {
        try {
            Some(Youth.Nationality(document))
        }
        catch
            case _: Throwable =>
                try {
                    Some(Senior.Nationality(document))
                } catch {
                    case _: Throwable => None
                }
    }
    else None

  lazy val since: Option[Int] = if (exists) {
        try {
            Some(Youth.Since(document))
        }
        catch
            case _: Throwable =>
                try {
                    Some(-1) //promoted to senior team
                } catch {
                    case _: Throwable => None
                }
    }
    else None

  lazy val availability: Option[String] = if( exists) Some(Youth.Availability(age.get)) else None

  lazy val bestPerformances: Option[String] = if(stillInYouthAcademy) Some(Youth.BestPerformances(document)) else None
  lazy val last5Performances: Option[(Seq[Double],String)] = if(stillInYouthAcademy) Some(Youth.Last5Performances(document)) else None

}

object Senior{

    def Nationality(document: Document): String = document.select("div.byline").select("img[title]").attr("title")

    def OnTL(document: Document): Boolean = document.select("div.transferPlayerSkills").asScala.length.equals(1)

    def NewDateFormat(pattern: String = "dd.MM.yyyy") = new SimpleDateFormat(pattern)

    def SinceFrom(document: Document): String = {
      document.select("span.shy span[dir=ltr]").text().split(" ")(1).replace(")", "")
    }

    def DaysInClub(joinDateString: String): Long = {
      val dateFormat = NewDateFormat() //default value = dd.mm.rrrr
      //val dateFormat: SimpleDateFormat = new SimpleDateFormat("dd.MM.yyyy") //default value = dd.mm.rrrr
      val today: Date = Calendar.getInstance().getTime
      val joinDate: Date = dateFormat.parse(joinDateString)
      (today.getTime - joinDate.getTime) / (1000 * 60 * 60 * 24)
    }

    def UpdateNonExistingPlayer(line: String): String = {

      val cols_colsDrop5 = PlayerClass.UpdatePreparation(line)

        PlayerClass.UpdateLineForNonExistingPlayer(cols_colsDrop5, false)
    }
    
    def UpdateExistingPlayer(sp: Senior, line: String): String = {

      val colsDrop5 = PlayerClass.UpdatePreparation(line)._2
      val age: String = PlayerClass.AgeFormatLine(sp.age.get._1)

       if(age.toDouble < 18.0)
         f"${sp.name.get},${sp.id.get},$age,${sp.speciality.getOrElse("-")},-2," + colsDrop5
       else
         null

    }

    def Skills(bufferElement: mutable.Buffer[Element]): (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)
    = {

      val index: Int = bufferElement.indexOf(bufferElement.find(_.text == "TSI").get)

      val TSI = bufferElement(index + 1).select("td").text().replaceAll(" ", "").toInt

      val Salary20 = bufferElement(index + 3).select("td").text().replaceAll(" ", "").replaceAll("zł/tydzień", "")
      val SalaryBASE = bufferElement(index + 3).select("td").select("span").attr("title").split(" ")(0).replaceAll("zł/tydzień", "").trim.replaceAll(" ", "")
      val Salary = (if (SalaryBASE.nonEmpty) SalaryBASE else Salary20).toInt

      val Form = bufferElement(index + 7).select("span.denominationNumber").text().split(" ").head.toInt
      val Condition = bufferElement(index + 10).select("span.denominationNumber").text().split(" ").head.toInt
      val GK = bufferElement(index + 14).select("span.denominationNumber").text().split(" ").head.toInt
      val DEF = bufferElement(index + 17).select("span.denominationNumber").text().split(" ").head.toInt
      val PM = bufferElement(index + 20).select("span.denominationNumber").text().split(" ").head.toInt
      val WG = bufferElement(index + 23).select("span.denominationNumber").text().split(" ").head.toInt
      val PASS = bufferElement(index + 26).select("span.denominationNumber").text().split(" ").head.toInt
      val SCO = bufferElement(index + 29).select("span.denominationNumber").text().split(" ").head.toInt
      val SP = bufferElement(index + 32).select("span.denominationNumber").text().split(" ").head.toInt

        (TSI, Salary, Form, Condition, GK, DEF, PM, WG, PASS, SCO, SP)

    }

  def Character(document: Document): String = {

    val gentleness = document.select("a[href^=/pl/Help/Rules/AppDenominations.aspx?lt=gentleness]").text()
    val aggressiveness = document.select("a[href^=/pl/Help/Rules/AppDenominations.aspx?lt=aggressiveness]").text()
    val honesty = document.select("a[href^=/pl/Help/Rules/AppDenominations.aspx?lt=honesty]").text()
    
    val leadership = document.select("a[href^=/pl/Help/Rules/AppDenominations.aspx?lt=leadership]").text()

    val gentlenessMap = Map("złośliwy" -> "0", "kontrowersyjny" -> "1", "przyjemny" -> "2", "sympatyczny" -> "3", "popularny" -> "4", "uwielbiany przez zespół" -> "5")
    val aggressivenessMap = Map("złośliwy" -> "0", "kontrowersyjny" -> "1", "przyjemny" -> "2", "sympatyczny" -> "3", "popularny" -> "4", "uwielbiany przez zespół" -> "5")

    val character = Seq(gentleness, aggressiveness, honesty, leadership).mkString(",")

    character

  }
    
}

class Senior(args: Array[String]) extends PlayerClass(args){

    /*var a = 0
    println(s"${a+=1}")*/

  lazy val nationality: Option[String] = if(exists) Some(Senior.Nationality(document)) else None

  lazy val onTL: Option[Boolean] = if(exists) Some(Senior.OnTL(document)) else None

  lazy val info: mutable.Buffer[Element] = document.select("td").asScala

  lazy val info2: mutable.Buffer[Element] = document.select("span").asScala

  lazy val sinceFrom: Option[String] = if(exists && has_club)Some(Senior.SinceFrom(document)) else None

  lazy val daysInClub: Option[Long] = if (exists && has_club) {
    Some(Senior.DaysInClub(sinceFrom.get))
  } else None

  lazy val skills: Option[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)] = if (onTL.getOrElse(false)) Some(Senior.Skills(info)) else None

  lazy val tsi: Option[Int] = if (onTL.getOrElse(false)) Some(skills.get._1) else None
  lazy val salary: Option[Int] = if (onTL.getOrElse(false)) Some(skills.get._2) else None
  lazy val form: Option[Int] = if (onTL.getOrElse(false)) Some(skills.get._3) else None
  lazy val condition: Option[Int] = if (onTL.getOrElse(false)) Some(skills.get._4) else None
  lazy val gk: Option[Int] = if (onTL.getOrElse(false)) Some(skills.get._5) else None
  lazy val df: Option[Int] = if (onTL.getOrElse(false)) Some(skills.get._6) else None
  lazy val pm: Option[Int] = if (onTL.getOrElse(false)) Some(skills.get._7) else None
  lazy val wg: Option[Int] = if (onTL.getOrElse(false)) Some(skills.get._8) else None
  lazy val pass: Option[Int] = if (onTL.getOrElse(false)) Some(skills.get._9) else None
  lazy val sco: Option[Int] = if (onTL.getOrElse(false)) Some(skills.get._10) else None
  lazy val sp: Option[Int] = if (onTL.getOrElse(false)) Some(skills.get._11) else None

  lazy val character: Option[String] = if(exists) Some(Senior.Character(document)) else None

}