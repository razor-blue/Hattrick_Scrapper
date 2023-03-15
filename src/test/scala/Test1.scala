object Test1 extends App {

  println(s"drugi test")
}


  //import Pla.{Age, Nationality}
  import org.jsoup.{Connection, Jsoup}
  import org.jsoup.nodes.Element
  import org.jsoup.select.Elements

  import scala.collection.mutable
  import scala.jdk.CollectionConverters.*
  import java.text.SimpleDateFormat
  import java.util.Date
  import java.util.Calendar
  import scala.annotation.tailrec
  import java.io.File
  import com.github.tototoshi.csv.*

/*import org.openqa.selenium.{By, WebDriver, WebElement}
import org.openqa.selenium.chrome.ChromeDriver
import org.openqa.selenium.support.ui.{ExpectedConditions, WebDriverWait}

import org.openqa.selenium.chrome.{ChromeDriver, ChromeOptions}*/
import java.io.File
import scala.util.Try

  import scala.concurrent.ExecutionContextExecutor

/*import collection.convert.ImplicitConversions.seqAsJavaList
import collection.convert.ImplicitConversionsToScala.collectionAsScalaIterable

import collection.convert.ImplicitConversions.listAsScalaBuffer

import collection.convert.ImplicitConversionsToScala.listAsScalaBuffer*/


/*object WebScraper {
    def main(args: Array[String]): Unit = {
      //val url = "https://www.example.com"
      val url = "https://hattrick.org"
      val doc = Jsoup.connect(url).get()
      val title = doc.title()
      println(s"Tytuł strony: $title")
    }
  }

object Main extends App {
  WebScraper.main(Array.empty)
}*/

import org.jsoup.Jsoup
import org.jsoup.nodes.Document

object Pla{

  def Age(document: Document): (Double, Int, Int) = {

    val wiek = document.select("div.byline").text()
    val years = wiek.split(" ")(0).toInt
    val days = wiek.split(" ")(3).toInt

    (years.toFloat + days.toFloat / 1000.0, years, days)

  }




}

class Pla(args: Array[String]) {

  val url: String = args(0)+args(1)
  val connection: Connection = Jsoup.connect(url)
  val document: Document = connection.get()

  val exists: Boolean = if (document.title.split("»").length == 1) false else true
  val has_club: Boolean = if (document.title.split("»").length == 4) true else false

  val id: Option[Int] = if(exists) Some(document.select("span.idNumber").text().replaceAll("[()]", "").toInt) else None

  val name: Option[String] = if (exists) Some(document.title.split("»").head.trim) else None

  val age: Option[(Double, Int, Int)] = if (exists) Some(Pla.Age(document)) else None
  val link: Option[String] = if (exists) Some(args(0)+args(1)) else None

  val speciality: Option[String] = if (exists) Some(document.select("td[colspan]").select("i[title]").attr("title")) else None


}

object Y{

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

    //println(s"${arrayString3.map(x => x.text())}")

    val stars: Seq[Double] = arrayString3.indices.map(i => arrayString3(i).text().toDouble)

    val nGames = arrayString2.length

    val dictionary = Seq("bramkarz", "stoper", "boczny_obrońca", "pomocnik", "skrzydłowy", "napastnik")
    val dictionaryMap = Map("bramkarz" -> 0, "stoper" -> 1, "boczny_obrońca" -> 2, "pomocnik" -> 3, "skrzydłowy" -> 4, "napastnik" -> 5)

    val lastMatchDetails: String = if(nGames > 0) arrayString1.head.split(" ").head + " " + arrayString1(2) else "--------"

    val data: Seq[(Int, Double)] = (0 until nGames).map(i => {
      val nPositionsPlayed = dictionary.count(p => arrayString1(2 + i * 3).contains(p))
      val minutesPlayed: Int = if (nPositionsPlayed == 1) arrayString1(2 + i * 3).split(" ").last.replaceAll("[(')]", "").toInt else 0
      val positionPlayed: Int = if (nPositionsPlayed == 1 && minutesPlayed == 90) dictionaryMap(arrayString1(2 + i * 3).split(" ").head) else -1
      val starsPlayed: Double = arrayString3(i).text().toDouble


      println(s"$i: ${arrayString1(0 + i * 3).split(" ").head} ${arrayString1(2 + i * 3)} -> ${arrayString3(i).text()} --> $nPositionsPlayed ---> $minutesPlayed ----> $positionPlayed -----> $starsPlayed")
      (positionPlayed, starsPlayed)
    })

    //println(s"data: $data")

    val newRecord: Seq[Double] = (0 to 5).map(i => {
      val tmp = data.filter(p => p._1 == i).map(_._2)
      val tmp2 = if (tmp.isEmpty) -1 else tmp.max
      tmp2
    })

    //println(newRecord)

    //println(arrayString2.length)
    //arrayString1.foreach(println(_))
    //arrayString2.foreach(println(_))

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
    val ageMax = WorldCupAgeMinMax(worldCupNumber)._2._1

    val ageCurrent = playerAge._1

    //if(ageCurrent <= ageMax && ageCurrent >= ageMin) worldCupNumber
    if (ageCurrent >= ageMin) worldCupNumber
    else WhichWorldCup(playerAge, worldCupNumber + 1)

  }

  @tailrec
  def f(td: mutable.Buffer[Element], worldCupNumber: Int, playerAge: Double, index: Int): String = {

    val age = td(index).text().split(" ")(0).toDouble + td(index).text().split(" ")(3).toDouble / 1000.0
    val round_id = ((index - 7) / 4.0 + 1.0).toInt
    val round = td(index - 5).text().replaceAll(",", " ") //round for previous
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
}

class Y(args: Array[String]) extends Pla(args){

  val stillInYouthAcademy: Boolean = id.get.equals(args(1).toInt)

  val nationality: Option[String] = if(exists) {
    try {
      Some(Y.Nationality(document))
    }
      catch
        case _: Throwable =>
          try {
            Some(S.Nationality(document))
          } catch {
            case _: Throwable => None
          }
  }
  else None

  val since: Option[Int] = if (exists) {
    try {
      Some(Y.Since(document))
    }
    catch
      case _: Throwable =>
        try {
          Some(-1) //przeniesiony do seniorów
        } catch {
          case _: Throwable => None
        }
  }
  else None

  val availability: Option[String] = if( exists) Some(Y.Availability(age.get)) else None

  val bestPerformances: Option[String] = if(stillInYouthAcademy) Some(Y.BestPerformances(document))  else None
  val last5Performances: Option[(Seq[Double], String)] = if(stillInYouthAcademy) Some(Y.Last5Performances(document)) else None


}

object S{

  def Nationality(document: Document): String = document.select("div.byline").select("img[title]").attr("title")


}

class S(args: Array[String]) extends Pla(args){

  val nationality: Option[String] = if(exists) Some(S.Nationality(document)) else None

}

object inh_test extends App{

  val List_of_ids = Seq("324275438","320411270","317512515")

  //val p1 = new Y(Array(s"https://www.hattrick.org/pl/Club/Players/YouthPlayer.aspx?YouthPlayerID=","317512515"))
  //val p1 = new Y(Array(s"https://www.hattrick.org/pl/Club/Players/YouthPlayer.aspx?YouthPlayerID=","324275438"))
  val p1 = new Y(Array(s"https://www.hattrick.org/pl/Club/Players/YouthPlayer.aspx?YouthPlayerID=","310526039"))
  //val p1 = new Y(Array("https://www.hattrick.org/pl/Club/Players/YouthPlayer.aspx?YouthPlayerID=","310723669"))

  val file = new File("C:\\Users\\Lukasz\\IdeaProjects\\Scrapper\\src\\data\\youthPlayerDatabase.csv")
  val writer = CSVWriter.open(file, append = false)
  List_of_ids.foreach(id => {

    val p = new Y(Array(s"https://www.hattrick.org/pl/Club/Players/YouthPlayer.aspx?YouthPlayerID=", s"$id"))
    if (p.exists && p.stillInYouthAcademy) {
      val l5p = p.last5Performances.get._1.mkString(",")
      println(l5p)
      writer.writeAll(List(
        List(s"${p.name.get},${p.id.get},${p.age.get._1},${p.since.get},${p.availability.get.replaceAll(" --> ", ",")},${p.bestPerformances.get},$l5p")
      ))
    }
  })
  writer.close()


  println(p1.exists)
  println(p1.has_club)
  println(p1.name.get)
  println(p1.age.get._1)
  println(p1.nationality.get)
  println(p1.since.get)
  println(p1.link.get)
  println(p1.availability.get)
  println(p1.id.get)
  println(s"Specjalność: ${p1.speciality.get}")
  if(p1.stillInYouthAcademy)println(p1.bestPerformances.get)
  if(p1.stillInYouthAcademy)println(p1.last5Performances.get)






  val p2 = new S(Array(s"https://www.hattrick.org/pl/Club/Players/Player.aspx?playerId=","468663386"))

  println(p2.exists)
  println(p2.has_club)
  println(p2.name.get)
  println(p2.age.get._1)
  println(p2.nationality.get)
  println(s"Specjalność: ${p2.speciality.get}")
  println(p2.link.get)



}

class Player{

  def main(args: Array[String]): Unit = {
    val url = args(0)
    val connection = Jsoup.connect(url)
    val document: Document = connection.get()
    //val forSale = checkIfForSale(document)
    val TL_status = document.select("div.transferPlayerInformation").asScala.length
  }

  def checkIfForSale(info: Document): Int = info.select("div.transferPlayerInformation").asScala.length
}

object Player1 {

  def elementContain(element: Element, string: String): Boolean = element.text().contains(string)

  def NoGamesPlayed(element: Element): Boolean = elementContain(element, "Ostatnio nie grał")

  def NewDateFormat(pattern: String = "dd.MM.yyyy") = new SimpleDateFormat(pattern)

  def SinceFrom(element: Element): String = element.select("span").text().split(" ")(1).replace(")", "")

  def OnTL(document: Document): Boolean = document.select("div.transferPlayerSkills").asScala.length.equals(1)

  def DaysInClub(joinDateString: String): Long = {
    val dateFormat = NewDateFormat() //default value = dd.mm.rrrr
    val today: Date = Calendar.getInstance().getTime
    val joinDate: Date = dateFormat.parse(joinDateString)
    (today.getTime - joinDate.getTime) / (1000 * 60 * 60 * 24)
  }

  def Loyalty(element: Element): String = element.select("span").text().split(" ").head.replaceAll("[()]", "")

  def Age(document: Document): (Double, Int, Int) = {

    val wiek = document.select("div.byline").text()
    val years = wiek.split(" ")(0).toInt
    val days = wiek.split(" ")(3).toInt

    (years.toFloat + days.toFloat / 1000.0, years, days)

  }

  def Skills(bufferElement: mutable.Buffer[Element]): (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) = {

    val index: Int = bufferElement.indexOf(bufferElement.find(_.text == "TSI").get)

    val TSI = bufferElement(index+1).select("td").text().replaceAll(" ", "").toInt

    val Salary20 = bufferElement(index + 3).select("td").text().replaceAll(" ", "").replaceAll("zł/tydzień", "")
    val SalaryBASE = bufferElement(index + 3).select("td").select("span").attr("title").split(" ")(0).replaceAll("zł/tydzień", "").trim.replaceAll(" ", "")
    val Salary = (if (SalaryBASE.nonEmpty) SalaryBASE else Salary20).toInt

    val Form = bufferElement(index+7).select("span.denominationNumber").text().split(" ").head.toInt
    val Condition = bufferElement(index+10).select("span.denominationNumber").text().split(" ").head.toInt
    val GK = bufferElement(index+14).select("span.denominationNumber").text().split(" ").head.toInt
    val DEF = bufferElement(index+17).select("span.denominationNumber").text().split(" ").head.toInt
    val PM = bufferElement(index+20).select("span.denominationNumber").text().split(" ").head.toInt
    val WG = bufferElement(index+23).select("span.denominationNumber").text().split(" ").head.toInt
    val PASS = bufferElement(index+26).select("span.denominationNumber").text().split(" ").head.toInt
    val SCO = bufferElement(index+29).select("span.denominationNumber").text().split(" ").head.toInt
    val SP = bufferElement(index+32).select("span.denominationNumber").text().split(" ").head.toInt

    (TSI,Salary,Form,Condition,GK,DEF,PM,WG,PASS,SCO,SP)

  }

}

class Player1 (args: Array[String]){

    val url: String = args(0)
    val connection: Connection = Jsoup.connect(url)
    val document: Document = connection.get()

    val exists: Boolean = if(document.title.split("»").length == 1) false else true
    val has_club: Boolean = if(document.title.split("»").length == 4) true else false

    val name: Option[String] = if(exists) Some(document.title.split("»").head) else None
    /*val exists = if(name.equalsIgnoreCase("hattrick")) false else true*/

  println(s"$name")

    val onTL: Boolean = Player1.OnTL(document)
    //val info = document.select("a.copyToClipboard").text.split(" ")
    val info: mutable.Buffer[Element] = document.select("td").asScala//.length
    val info1: mutable.Buffer[Element] = document.select("div").asScala//.length
    val info2: mutable.Buffer[Element] = document.select("span").asScala//.length
    val info3: mutable.Buffer[Element] = document.select("table.htbox-table").asScala//.length

    val no_match: Option[Boolean] = if(exists && has_club)Some(Player1.NoGamesPlayed(info1(2))) else None
    val sinceFrom: Option[String] = if(exists && has_club)Some(Player1.SinceFrom(info2(19))) else None
    /*println(s"${args(0)}")
    println(s"${sinceFrom}")*/
    val daysInClub: Option[Long] = if(exists && has_club)Some(Player1.DaysInClub(Player1.SinceFrom(info2(19)))) else None

    val loyalty: Option[String] = if(exists) Some(Player1.Loyalty(info2(18))) else None

    val age: Option[(Double, Int, Int)] = if(exists) Some(Player1.Age(document)) else None

    val skills: Option[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)] = if(onTL) Some(Player1.Skills(info)) else None

    val tsi: Option[Int] = if(onTL) Some(skills.get._1) else None
    val salary: Option[Int] = if(onTL) Some(skills.get._2) else None
    val form: Option[Int] = if(onTL) Some(skills.get._3) else None
    val condition: Option[Int] = if(onTL) Some(skills.get._4) else None
    val gk: Option[Int] = if(onTL) Some(skills.get._5) else None
    val df: Option[Int] = if(onTL) Some(skills.get._6) else None
    val pm: Option[Int] = if(onTL) Some(skills.get._7) else None
    val wg: Option[Int] = if(onTL) Some(skills.get._8) else None
    val pass: Option[Int] = if(onTL) Some(skills.get._9) else None
    val sco: Option[Int] = if(onTL) Some(skills.get._10) else None
    val sp: Option[Int] = if(onTL) Some(skills.get._11) else None


  }

object YouthPlayer{

  def Age(document: Document): (Double, Int, Int) = {

    val wiek = document.select("div.byline").text()
    val years = wiek.split(" ")(0).toInt
    val days = wiek.split(" ")(3).toInt

    (years.toFloat + days.toFloat / 1000.0, years, days)
    //(14.toFloat + 101.toFloat / 1000.0, 14, 111)

  }

  def Nationality(document: Document): String = document.select("div.byline").select("img[title]").attr("title").split(": ")(1)

  def BestPerformances(document: Document): String = {

    /*document.select("table.youthPlayerPerformance").select("td.middle").text().split(" ").
      map(s => if(s.equals("-")) s + " -1 "
      else s
    ).foldLeft("")((acc,s) => acc + " " +s).trim */

    val arrayString: Array[String] = document.select("table.youthPlayerPerformance").select("td.middle").
      text().replaceAll("-","-1 -").replaceAll("boczny obrońca","boczny_obrońca").split(" ")

    @tailrec
    def addString(str: String, stringToAdd: Array[String], index: Int): String =

      if(index >= stringToAdd.length) str
      else addString(str + "," + stringToAdd(index), stringToAdd, index + 3)

    addString(arrayString(1), arrayString,4)

  }

  def Last5Performances(document: Document): Seq[Double] = {

    val arrayString1: mutable.Buffer[String] = document.select("div.mainBox").select("td.middle").asScala.map(x => x.text().replaceAll("boczny obrońca" ,"boczny_obrońca"))
    val arrayString2 = document.select("div.mainBox").select("span.stars-full").asScala

    val nGames = arrayString2.length

    val dictionary = Seq("bramkarz", "boczny_obrońca", "stoper", "pomocnik", "skrzydłowy", "napastnik")
    val dictionaryMap = Map("bramkarz" -> 0, "boczny_obrońca" -> 1, "stoper" -> 2, "pomocnik" -> 3, "skrzydłowy" -> 4, "napastnik" -> 5)



      val data: Seq[(Int, Double)] = (0 until nGames).map(i => {
        val nPositionsPlayed = dictionary.count(p => arrayString1(2 + i * 3).contains(p))
        val minutesPlayed = if(nPositionsPlayed == 1) arrayString1(2 + i * 3).split(" ").last.replaceAll("[(')]", "").toInt else 0
        val positionPlayed: Int = if(nPositionsPlayed == 1) dictionaryMap(arrayString1(2 + i * 3).split(" ").head) else -1
        val starsPlayed: Double = arrayString2(i).text().toDouble

        //println(s"$i: ${arrayString1(2 + i * 3)} -> ${arrayString2(i)} --> $nPositionsPlayed ---> $minutesPlayed ----> $positionPlayed -----> $starsPlayed")
        (positionPlayed, starsPlayed)
      })

    //println(data)

    val newRecord: Seq[Double] = (0 to 5).map(i => {
      val tmp = data.filter(p => p._1 == i).map(_._2)
      val tmp2 = if(tmp.isEmpty) -1 else tmp.max
      tmp2
    })

    println(newRecord)

    println(arrayString2.length)
    arrayString1.foreach(println(_))
    arrayString2.foreach(println(_))

    newRecord

  }

  def Read_td(worldCupNumber: Int): Elements = {

    val url: String = s"https://hattrickportal.pro/Tracker/U20/U21WC.aspx?WorldCup=$worldCupNumber"
    val connection: Connection = Jsoup.connect(url)
    val document: Document = connection.get()

    document.select("td")

  }

  def WorldCupAgeMinMax(worldCupNumber: Int): ((Double, Int, Int),(Double, Int, Int)) = {

    val td = Read_td(worldCupNumber: Int).asScala

    val maxAgeYears = td(3).text().split(" ")(0).toInt
    val maxAgeDays = td(3).text().split(" ")(3).toInt

    val minAgeYears = td(167).text().split(" ")(0).toInt
    val minAgeDays = td(167).text().split(" ")(3).toInt

    val ageMax = maxAgeYears + maxAgeDays / 1000.0
    val ageMin = minAgeYears + minAgeDays / 1000.0

    ((ageMin,minAgeYears,minAgeDays),(ageMax,maxAgeYears,maxAgeDays))

  }

  @tailrec
  def WhichWorldCup(playerAge: (Double, Int, Int), worldCupNumber: Int): Int = {

    val ageMin = WorldCupAgeMinMax(worldCupNumber)._1._1
    val ageMax = WorldCupAgeMinMax(worldCupNumber)._2._1

    val ageCurrent = playerAge._1

    //if(ageCurrent <= ageMax && ageCurrent >= ageMin) worldCupNumber
    if(ageCurrent >= ageMin) worldCupNumber
    else WhichWorldCup(playerAge, worldCupNumber+1)

  }

  @tailrec
  def f(td: mutable.Buffer[Element], worldCupNumber: Int, playerAge: Double, index: Int): String = {

    val age = td(index).text().split(" ")(0).toDouble + td(index).text().split(" ")(3).toDouble / 1000.0
    val round = td( index - 5 ).text() //round for previous
    val date  = td( index - 6 ).text() //
    val week  = td( index - 7 ).text()

    if (playerAge > age) s"WC ${worldCupNumber-1} --> ${(index-7)/4.0+1.0} --> $round --> $date --> $week"
    else f(td, worldCupNumber, playerAge, index + 4)

  }
  
  def WhichRoundOfWorldCup(playerAge: (Double, Int, Int), worldCupNumber: Int): String = {

    val td: mutable.Buffer[Element] = Read_td(worldCupNumber).asScala

    val age = WorldCupAgeMinMax(worldCupNumber)

    val ageMin = age._1._1
    val ageMax = age._2._1

    if(playerAge._1 > ageMax) s"WC ${worldCupNumber-1} --> Final"
    else if(playerAge._1 <= ageMin) s"WC $worldCupNumber --> Final"
    else f(td, worldCupNumber, playerAge._1 ,7)
    
  }

  def Availability(age: (Double, Int, Int)): String = {

    val i = 37

    val worldCupNumber = WhichWorldCup(age, i)
    val worldCupRound = WhichRoundOfWorldCup(age, worldCupNumber)

   worldCupRound

  }

}

class YouthPlayer (args: Array[String]){

  val url: String = args(0)
  val connection: Connection = Jsoup.connect(url)
  val document: Document = connection.get()

  val name: String = document.title.split("»").head
  val age: (Double, Int, Int) = YouthPlayer.Age(document)
  val nationality: String = YouthPlayer.Nationality(document)
  val bestPerformances: String = YouthPlayer.BestPerformances(document)
  val last5Performances: Seq[Double] = YouthPlayer.Last5Performances(document)
  val availability: String= YouthPlayer.Availability(age)

}


class WebScraper {
  def main(args: Array[String]): Unit = {
    //val url = "https://www.example.com"
    val url = args(0)
    val connection = Jsoup.connect(url)
    val document: Document = connection.get()
    //val title = document.title
    //val body: Element = document.body() //całe body wypisuje
    //val full = document.select("title")
    //val full = document.select("div class")
    //val test = document.text("transferPlayer")
    //val title = document.select("Playmaking").text
    //val title = document.getElementsContainingText("Rozgrywanie")
    //val text: String = title.text.substring(title.text.indexOf("Rozgrywanie"))
    //val tl: String = full.substring(full.indexOf("transferPlayer"))
    //println(s"Title of the page: $title")
   // println(s"Title of the page: $text")
    //println(s"Title of the page: $full")
    //println(s"Title of the page: $test")
    //println(s"Body of the page: ${body.text}") //sam tekst, bez znaczników html
    println(s"Transfer Player Information: ${document.select("div.transferPlayerInformation").asScala.length}") // <div class="transferPlayerInformation">
    //println(s"Title of the page: $tl")
   // val splitted_text: Array[String] = text.split(" ")
   // println(s"${splitted_text(0)} ${splitted_text(3)} ")


  }
}

object Main extends App {
  /*val scraper = new WebScraper
  scraper.main(Array("https://www.hattrick.org/pl/Club/Players/Player.aspx?playerId=431317367"))*/

  //val player = (new Player).main(Array("https://www.hattrick.org/pl/Club/Players/Player.aspx?playerId=438617879"))

  val player1 = new Player1(Array("https://www.hattrick.org/pl/Club/Players/Player.aspx?playerId=474603272"))
  //val player1 = new Player1(Array("https://www.hattrick.org/pl/Club/Players/Player.aspx?playerId=474220925"))
  //val player1 = new Player1(Array("https://www.hattrick.org/pl/Club/Players/Player.aspx?playerId=454378333"))
  //val player1 = new Player1(Array("https://www.hattrick.org/pl/Club/Players/Player.aspx?playerId=441279411"))
  //player1.TL_status = 0 //setter
  println(player1.onTL) //getter
  println(player1.name)
  println(player1.exists)
  println(player1.info)
  println(player1.info1)
  player1.info.indices.foreach(i => println(s"$i => ${player1.info(i)}"))
  player1.info1.indices.foreach(i => println(s"$i => ${player1.info1(i)}"))
  player1.info2.indices.foreach(i => println(s"+++ $i => ${player1.info2(i)}"))
  println(s"XXX ${player1.no_match}")
  val loyalty: Option[String] = player1.loyalty
  println(s"${player1.info2(19)}")
  val since_from_string: String = player1.info2(19).select("span").text().split(" ")(1).replace(")","")
  val test = player1.info2(19).select("span.shy span[dir=ltr]").first.text().split(" ")(1).replace(")","")
  val dateFormat = new SimpleDateFormat("dd.MM.yyyy")
  val since_from_date: Date = dateFormat.parse(since_from_string)
  println(s"ZZZ $since_from_string $since_from_date $test")
  println(s"Loyalty: $loyalty")

  val today = Calendar.getInstance().getTime
  val difference = (today.getTime - since_from_date.getTime) / (1000 * 60 * 60 * 24)
  println(difference + " days")


  println(player1.info(4))
  //println(player1.info(135))
  //println(player1.info(135).select("td.right").text().replace(" ",""))
  //TSI
  println(player1.info(4).select("td").text().replaceAll(" ",""))
  //pensja
  println(player1.info(6).select("td").text().replaceAll(" ","")replaceAll("zł/tydzień",""))
  //forma
  println(s"Forma: ${player1.info(10).select("span.bar-denomination").text().split(" ").head}")
  println(s"Forma: ${player1.info(10).select("span.denominationNumber").text().split(" ").head}")
  //kondycja
  println(s"Kondycja: ${player1.info(13).select("span.bar-denomination").text().split(" ").head}")
  println(s"Kondycja: ${player1.info(13).select("span.denominationNumber").text().split(" ").head}")
  //specjalność
  println(s"Specjalność: ${player1.info(8).select("td").text().split(" ").head}")
  //Bronienie
  println(s"Bronienie: ${player1.info(16).select("span.bar-denomination").text().split(" ").head}")
  println(s"Bronienie: ${player1.info(16).select("span.denominationNumber").text().split(" ").head}")
  println(s"Defensywa: ${player1.info(19).select("span.bar-denomination").text().split(" ").head}")
  println(s"Defensywa: ${player1.info(19).select("span.denominationNumber").text().split(" ").head}")
  println(s"Rozgrywanie: ${player1.info(22).select("span.bar-denomination").text().split(" ").head}")
  println(s"Rozgrywanie: ${player1.info(22).select("span.denominationNumber").text().split(" ").head}")
  println(s"Dośrodkowania: ${player1.info(25).select("span.bar-denomination").text().split(" ").head}")
  println(s"Dośrodkowania: ${player1.info(25).select("span.denominationNumber").text().split(" ").head}")
  println(s"Podania: ${player1.info(28).select("span.bar-denomination").text().split(" ").head}")
  println(s"Podania: ${player1.info(28).select("span.denominationNumber").text().split(" ").head}")
  println(s"Skuteczność: ${player1.info(31).select("span.bar-denomination").text().split(" ").head}")
  println(s"Skuteczność: ${player1.info(31).select("span.denominationNumber").text().split(" ").head}")
  println(s"St. Fragmenty: ${player1.info(34).select("span.bar-denomination").text().split(" ").head}")
  println(s"St. Fragmenty: ${player1.info(34).select("span.denominationNumber").text().split(" ").head}")

}

object Test extends App{

  //val player1 = new Player1(Array("https://www.hattrick.org/pl/Club/Players/Player.aspx?playerId=438617879"))
  //val player1 = new Player1(Array("https://www.hattrick.org/pl/Club/Players/Player.aspx?playerId=454378333"))
  //val player1 = new Player1(Array("https://www.hattrick.org/pl/Club/Players/Player.aspx?playerId=441279411"))
  //val player1 = new Player1(Array("https://www.hattrick.org/pl/Club/Players/Player.aspx?playerId=441279412"))
  val player1 = new Player1(Array("https://www.hattrick.org/pl/Club/Players/Player.aspx?playerId=474603272"))

  player1.info.indices.foreach(i => println(s"$i => ${player1.info(i)} -> ${player1.info(i).text()}"))


  @tailrec
  def elementCompare(elements: mutable.Buffer[Element], index: Int, text: String): Int = {

    println(s"$index-${elements(index).text()}-$text-${elements(index).text() == text}")

    if (index == elements.length) -1
    else if(elements(index).text() == text) index
    else elementCompare(elements, index + 1, text)

  }

  //val index = elementCompare(player1.info, 0, "TSI")//działa
  val index = player1.info.indexOf(player1.info.find(_.text == "TSI").get)

  println(index)

  //TSI
  println(player1.info(index+1).select("td").text().replaceAll(" ", ""))
  //pensja
  val pensja20 = player1.info(index+3).select("td").text().replaceAll(" ", "").replaceAll("zł/tydzień", "")
  val pensjaBASE = player1.info(index+3).select("td").select("span").attr("title").split(" ")(0).replaceAll("zł/tydzień", "").trim.replaceAll(" ", "")
  val pensja = if (pensjaBASE.nonEmpty) pensjaBASE else pensja20
  println(pensja)
  //specjalność
  println(s"Specjalność: ${player1.info(index+5).select("td").text().split(" ").head}")
  //forma
  println(s"Forma: ${player1.info(index+7).select("span.bar-denomination").text().split(" ").head}")
  println(s"Forma: ${player1.info(index+7).select("span.denominationNumber").text().split(" ").head}")
  //kondycja
  println(s"Kondycja: ${player1.info(index+10).select("span.bar-denomination").text().split(" ").head}")
  println(s"Kondycja: ${player1.info(index+10).select("span.denominationNumber").text().split(" ").head}")
  //Bronienie
  println(s"Bronienie: ${player1.info(index+13).select("span.bar-denomination").text().split(" ").head}")
  println(s"Bronienie: ${player1.info(index+14).select("span.denominationNumber").text().split(" ").head}")
  println(s"Defensywa: ${player1.info(index+16).select("span.bar-denomination").text().split(" ").head}")
  println(s"Defensywa: ${player1.info(index+17).select("span.denominationNumber").text().split(" ").head}")
  println(s"Rozgrywanie: ${player1.info(index+19).select("span.bar-denomination").text().split(" ").head}")
  println(s"Rozgrywanie: ${player1.info(index+20).select("span.denominationNumber").text().split(" ").head}")
  println(s"Dośrodkowania: ${player1.info(index+22).select("span.bar-denomination").text().split(" ").head}")
  println(s"Dośrodkowania: ${player1.info(index+23).select("span.denominationNumber").text().split(" ").head}")
  println(s"Podania: ${player1.info(index+25).select("span.bar-denomination").text().split(" ").head}")
  println(s"Podania: ${player1.info(index+26).select("span.denominationNumber").text().split(" ").head}")
  println(s"Skuteczność: ${player1.info(index+28).select("span.bar-denomination").text().split(" ").head}")
  println(s"Skuteczność: ${player1.info(index+29).select("span.denominationNumber").text().split(" ").head}")
  println(s"St. Fragmenty: ${player1.info(index+31).select("span.bar-denomination").text().split(" ").head}")
  println(s"St. Fragmenty: ${player1.info(index+32).select("span.denominationNumber").text().split(" ").head}")

  println(s"Wiek: ${player1.age.get._1}")

}

object Test2 extends App{

  val Player_id_start = 441279411
  val Player_id_end = Player_id_start + 500

  for(i <- Player_id_start to Player_id_end){



    val player = new Player1(Array(s"https://www.hattrick.org/pl/Club/Players/Player.aspx?playerId=$i"))

    if(player.exists) {
      //println(s"${player.wiek(1)} ${player.wiek(2)}")
      val player_wiek: Double = player.age.get._1
      println(s"https://www.hattrick.org/pl/Club/Players/Player.aspx?playerId=$i -> $player_wiek ${player.onTL} ${player.no_match}")
    }

  }



}

object Test3 extends App{

  val file = new File("C:\\Users\\Lukasz\\IdeaProjects\\Scrapper\\src\\data\\playerDatabase.csv")
  val writer = CSVWriter.open(file, append = true)
  /*writer.writeAll(List(
    List("Jan", "Kowalski", "30"),
    List("Anna", "Nowak", "25")
  ))*/


  val Player_id_start = 474627757
  val Player_id_end = Player_id_start - 50000

  for(id <- Player_id_start to Player_id_end by -1){

    val player = new Player1(Array(s"https://www.hattrick.org/pl/Club/Players/Player.aspx?playerId=$id"))

    if(player.exists && player.has_club && player.age.get._1 < 18.000 && player.onTL && /*player.df.get >= 6 && player.df.get <=8 &&*/ player.no_match.get && player.loyalty.get.equals(20) && player.daysInClub.get.<=(7)) {

      //println(s"https://www.hattrick.org/pl/Club/Players/Player.aspx?playerId=$id -> ${player.age.get._1} ${player.onTL} ${player.no_match.get} ${player.loyalty.get} ${player.sinceFrom.get} ${player.daysInClub.get} ${player.skills.get._1} ${player.skills.get._2} ${player.skills.get._3} ${player.skills.get._4} ${player.skills.get._5} ${player.skills.get._6} ${player.skills.get._7}")
      println(s"https://www.hattrick.org/pl/Club/Players/Player.aspx?playerId=$id -> ${player.age.get._1} ${player.tsi.get} ${player.condition.get} ${player.gk.get} ${player.df.get} ${player.pm.get} ${player.wg.get} ${player.pass.get} ${player.sco.get} ${player.sp.get}")
      writer.writeAll(List(
        List(s"$id", "Kowalski", "30")
      ))
    }

  }





  writer.close()
}

object Test4 extends App{

  val PlayerID_start = 324275438//474603088
  val step = 100
  val backstep = 9
  val counterMax = 11

  @tailrec
  def searchID(id: Int, counter: Int, lastID: Int): Int = {

    //val player = new Player1(Array(s"https://www.hattrick.org/pl/Club/Players/Player.aspx?playerId=$id"))
    val player = new Player1(Array(s"https://www.hattrick.org/pl/Club/Players/YouthPlayer.aspx?YouthPlayerID=$id"))

    println(s"${player.name}")

    if(counter >= counterMax) lastID
    else if(player.exists)
    {
      println(s"https://www.hattrick.org/pl/Club/Players/Player.aspx?playerId=$id")
      searchID(id+step,0,id)
    }
    else searchID(id-backstep,counter+1,lastID)

  }

  val id = searchID(PlayerID_start, 0, PlayerID_start)

}

object Test5 extends App{

   
}

object Test6 extends App{

  val player = new Player1(Array(s"https://www.hattrick.org/pl/Club/Players/Player.aspx?playerId=468663386"))

  for(i <- player.info3.indices) println(s"$i -> ${player.info3(i)}")

}

object Test7 extends App{

  /*val file = new File("C:\\Users\\Lukasz\\IdeaProjects\\Scrapper\\src\\data\\YPD.csv")
  val writer = CSVWriter.open(file, append = false)

  val Youth_Player_id_start = 324275438
  val Youth_Player_id_end = Youth_Player_id_start - 50

  for (id <- Youth_Player_id_start to Youth_Player_id_end by -1) {

    val youthPlayer = new YouthPlayer(Array(s"https://www.hattrick.org/pl/Club/Players/YouthPlayer.aspx?YouthPlayerID=$id"))

    if (youthPlayer.exists && player.has_club && player.age.get._1 < 18.000 && player.onTL && /*player.df.get >= 6 && player.df.get <=8 &&*/ player.no_match.get && player.loyalty.get.equals(20) && player.daysInClub.get.<=(7)) {

      //println(s"https://www.hattrick.org/pl/Club/Players/Player.aspx?playerId=$id -> ${player.age.get._1} ${player.onTL} ${player.no_match.get} ${player.loyalty.get} ${player.sinceFrom.get} ${player.daysInClub.get} ${player.skills.get._1} ${player.skills.get._2} ${player.skills.get._3} ${player.skills.get._4} ${player.skills.get._5} ${player.skills.get._6} ${player.skills.get._7}")
      println(s"https://www.hattrick.org/pl/Club/Players/Player.aspx?playerId=$id -> ${player.age.get._1} ${player.tsi.get} ${player.condition.get} ${player.gk.get} ${player.df.get} ${player.pm.get} ${player.wg.get} ${player.pass.get} ${player.sco.get} ${player.sp.get}")
      writer.writeAll(List(
        List(s"$id", "Kowalski", "30")
      ))
    }

  }


  writer.close()*/

  }

object Test8 extends App{

/*



  val driver: WebDriver = new ChromeDriver()

  driver.get("https://www.hattrick.org/pl/Club/Players/Player.aspx?playerId=468663386")

  // Pobierz element przycisku "Zobacz historię transferów"
  val viewTransfersButton: WebElement = new WebDriverWait(driver, 10).until(
    ExpectedConditions.elementToBeClickable(By.id("ctl00_ctl00_CPContent_CPMain_btnViewTransferHistory"))
  )

  // Symuluj kliknięcie w przycisk "Zobacz historię transferów"
  viewTransfersButton.click()

  // Pobierz kod źródłowy strony z wynikami
  val transferHistoryPageSource: String = driver.getPageSource

  // Zamknij przeglądarkę
  driver.quit()
*/

}




