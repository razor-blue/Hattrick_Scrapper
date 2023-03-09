import com.github.tototoshi.csv.CSVWriter

import java.io.File
import com.github.tototoshi.csv.*

object YouthDatabase {

  def updatePlayer(id: String, l5p: Seq[Double]): Unit = {

    val yp = new Youth(Array(s"https://www.hattrick.org/pl/Club/Players/YouthPlayer.aspx?YouthPlayerID=",id))
    println(f" ${yp.name.get},${yp.id.get},${yp.age.get._1}%2.3f,${yp.since.get},${yp.availability.get.replaceAll(" --> ",",")},${yp.bestPerformances.get},${yp.last5Performances.get.mkString(",")}")
    println(f" ${yp.name.get},${yp.id.get},${yp.age.get._1}%2.3f,${yp.since.get},${yp.availability.get.replaceAll(" --> ",",")},${yp.bestPerformances.get},${yp.last5Performances.get.zip(l5p).map(x => math.max(x._1,x._2)).mkString(",")}")

  }

  def update(pathToCsvFile: String): Unit = {

    val bufferedSource = io.Source.fromFile(pathToCsvFile)
    for (line <- bufferedSource.getLines) {
      println(line)
      val cols = line.split(",").map(_.trim)
      //println(s"${cols(0)}|${cols(1)}|${cols(2)}|${cols(16)}|${cols(17)}|${cols(18)}|${cols(19)}|${cols(20)}|${cols(21)}")
      val l5p: Seq[Double] = Seq(cols(16),cols(17),cols(18),cols(19),cols(20),cols(21)).map(_.toDouble)
      //println(l5p)
      updatePlayer(cols(1),l5p)
    }
    bufferedSource.close

  }

}


object Testt extends App{

/*  val List_of_ids = Seq("324275438", "320411270", "317512515")

  //val p1 = new Youth(Array(s"https://www.hattrick.org/pl/Club/Players/YouthPlayer.aspx?YouthPlayerID=","317512515"))
  //val p1 = new Youth(Array(s"https://www.hattrick.org/pl/Club/Players/YouthPlayer.aspx?YouthPlayerID=","324275438"))
  val p1 = new Youth(Array(s"https://www.hattrick.org/pl/Club/Players/YouthPlayer.aspx?YouthPlayerID=", "310526039"))
  //val p1 = new Youth(Array("https://www.hattrick.org/pl/Club/Players/YouthPlayer.aspx?YouthPlayerID=","310723669"))

  val file = new File("C:\\Users\\Lukasz\\IdeaProjects\\Scrapper\\src\\data\\youthPlayerDatabase.csv")
  val writer = CSVWriter.open(file, append = false)
  List_of_ids.foreach(id => {

    val p = new Youth(Array(s"https://www.hattrick.org/pl/Club/Players/YouthPlayer.aspx?YouthPlayerID=", s"$id"))
    if (p.exists && p.stillInYouthAcademy) {
      val l5p = p.last5Performances.get.mkString(",")
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
  if (p1.stillInYouthAcademy) println(p1.bestPerformances.get)
  if (p1.stillInYouthAcademy) println(p1.last5Performances.get)


  val p2 = new Senior(Array(s"https://www.hattrick.org/pl/Club/Players/Player.aspx?playerId=", "468663386"))

  println(p2.exists)
  println(p2.has_club)
  println(p2.name.get)
  println(p2.age.get._1)
  println(p2.nationality.get)
  println(s"Specjalność: ${p2.speciality.get}")
  println(p2.link.get)*/

  YouthDatabase.update("C:\\Users\\Lukasz\\IdeaProjects\\Scrapper\\src\\data\\youthPlayerDatabase.csv")

}
