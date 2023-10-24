/*from object doSthWithDatabases */
/*{case "findMultipleRecords"
=> 

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

  writeToFile("src/data/allData.csv", true, Seq.empty[String], updatedRecords)


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
          writeToFile("src/data/allData.csv", true, Seq.empty[String], Seq(reducedLine))
          break()
        }

      }
      )


    }


  })

}*/