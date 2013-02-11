package search
import java.io._
import scala.collection.mutable.HashMap
import scala.collection.mutable.MutableList

class Query(idIndex : String, wordIndex : String) {
	val idMap = new HashMap[String, String]() // this stores each id and it's Title)
	val index = new HashMap[String, MutableList[(String, Double)]]()
	val numResults = 10
	val titleConstant = 2.0

	def makeIndex() : Unit = {
    	val bReader = new BufferedReader(new FileReader (wordIndex))

    	var line = bReader.readLine() //word|List(^id*score)
      line = bReader.readLine()

      while (line != null && line == "") {
        line = bReader.readLine()
      }

    	while (line != null && line != "") {

    	if (line != "") {
          val keyListPairs = line.split("&") // word :: List(^id*score)
          val word = keyListPairs(0)
          val klPairList = keyListPairs(1)
          val tupleList = MutableList[(String, Double)]() // Tuple id : score
          val tuples = klPairList.split("#") //List of (id * score)
            for (tuple <- tuples) {
              val idValPair = tuple.split("%")
              if (idValPair.length > 1) {
                val id = idValPair(0)
                val score = idValPair(1).toDouble
                new Tuple2(id, score) +=: tupleList
              }
            }
          index.put(word, tupleList)
          line = bReader.readLine
    	}
	}
}

	def makeIDMap() : Unit = {
		val source = scala.io.Source.fromFile(idIndex)
		var content = source.mkString
		source.close

		val pairs = content.split("@")
		for (pair <- pairs) {
			val tuple = pair.split("&")
      if (tuple.length > 1) {
        val id = tuple(0)
        val title = tuple(1)

        idMap.put(id, title)
      }
		}
	}

	def makeHashMaps() : Unit = {
		makeIDMap
		makeIndex
	}

	def queryLoop() : Unit = {
		val bReader = new BufferedReader(new InputStreamReader(System.in))
		println("Enter your query: ")
		var nextLine = bReader.readLine()
		while(nextLine != ":quit") {
			query(nextLine)
			println("Enter another query: ")
			nextLine = bReader.readLine()
		}
		println("Have a nice day")
	}

	def query(input : String) = { 

    	var wordArray = formatInput(input)
		val wordHash = makeWordHash(wordArray) 
	
		val topTen = sort(wordHash)
		var i = 1
		var results = ""

		for (id <- topTen if id != null) {
      	  results += "\n" + i + ": " + idMap(id)
      	  i += 1
		}
    	displayResults(results)
	}



	def sort(wordHash: HashMap[String, Double]): Array[String] = {
		var docs = wordHash.toArray
		var topSorted = new Array[String](10)

		scala.util.Sorting.stableSort(docs, 
			(x: Tuple2[String, Double], y: Tuple2[String, Double]) => x._2 < y._2)
    val toReturn = math.min(docs.length, numResults)
		for(i <- 0 to (toReturn - 1)){
			topSorted(i) = docs(i)._1
		}

		return topSorted
	}

  def makeWordHash(wordArray : Array[String]) : HashMap[String, Double] = {
    val wordHash = new HashMap[String, Double]()
    for (word <- wordArray if index.contains(word)) {
				val scoreList = index(word)
				for ((doc, score) <- scoreList) {
					if (wordHash.contains(doc)) {
						var currentScore = wordHash(doc)
						currentScore += score

						val title = idMap(doc)
						val titleArray = title.toLowerCase.split("[^a-z0-9]")
						var titleweight = 1.0

						for (word <- titleArray){
							if (word.equals("the") || word.equals("and") || word.equals("if") || word.equals("or") || word.equals("") || word.equals("but")){
							} else {
								titleweight = titleweight + titleConstant
							}
						}

						titleweight = titleweight * (titleArray.length)

						wordHash.update(doc, currentScore * titleweight)
					} else {
						wordHash.put(doc, score)
					}
				}
		}
    return wordHash
  }

  def formatInput(input : String) : Array[String] = {
    val lowerCase = input.toLowerCase
	val splitInput = lowerCase.split("[^a-z0-9]")
    return PorterStemmer.stemArray(splitInput)
  }

  def displayResults(results : String) : Unit = {
    println("Results")
    println("")
    if (results == "")
      println("No results matched your query")
    else
      println(results)
  }
}

object Query {
  def main(args : Array[String]) = {
    // id index and word index
    val query = new Query("search/IDMap.txt", "search/Index.txt")
    query.makeHashMaps
    query.queryLoop
  }
}
