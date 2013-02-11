package search
import scala.xml.Node
import scala.collection.mutable.HashMap
import scala.collection.mutable.MutableList
import java.io._
import scala.collection.mutable.StringBuilder
import scala.collection.mutable.Set

// implements cosine normalization
//  requires inverse doc frequency

class Index(corpus: String, outFile: String) {

	final val idDoc = "search/IDMap.txt"
	val idMap = new HashMap[String, String]() // this stores each id and it's Title)
	val docFreq = new HashMap[String, Double]() // stores each word, and the number of documents that it appears in
	val index = new HashMap[String, MutableList[(String, Double)]]()
	val titleFreq = new HashMap[String, Double]() // stores each word, and the number of titles that it appears in
	var totalDocs = 0.0
	
	/**
	 * extracts the text from an XML corpus
	 * @return - a list of tuples, with each tuple representing the id and text of a node of the XML corpus
	 */

	def dataExtractor(): MutableList[(String, Array[String])] = {
		val node = xml.XML.loadFile(corpus)
		var docList = MutableList[(String, Array[String])]()
		// mutablelist from list
		// count as expirimentation
		val children = node.child
		
		for (child <- children) {
			val id = (child \ "id").text.replace("\n", "")
			val title = (child \ "title").text.replace("\n", "")
			var text = (child \ "text").text.replace("\n", "")

			idMap.put(id, title)

      if (text != "") {
        val lowerCaseText = text.toLowerCase
        var textArray = text.split("[^a-z0-9]")

        docList = Tuple2(id, PorterStemmer.stemArray(textArray)) +=: docList
      }
		}
		return docList
	}

  /**
   *
   * creates a hashmap of inverse document frequencies
   * @return - a list of String, Array[String] tuples representing id to text pairs
   *
   */

   def calculateInverseDocFrequencies(words : MutableList[(String, Array[String])]) : Unit = {
     for ((id, text) <- words) {
       var uniqueWords = Set.empty[String]
       uniqueWords = uniqueWords ++ text.toList
       for (word <- uniqueWords) {
         if (docFreq.contains(word)) {
           val appearances = docFreq(word)
           docFreq.update(word, appearances + 1.0)
         } else
           docFreq.put(word, 1.0)
       }
       val titleString = idMap(id)
       val titleArray = titleString.split("[^a-z0-9]")
       for (word <- titleArray) {
         if (docFreq.contains(word)) {
           val appearances = docFreq(word)
           docFreq.update(word, appearances + 1.0)
         } else {
           docFreq.put(word, 1.0)
         }
       }
     }
   }

	/**
	 * writes the hashmap of each document's stats to a seperate file, and creates a file-id KV-pair in idMap
	 * @return - a list of 
	 */
	def docStats(words: MutableList[(String, Array[String])]) = {

    for ((id, text) <- words) { // for each id / word array pair

      val wordCounter = new HashMap[String, Double]() // word : frequency
      val titleString = idMap(id)
      val titleArray = titleString.split("[^a-z0-9]")
      var totalWords = 0.0

    	for (y <- text if y != "") { // for each word in text
      		if(wordCounter.contains(y)){ //if the word already appears in the hashmap, simply increment its number of appearances by 1
        		val appearances = wordCounter(y)
        		wordCounter.update(y, appearances + 1.0)
        } else {                 // if not, add it to the hashmap, and add it to the docFreq hashmap
            wordCounter.put(y, 1.0)

            /*if (docFreq.contains(y)){ // check if frequency for inverse doc frequency contains this word. only checks here since first occurance
              val appearances = docFreq(y)
              docFreq.update(y, appearances + 1.0) // if it does increment by one
          } else { // if it doesnt add it with 1
              docFreq.put(y, 1.0)
          } */
        }

       totalWords = totalWords + 1.0
     }
     for (word <- titleArray if word != "") {
       if (wordCounter.contains(word)) {
          val appearances = wordCounter(word)
          wordCounter.update(word, appearances + 10.0)
        } else {
          wordCounter.put(word, 100000.0)
        }
      }
   	 totalDocs = totalDocs + 1.0 //increment total documents

     updateForDocScore(id, wordCounter)

    } //end id : text pair iteration
  }

  def calculateEuclidian(wordCounter : HashMap[String, Double]) : Double = {
    var euclidian = 0.0
    for ((word, appearances) <- wordCounter)
      euclidian += (appearances * appearances)
    return math.sqrt(euclidian)
  }

  def calculateCosineNormalization(wordCounter : HashMap[String, Double]) : Double = {
    var cosineNorm = 0.0
    for ((word, appearances) <- wordCounter) {
      cosineNorm += appearances * docFreq(word)
    }
    return math.sqrt(cosineNorm)
  }

  def updateForDocScore(id : String, wordCounter : HashMap[String, Double]) : Unit = {
    //var euclidean = calculateEuclidian(wordCounter)
    var cosineNorm = calculateCosineNormalization(wordCounter)
    for ((word, appearances) <- wordCounter) {
      var value = appearances / cosineNorm
      val entry = new Tuple2(id, value)
      if (index.contains(word)) {
        val docList = index(word)
        entry +=: docList
        //index.update(word, docList)
      } else
        index.put(word, MutableList(entry))
    }
  }


	def createIndex() = {
		val bWriter = new BufferedWriter(new FileWriter(outFile))
		for ((word, alot) <- index){
			var wordText = new StringBuilder(word + "&")
			val inverseFreq = math.log10(totalDocs/ (1 + docFreq.get(word).get))

			for ((id, score) <- alot){
				wordText.append("#" + id + "%" + (score + inverseFreq))
			}
			bWriter.write("\n" + wordText.mkString)
			bWriter.flush
		}
	}

	def createIDMap() = {
		val bWriter = new BufferedWriter(new FileWriter(idDoc))
		for ((id, title) <- idMap){
      		if (id != "" && title != "")
        	bWriter.write("@" + id + "&" + title)
			bWriter.flush
		}
	}
}

object Index {

	def main(args : Array[String]) = {
		if (args.length > 1) {
			val wikiPath = args(0)
			val indexPath = args(1)
		val indexer = new Index(wikiPath, indexPath)
		//val indexer = new Index("/course/cs018/src/search/MedWiki.xml", "search/Index.txt")
    val wordList = indexer.dataExtractor()
    indexer.calculateInverseDocFrequencies(wordList)
		indexer.docStats(wordList)
		indexer.createIndex()
		indexer.createIDMap()
	}
	}
}
