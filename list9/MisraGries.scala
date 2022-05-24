package MisraGries {

  import scala.collection.mutable.Map

  object MisraGries {

    def misraGries(k: Int) = new MisraGries(k)

  }
  class MisraGries(private val k: Int) {

    private var registry = Map[String, Int]()
    private var threshold = 0.0

    def add(s: String): Unit = {
      if (registry contains s) registry.update(s, registry(s) + 1)
      else if (registry.size < k - 1) {
        registry.update(s, 1)
      } else {
        registry.mapValuesInPlace((k,v) => v - 1).filterInPlace((k,v) => v != 0)
      }
    }
    def summarizeStream(stream: Iterable[String]): Unit = {
      stream.foreach(s => add(s))
      threshold = stream.size.toDouble/k
    }
    def eliminateFalsePositive(stream: Iterable[String]): Unit = {
        var tmpRegistry = registry.clone()
        tmpRegistry.mapValuesInPlace((k,v) => 0)
        stream.foreach(s => {
            if(tmpRegistry contains s)
            {
                tmpRegistry.update(s, tmpRegistry(s) + 1)
            }
        })
        registry = tmpRegistry.filterInPlace((k,v) => v > threshold).mapValuesInPlace((k,v) => registry(k))
    }


    def getSummary(): Map[String, Int] = registry
  }

}

