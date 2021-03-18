
/**
 * @param size Größe des Puffers
 * @param perIt Anzahl der Punkte, die in jeder Iteration abgespeichert werden müssen
 * @param tol Toleranz
 * 
 * Speichert Punkte in Double-Arrays.
 * Kann berechnen, ob die Punkte sich mehr als die vorgegebene Toleranz tol verändert haben
 * Kann einen Iterator über die bisher gespeicherten Punkte zurück geben.
 */
case class BoundedPointBuffer(val size: Int, val perIt: Int, tol: Double) {
  
  /**
   * Gibt den i-ten Eintrag im Puffer zurück
   * @param i Die Stelle des Eintrags, der zurückgegeben werden soll
   * @return Ein Array von Punkten
   */
  def apply(i: Int) = try { 
    ring(i) 
    }catch{
      case e :ArrayIndexOutOfBoundsException => throw new Exception("No such element available.")
      case e : Exception => throw new Exception("Unknown error.")      
    }

  /**
   * Modulo-Funktion
   * @param i Integer, von dem ein Modulo-Wert berechnet wird
   * @return Der positive Modulo-Wert zu einer Zahl
   */
  private def mod(i: Int) = {
    var ret = i % size
    if (ret < 0) ret += size
    ret
  }

  /**
   * Ein Array von Double-Arrays.
   * Speichert die Punkte, die im Verfahren durchlaufen werden
   */
  private val ring = new Array[Array[Double]](size)
  for (i <- 0 until size) {
    ring(i) = new Array[Double](perIt)
  }

  /**
   * Die Position im Array, die als nächstes befüllt wird.
   */
  private var front = 0 
  /**
   * Die Anzahl der gespeicherten Punkte
   */
  private var amount = 0
  
  /**
   * Gibt die Anzahl der gespeicherten Punkte zurück
   * @return Int
   */
  def getAmount = amount

  /**
   * Gibt zurück, ob der Puffer voll ist
   * @return Boolean
   */
  def isFull: Boolean = amount == size
  
  /**
   * Gibt zurück, ob der Puffer leer ist
   * @return Boolean
   */
  def isEmpty: Boolean = amount == 0

  /**
   * Gibt die Position zurück, an der als nächstes gespeichert wird
   * @return Int  
   */
  def nextPos: Int = front
  /**
   * Gibt den zuletzt gespeicherten Eintrag zurück.
   * Es handelt sich um den aktuellen Eintrag.
   * @return Int
   */
  def currentPos: Int = mod(front - 1)
  /**
   * Gibt den zuvor gespeicherten Eintrag zurück.
   * Es handelt sich um den vorigen Eintrag.
   * @return Int
   */
  def lastPos: Int = mod(front - 2)

  /**
   * Aktualisiert die Variablen, die speichern
   * - in welchem Feld der nächste Eintrag gespeichert wird (front Variable)
   * - wie viele Einträge bereits gespeichert sind (amount Variable)
   */
  private def update: Unit = {
    front = mod(front + 1)
    if (amount < size) amount += 1
  }

  /**
   * Speichert einen neuen Eintrag
   * @param xs Ein Array von Punkten
   */
  def push(xs: Array[Double]): Unit = {
    ring(nextPos) = xs
    update
  }

  /**
   * Projiziert die aktuellen Einträge auf die zulässige Menge
   * @param f Die Funktion, die die Projektion durchführt
   */
  def proj(f: Array[Double => Double]) = {
    for (i <- 0 until perIt) {
      ring(currentPos)(i) = f(i)(ring(currentPos)(i))
    }
  }

  /**
   * Gibt die aktuellen Punkte zurück
   * @return Array[Double]
   */
  def top: Array[Double] = return ring(currentPos).clone
  
  /**
   * Gibt die vorigen Punkte zurück
   * @return Array[Double]
   */
  def last: Array[Double] = return ring(lastPos).clone

  /**
   * Gibt zurück, ob die Punkte sich im letzten Schritt weniger geändert haben
   * als eine vorgegebene Toleranz
   * @return false, wenn Veränderung kleiner als Toleranz, sonst true
   */
  def isDiff: Boolean = if (maxDiff < tol) false else true

  /**
   * Gibt die betragsmäßig größte Veränderung zwischen den aktuellen und den vorherigen Punkten zurück
   * @return Double 
   */
  def maxDiff: Double = {
    if (amount <= 1) tol * 2 // Eine Zahl, die größer als die Toleranz ist.
    else {
      val to = top
      val la = last
      var ret = 0.0d
      for (i <- 0 until perIt) {
        if (ret < scala.math.abs(to(i) - la(i)))
          ret = scala.math.abs(to(i) - la(i))
      }
      ret
    }
  }

  /**
   * Gibt einen Iterator über die bisher gespeicherten Punkte zurück
   * In allen Double-Arrays wird die j-te Stelle durch den aktuellen Punkt der j-ten Funktion ersetzt
   * @param j die j-te Funktion, deren Punkte benötigt werden.
   * @return Iterator über Double-Arrays
   */
  def oldPointIterator(j: Int): Iterator[Array[Double]] = {
    val ret = new Array[Array[Double]](amount)
    for (i <- 0 until amount) {
      ret(i) = ring(i).clone
      ret(i)(j) = ring(currentPos)(j)
    }
    ret.iterator
  }

}