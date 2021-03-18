//import algebra.Norms

import java.io._


/**
 * Berechnet Punkte, an denen vorgegebene Funktionen ein Nash-Gleichgewicht einnehmen
 * 
 * @param size Anzahl der maximal zu speichernden Punkte
 * @param tol Eine vorgegebene Toleranz größer 0
 * @param functions Ein Array mit Funktionen Array[Double] => Double
 * @param derivtives die Ableitungen der Funktionen in functions 
 * @param start Ein Array von Startpunkten
 * @param lowerBounds ein Array von unteren Schranken, die die Funktionswerte nicht unterschreiten sollen
 * @param upperBounds ein Array von oberen Schranken, die die Funktionswerte nicht überschreiten sollen
 * @param mu Armijo-Variable aus dem Intervall (0,1).
 */
case class NashCalculator(val size: Int, private var tol: Double, private var functions: Array[Array[Double] => Double],
                          private var derivatives: Array[Array[Double] => Double], private val start: Array[Double],
                          private var lowerBounds: Array[Double], private var upperBounds: Array[Double],
                          private var mu: Double) {

  /*
   * Voraussetzungen für die Funktionalität des Programms:
   * Es werden Größen der Arrays miteinander auf Kompatibilität überprüft.
   * Die Toleranz muss größer als 0 sein.
   * Die Armijo-Variable mu muss aus dem Intervall (0,1) sein.
   * Die size-Variable soll größer 4 sein, damit genügend Punkte gespeichert werden. 
   */
  require(size > 4, "Invalid size!")
  require(tol > 0, "Invalid tolerance!")
  require(functions.length == derivatives.length && functions.length > 1, "Invalid amount of functions and derivatives!")
  require(start.length == functions.length, "Invalid amount of starting points!")
  require(lowerBounds.length == upperBounds.length && lowerBounds.length == functions.length, "Invalid amount of Bounds!")
  require(mu < 1 && mu > 0, "Invalid initialization of the armijo variable mu!")
  
  /**
   * Ein Speicher für die Punkte, die im Laufe des Programms durchlaufen werden.
   */
  val points = BoundedPointBuffer(size, functions.length, tol)
  
  /*
   * Es werden die Startpunkte abgespeichert.
   */
  points.push(start)

  /**
   * Ein privates Array, um die Funktionen zu speichern,
   * die eine Projektion in den Bereich zwischen unteren und oberen Schranken abbilden.
   */
  private val proj = new Array[Double => Double](functions.length)
  for (i <- 0 until functions.length) {
    proj(i) = x => if (x < lowerBounds(i)) lowerBounds(i) else if (x > upperBounds(i)) upperBounds(i) else x
  }

  /**
   * Speichert die Schritte für alle Funktionen
   * discint steht für 'Diskretes Integral'
   */
  private var discInt: Array[Double] = new Array[Double](functions.length)

  /**
   * Ein Array von Schrittweiten für jede Funktion.
   */
  private val alpha: Array[Double] = new Array[Double](functions.length)
  /*
   * Initialisiert die Schrittweiten mit dem Wert 1 (Eins).
   */
  for (i <- 0 until functions.length) alpha(i) = 1
  
  /**
   * Vergrößert die i-te Schrittweite um den Faktor 2
   * @param i Integer. Index der Funktion, für die die Schrittweite vergrößert wird
   * 
   */
  private def largerAlpha(i: Int) = if (alpha(i) * 2 > 0) alpha(i) *= 2
  
  /**
   * Verkleinert die i-te Schrittweite um den Faktor 2
   * @param i Integer. Index der Funktion, für die die Schrittweite verkleiner wird
   */
  private def smallerAlpha(i: Int) = if (alpha(i) / 2 > 0) alpha(i) /= 2

  /**
   * Werte für die Liniensuche im Armijo-Verfahren
   */
  private val beta: Array[Double] = new Array[Double](functions.length)
  /*
   * Initialisert die Werte für die Liniensuche mit 1 (Eins)
   */
  for (i <- 0 until functions.length) beta(i) = 1
  
  /**
   * Verkleinert Beta auf die Hälfte, solange Beta durch 2 größer 0
   */
  private def smallerBeta(i: Int) = if (beta(i) / 2 > 0) beta(i) /= 2

  /**
   * Setzt die Werte für die Liniensuche auf 1 zurück
   */
  private def reset = {
    for (i <- 0 until functions.length) beta(i) = 1
  }

  /**
   * Kontrolliert, ob ein Punkt zur zulässigen Menge der i-ten Funktion gehört.
   * @param x Der Punkt
   * @param i Integer, gibt die Funktion an
   * @return Boolena
   */
  private def inSet(x: Double, i: Int): Boolean = {
    lowerBounds(i) <= x && upperBounds(i) >= x
  }
  
  /**
   * Aktualisiert die oberen Schranken
   * param limits Die oberen Schranken als Array
   */
  def updateUpperBounds(limits:Array[Double]) : Unit = {
    upperBounds = limits
  }
  
  /**
   * Aktualisiert die unteren Schranken
   * @param limits Die unteren Schranken als Array
   */
  def updateLowerBounds(limits:Array[Double]) : Unit = {
    lowerBounds = limits
  }

  /**
   * Implementiert Armijo mit Backtracking
   * @return Das vermutete Nash-Gleichgewicht der Punkte
   */
  def armijoBT: Array[Double] = {

    /**
     * Initialisiert einen Iterator über Double-Arrays
     * Wird benutzt, um die bereits bekannten Punkte zu durchlaufen
     */
    var it = Iterator[Array[Double]]()
    
    /**
     * Initialisiert ein Double-Array, um die aktuellen Punkte zu speichern.
     */
    var currentPoints = new Array[Double](functions.length)
    
    /**
     * Initialisiert einen Double mit Wert 0, um einen einzelnen Punkt zu betrachten
     */
    var ithPoint = 0.0

    /**
     * Führt Armijo mit Backtracking durch, solange mindestens ein Punkt sich mehr als die 
     * vorgegebene Toleranz verändert
     * points.maxDiff gibt die maximale Veränderung in den Punkten an
     */
    while (points.maxDiff > tol) {
      
      /**
       * Variable zum Speichern der aktuell berechneten Punkte
       */
      var newPoints = new Array[Double](functions.length)
      
      for (i <- 0 until functions.length) { // Berechne die diskreten Integrale,
        it = points.oldPointIterator(i) // Iterator für die i-te Funktion wird initialisert,

        /*
         * Berechnet das diskrete Integral,
         */
        while (it.hasNext) {
          discInt(i) += derivatives(i)(it.next)
        }

        /*
         * Normiert das diskrete Integral durch Division durch die Anzahl der bekannten Punkte.
         * Jeder Punkte wird gleich gewichtet.
         */
        discInt(i) *= 1.0 / points.getAmount

      }

      
      for (i <- 0 until functions.length) { //Armijo mit Backtracking
        currentPoints = points.top // Holt die aktuellen Punkte
        ithPoint = currentPoints(i) // Holt den i-ten Punkt
        currentPoints(i) -= alpha(i) * discInt(i) // Berechnet den nächsten Punkt

        if (functions(i)(currentPoints) < functions(i)(points.top) - mu * alpha(i) * discInt(i) * discInt(i) && inSet(currentPoints(i), i)) {

          largerAlpha(i)// Alpha wird vergrößert, falls Schritt erfolgreich.
        } else {// Schritt nicht erfolgreich

          /*
           * Backtracking, um einen besseren Punkt zu finden
           */
          while (functions(i)(currentPoints) >= functions(i)(points.top) && beta(i) > 0.0000000000000001) {
            /*
             * Beta verkleinern, bis besseres Ergebnis gefunden
             */
            smallerBeta(i)
            ithPoint = points.top(i) - alpha(i) * beta(i) * discInt(i)
            currentPoints(i) = ithPoint
            ithPoint = points.top(i)
          } // Ende while-Schleife
          if (!inSet(currentPoints(i), i)) {
            /*
             * Falls außerhalb der zulässigen Menge gelandet, wird der Punkt nicht aktualisiert
             */
            currentPoints(i) = points.top(i)
          }
          smallerAlpha(i)// Alpha kleiner, da Schritt nicht erfolgreich
        }

        /*
         * Der i-te Punkt, der soeben berechnet wurde, wird abgespeichert.
         */
        newPoints(i) = currentPoints(i)
      } // Ende des Backtrackings
      points.push(newPoints)
      reset

    } // Ende for-Schleife
    
    /*
     * Gibt das Ergebnis zurück
     */
    points.top
  }

  /**
   * Druckt Ergebnisse in der Konsole 
   */
  def printcsv: String = {
    var ret = ""

    /*
     * BoundedPointBuffer is full
     */
    if (points.isFull)
      for (i <- 0 until points.size) {
        for (j <- 0 until points.perIt - 1) {
          ret = ret + points(i)(j) + ","
        }
        ret = ret + points(i)(points.perIt - 1) + "," + '\n'
      }
    /*
     * BoundedPointBuffer is not full
     */
    else {
      for (i <- 0 until points.currentPos) {
        for (j <- 0 until points.perIt - 1) {
          ret = ret + points(i)(j) + ","
        }
        ret = ret + points(i)(points.perIt - 1) + "," + '\n'
      }
    }

    ret
  }

  /**
   * Speichert Ergebnisse in einer csv-Datei auf meinem Desktop
   * 
   */
  def savecsv: Unit = {

    val out = new FileWriter("C:/Users/André/Desktop/result.csv")

    /*
     * BoundedPointBuffer not full
     */
    if (points.isFull) {
      for (i <- 0 until functions.length - 1) {
        out.write("f" + (i + 1) + ",")
      }
      out.write("f" + functions.length + "\n")

      for (i <- 0 until points.size) {
        for (j <- 0 until points.perIt - 1) {
          out.write(points(i)(j) + ",")
        }
        out.write(points(i)(points.perIt - 1) + "\n")
      }
    } /*
     * BoundedPointBuffer is not full
     */ else {
      for (i <- 0 until functions.length - 1) {
        out.write("f" + (i + 1) + ",")
      }
      out.write("f" + functions.length + "\n")

      for (i <- 0 to points.currentPos) {
        for (j <- 0 until points.perIt - 1) {
          out.write(points(i)(j) + ",")
        }
        out.write(points(i)(points.perIt - 1) + "\n")
      }
    }

    out.close
  }
  
  def savevalues(k:Int): Unit = {

    val out = new FileWriter("C:/Users/André/Desktop/result.csv")

    /*
     * BoundedPointBuffer not full
     */
    if (points.isFull) {
      

      for (i <- 0 until points.size) {
          out.write(functions(k)(points(i)) + "\n")
        
      }
    } /*
     * BoundedPointBuffer is not full
     */ else {

      for (i <- 0 to points.currentPos) {
        
          out.write(functions(k)(points(i)) + "\n")
        
      }
    }

    out.close
  } 

}